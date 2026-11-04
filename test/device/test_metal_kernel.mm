// Opt-in Metal device oracle.
//
// Loads the metallib built from the FortGen-emitted leaf plus the launch
// wrapper, runs it on the GPU, and compares every output against an
// independent host oracle evaluated in double from the mathematical
// definition, with binary32 reciprocal rounding before sine.
// The oracle never reads the emitted source.
//
// Per-output expectation modes:
//   v compare with the double oracle    n expect NaN
//   p expect +inf                       m expect -inf
//   u expect -inf or NaN after a possible subnormal input flush
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include <cmath>
#include <cfloat>
#include <cstdio>
#include <cstdlib>

#ifndef FORTGEN_METAL_SAMPLES
#error "FORTGEN_METAL_SAMPLES must be defined"
#endif
#ifndef FORTGEN_METAL_LIBRARY
#error "FORTGEN_METAL_LIBRARY must name the compiled Metal library"
#endif

#include "metal_oracle.inc"

int main() {
    static_assert(kSamples == FORTGEN_METAL_SAMPLES,
                  "sample table and the compiled Metal wrapper must agree");
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (device == nil) {
            std::fprintf(stderr, "no Metal device; skipping the device oracle\n");
            return 77;
        }
        std::printf("device: %s\n", [[device name] UTF8String]);

        NSError* error = nil;
        id<MTLLibrary> library = [device newLibraryWithURL:
            [NSURL fileURLWithPath:@FORTGEN_METAL_LIBRARY] error:&error];
        if (library == nil) {
            std::fprintf(stderr, "cannot load Metal library: %s\n",
                        [[error localizedDescription] UTF8String]);
            return 1;
        }
        id<MTLFunction> function = [library newFunctionWithName:@"metal_oracle"];
        if (function == nil) {
            std::fprintf(stderr, "metal_oracle is missing from the library\n");
            return 1;
        }
        id<MTLComputePipelineState> pipeline =
            [device newComputePipelineStateWithFunction:function error:&error];
        if (pipeline == nil) {
            std::fprintf(stderr, "pipeline creation failed: %s\n",
                        [[error localizedDescription] UTF8String]);
            return 1;
        }

        const int threads = kSamples + kGuardThreads;
        float* inputs = static_cast<float*>(std::malloc(sizeof(float) * 3 * kSamples));
        for (int i = 0; i < kSamples; ++i) {
            inputs[i] = kTable[i].x;
            inputs[kSamples + i] = kTable[i].dx;
            inputs[2 * kSamples + i] = kTable[i].seed;
        }
        const int slots = threads * kOutputs;
        float* host = static_cast<float*>(std::malloc(sizeof(float) * slots));
        for (int i = 0; i < slots; ++i) host[i] = kSentinel;

        id<MTLBuffer> input_buffer = [device newBufferWithBytes:inputs
            length:sizeof(float) * 3 * kSamples options:MTLResourceStorageModeShared];
        id<MTLBuffer> output_buffer = [device newBufferWithBytes:host
            length:sizeof(float) * slots options:MTLResourceStorageModeShared];
        id<MTLCommandQueue> queue = [device newCommandQueue];
        id<MTLCommandBuffer> commands = [queue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [commands computeCommandEncoder];
        [encoder setComputePipelineState:pipeline];
        [encoder setBuffer:input_buffer offset:0 atIndex:0];
        [encoder setBuffer:output_buffer offset:0 atIndex:1];
        [encoder dispatchThreads:MTLSizeMake(threads, 1, 1)
             threadsPerThreadgroup:MTLSizeMake(16, 1, 1)];
        [encoder endEncoding];
        [commands commit];
        [commands waitUntilCompleted];
        if ([commands error] != nil) {
            std::fprintf(stderr, "command buffer failed: %s\n",
                [[[commands error] localizedDescription] UTF8String]);
            return 1;
        }

        const float* results = static_cast<const float*>([output_buffer contents]);
        double max_rel[kOutputs];
        for (int j = 0; j < kOutputs; ++j) max_rel[j] = 0.0;
        int compared = 0;
        for (int i = 0; i < kSamples; ++i) {
            double expected[kOutputs];
            oracle(kTable[i], expected);
            for (int j = 0; j < kOutputs; ++j) {
                ++compared;
                if (!check(i, j, results[i * kOutputs + j], expected[j],
                          kTable[i].mode[j], max_rel)) fail = YES;
            }
        }
        // Guard: the extra dispatched threads must not have written anywhere.
        int guards = 0;
        for (int i = kSamples; i < threads; ++i) {
            for (int j = 0; j < kOutputs; ++j) {
                if (results[i * kOutputs + j] != kSentinel) {
                    std::fprintf(stderr, "guard slot %d,%d was written\n", i, j);
                    fail = YES;
                }
                ++guards;
            }
        }
        if (!fail) {
            std::printf("max scaled error per output:");
            for (int j = 0; j < kOutputs; ++j)
                std::printf(" %s=%.2e", kNames[j], max_rel[j]);
            std::printf("\n");
        }
        std::free(inputs);
        std::free(host);
        if (fail) return 1;
        std::printf("Metal leaf: %d independent output checks over %d samples passed, "
                    "%d guard slots untouched\n", compared, kSamples, guards);
        return 0;
    }
}

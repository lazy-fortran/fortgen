#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>

static id<MTLDevice> typed_device;
static id<MTLComputePipelineState> typed_pipeline;
static id<MTLCommandQueue> typed_queue;

static void gpu_digit(unsigned long long x, unsigned long long y, float estimate,
    unsigned long long* lo, unsigned long long* hi, unsigned long long* sum,
    bool* carry, unsigned long long* difference, bool* borrow,
    unsigned long long* large, unsigned long long* signbit,
    unsigned long long* maximum, unsigned long long* shifted,
    unsigned long long* masked, unsigned long long* merged, unsigned int* digit,
    bool* valid, unsigned long long* residual, bool* cert,
    unsigned long long* product) {
    if (!typed_device) {
        typed_device = MTLCreateSystemDefaultDevice();
        if (!typed_device) std::exit(77);
        NSError* error = nil;
        id<MTLLibrary> library = [typed_device newLibraryWithURL:
            [NSURL fileURLWithPath:@FORTGEN_TYPED_METAL_LIBRARY] error:&error];
        if (!library) {
            std::fprintf(stderr, "library: %s\n", [[error localizedDescription] UTF8String]);
            std::exit(1);
        }
        id<MTLFunction> function = [library newFunctionWithName:@"typed_digit_oracle"];
        typed_pipeline = [typed_device newComputePipelineStateWithFunction:function error:&error];
        if (!typed_pipeline) std::exit(1);
        typed_queue = [typed_device newCommandQueue];
        std::printf("typed oracle device: %s\n", [[typed_device name] UTF8String]);
    }
    @autoreleasepool {
        const unsigned long long inputs[] = {x, y};
        unsigned long long sentinel[20];
        for (auto& v : sentinel) v = 0xdeadbeefdeadbeefULL;
        id<MTLBuffer> ib = [typed_device newBufferWithBytes:inputs length:sizeof(inputs)
            options:MTLResourceStorageModeShared];
        id<MTLBuffer> eb = [typed_device newBufferWithBytes:&estimate length:sizeof(estimate)
            options:MTLResourceStorageModeShared];
        id<MTLBuffer> ob = [typed_device newBufferWithBytes:sentinel length:sizeof(sentinel)
            options:MTLResourceStorageModeShared];
        id<MTLCommandBuffer> command = [typed_queue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [command computeCommandEncoder];
        [encoder setComputePipelineState:typed_pipeline];
        [encoder setBuffer:ib offset:0 atIndex:0];
        [encoder setBuffer:eb offset:0 atIndex:1];
        [encoder setBuffer:ob offset:0 atIndex:2];
        [encoder dispatchThreads:MTLSizeMake(4, 1, 1)
             threadsPerThreadgroup:MTLSizeMake(4, 1, 1)];
        [encoder endEncoding];
        [command commit];
        [command waitUntilCompleted];
        if (command.status != MTLCommandBufferStatusCompleted || command.error) std::exit(1);
        const auto* v = static_cast<const unsigned long long*>(ob.contents);
        for (int i = 17; i < 20; ++i) if (v[i] != sentinel[i]) std::exit(1);
        *lo = v[0]; *hi = v[1]; *sum = v[2]; *carry = v[3];
        *difference = v[4]; *borrow = v[5]; *large = v[6]; *signbit = v[7];
        *maximum = v[8]; *shifted = v[9]; *masked = v[10]; *merged = v[11];
        *digit = static_cast<unsigned int>(v[12]); *valid = v[13];
        *residual = v[14]; *cert = v[15]; *product = v[16];
    }
}

// Reuse the host mathematical oracle with device execution for the mixed leaf.
#define TYPED_DIGIT_EXEC gpu_digit
#include "../test_typed_host.cpp.in"

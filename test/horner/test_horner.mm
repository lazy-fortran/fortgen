// Metal Horner kernels in binary32 against a host long-double dense monomial
// oracle evaluated at the same binary32 coefficients and coordinates.
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include <cfloat>
#include <cmath>
#include <cstdio>
#include <vector>

#include "horner_cases.h"

#ifndef FORTGEN_HORNER_METAL_LIBRARY
#error "FORTGEN_HORNER_METAL_LIBRARY must name the compiled Metal library"
#endif

static long double monomial(int k, int a, float x) {
    if (a > k) return 0.0L;
    long double v = 1.0L;
    for (int i = k - a + 1; i <= k; ++i) v *= i;
    for (int i = 0; i < k - a; ++i) v *= x;
    return v;
}

int main() {
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (device == nil) {
            std::fprintf(stderr, "no Metal device; skipping the Horner oracle\n");
            return 77;
        }
        NSError* error = nil;
        id<MTLLibrary> library = [device newLibraryWithURL:
            [NSURL fileURLWithPath:@FORTGEN_HORNER_METAL_LIBRARY] error:&error];
        if (library == nil) {
            std::fprintf(stderr, "cannot load Metal library: %s\n",
                         [[error localizedDescription] UTF8String]);
            return 1;
        }
        id<MTLFunction> function = [library newFunctionWithName:@"horner_run_case"];
        id<MTLComputePipelineState> pipeline =
            [device newComputePipelineStateWithFunction:function error:&error];
        if (pipeline == nil) {
            std::fprintf(stderr, "pipeline creation failed: %s\n",
                         [[error localizedDescription] UTF8String]);
            return 1;
        }
        id<MTLCommandQueue> queue = [device newCommandQueue];

        const int nout = 256;
        unsigned state = 12345u;
        auto uniform = [&state]() {
            state = 1664525u*state + 1013904223u;
            return (state >> 8)*(1.0/16777216.0);
        };
        int failures = 0, order_base = 0;
        for (int kase = 1; kase <= horner_ncase; ++kase) {
            const int *t = horner_table + 5*(kase - 1);
            const int d = t[0], n[3] = {t[1], t[2], t[3]};
            const int nq = t[4] == 0 ? 3 : t[4];
            const int norders = horner_norders[kase - 1];
            const int *orders = horner_orders + 3*order_base;
            order_base += norders;
            const int n2 = d >= 2 ? n[1] : 0, n3 = d >= 3 ? n[2] : 0;
            const int ncoeff = nq*(n[0] + 1)*(n2 + 1)*(n3 + 1);
            if (norders*nq > nout) {
                std::fprintf(stderr, "case %d exceeds the output buffer\n", kase);
                return 1;
            }
            std::vector<float> c(ncoeff);
            float x[3];
            for (int trial = 0; trial < 25; ++trial) {
                for (float &v : c) v = static_cast<float>(2*uniform() - 1);
                for (float &v : x) v = static_cast<float>(1.5*uniform() - 0.25);
                id<MTLBuffer> cbuf = [device newBufferWithBytes:c.data()
                    length:sizeof(float)*ncoeff options:MTLResourceStorageModeShared];
                id<MTLBuffer> xbuf = [device newBufferWithBytes:x
                    length:sizeof(x) options:MTLResourceStorageModeShared];
                id<MTLBuffer> obuf = [device newBufferWithLength:sizeof(float)*nout
                    options:MTLResourceStorageModeShared];
                id<MTLCommandBuffer> commands = [queue commandBuffer];
                id<MTLComputeCommandEncoder> encoder = [commands computeCommandEncoder];
                [encoder setComputePipelineState:pipeline];
                [encoder setBytes:&kase length:sizeof(int) atIndex:0];
                [encoder setBytes:&nq length:sizeof(int) atIndex:1];
                [encoder setBuffer:cbuf offset:0 atIndex:2];
                [encoder setBuffer:xbuf offset:0 atIndex:3];
                [encoder setBuffer:obuf offset:0 atIndex:4];
                [encoder dispatchThreads:MTLSizeMake(1, 1, 1)
                     threadsPerThreadgroup:MTLSizeMake(1, 1, 1)];
                [encoder endEncoding];
                [commands commit];
                [commands waitUntilCompleted];
                if ([commands error] != nil) {
                    std::fprintf(stderr, "command buffer failed: %s\n",
                        [[[commands error] localizedDescription] UTF8String]);
                    return 1;
                }
                const float *out = static_cast<const float*>([obuf contents]);
                int offset = 0;
                for (int m = 0; m <= 3; ++m) {
                    int nm = 0;
                    for (int j = 0; j < norders; ++j)
                        if (orders[3*j] + orders[3*j+1] + orders[3*j+2] == m) ++nm;
                    int slot = 0;
                    for (int j = 0; j < norders; ++j) {
                        const int *a = orders + 3*j;
                        if (a[0] + a[1] + a[2] != m) continue;
                        for (int iq = 0; iq < nq; ++iq) {
                            long double ref = 0.0L, mag = 0.0L;
                            for (int k3 = 0; k3 <= n3; ++k3)
                                for (int k2 = 0; k2 <= n2; ++k2)
                                    for (int k1 = 0; k1 <= n[0]; ++k1) {
                                        const int idx = iq + nq*(k1 + (n[0] + 1)*
                                                        (k2 + (n2 + 1)*k3));
                                        const long double term = c[idx]*
                                            monomial(k1, a[0], x[0])*
                                            monomial(k2, a[1], x[1])*
                                            monomial(k3, a[2], x[2]);
                                        ref += term;
                                        mag += fabsl(term);
                                    }
                            const float actual = out[offset*nq + slot + nm*iq];
                            const long double tol = 64.0L*FLT_EPSILON*fmaxl(mag, FLT_MIN);
                            if (!std::isfinite(actual) || fabsl(actual - ref) > tol) {
                                std::fprintf(stderr, "case %d derivative %d%d%d quantity"
                                             " %d: %.9g vs %.17Lg\n", kase, a[0], a[1],
                                             a[2], iq, actual, ref);
                                ++failures;
                            }
                        }
                        ++slot;
                    }
                    offset += nm;
                }
            }
        }
        if (failures > 0) {
            std::fprintf(stderr, "%d Horner Metal outputs disagree\n", failures);
            return 1;
        }
        std::printf("all %d Horner Metal cases agree\n", horner_ncase);
        return 0;
    }
}

#include <cuda_runtime.h>
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include "select_kernel.cuh"

static void checked(cudaError_t status) {
    if (status != cudaSuccess) {
        std::fprintf(stderr, "%s\n", cudaGetErrorString(status));
        std::exit(1);
    }
}

__global__ void evaluate(double* values) {
    const int i = threadIdx.x;
    if (i >= 17) return;
    const double x = (i - 8)/4.0;
    const double y = (14 - i)/8.0;
    select_kernel(x, y, &values[3*i], &values[3*i + 1], &values[3*i + 2]);
}

int main() {
    double *device, values[51];
    bool saw_zero = false, saw_tie = false;
    checked(cudaMalloc(&device, sizeof(values)));
    evaluate<<<1, 32>>>(device);
    checked(cudaGetLastError());
    checked(cudaMemcpy(values, device, sizeof(values), cudaMemcpyDeviceToHost));
    checked(cudaFree(device));
    for (int i=0; i<17; ++i) {
        const long double x=(i-8)/4.0L, y=(14-i)/8.0L;
        long double reference[3];
        if (x != 0.0L) {
            reference[0] = sinl(x)/x;
        } else {
            reference[0] = 1.0L;
            saw_zero = true;
        }
        reference[1] = x < y ? x*y : x + y;
        reference[2] = (x <= y ? x : y) + std::min({x, y, 0.25L});
        if (x == y) {
            reference[2] += std::max({x, y, 0.25L});
            saw_tie = true;
        }
        for (int j=0; j<3; ++j) {
            // NaN fails this comparison too.
            if (!(fabsl(values[3*i+j]-reference[j]) <= 2e-14L)) {
                std::fprintf(stderr, "sample %d output %d mismatch\n", i, j);
                return 1;
            }
        }
    }
    if (!saw_zero || !saw_tie) {
        std::fprintf(stderr, "samples miss the guarded points\n");
        return 1;
    }
    std::puts("CUDA select leaf: 17 independent long-double samples passed");
}

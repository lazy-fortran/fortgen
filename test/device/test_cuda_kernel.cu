#include <cuda_runtime.h>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include "behavior_kernel.cuh"

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
    const double y = (11 - i)/8.0;
    behavior_kernel(x, y, &values[i]);
}

int main() {
    double *device, values[17];
    checked(cudaMalloc(&device, sizeof(values)));
    evaluate<<<1, 32>>>(device);
    checked(cudaGetLastError());
    checked(cudaMemcpy(values, device, sizeof(values), cudaMemcpyDeviceToHost));
    checked(cudaFree(device));
    for (int i=0; i<17; ++i) {
        const long double x=(i-8)/4.0L, y=(11-i)/8.0L;
        const long double reference=sinl(x*y)/(1.0L+x*x);
        if (!std::isfinite(values[i]) || fabsl(values[i]-reference)>2e-14L) {
            std::fprintf(stderr, "sample %d mismatch\n", i);
            return 1;
        }
    }
    std::puts("CUDA leaf: 17 independent long-double samples passed");
}

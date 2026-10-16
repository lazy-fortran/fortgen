#include <cuda_runtime.h>
#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include "reciprocal_kernel.cuh"

static void checked(cudaError_t status) {
    if (status != cudaSuccess) {
        std::fprintf(stderr, "%s\n", cudaGetErrorString(status));
        std::exit(1);
    }
}

__global__ void evaluate_reciprocal(const double *x, double *out, int n) {
    const int i = threadIdx.x;
    if (i >= n) return;
    reciprocal_kernel(x[i], -0.375, 1.25, out+5*i, out+5*i+1,
                      out+5*i+2, out+5*i+3, out+5*i+4);
}

int main() {
    const double inf = std::numeric_limits<double>::infinity();
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const double input[] = {-4.0, -0.125, 0.125, 3.0, -1e155, 1e155,
        -1e-155, 1e-155, DBL_MAX, -DBL_MAX, DBL_MIN, -DBL_MIN,
        0.0, -0.0, inf, -inf, nan};
    constexpr int n = sizeof(input)/sizeof(input[0]);
    double *x, *out, values[5*n];
    checked(cudaMalloc(&x, sizeof(input)));
    checked(cudaMalloc(&out, sizeof(values)));
    checked(cudaMemcpy(x, input, sizeof(input), cudaMemcpyHostToDevice));
    evaluate_reciprocal<<<1,32>>>(x, out, n);
    checked(cudaGetLastError());
    checked(cudaMemcpy(values, out, sizeof(values), cudaMemcpyDeviceToHost));
    checked(cudaFree(out));
    checked(cudaFree(x));
    for (int i = 0; i < n; ++i) {
        const long double r = 1.0L/static_cast<long double>(input[i]);
        const long double reference[] = {r, r*r, r*r*r, 0.375L*r*r, -1.25L*r*r};
        for (int j = 0; j < 5; ++j) {
            const double actual = values[5*i+j];
            const long double expected = reference[j];
            bool valid;
            if (std::isnan(expected)) valid = std::isnan(actual);
            else if (fabsl(expected) > DBL_MAX)
                valid = std::isinf(actual) && (std::signbit(actual) == std::signbit(expected));
            else {
                const long double tolerance = 16.0L*DBL_EPSILON*fmaxl(fabsl(expected), DBL_MIN);
                valid = std::isfinite(actual) && fabsl(actual-expected) <= tolerance;
                if (expected == 0.0L)
                    valid = valid && (std::signbit(actual) == std::signbit(expected));
            }
            if (!valid) {
                std::fprintf(stderr, "reciprocal sample %d output %d mismatch\n", i, j);
                return 1;
            }
        }
    }
    std::puts("CUDA reciprocal primal/JVP/VJP: 85 long-double/IEEE checks passed");
}

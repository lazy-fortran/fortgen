#include <cuda_runtime.h>
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <stdexcept>
#include <vector>
#include "batch_real64.cuh"
#include "batch_real32.cuh"
#include "batch_mixed.cuh"

static void check(cudaError_t rc) {
    if (rc != cudaSuccess) throw std::runtime_error(cudaGetErrorString(rc));
}

template<class Input, class Output, int precision>
static void run(std::size_t n) {
    // Tail sentinels also test a zero-length batch and partial final blocks.
    std::vector<Input> x(std::max<std::size_t>(5*n, 1));
    std::vector<Output> y(4*n + 16, Output(-987));
    for (std::size_t i=0; i<n; ++i) {
        x[i] = Input(0.25 + (i%97)/128.0);
        x[n+i] = Input(-0.125 + (i%53)/64.0);
        x[2*n+i] = Input(0.125);
        x[3*n+i] = Input(-0.25);
        x[4*n+i] = Input(0.5);
    }
    Input* device_x;
    Output* device_y;
    check(cudaMalloc(&device_x, x.size()*sizeof(Input)));
    check(cudaMalloc(&device_y, y.size()*sizeof(Output)));
    check(cudaMemcpy(device_x, x.data(), x.size()*sizeof(Input), cudaMemcpyHostToDevice));
    check(cudaMemcpy(device_y, y.data(), y.size()*sizeof(Output), cudaMemcpyHostToDevice));
    auto blocks = unsigned((n+127)/128 + (n == 0));
    if constexpr (precision == 0)
        batch_real64_batch<<<blocks,128>>>(n, device_x, device_y);
    if constexpr (precision == 1)
        batch_real32_batch<<<blocks,128>>>(n, device_x, device_y);
    if constexpr (precision == 2)
        batch_mixed_batch<<<blocks,128>>>(n, device_x, device_y);
    check(cudaGetLastError());
    check(cudaMemcpy(y.data(), device_y, y.size()*sizeof(Output), cudaMemcpyDeviceToHost));
    for (std::size_t i=0; i<n; ++i) {
        const long double a=x[i], b=x[n+i], da=x[2*n+i], db=x[3*n+i], seed=x[4*n+i];
        const long double reference[4] = {a*(a+b), (2*a+b)*da+a*db,
                                         (2*a+b)*seed, a*seed};
        for (std::size_t j=0; j<4; ++j) {
            const double tolerance = precision == 0 ? 2e-14 : 2e-6;
            if (std::abs(y[j*n+i]-reference[j]) > tolerance*(1+std::abs(reference[j])))
                throw std::runtime_error("CUDA batch primal/JVP/VJP oracle failed");
        }
    }
    for (std::size_t i=4*n; i<y.size(); ++i)
        if (y[i] != Output(-987)) throw std::runtime_error("CUDA batch tail overwrite");
    check(cudaFree(device_x));
    check(cudaFree(device_y));
}

int main() {
    for (std::size_t n : {0, 1, 257, 4099}) {
        run<double,double,0>(n);
        run<float,float,1>(n);
        run<float,double,2>(n);
    }
    std::puts("CUDA SoA batch primal/JVP/VJP: PASS (fp64, fp32, mixed)");
}

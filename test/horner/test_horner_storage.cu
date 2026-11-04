#include <cuda_runtime.h>
#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <initializer_list>
#include "horner_storage.cuh"

static void checked(cudaError_t status) {
    if (status != cudaSuccess) {
        std::fprintf(stderr, "%s\n", cudaGetErrorString(status));
        std::exit(1);
    }
}

__global__ void evaluate(const double* c, const float* low, const double* x,
                         double* out) {
    horner_storage64(c, x[0], x[1], x[2], out + 1, out + 4);
    horner_storage32(low, x[0], x[1], x[2], out + 15, out + 18);
}

int main() {
    const double h[3] = {0.0005, 0.2, 8.0}, sentinel = -7.25e300;
    double c[648], x[3], out[28], *dc, *dx, *dout;
    float low[648], *dlow;
    checked(cudaMalloc(&dc, sizeof(c)));
    checked(cudaMalloc(&dlow, sizeof(low)));
    checked(cudaMalloc(&dx, sizeof(x)));
    checked(cudaMalloc(&dout, sizeof(out)));
    for (int trial = 0; trial <= 12; ++trial) {
        for (int k = 0; k < 6; ++k)
            for (int j = 0; j < 6; ++j)
                for (int i = 0; i < 6; ++i)
                    for (int q = 0; q < 3; ++q) {
                        const int idx = q + 3*(i + 6*(j + 6*k));
                        c[idx] = trial == 0 ? 0.0 :
                            (((17*i + 11*j + 7*k + q + 1)%29) - 14.1)/
                            (31*std::pow(h[0], i)*std::pow(h[1], j)*std::pow(h[2], k));
                        low[idx] = static_cast<float>(c[idx]);
                    }
        x[0] = h[0]*trial/13; x[1] = h[1]*0.71; x[2] = -h[2]*0.23;
        if (trial == 12) for (int d = 0; d < 3; ++d) x[d] = h[d];
        for (double &v : out) v = sentinel;
        checked(cudaMemcpy(dc, c, sizeof(c), cudaMemcpyHostToDevice));
        checked(cudaMemcpy(dlow, low, sizeof(low), cudaMemcpyHostToDevice));
        checked(cudaMemcpy(dx, x, sizeof(x), cudaMemcpyHostToDevice));
        checked(cudaMemcpy(dout, out, sizeof(out), cudaMemcpyHostToDevice));
        evaluate<<<1, 1>>>(dc, dlow, dx, dout);
        checked(cudaGetLastError());
        checked(cudaMemcpy(out, dout, sizeof(out), cudaMemcpyDeviceToHost));
        for (int offset : {0, 14}) {
            if (out[offset] != sentinel || out[offset + 13] != sentinel) return 2;
            for (int q = 0; q < 3; ++q)
                for (int axis = 0; axis <= 3; ++axis) {
                    long double ref = 0, mag = 0;
                    for (int k = 0; k < 6; ++k)
                        for (int j = 0; j < 6; ++j)
                            for (int i = 0; i < 6; ++i) {
                                int ex[3] = {i, j, k};
                                const int idx = q + 3*(i + 6*(j + 6*k));
                                long double t = offset ? low[idx] : c[idx];
                                if (axis) {
                                    if (!ex[axis - 1]) continue;
                                    t *= ex[axis - 1]--;
                                }
                                for (int d = 0; d < 3; ++d)
                                    for (int e = 0; e < ex[d]; ++e) t *= x[d];
                                ref += t; mag += fabsl(t);
                            }
                    const int slot = axis ? 3 + 3*q + axis - 1 : q;
                    const double v = out[offset + 1 + slot];
                    if (!std::isfinite(v) || fabsl(v - ref) >
                        64*DBL_EPSILON*fmaxl(mag, DBL_MIN)) return 3;
                }
        }
    }
    checked(cudaFree(dc)); checked(cudaFree(dlow));
    checked(cudaFree(dx)); checked(cudaFree(dout));
    return 0;
}

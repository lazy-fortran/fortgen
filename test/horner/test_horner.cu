// Device Horner kernels against a host long-double dense monomial oracle.
#include <cuda_runtime.h>
#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include "horner_kernels.cuh"
#include "horner_cases.cuh"

static void checked(cudaError_t status) {
    if (status != cudaSuccess) {
        std::fprintf(stderr, "%s\n", cudaGetErrorString(status));
        std::exit(1);
    }
}

static long double monomial(int k, int a, double x) {
    if (a > k) return 0.0L;
    long double v = 1.0L;
    for (int i = k - a + 1; i <= k; ++i) v *= i;
    for (int i = 0; i < k - a; ++i) v *= x;
    return v;
}

int main() {
    const int nout = 4000;
    unsigned state = 12345u;
    auto uniform = [&state]() {
        state = 1664525u*state + 1013904223u;
        return (state >> 8)*(1.0/16777216.0);
    };
    int failures = 0, order_base = 0;
    for (int id = 1; id <= horner_ncase; ++id) {
        const int *t = horner_table + 5*(id - 1);
        const int d = t[0], n[3] = {t[1], t[2], t[3]};
        const int nq = t[4] == 0 ? 3 : t[4];
        const int norders = horner_norders[id - 1];
        const int *orders = horner_orders + 3*order_base;
        order_base += norders;
        const int n2 = d >= 2 ? n[1] : 0, n3 = d >= 3 ? n[2] : 0;
        const int ncoeff = nq*(n[0] + 1)*(n2 + 1)*(n3 + 1);
        std::vector<double> c(ncoeff), out(nout);
        double x[3];
        double *dc, *dx, *dout;
        checked(cudaMalloc(&dc, ncoeff*sizeof(double)));
        checked(cudaMalloc(&dx, 3*sizeof(double)));
        checked(cudaMalloc(&dout, nout*sizeof(double)));
        for (int trial = 0; trial < 25; ++trial) {
            for (double &v : c) v = 2*uniform() - 1;
            for (double &v : x) v = 1.5*uniform() - 0.25;
            checked(cudaMemcpy(dc, c.data(), ncoeff*sizeof(double),
                               cudaMemcpyHostToDevice));
            checked(cudaMemcpy(dx, x, 3*sizeof(double), cudaMemcpyHostToDevice));
            checked(cudaMemset(dout, 0, nout*sizeof(double)));
            horner_run_case<<<1, 1>>>(id, nq, dc, dx, dout);
            checked(cudaGetLastError());
            checked(cudaMemcpy(out.data(), dout, nout*sizeof(double),
                               cudaMemcpyDeviceToHost));
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
                        const double actual = out[offset*nq + slot + nm*iq];
                        const long double tol = 64.0L*DBL_EPSILON*fmaxl(mag, DBL_MIN);
                        if (!std::isfinite(actual) || fabsl(actual - ref) > tol) {
                            std::fprintf(stderr, "case %d derivative %d%d%d quantity %d:"
                                         " %.17g vs %.17Lg\n", id, a[0], a[1], a[2],
                                         iq, actual, ref);
                            ++failures;
                        }
                    }
                    ++slot;
                }
                offset += nm;
            }
        }
        checked(cudaFree(dout));
        checked(cudaFree(dx));
        checked(cudaFree(dc));
    }
    if (failures > 0) {
        std::fprintf(stderr, "%d Horner device outputs disagree\n", failures);
        return 1;
    }
    std::printf("all %d Horner device cases agree\n", horner_ncase);
    return 0;
}

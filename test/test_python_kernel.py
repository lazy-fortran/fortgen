"""Execute emitted Python kernels and compare them with direct formulas.

usage: test_python_kernel.py FORTGEN_CODEGEN python_kernel.fgir
"""
import cmath
import math
import subprocess
import sys

try:
    import numpy
except ModuleNotFoundError:
    print("numpy is not installed")
    sys.exit(77)


def reference(k, n, iota, sigma, z):
    x = k*iota + n
    g = 1j*x/(x**2 + sigma**2)
    h = (2 - z)**5*numpy.exp(-z) + numpy.pi*numpy.arctan2(z, k) \
        + numpy.maximum(numpy.maximum(z, k), n)
    return g, h, x**-2.0, numpy.sqrt(z) + z**7


def main():
    codegen, fixture = sys.argv[1:3]
    source = subprocess.run([codegen, "python", fixture], check=True,
                            capture_output=True, text=True).stdout
    assert "1j" in source and "** 7)" in source, source
    namespace = {}
    exec(compile(source, "python_kernel.py", "exec"), namespace)
    kernel = namespace["python_kernel"]

    scalar = (0.75, -1.25, 0.375, 0.5, 1.5)
    got = kernel(*scalar)
    want = reference(*scalar)
    assert isinstance(got, tuple) and len(got) == 4
    assert isinstance(got[0], complex) and got[0].real == 0.0
    for value, expected in zip(got, want):
        assert cmath.isclose(value, complex(expected), rel_tol=1e-14), \
            (value, expected)
    x = scalar[0]*scalar[2] + scalar[1]
    assert math.isclose(got[0].imag, x/(x*x + 0.25), rel_tol=1e-15)

    # Broadcasting: one array input, scalars elsewhere.
    z = numpy.linspace(0.25, 2.0, 7)
    got = kernel(0.75, -1.25, 0.375, 0.5, z)
    want = reference(0.75, -1.25, 0.375, 0.5, z)
    for value, expected in zip(got, want):
        value = numpy.broadcast_to(value, z.shape)
        assert numpy.allclose(value, expected, rtol=1e-14, atol=0.0), \
            (value, expected)
    print("test_python_kernel: all checks passed")


if __name__ == "__main__":
    main()

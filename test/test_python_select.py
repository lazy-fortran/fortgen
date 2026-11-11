"""Execute the emitted Python selection kernel against branching formulas.

usage: test_python_select.py FORTGEN_CODEGEN select_kernel.fgir
"""
import math
import subprocess
import sys

try:
    import numpy
except ModuleNotFoundError:
    print("numpy is not installed")
    sys.exit(77)


def reference(x, y):
    f = math.sin(x)/x if x != 0.0 else 1.0
    g = x*y if x < y else x + y
    h = (x if x <= y else y) + min(x, y, 0.25)
    if x == y:
        h += max(x, y, 0.25)
    return f, g, h


def main():
    codegen, fixture = sys.argv[1:3]
    source = subprocess.run([codegen, "python", fixture], check=True,
                            capture_output=True, text=True).stdout
    assert "numpy.where(" in source, source
    namespace = {}
    exec(compile(source, "select_kernel.py", "exec"), namespace)
    kernel = namespace["select_kernel"]

    x = numpy.arange(-8, 9)/4.0
    y = (6 - numpy.arange(-8, 9))/8.0
    assert 0.0 in x and numpy.any(x == y)
    # The unselected quotient is still evaluated, as in every backend.
    with numpy.errstate(all="raise"):
        arrays = kernel(x, y)
    for i in range(x.size):
        want = reference(float(x[i]), float(y[i]))
        scalars = kernel(float(x[i]), float(y[i]))
        for j in range(3):
            assert math.isclose(arrays[j][i], want[j], rel_tol=1e-14), (i, j)
            assert math.isclose(float(scalars[j]), want[j], rel_tol=1e-14), (i, j)
    print("test_python_select: all checks passed")


if __name__ == "__main__":
    main()

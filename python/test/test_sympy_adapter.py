import sympy as sp
from fortgen import kernel
import math
import shutil
import subprocess
import pytest


def test_sympy_ir_is_stable_and_codegen_round_trips():
    x, y = sp.symbols("x y", real=True)
    k = kernel({"f": sp.sin(x + y) + (x + y) ** 2},
               args=[x, y], name="sympy_demo")
    text = k.to_ir()
    assert text.startswith("fortgen.kernel_ir.v1\n")
    assert "kernel sympy_demo" in text
    assert "function sin" in text
    # Structural memoisation gives one shared x+y node used by both consumers.
    add_lines = [line for line in text.splitlines() if " add " in line]
    assert len(add_lines) >= 2  # root addition plus shared x+y
    source = k.emit_fortran()
    assert "subroutine sympy_demo" in source
    assert "sin(" in source
    cuda = k.emit_cuda()
    assert "__device__" in cuda


def test_sympy_emission_compiles_and_matches_independent_values(tmp_path):
    compiler = shutil.which("gfortran")
    if compiler is None:
        pytest.skip("gfortran unavailable for compiled frontend oracle")
    x, y = sp.symbols("x y", real=True)
    expression = sp.sin(x*y)/(1 + x**2)
    source = kernel({"f": expression}, args=[x, y], name="frontend_fixture")
    leaf = tmp_path / "leaf.f90"
    leaf.write_text(source.emit_fortran())
    driver = tmp_path / "driver.f90"
    driver.write_text('''program driver
use, intrinsic :: iso_fortran_env, only: real64
implicit none
real(real64) :: x, y, f
integer :: i
do i=-8,8
    x=real(i,real64)/4.0_real64
    y=real(3-i,real64)/8.0_real64
    call frontend_fixture(x,y,f)
    write(*,'(3(es25.16,1x))') x,y,f
end do
end program driver
''')
    executable = tmp_path / "fixture"
    subprocess.run([compiler, "-O2", "-fno-fast-math", str(leaf),
                    str(driver), "-o", str(executable)], check=True)
    rows = subprocess.check_output([str(executable)], text=True).splitlines()
    assert len(rows) == 17
    for row in rows:
        xv, yv, actual = map(float, row.split())
        expected = math.sin(xv*yv)/(1 + xv*xv)
        assert math.isclose(actual, expected, rel_tol=2e-14, abs_tol=2e-14)

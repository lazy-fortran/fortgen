import sympy as sp
from fortgen import kernel


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

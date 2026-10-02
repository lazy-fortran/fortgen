# FortGen Python frontend

The Python package is intentionally thin. It accepts ordinary `sympy.Expr`
objects, lowers the supported scalar subset to `fortgen.kernel_ir.v1`, and
invokes the installed `fortgen-codegen` executable. Symbolic algebra remains
SymPy's job; source generation remains the Fortran FortGen backend's job.

```python
import sympy as sp
from fortgen import kernel

x, y = sp.symbols("x y", real=True)
k = kernel({"f": sp.sin(x + y) + (x + y)**2}, name="demo")
print(k.emit_fortran())
```

Set `FORTGEN_CODEGEN` to override the executable command.

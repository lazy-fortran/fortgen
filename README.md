# fortgen

Shared target-neutral kernel IR and source-generation backend for the
lazy-fortran toolchain.

FortGen is the neutral boundary between frontends and generated numerical
leaves:

```text
FortSym expr_t -------\
                       > fortgen.kernel_ir.v1 -> Fortran / CUDA
real SymPy Expr ------/
```

The public scalar IR is specified in
[`spec/kernel-ir-v1.md`](spec/kernel-ir-v1.md). The Fortran API exposes the
same node/operand representation through `fortgen_kernel_ir`; the Python
package lowers ordinary `sympy.Expr` objects to the text interchange and
invokes the same `fortgen-codegen` executable.

Existing shared source-generation utilities remain available:

- `fortgen_buffer`: append-oriented geometric-growth text buffer;
- `fortgen_layout`: token-safe Fortran line continuation;
- `fortgen_banner`: provenance banners.

The scalar emitter additionally owns stable target IDs, precision choices,
identifier collision handling, source-level policies (small-power expansion,
constant folding/division elimination, FMA shaping), and Fortran/CUDA spelling.

## Python / SymPy

```python
import sympy as sp
from fortgen import kernel

x, y = sp.symbols("x y", real=True)
k = kernel({"f": sp.sin(x + y) + (x + y)**2}, name="demo")
print(k.emit_fortran())
```

The adapter is intentionally thin: SymPy owns symbolic mathematics; FortGen
owns source generation.

## Pure Fortran

Fortran callers construct `kernel_ir_t` directly or use a frontend such as
FortSym. No Python or SymPy runtime is required.

## Build

```console
fpm test
fpm install --prefix ~/.local
```

The installed `fortgen-codegen` command accepts:

```console
fortgen-codegen fortran kernel.fgir
fortgen-codegen cuda kernel.fgir
```

## Compatibility

FortAD continues to use the original buffer/layout/banner APIs. FortSym
migration is deliberately done in a separate PR so existing code-generation
outputs can be compared independently before the shared backend is promoted.

## Licence

MIT.

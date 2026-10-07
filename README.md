# fortgen

Target-neutral scalar kernel IR and source generation for lazy-fortran.

[PLAN.md](PLAN.md) records the reviewed work order for extending FortGen to a
neutral expression/computation IR and code-generation boundary. Scalar Kernel IR
and Fortran/CUDA leaf emission are available; exact mathematical Expr IR remains
planned work.
Layer ownership and evidence requirements are in
[docs/principles.md](docs/principles.md).

FortSym lowers symbolic expressions to the shared computation boundary. The
optional ordinary-SymPy frontend feeds the same backend without importing
FortSym. FortAD uses the text utilities; imperative IR lowering remains separate
work. Frontends retain algebra, assumptions and source-language semantics.

## Motivation

This repository was not created on the suspicion that code might be shared. It
was created after finding the same three non-obvious things solved twice, in
the same way, for the same stated reasons:

- **A geometric-growth text buffer.** Both codebases carry a comment explaining
  that building generated code with repeated `s = s // more` is quadratic and
  becomes the dominant cost on a large kernel.
- **Line-limit continuation.** Both break generated statements before the
  column limit, both leave room for the ampersand and indentation, and both
  discovered that a break must never land inside a token — a derivative chain
  or an expanded polynomial passes 132 columns routinely, so this is a
  correctness requirement, not formatting taste.
- **A provenance banner.** Both stamp generated files with what produced them
  and a "do not edit" line, because generated code that cannot be traced back
  to its generator becomes unmaintainable the first time somebody edits it.

Two independent implementations of the same subtle logic is the bar for
extracting a shared abstraction. One would have been a guess.

## Modules

| Module | Provides |
|---|---|
| `fortgen_buffer` | `buffer_t`: append-oriented text accumulation, geometric growth |
| `fortgen_layout` | line-limit continuation that never splits a token, indentation |
| `fortgen_banner` | provenance headers for generated files |
| `fortgen_kernel_ir` | topologically ordered scalar computation DAG |
| `fortgen_kernel_emit` | Fortran and CUDA leaf source emission |
| `fortgen_kernel_target`, `fortgen_precision` | target and precision descriptors |
| `fortgen_ir_text` | versioned scalar computation serialization |

## Current scope boundaries

- [Kernel IR v1](spec/kernel-ir-v1.md) contains binary64 numerical literals.
  It does not preserve exact mathematical expressions, assumptions or histories.
- FortSym's public symbolic/kernel owners remain in FortSym; its adapter
  transfers numerical computation and explicit emission policies.
- No scheduling, launch geometry, residency or application physics is owned
  here. CUDA source emission alone establishes no device execution result.

## Status

Build and run the registered native oracle:

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
```

`fortgen-codegen fortran INPUT.fgir OUTPUT.f90` emits a scalar leaf. The native
compiled oracle compares emitted values against independent real128 formulas.
Optional direct-SymPy examples and requirements are in [python/README.md](python/README.md).
Migration ownership is in [docs/migration.md](docs/migration.md).

## Licence

MIT.

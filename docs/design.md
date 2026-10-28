# fortgen design

## Why a shared library

fortgen was extracted after the same three pieces of non-obvious logic had been
written twice, independently and for the same reasons, in two code generators:

- A text buffer with geometric growth. Building generated code with repeated
  `s = s // more` is quadratic and dominates the cost for large kernels.
- Line continuation. Generated statements are broken before the column limit,
  leaving room for the ampersand and indentation, and a break must never land
  inside a token. Derivative chains and expanded polynomials routinely exceed
  132 columns, so this is a correctness requirement, not formatting.
- A provenance banner. Generated files record what produced them and carry a
  "do not edit" line, so that they can always be traced back to their
  generator.

Two independent implementations of the same subtle logic were taken as the
threshold for a shared abstraction.

## Layers

Frontends keep their own algebra, assumptions and source-language semantics.
FortSym lowers symbolic expressions to the kernel IR; the optional Python
package lowers ordinary SymPy expressions to the same IR without importing
FortSym. FortAD uses the text utilities, while its imperative differentiation
IR stays separate. See [migration.md](migration.md) and
[principles.md](principles.md).

## Scope

- Kernel IR v1 ([spec](../spec/kernel-ir-v1.md)) holds binary64 numerical
  literals. It does not preserve exact mathematical expressions, assumptions or
  derivation history; that is the purpose of the separate exact expression IR
  ([spec](../spec/expr-ir-v1.md)).
- FortSym's public symbolic and kernel interfaces remain in FortSym; its
  adapter passes the numerical computation and explicit emission policies to
  fortgen.
- Scheduling, launch geometry, data residency and application physics are not
  part of fortgen. Emitting CUDA source does not by itself establish a result on
  a device.

## CUDA batch kernels

`emit_cuda_batch_ir(ir, spec, ok, message)` emits the scalar device function
plus `NAME_batch(n, inputs, outputs)`, a CUDA global entry point using packed
structure-of-arrays storage: field `j` starts at offset `j*n` (zero-based).
FP64, FP32, and FP32 compute with FP64 output use the same IR.

Callers own launch dimensions, streams, allocations, reductions and the choice
of precision. A zero-length batch performs no loads or stores. Inputs and
outputs must not overlap. Half-precision formats are CPU storage codecs only;
this entry point does not convert to BF16/FP16 or bound roundoff.

## Test fixtures

The kernels generated for the tests are checked in under `test/`, so that
`fpm test` can link and run the numerical checks without a generation step. The
CMake build regenerates them and compares them with the checked-in copies.
After changing an emitter or fixture, regenerate the copy with
`fortgen-codegen` or the corresponding fixture generator from the CMake build.

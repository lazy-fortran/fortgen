# Migrating generators to FortGen

FortGen now owns two distinct shared layers:

1. the long-standing source-generation utilities (`fortgen_buffer`,
   `fortgen_layout`, `fortgen_banner`);
2. the stable scalar kernel contract (`fortgen.kernel_ir.v1`) and ordinary
   Fortran/CUDA scalar emitter.

## FortAD

FortAD uses the shared buffer/layout/banner utilities. Its differentiation IR
is intentionally not forced through the scalar kernel IR: it represents
imperative source transformation, control flow, taped reverse-mode state, and
other concepts that are not scalar symbolic DAGs. If a future FortAD product
naturally lowers to the scalar kernel IR, that should be added at that
boundary rather than by replacing FortAD's own IR.

## FortSym

The scalar backend migration is implemented in the companion FortSym PR.
FortSym keeps ownership of:

- `expr_t` and its hash-consed symbolic arena;
- CAS engines and multi-engine adjudication;
- symbolic lowering from `expr_t` to its established kernel DAG;
- typed, Taylor, rigorous-enclosure, table, and other expression-aware
  generators.

At the ordinary scalar Fortran/CUDA emission boundary,
`fortsym_fortgen_adapter` converts FortSym's established kernel DAG to
FortGen's shared scalar IR, then calls the same FortGen emitter used by the
direct SymPy frontend. Existing FortSym public types remain source-compatible.

This staged adapter is deliberate. It lets FortSym's existing generated-source
and executable-oracle tests compare the shared backend against the previous
behavior before any redundant FortSym emitter implementation is deleted.

## Direct SymPy

The Python package in `python/` translates ordinary `sympy.Expr` trees
directly to `fortgen.kernel_ir.v1` and invokes `fortgen-codegen`. It owns no
simplification, differentiation, or other CAS algorithms.

Thus the two Python routes are intentionally independent above the shared
backend:

```text
real SymPy Expr --------------------> FortGen IR -> FortGen emitter
fortsym.sympy -> FortSym expr_t ----> FortGen IR -> FortGen emitter
```

Their common subset is suitable for differential testing without maintaining
two code generators.

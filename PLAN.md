# FortGen implementation plan

## Scope

- Shared text utilities, scalar Kernel IR, Fortran/CUDA leaf emission and an
  independent SymPy frontend are integrated from
  [PR #7](https://github.com/lazy-fortran/fortgen/pull/7).
- Expand incrementally toward a target-neutral expression/computation boundary.
  The proposed exact Expr IR is additional reviewed work; it is not supplied
  by the scalar backend's binary64 numerical literals.
- Keep direct SymPy input independent of FortSym, and the native path free of
  Python requirements. Neither frontend owns the neutral contract.

## Work order

| Stage | Issue / existing work | Gate |
|---|---|---|
| H0 | Scalar backend and companion FortSym adapter integrated | Independent compiled numerical oracles passed; final ref cleanup controller-owned |
| K0 | Remaining consumer compatibility [#2](https://github.com/lazy-fortran/fortgen/issues/2) | Fresh generated FortNum consumer gate; retain explicit lowering ownership |
| K1 | Targets complete [#4](https://github.com/lazy-fortran/fortgen/issues/4) | Descriptor/serialization and target decorations verified; device evidence separately scoped |
| L0 | Layering complete [#5](https://github.com/lazy-fortran/fortgen/issues/5) | Useful #6 contracts integrated with unsupported historical claims removed |
| E0 | Exact Expr IR [#8](https://github.com/lazy-fortran/fortgen/issues/8) | Exact arithmetic, assumptions/scope, deterministic serialization, refusal tests |
| E1 | Independent adapters [#9](https://github.com/lazy-fortran/fortgen/issues/9) | E0 plus semantic oracles and explicit numerical lowering |
| S0 | Statement extension [#3](https://github.com/lazy-fortran/fortgen/issues/3) | Actual imperative consumer/rejection evidence before adding nodes |

## Public contracts

- `spec/expr-ir-v1.md`: mathematical expression meaning, exact constants,
  symbols/domains, supported operations and canonical serialization.
- `spec/kernel-ir-v1.md`: numerical computation, ordered scalar dependencies,
  precision/rounding policies and supported backend operations.
- FortSym retains algebra, derivation histories and evidence workflow.
- Optional `python/fortgen` adapters translate ordinary SymPy objects without
  implementing a second simplifier or importing FortSym.
- Fortran native adapters/types implement public contracts; internal object
  representation is not the specification.
- Scheduling, memory residency, launch geometry and application theory remain
  separately owned. FortAD need not enter mathematical Expr IR.
- Migration must preserve tested consumers and record any deliberate semantic
  change. Exact structure equality is distinct from mathematical equivalence.

## Verified status

- Verified source base: `eca530ed783f41e9343493f9d4173cc149a05ec9`.
- [PR #7](https://github.com/lazy-fortran/fortgen/pull/7) is integrated; superseded
  [PR #6](https://github.com/lazy-fortran/fortgen/pull/6) is closed after useful
  layering contracts were retained in [docs/principles.md](docs/principles.md).
- Native CMake/CTest: 2 passed, including 65 compiled real128 oracle samples.
  Optional ordinary-SymPy frontend: 2 passed, including 17 compiled samples.
- Four serialized Fortran target profiles passed independent host-compiled
  checks. Host compilation alone does not establish OpenMP/OpenACC offload.
- Opt-in [CUDA device gate](test/device/README.md): 3 CTest tests passed with
  CUDA 13.4, GCC 14 and an RTX 5060 Ti, including 17 finite device samples.
  CUDA Max/Min remain explicit refusals.
- Issues [#1](https://github.com/lazy-fortran/fortgen/issues/1),
  [#4](https://github.com/lazy-fortran/fortgen/issues/4) and
  [#5](https://github.com/lazy-fortran/fortgen/issues/5) are closed with recorded
  dispositions. Open issues [#2](https://github.com/lazy-fortran/fortgen/issues/2),
  [#3](https://github.com/lazy-fortran/fortgen/issues/3),
  [#8](https://github.com/lazy-fortran/fortgen/issues/8) and
  [#9](https://github.com/lazy-fortran/fortgen/issues/9) retain unfinished work.
- Exact Expr IR and full derivation replay remain planned. Obsolete branch
  and worktree removal belongs to the integration controller.

## Artifacts

- Upload new plots and rendered writeup PDFs to slopbox; never commit generated plots/PDFs. Retain scripts, inputs, numerical data, hashes and artifact URLs for reproduction.

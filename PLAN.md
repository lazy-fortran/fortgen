# FortGen implementation plan

## Scope

- Main currently owns shared source text utilities.
- [PR #7](https://github.com/lazy-fortran/fortgen/pull/7) proposes the shared
  scalar Kernel IR, Fortran/CUDA emission and independent SymPy frontend.
- Expand incrementally toward a target-neutral expression/computation boundary.
  The proposed exact Expr IR is additional reviewed work; it is not supplied
  by PR #7's binary64 numerical literals.
- Keep direct SymPy input independent of FortSym, and the native path free of
  Python requirements. Neither frontend owns the neutral contract.

## Work order

| Stage | Issue / existing work | Gate |
|---|---|---|
| H0 | Review #6/#7 and companion FortSym #77 | Reconcile current main; independent compiled numerical oracles |
| K0 | Shared backend [#2](https://github.com/lazy-fortran/fortgen/issues/2) | Preserve consumer compatibility and explicit lowering ownership |
| K1 | Targets [#4](https://github.com/lazy-fortran/fortgen/issues/4) | Verify implemented descriptor/serialization and target decoration |
| L0 | Layering [#5](https://github.com/lazy-fortran/fortgen/issues/5) | Revise #6 unsupported historical performance/rewrite claims |
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

## Hygiene status

- Inspected main: `ea422bb282ba26dd39341c3f96f727d5892a8ede`.
- Open PR #7 at `02a63c3c432fe660e4bdf0c555da403870b36dd3` contains useful code;
  historical CI passed. Fresh validation and current integration remain open.
- Open PR #6 at `a118158227571c0397ebe23990f86e996e092877` contains useful layering
  documentation. [docs/principles.md](docs/principles.md) retains its useful
  contracts with unsupported historical claims removed; promote that revision
  before closing the superseded PR and issue #5.
- Both remote feature branches remain protected until useful content is on
  main or an explicit evidence-based disposition is recorded.
- [#1](https://github.com/lazy-fortran/fortgen/issues/1) is a tracker; PLAN owns
  order and atomic issues own remaining work. Close the tracker after its
  references/disposition are updated, without claiming unfinished work done.
- Existing issues #2–5 stay open until their acceptance criteria are checked
  against integrated main. This inventory is not completed cleanup.

## Artifacts

- Upload new plots and rendered writeup PDFs to slopbox; never commit generated plots/PDFs. Retain scripts, inputs, numerical data, hashes and artifact URLs for reproduction.

# The layering principle and the measurement contract

The design across fortsym, fortnum, fortgen, and fortad rests on a small number
of principles. They are written here, where the shared layer lives, so a
decision can be checked against them rather than re-argued.

This is a short document. No new mechanism is introduced.

## The governing principle

> **Optimise at the level that holds the most semantic information, and hand
> each lower level a form it can improve but never has to reconstruct.**

The concrete consequence, and the reason the whole stack is shaped this way: a
compiler cannot reassociate floating point without `-ffast-math`, because it is
transforming a float program and must preserve its semantics. fortsym starts
from the *exact expression* and merely chooses which float program to emit.
**That yields fast-math-class rewrites, with justification, while compiling at
`-O3` without fast-math.** No flag has to be trusted.

The same reasoning explains fortad's measured position against Enzyme. Enzyme
argues AD belongs on optimised LLVM IR
([arXiv 2010.01709](https://arxiv.org/abs/2010.01709)); optimised IR has already
destroyed algebraic identity, exact zeros, and structural sparsity.
fortad-bench's record -- 5 of 5 on Enzyme's own historical suite, worst ratios
1.69x and 1.63x across 59 downstream operators, explicitly not a whole-suite
claim -- shows the claim's generality does not hold. Cite it as *measured*, not
as a victory claim.

## Layer ownership

| Layer | Owns | Must not |
|---|---|---|
| fortsym expression graph | algebraic form, association, exact zeros, sparsity | know about targets |
| kernel IR (here) | operation structure, CSE level, rematerialisation | contain dialect branches |
| emitter (here) | spelling, decoration, target descriptor | know about launch geometry, residency, grids |
| compiler | register allocation, instruction selection, scheduling | be fought |
| harness (hand-written, per target) | loops, memory layout, launch configuration | be generated, for now |

The one contested zone is **temporaries**: a CSE decision changes register
pressure the compiler must then absorb, and it can go either way. That is why it
is a measured knob
([lazy-fortran/fortsym#64](https://github.com/lazy-fortran/fortsym/issues/64))
rather than a fixed choice.

## The measurement contract

Four counts, three gaps, each attributable to exactly one layer:

| Count | Meaning | Gap below it |
|---|---|---|
| `N_sym` | arithmetic the mathematics requires | `→ N_emit` = the generator's doing |
| `N_emit` | arithmetic the generator wrote | `→ N_machine` = the compiler's doing |
| `N_machine` | arithmetic the compiler produced | `→ T_meas` = the hardware's doing |
| `T_meas` | what the hardware delivered | |

**`N_sym` is the count of a particular algebraic form, not a proven lower
bound.** It measures distance from the best form we know. Say so wherever it is
reported; a number presented as absolute optimality would be wrong and would
eventually be caught being wrong.

An unmeasured level is **absent**, never `"skipped"` and never zero.

## Deliberately excluded, and why

- **Harness generation.** Loops, memory layout, launch geometry. This is where
  Kokkos and Halide live, and the measurement track exists to decide whether
  entering it is ever justified. Do not pre-empt that with an assumption.
- **A schedule language.** Halide's other half; where TVM and Exo spend most of
  their complexity.
- **Equality saturation for speed.** DAG-cost extraction is NP-hard by
  reduction from minimal set cover, and these expressions are small enough that
  exhaustive variant enumeration wins. Worth revisiting only for *accuracy*
  rewriting ([Herbie](https://herbie.uwplse.org/)), which optimises an objective
  no compiler touches.
- **ML cost models.** The variant space is dozens of points, not the
  astronomical schedule spaces that force Ansor and AutoTVM into ~1000 trials
  per case.
- **Contract and annotation languages.** A specification that must be authored
  in a new notation fails the test that governs everything here: **only pursue
  checks whose specification is a byproduct of work already being done.**
  Readback needs no specification because the expression already exists. Nothing
  in this stack introduces a notation anyone must learn.
- **Classical PGO** (`-fprofile-use`). Straight-line kernels have no branches.
  Not to be confused with the variant sweep in
  [lazy-fortran/fortnum#81](https://github.com/lazy-fortran/fortnum/issues/81).

## Prior art the design follows

- **BLIS** -- a tiny architecture-specific microkernel, everything else
  portable, blocking from an analytical model rather than search. The closest
  structural analogue to leaf-plus-harness. Simpler than OpenBLAS's per-
  microarchitecture assembly, and generally a little slower; that trade is the
  right one here.
- **Devito** and **Firedrake/TSFC** -- symbolic on top, a separate loop-level
  optimiser below. The layering this stack already matches.
- **SymPy codegen** -- `cse()` not applied automatically, `pow(x,2)` printing.
  What not to inherit.

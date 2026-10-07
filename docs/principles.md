# Layer ownership and measurement

## Ownership

- Optimize where the required mathematical or computational information is
  available. Lower layers do not reconstruct lost assumptions or exactness.
- Exact-expression transformations and floating-point program transformations
  have different contracts. Reassociation or FMA shaping requires an explicit
  arithmetic policy and independent accuracy evidence.
- Shared backend code serves independent producers. Keep frontend algebra and
  source-language interpretation at their owning repositories.
- Generated source retains its generator, input/specification identity and
  provenance. Readback and compiled behavioral checks detect drift.

| Layer | Owns |
|---|---|
| Frontend | Mathematical algebra or imperative source semantics |
| Exact Expr IR | Public expression meaning, exact literals, symbols and domains |
| Kernel IR and lowering | Numerical computation, dependencies and declared precision/rounding policy |
| Emitter | Target spelling and leaf decoration |
| Compiler | Instruction selection, register allocation and machine scheduling |
| Consumer harness | Execution loops, layout, launch configuration and residency |

- FortSym owns mathematical derivation history, assumptions and evidence.
- Direct ordinary-SymPy input to FortGen does not import FortSym.
- FortAD may lower an imperative computation without using mathematical Expr IR.
- Execution schedules and application physics remain separately owned.
- CSE and rematerialization change temporary lifetimes and repeated work; choose
  their policies using measured performance and retained correctness evidence.

## Measurement

| Quantity | Meaning | Attribution |
|---|---|---|
| `N_sym` | Count for the selected mathematical expression form | Frontend/form selection |
| `N_emit` | Operations in emitted source | Generator/lowering |
| `N_machine` | Operations/instructions in compiled code | Compiler, including FMA and spills |
| `T_meas` | Measured execution cost | Hardware and execution harness |

- `N_sym` is not a proved global minimum.
- Counts are comparable only with stated operation definitions, domains,
  precision and workload. FMA or spills require explicit counting conventions.
- Unmeasured evidence remains absent. Historical numbers require exact
  revisions, commands, machine metadata and workload scope.
- Correctness, latency, throughput, memory use and certified accuracy remain
  separate measurements; a faster kernel must still meet its declared domain
  and error requirement.
- Exact algebra alone does not justify a floating-point accuracy budget.

## Incremental scope

- Text utilities and scalar Kernel IR/backend are implemented. Exact Expr IR
  and broader computation work remain in [PLAN.md](../PLAN.md).
- Statements/control flow need an actual consumer and independent execution
  oracle. A statement extension does not introduce a scheduling language.
- Canonical serialization supports reproducibility and structural comparisons;
  mathematical equivalence needs its own evidence.
- Rigorous emission needs a justified arithmetic/runtime contract; numerical
  samples and arbitrary padding cannot prove an enclosure.

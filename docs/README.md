# FortGen user guide

Build and run the first command-line example in the [README](../README.md).
The [module overview](../README.md#modules) identifies the Fortran interfaces.

Use the [kernel IR specification](../spec/kernel-ir-v1.md) when producing a
serialized kernel, the [typed scalar contract](../spec/typed-scalar-ir.md)
for precision and boolean semantics, and the
[tensor-kernel contract](../spec/tensor-kernels.md) for interpolation and
batch schedules. The [exact expression IR](../spec/expr-ir-v1.md) preserves
exact expression structure before numerical lowering.

The [design guide](design.md) describes emission and CUDA batch interfaces;
[precision](precision.md) describes storage variants. Generator authors can
follow the [migration guide](migration.md) and the
[layer ownership contract](principles.md).
See [status and verification](status.md) for the limits of these interfaces.

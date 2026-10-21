# CPU precision families

`fortgen_precision_emit:emit_cpu_precision_variants(ir, spec, ok, message, compute)`
lowers one scalar Kernel IR into `NAME_fp64`, `NAME_fp32`, `NAME_fp16`, and
`NAME_bf16`. The optional `compute` chooses `PRECISION_REAL32` (default) or
`PRECISION_REAL64` arithmetic for the two half-storage variants. The full-width
variants retain their named arithmetic precision. Existing single-variant
emitters and their defaults are unchanged. This additive CPU API leaves CUDA
emission to its existing backend.

The half interfaces take `integer(int16)` IEEE encodings. They decode inputs,
compute in an explicit Fortran real kind, then round outputs to storage.
`fortgen_storage` supplies pure elemental `pack_fp16`, `unpack_fp16`,
`pack_bf16`, and `unpack_bf16`. Conversion rounds directly from binary64 to
nearest with ties to even, preserving signed zero, infinities and quiet NaNs.
Finite overflow becomes infinity; callers must scale values before a half
boundary when needed. A scalar kernel has no implicit physical scale.

These are portable storage formats, not native Fortran real kinds. There is
no claim of native half or BF16 arithmetic or automatic SIMD speedup. CPU
arithmetic is fp32/fp64 on every host. Generated half sources require only
FortGen's small runtime, not a symbolic library. `STORAGE_FP64`, `STORAGE_FP32`,
`STORAGE_FP16`, and `STORAGE_BF16` are stable storage identities; the legacy
`PRECISION_MIXED` remains an emission choice rather than a storage format.

The family API requires CPU/default target, scalar valid Fortran names, and
names leaving room for the five-character suffix. Runtime codec/kind names
are reserved at this boundary; an unsupported request returns `ok=.false.`
and no partial source. Generator and revision provenance from `spec` is carried
by every compute leaf; half wrappers declare storage, compute and rounding.

`precision_storage` exhausts the encoded space and checks independent quantum
bounds, ties, overflow and subnormals. `precision_kernel` compiles regenerated
sources and compares values against an independent polynomial, with tolerances
from input conditioning and unit roundoff. Generated fixtures are byte-checked.

CMake numeric consumers can fetch `SOURCE_SUBDIR runtime` and link
`fortgen_runtime` (`fortgen::runtime`). That static target contains only format
identities and conversion routines; it builds no IR or generator executable.
The full FortGen target publicly links the same runtime.

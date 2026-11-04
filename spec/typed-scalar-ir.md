# Typed scalar kernel IR

The scalar DAG supports explicit `TYPE_REAL32`, `TYPE_REAL64`, `TYPE_UINT32`,
`TYPE_UINT64` and `TYPE_BOOL` nodes for mixed scalar arithmetic. Untyped callers retain
`TYPE_DEFAULT` and the existing precision policy. A typed graph specifies every
node type and both `kernel_emit_spec_t%arg_types` and `%output_types`; these
arrays match the argument and output names in order. A symbol's type must match
its argument, and each output type must match its root. There are no implicit
arithmetic conversions.

Unsigned literal nodes use `%uint_hi` and `%uint_lo`, nonnegative `int64` digits
in base 2**32. Each digit is at most 4294967295; uint32 literals require a zero
high digit. They never pass through `%value` or floating-point arithmetic.
For example, 2**53+1 uses high=2097152, low=1, while the largest uint64 uses
high=low=4294967295. Real literals use `%value`; real32 values round to binary32
in the emitted literal. Boolean literals use `%bool_value`.

| Operation | Operands and result |
|---|---|
| `IR_ADD`, `IR_MUL`, `IR_SUB` | Two matching real32, real64 or unsigned operands, same result type |
| `IR_DIV` | Two matching real32 or real64 operands, same result type |
| `IR_BITAND`, `IR_BITOR`, `IR_MULHI` | Two matching unsigned operands, same result type |
| `IR_SHL`, `IR_SHR` | One unsigned operand; `%shift` is a literal count from zero to width minus one |
| `IR_LT`, `IR_LE`, `IR_EQ`, `IR_NE`, `IR_GE`, `IR_GT` | Matching operands, boolean result; boolean operands allow only equality and inequality |
| `IR_AND`, `IR_OR`, `IR_NOT` | Boolean operands and result; NOT has one operand |
| `IR_SELECT` | Boolean condition, two matching values, result of their type |
| `IR_CAST` | One real or unsigned operand, explicit real or unsigned result |

Unsigned arithmetic wraps modulo 2**width. `IR_MULHI` returns the upper half of
the full unsigned product. Casts between unsigned widths follow C++ unsigned
conversion; real-to-unsigned casts require a finite, in-range input after
truncation. Validation checks types and widths, not dynamic value ranges.
Signed integers, bitcasts, general functions and powers are outside this layer.

CUDA supports all typed scalars. Metal supports real32, unsigned integers and
booleans, and explicitly rejects every graph or argument with native real64.
The existing Fortran emitter supports typed real32, real64 and boolean graphs;
it rejects unsigned scalars, including unused unsigned arguments. Typed Fortran
also rejects case-insensitive identifiers `real`, `merge`, `real32` and `real64`,
which would shadow its conversion/selection intrinsics or imported kind names.
This check covers procedure names, arguments, outputs and generated temporaries;
CUDA retains its own identifier rules. Python,
packed CUDA batches and text IR v1 reject typed graphs explicitly. Text
serialization refuses before opening the destination, so an existing file is preserved. No typed text
round-trip format is provided yet.

Typed graphs require `small_power_limit=0`, `fold_exact_constants=.false.`,
`eliminate_constant_divisions=.false.` and `shape_fma=.false.`. They preserve
nodes and payloads without legacy real64 folding. Consumers must compile with
fast math and floating-point contraction disabled; the emitter cannot set those
compiler options. On Metal, use `-fno-fast-math -ffp-contract=off`. The precision
field remains real32 for Metal; explicit node types determine scalar declarations.

`IR_SELECT` selects values, not regions. Earlier compound nodes are emitted as
eager temporaries, so `SELECT(valid, CAST(t), 0)` does not protect the cast.
Instead, construct `safe_t=SELECT(valid,t,0.0f)` followed by `CAST(safe_t)`.
For a digit below 65536, `valid=(t>=0.0f && t<65536.0f)` rejects NaNs, infinities
and out-of-range inputs before conversion. Return `valid` with the candidate;
a separate integer residual check establishes its certificate.

The native `typed_scalar` test checks rejection boundaries and generates the
same mixed leaf for CUDA and Metal. `typed_host` executes generated CUDA
expressions with host implementations of the multiply-high intrinsics and an
independent unsigned 128-bit arithmetic oracle. Supported host compilers also
run undefined-behavior and float-cast-overflow sanitizers. With
`FORTGEN_BUILD_METAL_TESTING=ON`, `typed_metal` runs the mixed leaf on the GPU,
checks command completion and untouched output guards, and uses the same oracle.
This is a generated digit-product/candidate certificate fixture, not a complete
binary64 implementation or a proof of every possible input.

Explicit real32 and real64 node types determine computation precision,
independently of the legacy `precision` setting or output storage. An operation requires
matching operand types; use `IR_CAST` at every precision boundary. In particular,
widening a real32 product does not recover its lost digits. The `mixed_real`
fixture performs a real32 bulk product, a real64 residual and a real64 correction;
both generated Fortran and host-executed CUDA expressions are checked against
exact integer products of dyadic inputs. Their literals and temporaries retain
the declared precision even when the legacy setting is `PRECISION_REAL32`.

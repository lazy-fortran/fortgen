# FortGen scalar kernel IR v1

`fortgen.kernel_ir.v1` is the stable interchange between symbolic frontends
and FortGen's scalar Fortran, CUDA and Python backends. It deliberately represents a
computation rather than a computer-algebra system.

## In-memory contract

A kernel is a topologically ordered DAG. Each node has:

- an operation: `literal`, `symbol`, `constant`, `add`, `mul`, `pow`,
  or `function`;
- a contiguous operand slice containing 1-based indices of earlier nodes;
- a binary64 literal value when the operation is `literal`;
- a canonical name for symbols, constants, and functions.

The kernel also carries an ordered list of output roots. Frontends own symbolic
algebra and assumptions; FortGen owns only lowering/source generation.

Canonical constants in v1 are `pi` and `e`. Canonical function names use the
FortSym/SymPy-compatible lowercase vocabulary, with `Min`/`Max` retained for
n-ary extrema.

## Text interchange

The text form is whitespace-separated and intentionally trivial to parse:

```text
fortgen.kernel_ir.v1
kernel demo
precision real64
target fortran_cpu
arg x
arg y
out result 7
node 1 symbol x
node 2 symbol y
node 3 add 2 1 2
node 4 literal 2.00000000000000000e+00
node 5 pow 2 3 4
node 6 function sin 1 3
node 7 add 2 5 6
end
```

Node IDs must be contiguous and topological. Symbols and names must not contain
whitespace. The supported target names are `default`, `fortran_cpu`,
`fortran_openmp_target`, `fortran_openacc`,
`fortran_openmp_target+fortran_openacc`, and `cuda`.

The format is versioned. Incompatible semantic changes require a new header.

Constants are `pi` and `e`. The Python backend also accepts the imaginary unit
`i`, emitted as `1j`; the Fortran and CUDA backends refuse it, since their
leaves are real.

## Python backend

`emit_python_kernel_ir` (and `fortgen-codegen python`) emits a self-contained
`def name(args):` function that returns one value, or a tuple in declared
output order. It requires the `default` target and `real64`. Literals keep 17
significant digits, so Python parses them to the same binary64 values. Integer
exponents outside the small-power expansion print as integer `**` operands.
Arithmetic broadcasts over NumPy arrays. Functions and `pi`/`e` use
`numpy` ufuncs (`numpy.arcsin`, `numpy.maximum`, ...), imported inside the
function only when used; functions without a NumPy ufunc (`erf`, `gamma`,
Bessel and elliptic functions) are refused, as are Python keywords and `numpy`
as names. An optional docstring is emitted when it contains no quote,
backslash or control character other than newline. Python scalars follow
Python semantics: for example a negative float base with a noninteger exponent
gives a complex result, where NumPy arrays give NaN.

## Frontend rule

Adapters must be deliberately thin. A SymPy adapter may translate a SymPy AST
into this IR, but it must not silently reimplement simplification or
differentiation. A FortSym adapter similarly lowers an already-established
`expr_t` DAG. This keeps the IR a neutral boundary and makes differential
testing meaningful.

## Small integer powers

The existing `small_power_limit` source policy expands exact literal integer
powers of magnitude at most its limit (default three). Zero disables this
expansion. Negative powers multiply typed reciprocals, so an overflowing
positive denominator power does not erase a representable reciprocal result.
Noninteger exponents, including adjacent binary64 values, and powers outside
the limit retain the target's general power operation.

This is floating-point source shaping, not faithful rounding of a complete
rational expression. In particular, `a * x**(-1)` remains multiply-after-
reciprocal; it is not fused to `a / x`. The intermediate reciprocal can overflow
although a complete quotient would be finite. Consumers retain their arithmetic
range/error contract. Mathematical negative powers require a nonzero base;
IEEE zero/infinity/NaN classifications and signed zeros are checked separately
without making a finite mathematical claim at a pole. Rigorous enclosure
emission has its own runtime contract and is not changed by this policy.

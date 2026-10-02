# FortGen scalar kernel IR v1

`fortgen.kernel_ir.v1` is the stable interchange between symbolic frontends
and FortGen's scalar Fortran/CUDA backend. It deliberately represents a
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

## Frontend rule

Adapters must be deliberately thin. A SymPy adapter may translate a SymPy AST
into this IR, but it must not silently reimplement simplification or
differentiation. A FortSym adapter similarly lowers an already-established
`expr_t` DAG. This keeps the IR a neutral boundary and makes differential
testing meaningful.

# Exact Expr IR v1 (bounded scalar slice)

This mathematical interchange is separate from Kernel IR: it neither rounds to
binary64 nor authorizes floating reassociation, FMA or branch changes. Integers
and rational numerator/denominator atoms are decimal strings. Powers use the
principal complex convention of the pinned SymPy profile; no positive-real
rewrite is implied by serialization. Domain exclusions belong to the derivation
transcript, not to an implicitly simplified expression.

The deterministic line format starts with `fortgen-expr-ir 1`. Subsequent rows
are `index tag 'text' 'scope' fact_bits input_index ...`, with contiguous one-based
indices and backward references. Native `expr_ir_t%add` validates construction;
`read_expr_ir` validates serialized input. Python `fortgen.expr_ir` independently
adapts ordinary SymPy, without importing FortSym. Structural order is insertion
order, not algebraic canonicalization. Duplicate structures are permitted.

Single-quoted ASCII text escapes controls, quote and backslash as `\xHH`.
Names are data, never executable identifiers. No source is evaluated. Scope and
name form symbol identity; conflicting fact masks are refused. Empty scope
uses ordinary symbols; nonempty scopes use injective `@length:scope:name` names at
the SymPy boundary. The native FortSym adapter currently supplies empty (arena-global) scope.
Facts, bit 0 through 10: real, positive, nonnegative, nonzero, integer,
positive-integer, zero, negative, nonpositive, rational, algebraic. Only explicit
positive facts are supported; inference belongs to the CAS. Contradictions are
refused, with semantic consistency additionally checked by the receiving CAS.

Tags: integer, rational (two integer inputs), constant (pi/E/I/oo/zoo/nan),
symbol, add, mul, pow, function, uninterpreted. The explicit uninterpreted tag
preserves undefined functions even when named sin or another builtin. Audited
unary elementary function nodes retain their heads and require one input;
legacy unknown function names remain uninterpreted. Undefined-function attributes
(assumptions or constrained arity) are refused by the ordinary-SymPy adapter. Unsupported nodes, float
atoms, versions, forward references and malformed literals are refused.
Defaults: 10,000 nodes, 128 levels, 64 inputs per node and 65,536 ASCII bytes per
text field. Native constructor rendering returns an explicit status and caps
expanded visits at 10,000 and source at 1 MiB, including shared-DAG expansion.
The Python reader additionally caps total serialized input at 16 MiB. Native
callers own input-buffer size. Exact-to-Kernel-IR lowering is deliberately not
provided by this slice: it requires a separate explicit numerical policy.

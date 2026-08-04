# Migrating an existing generator to fortgen

## fortad

Done. `fortad_emit` uses `fortgen_buffer`, `fortgen_layout`, and
`fortgen_banner`; `fortad_text` is gone.

## fortsym

Not done, and deliberately not forced.

fortsym's emitter works, is tested against a 384-derivation corpus, and is not
currently being touched. Rewriting a working emitter to adopt an identical
implementation buys nothing today and risks a regression in the one place where
a subtle bug is hardest to notice - generated code that compiles but computes
something slightly different.

Take this migration when `fortsym_kernel` is next opened for another reason.

### What maps onto what

| fortsym | fortgen |
|---|---|
| `strbuf_t` in `fortsym_string` | `buffer_t` in `fortgen_buffer` |
| `LINE_LIMIT` and its statement breaker | `put_wrapped` in `fortgen_layout` |
| `append_banner` | `put_banner` in `fortgen_banner` |

### One difference to reconcile first

fortsym breaks lines at a set of operator positions and has a special case
forbidding a break between the two asterisks of `**`. fortgen breaks only at
whitespace, which makes that special case unnecessary - a break can never land
inside any token.

That is a stricter rule, so fortgen will sometimes wrap earlier than fortsym
does. Before migrating, check the committed generated kernels in
`fortsym-bench` for line-count churn and regenerate them in the same commit, so
the diff is reviewable as formatting rather than mixed with a behaviour change.

### What not to move

`kernel_spec_t` stays in fortsym. It carries engine provenance, CSE results,
and OpenMP/OpenACC annotations that no other generator has asked for. Moving it
would make fortgen a home for one project's concepts rather than shared ones.

# fortgen

Shared conventions for the lazy-fortran tools that **generate Fortran source**.

Today that is [fortsym](https://github.com/lazy-fortran/fortsym), which emits
kernels from symbolic expressions, and
[fortad](https://github.com/lazy-fortran/fortad), which emits derivative code
from a differentiation IR. They emit from different representations, so their
expression printers are properly separate. Everything *downstream* of the
expression, though, they had each written independently and identically.

## Why this exists

This repository was not created on the suspicion that code might be shared. It
was created after finding the same three non-obvious things solved twice, in
the same way, for the same stated reasons:

- **A geometric-growth text buffer.** Both codebases carry a comment explaining
  that building generated code with repeated `s = s // more` is quadratic and
  becomes the dominant cost on a large kernel.
- **Line-limit continuation.** Both break generated statements before the
  column limit, both leave room for the ampersand and indentation, and both
  discovered that a break must never land inside a token — a derivative chain
  or an expanded polynomial passes 132 columns routinely, so this is a
  correctness requirement, not formatting taste.
- **A provenance banner.** Both stamp generated files with what produced them
  and a "do not edit" line, because generated code that cannot be traced back
  to its generator becomes unmaintainable the first time somebody edits it.

Two independent implementations of the same subtle logic is the bar for
extracting a shared abstraction. One would have been a guess.

## What is here

| Module | Provides |
|---|---|
| `fortgen_buffer` | `buffer_t`: append-oriented text accumulation, geometric growth |
| `fortgen_layout` | line-limit continuation that never splits a token, indentation |
| `fortgen_banner` | provenance headers for generated files |

## What is deliberately not here

Expression printing. fortsym prints a hash-consed symbolic DAG; fortad prints a
differentiation IR. Forcing those through one interface would produce an
abstraction that fits neither, and the shared part — precedence and
parenthesisation rules — is a dozen lines each.

Nor is there a "generated kernel" type. fortsym's `kernel_spec_t` carries
engine provenance, CSE results, and OpenMP/OpenACC annotations that fortad has
no use for. That belongs to fortsym until something else needs it.

## Principles

The governing principle and the measurement contract that shape this stack are documented in
[docs/principles.md](docs/principles.md), where the shared layer lives, so a
decision can be checked against them rather than re-argued.

## Status

fortad uses it. fortsym has an equivalent implementation in place and is not
being disrupted to adopt this one; the migration path is documented in
[docs/migration.md](docs/migration.md) and should be taken when fortsym's
emitter is next touched for another reason.

## Licence

MIT.

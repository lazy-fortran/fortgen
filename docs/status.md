# Status and verification

FortGen emits Fortran, CUDA and Python from a shared scalar kernel
intermediate representation (IR). The serialized kernel and exact-expression
formats are specified under [spec/](../spec/); other interfaces may change.

The native tests require a compiler with `real128` support. They compile
emitted Fortran and compare it with independent
reference formulas, including higher-precision references where available.
They also check serialization, invalid-input rejection, scalar types,
precision storage and generated fixture reproducibility. The optional Python
frontend has its own SymPy-based tests.

CUDA device tests are opt-in and require a CUDA compiler, an explicit target
architecture and a usable device. Compiling the Fortran emitted for an
OpenMP or OpenACC target on the host does not verify device execution.
CUDA leaf functions require a caller-owned execution wrapper; launch geometry,
memory residency and streams remain the caller's responsibility.

The exact expression IR is bounded by its specified operations and domains.
Structural equality of serialized expressions does not establish mathematical
equivalence. Rigorous enclosure emission needs a separate justified arithmetic
contract; numerical samples alone do not prove an enclosure.

Remaining work is tracked in the
[issue tracker](https://github.com/lazy-fortran/fortgen/issues).

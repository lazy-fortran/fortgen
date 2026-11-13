# fortgen

fortgen is a small Fortran library and command-line tool for generating
numerical source code. It defines a scalar kernel intermediate representation
(IR), a topologically ordered directed acyclic graph, and emits it as Fortran,
CUDA or Python, together with the
text utilities that code generators in the lazy-fortran projects share.

Status: under development; the kernel IR v1 format is versioned, other
interfaces may change.

## Features

- Explicit real32/real64 and boolean scalar DAGs for Fortran and CUDA,
  plus unsigned CUDA/Metal digit helpers ([spec/typed-scalar-ir.md](spec/typed-scalar-ir.md)).
- Scalar kernel IR with a versioned text format
  ([spec/kernel-ir-v1.md](spec/kernel-ir-v1.md)).
- Emitters for Fortran and CUDA leaf functions, CUDA batch kernels and Python
  functions, including branch-free relational selections and n-ary extrema.
- CPU precision variants (FP64, FP32, and FP16/BF16 storage) of one kernel.
- Tensor-product cell schedules around a scalar leaf
  ([spec/tensor-kernels.md](spec/tensor-kernels.md)).
- Fixed-degree tensor-product polynomial evaluation with fused partial
  derivatives for Fortran, OpenACC and CUDA
  ([spec/tensor-kernels.md](spec/tensor-kernels.md#horner-polynomial-kernels)).
- A bounded exact expression IR, separate from the floating-point kernel IR
  ([spec/expr-ir-v1.md](spec/expr-ir-v1.md)).
- Text utilities: an append buffer with geometric growth, line continuation
  that never splits a token, and provenance banners for generated files.
- An optional Python package that lowers ordinary SymPy expressions to the
  kernel IR ([python/README.md](python/README.md)).

## Build

The native tests need a compiler with `real128` support, such as GFortran.
Library-only CMake builds can disable tests with `FORTGEN_BUILD_TESTING=OFF`.

With fpm:

```sh
fpm build
fpm test
fpm install --prefix ~/.local
```

With CMake, which also regenerates the test fixtures and checks them against
the checked-in copies:

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build -j
ctest --test-dir build --output-on-failure
```

CUDA is opt-in; device tests require an explicit `CMAKE_CUDA_ARCHITECTURES`.

## Usage

```sh
fortgen-codegen fortran test/reciprocal_kernel.fgir reciprocal.f90
fortgen-codegen cuda|cuda-batch|python INPUT [OUTPUT]
```

The compiled tests compare emitted kernels against independent quad-precision
reference formulas.

## Modules

| Module | Provides |
|---|---|
| `fortgen_buffer` | `buffer_t`: append-oriented text accumulation |
| `fortgen_layout` | line continuation and indentation |
| `fortgen_banner` | provenance headers for generated files |
| `fortgen_kernel_ir` | scalar computation DAG |
| `fortgen_ir_text` | kernel IR serialisation |
| `fortgen_kernel_emit` | Fortran, CUDA and Python emission |
| `fortgen_kernel_target`, `fortgen_precision` | target and precision descriptors |
| `fortgen_precision_emit`, `fortgen_storage` | CPU precision variants and half-precision codecs |
| `fortgen_tensor_emit` | tensor-product schedules |
| `fortgen_horner_emit` | tensor-product Horner evaluation with fused derivatives |
| `fortgen_lagrange_emit` | tensor-product Lagrange interpolation at a point from nodal values, with gradient |
| `fortgen_derivative_set` | which partial derivatives a kernel emits, shared with FortSym |
| `fortgen_expr_ir` | exact expression IR |

## Documentation

- [docs/design.md](docs/design.md): motivation, scope and CUDA batch interface
- [docs/precision.md](docs/precision.md): CPU precision families
- [docs/principles.md](docs/principles.md): layer ownership and measurement conventions
- [docs/migration.md](docs/migration.md): how FortSym and FortAD use fortgen
- [Status and verification](docs/status.md)
- [User guide](docs/README.md)
- [Open development work](https://github.com/lazy-fortran/fortgen/issues)

## Related projects

- [fortsym](https://github.com/lazy-fortran/fortsym): symbolic frontend that
  lowers expressions to the kernel IR

## Contributing and citation

See [CONTRIBUTING.md](CONTRIBUTING.md) for checks and
[CITATION.cff](CITATION.cff) for citation metadata.

## Licence

MIT, see [LICENSE](LICENSE).

# Tensor-product kernel schedules

`fortgen_tensor_emit` emits a complete cell apply around a scalar Kernel IR v1
leaf. This is a schedule extension, with no change to the scalar interchange
format. No basis, quadrature rule, geometry, or physical operator is implicit.

For dimension `d`, let `T_0` be the tensor product of the supplied interpolation
matrices. `T_a`, for `a=1..d`, replaces direction `a` by its derivative matrix.
The generated apply computes

```
v_h = T_h u                       h = 0..d
f = leaf(v_0, ..., v_d, coefficients)
y = sum_h transpose(T_h) f_h
```

The leaf has `d+1+ncoeff` scalar inputs and `d+1` scalar outputs, in that order.
Output zero is the value/test channel and outputs 1..d are reference gradient
channels. A gradient-only operator sets output zero to zero. Coefficients may
include integration weights. The caller supplies them exactly once. Each
contraction is a sequence of one-dimensional matrix products, costing
O((d+1)*d*p**(d+1)) rather than O(p**(2*d)). Intermediate products are reused
within a channel, but this version does not share partial transforms between
channels.

`tensor_kernel_spec_t` contains `name` (FortGen `str_t`), `dimension` (2 or 3),
`nloc` (2..9), `nq` (`nloc` or `nloc+1`), `batch` (1..256, default 8),
`ncoeff`, `precision` (`PRECISION_REAL32` or `PRECISION_REAL64`), and
`cell_tables` (default false). Invalid extents, precision, identifiers or leaf
arity return empty source, `ok=.false.` and a diagnostic. Scalar IR validation
and supported scalar function rules still apply.

```fortran
source = emit_tensor_fortran(ir, leaf_spec, tensor_spec, ok, message)
source = emit_tensor_cuda(ir, leaf_spec, tensor_spec, ok, message)
```

## CPU ABI and threading

The emitted module is `NAME_module`, exporting `NAME(nc,u,b,db,coef,y)`:

- `0 <= nc <= batch` is the number of active cells in this batch.
- `u(batch,nloc**d)` and `y(batch,nloc**d)` store cells in the leftmost index.
- `b(nq,nloc,d,nt)` and `db(nq,nloc,d,nt)` are per-direction 1D matrices.
  `nt=1` for shared tables and `nt=nc` for cell tables. The latter allows
  non-uniform spline cells, without prescribing their basis construction.
- `coef(batch,nq**d,ncoeff)` follows the leaf's coefficient input order.
- Tensor flattening has direction 1 fastest. Only active cells are written.
  Inputs and outputs must not alias.

All contraction and copy loops carry `!$omp simd` across cells. The scalar leaf
is private in the same module, allowing inlining. The caller owns OpenMP over
batches and gather/scatter. Compile emitted products with OpenMP and optimization.
Scratch extents are constant, with no allocation in the apply. Explicit
recursion makes scratch private to each call even without compiler recursion
flags. For the largest binary64 case scratch is about 640 KiB per active call.

Binary32 contractions use compensated products and sums, carrying a binary32
low part between stages. The CPU splits IEEE binary32 significands into 12-bit
parts with integer masks, avoiding splitter overflow; CUDA uses native `fmaf`
product residuals. All floating arithmetic and correction storage remain binary32. Binary64 uses a balanced reduction tree. Do not enable
reassociation/fast-math if relying on these roundoff properties. They are not
certified error bounds, and cancellation still affects relative accuracy.

## CUDA ABI and launch

CUDA emission uses the same neutral scalar IR and `emit_cuda_device_ir`. The
export is `extern "C" __global__ void NAME(int nc, const T* u, const T* b,
const T* db, const T* coef, T* y)` with `T=float` or `double`.

Launch a one-dimensional grid of `nc` blocks and one-dimensional blocks of
`round_up(max(nloc,nq)**d,32)` threads (at most 1024 for this size range).
Each block owns a cell, threads own tensor indices, and the 1D matrices and
intermediate values live in static shared memory. The largest binary64 kernel
uses 44320 bytes of shared memory. The device must support these resources.
An undersized/multidimensional block returns without writing output.

CUDA arrays are cell-major: `u[cell*ndof+i]`, `y[cell*ndof+i]`,
`coef[(cell*ncoeff+k)*nquad+q]`. Tables retain Fortran ordering within each
cell: `b[q+nq*(i+nloc*(axis+d*table_cell))]`, where `table_cell=0` for
shared tables, and otherwise the cell index. All indices here are zero based.
The CPU and CUDA packing differ deliberately to match their parallel axes.

## Oracles and measurements

CMake builds `test_tensor_32` and `test_tensor_64`: 64 cases per precision
cover dimensions 2/3, degrees 1..8, quadrature increments 0/1 and both table
layouts. Independent dense Kronecker products check interpolation, all reference
gradients, a fully coupled coefficient leaf and all transpose channels. CPU
also checks empty/partial batches, followed by all eight active cells. The error is the maximum per-cell relative
infinity norm, limited to 1e-14/1e-6. CPU binary32 reference accumulation is
binary64, CUDA uses host long double. Synthetic tables vary across directions
and cells. They are not basis or geometry validation.

Enable `FORTGEN_BUILD_CUDA_TESTING` for `cuda_tensor_32`/`cuda_tensor_64` and the
existing device oracles. See `test/device/README.md` for compiler/architecture
selection. Both device precisions are tested and reported separately.

The CPU executables accept `--benchmark`, timing caller-parallel batches for
3D, shared tables, `nq=nloc`, all degrees. The CUDA executables accept the same
option for a short 256-cell event-timed apply run. They first verify each timed
case against the dense oracle and check all timed outputs afterwards. CPU
records a checksum, CUDA synchronizes events.
Timings exclude allocation, packing, transfer and dense-reference work.
CSV rows give backend, bits, degree, threads, DOFs, repetitions, relative error,
seconds, nominal GFLOP/s and DOF/s (CPU appends a checksum). Nominal FLOPs count
multiply-add as two and omit compensation overhead, address arithmetic, copies
and synchronization. These are apply throughput measurements, not solver rates.
Use `/usr/bin/time -v` (Linux) or `/usr/bin/time -l` (macOS) to retain wall time
and peak RSS beside each command, compiler flags and source revision.

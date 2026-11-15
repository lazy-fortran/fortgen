# Backends

fortgen emits one kernel specification through several backends and uses,
for each kind of kernel and each target, the backend that measures fastest.
No backend is a default for everything, and a slower backend is kept only
where it does a job the fastest one cannot.

| Kind of kernel | Backend | Why |
|---|---|---|
| Fixed-structure hot loops: GEMM register tiles, later small fixed-size matrix operations, FFT butterflies, batched spline and stencil evaluation | Assembly per instruction set, in a C translation unit as a top-level `asm` block | Fastest or tied on every CPU measured (below); the emitter fixes FMA use and operation order, so results do not depend on compiler or flags |
| Pointwise scalar kernels called inside a caller's loop | Fortran | The compiler inlines them, folds constants and vectorises across the caller's loop; an assembly function is opaque and costs a call per point. Batched assembly variants are to be measured against this |
| Quad precision and directed-rounding (verified) kernels | Fortran | The compiler's soft-float and rounding control |
| GPU | CUDA and Metal today; planned: PTX for NVIDIA, GCN/RDNA/CDNA assembly for AMD, AIR for Apple | The lowest level each vendor accepts: PTX and AIR are compiled by the driver for the GPU at hand (no nvcc or Metal compiler at build time), AMD assembly is final machine code. Apple's GPU instruction set is not public and macOS loads only Metal source or AIR |

The assembly of the tile kernels is written in the syntax that both the GNU
assembler and LLVM's integrated assembler accept: AT&T syntax on x86_64, the
standard syntax on aarch64, only the directives `.text`, `.p2align` and
`.globl`, local labels through a macro (`.L` on ELF, `L` on Mach-O) and the
Mach-O symbol underscore through another. Every C compiler our builds use (gcc,
clang, emscripten, icx, nvc) compiles it; an architecture without an emitter
is a build error rather than a slow fallback.

## Tile kernels

`fortgen_tile_kernel` emits `acc = A B^T` for packed panels `A(mr, k)` and
`B(nr, k)` with C prototype

```c
void name(int64_t k, const double *a, const double *b, double *acc);
```

for x86_64 with AVX2 and FMA, x86_64 with AVX-512 and aarch64 with NEON. The
tile stays in vector registers; A is loaded one vector per instruction, B is
broadcast once per element and reused for every A vector, and k is unrolled.
`fortgen-tile ISA MR NR K_UNROLL PREFETCH NAME OUTPUT` writes one kernel.

Measured on 14 November 2026: one core, panels in L1 (k = 256), best
variant of each route, all checked against a direct sum. Assembly from this
emitter, the same kernel as LLVM IR with explicit vectors compiled by clang,
and as Fortran compiled by gfortran, in GFLOP/s:

| CPU | tile | assembly | LLVM IR | Fortran |
|---|---|---|---|---|
| AMD Ryzen 9 5950X (Zen 3), AVX2 | 8x6 | 65.6 | 64.6 | 63.5 |
| Intel Xeon E5-2630 v4 (Broadwell), AVX2 | 8x6 | 43.7 | 43.6 | 42.0 |
| AMD EPYC 9455 (Zen 5), AVX-512 | 24x8 | 121.6 | 120.2 | 115.9 |
| AMD EPYC 9455 (Zen 5), AVX-512 | 16x14 | 118.8 | 117.5 | 53.8 |
| Apple M3 Ultra, NEON | 8x6 | 60.5 | 58.2 | 35.5 |

Two lessons are built into the emitter. On AVX-512, broadcasting B from
memory inside every FMA made a 24x8 tile load-bound (105 GFLOP/s), so taller
tiles broadcast into a register once per element of B. On aarch64 the
procedure call standard keeps the low halves of v8 to v15, which the kernel
saves and restores.

### Apple AMX

`aarch64_amx` targets the matrix coprocessor of Apple M1 to M3, which Apple
does not document and reaches only through its own BLAS (Accelerate). The
encodings follow [corsix/amx](https://github.com/corsix/amx); the emitter
writes them as `.word` directives, so any assembler accepts them. One fp64
outer-product instruction adds the 8 x 8 product of two 64-byte registers to
one of eight accumulators in the coprocessor, so a tile holds up to 64 fp64
values in each of (mr/8)(nr/8) <= 8 accumulators; 32x16 fills all of them.
C is loaded into the accumulators, updated and stored back, two rows per
instruction. A and B are loaded two registers per instruction and need no
alignment, so `a_strided` reads columns of A in place. Rows of B are not
contiguous in a column-major matrix, so `b_strided` is not offered. Each call
enables and disables the coprocessor at no measurable cost. The kernel builds
every operand before its loop: the load operands sit in the high bits of the
pointers (bits 56 to 62, which addressing ignores), the outer-product operands
in registers.

Measured on 14 November 2026 on one core of an Apple M3 Ultra, C added in
place with leading dimension 37, in GFLOP/s:

| tile | k unroll | k = 64 | k = 512 |
|---|---|---|---|
| 32x16 | 1 | 88 | 351 |
| 32x16 | 2 | 130 | 384 |
| 16x32 | 2 | 82 | 365 |
| 64x8 | 1 | 80 | 367 |

The NEON 8x6 tile reaches 60 GFLOP/s on the same core.

### WebAssembly

`wasm32_simd128` emits LLVM's WebAssembly assembly for 128-bit SIMD, for
browsers and node; emscripten's clang assembles it from the same top-level
asm block. WebAssembly has no registers, only locals, so the tile lives in
v128 locals that the engine's compiler maps to registers: 32 on arm64
(JavaScriptCore, which every iPad browser uses, and V8 on Apple silicon),
16 for V8 on x86_64. A product is `f64x2.mul` followed by `f64x2.add`:
fused multiply-add exists only in relaxed SIMD, and JavaScriptCore as released
with macOS 26.6 refuses to compile a module that uses it.
B is broadcast with `v128.load64_splat`, so packed and unpacked B cost the
same; the tile is stored or added to C at the end. Locals start at zero, so
the kernel needs no clearing.

Measured on 15 November 2026 on one core, C added in place with leading
dimension mr and k = 256, in GFLOP/s:

| tile | k unroll | JavaScriptCore, M3 Ultra | node 26 (V8), M3 Ultra | node 22 (V8), Zen 3 |
|---|---|---|---|---|
| 4x4 | 4 | 25.8 | 21.1 | 9.3 |
| 6x4 | 4 | 27.7 | 23.5 | 9.9 |
| 8x4 | 4 | 28.6 | 24.2 | 10.7 |
| 10x4 | 4 | 29.3 | 24.3 | 11.7 |
| 8x6 | 4 | 28.6 | 14.3 | 11.8 |

A multiply and an add per pair of doubles limit an M3 core to about 32
GFLOP/s, so JavaScriptCore runs the 8x4 tile close to that limit.

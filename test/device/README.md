# CUDA scalar leaf oracle

- Native FortGen reads the retained scalar fixture and emits a CUDA leaf.
- The separately authored wrapper launches 17 fixed inputs and compares device
  output with an independent host long-double formula, including finite checks.
- The default native build needs neither CUDA nor a device. Enabling this gate
  requires the CUDA compiler and a usable device; resource failures are failures.
- The CUDA emitter's supported function set remains explicit. This fixture
  uses sine, products and powers; it does not establish Min/Max support.
- No fast-math flags are used. This is a correctness fixture, not a benchmark.

Replay the verified RTX 5060 Ti profile (CUDA 13.4, GNU C++ 14):

```sh
cmake -S . -B build-cuda -G Ninja -DCMAKE_BUILD_TYPE=Release \
  -DFORTGEN_BUILD_CUDA_TESTING=ON \
  -DCMAKE_CUDA_COMPILER=/opt/cuda/bin/nvcc \
  -DCMAKE_CUDA_HOST_COMPILER=/usr/bin/g++-14 \
  -DCMAKE_CUDA_ARCHITECTURES=120
cmake --build build-cuda --target test_cuda_kernel -j 4
ctest --test-dir build-cuda -R '^cuda_kernel$' --output-on-failure
```

Choose the architecture and supported host compiler explicitly for another
device. Keep the configure command, source revision and executable hash with
the result; generated headers and binaries stay in the build tree.

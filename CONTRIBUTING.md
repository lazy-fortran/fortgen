# Contributing

Keep changes small and preserve the public contracts in [spec/](spec/).
Behavior changes need an independent numerical or semantic oracle, including
invalid inputs and relevant boundary cases. Generated source changes must
retain their generator and reproducible inputs.

Run both native build interfaces before submitting:

```sh
fpm build
fpm test
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

For Python changes, run `python3 -m pip install './python[sympy,test]'`
and `pytest python/test`
with the built `fortgen-codegen` on `PATH`. CUDA changes also need the
[device checks](test/device/README.md); host compilation does not replace them.
State the compiler, commands and limits of the checks in the pull request.

// Launch wrapper for the FortGen-emitted Metal leaf. The leaf itself is
// generated into the build directory; nothing here rewrites its arithmetic.
#include <metal_stdlib>
#include "metal_reciprocal.metal"

kernel void metal_oracle(device const float* inputs, device float* outputs,
                         uint id [[thread_position_in_grid]]) {
    if (id >= FORTGEN_METAL_SAMPLES) return;
    thread float value, refined, square, cube, tangent, adjoint, wave, extent,
        root, scaled;
    metal_reciprocal(inputs[id], inputs[FORTGEN_METAL_SAMPLES + id],
                     inputs[2 * FORTGEN_METAL_SAMPLES + id],
                     &value, &refined, &square, &cube, &tangent, &adjoint,
                     &wave, &extent, &root, &scaled);
    const uint base = id * 10u;
    outputs[base + 0u] = value;
    outputs[base + 1u] = refined;
    outputs[base + 2u] = square;
    outputs[base + 3u] = cube;
    outputs[base + 4u] = tangent;
    outputs[base + 5u] = adjoint;
    outputs[base + 6u] = wave;
    outputs[base + 7u] = extent;
    outputs[base + 8u] = root;
    outputs[base + 9u] = scaled;
}

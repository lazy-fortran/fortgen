#include "typed_digit.metal"

kernel void typed_digit_oracle(device const ulong* input [[buffer(0)]],
    device const float* estimate [[buffer(1)]], device ulong* output [[buffer(2)]],
    uint i [[thread_position_in_grid]]) {
    if (i != 0) return;
    ulong lo, hi, sum, difference, large, signbit, maximum, shifted, masked, merged;
    ulong residual, product;
    uint digit;
    bool carry, borrow, valid, cert;
    typed_digit(input[0], input[1], estimate[0], &lo, &hi, &sum, &carry,
        &difference, &borrow, &large, &signbit, &maximum, &shifted, &masked,
        &merged, &digit, &valid, &residual, &cert, &product);
    output[0] = lo; output[1] = hi; output[2] = sum; output[3] = carry;
    output[4] = difference; output[5] = borrow; output[6] = large;
    output[7] = signbit; output[8] = maximum; output[9] = shifted;
    output[10] = masked; output[11] = merged; output[12] = digit;
    output[13] = valid; output[14] = residual; output[15] = cert;
    output[16] = product;
}

program test_precision_storage
    use iso_fortran_env, only: int16, real64
    use ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, ieee_value, &
        ieee_positive_inf, ieee_quiet_nan
    use fortgen_storage, only: pack_fp16, unpack_fp16, pack_bf16, unpack_bf16
    implicit none
    integer :: i, signed_word, e
    integer(int16) :: bits
    real(real64) :: x, q, quantum
    ! Exhaust every finite encoded value, including signs and subnormals.
    do i=0,65535
        signed_word = i
        if (i >= 32768) signed_word = i-65536
        bits = int(signed_word, int16)
        x = unpack_fp16(bits)
        if (.not. ieee_is_nan(x)) then
            if (pack_fp16(x) /= bits) error stop 'fp16 roundtrip'
        end if
        x = unpack_bf16(bits)
        if (.not. ieee_is_nan(x)) then
            if (pack_bf16(x) /= bits) error stop 'bf16 roundtrip'
        end if
    end do
    if (int(pack_fp16(1.0_real64)) /= 15360) error stop 'half one'
    if (int(pack_bf16(1.0_real64)) /= 16256) error stop 'bf16 one'
    if (unpack_fp16(int(1,int16)) /= 2.0_real64**(-24)) &
        error stop 'half smallest subnormal'
    if (unpack_bf16(int(1,int16)) /= 2.0_real64**(-133)) &
        error stop 'bf16 smallest subnormal'
    if (unpack_fp16(pack_fp16(1.0_real64+2.0_real64**(-11))) /= &
        1.0_real64) error stop 'half ties even'
    if (unpack_bf16(pack_bf16(1.0_real64+2.0_real64**(-8))) /= &
        1.0_real64) error stop 'bf16 ties even'
    ! Direct binary64 rounding must not first round the input to binary32.
    x = 1.0_real64 + 2.0_real64**(-11) + 2.0_real64**(-30)
    if (unpack_fp16(pack_fp16(x)) <= 1.0_real64) error stop 'double rounding'
    if (unpack_fp16(pack_fp16(65504.0_real64)) /= 65504.0_real64) &
        error stop 'maximum finite'
    if (ieee_is_finite(unpack_fp16(pack_fp16(65520.0_real64)))) &
        error stop 'overflow rounding'
    x = ieee_value(0.0_real64, ieee_quiet_nan)
    if (.not. ieee_is_nan(unpack_fp16(pack_fp16(x)))) error stop 'nan'
    do e=-20,15
        quantum = scale(1.0_real64, max(e-10,-24))
        do i=1,257
            x = scale(1.0_real64+real(i,real64)/258, e)
            q = unpack_fp16(pack_fp16(x))
            if (abs(q-x) > quantum/2) error stop 'independent half error bound'
        end do
    end do
    print *, 'IEEE storage: exhaustive roundtrips and independent error bounds PASS'
end program

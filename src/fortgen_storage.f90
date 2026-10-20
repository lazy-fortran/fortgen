module fortgen_storage
    !! Portable IEEE binary16/bfloat16 storage; arithmetic stays fp32/fp64.
    !! Round directly from binary64, nearest with ties to even (no double round).
    use, intrinsic :: iso_fortran_env, only: int16, int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, &
        ieee_value, ieee_positive_inf, ieee_quiet_nan
    implicit none
    private
    public :: pack_fp16, unpack_fp16, pack_bf16, unpack_bf16
contains
    elemental function pack_fp16(x) result(bits)
        real(real64), intent(in) :: x
        integer(int16) :: bits
        bits = pack16(x, 5, 10)
    end function

    elemental function pack_bf16(x) result(bits)
        real(real64), intent(in) :: x
        integer(int16) :: bits
        bits = pack16(x, 8, 7)
    end function

    elemental function unpack_fp16(bits) result(x)
        integer(int16), intent(in) :: bits
        real(real64) :: x
        x = unpack16(bits, 5, 10)
    end function

    elemental function unpack_bf16(bits) result(x)
        integer(int16), intent(in) :: bits
        real(real64) :: x
        x = unpack16(bits, 8, 7)
    end function

    pure function pack16(x, ebits, fbits) result(bits)
        real(real64), intent(in) :: x
        integer, intent(in) :: ebits, fbits
        integer(int16) :: bits
        integer(int32) :: word, sign_bit, mantissa, biased, bias, emax
        real(real64) :: a, scaled, remainder
        sign_bit = 0
        if (btest(transfer(x, 0_int64), 63)) sign_bit = 32768
        bias = 2**(ebits - 1) - 1
        emax = 2**ebits - 1
        if (ieee_is_nan(x)) then
            word = ior(shiftl(emax, fbits), 2**(fbits - 1))
        else if (.not. ieee_is_finite(x)) then
            word = shiftl(emax, fbits)
        else
            a = abs(x)
            biased = 0
            mantissa = 0
            if (a > 0.0_real64) then
                biased = exponent(a) - 1 + bias
                if (biased >= emax) then
                    biased = emax
                else
                    ! Subnormals have the same quantum as the smallest normal.
                    scaled = scale(a, fbits - max(1, biased) + bias)
                    mantissa = int(scaled, int32)
                    remainder = scaled - real(mantissa, real64)
                    if (remainder > 0.5_real64 .or. &
                        (remainder == 0.5_real64 .and. btest(mantissa, 0))) &
                        mantissa = mantissa + 1
                    if (biased <= 0) then
                        biased = 0
                        if (mantissa == 2**fbits) biased = 1
                    else if (mantissa == 2**(fbits + 1)) then
                        biased = biased + 1
                        mantissa = 0
                    end if
                    mantissa = iand(mantissa, 2**fbits - 1)
                end if
            end if
            word = ior(shiftl(biased, fbits), mantissa)
        end if
        word = ior(word, sign_bit)
        ! Avoid out-of-range integer conversion on processors with checking.
        if (word >= 32768) word = word - 65536
        bits = int(word, int16)
    end function

    pure function unpack16(bits, ebits, fbits) result(x)
        integer(int16), intent(in) :: bits
        integer, intent(in) :: ebits, fbits
        real(real64) :: x
        integer :: word, biased, mantissa, bias
        word = iand(int(bits), 65535)
        biased = iand(shiftr(word, fbits), 2**ebits - 1)
        mantissa = iand(word, 2**fbits - 1)
        bias = 2**(ebits - 1) - 1
        if (biased == 2**ebits - 1) then
            x = ieee_value(0.0_real64, ieee_positive_inf)
            if (mantissa /= 0) x = ieee_value(x, ieee_quiet_nan)
        else if (biased == 0) then
            x = scale(real(mantissa, real64), 1 - bias - fbits)
        else
            x = scale(real(2**fbits + mantissa, real64), biased - bias - fbits)
        end if
        if (btest(word, 15)) x = -x
    end function
end module fortgen_storage

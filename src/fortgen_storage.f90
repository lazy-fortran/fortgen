module fortgen_storage
    !! Portable IEEE binary16/bfloat16 storage; arithmetic stays fp32/fp64.
    !! Round directly from binary64, nearest with ties to even (no double round).
    use, intrinsic :: iso_fortran_env, only: int16, int32, int64, real64
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
        integer(int64) :: raw, significand, rounded
        integer(int32) :: word, sign_bit, biased, bias, emax, source_exp
        raw = transfer(x, 0_int64)
        sign_bit = 0
        if (btest(raw, 63)) sign_bit = 32768
        source_exp = int(ibits(raw, 52, 11))
        significand = ibits(raw, 0, 52)
        bias = 2**(ebits - 1) - 1
        emax = 2**ebits - 1
        if (source_exp == 2047) then
            word = shiftl(emax, fbits)
            if (significand /= 0) word = ior(word, 2**(fbits - 1))
        else if (source_exp == 0) then
            ! Every binary64 subnormal lies below half a storage subnormal.
            word = 0
        else
            biased = source_exp - 1023 + bias
            significand = ibset(significand, 52)
            if (biased >= emax) then
                word = shiftl(emax, fbits)
            else if (biased <= 0) then
                rounded = round_shift(significand, 53 - fbits - biased)
                ! Carry into bit fbits produces the smallest normal directly.
                word = int(rounded, int32)
            else
                rounded = round_shift(significand, 52 - fbits)
                if (rounded == 2_int64**(fbits + 1)) then
                    biased = biased + 1
                    rounded = 0
                end if
                word = ior(shiftl(biased, fbits), &
                    iand(int(rounded, int32), 2**fbits - 1))
            end if
        end if
        word = ior(word, sign_bit)
        if (word >= 32768) word = word - 65536
        bits = int(word, int16)
    end function

    pure integer(int64) function round_shift(value, shift) result(rounded)
        !! Exact integer guard/round/sticky decision, nearest with ties to even.
        integer(int64), intent(in) :: value
        integer, intent(in) :: shift
        integer(int64) :: discarded, halfway
        rounded = 0
        if (shift > 53) return
        rounded = shiftr(value, shift)
        halfway = shiftl(1_int64, shift - 1)
        discarded = iand(value, shiftl(1_int64, shift) - 1)
        if (discarded > halfway .or. &
            (discarded == halfway .and. btest(rounded, 0))) rounded = rounded + 1
    end function

    pure function unpack16(bits, ebits, fbits) result(x)
        integer(int16), intent(in) :: bits
        integer, intent(in) :: ebits, fbits
        real(real64) :: x
        integer(int64) :: raw
        integer :: word, biased, mantissa, bias, leading
        word = iand(int(bits), 65535)
        biased = iand(shiftr(word, fbits), 2**ebits - 1)
        mantissa = iand(word, 2**fbits - 1)
        bias = 2**(ebits - 1) - 1
        raw = 0
        if (biased == 2**ebits - 1) then
            raw = shiftl(2047_int64, 52)
            if (mantissa /= 0) raw = ibset(raw, 51)
        else if (biased > 0) then
            raw = ior(shiftl(int(biased - bias + 1023, int64), 52), &
                shiftl(int(mantissa, int64), 52 - fbits))
        else if (mantissa > 0) then
            leading = bit_size(mantissa) - leadz(mantissa) - 1
            raw = ior(shiftl(int(leading + 1 - bias - fbits + 1023, int64), 52), &
                shiftl(int(mantissa - 2**leading, int64), 52 - leading))
        end if
        if (btest(word, 15)) raw = ibset(raw, 63)
        x = transfer(raw, x)
    end function
end module fortgen_storage

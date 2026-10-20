program test_precision_kernel
    use iso_fortran_env, only: int16, real32, real64
    use fortgen_storage, only: pack_fp16, unpack_fp16, pack_bf16, unpack_bf16
    implicit none
    interface
        pure subroutine polynomial_fp64(x,y)
            import real64
            real(real64), intent(in) :: x
            real(real64), intent(out) :: y
        end subroutine
        pure subroutine polynomial_fp32(x,y)
            import real32
            real(real32), intent(in) :: x
            real(real32), intent(out) :: y
        end subroutine
        pure subroutine polynomial_fp16(x,y)
            import int16
            integer(int16), intent(in) :: x
            integer(int16), intent(out) :: y
        end subroutine
        pure subroutine polynomial_bf16(x,y)
            import int16
            integer(int16), intent(in) :: x
            integer(int16), intent(out) :: y
        end subroutine
    end interface
    real(real64) :: x,y,reference,quantized
    real(real32) :: single
    integer(int16) :: half
    integer :: i
    do i=0,256
        x = real(i,real64)/129
        reference = x*x+2*x+1
        call polynomial_fp64(x,y)
        if (abs(y-reference) > 4*epsilon(y)*reference) error stop 'fp64'
        call polynomial_fp32(real(x,real32),single)
        if (abs(real(single,real64)-reference) > &
            8*epsilon(single)*reference) error stop 'fp32'
        call polynomial_fp16(pack_fp16(x),half)
        quantized = unpack_fp16(half)
        if (abs(quantized-reference) > 8*2.0_real64**(-11)*reference) &
            error stop 'fp16 storage'
        call polynomial_bf16(pack_bf16(x),half)
        quantized = unpack_bf16(half)
        if (abs(quantized-reference) > 8*2.0_real64**(-8)*reference) &
            error stop 'bf16 storage'
    end do
    print *, 'compiled precision family: conditioned roundoff bounds PASS'
end program

module fortgen_functions
    !! Canonical scalar function vocabulary understood by FortGen emitters.
    implicit none
    private

    public :: fortran_function_supported, fortran_function_arity_ok
    public :: fortran_function_uses_special, fortran_function_spelling

contains

    pure logical function fortran_function_supported(name) result(ok)
        character(*), intent(in) :: name
        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
                "exp", "log", "log10", "sqrt", "abs", "erf", "erfc", &
                "gamma", "loggamma", "max", "min", "Min", "Max", &
                "besselj", "bessely", "besseli", "besselk")
            ok = .true.
        case default
            ok = .false.
        end select
    end function fortran_function_supported

    pure logical function fortran_function_arity_ok(name, n) result(ok)
        character(*), intent(in) :: name
        integer, intent(in) :: n
        if (name == "max" .or. name == "min" .or. name == "Min" .or. &
            name == "Max") then
            ok = n >= 2
        else if (name == "atan2" .or. name == "besselj" .or. &
                name == "bessely" .or. name == "besseli" .or. name == "besselk") then
            ok = n == 2
        else
            ok = n == 1
        end if
    end function fortran_function_arity_ok

    pure logical function fortran_function_uses_special(name) result(ok)
        character(*), intent(in) :: name
        ok = name == "besseli" .or. name == "besselk"
    end function fortran_function_uses_special

    pure function fortran_function_spelling(name) result(text)
        character(*), intent(in) :: name
        character(:), allocatable :: text
        select case (name)
        case ("loggamma"); text = "log_gamma"
        case ("besselj");  text = "bessel_jn"
        case ("bessely");  text = "bessel_yn"
        case ("besseli");  text = "bessel_in"
        case ("besselk");  text = "bessel_kn"
        case ("Min");      text = "min"
        case ("Max");      text = "max"
        case default;      text = name
        end select
    end function fortran_function_spelling

end module fortgen_functions

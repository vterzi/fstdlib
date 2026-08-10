#define _PROC _CAT3(character,_KIND_LABEL1,_LABEL2)
pure function _PROC( &
    arg, kind, mode, width, decimals, expwidth, sign &
) result(res)
    _TYPE2(_KIND2), intent(in) :: arg
#if _KIND1 == _DEFAULT_KIND1
    character(len=*, kind=_KIND1), intent(in), optional :: kind
#else
    character(len=*, kind=_KIND1), intent(in) :: kind
#endif
    integer, intent(in), optional :: mode, width, decimals, expwidth
    logical, intent(in), optional :: sign
    character(len=:, kind=_KIND1), allocatable :: res

    logical :: present_mode, present_width, present_decimals, &
        present_expwidth, present_sign, sign_
    integer :: mode_, width_, decimals_, expwidth_
    character(len=:), allocatable :: fmt
    character(len=_CAT3(MAX_LEN_REAL,_KIND_LABEL2,STR), kind=_KIND1) :: buf

    present_mode = present(mode)
    present_width = present(width)
    present_decimals = present(decimals)
    present_expwidth = present(expwidth)
    present_sign = present(sign)
    if ( &
        present_mode &
        .or. present_width &
        .or. present_decimals &
        .or. present_expwidth &
        .or. present_sign &
    ) then
        fmt = "("
        if (present_sign) then
            sign_ = sign
        else
            sign_ = .false.
        end if
        if (sign_) fmt = fmt // "sp,"
        if (present_mode) then
            mode_ = mode
        else
            mode_ = MODE_GENERAL
        end if
        select case (mode_)
        case (MODE_FIXED)
            fmt = fmt // "f"
            present_expwidth = .false.
            width_ = FIXED_EXTRA_WIDTH + max(0, es_exponent(arg))
        case (MODE_EXPONENTIAL)
            fmt = fmt // "e"
            width_ = EXPONENTIAL_EXTRA_WIDTH
        case (MODE_SCIENTIFIC)
            fmt = fmt // "es"
            width_ = SCIENTIFIC_EXTRA_WIDTH
        case (MODE_ENGINEERING)
            fmt = fmt // "en"
            width_ = SCIENTIFIC_EXTRA_WIDTH + modulo(es_exponent(arg), 3)
        case (MODE_EXPONENTIAL_D)
            fmt = fmt // "d"
            width_ = EXPONENTIAL_EXTRA_WIDTH
        case default
            fmt = fmt // "g"
            width_ = EXPONENTIAL_EXTRA_WIDTH
        end select
        if (present_decimals) then
            decimals_ = max(0, decimals)
        else
            decimals_ = _CAT3(REAL,_KIND_LABEL2,DEC_DIGITS)
        end if
        if (present_expwidth) then
            expwidth_ = max(1, expwidth)
        else
            expwidth_ = _CAT3(REAL,_KIND_LABEL2,EXP_WIDTH)
        end if
        if (present_width) then
            width_ = max(1, width)
        else
            width_ = width_ + decimals_
            if (mode /= MODE_FIXED) width_ = width_ + expwidth_
            if (arg < 0 .or. sign_) width_ = width_ + 1
        end if
        fmt = fmt // str(width_) // "." // str(decimals_)
        if (present_expwidth) fmt = fmt // "e" // str(expwidth_)
        fmt = fmt // ")"
        allocate(character(len=width_, kind=_KIND1) :: res)
        write(res, fmt) arg
    else
        write(buf, *) arg
        res = trim(adjustl(buf))
    end if
end function _PROC
#undef _PROC

#if _TYPE == _LOGICAL
#  define _EQ .eqv.
#  define _NE .neqv.
#else
#  define _EQ ==
#  define _NE /=
#endif


#define _PROC _CAT(assert_equal_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first _EQ second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '/=', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(assert_not_equal_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first _NE second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '==', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC


#if _TYPE == _INTEGER | _TYPE == _REAL | _TYPE == _CHARACTER
#  define _PROC _CAT(assert_greater_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first > second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '<=', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _CAT(assert_greater_equal_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first >= second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '<', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _CAT(assert_less_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first < second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '>=', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _CAT(assert_less_equal_,_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first <= second)) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '>', second
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC
#endif


#undef _EQ
#undef _NE

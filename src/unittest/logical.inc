#define _PROC _CAT(assert_true_,_LABEL)
subroutine _PROC(expr, msg, success)
    _ARG_TYPE, intent(in) :: expr
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. expr) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) expr, 'is not true'
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(assert_false_,_LABEL)
subroutine _PROC(expr, msg, success)
    _ARG_TYPE, intent(in) :: expr
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (expr) then
        if (PRESENT(msg)) then
            write(*, *) msg
        end if
        write(*, *) expr, 'is not false'
        if (PRESENT(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC

#ifdef _ARG_ARRAY
#  define _PROC_NAME _SUBPROC(_LIST,Append,Array)
#  define _ARG_IN_TYPE _IN_TYPE_ARR
#  define _ARG_SIZE _ARR_SIZE(arg)
#  define _ARG_ELEM(X) arg(_ARR_IDX(X))
#else
#  define _PROC_NAME _SUBPROC(_LIST,Append,List)
#  define _ARG_IN_TYPE class(_LIST)
#  define _ARG_SIZE arg%Size()
#  define _ARG_ELEM(X) iter%Next()
#endif


subroutine _PROC_NAME(this, arg)
    class(_LIST), intent(inout) :: this
    _ARG_IN_TYPE, intent(in) :: arg

    integer :: i, n
    type(_NODE), pointer :: appended
#ifndef _ARG_ARRAY
    type(_ITER) :: iter

    iter = _ITER(arg)
#endif

    n = _ARG_SIZE
    if (n > 0) then
        call this%Append(_ARG_ELEM(1))
        appended => this%last
        do i = 2, n
            allocate(appended%next)
            appended => appended%next
            appended%val = _ARG_ELEM(i)
        end do
        this%last => appended
        this%size_ = this%size_ + n - 1
    end if
end subroutine _PROC_NAME


#undef _PROC_NAME
#undef _ARG_IN_TYPE
#undef _ARG_SIZE
#undef _ARG_ELEM

#undef _ARG_ARRAY

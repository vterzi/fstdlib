#ifdef _ARG_ARRAY
#  define _PROC_NAME _SUBPROC(_LIST,Insert,Array)
#  define _ARG_IN_TYPE _IN_TYPE_ARR
#  define _ARG_SIZE _ARR_SIZE(arg)
#  define _ARG_ELEM(X) arg(_ARR_IDX(X))
#else
#  define _PROC_NAME _SUBPROC(_LIST,Insert,List)
#  define _ARG_IN_TYPE class(_LIST)
#  define _ARG_SIZE arg%Size()
#  define _ARG_ELEM(X) iter%Next()
#endif


subroutine _PROC_NAME(this, idx, arg)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: idx
    _ARG_IN_TYPE, intent(in) :: arg

    integer :: idx_, i, n
    type(_NODE), pointer :: inserted, last
#ifndef _ARG_ARRAY
    type(_ITER) :: iter

    iter = _ITER(arg)
#endif

    n = _ARG_SIZE
    if (n > 0) then
        if (idx > this%size_) then
            call this%Append(arg)
        else
            if (idx < - this%size_ .or. idx == 0) then
                idx_ = 1
            else
                idx_ = this%Index(idx)
            end if
            call this%Insert(idx_, _ARG_ELEM(1))
            inserted => this%Node(idx_)
            last => inserted%next
            do i = 2, n
                allocate(inserted%next)
                inserted => inserted%next
                inserted%val = _ARG_ELEM(i)
            end do
            inserted%next => last
            this%size_ = this%size_ + n - 1
        end if
    end if
end subroutine _PROC_NAME


#undef _PROC_NAME
#undef _ARG_IN_TYPE
#undef _ARG_SIZE
#undef _ARG_ELEM

#undef _ARG_ARRAY

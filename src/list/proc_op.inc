subroutine _PROC(_LIST,_PROC_SUFFIX)(this, idx, val)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: idx
    _ARG_IN_TYPE, intent(in) :: val

    type(_NODE), pointer :: node

    node => this%Node(this%Index(idx))
    node%val = node%val _OP val
end subroutine _PROC(_LIST,_PROC_SUFFIX)


#undef _PROC_SUFFIX
#undef _OP
#undef _ARG_IN_TYPE

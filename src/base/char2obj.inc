#define _PROC0 _CAT(to_,_TYPE_NAME)
#ifdef _KIND
#  define _PROC _CAT(_PROC0,_KIND)
#else
#  define _PROC _PROC0
#endif
elemental function _PROC(arg) result(res)
    character(len=*), intent(in) :: arg
    _RES_TYPE :: res

    read(arg, *) res
end function _PROC
#undef _PROC0
#undef _PROC

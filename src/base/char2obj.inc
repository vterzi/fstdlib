! {"id.inc"(_ID)} + () -(_PROC0, _PROC)> ()
! Define conversion functions from the character type to other basic data types.


#define _PROC0 _CAT(to_,_TYPE_NAME)
#ifdef _KIND_LABEL
#  define _PROC _CAT(_PROC0,_KIND_LABEL)
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

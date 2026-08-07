#undef _DIM
#if defined(_RANK) && _RANK > 0
#  if _RANK == 1
#    define _DIM dimension(:)
#  endif
#  if _RANK == 2
#    define _DIM dimension(:, :)
#  endif
#  if _RANK == 3
#    define _DIM dimension(:, :, :)
#  endif
#  if _RANK == 4
#    define _DIM dimension(:, :, :, :)
#  endif
#  if _RANK == 5
#    define _DIM dimension(:, :, :, :, :)
#  endif
#  if _RANK == 6
#    define _DIM dimension(:, :, :, :, :, :)
#  endif
#  if _RANK == 7
#    define _DIM dimension(:, :, :, :, :, :, :)
#  endif
#  if _RANK == 8
#    define _DIM dimension(:, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 9
#    define _DIM dimension(:, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 10
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 11
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 12
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 13
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 14
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK == 15
#    define _DIM dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#endif
#undef _TYPE_LEN
#undef _ELEM_TYPE
#undef _ARG_TYPE
#undef _RES_TYPE
#if _TYPE_ID == _CHARACTER
#  define _TYPE_LEN(X) _TYPE(len=X, kind=_KIND)
#  define _ELEM_TYPE _TYPE_LEN(:), allocatable
#  ifdef _DIM
#    define _ARG_TYPE _TYPE_LEN(*), _DIM
#    define _RES_TYPE _ELEM_TYPE, _DIM
#  else
#    define _ARG_TYPE _TYPE_LEN(*)
#    define _RES_TYPE _ELEM_TYPE
#  endif
#else
#  define _TYPE_LEN(X) _TYPE(_KIND)
#  define _ELEM_TYPE _TYPE_LEN()
#  ifdef _DIM
#    define _ARG_TYPE _ELEM_TYPE, _DIM
#    define _RES_TYPE _ELEM_TYPE, _DIM, allocatable
#  else
#    define _ARG_TYPE _ELEM_TYPE
#    define _RES_TYPE _ELEM_TYPE
#  endif
#endif

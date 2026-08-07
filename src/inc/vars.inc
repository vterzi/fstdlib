#undef _DIM1
#if defined(_RANK1) && _RANK1 > 0
#  if _RANK1 == 1
#    define _DIM1 dimension(:)
#  endif
#  if _RANK1 == 2
#    define _DIM1 dimension(:, :)
#  endif
#  if _RANK1 == 3
#    define _DIM1 dimension(:, :, :)
#  endif
#  if _RANK1 == 4
#    define _DIM1 dimension(:, :, :, :)
#  endif
#  if _RANK1 == 5
#    define _DIM1 dimension(:, :, :, :, :)
#  endif
#  if _RANK1 == 6
#    define _DIM1 dimension(:, :, :, :, :, :)
#  endif
#  if _RANK1 == 7
#    define _DIM1 dimension(:, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 8
#    define _DIM1 dimension(:, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 9
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 10
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 11
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 12
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 13
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 14
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK1 == 15
#    define _DIM1 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#endif
#undef _TYPE_LEN1
#undef _ELEM_TYPE1
#undef _ARG_TYPE1
#undef _RES_TYPE1
#if _TYPE_ID1 == _CHARACTER
#  define _TYPE_LEN1(X) _TYPE1(len=X, kind=_KIND1)
#  define _ELEM_TYPE1 _TYPE_LEN1(:), allocatable
#  ifdef _DIM1
#    define _ARG_TYPE1 _TYPE_LEN1(*), _DIM1
#    define _RES_TYPE1 _ELEM_TYPE1, _DIM1
#  else
#    define _ARG_TYPE1 _TYPE_LEN1(*)
#    define _RES_TYPE1 _ELEM_TYPE1
#  endif
#else
#  define _TYPE_LEN1(X) _TYPE1(_KIND1)
#  define _ELEM_TYPE1 _TYPE_LEN1()
#  ifdef _DIM1
#    define _ARG_TYPE1 _ELEM_TYPE1, _DIM1
#    define _RES_TYPE1 _ELEM_TYPE1, _DIM1, allocatable
#  else
#    define _ARG_TYPE1 _ELEM_TYPE1
#    define _RES_TYPE1 _ELEM_TYPE1
#  endif
#endif
#undef _DIM2
#if defined(_RANK2) && _RANK2 > 0
#  if _RANK2 == 1
#    define _DIM2 dimension(:)
#  endif
#  if _RANK2 == 2
#    define _DIM2 dimension(:, :)
#  endif
#  if _RANK2 == 3
#    define _DIM2 dimension(:, :, :)
#  endif
#  if _RANK2 == 4
#    define _DIM2 dimension(:, :, :, :)
#  endif
#  if _RANK2 == 5
#    define _DIM2 dimension(:, :, :, :, :)
#  endif
#  if _RANK2 == 6
#    define _DIM2 dimension(:, :, :, :, :, :)
#  endif
#  if _RANK2 == 7
#    define _DIM2 dimension(:, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 8
#    define _DIM2 dimension(:, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 9
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 10
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 11
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 12
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 13
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 14
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#  if _RANK2 == 15
#    define _DIM2 dimension(:, :, :, :, :, :, :, :, :, :, :, :, :, :, :)
#  endif
#endif
#undef _TYPE_LEN2
#undef _ELEM_TYPE2
#undef _ARG_TYPE2
#undef _RES_TYPE2
#if _TYPE_ID2 == _CHARACTER
#  define _TYPE_LEN2(X) _TYPE2(len=X, kind=_KIND2)
#  define _ELEM_TYPE2 _TYPE_LEN2(:), allocatable
#  ifdef _DIM2
#    define _ARG_TYPE2 _TYPE_LEN2(*), _DIM2
#    define _RES_TYPE2 _ELEM_TYPE2, _DIM2
#  else
#    define _ARG_TYPE2 _TYPE_LEN2(*)
#    define _RES_TYPE2 _ELEM_TYPE2
#  endif
#else
#  define _TYPE_LEN2(X) _TYPE2(_KIND2)
#  define _ELEM_TYPE2 _TYPE_LEN2()
#  ifdef _DIM2
#    define _ARG_TYPE2 _ELEM_TYPE2, _DIM2
#    define _RES_TYPE2 _ELEM_TYPE2, _DIM2, allocatable
#  else
#    define _ARG_TYPE2 _ELEM_TYPE2
#    define _RES_TYPE2 _ELEM_TYPE2
#  endif
#endif
#undef _DIM0
#undef _TYPE_LEN0
#undef _ELEM_TYPE0
#undef _ARG_TYPE0
#undef _RES_TYPE0
#if (_TYPE_ID1 | _KIND1) >= (_TYPE_ID2 | _KIND2)
#  define _DIM0 _DIM1
#  define _TYPE_LEN0 _TYPE_LEN1
#  define _ELEM_TYPE0 _ELEM_TYPE1
#  define _ARG_TYPE0 _ARG_TYPE1
#  define _RES_TYPE0 _RES_TYPE1
#else
#  define _DIM0 _DIM2
#  define _TYPE_LEN0 _TYPE_LEN2
#  define _ELEM_TYPE0 _ELEM_TYPE2
#  define _ARG_TYPE0 _ARG_TYPE2
#  define _RES_TYPE0 _RES_TYPE2
#endif

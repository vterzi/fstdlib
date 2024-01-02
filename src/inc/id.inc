! $1 = (_LOGICAL, _INTEGER, _REAL, _COMPLEX, _CHARACTER, _CLASS_K, _CLASS_P, _CLASS_S)
! $2<?> = (_CLASS?, _TYPE?, _KIND?, _VAR_TYPE?, _ARG_TYPE?, _RES_TYPE?, _TYPE_LEN?, _TYPE_NAME?, _TYPE_LABEL?, _KIND_LABEL?,
!   _LABEL?, [_NUMBER?])
! {_KEEP_ARGS} + (_ID) -(_DIFF)> ("cat.inc", [_ID], $1, $2<>, _N_IDS)
! {_KEEP_ARGS} + (_ID1, _ID2) -(_DIFF)> ("cat.inc", [_ID1], [_ID2], $1, $2<1>, $2<2>, $2<0>, _N_IDS)
! Determine information about either one basic data type `_ID` or two basic data types `_ID1` and `_ID2`.  `_ID?` is the sum of a
! type (`_LOGICAL`, `_INTEGER`, `_REAL`, `_COMPLEX`, `_CHARACTER`) and a kind (`_K1`, `_K2`, `_K4`, `_K16`, `_HP`, `_SP`, `_DP`,
! `_XDP`, `_QP`, `_ASCII`, `_UCS4`) value.  The number of arguments is saved in `_N_IDS`.  If `_KEEP_ARGS` is defined, the arguments
! are not undefined at the end of the file.  The resulting informations are:
!   * `_CLASS?`: kind class of the type (`_CLASS_K` for logical or integer, `_CLASS_P` for real or complex, `_CLASS_S` for
!     character),
!   * `_TYPE?`: the type value,
!   * `_KIND?`: the kind value,
!   * `_VAR_TYPE?`: the type specifier for declarations of variables,
!   * `_ARG_TYPE?`: the type specifier for declarations of procedure arguments,
!   * `_RES_TYPE?`: the type specifier for declarations of procedure results,
!   * `_TYPE_LEN?(<length>)`: the type specifier for declarations of character variables with a given length,
!   * `_TYPE_NAME?`: the full name of the type,
!   * `_TYPE_LABEL?`: the label of the type,
!   * `_KIND_LABEL?`: the label of the kind,
!   * `_LABEL?`: the label of the type and the kind.
!   * `_NUMBER?`: a defined variable if the type is a number type,
! In the case of two arguments, the information of the larger type and kind (larger value of `_ID?`) is copied to variables with
! the label `0` ($2<0>).
!   * `_LOGICAL < _INTEGER < _REAL < _COMPLEX < _CHARACTER`,
!   * `_K1 < _K2 < _K4 < _K8 < _K16`,
!   * `_HP < _SP < _DP < _XDP < _QP`,
!   * `_ASCII < _UCS4`.

#include "../inc/cat.inc"

#undef _LOGICAL
#define _LOGICAL (_MAX_KIND+1)
#undef _INTEGER
#define _INTEGER (_LOGICAL+_MAX_KIND+1)
#undef _REAL
#define _REAL (_INTEGER+_MAX_KIND+1)
#undef _COMPLEX
#define _COMPLEX (_REAL+_MAX_KIND+1)
#undef _CHARACTER
#define _CHARACTER (_COMPLEX+_MAX_KIND+1)
#undef _CLASS_K
#define _CLASS_K _INTEGER
#undef _CLASS_P
#define _CLASS_P _REAL
#undef _CLASS_S
#define _CLASS_S _CHARACTER

#undef _N_IDS
#if defined(_ID1) & defined(_ID2)
#  define _N_IDS 2
#  undef _TYPE_NAME1
#  undef _CLASS1
#  undef _KIND1
#  undef _VAR_TYPE1
#  undef _ARG_TYPE1
#  undef _RES_TYPE1
#  undef _TYPE_LEN1
#  undef _TYPE_LABEL1
#  undef _KIND_LABEL1
#  undef _LABEL1
#  undef _NUMBER1
#  if _CHARACTER <= _ID1 & _ID1 <= _CHARACTER + _MAX_KIND
#    define _TYPE_NAME1 character
#    define _TYPE_LABEL1 S
#    undef _TYPE1
#    define _TYPE1 _CHARACTER
#    define _CLASS1 _CLASS_S
#    define _DIFF (_ID1-_TYPE1)
#    if defined(_ASCII) & _DIFF == _ASCII
#      define _KIND1 ASCII
#      define _KIND_LABEL1 A
#    elif defined(_UCS4) & _DIFF == _UCS4
#      define _KIND1 UCS4
#      define _KIND_LABEL1 U
#    else
#      define _KIND_LABEL1
#    endif
#    ifdef _KIND1
#      define _TYPE_LEN1(X) _TYPE_NAME1(len=X, kind=_KIND1)
#    else
#      define _TYPE_LEN1(X) _TYPE_NAME1(len=X)
#    endif
#    define _LABEL1 _CAT(_TYPE_LABEL1,_KIND_LABEL1)
#    define _VAR_TYPE1 _TYPE_LEN1(:), allocatable
#    define _ARG_TYPE1 _TYPE_LEN1(*)
#    define _RES_TYPE1 _VAR_TYPE1
#  else
#    if _LOGICAL <= _ID1 & _ID1 <= _LOGICAL + _MAX_KIND
#      define _TYPE_NAME1 logical
#      define _TYPE_LABEL1 L
#      undef _TYPE1
#      define _TYPE1 _LOGICAL
#      define _CLASS1 _CLASS_K
#    elif _INTEGER <= _ID1 & _ID1 <= _INTEGER + _MAX_KIND
#      define _TYPE_NAME1 integer
#      define _TYPE_LABEL1 I
#      undef _TYPE1
#      define _TYPE1 _INTEGER
#      define _CLASS1 _CLASS_K
#      define _NUMBER1
#    elif _REAL <= _ID1 & _ID1 <= _REAL + _MAX_KIND
#      define _TYPE_NAME1 real
#      define _TYPE_LABEL1 R
#      undef _TYPE1
#      define _TYPE1 _REAL
#      define _CLASS1 _CLASS_P
#      define _NUMBER1
#    elif _COMPLEX <= _ID1 & _ID1 <= _COMPLEX + _MAX_KIND
#      define _TYPE_NAME1 complex
#      define _TYPE_LABEL1 C
#      undef _TYPE1
#      define _TYPE1 _COMPLEX
#      define _CLASS1 _CLASS_P
#      define _NUMBER1
#    else
#      error Unknown type of `_ID1`
#    endif
#    define _DIFF (_ID1-_TYPE1)
#    if _CLASS1 == _CLASS_K
#      if defined(_K1) & _DIFF == _K1
#        define _KIND1 K1
#      elif defined(_K2) & _DIFF == _K2
#        define _KIND1 K2
#      elif defined(_K4) & _DIFF == _K4
#        define _KIND1 K4
#      elif defined(_K8) & _DIFF == _K8
#        define _KIND1 K8
#      elif defined(_K16) & _DIFF == _K16
#        define _KIND1 K16
#      endif
#    elif _CLASS1 == _CLASS_P
#      if defined(_HP) & _DIFF == _HP
#        define _KIND1 HP
#      elif defined(_SP) & _DIFF == _SP
#        define _KIND1 SP
#      elif defined(_DP) & _DIFF == _DP
#        define _KIND1 DP
#      elif defined(_XDP) & _DIFF == _XDP
#        define _KIND1 XDP
#      elif defined(_QP) & _DIFF == _QP
#        define _KIND1 QP
#      endif
#    endif
#    ifdef _KIND1
#      define _VAR_TYPE1 _TYPE_NAME1(_KIND1)
#      define _KIND_LABEL1 _KIND1
#    else
#      define _VAR_TYPE1 _TYPE_NAME1
#      define _KIND_LABEL1
#    endif
#    define _LABEL1 _CAT(_TYPE_LABEL1,_KIND_LABEL1)
#    define _ARG_TYPE1 _VAR_TYPE1
#    define _RES_TYPE1 _VAR_TYPE1
#    define _TYPE_LEN1(X) _VAR_TYPE1
#  endif
#  undef _DIFF
#  undef _TYPE_NAME2
#  undef _CLASS2
#  undef _KIND2
#  undef _VAR_TYPE2
#  undef _ARG_TYPE2
#  undef _RES_TYPE2
#  undef _TYPE_LEN2
#  undef _TYPE_LABEL2
#  undef _KIND_LABEL2
#  undef _LABEL2
#  undef _NUMBER2
#  if _CHARACTER <= _ID2 & _ID2 <= _CHARACTER + _MAX_KIND
#    define _TYPE_NAME2 character
#    define _TYPE_LABEL2 S
#    undef _TYPE2
#    define _TYPE2 _CHARACTER
#    define _CLASS2 _CLASS_S
#    define _DIFF (_ID2-_TYPE2)
#    if defined(_ASCII) & _DIFF == _ASCII
#      define _KIND2 ASCII
#      define _KIND_LABEL2 A
#    elif defined(_UCS4) & _DIFF == _UCS4
#      define _KIND2 UCS4
#      define _KIND_LABEL2 U
#    else
#      define _KIND_LABEL2
#    endif
#    ifdef _KIND2
#      define _TYPE_LEN2(X) _TYPE_NAME2(len=X, kind=_KIND2)
#    else
#      define _TYPE_LEN2(X) _TYPE_NAME2(len=X)
#    endif
#    define _LABEL2 _CAT(_TYPE_LABEL2,_KIND_LABEL2)
#    define _VAR_TYPE2 _TYPE_LEN2(:), allocatable
#    define _ARG_TYPE2 _TYPE_LEN2(*)
#    define _RES_TYPE2 _VAR_TYPE2
#  else
#    if _LOGICAL <= _ID2 & _ID2 <= _LOGICAL + _MAX_KIND
#      define _TYPE_NAME2 logical
#      define _TYPE_LABEL2 L
#      undef _TYPE2
#      define _TYPE2 _LOGICAL
#      define _CLASS2 _CLASS_K
#    elif _INTEGER <= _ID2 & _ID2 <= _INTEGER + _MAX_KIND
#      define _TYPE_NAME2 integer
#      define _TYPE_LABEL2 I
#      undef _TYPE2
#      define _TYPE2 _INTEGER
#      define _CLASS2 _CLASS_K
#      define _NUMBER2
#    elif _REAL <= _ID2 & _ID2 <= _REAL + _MAX_KIND
#      define _TYPE_NAME2 real
#      define _TYPE_LABEL2 R
#      undef _TYPE2
#      define _TYPE2 _REAL
#      define _CLASS2 _CLASS_P
#      define _NUMBER2
#    elif _COMPLEX <= _ID2 & _ID2 <= _COMPLEX + _MAX_KIND
#      define _TYPE_NAME2 complex
#      define _TYPE_LABEL2 C
#      undef _TYPE2
#      define _TYPE2 _COMPLEX
#      define _CLASS2 _CLASS_P
#      define _NUMBER2
#    else
#      error Unknown type of `_ID2`
#    endif
#    define _DIFF (_ID2-_TYPE2)
#    if _CLASS2 == _CLASS_K
#      if defined(_K1) & _DIFF == _K1
#        define _KIND2 K1
#      elif defined(_K2) & _DIFF == _K2
#        define _KIND2 K2
#      elif defined(_K4) & _DIFF == _K4
#        define _KIND2 K4
#      elif defined(_K8) & _DIFF == _K8
#        define _KIND2 K8
#      elif defined(_K16) & _DIFF == _K16
#        define _KIND2 K16
#      endif
#    elif _CLASS2 == _CLASS_P
#      if defined(_HP) & _DIFF == _HP
#        define _KIND2 HP
#      elif defined(_SP) & _DIFF == _SP
#        define _KIND2 SP
#      elif defined(_DP) & _DIFF == _DP
#        define _KIND2 DP
#      elif defined(_XDP) & _DIFF == _XDP
#        define _KIND2 XDP
#      elif defined(_QP) & _DIFF == _QP
#        define _KIND2 QP
#      endif
#    endif
#    ifdef _KIND2
#      define _VAR_TYPE2 _TYPE_NAME2(_KIND2)
#      define _KIND_LABEL2 _KIND2
#    else
#      define _VAR_TYPE2 _TYPE_NAME2
#      define _KIND_LABEL2
#    endif
#    define _LABEL2 _CAT(_TYPE_LABEL2,_KIND_LABEL2)
#    define _ARG_TYPE2 _VAR_TYPE2
#    define _RES_TYPE2 _VAR_TYPE2
#    define _TYPE_LEN2(X) _VAR_TYPE2
#  endif
#  undef _DIFF
#  undef _TYPE_NAME0
#  undef _CLASS0
#  undef _KIND0
#  undef _VAR_TYPE0
#  undef _ARG_TYPE0
#  undef _RES_TYPE0
#  undef _TYPE_LEN0
#  undef _TYPE_LABEL0
#  undef _KIND_LABEL0
#  undef _LABEL0
#  undef _NUMBER0
#  if _ID1 >= _ID2
#    define _TYPE_NAME0 _TYPE_NAME1
#    undef _TYPE0
#    define _TYPE0 _TYPE1
#    define _CLASS0 _CLASS1
#    ifdef _KIND1
#      define _KIND0 _KIND1
#    endif
#    define _VAR_TYPE0 _VAR_TYPE1
#    define _ARG_TYPE0 _ARG_TYPE1
#    define _RES_TYPE0 _RES_TYPE1
#    define _TYPE_LEN0 _TYPE_LEN1
#    define _TYPE_LABEL0 _TYPE_LABEL1
#    define _KIND_LABEL0 _KIND_LABEL1
#    define _LABEL0 _LABEL1
#  else
#    define _TYPE_NAME0 _TYPE_NAME2
#    undef _TYPE0
#    define _TYPE0 _TYPE2
#    define _CLASS0 _CLASS2
#    ifdef _KIND2
#      define _KIND0 _KIND2
#    endif
#    define _VAR_TYPE0 _VAR_TYPE2
#    define _ARG_TYPE0 _ARG_TYPE2
#    define _RES_TYPE0 _RES_TYPE2
#    define _TYPE_LEN0 _TYPE_LEN2
#    define _TYPE_LABEL0 _TYPE_LABEL2
#    define _KIND_LABEL0 _KIND_LABEL2
#    define _LABEL0 _LABEL2
#  endif
#  ifndef _KEEP_ARGS
#    undef _ID1
#    undef _ID2
#  endif
#elif defined(_ID)
#  define _N_IDS 1
#  undef _TYPE_NAME
#  undef _CLASS
#  undef _KIND
#  undef _VAR_TYPE
#  undef _ARG_TYPE
#  undef _RES_TYPE
#  undef _TYPE_LEN
#  undef _TYPE_LABEL
#  undef _KIND_LABEL
#  undef _LABEL
#  undef _NUMBER
#  if _CHARACTER <= _ID & _ID <= _CHARACTER + _MAX_KIND
#    define _TYPE_NAME character
#    define _TYPE_LABEL S
#    undef _TYPE
#    define _TYPE _CHARACTER
#    define _CLASS _CLASS_S
#    define _DIFF (_ID-_TYPE)
#    if defined(_ASCII) & _DIFF == _ASCII
#      define _KIND ASCII
#      define _KIND_LABEL A
#    elif defined(_UCS4) & _DIFF == _UCS4
#      define _KIND UCS4
#      define _KIND_LABEL U
#    else
#      define _KIND_LABEL
#    endif
#    ifdef _KIND
#      define _TYPE_LEN(X) _TYPE_NAME(len=X, kind=_KIND)
#    else
#      define _TYPE_LEN(X) _TYPE_NAME(len=X)
#    endif
#    define _LABEL _CAT(_TYPE_LABEL,_KIND_LABEL)
#    define _VAR_TYPE _TYPE_LEN(:), allocatable
#    define _ARG_TYPE _TYPE_LEN(*)
#    define _RES_TYPE _VAR_TYPE
#  else
#    if _LOGICAL <= _ID & _ID <= _LOGICAL + _MAX_KIND
#      define _TYPE_NAME logical
#      define _TYPE_LABEL L
#      undef _TYPE
#      define _TYPE _LOGICAL
#      define _CLASS _CLASS_K
#    elif _INTEGER <= _ID & _ID <= _INTEGER + _MAX_KIND
#      define _TYPE_NAME integer
#      define _TYPE_LABEL I
#      undef _TYPE
#      define _TYPE _INTEGER
#      define _CLASS _CLASS_K
#      define _NUMBER
#    elif _REAL <= _ID & _ID <= _REAL + _MAX_KIND
#      define _TYPE_NAME real
#      define _TYPE_LABEL R
#      undef _TYPE
#      define _TYPE _REAL
#      define _CLASS _CLASS_P
#      define _NUMBER
#    elif _COMPLEX <= _ID & _ID <= _COMPLEX + _MAX_KIND
#      define _TYPE_NAME complex
#      define _TYPE_LABEL C
#      undef _TYPE
#      define _TYPE _COMPLEX
#      define _CLASS _CLASS_P
#      define _NUMBER
#    else
#      error Unknown type of `_ID`
#    endif
#    define _DIFF (_ID-_TYPE)
#    if _CLASS == _CLASS_K
#      if defined(_K1) & _DIFF == _K1
#        define _KIND K1
#      elif defined(_K2) & _DIFF == _K2
#        define _KIND K2
#      elif defined(_K4) & _DIFF == _K4
#        define _KIND K4
#      elif defined(_K8) & _DIFF == _K8
#        define _KIND K8
#      elif defined(_K16) & _DIFF == _K16
#        define _KIND K16
#      endif
#    elif _CLASS == _CLASS_P
#      if defined(_HP) & _DIFF == _HP
#        define _KIND HP
#      elif defined(_SP) & _DIFF == _SP
#        define _KIND SP
#      elif defined(_DP) & _DIFF == _DP
#        define _KIND DP
#      elif defined(_XDP) & _DIFF == _XDP
#        define _KIND XDP
#      elif defined(_QP) & _DIFF == _QP
#        define _KIND QP
#      endif
#    endif
#    ifdef _KIND
#      define _VAR_TYPE _TYPE_NAME(_KIND)
#      define _KIND_LABEL _KIND
#    else
#      define _VAR_TYPE _TYPE_NAME
#      define _KIND_LABEL
#    endif
#    define _LABEL _CAT(_TYPE_LABEL,_KIND_LABEL)
#    define _ARG_TYPE _VAR_TYPE
#    define _RES_TYPE _VAR_TYPE
#    define _TYPE_LEN(X) _VAR_TYPE
#  endif
#  undef _DIFF
#  ifndef _KEEP_ARGS
#    undef _ID
#  endif
#else
#  error Two arguments `_ID1` and `_ID2` or one argument `_ID` required
#endif

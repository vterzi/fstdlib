#if (_TYPE_ID2 == _LOGICAL) || (_TYPE_ID2 == _INTEGER)
#  ifdef _K1
#    define _KIND2 _K1
#    define _KIND_LABEL2 K1
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _K2
#    define _KIND2 _K2
#    define _KIND_LABEL2 K2
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _K4
#    define _KIND2 _K4
#    define _KIND_LABEL2 K4
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _K8
#    define _KIND2 _K8
#    define _KIND_LABEL2 K8
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _K16
#    define _KIND2 _K16
#    define _KIND_LABEL2 K16
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#elif (_TYPE_ID2 == _REAL) || (_TYPE_ID2 == _COMPLEX)
#  ifdef _HP
#    define _KIND2 _HP
#    define _KIND_LABEL2 HP
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _SP
#    define _KIND2 _SP
#    define _KIND_LABEL2 SP
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _DP
#    define _KIND2 _DP
#    define _KIND_LABEL2 DP
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _XDP
#    define _KIND2 _XDP
#    define _KIND_LABEL2 XDP
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _QP
#    define _KIND2 _QP
#    define _KIND_LABEL2 QP
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#elif _TYPE_ID2 == _CHARACTER
#  ifdef _ASCII
#    define _KIND2 _ASCII
#    define _KIND_LABEL2 A
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#  ifdef _UCS4
#    define _KIND2 _UCS4
#    define _KIND_LABEL2 U
#    include _FILE
#    undef _KIND2
#    undef _KIND_LABEL2
#  endif
#endif

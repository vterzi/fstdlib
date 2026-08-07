#if (_TYPE_ID1 == _LOGICAL) || (_TYPE_ID1 == _INTEGER)
#  ifdef _K1
#    define _KIND1 _K1
#    define _KIND_LABEL1 K1
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _K2
#    define _KIND1 _K2
#    define _KIND_LABEL1 K2
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _K4
#    define _KIND1 _K4
#    define _KIND_LABEL1 K4
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _K8
#    define _KIND1 _K8
#    define _KIND_LABEL1 K8
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _K16
#    define _KIND1 _K16
#    define _KIND_LABEL1 K16
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#elif (_TYPE_ID1 == _REAL) || (_TYPE_ID1 == _COMPLEX)
#  ifdef _HP
#    define _KIND1 _HP
#    define _KIND_LABEL1 HP
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _SP
#    define _KIND1 _SP
#    define _KIND_LABEL1 SP
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _DP
#    define _KIND1 _DP
#    define _KIND_LABEL1 DP
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _XDP
#    define _KIND1 _XDP
#    define _KIND_LABEL1 XDP
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _QP
#    define _KIND1 _QP
#    define _KIND_LABEL1 QP
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#elif _TYPE_ID1 == _CHARACTER
#  ifdef _ASCII
#    define _KIND1 _ASCII
#    define _KIND_LABEL1 A
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#  ifdef _UCS4
#    define _KIND1 _UCS4
#    define _KIND_LABEL1 U
#    include "../inc/kind2.inc"
#    undef _KIND1
#    undef _KIND_LABEL1
#  endif
#endif

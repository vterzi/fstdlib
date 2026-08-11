#include "../inc/init.inc"
#include "../inc/utils.inc"

module pytran_conversion
    use :: pytran_utils, only: str
    use :: pytran_number

    implicit none

    private

#ifdef _K1
    integer, parameter :: MAX_LEN_INTEGER_K1_STR = _MAX_LEN_INTEGER_K1_STR
#endif
#ifdef _K2
    integer, parameter :: MAX_LEN_INTEGER_K2_STR = _MAX_LEN_INTEGER_K2_STR
#endif
#ifdef _K4
    integer, parameter :: MAX_LEN_INTEGER_K4_STR = _MAX_LEN_INTEGER_K4_STR
#endif
#ifdef _K8
    integer, parameter :: MAX_LEN_INTEGER_K8_STR = _MAX_LEN_INTEGER_K8_STR
#endif
#ifdef _K16
    integer, parameter :: MAX_LEN_INTEGER_K16_STR = _MAX_LEN_INTEGER_K16_STR
#endif

#ifdef _HP
    integer, parameter :: &
        MAX_LEN_REAL_HP_STR = _MAX_LEN_REAL_HP_STR, &
        MAX_LEN_COMPLEX_HP_STR = _MAX_LEN_COMPLEX_HP_STR
#endif
#ifdef _SP
    integer, parameter :: &
        MAX_LEN_REAL_SP_STR = _MAX_LEN_REAL_SP_STR, &
        MAX_LEN_COMPLEX_SP_STR = _MAX_LEN_COMPLEX_SP_STR
#endif
#ifdef _DP
    integer, parameter :: &
        MAX_LEN_REAL_DP_STR = _MAX_LEN_REAL_DP_STR, &
        MAX_LEN_COMPLEX_DP_STR = _MAX_LEN_COMPLEX_DP_STR
#endif
#ifdef _XDP
    integer, parameter :: &
        MAX_LEN_REAL_XDP_STR = _MAX_LEN_REAL_XDP_STR, &
        MAX_LEN_COMPLEX_XDP_STR = _MAX_LEN_COMPLEX_XDP_STR
#endif
#ifdef _QP
    integer, parameter :: &
        MAX_LEN_REAL_QP_STR = _MAX_LEN_REAL_QP_STR, &
        MAX_LEN_COMPLEX_QP_STR = _MAX_LEN_COMPLEX_QP_STR
#endif

    integer, parameter :: &
        MAX_LEN_LOGICAL_STR = _MAX_LEN_LOGICAL_STR, &
        MAX_LEN_INTEGER_STR = _MAX_LEN_INTEGER_STR, &
        MAX_LEN_REAL_STR = _MAX_LEN_REAL_STR, &
        MAX_LEN_COMPLEX_STR = _MAX_LEN_COMPLEX_STR

    integer, parameter, public :: &
        MODE_GENERAL = 0, &  ! `g`
        MODE_FIXED = 1, &  ! `f`
        MODE_EXPONENTIAL = 2, &  ! `e`
        MODE_SCIENTIFIC = 3, &  ! `es`
        MODE_ENGINEERING = 4, &  ! `en`
        MODE_EXPONENTIAL_D = 5  ! `d`
    integer, parameter :: &
        ! leading digit + decimal point
        FIXED_EXTRA_WIDTH = 2, &
        ! ... + exponent delimiter + exponent sign
        SCIENTIFIC_EXTRA_WIDTH = FIXED_EXTRA_WIDTH + 2, &
        ! ... + leading zero
        EXPONENTIAL_EXTRA_WIDTH = SCIENTIFIC_EXTRA_WIDTH + 1, &
        ! left parenthesis + comma + right parenthesis
        COMPLEX_EXTRA_WIDTH = 3

#define _PROC _UNARY_OP(_OP)
#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _OP bin
#include "../inc/iface.inc"
#define _OP oct
#include "../inc/iface.inc"
#define _OP hex
#include "../inc/iface.inc"
#undef _TYPE_IDS
#undef _PROC

#define _PROC _CAT3(_TYPE1,_KIND_LABEL1,_LABEL2)

#define _TYPE_IDS2 _CHARACTER
#define _TYPE_IDS1 _LOGICAL
#define _OP logical
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _INTEGER
#define _OP integer
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _REAL
#define _OP real
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _COMPLEX
#define _OP complex
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2

#define _TYPE_IDS1 _CHARACTER
#define _TYPE_IDS2 (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _OP character
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2

#undef _PROC

contains

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _FILE "../conversion/boz.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

#define _FILE "../conversion/type.inc"
#define _TYPE_IDS2 _CHARACTER
#define _TYPE_IDS1 _LOGICAL
#include "../inc/types.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _INTEGER
#include "../inc/types.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _REAL
#include "../inc/types.inc"
#undef _TYPE_IDS1
#define _TYPE_IDS1 _COMPLEX
#include "../inc/types.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#undef _FILE

#define _TYPE_IDS1 _CHARACTER
#define _TYPE_IDS2 _LOGICAL
#define _FILE "../conversion/character_l.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS2
#define _TYPE_IDS2 _INTEGER
#define _FILE "../conversion/character_i.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS2
#define _TYPE_IDS2 _REAL
#define _FILE "../conversion/character_r.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS2
#define _TYPE_IDS2 _COMPLEX
#define _FILE "../conversion/character_c.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS2
#define _TYPE_IDS2 _CHARACTER
#define _FILE "../conversion/character_s.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS2
#undef _TYPE_IDS1

end module pytran_conversion

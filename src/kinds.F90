! Kinds of basic data types as parameters


module stdlib_kinds
    use, intrinsic :: iso_c_binding, only: &
        C_BOOL, &
        C_SIGNED_CHAR, C_SHORT, C_INT, C_LONG, C_LONG_LONG, C_SIZE_T, &
        C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T, &
        C_INT_LEAST8_T, C_INT_LEAST16_T, C_INT_LEAST32_T, C_INT_LEAST64_T, &
        C_INT_FAST8_T, C_INT_FAST16_T, C_INT_FAST32_T, C_INT_FAST64_T, &
        C_INTMAX_T, C_INTPTR_T, C_PTRDIFF_T, &
        C_FLOAT, C_DOUBLE, C_LONG_DOUBLE, &
        C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX, &
        C_CHAR

    implicit none

    private
    public :: &
        C_BOOL, &
        C_SIGNED_CHAR, C_SHORT, C_INT, C_LONG, C_LONG_LONG, C_SIZE_T, &
        C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T, &
        C_INT_LEAST8_T, C_INT_LEAST16_T, C_INT_LEAST32_T, C_INT_LEAST64_T, &
        C_INT_FAST8_T, C_INT_FAST16_T, C_INT_FAST32_T, C_INT_FAST64_T, &
        C_INTMAX_T, C_INTPTR_T, C_PTRDIFF_T, &
        C_FLOAT, C_DOUBLE, C_LONG_DOUBLE, &
        C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX, &
        C_CHAR

    ! logical, integer (K = kind)
#ifdef _K1
    integer, parameter, public :: &
        K1 = _K1, &
        MAX_LEN_INTEGER_K1_STR = _MAX_LEN_INTEGER_K1_STR
#endif
#ifdef _K2
    integer, parameter, public :: &
        K2 = _K2, &
        MAX_LEN_INTEGER_K2_STR = _MAX_LEN_INTEGER_K2_STR
#endif
#ifdef _K4
    integer, parameter, public :: &
        K4 = _K4, &
        MAX_LEN_INTEGER_K4_STR = _MAX_LEN_INTEGER_K4_STR
#endif
#ifdef _K8
    integer, parameter, public :: &
        K8 = _K8, &
        MAX_LEN_INTEGER_K8_STR = _MAX_LEN_INTEGER_K8_STR
#endif
#ifdef _K16
    integer, parameter, public :: &
        K16 = _K16, &
        MAX_LEN_INTEGER_K16_STR = _MAX_LEN_INTEGER_K16_STR
#endif

    ! real, complex (P = precision)
#ifdef _HP
    ! half
    integer, parameter, public :: &
        HP = _HP, &
        MAX_LEN_REAL_HP_STR = _MAX_LEN_REAL_HP_STR, &
        MAX_LEN_COMPLEX_HP_STR = _MAX_LEN_COMPLEX_HP_STR
#endif
#ifdef _SP
    ! single
    integer, parameter, public :: &
        SP = _SP, &
        MAX_LEN_REAL_SP_STR = _MAX_LEN_REAL_SP_STR, &
        MAX_LEN_COMPLEX_SP_STR = _MAX_LEN_COMPLEX_SP_STR
#endif
#ifdef _DP
    ! double
    integer, parameter, public :: &
        DP = _DP, &
        MAX_LEN_REAL_DP_STR = _MAX_LEN_REAL_DP_STR, &
        MAX_LEN_COMPLEX_DP_STR = _MAX_LEN_COMPLEX_DP_STR
#endif
#ifdef _XDP
    ! extended double
    integer, parameter, public :: &
        XDP = _XDP, &
        MAX_LEN_REAL_XDP_STR = _MAX_LEN_REAL_XDP_STR, &
        MAX_LEN_COMPLEX_XDP_STR = _MAX_LEN_COMPLEX_XDP_STR
#endif
#ifdef _QP
    ! quadruple
    integer, parameter, public :: &
        QP = _QP, &
        MAX_LEN_REAL_QP_STR = _MAX_LEN_REAL_QP_STR, &
        MAX_LEN_COMPLEX_QP_STR = _MAX_LEN_COMPLEX_QP_STR
#endif

    ! character
#ifdef _ASCII
    integer, parameter, public :: ASCII = _ASCII
#endif
#ifdef _UCS4
    integer, parameter, public :: UCS4 = _UCS4
#endif

    ! default kinds
    integer, parameter, public :: &
        LK = kind(.false.), &  ! logical
        IK = kind(0), &  ! integer
        RK = kind(0.), &  ! real
        CK = kind((0, 0)), &  ! complex
        SK = kind('')  ! character

    ! maximum lengths of default kinds
    integer, parameter, public :: &
        MAX_LEN_LOGICAL_STR = _MAX_LEN_LOGICAL_STR, &
        MAX_LEN_INTEGER_STR = _MAX_LEN_INTEGER_STR, &
        MAX_LEN_REAL_STR = _MAX_LEN_REAL_STR, &
        MAX_LEN_COMPLEX_STR = _MAX_LEN_COMPLEX_STR
end module stdlib_kinds

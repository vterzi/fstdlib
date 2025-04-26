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

    ! logical, integer (K = kind)
#ifdef _K1
    integer, parameter, public :: K1 = selected_int_kind(_K1_ARG)
#endif
#ifdef _K2
    integer, parameter, public :: K2 = selected_int_kind(_K2_ARG)
#endif
#ifdef _K4
    integer, parameter, public :: K4 = selected_int_kind(_K4_ARG)
#endif
#ifdef _K8
    integer, parameter, public :: K8 = selected_int_kind(_K8_ARG)
#endif
#ifdef _K16
    integer, parameter, public :: K16 = selected_int_kind(_K16_ARG)
#endif

    ! real, complex (P = precision)
#ifdef _HP
    ! half
    integer, parameter, public :: HP = selected_real_kind(_HP_ARG)
#endif
#ifdef _SP
    ! single
    integer, parameter, public :: SP = selected_real_kind(_SP_ARG)
#endif
#ifdef _DP
    ! double
    integer, parameter, public :: DP = selected_real_kind(_DP_ARG)
#endif
#ifdef _XDP
    ! extended double
    integer, parameter, public :: XDP = selected_real_kind(_XDP_ARG)
#endif
#ifdef _QP
    ! quadruple
    integer, parameter, public :: QP = selected_real_kind(_QP_ARG)
#endif

    ! character
#ifdef _ASCII
    integer, parameter, public :: ASCII = selected_char_kind(_ASCII_ARG)
#endif
#ifdef _UCS4
    integer, parameter, public :: UCS4 = selected_char_kind(_UCS4_ARG)
#endif

    ! default kinds
    integer, parameter, public :: &
        LK = kind(.false.), &  ! logical
        IK = kind(0), &  ! integer
        RK = kind(0.), &  ! real
        CK = RK, &  ! complex
        SK = kind('')  ! character
end module stdlib_kinds

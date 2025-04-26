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

    ! logical, integer (K = kind)
#ifdef _K1
    integer, parameter :: K1 = selected_int_kind(_K1_ARG)
#endif
#ifdef _K2
    integer, parameter :: K2 = selected_int_kind(_K2_ARG)
#endif
#ifdef _K4
    integer, parameter :: K4 = selected_int_kind(_K4_ARG)
#endif
#ifdef _K8
    integer, parameter :: K8 = selected_int_kind(_K8_ARG)
#endif
#ifdef _K16
    integer, parameter :: K16 = selected_int_kind(_K16_ARG)
#endif

    ! real, complex (P = precision)
#ifdef _HP
    integer, parameter :: HP = selected_real_kind(_HP_ARG)  ! half
#endif
#ifdef _SP
    integer, parameter :: SP = selected_real_kind(_SP_ARG)  ! single
#endif
#ifdef _DP
    integer, parameter :: DP = selected_real_kind(_DP_ARG)  ! double
#endif
#ifdef _XDP
    integer, parameter :: XDP = selected_real_kind(_XDP_ARG)  ! extended double
#endif
#ifdef _QP
    integer, parameter :: QP = selected_real_kind(_QP_ARG)  ! quadruple
#endif

    ! character
#ifdef _ASCII
    integer, parameter :: ASCII = selected_char_kind(_ASCII_ARG)
#endif
#ifdef _UCS4
    integer, parameter :: UCS4 = selected_char_kind(_UCS4_ARG)
#endif

    ! default kinds
    integer, parameter :: &
        LK = kind(.false.), &  ! logical
        IK = kind(0), &  ! integer
        RK = kind(0.), &  ! real
        CK = RK, &  ! complex
        SK = kind('')  ! character
end module stdlib_kinds

! Kinds of basic data types as parameters

module stdlib_kinds
    use, intrinsic :: ISO_FORTRAN_ENV, only: &
        INT8, INT16, INT32, INT64, &
        REAL32, REAL64, REAL128, &
        LOGICAL_KINDS, INTEGER_KINDS, REAL_KINDS, CHARACTER_KINDS
    use, intrinsic :: ISO_C_BINDING, only: &
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

    integer, parameter :: &
        ! logical, integer
        ! K = kind
        K1 = SELECTED_INT_KIND(2), &
        K2 = SELECTED_INT_KIND(4), &
        K4 = SELECTED_INT_KIND(9), &
        K8 = SELECTED_INT_KIND(18), &
#ifdef _K16
        K16 = SELECTED_INT_KIND(38), &
#endif
        ! real, complex
        ! P = precision
#ifdef _HP
        HP = SELECTED_REAL_KIND(3), &  ! half
#endif
        SP = SELECTED_REAL_KIND(6), &  ! single
        DP = SELECTED_REAL_KIND(15), &  ! double
#ifdef _XDP
        XDP = SELECTED_REAL_KIND(18), &  ! extended double
#endif
#ifdef _QP
        QP = SELECTED_REAL_KIND(33), &  ! quadruple
#endif
        ! character
        ASCII = SELECTED_CHAR_KIND('ASCII'), &
#ifdef _UCS4
        UCS4 = SELECTED_CHAR_KIND('ISO_10646'), &
#endif
        ! default kinds
        LK = KIND(.false.), &  ! logical
        IK = KIND(0), &  ! integer
        RK = KIND(0.), &  ! real
        CK = RK, &  ! complex
        SK = KIND('')  ! character
end module stdlib_kinds

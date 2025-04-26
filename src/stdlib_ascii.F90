! ASCII characters


module stdlib_ascii
    use, intrinsic :: iso_c_binding, only: &
        C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_HORIZONTAL_TAB, C_NEW_LINE, &
        C_VERTICAL_TAB, C_FORM_FEED, C_CARRIAGE_RETURN
    use stdlib_kinds

    implicit none

    private
    public :: &
        C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_HORIZONTAL_TAB, C_NEW_LINE, &
        C_VERTICAL_TAB, C_FORM_FEED, C_CARRIAGE_RETURN, &
        isspace, isdigit, isdecimal, isnumeric, isalpha, isalnum, &
        isidentifier, isprintable, isascii, isupper, islower, &
        upper, lower, strip, operator(.in.)

    character(len=*), parameter, public :: &
        NUL = achar(0), &  ! null
        SOH = achar(1), &  ! start of heading
        STX = achar(2), &  ! start of text
        ETX = achar(3), &  ! end of text
        EOT = achar(4), &  ! end of transmission
        ENQ = achar(5), &  ! enquiry
        ACK = achar(6), &  ! acknowledge
        BEL = achar(7), &  ! bell
        BS = achar(8), &  ! backspace
        TAB = achar(9), &  ! horizontal tab
        LF = achar(10), &  ! line feed
        VT = achar(11), &  ! vertical tab
        FF = achar(12), &  ! form feed
        CR = achar(13), &  ! carriage return
        SO = achar(14), &  ! shift out
        SI = achar(15), &  ! shift in
        DLE = achar(16), &  ! data link escape
        DC1 = achar(17), &  ! device control 1
        DC2 = achar(18), &  ! device control 2
        DC3 = achar(19), &  ! device control 3
        DC4 = achar(20), &  ! device control 4
        NAK = achar(21), &  ! negative acknowledge
        SYN = achar(22), &  ! synchronous idle
        ETB = achar(23), &  ! end of transmission block
        CAN = achar(24), &  ! cancel
        EM = achar(25), &  ! end of medium
        SUB = achar(26), &  ! substitute
        ESC = achar(27), &  ! escape
        FS = achar(28), &  ! file separator
        GS = achar(29), &  ! group separator
        RS = achar(30), &  ! record separator
        US = achar(31), &  ! unit separator
        DEL = achar(127), &  ! delete
        UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
        LOWERCASE = 'abcdefghijklmnopqrstuvwxyz', &
        LETTERS = UPPERCASE // LOWERCASE, &
        DIGITS = '0123456789', &
        BINDIGITS = DIGITS(:2), &
        OCTDIGITS = DIGITS(:8), &
        HEXDIGITS = DIGITS // UPPERCASE(:5) // LOWERCASE(:5), &
        PUNCTUATION = '!"#$%&' // "'" // '()*+,-./:;<=>?@[' // achar(92) &
            // ']^_`{|}~', &
        WHITESPACE = TAB // LF // VT // FF // CR // ' ', &
        PRINTABLE = LETTERS // DIGITS // PUNCTUATION // WHITESPACE, &
        NONPRINTABLE = NUL // SOH // STX // ETX // EOT // ENQ // ACK // BEL &
            // BS // SO // SI // DLE // DC1 // DC2 // DC3 // DC4 // NAK &
            // SYN // ETB // CAN // EM // SUB // ESC // FS // GS // RS // US &
            // DEL, &
        WORDCHARS = LETTERS // '_' // DIGITS

    integer, parameter :: &
        CASESHIFT = IACHAR(LOWERCASE(:1)) - IACHAR(UPPERCASE(:1))

    interface isdecimal
        module procedure isdigit
    end interface isdecimal

    interface isnumeric
        module procedure isdigit
    end interface isnumeric

    interface operator(.in.)
        module procedure contains
    end interface

contains
    ! Iteration over characters may be faster than `verify` on some compilers
    ! but slower on others.  Therefore, a simpler implementation that uses
    ! intrinsic procedures is preferred.  An example of the alternative code
    ! for a slice of the ASCII collating sequence `CHARACTERS`:
    ! ```
    ! integer :: i
    ! character(len=1) :: chr
    ! res = .false.
    ! do i = 1, len(arg)
    !     chr = arg(i:i)
    !     res = CHARACTERS(:1) <= chr .and. chr <= CHARACTERS(len(CHARACTERS):)
    !     if (.not. res) exit
    ! end do
    ! ```

    elemental function isspace(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, WHITESPACE) == 0
    end function isspace


    elemental function isdigit(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, DIGITS) == 0
    end function isdigit


    elemental function isalpha(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, LETTERS) == 0
    end function isalpha


    elemental function isalnum(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, LETTERS // DIGITS) == 0
    end function isalnum


    elemental function isidentifier(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 &
            .and. verify(arg, WORDCHARS) == 0 &
            .and. arg(1:1) /= '_'
    end function isidentifier


    elemental function isprintable(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = verify(arg, PRINTABLE) == 0
    end function isprintable


    elemental function isascii(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = verify(arg, PRINTABLE // NONPRINTABLE) == 0
    end function isascii


    elemental function isupper(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = scan(arg, UPPERCASE) > 0 .and. verify(arg, LOWERCASE) == 0
    end function isupper


    elemental function islower(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = scan(arg, LOWERCASE) > 0 .and. verify(arg, UPPERCASE) == 0
    end function islower


    elemental function upper(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        integer :: i

        res = arg
        i = 0
        do
            i = scan(res(i + 1 :), LOWERCASE)
            if (i == 0) exit
            res(i:i) = achar(iachar(res(i:i)) - CASESHIFT)
        end do
    end function upper


    elemental function lower(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        integer :: i

        res = arg
        i = 0
        do
            i = scan(res(i + 1 :), UPPERCASE)
            if (i == 0) exit
            res(i:i) = achar(iachar(res(i:i)) + CASESHIFT)
        end do
    end function lower


    pure function strip(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: res

        ! `trim(adjustl(arg))` only works on spaces.
        res = arg(verify(arg, WHITESPACE) : verify(arg, WHITESPACE, .true.))
    end function strip


    elemental function contains(substr, str) result(res)
        character(len=*), intent(in) :: substr, str
        logical :: res

        res = len(substr) == 0 .or. index(str, substr) > 0
    end function contains
end module stdlib_ascii

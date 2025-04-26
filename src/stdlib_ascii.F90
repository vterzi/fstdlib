! ASCII characters


module stdlib_ascii
    use, intrinsic :: iso_c_binding, only: &
        C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_HORIZONTAL_TAB, C_NEW_LINE, &
        C_VERTICAL_TAB, C_FORM_FEED, C_CARRIAGE_RETURN
    use stdlib_kinds

    implicit none

    character(len=*), parameter :: &
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
        WORDCHARS = LETTERS // '_' // DIGITS
end module stdlib_ascii

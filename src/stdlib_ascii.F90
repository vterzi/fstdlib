! ASCII characters

module stdlib_ascii
    use, intrinsic :: ISO_C_BINDING, only: C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_HORIZONTAL_TAB, C_NEW_LINE, C_VERTICAL_TAB, &
        C_FORM_FEED, C_CARRIAGE_RETURN
    use stdlib_kinds

    implicit none

    character(len=*), parameter :: &
        NUL = ACHAR(0), &  ! null
        SOH = ACHAR(1), &  ! start of heading
        STX = ACHAR(2), &  ! start of text
        ETX = ACHAR(3), &  ! end of text
        EOT = ACHAR(4), &  ! end of transmission
        ENQ = ACHAR(5), &  ! enquiry
        ACK = ACHAR(6), &  ! acknowledge
        BEL = ACHAR(7), &  ! bell
        BS = ACHAR(8), &  ! backspace
        TAB = ACHAR(9), &  ! horizontal tab
        LF = ACHAR(10), &  ! line feed
        VT = ACHAR(11), &  ! vertical tab
        FF = ACHAR(12), &  ! form feed
        CR = ACHAR(13), &  ! carriage return
        SO = ACHAR(14), &  ! shift out
        SI = ACHAR(15), &  ! shift in
        DLE = ACHAR(16), &  ! data link escape
        DC1 = ACHAR(17), &  ! device control 1
        DC2 = ACHAR(18), &  ! device control 2
        DC3 = ACHAR(19), &  ! device control 3
        DC4 = ACHAR(20), &  ! device control 4
        NAK = ACHAR(21), &  ! negative acknowledge
        SYN = ACHAR(22), &  ! synchronous idle
        ETB = ACHAR(23), &  ! end of transmission block
        CAN = ACHAR(24), &  ! cancel
        EM = ACHAR(25), &  ! end of medium
        SUB = ACHAR(26), &  ! substitute
        ESC = ACHAR(27), &  ! escape
        FS = ACHAR(28), &  ! file separator
        GS = ACHAR(29), &  ! group separator
        RS = ACHAR(30), &  ! record separator
        US = ACHAR(31), &  ! unit separator
        DEL = ACHAR(127), &  ! delete
        UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
        LOWERCASE = 'abcdefghijklmnopqrstuvwxyz', &
        LETTERS = UPPERCASE // LOWERCASE, &
        DIGITS = '0123456789', &
        BINDIGITS = DIGITS(:2), &
        OCTDIGITS = DIGITS(:8), &
        HEXDIGITS = DIGITS // UPPERCASE(:5) // LOWERCASE(:5), &
        PUNCTUATION = '!"#$%&' // "'" // '()*+,-./:;<=>?@[' // ACHAR(92) // ']^_`{|}~', &
        WHITESPACE = TAB // LF // VT // FF // CR // ' ', &
        PRINTABLE = LETTERS // DIGITS // PUNCTUATION // WHITESPACE, &
        WORD_CHARS = LETTERS // '_' // DIGITS
#ifdef _UCS4
    character(len=*, kind=UCS4), parameter :: &
        WHITESPACE_UCS4 = CHAR(9, UCS4) // CHAR(10, UCS4) // CHAR(11, UCS4) // CHAR(12, UCS4) // CHAR(13, UCS4) // CHAR(32, UCS4) &
            // CHAR(160, UCS4) // CHAR(5760, UCS4) // CHAR(6158, UCS4) // CHAR(8192, UCS4) // CHAR(8193, UCS4) // CHAR(8194, UCS4) &
            // CHAR(8195, UCS4) // CHAR(8196, UCS4) // CHAR(8197, UCS4) // CHAR(8198, UCS4) // CHAR(8199, UCS4) &
            // CHAR(8200, UCS4) // CHAR(8201, UCS4) // CHAR(8202, UCS4) // CHAR(8203, UCS4) // CHAR(8232, UCS4) &
            // CHAR(8233, UCS4) // CHAR(8239, UCS4) // CHAR(8287, UCS4) // CHAR(12288, UCS4) // CHAR(65279, UCS4)
#endif
end module stdlib_ascii

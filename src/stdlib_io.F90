module stdlib_io
    use, intrinsic :: ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
    use stdlib_kinds
    use stdlib_ascii, only: LF, WHITESPACE
    use stdlib_base, only: Strip
    use stdlib_list, only: CharacterList

    implicit none

    private
    public :: ReadLine, ReadUnit, ReadFile, String2Lines, Split, Tokenize

    integer, parameter :: LEN_BUFFER = 256

contains
    subroutine ReadLine(unit, line, eof, success)
        integer, intent(in) :: unit
        logical, intent(out), optional :: eof, success
        character(len=:), allocatable, intent(out) :: line

        integer :: size_, iostat
        character(len=LEN_BUFFER) :: buffer

        if (PRESENT(eof)) eof = .false.
        if (PRESENT(success)) success = .true.
        iostat = 0
        line = ''
        do while (iostat /= IOSTAT_EOR)
            read(unit, '(a)', advance='no', size=size_, iostat=iostat) buffer
            if (iostat > 0) then
                if (PRESENT(success)) success = .false.
                return
            else if (iostat == IOSTAT_END) then
                if (PRESENT(eof)) eof = .true.
                return
            end if
            line = line // buffer(:size_)
        end do
    end subroutine ReadLine


    function ReadUnit(unit) result(string)
        integer, intent(in) :: unit
        character(len=:), allocatable :: string

        logical :: eof
        character(len=:), allocatable :: line

        string = ''
        do
            call ReadLine(unit, line, eof)
            string = string // line
            if (eof) exit
            string = string // LF
        end do
    end function ReadUnit


    function ReadFile(filename, delete) result(string)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: delete
        character(len=:), allocatable :: string

        logical :: delete_
        integer :: unit

        delete_ = .false.
        if (PRESENT(delete)) delete_ = delete

        open(newunit=unit, file=filename)
        string = ReadUnit(unit)
        if (delete_) then
            close(unit, status='delete')
        else
            close(unit)
        end if
    end function ReadFile


    function String2Lines(string, crop, feed, discard) result(lines)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: crop, feed
        character(len=*), intent(in), optional :: discard
        type(CharacterList) :: lines

        logical :: crop_
        integer :: i
        character(len=:), allocatable :: buffer, line

        crop_ = .false.
        if (PRESENT(crop)) crop_ = crop

        buffer = string
        do while (LEN(buffer) > 0)
            i = SCAN(buffer, LF)
            if (i == 0) i = LEN(buffer) + 1
            line = buffer(: i-1)
            buffer = buffer(i+1 :)
            if (PRESENT(discard)) then
                i = SCAN(line, discard)
                if (i > 0) line = line(: i-1)
            end if
            if (crop_) line = Strip(line)
            call lines%Append(line)
        end do
        if (crop_) then
            do i = -1, 1, 2
                do while (LEN_TRIM(lines%Get(i)) == 0)
                    call lines%Delete(i)
                end do
            end do
        end if
        if (PRESENT(feed)) then
            if (feed) call lines%Append('')
        end if
    end function String2Lines


    function Split(string, delimiter) result(tokens)
        character(len=*), intent(in) :: string, delimiter
        type(CharacterList) :: tokens

        integer :: lenDelimiter, i
        character(len=:), allocatable :: buffer

        lenDelimiter = LEN(delimiter)
        buffer = string
        do
            i = INDEX(buffer, delimiter)
            if (i == 0) exit
            call tokens%Append(buffer(: i-1))
            buffer = buffer(i+lenDelimiter :)
        end do
        call tokens%Append(buffer)
    end function Split


    function Tokenize(string, delimiter) result(tokens)
        character(len=*), intent(in) :: string
        character(len=*), intent(in), optional :: delimiter
        type(CharacterList) :: tokens

        integer :: i
        character(len=:), allocatable :: buffer, delimiter_

        delimiter_ = WHITESPACE
        if (PRESENT(delimiter)) delimiter_ = delimiter

        buffer = string
        do
            i = VERIFY(buffer, delimiter_)
            if (i == 0) exit
            buffer = buffer(i:)
            i = SCAN(buffer, delimiter_)
            if (i == 0) i = LEN(buffer) + 1
            call tokens%Append(buffer(: i-1))
            buffer = buffer(i:)
        end do
    end function Tokenize
end module stdlib_io

program kinds
    implicit none

    integer :: i, k, sk, mk
    character(len=3) :: c

    mk = 0

    print *, 'INTEGER kinds:'
    k = 0
    do i = 1, 64
        sk = SELECTED_INT_KIND(i)
        if (i > 1 .and. sk /= k) then
            select case (i - 1)
            case (2)
                c = 'K1'
            case (4)
                c = 'K2'
            case (9)
                c = 'K4'
            case (18)
                c = 'K8'
            case (38)
                c = 'K16'
            case default
                c = '???'
            end select
            print *, c, k, i - 1
        end if
        k = sk
        mk = MAX(mk, k)
    end do

    print *, ''

    print *, 'REAL kinds:'
    k = 0
    do i = 1, 64
        sk = SELECTED_REAL_KIND(i)
        if (i > 1 .and. sk /= k) then
            select case (i - 1)
            case (3)
                c = 'HP'
            case (6)
                c = 'SP'
            case (15)
                c = 'DP'
            case (18)
                c = 'XDP'
            case (31, 33)
                c = 'QP'
            case default
                c = '???'
            end select
            print *, c, k, i - 1
        end if
        k = sk
        mk = MAX(mk, k)
    end do

    print *, ''

    print *, 'CHARACTER kinds:'
    k = SELECTED_CHAR_KIND('ASCII')
    if (k > 0) print *, 'ASCII', k, '"ASCII"'
    mk = MAX(mk, k)
    k = SELECTED_CHAR_KIND('ISO_10646')
    if (k > 0) print *, 'UCS4 ', k, '"ISO_10646"'
    mk = MAX(mk, k)

    print *, ''

    print *, 'Defalut kinds:'
    print *, 'LOGICAL  ', KIND(.false.)
    print *, 'INTEGER  ', KIND(0)
    print *, 'REAL     ', KIND(0.)
    print *, 'COMPLEX  ', KIND((0., 0.))
    print *, 'CHARACTER', KIND('')

    print *, ''

    print *, 'Maximum kind:', mk
end program kinds

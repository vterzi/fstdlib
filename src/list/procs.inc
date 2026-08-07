#include "../inc/var.inc"
#include "../inc/cmp.inc"


#define _PROC _CAT2(_LIST,item)
function _PROC(self, index) result(item)
    class(_LIST), intent(in) :: self
    integer, intent(in) :: index
    type(_ITEM), pointer :: item

    integer :: idx

    idx = index
    item => self%head
    do while (idx > 1)
        item => item%next
        idx = idx - 1
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,getitem_index)
function _PROC(self, i) result(val)
    class(_LIST), intent(in) :: self
    integer, intent(in) :: i
    _RES_TYPE :: val

    type(_ITEM), pointer :: item

    item => self%item(wraparound_index(i, self%size, .true.))
    val = item%val
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,getitem_slice)
function _PROC(self, s) result(list)
    class(_LIST), intent(in) :: self
    type(slice), intent(in) :: s
    type(_LIST) :: list

    integer :: size, s_start, s_stop, s_step, idx, i
    type(_ITEM), pointer :: item, prev, next

    size = self%size
    s_start = wraparound_index(s%start, size, .true.)
    s_stop = wraparound_index(s%stop, size, .true.)
    s_step = s%step
    if (s_step == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (s_step > 0 .and. s_start <= s_stop) then
        idx = s_start
        item => self%item(idx)
        allocate(next)
        next%val = item%val
        list%head => next
        idx = idx + s_step
        do while (idx <= s_stop)
            do i = 1, s_step
                item => item%next
            end do
            prev => next
            allocate(next)
            next%val = item%val
            prev%next => next
            idx = idx + s_step
        end do
        list%tail => next
    else if (s_step < 0 .and. s_start >= s_stop) then
        s_step = -s_step
        i = s_start
        s_start = s_start - ((s_start - s_stop) / s_step) * s_step
        s_stop = i
        idx = s_start
        item => self%item(idx)
        allocate(prev)
        prev%val = item%val
        list%tail => prev
        idx = idx + s_step
        do while (idx <= s_stop)
            do i = 1, s_step
                item => item%next
            end do
            next => prev
            allocate(prev)
            prev%val = item%val
            prev%next => next
            idx = idx + s_step
        end do
        list%head => prev
    end if
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,setitem_index)
subroutine _PROC(self, key, val)
    class(_LIST), intent(inout) :: self
    integer, intent(in) :: key
    _ARG_TYPE, intent(in) :: val

    type(_ITEM), pointer :: item

    item => self%item(wraparound_index(key, self%size, .true.))
    item%val = val
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,setitem_slice_item)
subroutine _PROC(self, key, val)
    class(_LIST), intent(inout) :: self
    type(slice), intent(in) :: key
    _ARG_TYPE, intent(in) :: val

    integer :: size, key_start, key_stop, key_step, idx, i
    type(_ITEM), pointer :: item

    size = self%size
    key_start = wraparound_index(key%start, size, .true.)
    key_stop = wraparound_index(key%stop, size, .true.)
    key_step = key%step
    if (key_step == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (key_step < 0) then
        key_step = -key_step
        i = key_start
        key_start = key_start - ((key_start - key_stop) / key_step) * key_step
        key_stop = i
    end if
    if (key_start <= key_stop) then
        idx = key_start
        item => self%item(idx)
        item%val = val
        idx = idx + key_step
        do while (idx <= key_stop)
            do i = 1, key_step
                item => item%next
            end do
            item%val = val
            idx = idx + key_step
        end do
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,setitem_slice_list)
subroutine _PROC(self, key, val)
    class(_LIST), intent(inout) :: self
    type(slice), intent(in) :: key
    class(_LIST), intent(in) :: val

    logical :: reversed
    integer :: size_self, key_start, key_stop, key_step, size_key, size_val, &
        idx, i
    type(_ITEM), target :: sentinel
    type(_ITEM), pointer :: prev, next, self_item, val_item

    size_self = self%size
    key_start = wraparound_index(key%start, size_self, .false.)
    key_stop = wraparound_index(key%stop, size_self, .false.)
    key_step = key%step
    size_val = val%size
    if (key_step == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (key_step < 0) then
        reversed = .true.
        key_step = -key_step
        i = key_start
        key_start = key_start - ((key_start - key_stop) / key_step) * key_step
        key_stop = i
    else
        reversed = .false.
    end if
    key_start = min(max(key_start, 1), size_self + 1)
    key_stop = min(max(key_stop, 0), size_self)
    if (key_step == 1) then
        size_key = max(key_stop - key_start + 1, 0)
        sentinel%next => self%head
        prev => sentinel
        do i = 1, key_start - 1
            prev => prev%next
        end do
        next => prev%next
        do i = key_start, key_stop
            self_item => next
            next => next%next
            deallocate(self_item)
        end do
        val_item => val%head
        if (reversed) then
            do while (associated(val_item))
                allocate(self_item)
                self_item%val = val_item%val
                self_item%next => next
                next => self_item
                val_item => val_item%next
            end do
        else
            do while (associated(val_item))
                allocate(self_item)
                self_item%val = val_item%val
                prev%next => self_item
                prev => self_item
                val_item => val_item%next
            end do
        end if
        prev%next => next
        if (key_start == 1) self%head => sentinel%next
        if (key_stop == size_self) then
            if (reversed) then
                do while (associated(prev%next))
                    prev => prev%next
                end do
            end if
            if (.not. associated(prev, sentinel)) then
                self%tail => prev
            else
                nullify(self%tail)
            end if
        end if
        self%size = size_self - size_key + size_val
    else
        size_key = max((key_stop - key_start + key_step) / key_step, 0)
        if (size_key /= size_val) error stop "attempt to assign sequence to" &
            // " extended slice of incompatible size"  ! ValueError
        if (size_val > 0) then
            val_item => val%head
            if (reversed) then
                nullify(next)
                do while (associated(val_item))
                    allocate(prev)
                    prev%val = val_item%val
                    prev%next => next
                    next => prev
                    val_item => val_item%next
                end do
                val_item => prev
            end if
            self_item => self%item(key_start)
            self_item%val = val_item%val
            do idx = 2, size_val
                do i = 1, key_step
                    self_item => self_item%next
                end do
                val_item => val_item%next
                self_item%val = val_item%val
            end do
            if (reversed) then
                do while (associated(prev))
                    next => prev%next
                    deallocate(prev)
                    prev => next
                end do
            end if
        end if
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,setitem_slice_array)
subroutine _PROC(self, key, val)
    class(_LIST), intent(inout) :: self
    type(slice), intent(in) :: key
    _ARG_TYPE, intent(in) :: val(:)

    integer :: size_self, key_start, key_stop, key_step, size_key, size_val, &
        val_start, val_stop, val_step, size_diff, idx, i
    type(_ITEM), target :: sentinel
    type(_ITEM), pointer :: prev, next, item

    size_self = self%size
    key_start = wraparound_index(key%start, size_self, .false.)
    key_stop = wraparound_index(key%stop, size_self, .false.)
    key_step = key%step
    size_val = size(val)
    if (key_step == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (key_step < 0) then
        key_step = -key_step
        i = key_start
        key_start = key_start - ((key_start - key_stop) / key_step) * key_step
        key_stop = i
        val_start = size_val
        val_stop = 1
        val_step = -1
    else
        val_start = 1
        val_stop = size_val
        val_step = 1
    end if
    key_start = min(max(key_start, 1), size_self + 1)
    key_stop = min(max(key_stop, 0), size_self)
    if (key_step == 1) then
        size_key = key_stop - key_start + 1
        size_diff = size_val - size_key
        sentinel%next => self%head
        item => sentinel
        do i = 1, key_start - 1
            item => item%next
        end do
        idx = val_start
        do i = 1, min(size_val, size_key)
            item => item%next
            item%val = val(idx)
            idx = idx + val_step
        end do
        if (size_diff /= 0) then
            prev => item
            next => prev%next
            if (size_diff > 0) then
                do i = 1, size_diff
                    allocate(item)
                    item%val = val(idx)
                    idx = idx + val_step
                    prev%next => item
                    prev => item
                end do
            else if (size_diff < 0) then
                do i = 1, -size_diff
                    item => next
                    next => item%next
                    deallocate(item)
                end do
            end if
            prev%next => next
        end if
        if (key_start == 1) self%head => sentinel%next
        if (key_stop == size_self) then
            if (.not. associated(item, sentinel)) then
                self%tail => item
            else
                nullify(self%tail)
            end if
        end if
        self%size = size_self - size_key + size_val
    else
        size_key = max((key_stop - key_start + key_step) / key_step, 0)
        if (size_val /= size_key) error stop "attempt to assign sequence to" &
            // " extended slice of incompatible size"  ! ValueError
        if (size_val > 0) then
            item => self%item(key_start)
            item%val = val(val_start)
            do idx = val_start + val_step, val_stop, val_step
                do i = 1, key_step
                    item => item%next
                end do
                item%val = val(idx)
            end do
        end if
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,append)
elemental subroutine _PROC(self, object)
    class(_LIST), intent(inout) :: self
    _ARG_TYPE, intent(in) :: object

    type(_ITEM), pointer :: new

    allocate(new)
    new%val = object
    if (associated(self%tail)) then
        self%tail%next => new
    else
        self%head => new
    end if
    self%tail => new
    self%size = self%size + 1
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,clear)
elemental subroutine _PROC(self)
    class(_LIST), intent(inout) :: self

    type(_ITEM), pointer :: item, next

    item => self%head
    do while (associated(item))
        next => item%next
        deallocate(item)
        item => next
    end do
    nullify(self%head, self%tail)
    self%size = 0
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,copy)
subroutine _PROC(self, other)
    class(_LIST), intent(in) :: self
    type(_LIST), intent(inout) :: other

    type(_ITEM), pointer :: item, new

    item => self%head
    if (associated(item)) then
        new => other%head
        if (.not. associated(new)) then
            allocate(new)
            other%head => new
        end if
        new%val = item%val
        item => item%next
        do while (associated(item))
            if (.not. associated(new%next)) allocate(new%next)
            new => new%next
            new%val = item%val
            item => item%next
        end do
        other%tail => new
        item => new%next
        do while (associated(item))
            new => item%next
            deallocate(item)
            item => new
        end do
        other%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,count)
function _PROC(self, val) result(count)
    class(_LIST), intent(in) :: self
    _ARG_TYPE, intent(in) :: val
    integer :: count

    type(_ITEM), pointer :: item

    count = 0
    item => self%head
    do while (associated(item))
        if (_EQ(item%val,val)) count = count + 1
        item => item%next
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,extend_list)
subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    class(_LIST), intent(in) :: iterable

    type(_ITEM), pointer :: item, new

    item => iterable%head
    if (associated(item)) then
        allocate(new)
        new%val = item%val
        if (associated(self%tail)) then
            self%tail%next => new
        else
            self%head => new
        end if
        item => item%next
        do while (associated(item))
            allocate(new%next)
            new => new%next
            new%val = item%val
            item => item%next
        end do
        self%tail => new
        self%size = self%size + iterable%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,extend_array)
pure subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    _ARG_TYPE, dimension(:), intent(in) :: iterable

    integer :: n, i
    type(_ITEM), pointer :: new

    n = size(iterable)
    if (n > 0) then
        allocate(new)
        new%val = iterable(1)
        if (associated(self%tail)) then
            self%tail%next => new
        else
            self%head => new
        end if
        do i = 2, n
            allocate(new%next)
            new => new%next
            new%val = iterable(i)
        end do
        self%tail => new
        self%size = self%size + n
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,index)
function _PROC(self, val, start, stop) result(index)
    class(_LIST), intent(in) :: self
    _ARG_TYPE, intent(in) :: val
    integer, intent(in), optional :: start, stop
    integer :: index

    integer :: lo, up, i
    type(_ITEM), pointer :: item

    if (present(start)) then
        lo = start
    else
        lo = 1
    end if
    if (present(stop)) then
        up = stop
    else
        up = self%size
    end if
    index = 0
    item => self%head
    i = 1
    do while (associated(item))
        if (i > up) then
            exit
        else if (i >= lo .and. _EQ(item%val,val)) then
            index = i
            return
        end if
        item => item%next
        i = i + 1
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,insert)
subroutine _PROC(self, index, object)
    class(_LIST), intent(inout) :: self
    integer, intent(in) :: index
    _ARG_TYPE, intent(in) :: object

    integer :: size, idx
    type(_ITEM), pointer :: item, new

    size = self%size
    idx = wraparound_index(index, size, .false.)
    if (idx > size) then
        call self%append(object)
    else
        allocate(new)
        new%val = object
        if (idx < 1) then
            new%next => self%head
            self%head => new
        else
            item => self%item(idx)
            new%next => item%next
            item%next => new
        end if
        self%size = self%size + 1
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,pop)
function _PROC(self, index) result(val)
    class(_LIST), intent(inout) :: self
    integer, intent(in), optional :: index
    _RES_TYPE :: val

    logical :: not_first
    integer :: size, idx
    type(_ITEM), pointer :: item, old

    size = self%size
    if (size == 0) error stop "pop from empty list"  ! IndexError
    if (present(index)) then
        idx = wraparound_index(index, size, .true.)
    else
        idx = size
    end if
    not_first = idx > 1
    if (not_first) then
        item => self%item(idx - 1)
        old => item%next
        item%next => old%next
    else
        old => self%head
        self%head => old%next
    end if
    if (idx == size) then
        if (not_first) then
            self%tail => item
        else
            nullify(self%tail)
        end if
    end if
    val = old%val
    deallocate(old)
    self%size = self%size - 1
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,remove)
elemental subroutine _PROC(self, val)
    class(_LIST), intent(inout) :: self
    _ARG_TYPE, intent(in) :: val

    type(_ITEM), pointer :: item, old

    nullify(old)
    item => self%head
    if (associated(item)) then
        if (_EQ(item%val,val)) then
            old => item
            nullify(item)
            self%head => old%next
        else
            do while (associated(item%next))
                if (_EQ(item%next%val,val)) then
                    old => item%next
                    item%next => old%next
                    exit
                end if
                item => item%next
            end do
        end if
    end if
    if (associated(old)) then
        if (.not. associated(old%next)) self%tail => item
        deallocate(old)
        self%size = self%size - 1
    else
        error stop "List%remove(x): x not found"  ! ValueError
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,reverse)
subroutine _PROC(self)
    class(_LIST), intent(inout) :: self

    type(_ITEM), pointer :: item, next, head

    item => self%head
    self%tail => item
    nullify(head)
    do while (associated(item))
        next => item%next
        item%next => head
        head => item
        item => next
    end do
    self%head => head
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,sort)
subroutine _PROC(self)
    class(_LIST), intent(inout) :: self

    integer :: size, sorted_size, size1, size2, num, i
    type(_ITEM), target :: sentinel
    type(_ITEM), pointer :: tail, item1, item2, item

#if _TYPE_ID != _COMPLEX
    size = self%size
    sentinel%next => self%head
    tail => self%tail
    sorted_size = 1
    do while (sorted_size < size)
        tail => sentinel
        item1 => sentinel%next
        num = size
        do while (num > 0)
            size1 = min(sorted_size, num)
            num = num - size1
            item2 => item1
            do i = 1, size1
                item2 => item2%next
            end do
            size2 = min(sorted_size, num)
            num = num - size2
            do
                if (size2 == 0) then
                    tail%next => item1
                    do i = 1, size1
                        tail => tail%next
                    end do
                    item1 => item2
                    exit
                else if (size1 == 0) then
                    tail%next => item2
                    do i = 1, size2
                        tail => tail%next
                    end do
                    item1 => tail%next
                    exit
                end if
                if (_LE(item1%val,item2%val)) then
                    item => item1
                    item1 => item1%next
                    size1 = size1 - 1
                else
                    item => item2
                    item2 => item2%next
                    size2 = size2 - 1
                end if
                tail%next => item
                tail => item
            end do
        end do
        nullify(tail%next)
        sorted_size = sorted_size * 2
    end do
    self%head => sentinel%next
    self%tail => tail
#else
    if (self%size > 1) error stop "'<' not supported between instances of" &
        // " 'complex' and 'complex'"  ! TypeError
#endif
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,assign_list)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    class(_LIST), intent(in) :: other

    call other%copy(self)
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,assign_array)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    _ARG_TYPE, dimension(:), intent(in) :: other

    integer :: n, i
    type(_ITEM), pointer :: new

    n = size(other)
    if (n > 0) then
        allocate(new)
        new%val = other(1)
        self%head => new
        do i = 2, n
            allocate(new%next)
            new => new%next
            new%val = other(i)
        end do
        self%tail => new
        self%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,final)
elemental subroutine _PROC(self)
    type(_LIST), intent(inout) :: self

    call self%clear()
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(array_assign,_LIST)
subroutine _PROC(self, other)
    _TYPE_LEN(:), dimension(:), allocatable, intent(out) :: self
    class(_LIST), intent(in) :: other

    integer :: n, i
    type(_ITEM), pointer :: item

    n = size(other)
#if _TYPE_ID == _CHARACTER
    allocate(character(len=len(other)) :: self(n))
#else
    allocate(self(n))
#endif
    item => other%head
    do i = 1, n
        self(i) = item%val
        item => item%next
    end do
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(size,_LIST)
elemental function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    res = self%size
end function _PROC
#undef _PROC


#if _TYPE_ID == _LOGICAL
#  define _PROC _CAT2(all,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: item

    res = .true.
    item => self%head
    do while (associated(item))
        if (.not. item%val) then
            res = .false.
            return
        end if
        item => item%next
    end do
end function _PROC
#  undef _PROC


#  define _PROC _CAT2(any,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: item

    res = .false.
    item => self%head
    do while (associated(item))
        if (item%val) then
            res = .true.
            return
        end if
        item => item%next
    end do
end function _PROC
#  undef _PROC

#elif _TYPE_ID == _CHARACTER

#  define _PROC _CAT2(len,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    integer :: length
    type(_ITEM), pointer :: item

    res = 0
    item => self%head
    do while (associated(item))
        length = len(item%val)
        if (res < length) res = length
        item => item%next
    end do
end function _PROC
#  undef _PROC
#endif


#define _PROC _CAT2(_ITER,stop)
elemental function _PROC(self) result(res)
    class(_ITER), intent(in) :: self
    logical :: res

    res = .not. associated(self%item)
end function _PROC
#undef _PROC


#define _PROC _CAT2(_ITER,getitem)
pure function _PROC(self) result(res)
    class(_ITER), intent(in) :: self
    _RES_TYPE :: res

    if (self%stop()) error stop  ! StopIteration
    res = self%item%val
end function _PROC
#undef _PROC


#define _PROC _CAT2(_ITER,setitem)
elemental subroutine _PROC(self, val)
    class(_ITER), intent(inout) :: self
    _ARG_TYPE, intent(in) :: val

    if (self%stop()) error stop  ! StopIteration
    self%item%val = val
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_ITER,next)
elemental subroutine _PROC(self)
    class(_ITER), intent(inout) :: self

    if (self%stop()) error stop  ! StopIteration
    self%item => self%item%next
end subroutine _PROC
#undef _PROC


#define _PROC _CAT3(_ITER,assign,_LIST)
subroutine _PROC(self, other)
    class(_ITER), intent(out) :: self
    class(_LIST), intent(in) :: other

    self%item => other%head
end subroutine _PROC
#undef _PROC


#define _PROC _CAT3(_ITER,assign,_ITER)
subroutine _PROC(self, other)
    class(_ITER), intent(out) :: self
    class(_ITER), intent(in) :: other

    self%item => other%item
end subroutine _PROC
#undef _PROC

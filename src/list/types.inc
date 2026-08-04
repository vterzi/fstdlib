#undef _TYPE_PROC
#define _TYPE_PROC(T,P) procedure :: P => _CAT2(T,P)
#undef _TYPE_PRIVATE_PROC
#define _TYPE_PRIVATE_PROC(T,P) procedure, private :: P => _CAT2(T,P)
#undef _ELEM_TYPE
#if _TYPE_ID == _CHARACTER
#  define _ELEM_TYPE _TYPE(len=:, kind=_KIND), allocatable
#else
#  define _ELEM_TYPE _TYPE(_KIND)
#endif

type :: _ITEM
    private
    _ELEM_TYPE :: val
    type(_ITEM), pointer :: next => null()
end type _ITEM

type, public :: _LIST
    private
    integer :: size = 0
    type(_ITEM), pointer :: head => null(), tail => null()
contains
    _TYPE_PRIVATE_PROC(_LIST,item)
    _TYPE_PRIVATE_PROC(_LIST,getitem_index)
    _TYPE_PRIVATE_PROC(_LIST,getitem_slice)
    generic :: getitem => getitem_index, getitem_slice
    _TYPE_PRIVATE_PROC(_LIST,setitem_index)
    _TYPE_PRIVATE_PROC(_LIST,setitem_slice_item)
    _TYPE_PRIVATE_PROC(_LIST,setitem_slice_list)
    _TYPE_PRIVATE_PROC(_LIST,setitem_slice_array)
    generic :: setitem => setitem_index, setitem_slice_item, &
        setitem_slice_list, setitem_slice_array
    _TYPE_PROC(_LIST,append)
    _TYPE_PROC(_LIST,clear)
    _TYPE_PROC(_LIST,copy)
    _TYPE_PROC(_LIST,count)
    _TYPE_PRIVATE_PROC(_LIST,extend_list)
    _TYPE_PRIVATE_PROC(_LIST,extend_array)
    generic :: extend => extend_list, extend_array
    _TYPE_PROC(_LIST,index)
    _TYPE_PROC(_LIST,insert)
    _TYPE_PROC(_LIST,pop)
    _TYPE_PROC(_LIST,remove)
    _TYPE_PROC(_LIST,reverse)
    _TYPE_PROC(_LIST,sort)
    _TYPE_PRIVATE_PROC(_LIST,assign_list)
    _TYPE_PRIVATE_PROC(_LIST,assign_array)
    generic :: assignment(=) => assign_list, assign_array
    final :: _CAT2(_LIST,final)
end type _LIST

interface assignment(=)
    module procedure :: _CAT2(array_assign,_LIST)
end interface assignment(=)

interface size
    module procedure :: _CAT2(size,_LIST)
end interface size

#if _TYPE_ID == _LOGICAL
interface all
    module procedure :: _CAT2(all,_LIST)
end interface all

interface any
    module procedure :: _CAT2(any,_LIST)
end interface any

#elif _TYPE_ID == _CHARACTER

interface len
    module procedure :: _CAT2(len,_LIST)
end interface len
#endif

type, public :: _ITER
    private
    type(_ITEM), pointer :: item => null()
contains
    _TYPE_PROC(_ITER,stop)
    _TYPE_PROC(_ITER,getitem)
    _TYPE_PROC(_ITER,setitem)
    _TYPE_PROC(_ITER,next)
end type _ITER

interface assignment(=)
    module procedure :: _CAT3(_ITER,assign,_LIST)
    module procedure :: _CAT3(_ITER,assign,_ITER)
end interface assignment(=)

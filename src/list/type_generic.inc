procedure, private :: &
#ifdef _ARG3
        _CAT(_PREFIX,_ARG3), &
#endif
        _CAT(_PREFIX,_ARG2), &
        _CAT(_PREFIX,_ARG1)
generic :: _GENERIC_NAME => &
#ifdef _ARG3
        _CAT(_PREFIX,_ARG3), &
#endif
        _CAT(_PREFIX,_ARG2), &
        _CAT(_PREFIX,_ARG1)


#undef _PREFIX
#undef _GENERIC_NAME
#undef _ARG1
#undef _ARG2
#undef _ARG3

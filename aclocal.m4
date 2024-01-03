
# AC_ADD_CFLAG(flag_name)
# 
# This is like
# CFLAGS="$CFLAGS <flag_name>"
# but does it only if the compiler understands the command line flag.
# Should work for both C/CFLAGS and C++/CXXFLAGS

AC_DEFUN([AC_ADD_COMPILER_FLAG], [
    ac_saved_compiler_flags="$[]_AC_LANG_PREFIX[]FLAGS"
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -Werror $1"
    AC_COMPILE_IFELSE([
	AC_LANG_PROGRAM([], [return 0;])],
	[ _AC_LANG_PREFIX[]FLAGS="$ac_saved_compiler_flags $1"
	],
	[ _AC_LANG_PREFIX[]FLAGS="$ac_saved_compiler_flags"
	  AC_MSG_WARN([_AC_LANG_PREFIX compiler doesn't understand $1])
	])
])


# AC_ADD_IF_VALID_COMPILER_FLAG(var,flag_name)
#
# This is like
# var="$var <flag_name>"
# but does it only if the compiler understands the command line flag.
# Should work for both C and C++

AC_DEFUN([AC_ADD_IF_VALID_COMPILER_FLAG], [
    ac_saved_compiler_flags="$[]_AC_LANG_PREFIX[]FLAGS"
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -Werror $2"
    AC_COMPILE_IFELSE([
	AC_LANG_PROGRAM([], [return 0;])],
	[ $1="$[]$1 $2"
	],
	[ AC_MSG_WARN([_AC_LANG_PREFIX compiler doesn't understand $2])
	])

    _AC_LANG_PREFIX[]FLAGS="$ac_saved_compiler_flags"
])


# AC_EXTRACT_VERSION_INFO(file,major_name,minor_name,major_var,minor_var,combined_var,pad)
#
# AC_EXTRACT_VERSION_INFO(foo.h,VER_MAJ,VER_MIN,MAJOR,MINOR,VERSION,0)
# will extract lines like
# #define VER_MAJ 12
# #define VER_MIN 7
# from file foo.h, and set result variables as follows:
# MAJOR=12
# MINOR=7
# VERSION=1207

AC_DEFUN([AC_EXTRACT_VERSION_INFO], [
    $4=$[](grep $2 "$1" | sed 's/[[^0-9]]*\([[0-9]]*\)[[^0-9]]*/\1/')
    $5=$[](grep $3 "$1" | sed 's/[[^0-9]]*\([[0-9]]*\)[[^0-9]]*/\1/')
    if test -n "$[]$5"; then
        if test $[]$5 -le 9; then
            $6=$[]{$4}$7$[]$5
        else
            $6=$[]{$4}$[]$5
        fi
    else
        $5="?"
        $6=$[]$4
    fi
])


# AC_FOREACH_UNTIL(Var,Lines,Test)
# Do something for every line in Lines.
# Lines is a newline- or tab-separated list of strings.
# Var is the variable name that gets assigned in every iteration.
# If Test succeeds, the loop terminates with Var set to the success string.
# If all Tests fail, the loop terminates with Var unset.

AC_DEFUN([AC_FOREACH_UNTIL], [
    IFSorig=$[]IFS
    IFS=`printf '\n\t'`
    $1=
    for $1 in $2; do
        if
            $3
        then
	    break
	fi
	$1=
    done
    IFS=$[]IFSorig
])

dnl BEGIN LICENSE BLOCK
dnl Version: CMPL 1.1
dnl
dnl The contents of this file are subject to the Cisco-style Mozilla Public
dnl License Version 1.1 (the "License"); you may not use this file except
dnl in compliance with the License.  You may obtain a copy of the License
dnl at www.eclipse-clp.org/license.
dnl 
dnl Software distributed under the License is distributed on an "AS IS"
dnl basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
dnl the License for the specific language governing rights and limitations
dnl under the License. 
dnl 
dnl The Original Code is  The ECLiPSe Constraint Logic Programming System. 
dnl The Initial Developer of the Original Code is  Cisco Systems, Inc. 
dnl Portions created by the Initial Developer are
dnl Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
dnl 
dnl Contributor(s): ECRC GmbH
dnl Contributor(s): IC-Parc, Imperial College London
dnl Contributor(s): CrossCore Optimization
dnl 
dnl END LICENSE BLOCK
dnl
dnl ECLiPSe Configuration File
dnl
dnl Much of this file was previously sepia/src/configure.in
dnl

#----------------------------------------------------------------------
# General
#----------------------------------------------------------------------

# AC_INIT defines PACKAGE_NAME and PACKAGE_VERSION
AC_INIT([ECLiPSe], [7.2])
AC_PREREQ([2.59])
AC_COPYRIGHT([ECLiPSe is copyright Cisco Systems Inc, and open-sourced under eclipseclp.org/licence.html])
AC_REVISION([$Id: configure.ac,v 1.78 2017/09/08 14:07:48 jschimpf Exp $])
AC_CONFIG_SRCDIR([BUILD_ROTD])
AC_CONFIG_AUX_DIR([config])
AC_PREFIX_DEFAULT([\${top_srcdir}])


#----------------------------------------------------------------------
# Host
#----------------------------------------------------------------------

# Where we build
AC_CANONICAL_BUILD

# What we build for
AC_CANONICAL_HOST

# Map autoconf's canonical host names to ECLiPSe's ARCH.
# Please don't put anything else in this switch!
case $host in

    i?86-*-linux*)
    	arch=i386_linux
	;;
    x86_64-*-linux*)
    	arch=x86_64_linux
	;;
    i?86-*-solaris2.*)
    	arch=i386_sunos5
	;;
    x86_64-*-solaris2.*)
    	arch=x86_64_sunos5
	;;
    i?86-*-mingw* | i?86-*-cygwin*)
    	arch=i386_nt
	;;
    x86_64-*-mingw* | x86_64-*-cygwin*)
    	arch=x86_64_nt
	;;
    sparc*-*-sunos[1-4]*)
    	arch=sun4
	;;
    sparc*-*-solaris2.* | sparc*-*-sunos5*)
    	arch=sparc_sunos5
	;;
    powerpc*-*-darwin*)
	arch=ppc_macosx
	;;
    i?86-*-darwin*)
	arch=i386_macosx
	;;
    x86_64-*-darwin*)
        arch=x86_64_macosx
	;;
    alpha*-*-linux*)
	arch=alpha_linux
	;;
    armv7*-*-linux* | arm-*-linux-gnueabihf)
	arch=armv7_linux
	;;
    aarch64-*-linux*)
	arch=aarch64_linux
	;;
    *)
	AC_MSG_ERROR([Unknown architecture: $host])
	;;
esac

# Can we run the generated executable (for host) on the build machine?
if test "$host" = "$build"; then
    can_execute=true
else
    # special relaxed cases
    can_execute=false
    case $build in
	x86_64-*-linux*)
	    case $host in
		x86_64-*-linux* | i*86-*-linux*) can_execute=true ;;
	    esac ;;
	i?86-*-cygwin*)
	    case $host in
		i?86-*-cygwin* | i?86-*-mingw*) can_execute=true ;; 
	    esac ;;
	x86_64-*-cygwin*)
	    case $host in
		x86_64-*-cygwin* | x86_64-*-mingw*) can_execute=true ;;
	    esac ;;
	aarch64-*-linux*)
	    case $host in
		aarch64-*-linux* | armv7*-*-linux* | arm-*-linux-gnueabihf) can_execute=true ;;
	    esac ;;
    esac
fi
AC_MSG_NOTICE([Can we run the generated executables: $can_execute])

# If ARCH was set, make sure it matches what was detected
if test -z "$ARCH" ; then
    ARCH=$arch
else
    if test "$ARCH" != "$arch" ; then
        # config.guess does not seem to distinguish 32/64 bit PPC for the Mac
        # assume ARCH setting is what the user wants to build
        if test "$ARCH" == "ppc64_macosx" ; then
            arch=$ARCH
        else
	    AC_MSG_ERROR([Mismatch between target host ($host) and ECLiPSe arch ($ARCH)!!!])
        fi
    fi
fi

AC_MSG_NOTICE([Building for ECLiPSe architecture ----- $ARCH -----])

AC_DEFINE_UNQUOTED(HOSTARCH, "$ARCH")

# When cross-building, make sure we have a site defaults file,
# because many tests won't work!!!
if test "$host" != "$build"; then
    if ! test -r "$CONFIG_SITE"; then
	AC_MSG_WARN([Cross-compiling without site config file $CONFIG_SITE!!!])
    fi
fi

# Make sure we are using a reasonable CONFIG_SITE file
if test -r "$CONFIG_SITE"; then
    if basename "$CONFIG_SITE" | grep -v -q "^config.*\.$ARCH"; then
	AC_MSG_ERROR([Won't use "$CONFIG_SITE" for architecture $ARCH,
	    change or unset CONFIG_SITE])
    fi
else
    AC_MSG_NOTICE([No file $CONFIG_SITE])
fi

# This is how we tell the makefiles that we are cross-compiling
if $can_execute; then
    MAKE_TARGET=install_all
else
    MAKE_TARGET=install_cross
fi


#----------------------------------------------------------------------
# Defaults for variables
#----------------------------------------------------------------------

# For some cross-compilations, gcc's include path is in the wrong order.
# We can fix this by adding an explicit -isystem <dir> argument.
if test -z "$OS_INCLUDES"; then
    OS_INCLUDES=""
fi

# OPT_LDFLAGS will be prefixed to LDFLAGS if not --enable-debug
OPT_LDFLAGS="-s"


# DYNLDFLAGS is used when linking shared libraries
# In principle, we should also use -Wl,-Bsymbolic here, to link the
# references to global symbols within the library directly to the definition
# within the library, and avoiding strange bugs due to symbol clashes.
# However, this does not work properly with data symbols (like ec_),
# due to the following behaviour. Quoting from
# http://docs.sun.com/app/docs/doc/806-0641/6j9vuqujb?a=view:
#  "If a symbolically bound symbol is interposed upon, then references to
#   the symbol from outside of the symbolically bound object will bind to
#   the interposer, whereas the object itself is already bound internally. 
#   Essentially two symbols with the same name are now being referenced
#   from within the process.  A symbolically bound data symbol that
#   results in a copy relocation (see "Copy Relocations") creates the same
#   interposition situation."
DYNLDFLAGS="-shared"

# use with $CC for specifying runtime load path
RUNPATH_LDFLAG="-Wl,-rpath,"

# OPT_LOCAL_FLAGS will be appended to LOCALFLAGS if not --enable-debug
OPT_LOCAL_FLAGS="-DTHREADED"
OPTIMIZE=cp

#
# SYSLIBS contains libraries whose use is different on various
# platforms and which are not needed during autoconfiguration
# LIBS contains libraries needed to compile programs inside autoconf
SYSLIBS=""
GRAPHLIBS=""
OBJECT_SUFFIX=o
OBJECTS_SUFFIX=a
LIB_PREFIX=lib
DLIBDIR=lib
BUILDO=o

# Default memory block sizes (in Mega-Pwords)
VIRTUAL_HEAP_DEFAULT="4"
VIRTUAL_SHARED_DEFAULT="8"
VIRTUAL_LOCAL_STACK_DEFAULT="8"
VIRTUAL_GLOBAL_STACK_DEFAULT="32"
SHARED_MEM_OFFSET_HEAP="16*MB"

LDTCL=""
LOCK="lock.o"

LL_C_SO=".c.so:"
LD_C_SO="touch \$@"
PL_C_SO=""
PD_C_SO=""
PL_O_SO=""
PD_O_SO=""

WANT_MPS=n

dnl this must be before bsd on AIX
LIBS="-lm $LIBS"

if test -z "$LD"; then
    LD=ld
fi

CXXSUFFIX=cc

origostype=$OSTYPE

# define the broad type of os -- unix is the default, and currently the only
# other supported class is win
OSCLASS="unix"


# Find compiler CC, also sets OBJEXT and EXEEXT
if test -z "$CFLAGS"; then
    # stop AC_PROG_CC from initialising CFLAGS to something like -g -O2
    CFLAGS=" "
fi

AC_PROG_CC


# -D_REENTRANT is e.g. for making use of errno thread-safe
#	(we could use -pthread but that isn't supported everywhere)
AC_ADD_COMPILER_FLAG(-D_REENTRANT)

# -fPIC is recommended for shared libraries
AC_ADD_COMPILER_FLAG(-fPIC)

# -fstrict-overflow breaks some code that does integer overflow detection
AC_ADD_COMPILER_FLAG(-fno-strict-overflow)

# suppress warnings
AC_ADD_COMPILER_FLAG(-Wno-logical-op-parentheses)
AC_ADD_COMPILER_FLAG(-Wno-bitwise-op-parentheses)
AC_ADD_COMPILER_FLAG(-Wno-pointer-sign)


# Notes on OPT_FLAGS* -- these settings are for gcc (tested with gcc 5.4 - 7.1)
# -fomit-frame-pointer gives faster code for x86
# no -fomit-frame-pointer from optimised debugging -- it makes debugging
# impossible on some machines (otherwise -O1+ will turn it on) 
OPTDBG_FLAGS="-O2"

OPTDBG_FLAGS_EMU=$OPTDBG_FLAGS
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -fno-strict-aliasing)
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -fno-schedule-insns)
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -fno-schedule-insns2)
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -fira-loop-pressure)
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -fschedule-fusion)
AC_ADD_IF_VALID_COMPILER_FLAG(OPTDBG_FLAGS_EMU, -frename-registers)

OPT_FLAGS="$OPTDBG_FLAGS"
AC_ADD_IF_VALID_COMPILER_FLAG(OPT_FLAGS, -fomit-frame-pointer)

OPT_FLAGS_EMU="$OPTDBG_FLAGS_EMU"
AC_ADD_IF_VALID_COMPILER_FLAG(OPT_FLAGS_EMU, -fomit-frame-pointer)


# needed for strip on Mac OS X 
STRIPFLAGS=

JAVA_PATH_SEP=:

#----------------------------------------------------------------------
# Explicit settings - things that can't be figured out automatically
#----------------------------------------------------------------------
case "$ARCH" in
 i386_macosx)
    MACOS_VERSION_MIN=10.9
    # it seems that the stdc++/gcc libs that needs to be linked in are at
    # gcc version dependent directories. The following tries to determine
    # the exact path using gcc to obtain its version information
    # Next line should work for clang and gcc:
    GCCLIBDIR=`dirname \`$CC -print-libgcc-file-name\``
    if test -z "$GCCLIBDIR" ; then
	[GCC_VERSION=`$CC -v 2>&1 | sed -n 's/^[^ ]\+ version \([0-9.]\+\).*$/\1/p'`]
	AC_MSG_NOTICE([GCC_VERSION is $GCC_VERSION])
	GCCLIBDIR=`ls -d /usr/lib/gcc/i686*/$GCC_VERSION 2>/dev/null`
	if test -z "$GCCLIBDIR" ; then
	    GCCLIBDIR=.
	fi
    fi
    AC_MSG_NOTICE([GCCLIBDIR is $GCCLIBDIR])
    CFLAGS="-m32  -mmacosx-version-min=$MACOS_VERSION_MIN $CFLAGS"
    LOCAL_FLAGS="-DD_LOAD -no-cpp-precomp -dynamic"
    OBJECT_SUFFIX=dylib
    OBJECTS_SUFFIX=dylib
    # these needed to be changed to build at SourceForge
    LDFLAGS="-m32 -mmacosx-version-min=$MACOS_VERSION_MIN"
    # -dynamiclib is the official Apple way of producing dynamic libraries
    # -shared works on 10.6, but apparently not 10.5
    DYNLDFLAGS="-dynamiclib -single_module -flat_namespace -m32 -mmacosx-version-min=$MACOS_VERSION_MIN -L$GCCLIBDIR"
    LD_C_SO=""
    PL_C_SO=".c.dylib:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.dylib:"
    PD_O_SO="$LD_C_SO"
    # gcc can be used in recent Mac OS X
    DYLD="gcc"
    DYLDCXX="g++"
    # LDTCL for loading Tcl/Tk, implemented as a Framework
    LDTCL="-framework Tcl"
    LDD="otool"
    LDDFLAG="-L"
    STRIPFLAGS="-u -i -S"
    LOCK=
    OSTYPE="darwin"
    ;;
 x86_64_macosx)
    MACOS_VERSION_MIN=10.9
    # it seems that the stdc++/gcc libs that needs to be linked in are at
    # gcc version dependent directories. The following tries to determine
    # the exact path using gcc to obtain its version information
    # Next line should work for clang and gcc:
    GCCLIBDIR=`dirname \`$CC -print-libgcc-file-name\``
    if test -z "$GCCLIBDIR" ; then
	[GCC_VERSION=`$CC -v 2>&1 | sed -n 's/^[^ ]\+ version \([0-9.]\+\).*$/\1/p'`]
	AC_MSG_NOTICE([GCC_VERSION is $GCC_VERSION])
	GCCLIBDIR=`ls -d /usr/lib/gcc/x86_64*/$GCC_VERSION 2>/dev/null`
	if test -z "$GCCLIBDIR" ; then
	    GCCLIBDIR=.
	fi
    fi
    AC_MSG_NOTICE([GCCLIBDIR is $GCCLIBDIR])
    CFLAGS="-m64  -mmacosx-version-min=$MACOS_VERSION_MIN $CFLAGS"
    LOCAL_FLAGS="-DD_LOAD -no-cpp-precomp -dynamic"
    OBJECT_SUFFIX=dylib
    OBJECTS_SUFFIX=dylib
    # these needed to be changed to build at SourceForge
    LDFLAGS="-m64 -mmacosx-version-min=$MACOS_VERSION_MIN"
    # -dynamiclib is the official Apple way of producing dynamic libraries
    # -shared works on 10.6, but apparently not 10.5
    DYNLDFLAGS="-dynamiclib -single_module -flat_namespace -m64 -mmacosx-version-min=$MACOS_VERSION_MIN -L$GCCLIBDIR"
    LD_C_SO=""
    PL_C_SO=".c.dylib:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.dylib:"
    PD_O_SO="$LD_C_SO"
    DYLD="gcc"
    DYLDCXX="g++"
    # LDTCL for loading Tcl/Tk, implemented as a Framework
    LDTCL="-framework Tcl"
    LDD="otool"
    LDDFLAG="-L"
    STRIPFLAGS="-u -i -S"
    LOCK=
    OSTYPE="darwin"
    ;;
 ppc_macosx)
    GCC_VERSION=`gcc -v 2>&1 | sed -n 's/gcc version //p' | sed -e 's/\([[^ ]]*\) .*/\1/'`
    GCCLIBDIR=$(ls -d /usr/lib/gcc/powerpc*/$GCC_VERSION 2>/dev/null)
    if test $? -ne 0; then
	GCCLIBDIR="."
    fi
    # this is needed to catch the case of empty GCCLIBDIR (David Dreisgmeyer)
    if [ -z $GCCLIBDIR ]; then
        GCCLIBDIR="."
    fi
    CFLAGS="-m32  -mmacosx-version-min=10.3 $CFLAGS"
    LOCAL_FLAGS="-DD_LOAD -no-cpp-precomp -dynamic -m32"
    OBJECT_SUFFIX=dylib
    OBJECTS_SUFFIX=dylib
    # these needed to be changed to build at SourceForge
    #LDFLAGS="-Wl,-search_paths_first"
    #DYNLDFLAGS="-dynamic -single_module -flat_namespace -search_paths_first -lgcc"
    # It seems that PPC MacOSX have some problems linking that requires 
    # libgcc to be linked in to define some undefined symbols, but the 
    # method to do this varies with the OS versions:
    # -lgcc (10.2)
    # -lcc_dynamic (10.3? -- suggested on the web, but not tested by us)
    # -L$GCCLIBDIR -lgcc (10.4), where $GCCLIBDIR is a directory in 
    # /usr/lib/gcc, which the script tries to determine. The flag
    # -arch_only ppc is also required, as the libgcc.a cannot be linked 
    # for ppc64 or Intel
    LDFLAGS="-m32"
    DYNLDFLAGS="-dynamic -single_module -flat_namespace  -search_paths_first -L$GCCLIBDIR  -lgcc -arch_only ppc"
    LD_C_SO=""
    PL_C_SO=".c.dylib:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.dylib:"
    PD_O_SO="$LD_C_SO"
    DYLD="libtool"
    DYLDCXX="libtool"
    # LDTCL for loading Tcl/Tk, implemented as a Framework
    #LDTCL="-framework tcl"
    LDTCL=" -framework Tcl"
    # -lSystemStubs needed for PPC MacOs X 10.4.9 we built on
    LIBS="$LIBS -lSystemStubs"
    # RUNPATH_LDLAG is really a noop here
    RUNPATH_LDFLAG="-L"
    LDD="otool"
    LDDFLAG="-L"
    STRIPFLAGS="-u -i -S"
    LOCK=
    ;;
 ppc64_macosx)
    GCC_VERSION=`gcc -v 2>&1 | sed -n 's/gcc version //p' | sed -e 's/\([[^ ]]*\) .*/\1/'`
    GCCLIBDIR=$(ls -d /usr/lib/gcc/powerpc*/$GCC_VERSION/ppc64 2>/dev/null)
    if test $? -ne 0; then
	GCCLIBDIR="."
    fi
    # this is needed to catch the case of empty GCCLIBDIR (David Dreisgmeyer)
    if [ -z $GCCLIBDIR ]; then
        GCCLIBDIR="."
    fi
    CFLAGS="-m64  -mmacosx-version-min=10.4 $CFLAGS"
    LOCAL_FLAGS="-DD_LOAD -no-cpp-precomp -dynamic -m64"
    OBJECT_SUFFIX=dylib
    OBJECTS_SUFFIX=dylib
    # these needed to be changed to build at SourceForge
    #LDFLAGS="-Wl,-search_paths_first"
    #DYNLDFLAGS="-dynamic -single_module -flat_namespace -search_paths_first -lgcc"
    # It seems that PPC MacOSX have some problems linking that requires 
    # libgcc to be linked in to define some undefined symbols, but the 
    # method to do this varies with the OS versions:
    # -lgcc (10.2)
    # -lcc_dynamic (10.3? -- suggested on the web, but not tested by us)
    # -L$GCCLIBDIR -lgcc (10.4), where $GCCLIBDIR is a directory in 
    # /usr/lib/gcc, which the script tries to determine. The flag
    # -arch_only ppc is also required, as the libgcc.a cannot be linked 
    # for ppc64 or Intel
    LDFLAGS="-m64"
    DYNLDFLAGS="-dynamic -single_module -flat_namespace  -search_paths_first -L$GCCLIBDIR  -lgcc -arch_only ppc64"
    LD_C_SO=""
    PL_C_SO=".c.dylib:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.dylib:"
    PD_O_SO="$LD_C_SO"
    DYLD="libtool"
    DYLDCXX="libtool"
    # LDTCL for loading Tcl/Tk, implemented as a Framework
    #LDTCL="-framework tcl"
    LDTCL=" -framework Tcl"
    # -lSystemStubs needed for PPC MacOs X 10.4.9 we built on
    LIBS="$LIBS -lSystemStubs"
    # RUNPATH_LDLAG is really a noop here
    RUNPATH_LDFLAG="-L"
    LDD="otool"
    LDDFLAG="-L"
    STRIPFLAGS="-u -i -S"
    LOCK=
    ;;
 sun4)
    LOCAL_FLAGS="-Dsun4_0 -DD_LOAD"
    OPT_LDFLAGS="-Xlinker -x"
    LDFLAGS="-static"
    RUNPATH_LDFLAG="-L"

    WANT_MPS=y
    ;;
 supersparc_sunos4)
    CFLAGS="-msupersparc $CFLAGS"
    LOCAL_FLAGS="-Dsun4_0 -DD_LOAD"
    LDFLAGS="-Xlinker -Bstatic"
    RUNPATH_LDFLAG="-L"

    WANT_MPS=y
    ;;
 sparc_sunos5)
    LOCAL_FLAGS="-DD_LOAD"
    AC_DEFINE(SVR4)
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -G \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    WANT_MPS=y
    # some bash shell set OSTYPE to solaris2.10
    OSTYPE="solaris"
    AC_DEFINE(HAVE_MAP_NORESERVE)
    ;;
 i386_sunos5)
    CFLAGS="-m32 $CFLAGS"
    LDFLAGS="-m32"
    DYNLDFLAGS="-m32 -G"
    LOCAL_FLAGS="-DD_LOAD"
    AC_DEFINE(SVR4)
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -G \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    WANT_MPS=y
    OSTYPE="solaris"
    AC_DEFINE(HAVE_MAP_NORESERVE)
    ;;
 x86_64_sunos5)
    CFLAGS="-m64 $CFLAGS"
    LDFLAGS="-m64 "
    DYNLDFLAGS="-m64 -shared"
    LOCAL_FLAGS="-DD_LOAD"

    AC_DEFINE(SVR4)
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    WANT_MPS=y
    AC_DEFINE(HAVE_MAP_NORESERVE)
    ;;
 alpha_linux)
    LOCAL_FLAGS="-DD_LOAD"
    FLOAT_ROUND_FLAGS="-mfp-rounding-mode=d"
    CFLAGS="-mieee $CFLAGS"
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    ;;
 i386_linux)
    OPTIMIZE="sh ../src/opt_i386_linux.sh"
    LOCAL_FLAGS="-DD_LOAD"
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    CFLAGS="-m32 $CFLAGS"
    LDFLAGS="$LDFLAGS -m32"
    DYNLDFLAGS="$DYNLDFLAGS -m32"
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    WANT_MPS=y
    # the check for mman.h does not work in my linux release because
    # of a compiler warning. The following fix fakes the cache value:
    ac_cv_header_sys_mman_h=yes
    # at least some i386 linux libcs have this bug:
    AC_DEFINE(ATOF_NEGZERO_BUG)
    # some bash shell set OSTYPE to gnu-linux
    OSTYPE="linux"  
    ;;
 x86_64_linux)
    OPTIMIZE="sh ../src/opt_i386_linux.sh"
    LOCAL_FLAGS="-DD_LOAD"
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    WANT_MPS=y
    # some bash shell set OSTYPE to gnu-linux
    OSTYPE="linux"  
    ;;
 armv7_linux | aarch64_linux)
    LOCAL_FLAGS="-DD_LOAD"
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    # char is unsigned in ARM
    AC_ADD_COMPILER_FLAG(-fsigned-char)
    LDFLAGS="$LDFLAGS"
    DYNLDFLAGS="$DYNLDFLAGS"
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    # some bash shell set OSTYPE to gnu-linux
    OSTYPE="linux"  
    ;;
 i386_nt)
    ac_cv_func_select=yes
    ac_cv_func_gethostname=yes
    ac_cv_func_getpagesize=yes

    OPTIMIZE="sh ../src/opt_i386_linux.sh"
    OBJECT_SUFFIX=dll
    OBJECTS_SUFFIX=dll
    LIB_PREFIX=
    DLIBDIR=bin
    dnl wsock32 is for select(), gethostname() and sockets
    dnl netapi32 is for the Netbios() function
    LIBS="-lwsock32 -lnetapi32 $LIBS"
    OSTYPE="win32"
    OSCLASS="win"
    JAVA_PATH_SEP=";"
    ;;
 x86_64_nt)
    CFLAGS="$CFLAGS"
    OBJECT_SUFFIX=dll
    OBJECTS_SUFFIX=dll
    LIB_PREFIX=
    DLIBDIR=bin
    DYLD="\$(CC)"
    DYLDCXX="\$(CXX)"
    dnl ws2_32 is for select(), gethostname() and sockets
    dnl netapi32 is for the Netbios() function
    LIBS="-lws2_32 -lnetapi32 $LIBS"
    OSTYPE="win64"
    OSCLASS="win"
    JAVA_PATH_SEP=";"
    dnl nextafter(-0.0,0.0) gives nonzero!
    AC_DEFINE(HAVE_NEXTAFTER_ZERO_BUG)
    ;;
 i386_dynix3)
    LOCAL_FLAGS="-DD_LOAD"
    ;;
 dec5000)
    LOCAL_FLAGS="-DD_LOAD"
    ;;
 alpha_osf32)
    CFLAGS="-taso -Olimit 4500 -ieee_with_inexact $CFLAGS"
    OPT_FLAGS_EMU=-O2
    LDFLAGS=-taso
    DYNLDFLAGS="-G -taso"
    LOCAL_FLAGS="-DD_LOAD"
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_O_SO=".o.so:"
    PD_O_SO="ld -shared \$< -o \$@"
    DYLD = "\$(CC)"
    DYLDCXX = "\$(CXX)"
    WANT_MPS=y
    ;;
 rs_aix32)
    CFLAGS="-fsigned-char $CFLAGS"
    OPT_FLAGS_EMU=" "
    OPT_FLAGS_ARITH=" "
    LOCAL_FLAGS="-DD_LOAD"
    LDFLAGS="-Xlinker -bexport:../../Kernel/src/expsymtab"
    AC_DEFINE(AIX)
    AC_DEFINE(_BSD,44)
    AC_DEFINE(_ALL_SOURCE)
    AC_DEFINE(SMALL_SPACE)
    AC_DEFINE(STACK_BASE,0x30000000)
    ;;
 rs_aix32_cc)
    CC=cc
    CFLAGS="-qchars=signed $CFLAGS"
    OPT_FLAGS_EMU=" "
    OPT_FLAGS_ARITH=" "
    LOCAL_FLAGS="-DD_LOAD"
    LDFLAGS="-Wl,-bexport:../../Kernel/src/expsymtab"
    AC_DEFINE(AIX)
    AC_DEFINE(_BSD,44)
    AC_DEFINE(_ALL_SOURCE)
    AC_DEFINE(SMALL_SPACE)
    AC_DEFINE(STACK_BASE,0x30000000)
    ;;
 rs_aix41)
    CFLAGS="-fsigned-char $CFLAGS"
    OPT_FLAGS_EMU=" "
    OPT_FLAGS_ARITH=" "
    LOCAL_FLAGS="-DD_LOAD"
    LDFLAGS="-Xlinker -bexport:../../Kernel/src/expsymtab"
    AC_DEFINE(AIX)
    AC_DEFINE(_BSD,44)
    AC_DEFINE(_ALL_SOURCE)
    AC_DEFINE(SMALL_SPACE)
    ;;
 hp700_ux90)
    LOCAL_FLAGS="-DD_LOAD"
    LIBS="-L/lib/pa1.1 $LIBS"
    LDFLAGS="-Xlinker -a -Xlinker archive"
    AC_DEFINE(SVR4)
    ;;
 spp_ux90)
    LOCAL_FLAGS="-DD_LOAD"
    LIBS="-L/lib/pa1.1 $LIBS"
    LDFLAGS="-Xlinker -a -Xlinker archive"
    AC_DEFINE(SVR4)
    ;;
 sgi_irix52)
    CFLAGS="-signed -Olimit 4300 -mips2 $CFLAGS"
    LOCAL_FLAGS="-DD_LOAD"
    AC_DEFINE(SVR4)
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LL_C_SO=""
    LD_C_SO=""
    PL_O_SO=".o.so:"
    PD_O_SO="ld -shared \$< -o \$@"
    DYLD="ld"
    DYLDCXX="ld"
    WANT_MPS=y
    ;;
 next_mach)
    LOCAL_FLAGS="-DNO_SAVE"
    ;;
 m88k_svr4)
    OPT_FLAGS_EMU=-O
    LOCAL_FLAGS="-DD_LOAD -DGETTIME1"
    SYSLIBS="-ltermcap $SYSLIBS"
    AC_DEFINE(SVR4)
#    BUILDO=so		# problems with the dynamic linker
    OBJECT_SUFFIX=so
    OBJECTS_SUFFIX=so
    LD_C_SO="\$(CC) \$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared \$< -o \$@"
    PL_C_SO=".c.so:"
    PD_C_SO="$LD_C_SO"
    PL_O_SO=".o.so:"
    PD_O_SO="$LD_C_SO"
    DYNLDFLAGS="\$(OPT_FLAGS) \$(CFLAGS) \$(CPPFLAGS) -shared"
    WANT_MPS=y
    # FIND_X is buggy, but not path is needed, so the default one will do
    x_libraries=/usr/lib
    ;;
 *)
    echo $ARCH:Unknown architecture
    exit
    ;;
esac


if test -z "$OSTYPE"; then
   AC_MSG_ERROR([Unable to determine OSTYPE])
else
   if test "$host" != "$build"; then
      # if cross-compiling, should not be using the shell's OSTYPE
      if test "$origostype" == "$OSTYPE"; then
          AC_MSG_ERROR([OSTYPE not set for cross-compile]) 
      fi
   fi
   AC_MSG_NOTICE([OSTYPE is $OSTYPE])
fi

#----------------------------------------------------------------------
# checks for programs
#----------------------------------------------------------------------

if test -z "$DYLD"; then
    DYLD="\$(CC)"
    DYLDCXX="\$(CXX)"
fi

if test -z "$LDD"; then
    LDD="ldd"
    LDDFLAG=
fi

AC_PROG_CPP
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_AWK
AC_PATH_PROG(AR,ar,ar,[$PATH:/usr/ccs/bin])
AC_PROG_RANLIB
AC_PROG_LN_S
AC_CHECK_PROG(SED,sed,sed,false)
AC_CHECK_TOOL([STRIP],[strip],[:])

# Compiler characteristics

AC_C_CONST
AC_C_INLINE
AC_C_VOLATILE
AC_C_VARARRAYS
AC_C_BIGENDIAN
AC_C_CHAR_UNSIGNED
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(long long,8)
AC_CHECK_SIZEOF(char *,4)
AC_CHECK_SIZEOF(long *,4)

AC_MSG_CHECKING(whether compiler supports computed gotos)
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([], [[void *p = &&label; goto *p; label:]])],
    [AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_COMPUTED_GOTO)],
    [AC_MSG_RESULT(no)])


AC_CHECK_FUNC(__fpu_control, [], [AC_CHECK_LIB(ieee,main)])
AC_CHECK_LIB(dl,dlopen)
AC_CHECK_LIB(nsl,main)
AC_CHECK_LIB(socket,socket)

# LDFLAGS is used when linking executables
# Check to see if we can include GNU linker's specific flags:
# --export-dynamic replaces obsolete -rdynamic option
# It is only meaningful when linking an executable, and makes executable's
# symbols visible in the dynamic symbol table (which would otherwise only
# contain symbols exported from dynamically linked libraries). It is
# probably only useful when making a statically linked eclipse, rather
# than one using libeclipse.so, but probably doesn't hurt otherwise.
AC_MSG_CHECKING(for version-dependent linker options)
SAVED_LDFLAGS="$LDFLAGS"
LDFLAGS="-Wl,--export-dynamic $LDFLAGS"
AC_LINK_IFELSE([AC_LANG_PROGRAM([],[return 0;])], 
	[AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no) 
	LDFLAGS="$SAVED_LDFLAGS"])


# RPATH_ORIGIN: instruct the linker to set an rpath (i.e. where to
# look for needed shared libraries) relative to the linked object.
# INSTALL_NAME: set install_name for MacOS -- this is needed to
# force these libraries to get looked up in the rpath.

if test "$OBJECT_SUFFIX" = "dylib" ; then
    RPATH_ORIGIN="${RUNPATH_LDFLAG}@loader_path"
    INSTALL_NAME="-Wl,-install_name,@rpath/\$(@F)"
elif test "$OBJECT_SUFFIX" = "dll" ; then
    RPATH_ORIGIN=
    INSTALL_NAME=
else
    AC_MSG_CHECKING(if linker understands ORIGIN)
    SAVED_LDFLAGS="$LDFLAGS"
    # Need an extra $ for quoting in Makefile:
    RPATH_ORIGIN="${RUNPATH_LDFLAG}'\$\$ORIGIN' -Wl,-z,origin"
    LDFLAGS="${RUNPATH_LDFLAG}'\$ORIGIN' -Wl,-z,origin $LDFLAGS"
    # If unsuccessful, we set rpath to "." as a last resort
    AC_LINK_IFELSE([AC_LANG_PROGRAM([],[return 0;])], 
	[AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no) 
	RPATH_ORIGIN="${RUNPATH_LDFLAG}."])
    LDFLAGS="$SAVED_LDFLAGS"
    INSTALL_NAME=
fi

#----------------------------------------------------------------------
# The --enable-xxx options
#----------------------------------------------------------------------
AC_ARG_ENABLE(debug,
	[AS_HELP_STRING([--enable-debug],[
	    compile with debugging features (ARG=opt for debugging optimised code)])],
	AC_MSG_NOTICE([Configuring for debug=$enable_debug])
	[if test "x$enable_debug" != "xopt"; then 
	    [OPT_FLAGS_EMU=-g OPT_FLAGS_ARITH=-g OPT_FLAGS=-g LOCAL_FLAGS="$LOCAL_FLAGS -DPRINTAM"]
         else
	    [OPT_FLAGS_EMU="$OPTDBG_FLAGS_EMU -g" OPT_FLAGS_ARITH="$OPT_FLAGS_ARITH -g" OPT_FLAGS="$OPTDBG_FLAGS -g" LOCAL_FLAGS="$LOCAL_FLAGS -DPRINTAM"]
         fi
	 STRIP=":" OPTIMIZE=cp],
	[NICE=nice LDFLAGS="$OPT_LDFLAGS $LDFLAGS" LOCAL_FLAGS="$LOCAL_FLAGS $OPT_LOCAL_FLAGS"]
)

if test -z "$OPT_FLAGS_EMU"; then
	OPT_FLAGS_EMU="$OPT_FLAGS"
fi
if test -z "$OPT_FLAGS_ARITH"; then
	OPT_FLAGS_ARITH="$OPT_FLAGS"
fi

#----------------------------------------------------------------------
# The --with-readline option
#----------------------------------------------------------------------
#AC_ARG_WITH(readline,
#	[  --with-readline         support fancy command line editing],
#	LOCAL_FLAGS="$LOCAL_FLAGS -DHAVE_READLINE" SYSLIBS="-lreadline -lhistory -ltermcap $SYSLIBS",
#	[])


AC_DEFINE_UNQUOTED(OBJECT_SUFFIX_STRING, "$OBJECT_SUFFIX")
AC_DEFINE_UNQUOTED(VIRTUAL_HEAP_DEFAULT, $VIRTUAL_HEAP_DEFAULT)
AC_DEFINE_UNQUOTED(VIRTUAL_SHARED_DEFAULT, $VIRTUAL_SHARED_DEFAULT)
AC_DEFINE_UNQUOTED(VIRTUAL_LOCAL_STACK_DEFAULT, $VIRTUAL_LOCAL_STACK_DEFAULT)
AC_DEFINE_UNQUOTED(VIRTUAL_GLOBAL_STACK_DEFAULT, $VIRTUAL_GLOBAL_STACK_DEFAULT)
AC_DEFINE_UNQUOTED(SHARED_MEM_OFFSET_HEAP, $SHARED_MEM_OFFSET_HEAP)

#----------------------------------------------------------------------
# Programs
#----------------------------------------------------------------------

echo checking how to process .S files
rm -f conftest*
cat > conftest.S <<EOF
#include "confdefs.h"
l:
EOF
/bin/cp conftest.S conftest1.S
err=`eval "($CC -c -I. -I../include conftest.S >/dev/null) 2>&1"`
if test -z "$err" && test -f conftest.o; then
  S_TO_O='$(CC) $(CPPFLAGS) $(CFLAGS) -c $<'
else
  rm -f conftest.o
  /bin/cp conftest1.S conftest.S
  err=`eval "(/lib/cpp -P -I. -I../include conftest.S >conftest.s;$CC -c conftest.s) 2>&1"`
  if test -z "$err"; then
    S_TO_O='/lib/cpp $(CPPFLAGS) -P $< > $*.s; $(CC) $(CFLAGS) -c $*.s'
  else
    rm -f conftest.o
    err=`eval "($CC -E -P -I. -I../include conftest.S >conftest.s; $CC -c conftest.s) 2>&1"`
    if test -z "$err"; then
      S_TO_O='$(CC) $(CPPFLAGS) -E -P $< > $*.s; $(CC) $(CFLAGS) -c $*.s'
    else
	rm -f conftest.o
	err=`eval "($CC -E -I. -I../include conftest.S >conftest.s; $CC -c conftest.s) 2>&1"`
	if test -z "$err"; then
	  S_TO_O='$(CC) $(CPPFLAGS) -E $< > $*.s; $(CC) $(CFLAGS) -c $*.s'
	else
	  echo "...I have found no way to process the .S files"
	fi
    fi
  fi
fi
rm -f conftest*
test -n "$verbose" && echo "	.S will be processed by $S_TO_O"


#----------------------------------------------------------------------
# Header files
#----------------------------------------------------------------------
dnl Check how to read a directory
AC_HEADER_DIRENT
dnl Check for ANSI C header files and string operations
AC_HEADER_STDC
AC_CHECKING(access types)
AC_TRY_COMPILE([#include <unistd.h>], [
    (void) access("/", R_OK);
    ], AC_DEFINE(ACCESS_IN_UNISTD))
dnl Check if sbrk is declared in unistd.h
AC_EGREP_HEADER(sbrk, [unistd.h],, AC_DEFINE(SBRK_UNDEF))
AC_EGREP_HEADER(gethostid, [unistd.h],, AC_DEFINE(GETHOSTID_UNDEF))
AC_EGREP_HEADER(strtol, [unistd.h],, AC_DEFINE(STRTOL_UNDEF))
AC_EGREP_HEADER(memcpy, [string.h], AC_DEFINE(MEMCPY_STRING),
    AC_EGREP_HEADER(memcpy, [memory.h],AC_DEFINE(MEMCPY_MEMORY),))
AC_EGREP_HEADER(PATH_MAX, limits.h, AC_DEFINE(PATH_IN_LIMITS))

AC_CHECK_HEADERS(ctype.h memory.h string.h unistd.h sys/mman.h sys/param.h \
		sys/systeminfo.h sys/utsname.h netdb.h fpu_control.h \
		fcntl.h limits.h windows.h)

dnl C99 headers
AC_CHECK_HEADERS(fenv.h inttypes.h stdatomic.h stdint.h)


#
# Typedefs
#
dnl Check for pid_t
AC_TYPE_PID_T
dnl Check the return type of the signal handler function
AC_TYPE_SIGNAL

dnl integer types
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_INT8_T
AC_TYPE_UINT8_T
AC_TYPE_INT32_T
AC_TYPE_UINT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_TYPE_INTPTR_T
AC_TYPE_UINTPTR_T

AC_CHECK_TYPES([long long, __int64, int128_t, __int128])


#----------------------------------------------------------------------
# Library functions
#----------------------------------------------------------------------
dnl AC_FUNC_VFORK - this seems too strong for our purposes - bad on Solaris 2.x
AC_CHECK_FUNC(vfork, , AC_DEFINE(vfork, fork) echo "...No vfork available")
AC_CHECK_FUNCS(strerror bcopy memcpy memmove gethrtime localtime_r)
AC_CHECK_FUNCS(getcwd gethostid uname getrusage getwd random)
AC_CHECK_FUNCS(sigvec sigstack siginterrupt)
AC_CHECK_FUNCS(sigaction sigaltstack sigprocmask)
AC_CHECK_FUNCS(setsid sysconf sysinfo tcgetattr times)
AC_CHECK_FUNCS(fpclassify isfinite isinf isnan signbit sincos)
AC_CHECK_FUNC(alarm, AC_DEFINE(HAVE_ALARM), echo "...Alarm not found")
AC_CHECK_FUNC(pipe, AC_DEFINE(HAVE_PIPE), echo "...No pipes available")
AC_CHECK_FUNC(rename, AC_DEFINE(HAVE_RENAME), echo "...Rename not found")
AC_CHECK_FUNC(setitimer, AC_DEFINE(HAVE_SETITIMER), echo "...Setitimer not found")
AC_CHECK_FUNC(putenv, AC_DEFINE(HAVE_PUTENV), echo "...Putenv not found")
AC_CHECK_FUNC(sleep, AC_DEFINE(HAVE_SLEEP), echo "...Sleep not found")
AC_CHECK_FUNC(vsprintf, AC_DEFINE(HAVE_VARARGS), echo "...Vsprintf not found")
AC_CHECK_FUNCS(vsprintf vsnprintf)
AC_CHECK_FUNCS(fpsetround ieee_flags)
AC_CHECK_FUNCS(realpath)

AC_SEARCH_LIBS(clock_gettime,rt)
AC_CHECK_FUNCS(clock_gettime)

dnl The following require wsock32 on Windows, and the check doesn't work:
dnl this doesn't work: AC_SEARCH_LIBS(select, wsock32)
dnl this doesn't work: AC_SEARCH_LIBS(gethostname, wsock32)
AC_CHECK_FUNCS(select gethostname)

AC_FUNC_MMAP

AC_CHECK_DECLS([sigsetjmp],,,[[#include <setjmp.h>]])

AC_MSG_CHECKING([for pow(zero,neg) bug])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
double (*ppow)(double,double) = pow;
]],[[
return (*ppow)(0.0,-2.0) < 0.0 ? 0 : 1;
]])],
[AC_MSG_RESULT(yes)
 AC_DEFINE(HAVE_POW_ZERO_NEG_BUG)],
[AC_MSG_RESULT(no)],
[AC_MSG_RESULT(maybe)]
)

dnl check if rint() correctly rounds to even and to -0.0
AC_MSG_CHECKING(for working rint)
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
double (*print)(double) = rint;
]],[[
exit((*print)(1.5)==2.0 && (*print)(2.5)==2.0 && 1/(*print)(-0.2)<0.0 ? 0 : 1);
]])],
[AC_MSG_RESULT(yes)
 AC_DEFINE(HAVE_RINT)],
[AC_MSG_RESULT(no)],
[AC_MSG_RESULT(maybe)
 AC_CHECK_FUNC(rint, AC_DEFINE(HAVE_RINT))]
)

dnl check if trunc() correctly rounds to -0.0
AC_MSG_CHECKING(for working trunc)
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
double (*ptrunc)(double) = trunc;
]],[[
/* Intel Mac OS X (10.4) has buggy trunc (>-1.0,<-0.0 gives +0.0) */
exit((*ptrunc)(1.6)==1.0 && 1/(*ptrunc)(-0.3)<0 ? 0 : 1);
]])],
[AC_MSG_RESULT(yes) 
 AC_DEFINE(HAVE_TRUNC)],
[AC_MSG_RESULT(no)],
[AC_MSG_RESULT(maybe)
 AC_CHECK_FUNC(trunc, AC_DEFINE(HAVE_TRUNC))]
)

dnl check if ceil() correctly rounds to -0.0
AC_MSG_CHECKING(for working ceil)
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
double (*pceil)(double) = ceil;
]],[[
exit((*pceil)(1.1)==2.0 && 1/(*pceil)(-0.3)<0 ? 0 : 1);
]])],
[AC_MSG_RESULT(yes) 
 AC_DEFINE(HAVE_CEIL)],
[AC_MSG_RESULT(no)],
[AC_MSG_RESULT(maybe)
 AC_CHECK_FUNC(trunc, AC_DEFINE(HAVE_CEIL))]
)

AC_CACHE_CHECK(whether sprintf returns length, ec_cv_func_sprintf,
    AC_TRY_RUN([
#include <stdio.h>
int main() {char buf; exit(sprintf(&buf,""));}
], ec_cv_func_sprintf=yes, ec_cv_func_sprintf=no, ec_cv_func_sprintf=yes))
if test "$ec_cv_func_sprintf" = yes; then
    AC_DEFINE(SPRINTF_RETURNS_LENGTH)
fi

AC_COMPILE_CHECK(fstat, [#include <sys/stat.h>], [
struct stat buf;
(void) fstat(0, &buf);
],
[AC_MSG_RESULT(yes)
 AC_DEFINE(HAVE_FSTAT)],
[AC_MSG_RESULT(no)])

AC_COMPILE_CHECK(svr4 dynamic loading, [#include <dlfcn.h>], [
(void) dlopen("/", 0);
],
[AC_MSG_RESULT(yes)
 AC_DEFINE(HAVE_DLOPEN)],
[AC_MSG_RESULT(no)])

AC_CHECK_HEADERS(mach-o/dyld.h)

AC_COMPILE_CHECK(nlist, [#include <nlist.h>], [
struct nlist nl;
(void) nlist("/", &nl);
],
[AC_MSG_RESULT(yes)
 AC_DEFINE(HAVE_NLIST)],
[AC_MSG_RESULT(no)])

echo checking for sgihrclock
AC_TRY_RUN([
#include <sys/types.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
#include <errno.h>

main()
{
__psunsigned_t cycleval;
if(syssgi(SGI_QUERY_CYCLECNTR, &cycleval) == -1
   && errno == ENODEV)
	exit(-1);
}], AC_DEFINE(HAVE_SGIHRCLOCK,1), :,:)

dnl The ':' arguments are to say do nothing in these cases

echo checking for sgi64bitclock
AC_TRY_RUN([
#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
#include <errno.h>

typedef unsigned long long iotimer_t;

main()
{
__psunsigned_t phys_addr, raddr, cycleval;
volatile iotimer_t start_value, *iotimer_addr;
int fd, poffmask;
poffmask = getpagesize() - 1;
if((phys_addr = syssgi(SGI_QUERY_CYCLECNTR, &cycleval)) == -1
   && errno == ENODEV)
	exit(-1);
raddr = phys_addr & ~poffmask;
fd = open("/dev/mmem", O_RDONLY);
iotimer_addr = (volatile iotimer_t *)mmap(0, poffmask, PROT_READ,
					    MAP_PRIVATE, fd, (__psint_t)raddr);
iotimer_addr = (iotimer_t *)((__psunsigned_t)iotimer_addr + (phys_addr & poffmask));
start_value = *iotimer_addr;
}], AC_DEFINE(HAVE_SGI64BITCLOCK,1), :,:)

#
# Structures
#
dnl Check if struct stat contains st_blksize
AC_STRUCT_ST_BLKSIZE

AC_COMPILE_CHECK([n_un in struct nlist],
[#include <a.out.h>] , [struct nlist n; n.n_un.n_name;],
[AC_MSG_RESULT(yes)
 AC_DEFINE(N_NAME, n_un.n_name)],
[AC_MSG_RESULT(no)
 AC_DEFINE(N_NAME, n_name)])


#----------------------------------------------------------------------
# System services
#----------------------------------------------------------------------

dnl Check for /dev/zero
AC_CACHE_CHECK(for /dev/zero, ec_cv_dev_zero,
    if test -r /dev/zero; then ec_cv_dev_zero=yes; else ec_cv_dev_zero=no; fi)
if test "$ec_cv_dev_zero" = yes; then
    AC_DEFINE(HAVE_DEV_ZERO)
fi

echo "checking if times() returns elapsed real time"
AC_TRY_RUN([#include <sys/types.h>
#include <sys/times.h>
int main() {
struct tms buf;
exit( times(&buf) ? 1:0 );}], AC_DEFINE(BSD_TIMES),:,:)
AC_CHECK_PROG(UPT, uptime, uptime, '')
AC_DEFINE_UNQUOTED(UPTIME, $UPT)

echo check if we can limit cputime
check="/bin/csh -c 'limit cputime 10' >/dev/null 2>&1"
if eval $check; then
    AC_DEFINE(HAVE_CPU_LIMIT)
fi

#
# Check how to get the page size
#
AC_COMPILE_CHECK(getpagesize, [], [
(void) getpagesize();
], AC_DEFINE(HAVE_GETPAGESIZE) vpage=1)
if test -z "$vpage"; then
#SVR4
AC_COMPILE_CHECK(sysconf1, [#include <unistd.h>], [
(void) sysconf(_SC_PAGESIZE);
], AC_DEFINE(SYSCONF_PAGE, _SC_PAGESIZE) vpage=1)
fi
if test -z "$vpage"; then
#AIX
AC_COMPILE_CHECK(sysconf2, [#include <unistd.h>], [
(void) sysconf(_SC_PAGE_SIZE);
], AC_DEFINE(SYSCONF_PAGE, _SC_PAGE_SIZE) vpage=1)
fi
if test -z "$vpage"; then
    echo "...don't know how to get the page size, using 4096"
fi

#
# Check if we can get a signal when data is ready
#
# BSD
AC_CACHE_CHECK(for bsd_sigio, ec_cv_sys_bsd_sigio,
AC_COMPILE_CHECK(, [#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>], [
int i, fd;
(void) fcntl(fd, F_SETOWN, getpid());
i = fcntl(fd, F_GETFL, 0);
(void) fcntl(fd, F_SETFL, i | FASYNC);
], ec_cv_sys_bsd_sigio=yes, ec_cv_sys_bsd_sigio=no))

#HP-UX
AC_CACHE_CHECK(for hp_ux_sigio, ec_cv_sys_hp_ux_sigio,
AC_COMPILE_CHECK(, [#include <sys/types.h>
#include <sys/ioctl.h>], [
int i, fd, pid;
pid = (int) getpid();
(void) ioctl(fd, SIOCSPGRP, &pid);
i = 1;
(void) ioctl(fd, FIOASYNC, &i);
], ec_cv_sys_hp_ux_sigio=yes, ec_cv_sys_hp_ux_sigio=no))

#SVR4
AC_CACHE_CHECK(for sigpoll, ec_cv_sys_sigpoll,
AC_COMPILE_CHECK(, [#include <sys/types.h>
#include <stropts.h>
#include <sys/conf.h>], [
int i, fd;
i = ioctl(fd, I_SETSIG, S_RDNORM|S_RDBAND|S_HIPRI|S_BANDURG);
], ec_cv_sys_sigpoll=yes, ec_cv_sys_sigpoll=no))

if test "$ec_cv_sys_bsd_sigio" = yes; then
    AC_DEFINE(SIGIO_FASYNC)
elif test "$ec_cv_sys_hp_ux_sigio" = yes; then
    AC_DEFINE(SIGIO_FIOASYNC)
elif test "$ec_cv_sys_sigpoll" = yes; then
    AC_DEFINE(SIGIO_SETSIG)
else
    echo "...SIGIO not available"
fi

#
# Check if the signal action is reset to SIG_DLF before entering the handler
#
echo checking if signal action is reset before entering the handler
AC_TRY_RUN([#include <signal.h>
handler (n) {exit signal(n, SIG_IGN) == SIG_DFL;}
main () {
signal (SIGINT, handler);
kill (getppid (), SIGINT);
}
], AC_DEFINE(HANDLER_RESET),:,:)

#
# Check if mprotect can be used to test for an area already allocated
#
echo checking if mprotect can be used to check for allocated pages
AC_TRY_RUN([#include	<sys/types.h>
#include	<sys/mman.h>
#include	<errno.h>
main () {
return !(mprotect((char *) 0x3bffc000, 16384, PROT_READ|PROT_WRITE) == -1 &&
	errno == ENOMEM);
}
], AC_DEFINE(HAVE_MPROTECT), echo "...mprotect does not work",
echo "...mprotect not tested")

#
# Terminal I/O
#
AC_COMPILE_CHECK(termio, [#include <termio.h>], [
struct termio   rawbuf;
int		fd;
rawbuf.c_iflag = tbuf->c_iflag & ~(INLCR | ICRNL);
(void) ioctl(fd, TCGETA, &rawbuf);
], AC_DEFINE(TERMIO_SYS_V_STYLE))

AC_TRY_RUN([#include <termio.h>
int main() {
char	c = '\n';
int res = ioctl(0, TIOCSTI, &c);
if (res == 0) read(0, &c, 1);
return res;}
], AC_DEFINE(HAVE_PUSHBACK), :, :)


AC_DEFINE(SOCKETS)

AC_CHECK_HEADER([sys/un.h], AC_DEFINE(HAVE_AF_UNIX), [])


dnl Check if we can get Ethernet Adapter Address 
AC_CACHE_CHECK(if we can get Ethernet Adapter Address, ec_cv_siocgifhwaddr,
    AC_TRY_COMPILE([#include <sys/ioctl.h>
#include <net/if.h>], [
	int fd;
	struct ifreq req;
	(void) ioctl(fd, SIOCGIFHWADDR, &req);
	],
	ec_cv_siocgifhwaddr=yes, ec_cv_siocgifhwaddr=no))

if test "$ec_cv_siocgifhwaddr" = yes; then
    AC_DEFINE(HAVE_SIOCGIFHWADDR)
fi


dnl Check if we can access registers from within a signal handler
AC_CACHE_CHECK(if we can access registers from within a signal handler, ec_cv_ucontextgregs,
    AC_TRY_COMPILE([#include <signal.h>
#include <ucontext.h>], [[
	ucontext_t ctx;
	greg_t x = ctx.uc_mcontext.gregs[0];
	]],
	ec_cv_ucontextgregs=yes, ec_cv_ucontextgregs=no))

if test "$ec_cv_ucontextgregs" = yes; then
    AC_DEFINE(HAVE_UCONTEXTGREGS)
fi

#----------------------------------------------------------------------
# Linking libstdc++ 
#----------------------------------------------------------------------

AC_MSG_CHECKING(test whether we can statically link lstdc++ into a shared library)
AC_LANG_PUSH([C++])
SAVED_LDFLAGS="$LDFLAGS"
STDCXXLIB="-static-libstdc++ -static-libgcc"
LDFLAGS="$LDFLAGS $DYNLDFLAGS $STDCXXLIB"
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([#include<iostream>], [return 0;])],
    [AC_MSG_RESULT(yes)],
    [AC_MSG_RESULT(no)
     STDCXXLIB=])
LDFLAGS="$SAVED_LDFLAGS"
AC_LANG_POP([C++])

AC_MSG_CHECKING(test whether we can statically link libgcc)
SAVED_LDFLAGS="$LDFLAGS"
GCCLIB="-static-libgcc"
LDFLAGS="$LDFLAGS $DYNLDFLAGS $GCCLIB"
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([#include <stdio.h>], [return 0;])],
    [AC_MSG_RESULT(yes)],
    [AC_MSG_RESULT(no)
     GCCLIB=])
LDFLAGS="$SAVED_LDFLAGS"

# For Windows, statically link libgcc to avoid dependency on extra dlls
if test "$OBJECTS_SUFFIX" = dll && test -n "$GCCLIB"; then
    AC_MSG_NOTICE([Linking libgcc statically])
    DYNLDFLAGS="$DYNLDFLAGS $GCCLIB"
fi


#----------------------------------------------------------------------
# Check for various third party components
#----------------------------------------------------------------------

ECLIPSETHIRDPARTY=${ECLIPSETHIRDPARTY:-"/vol/Eclipse/thirdparty"}


#----------
# TCL
#----------


AC_ARG_WITH([tcl], [AS_HELP_STRING([--without-tcl],[
	Build ECLiPSe without the tkeclipse development environment
	and without the Tcl/Tk interface (not recommended).])],
    [tcl_requested="$withval"], [tcl_requested=yes])

if test "$tcl_requested" = "yes"; then

    TCL_REQUIRED="8.6 8.5 8.4 8.3"

    if test "$OSCLASS" != "win" ; then
	dnl find X includes and libraries
	AC_PATH_X
	if test ! -z "$x_libraries"; then
	    GRAPHLIBS="-L$x_libraries -lX11 $GRAPHLIBS"
	else
	    GRAPHLIBS="-lX11 $GRAPHLIBS"
	fi
	if test ! -z "$x_includes"; then
	    OS_INCLUDES="-I$x_includes $OS_INCLUDES"
	fi
    fi
    found_tcl=
    for TCL_VERSION in $TCL_REQUIRED; do
	if test "$OSCLASS" != "win"; then
	    tcl_lib_version=$TCL_VERSION
	else
	    tcl_lib_version=`echo $TCL_VERSION|tr -d .`
	fi
	# look in thirdparty directory first
	TCL_ARCH_DIR="$ECLIPSETHIRDPARTY/tcltk$TCL_VERSION/$ARCH"
	if test -d "$TCL_ARCH_DIR/lib" ; then
	    found_tcl=yes
	    TCL_DIR="$TCL_ARCH_DIR"
	    TCL_INCL_DIR="$TCL_DIR/include"
	    break
	fi
	# check in standard locations
	if test "$OSCLASS" = "win"; then
	    TCL_DIR="$PROGRAMFILES/Tcl"
	    if test -d "$TCL_DIR/lib/tcl$TCL_VERSION" ; then
		found_tcl=yes
		TCL_ARCH_DIR="$TCL_DIR"
		TCL_INCL_DIR="$TCL_DIR/include"
		break
	    fi
	else
	    # See if we can find Tcl without specifying locations
	    ac_save_LIBS=$LIBS
	    LIBS="-ltcl$tcl_lib_version -ltk$tcl_lib_version $LIBS"
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <tcl.h>],[])], 
	                   [found_tcl=yes
                            LIBS=$ac_save_LIBS
		            # We can link tcl/tk with no special library path.
		            TCL_ARCH_DIR=
		            TCL_INCL_DIR=
		            GRAPHLIBS="-ltcl$tcl_lib_version -ltk$tcl_lib_version $GRAPHLIBS"
		            break],
			   [LDFLAGS="$SAVED_LDFLAGS"])

	    TCL_DIRS="/usr /usr/local"
	    for TCL_DIR in $TCL_DIRS; do
		if test -d "$TCL_DIR/lib/tcl$TCL_VERSION" ; then
		    found_tcl=yes
		    TCL_ARCH_DIR="$TCL_DIR"
		    TCL_INCL_DIR="$TCL_DIR/include/tcl$TCL_VERSION"
		    GRAPHLIBS="-L$TCL_ARCH_DIR/lib -ltcl$tcl_lib_version -ltk$tcl_lib_version $GRAPHLIBS"
		    break
		fi
	    done
	    if test -n "$found_tcl"; then
		break
	    fi 
	fi
    done
    if test -z "$found_tcl"; then
	AC_MSG_WARN([Could not find usable Tcl ($TCL_REQUIRED) in $ECLIPSETHIRDPARTY or elsewhere])
	BUILD_WITH_TCL="no"
    else
	AC_MSG_NOTICE([Using Tcl $TCL_VERSION in $TCL_DIR])
	AC_DEFINE(HAVE_TCL)
	AC_DEFINE(HAVE_TK)
	GRAPHLIBS="-L\"$TCL_ARCH_DIR/lib\" -ltcl$tcl_lib_version -ltk$tcl_lib_version $GRAPHLIBS"
	if test -n "$TCL_INCL_DIR"; then
	    OS_INCLUDES="-I\"$TCL_INCL_DIR\" $OS_INCLUDES"
	fi
	BUILD_WITH_TCL="yes"
    fi

else
    AC_MSG_WARN([Not building any components that need Tcl/Tk])
fi


#----------
# PTHREADS (don't use on Windows, even if available)
#----------

if test "$OSCLASS" != "win"; then
    AC_CHECK_HEADERS(pthread.h)
    AC_CHECK_LIB(pthread,pthread_create)
fi


#----------
# GMP
#----------

AC_ARG_WITH([gmp], [AS_HELP_STRING([--without-gmp],
	[Build ECLiPSe without bignum/rational support
	(Not recommended - compiler will be restricted).
	Default is to try finding GMP to build this support.])],
    [gmp_requested="$withval"], [gmp_requested=yes])

if test "$gmp_requested" = "yes"; then
    # try versionless first, then versions in order
    GMP_REQUIRED="6.2 4.2 4.1"
    for gmp_version in "" $GMP_REQUIRED; do
	GMP_DIR="$ECLIPSETHIRDPARTY/gmp$gmp_version"
	for f in "$GMP_DIR"/$ARCH/$DLIBDIR/*gmp*.$OBJECTS_SUFFIX ; do
	    if test -f "$f"; then
		GMP_LIBDIR=$(dirname "$f")
		GMP_LIB=$(basename "$f")
		case $OSCLASS in
		    unix) gmp_libname=$(basename "$f" .$OBJECTS_SUFFIX | sed -e 's/^lib//g') ;;
		    win) gmp_libname=$(basename "$f" .$OBJECTS_SUFFIX) ;;
		esac
		GMP_LD="-L$GMP_LIBDIR -l$gmp_libname"

		if test -d "$GMP_DIR/$ARCH/include"; then
		    OS_INCLUDES="-I$GMP_DIR/$ARCH/include $OS_INCLUDES"
		else
		    OS_INCLUDES="-I$GMP_DIR/include $OS_INCLUDES"
		fi

		# MacOS X does not support -rpath until 10.5; so to run on
		# earlier Mac OS X, we link in static GMP for sepia
		# executable to avoid setting DYLD_LIBRARY_PATH
		if test "$ARCH" = "i386_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
		elif test "$ARCH" = "ppc_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
		elif test "$ARCH" = "x86_64_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
		else
		    GMP_EXE_LD="$GMP_LD"
		fi

		INSTALL_GMP=install_gmp
		found_mp=gmp
		AC_DEFINE(HAVE_LIBGMP)
		break 2
	    fi
	done
    done
    if test -z "$found_mp"; then
        # try finding MPIR instead
	MPIR_REQUIRED="1.3.1"
    	for mpir_version in $MPIR_REQUIRED; do
	    MPIR_DIR=$ECLIPSETHIRDPARTY/mpir$mpir_version
	    if test -d $MPIR_DIR/$ARCH/lib; then
	        found_mp=mpir
	    	AC_DEFINE(HAVE_LIBGMP)
	    	AC_DEFINE(USING_MPIR)
	    	if test -d "$MPIR_DIR/$ARCH/include"; then
		    OS_INCLUDES="-I$MPIR_DIR/$ARCH/include $OS_INCLUDES"
	       	else
		    OS_INCLUDES="-I$MPIR_DIR/include $OS_INCLUDES"
	    	fi
	    	GMP_LIBDIR="$MPIR_DIR/$ARCH/lib"
		GMP_LIB="gmp"
	    	GMP_LD="-L$GMP_LIBDIR -l$GMP_LIB"
	        # MacOS X does not support -rpath until 10.5; so to run on
                # earlier Mac OS X, we link in static GMP for sepia
	        # executable to avoid setting DYLD_LIBRARY_PATH
		if test "$ARCH" = "i386_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
	        elif test "$ARCH" = "ppc_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
	        elif test "$ARCH" = "x86_64_macosx"; then
		    GMP_EXE_LD="$GMP_LIBDIR/lib$GMP_LIB.a"
	        else
		    GMP_EXE_LD="$GMP_LD"
	        fi
	    	INSTALL_GMP=install_gmp
		break
	    fi
	done
    fi	
    if test -z "$found_mp"; then
	AC_MSG_WARN([Could not find Gmp ($GMP_REQUIRED) in $ECLIPSETHIRDPARTY])
	# set to something, so we can use -R$(GMP_LIBDIR) without syntax error
	GMP_LIBDIR="."

	# If we have nothing in ECLIPSETHIRDPARTY, try if it's a standard lib.
	# Defines only -lgmp and HAVE_LIBGMP, no attempts to package the gmp lib
	# with ECLiPSe.  Check for mpz_import to require a new enough version.
	AC_CHECK_LIB(gmp, __gmpz_import)
    elif test "$found_mp" = "gmp"; then
	AC_MSG_NOTICE([Using Gmp in $GMP_DIR])
    else		     
	AC_MSG_NOTICE([Using MPIR in $MPIR_DIR])
    fi
else
    AC_MSG_NOTICE([Not using Gmp, not building bignum/rational support])
    # set to something, so we can use -R$(GMP_LIBDIR) without syntax error
    GMP_LIBDIR="."
fi


#----------
# Osi
#----------

AC_ARG_WITH([osi], [AS_HELP_STRING([[--with-osi[=versions]]],[
	Build ECLiPSe interface to COIN-OR Osi.  You can
	optionally specify a version list, e.g. "clpcbc symclp".
	Default is to build all available versions.])],
    [osi_versions_requested="$withval"], [osi_versions_requested=yes])

OSIBASEDIR="$ECLIPSETHIRDPARTY/coin"
OSICLP_ADDLIB=""
OSICLP_ADDDEF=""
if test "$osi_versions_requested" != "no"; then
    if test "$osi_versions_requested" = "yes"; then
    	osi_versions_requested="clpcbc symclp"
    fi
    for version in $osi_versions_requested; do
	found_osi_version=
	dnl For backward compatibility, allow ${version}amd as well
	OSIDIRS="$OSIBASEDIR${version}amd $OSIBASEDIR$version"
	for OSIDIR in $OSIDIRS; do
	    if test -d "$OSIDIR/$ARCH/lib"; then
		found_osi_version=yes
		OSI_VERSIONS="$OSI_VERSIONS $version"
		AC_MSG_NOTICE([Using OSI $version in $OSIDIR])
		case $version in
		clpcbc) CLPCBCDIR="$OSIDIR" ;;
		symclp) SYMCLPDIR="$OSIDIR" ;;
		glpk)  OSIGLPKDIR="$OSIDIR" ;;
		esac
		ls -d $OSIDIR/$ARCH/lib/*amd* >/dev/null 2>/dev/null
		if test $? -eq 0; then
		     AC_MSG_NOTICE([Found AMD support for $version])
		     OSICLP_ADDLIB="-lamd"
		     OSICLP_ADDDEF="-DUFL_BARRIER"
		fi
		break
	    fi
	done
	if test -z "$found_osi_version"; then
	    AC_MSG_WARN([Could not find $ARCH support for $version in $OSIDIRS])
	fi
    done
fi
if test -n "$OSI_VERSIONS"; then
    AC_MSG_NOTICE([Will build OSI interfaces for versions $OSI_VERSIONS])
    INSTALL_EPLEX=install_eplex
else
    AC_MSG_NOTICE([Not building any OSI interfaces])
fi


#----------
# Cplex
#----------

AC_ARG_WITH([cplex], [AS_HELP_STRING([[--with-cplex[=dir]]],[
	Build ECLiPSe interface to ILOG CPLEX optimizer.  You can optionally
        specify a directory where to find include and lib subdirectories.])],
    [with_cplex="$withval"], [with_cplex=yes])

case $ARCH in
    i386_linux)		CPLEX_ARCH=x86_linux ;;
    x86_64_linux)	CPLEX_ARCH=x86-64_linux ;;
    sparc_sunos5)	CPLEX_ARCH=ultrasparc32 ;;
    i386_nt)		CPLEX_ARCH=x86_win32 ;;
    x86_64_nt)		CPLEX_ARCH=x64_win64 ;;
    i386_macosx)	CPLEX_ARCH=x86_darwin ;;
    x86_64_macosx)	CPLEX_ARCH=x86-64_darwin ;;
    *)			CPLEX_ARCH=unsupported ;;
esac

CPLEX_SEARCH_PATH="\
$ECLIPSETHIRDPARTY/cplex/$ARCH
/usr
/usr/local"

CPLEXDIR=
if test "$with_cplex" != "no"; then
    if test "$with_cplex" != "yes"; then
	CPLEX_SEARCH_PATH="$with_cplex"
    fi
    AC_FOREACH_UNTIL(CPLEXDIR, $CPLEX_SEARCH_PATH, [
        CPLEX_HEADER="$CPLEXDIR/include/ilcplex/cpxconst.h" &&
        test -f "$CPLEX_HEADER" -a -d "$CPLEXDIR/lib" &&
        { 
            test -d "$CPLEXDIR/lib/$CPLEX_ARCH" ||
            test -d "$CPLEXDIR/bin/$CPLEX_ARCH" ||
            { test -d "$CPLEXDIR/lib/$ARCH" && CPLEX_ARCH="$ARCH"; }
        } >/dev/null 2>&1
    ])
fi
if test -n "$CPLEXDIR"; then
    AC_EXTRACT_VERSION_INFO($CPLEX_HEADER,CPX_VERSION_VERSION,CPX_VERSION_RELEASE,CPLEX_MAJOR,CPLEX_MINOR,CPLEX_VERSION,)
    AC_MSG_NOTICE([Will build CPLEX interface for version $CPLEX_VERSION ($CPLEX_MAJOR.$CPLEX_MINOR)])
    AC_MSG_NOTICE([using CPLEXDIR=$CPLEXDIR])
    INSTALL_EPLEX=install_eplex
else
    AC_MSG_NOTICE([Not building CPLEX interface])
fi


#----------
# Xpress
#----------

AC_ARG_WITH([xpress], [AS_HELP_STRING([[--with-xpress[=dir]]],[
        Build ECLiPSe interface to Dash Xpress-MP.  You can optionally
        specify a directory where to find include and lib subdirectories.])],
    [with_xpress="$withval"], [with_xpress=yes])

XPRESS_SEARCH_PATH="\
$ECLIPSETHIRDPARTY/xosl/$GRB_ARCH
$ECLIPSETHIRDPARTY/xosl/$ARCH
/opt
C:
/Library"

XPRESSDIR=
if test "$with_xpress" != "no"; then
    if test "$with_xpress" != "yes"; then
	XPRESS_SEARCH_PATH="$with_xpress"
    fi
    AC_FOREACH_UNTIL(XPRESSDIR, $XPRESS_SEARCH_PATH, [
        XPRESS_HEADER="$XPRESSDIR/include/xprs.h"
        test -d "$XPRESSDIR/include" -a -d "$XPRESSDIR/lib"
    ])
fi
if test -n "$XPRESSDIR"; then
    AC_EXTRACT_VERSION_INFO($XPRESS_HEADER,XPVERSION,XPMINOR,XPRESS_MAJOR,XPRESS_MINOR,XPRESS_VERSION,)
    AC_MSG_NOTICE([Will build XPRESS interface for version $XPRESS_VERSION ($XPRESS_MAJOR.$XPRESS_MINOR)])
    AC_MSG_NOTICE([using XPRESSDIR=$XPRESSDIR])
    INSTALL_EPLEX=install_eplex
else
    AC_MSG_NOTICE([Not building XPRESS interface])
fi


#----------
# Gurobi
#----------

AC_ARG_WITH([gurobi], [AS_HELP_STRING([[--with-gurobi[=dir]]],[
	Build ECLiPSe interface to the Gurobi optimizer.  You can optionally
        specify a directory where to find include and lib subdirectories.])],
    [with_gurobi="$withval"], [with_gurobi=yes])

case $ARCH in
    i386_linux)		GRB_ARCH=linux32 ;;
    x86_64_linux)	GRB_ARCH=linux64 ;;
    i386_nt)		GRB_ARCH=win32 ;;
    x86_64_nt)		GRB_ARCH=win64 ;;
    i386_macosx)	GRB_ARCH=mac32 ;;
    x86_64_macosx)	GRB_ARCH=mac64 ;;
    *)			GRB_ARCH=unsupported ;;
esac

GUROBI_SEARCH_PATH="\
$ECLIPSETHIRDPARTY/gurobi/$GRB_ARCH
$ECLIPSETHIRDPARTY/gurobi/$ARCH
/opt
C:
/Library"

GUROBIDIR=
if test "$with_gurobi" != "no"; then
    if test "$with_gurobi" != "yes"; then
	GUROBI_SEARCH_PATH="$with_gurobi"
    fi
    AC_FOREACH_UNTIL(GUROBIDIR, $GUROBI_SEARCH_PATH, [
        GUROBI_HEADER="$GUROBIDIR/include/gurobi_c.h"
	test -f "$GUROBI_HEADER" -a -d "$GUROBIDIR/$DLIBDIR"
    ])
fi
if test -n "$GUROBIDIR"; then
    AC_EXTRACT_VERSION_INFO($GUROBI_HEADER,GRB_VERSION_MAJOR,GRB_VERSION_MINOR,GUROBI_MAJOR,GUROBI_MINOR,GUROBI_VERSION,)
    AC_MSG_NOTICE([Will build Gurobi interface for version $GUROBI_VERSION ($GUROBI_MAJOR.$GUROBI_MINOR)])
    AC_MSG_NOTICE([using GUROBIDIR=$GUROBIDIR])
    INSTALL_EPLEX=install_eplex
else
    AC_MSG_NOTICE([Not building Gurobi interface])
fi


#----------
# GLPK
#----------

AC_ARG_WITH([glpk], [AS_HELP_STRING([[--with-glpk[=dir]]],[
	Build ECLiPSe interface to the GLPK optimizer.  You can optionally
        specify a directory where to find include and lib subdirectories.])],
    [with_glpk="$withval"], [with_glpk=yes])

GLPK_SEARCH_PATH="\
$ECLIPSETHIRDPARTY/glpk/$ARCH"

GLPK_INCLUDE_PATH="\
/usr
/usr/local"

GLPKDIR=
if test "$with_glpk" != "no"; then
    if test "$with_glpk" != "yes"; then
	GLPK_SEARCH_PATH="$with_glpk"
    fi
    AC_FOREACH_UNTIL(GLPKDIR, $GLPK_SEARCH_PATH, [
        GLPK_HEADER="$GLPKDIR/include/glpk.h" &&
	test -f "$GLPK_HEADER" &&
	test -f "$GLPKDIR/$DLIBDIR/${LIB_PREFIX}glpk.$OBJECTS_SUFFIX"
    ])
    # if not found, look for global GLPK installation
    if test -z "$GLPKDIR"; then
        # Use AC_CHECK_LIB because lib may be under lib/<gnu-arch>/
        # on a multi-architecture machine
        AC_CHECK_LIB(glpk, glp_init_env, [
            AC_FOREACH_UNTIL(GLPKDIR, $GLPK_INCLUDE_PATH, [
                GLPK_HEADER="$GLPKDIR/include/glpk.h" &&
                test -f "$GLPK_HEADER"
            ])
        ])
        
    fi
fi
if test -n "$GLPKDIR"; then
    AC_EXTRACT_VERSION_INFO($GLPK_HEADER,GLP_MAJOR_VERSION,GLP_MINOR_VERSION,GLPK_MAJOR,GLPK_MINOR,GLPK_VERSION,0)
    AC_MSG_NOTICE([Will build GLPK interface for version $GLPK_VERSION ($GLPK_MAJOR.$GLPK_MINOR)])
    AC_MSG_NOTICE([using GLPKDIR=$GLPKDIR])
    INSTALL_EPLEX=install_eplex
else
    AC_MSG_NOTICE([Not building GLPK interface])
fi


#----------
# General Eplex build option
#----------

AC_ARG_ENABLE(eplex_own_malloc, [[  --enable-eplex-own-malloc
			Use ECLiPSe's own allocator instead of malloc in Eplex.
	]],
	[ EPLEX_FLAGS="-DUSE_OWN_MALLOC" ]
	AC_MSG_NOTICE(Eplex uses ECLiPSe's own allocator instead of malloc),
	AC_MSG_NOTICE(Eplex uses standard malloc)
)


#----------
# Gecode
#----------

AC_ARG_WITH([gfd], [AS_HELP_STRING([--with-gfd],[
	Build ECLiPSe interface to Gecode Solver.
	Default is to build the interface.])],
    [gfd_requested="$withval"], [gfd_requested=yes])

ALL_WITH_GECODE=
ALL_CROSS_WITH_GECODE=
GECODE_BASEDIR="$ECLIPSETHIRDPARTY/gecode-"
GECODE_REQUIRED="6.2"
GECODE_VERSION="6.2.0"

if test "$gfd_requested" = "yes"; then
   GECODEDIR=$(ls -d $GECODE_BASEDIR$GECODE_REQUIRED*/$ARCH  2>/dev/null)
   if test -d "$GECODEDIR"; then
      ALL_WITH_GECODE=all_with_gecode
      ALL_CROSS_WITH_GECODE=all_cross_with_gecode
      AC_MSG_NOTICE([Using Gecode in $GECODEDIR])
      AC_MSG_NOTICE([Will build Gecode Interface for Gecode version $GECODE_REQUIRED])
   else
# try a specific version of Gecode
      GECODEDIR=$(ls -d $GECODE_BASEDIR$GECODE_VERSION/$ARCH  2>/dev/null)
      if test -d "$GECODEDIR"; then
         ALL_WITH_GECODE=all_with_gecode
         ALL_CROSS_WITH_GECODE=all_cross_with_gecode
         AC_MSG_NOTICE([Using Gecode in $GECODEDIR])
         AC_MSG_NOTICE([Will build Gecode Interface for Gecode version $GECODE_REQUIRED])
     fi
   fi
fi
if test -z "$ALL_WITH_GECODE"; then
    AC_MSG_NOTICE([Not building Gecode interface])
fi

#----------
# Java JDK
#----------

JAVA_SEARCH_PATH="\
$ECLIPSETHIRDPARTY/java/$ARCH/java
/usr/java
/usr/lib/java
/usr/lib/jvm
/usr/lib/jvm/java
/usr/lib/jvm/default-java
/usr/j2se
C:/Program Files/Java/jdk*
$PROGRAMFILES/Java/jdk*
/Library/Java/Home"

AC_ARG_WITH([java], [AS_HELP_STRING([[--with-java[=dir]]],[
	Build ECLiPSe/Java interface.
	If dir is given, this is taken as the Java installation
	to use, otherwise the value of a JAVA_HOME environment
	variable is used, otherwise a number of standard
	locations is searched for a Java installation.
	Default:yes])],
    [with_java="$withval"], [with_java=yes])

if test "$with_java" = "no"; then
    unset JAVA_HOME
    unset JRE_HOME
    ALL_WITH_JAVA=
    ALL_CROSS_WITH_JAVA=
    AC_MSG_NOTICE([Not building Java interface])
else
    if test "$with_java" != "yes"; then
	JAVA_SEARCH_PATH="$with_java"
    elif test -n "$JAVA_HOME"; then
	JAVA_SEARCH_PATH="$JAVA_HOME"
    fi
    IFSorig=$IFS
    IFS=`printf '\n\t'`
    java_home_jre=
    for JAVA_HOME in $JAVA_SEARCH_PATH; do
	if test -x "$JAVA_HOME/jre/bin/java"; then
            java_home_jre="$JAVA_HOME/jre"
	elif test -x "$JAVA_HOME/bin/java"; then
            java_home_jre="$JAVA_HOME"
        fi
	if test -x "$JAVA_HOME/bin/javac" -a -d "$JAVA_HOME/include"; then
	    break
	fi
	JAVA_HOME=
    done
    IFS=$IFSorig
    if test -n "$JAVA_HOME"; then
	AC_DEFINE(HAVE_JAVA)
	ALL_WITH_JAVA=all_with_java
	ALL_CROSS_WITH_JAVA=all_cross_with_java
	AC_MSG_NOTICE([Using Java JDK in $JAVA_HOME])
    else
	unset JAVA_HOME
	ALL_WITH_JAVA=
	ALL_CROSS_WITH_JAVA=
	AC_MSG_WARN([Could not find a usable Java installation])
    fi
    if test -z "$JRE_HOME" -a -n "$java_home_jre"; then
	JRE_HOME="$java_home_jre"
    fi
    if test -n "$JRE_HOME" -a -x "$JRE_HOME/bin/java"; then
	AC_MSG_NOTICE([Using Java JRE in $JRE_HOME])
    else
	JRE_HOME=
	AC_MSG_WARN([Could not find a usable Java JRE])
    fi
fi


#----------
# FlexLM licence manager
#----------

FLEXLM_VERSION_PREF="7.2"

AC_ARG_WITH([flexlm], [AS_HELP_STRING([[--with-flexlm[=version]]],[
	Build ECLiPSe interface to FlexLM. You can optionally specify
	a version preference list.  Default is to build first available.])],
    [flexlm_versions="$withval"], [flexlm_versions=yes])

FLEXLMDIR=
if test "$flexlm_versions" != "no"; then
    if test "$flexlm_versions" = "yes"; then
    	flexlm_versions="$FLEXLM_VERSION_PREF"
    fi
    for version in $flexlm_versions; do
	FLEXLMDIR="$ECLIPSETHIRDPARTY/flexlm$version/$ARCH"
	if test -r "$FLEXLMDIR/lmclient.h"; then
	    break
	else
	    FLEXLMDIR=
	fi
    done
    if test -z "$FLEXLMDIR"; then
	CPPFLAGS_WITH_WITHOUT_FLEXLM=CPPFLAGS_WITHOUT_FLEXLM
	LIBS_WITH_WITHOUT_FLEXLM=LIBS_WITHOUT_FLEXLM
	FILES_WITH_WITHOUT_FLEXLM=FILES_WITHOUT_FLEXLM
	AC_MSG_WARN([Could not find FlexLM $flexlm_versions])
    else
	AC_DEFINE(HAVE_FLEXLM)
	CPPFLAGS_WITH_WITHOUT_FLEXLM=CPPFLAGS_WITH_FLEXLM
	LIBS_WITH_WITHOUT_FLEXLM=LIBS_WITH_FLEXLM
	FILES_WITH_WITHOUT_FLEXLM=FILES_WITH_FLEXLM
	AC_MSG_NOTICE([Using FlexLM in $FLEXLMDIR])
    fi
else
    CPPFLAGS_WITH_WITHOUT_FLEXLM=CPPFLAGS_WITHOUT_FLEXLM
    LIBS_WITH_WITHOUT_FLEXLM=LIBS_WITHOUT_FLEXLM
    FILES_WITH_WITHOUT_FLEXLM=FILES_WITHOUT_FLEXLM
    AC_MSG_NOTICE([Not building the FlexLM interface])
fi


#----------
# Grappa (prerequisite for Visualisation component)
#----------

ALL_VISUALISATION=
if test -n "$JAVA_HOME"; then

    GRAPPA_REQUIRED="1.2"
    for version in $GRAPPA_REQUIRED; do
	GRAPPA_DIR=$ECLIPSETHIRDPARTY/grappa$version
	GRAPPA_JAR=grappa`echo $version|tr . _`.jar
	if test -r $GRAPPA_DIR/$GRAPPA_JAR; then
	    break
	fi
	GRAPPA_JAR=
    done
    if test -z "$GRAPPA_JAR"; then
	AC_MSG_WARN([Could not find Grappa ($GRAPPA_REQUIRED) in $ECLIPSETHIRDPARTY])
    elif test -z $JRE_HOME; then		 
       AC_MSG_WARN([Could not find JRE for Java; Visualisation tools will not be built.])
    else
       ALL_VISUALISATION=all_visualisation
       AC_MSG_NOTICE([Using Grappa $GRAPPA_DIR/$GRAPPA_JAR])
    fi

else
    AC_MSG_WARN([Not looking for Grappa because no Java available])
fi


#----------
# Graphviz
#----------

AC_ARG_WITH([graphviz], [AS_HELP_STRING([[--with-graphviz[=versions]]],[
	Copy graphviz executables into the installation.
	You can optionally specify a version list, e.g. "1.10".
	Default is to use the first available version.])],
    [graphviz_versions_requested="$withval"], [graphviz_versions_requested=yes])

GRAPHVIZ_TARGET=
GRAPHVIZ_DIR=
if test "$graphviz_versions_requested" != "no"; then
    if test "$graphviz_versions_requested" = "yes"; then
    	graphviz_versions_requested="2.26 1.10"
    fi
    for version in $graphviz_versions_requested; do
	GRAPHVIZ_DIR="$ECLIPSETHIRDPARTY/graphviz$version/$ARCH"
	if test -d "$GRAPHVIZ_DIR"; then
	    GRAPHVIZ_TARGET=install_graphviz
	    AC_MSG_NOTICE([Using Graphviz version $version])
	    break
	else
	    AC_MSG_WARN([Could not find $GRAPHVIZ_DIR])
	fi
    done
fi
if test -z "$GRAPHVIZ_TARGET"; then
    AC_MSG_NOTICE([Not adding Graphviz executables])
fi


#----------
# MySQL
#----------

AC_ARG_WITH([mysql], [AS_HELP_STRING([[--with-mysql[=versions]]],[
	Build ECLiPSe database interface.  You can optionally
	specify a version list, e.g. "50".
	Default is to build the first available version.])],
    [mysql_versions_requested="$withval"], [mysql_versions_requested=yes])

ALL_WITH_MYSQL=
ALL_CROSS_WITH_MYSQL=
if test "$mysql_versions_requested" != "no"; then
    if test "$mysql_versions_requested" = "yes"; then
	mysql_versions_requested="55 54 53 52 51 50"
    fi
    for version in $mysql_versions_requested; do
	MYSQLDIR="$ECLIPSETHIRDPARTY/mysql$version/$ARCH"
	if test -d "$MYSQLDIR"; then
	    ALL_WITH_MYSQL=all_with_mysql
	    ALL_CROSS_WITH_MYSQL=all_cross_with_mysql
	    AC_MSG_NOTICE([Using MySQL version $version])
	    break
	else
	    AC_MSG_WARN([Could not find $MYSQLDIR])
	fi
    done
fi
if test -z "$ALL_WITH_MYSQL"; then
    AC_MSG_NOTICE([Not building MySQL interface])
fi


#----------
# CP-Viz
#----------

AC_ARG_WITH([cpviz], [AS_HELP_STRING([--without-cpviz],[
	Build ECLiPSe without the CP-Viz constraint visualiser
	(CP-Viz requires Java, Batik and Javahelp).])],
    [cpviz_requested="$withval"], [cpviz_requested=yes])


ALL_CPVIZ=
if test "$cpviz_requested" != "no"; then
    for f in "$ECLIPSETHIRDPARTY/batik-"*/lib /usr/share/java ; do
	if test -r "$f/batik-util.jar"; then BATIK_LIBDIR="$f"; break; fi
    done
    for f in "$ECLIPSETHIRDPARTY/jh"*/javahelp/lib /usr/share/java ; do
	if test -r "$f/jhall.jar"; then JHELP_LIBDIR="$f"; break; fi
    done
    if test -n "$BATIK_LIBDIR" -a -n "$JHELP_LIBDIR" -a -n "$JAVA_HOME"; then
	ALL_CPVIZ=all_cpviz
    fi
fi
if test -n "$ALL_CPVIZ"; then
    AC_MSG_NOTICE([Will build CP-Viz tool])
    AC_MSG_NOTICE([Using Batik $BATIK_LIBDIR])
    AC_MSG_NOTICE([Using Javahelp $JHELP_LIBDIR])
else
    AC_MSG_NOTICE([Not building CP-Viz tool])
fi


#----------
# Documents
#----------

AC_CHECK_PROG(PROG_LATEX, latex, yes,no)
AC_CHECK_PROG(PROG_PS2PDF,ps2pdf,yes,no)
AC_CHECK_PROG(PROG_HEVEA, hevea, yes,no)

MAKE_DOCUMENTS=
if test "$PROG_LATEX" = yes -a "$PROG_PS2PDF" = yes -a "$PROG_HEVEA" = yes; then
    MAKE_DOCUMENTS="make_documents"
    AC_MSG_NOTICE([Can build ECLiPSe documentation])
else
    AC_MSG_WARN([No tools to build ECLiPSe documentation])
fi


#----------------------------------------------------------------------

LIBS="-L. $SYSLIBS $LIBS"


#----------------------------------------------------------------------
# The files to generate
#----------------------------------------------------------------------

AC_CONFIG_FILES([
	Makefile.$ARCH:Makefile.in
	Kernel/$ARCH/Makefile:Kernel/src/Makefile.in
	Kernel/$ARCH/INST_PARAMS:Kernel/src/INST_PARAMS.in
	Kernel/$ARCH/Makefile.external:Kernel/src/Makefile.external.in
	Shm/$ARCH/Makefile:Shm/src/Makefile.in
	Eplex/Makefile.$ARCH:Eplex/Makefile.in
	icparc_solvers/Makefile.$ARCH:icparc_solvers/Makefile.in
	JavaInterface/Makefile.$ARCH:JavaInterface/Makefile.in
	Flexlm/Makefile.$ARCH:Flexlm/Makefile.in
	Visualisation/Makefile.$ARCH:Visualisation/Makefile.in
	Compiler/Makefile.$ARCH:Compiler/Makefile.in
	Contrib/Makefile.$ARCH:Contrib/Makefile.in
	Mercury/Makefile.$ARCH:Mercury/Makefile.in
	ecrc_solvers/Makefile.$ARCH:ecrc_solvers/Makefile.in
	pt_libraries/Makefile.$ARCH:pt_libraries/Makefile.in
	Oci/Makefile.$ARCH:Oci/Makefile.in
	ZincInterface/Makefile.$ARCH:ZincInterface/Makefile.in
	GecodeInterface/Makefile.$ARCH:GecodeInterface/Makefile.in
	CPViz/Makefile.$ARCH:CPViz/Makefile.in
	])
AC_CONFIG_HEADERS([
	Kernel/$ARCH/config.h:Kernel/src/config.h.in
	Shm/$ARCH/config.h:Shm/src/config.h.in],
	[echo timestamp > Kernel/$ARCH/stamp-h])


dnl Words to substitute in the Makefiles
AC_SUBST(CONFIG_SITE)
AC_SUBST(ARCH)
AC_SUBST(host)
AC_SUBST(build)
AC_SUBST(ECLIPSETHIRDPARTY)
AC_SUBST(top_srcdir)
AC_SUBST(top_builddir)
AC_SUBST(abs_top_builddir)
AC_SUBST(prefix)
AC_SUBST(CC)
AC_SUBST(CXX)
AC_SUBST(CXXSUFFIX)
AC_SUBST(LD)
AC_SUBST(AR)
AC_SUBST(RANLIB)
AC_SUBST(STRIP)
AC_SUBST(EXEEXT)
AC_SUBST(NICE)
AC_SUBST(OPT_FLAGS)
AC_SUBST(OPT_FLAGS_ARITH)
AC_SUBST(OPT_FLAGS_EMU)
AC_SUBST(OSTYPE)
AC_SUBST(OSCLASS)
AC_SUBST(FLOAT_ROUND_FLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(LOCAL_FLAGS)
AC_SUBST(OS_INCLUDES)
AC_SUBST(OPTIMIZE)
AC_SUBST(LDFLAGS)
AC_SUBST(DYNLDFLAGS)
AC_SUBST(RUNPATH_LDFLAG)
AC_SUBST(RPATH_ORIGIN)
AC_SUBST(INSTALL_NAME)
AC_SUBST(STRIPFLAGS)
AC_SUBST(S_TO_O)
AC_SUBST(C_TO_I)
AC_SUBST(LL_C_SO)
AC_SUBST(LD_C_SO)
AC_SUBST(PL_C_SO)
AC_SUBST(PD_C_SO)
AC_SUBST(PL_O_SO)
AC_SUBST(PD_O_SO)
AC_SUBST(DYLD)
AC_SUBST(DYLDCXX)
AC_SUBST(LDTCL)
AC_SUBST(TCL_DIR)
AC_SUBST(BUILD_WITH_TCL)
AC_SUBST(TCL_ARCH_DIR)
AC_SUBST(BUILD_WITH_TCL)
AC_SUBST(TCL_VERSION)
AC_SUBST(BUILDO)
AC_SUBST(GRAPHLIBS)
AC_SUBST(LIBS)
AC_SUBST(LDD)
AC_SUBST(LDDFLAG)
AC_SUBST(WANT_MPS)
AC_SUBST(OBJECT_SUFFIX)
AC_SUBST(OBJECTS_SUFFIX)
AC_SUBST(LIB_PREFIX)
AC_SUBST(DLIBDIR)
AC_SUBST(GMP_LD)
AC_SUBST(GMP_EXE_LD)
AC_SUBST(GMP_LIBDIR)
AC_SUBST(GMP_LIB)
AC_SUBST(INSTALL_GMP)
AC_SUBST(LOCK)
AC_SUBST(INSTALL_EPLEX)
AC_SUBST(EPLEX_FLAGS)
AC_SUBST(OSI_VERSIONS)
AC_SUBST(CLPCBCDIR)
AC_SUBST(SYMCLPDIR)
AC_SUBST(OSIGLPKDIR)
AC_SUBST(OSICLP_ADDLIB)
AC_SUBST(OSICLP_ADDDEF)
AC_SUBST(CPLEXDIR)
AC_SUBST(CPLEX_ARCH)
AC_SUBST(CPLEX_VERSION)
AC_SUBST(XPRESSDIR)
AC_SUBST(XPRESS_VERSION)
AC_SUBST(XPRESS_MAJOR)
AC_SUBST(GUROBIDIR)
AC_SUBST(GUROBI_VERSION)
AC_SUBST(GLPKDIR)
AC_SUBST(GLPK_VERSION)
AC_SUBST(ALL_WITH_GECODE)
AC_SUBST(ALL_CROSS_WITH_GECODE)
AC_SUBST(GECODEDIR)
AC_SUBST(GECODE_VERSION)
AC_SUBST(ALL_WITH_JAVA)
AC_SUBST(ALL_CROSS_WITH_JAVA)
AC_SUBST(JAVA_HOME)
AC_SUBST(JRE_HOME)
AC_SUBST(CPPFLAGS_WITH_WITHOUT_FLEXLM)
AC_SUBST(LIBS_WITH_WITHOUT_FLEXLM)
AC_SUBST(FILES_WITH_WITHOUT_FLEXLM)
AC_SUBST(FLEXLMDIR)
AC_SUBST(GRAPPA_DIR)
AC_SUBST(GRAPPA_JAR)
AC_SUBST(ALL_VISUALISATION)
AC_SUBST(MAKE_TARGET)
AC_SUBST(MAKE_DOCUMENTS)
AC_SUBST(ALL_WITH_MYSQL)
AC_SUBST(ALL_CROSS_WITH_MYSQL)
AC_SUBST(MYSQLDIR)
AC_SUBST(GRAPHVIZ_TARGET)
AC_SUBST(GRAPHVIZ_DIR)
AC_SUBST(STDCXXLIB)
AC_SUBST(ALL_CPVIZ)
AC_SUBST(BATIK_LIBDIR)
AC_SUBST(JHELP_LIBDIR)
AC_SUBST(JAVA_PATH_SEP)

AC_OUTPUT

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.72])

AC_INIT([Wisp R5RS Scheme], [0.2], [alasdair.mcintyre@gmail.com])

# where to store auxiliary build tools
AC_CONFIG_AUX_DIR([build-aux])

# resp: turn on all automake warnings, treat as errors, don't expect files required for a GNU project.
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# searches for c compiler and set CC
AC_PROG_CC
# ditto for install
AC_PROG_INSTALL

# causes configure to create a config.h file gathering ‘#define’s defined by other macros in configure.ac.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])

ACX_WITH_READLINE
AX_LIB_MYSQL([3.0])

# declare list of files configure should create from *.in templates.
# automake also scans this list to find the Makefile.am files it must process. 
AC_CONFIG_FILES([
    Makefile
    src/Makefile
    doc/Makefile
    m4/Makefile
])

# produce part of script for creating files registered with AC_CONFIG_HEADERS and AC_CONFIG_FILES.
AC_OUTPUT

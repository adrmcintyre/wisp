AC_INIT([Wisp R5RS Scheme], [0.1])
# where to store the auxiliary build tools
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC
AC_PROG_INSTALL
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])
ACX_WITH_READLINE
AC_CONFIG_FILES([
    Makefile
    src/Makefile
    doc/Makefile
    m4/Makefile
])
AC_OUTPUT

### m4/readline.m4 --- autoconf check for readline    -*- autoconf -*-
## Copyright (C) ????  Ville Laurikari <vl@iki.fi>
## Copyright (C) 2004  Daniel Brockman <daniel@brockman.se>

## This file is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 2 of the License,
## or (at your option) any later version.

## This file is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with Denide (see the file named `COPYING').  If not, write to

##    The Free Software Foundation
##    59 Temple Place - Suite 330
##    Boston MA 02111-1307 USA

### Helper functions:

AC_DEFUN([ACX_CHECK_ADD_HISTORY],
  [AC_CACHE_CHECK([whether readline supports history],
                  [acx_cv_add_history],
     [acx_cv_add_history=no
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[extern void add_history ();]],
           [[add_history ("foo");]])],
        [acx_cv_add_history=yes])])

   if test "x$acx_cv_add_history" = xyes; then
     AC_DEFINE([HAVE_ADD_HISTORY], [1],
               [Define to 1 if your readline library has the
                `add_history' function.])
     AC_CHECK_HEADERS([readline/history.h history.h] dnl
                      [editline/history.h])
   fi])

AC_DEFUN([ACX_CHECK_RL_COMPLETION_ENTRY_FUNCTION],
  [AC_CACHE_CHECK([whether readline supports custom completion],
                  [acx_cv_rl_completion_entry_function],
     [acx_cv_rl_completion_entry_function=no
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[extern char *(*rl_completion_entry_function) ();]],
           [[rl_completion_entry_function = 0;]])],
        [acx_cv_rl_completion_entry_function=yes])])

   if test "x$acx_cv_rl_completion_entry_function" = xyes; then
     AC_DEFINE([HAVE_RL_COMPLETION_ENTRY_FUNCTION], [1],
               [Define to 1 if your readline library has the
                `rl_completion_entry_function' variable.])
   fi])

AC_DEFUN([ACX_CHECK_RL_COMPLETION_MATCHES],
  [AC_CACHE_CHECK([how readline names the completion_matches function],
                  [acx_cv_rl_completion_matches],
     [acx_cv_rl_completion_matches=no
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[extern void rl_completion_matches ();]],
           [[rl_completion_matches(0,0);]])],
        [acx_cv_rl_completion_matches=yes])])

   if test "x$acx_cv_rl_completion_matches" = xyes; then
     AC_DEFINE([HAVE_RL_COMPLETION_MATCHES], [1],
               [Define to 1 if your readline library has the
                `rl_completion_matches' function.])
   fi])

##--------------------------------------------------------------------
## ACX_WITH_READLINE([ACTION-IF-FOUND, [ACTION-IF-NOT-FOUND]])
##--------------------------------------------------------------------
## Detect readline-compatible line editing library,
## such as GNU Readline.

## If a readline library is found,
##   * defines to 1 `HAVE_READLINE' and zero or more of
##       - `HAVE_READLINE_READLINE_H',
##       - `HAVE_READLINE_H',
##       - `HAVE_EDITLINE_READLINE_H',
##       - `HAVE_EDITLINE_H';
##   * defines `READLINE_LIBS' to the flags required to link with the
##     readline library;
##   * if the library has the `add_history' function, defines to 1
##     `HAVE_ADD_HISTORY' and zero or more of
##       - `HAVE_READLINE_HISTORY_H',
##       - 'HAVE_HISTORY_H',
##       - 'HAVE_EDITLINE_HISTORY_H';
##   * if the library has the `rl_completion_entry_function' variable,
##     defines to 1 `HAVE_RL_COMPLETION_ENTRY_FUNCTION'; and finally,
##   * executes `ACTION-IF-FOUND'.

## Otherwise, executes `ACTION-IF-NOT-FOUND'.

## The libraries that may be readline-compatible are `libedit',
## `libeditline' and `libreadline'.  Sometimes we need to link a
## termcap library for readline to work, this macro tests these cases
## too by trying to link with `libtermcap', `libcurses' or
## `libncurses' before giving up.

## This macro adds the configure option `--with-readline', which lets
## the user select whether they want to build with readline support,
## and, if so, what specific readline-compatible library to use.

## Here is an example of how to use the information provided by this
## macro to perform the necessary includes and/or declarations in a C
## file:

##     #ifdef HAVE_READLINE
##     #  if defined(HAVE_READLINE_READLINE_H)
##     #    include <readline/readline.h>
##     #  elif defined(HAVE_READLINE_H)
##     #    include <readline.h>
##     #  elif defined(HAVE_EDITLINE_READLINE_H)
##     #    include <editline/readline.h>
##     #  elif defined(HAVE_EDITLINE_H)
##     #    include <editline.h>
##     #  else
##     extern char *readline ();
##     #  endif
##     #  ifdef HAVE_ADD_HISTORY
##     #    if defined(HAVE_READLINE_HISTORY_H)
##     #      include <readline/history.h>
##     #    elif defined(HAVE_HISTORY_H)
##     #      include <history.h>
##     #    elif defined(HAVE_EDITLINE_HISTORY_H)
##     #      include <editline/history.h>
##     #    else
##     extern void add_history ();
##     #    endif
##     #  endif /* HAVE_ADD_HISTORY */
##     #else
##     #  error "Cannot build without readline support."
##     #endif /* HAVE_READLINE */

AC_DEFUN([ACX_WITH_READLINE],
  [AC_ARG_WITH([readline],
     [[  --with-readline[=no/readline/edit/editline/auto]
                          build with readline support [default=auto]]])
   if test -z "$with_readline" ; then
     with_readline=auto
   fi
   READLINE_LIBS=
   AC_CACHE_CHECK([for a readline-compatible line editing library],
                  [acx_cv_readline_libs],
     [acx_orig_libs="$LIBS"
      for acx_readline_lib in readline edit editline ; do
        if test "x$with_readline" = xauto \
                -o "x$with_readline" = "x$acx_readline_lib" ; then
          for acx_termcap_lib in "" termcap curses ncurses ; do
            if test -z "$acx_termcap_lib"; then
              acx_try_libs="-l$acx_readline_lib"
            else
              acx_try_libs="-l$acx_readline_lib -l$acx_termcap_lib"
            fi
            LIBS="$acx_orig_libs $acx_try_libs"
            AC_LINK_IFELSE(
              [AC_LANG_PROGRAM(
                 [[extern char *readline ();]],
                 [[readline ("prompt");]])],
              [acx_cv_readline_libs="$acx_try_libs"])
            if test -n "$acx_cv_readline_libs" ; then
              break
            fi
          done
          if test -n "$acx_cv_readline_libs" ; then
            break
          fi
        fi
      done
      if test -z "$acx_cv_readline_libs" ; then
        acx_cv_readline_libs=no
      fi
      LIBS="$acx_orig_libs"])

   if test "x$acx_cv_readline_libs" != xno ; then
     AC_DEFINE([HAVE_READLINE], [1],
               [Define to 1 if you have a readline-compatible
                editing library.])
     AC_SUBST([READLINE_LIBS], ["$acx_cv_readline_libs"])

     AC_CHECK_HEADERS([readline/readline.h readline.h] dnl
                      [editline/readline.h editline.h])
    
     acx_orig_libs="$LIBS"
     LIBS="$acx_orig_libs $READLINE_LIBS"

     ACX_CHECK_ADD_HISTORY
     ACX_CHECK_RL_COMPLETION_ENTRY_FUNCTION
     ACX_CHECK_RL_COMPLETION_MATCHES

     LIBS="$acx_orig_libs"
   fi])

### m4/readline.m4 ends here.

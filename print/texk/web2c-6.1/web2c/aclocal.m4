dnl Find additional X libraries, magic flags, etc.
define(AC_FIND_XTRA, [
if test -n "$x_includes"; then
  x_include_flags=-I$x_includes
elif test -n "$no_x"; then 
  # Not all programs may use this symbol, but it won't hurt to define it.
  x_include_flags=-DX_DISPLAY_MISSING
fi
#
# It would be nice to have a more robust check for the -R ld option then
# just checking for Solaris.
# 
# It would also be nice to do this for all -L options, not just this one.
if test -n "$x_libraries"; then
  x_lib_flags=-L$x_libraries
  if test "`uname 2>/dev/null`" = SunOS \
     && uname -r | grep '^5' >/dev/null; then
    x_lib_flags="$x_lib_flags -R$x_libraries"
  fi
fi
#
# Check for additional X libraries.
# 
# Since we already have an explicit check for POSIXified ISC, assume
# it's been used.
if test -n "$ISC"; then
  x_extra_libs="$x_extra_libs -lnsl_s -linet"
  test -n "$verbose" && echo "	adding -lnsl_s -linet to x_extra_libs (for ISC)"
else
  # Martyn.Johnson@cl.cam.ac.uk says this is needed for Ultrix, if the X
  # libraries were built with DECnet support.  And karl@cs.umb.edu's Alpha
  # needs dnet_stub.
  AC_HAVE_LIBRARY(dnet,
    [x_extra_libs="$x_extra_libs -ldnet"
     have_dnet=t
     test -n "$verbose" && echo "	adding -ldnet to x_extra_libs"])
  if test -z "$have_dnet"; then
    AC_HAVE_LIBRARY(dnet_stub,
      [x_extra_libs="$x_extra_libs -ldnet_stub"
       test -n "$verbose" && echo "	adding -ldnet_stub to x_extra_libs"])
  fi
  # lieder@skyler.mavd.honeywell.com says without -lsocket,
  # socket/setsockopt and other routines are undefined under SCO ODT 2.0.
  # But -lsocket is broken on IRIX.
  if test "`uname 2>/dev/null`" != IRIX; then
    AC_HAVE_LIBRARY(socket,
      [x_extra_libs="$x_extra_libs -lsocket"
       test -n "$verbose" && echo "	adding -lsocket to x_extra_libs"])
  fi
fi
#
AC_SUBST(x_include_flags)dnl
AC_SUBST(x_lib_flags)dnl
AC_SUBST(x_extra_libs)dnl
])dnl

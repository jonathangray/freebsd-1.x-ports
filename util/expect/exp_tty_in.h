/* exp_tty_in.h - internal tty support definitions */

/* Definitions for handling termio inclusion are localized here */
/* This file should be included only if direct access to tty structures are */
/* required.  This file is necessary to avoid  mismatch between gcc's and */
/* vendor's include files */

/* Written by Rob Savoye <rob@cygnus.com>. Mon Feb 22 11:16:53 RMT 1993 */

#ifndef __EXP_TTY_IN_H__
#define __EXP_TTY_IN_H__

#include "expect_cf.h"

/*
 * Set up some macros to isolate tty differences
 */

#if defined(HAVE_TERMIO) && !defined(HAVE_TERMIOS)
#  include <termio.h>
#  undef POSIX
#  define TERMINAL termio
#  ifndef TCGETS
#    define TCGETS	TCGETA
#    define TCSETS	TCSETA
#    define TCSETSW	TCSETAW
#    define TCSETSF	TCSETAF
#  endif
#endif

#if defined(HAVE_SGTTYB) && !defined(HAVE_TERMIOS)
#  undef HAVE_TERMIO
#  undef POSIX
#ifndef TCGETS
#  define TCGETS	TIOCGETP
#  define TCSETS	TIOCSETP
#endif
#ifndef TCSETSW
#  define TCSETSW	TIOCSETN
#endif
#  define TERMINAL sgttyb
#  ifdef HAVE_SYS_FCNTL_H
#    include <sys/fcntl.h>
#  else
#    include <fcntl.h>
#  endif
#  include <sgtty.h>
#  include <sys/ioctl.h>
#endif

#if defined(HAVE_TERMIOS)
#  undef HAVE_TERMIO
#  undef HAVE_SGTTYB
#  include <termios.h>
#  define TERMINAL termios
#  if !defined(TCGETS) || !defined(TCSETS)
#    define TCGETS	TCGETA
#    define TCSETS	TCSETA
#    define TCSETSW	TCSETAW
#    define TCSETSF	TCSETAF
#  endif
#endif

/* This section was written by: Don Libes, NIST, 2/6/90 */

typedef struct TERMINAL exp_tty;
extern exp_tty exp_tty_original;

#include "exp_tty.h"

#endif	/* __EXP_TTY_IN_H__ */

/* exp_console.c - grab console.  This stuff is in a separate file to
avoid unpleasantness of AIX (3.2.4) .h files which provide no way to
reference TIOCCONS and include both sys/ioctl.h and sys/sys/stropts.h
without getting some sort of warning from the compiler.  The problem
is that both define _IO but only ioctl.h checks to see if it is
defined first.  This would suggest that it is sufficient to include
ioctl.h after stropts.h.  Unfortunately, ioctl.h, having seen that _IO
is defined, then fails to define other important things (like _IOW).

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include "expect_cf.h"
#include <stdio.h>
#include <sys/ioctl.h>
#include "tcl.h"
#include "exp_rename.h"
#include "exp_prog.h"
#include "exp_log.h"

void
exp_console_set()
{
#ifdef TIOCCONS
	int on = 1;

	if (ioctl(0,TIOCCONS,(char *)&on) == -1) {
		errorlog(stderr, "expect: spawn: cannot open console, check permissions of /dev/console\n");
		exit(-1);
	}
#endif /*TIOCCONS*/
}

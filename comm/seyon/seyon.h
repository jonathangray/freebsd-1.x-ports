
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
*/

#include "config.h"

/* globals */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#ifndef NOSTDHDRS
#include <stdlib.h>
#endif
#ifndef SUNOS_3
#include <setjmp.h>
#endif
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>

struct kw {			/* Used by command parsing routines */
  char           *keyword;
  void            (*rtn) ();
};

struct command {
  char           *name;
  void            (*call_back) ();
};

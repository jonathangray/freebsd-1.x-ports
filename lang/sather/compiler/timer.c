/*  -*- Mode: C;  -*-
 * File: timer.c
 * Author: Chu-Cheow Lim
 * Copyright (C) International Computer Science Institute, 1990, 1991, 1992, 1993 
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.  
 * 
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION: Provides support for keeping track of both user and system time.
 **
 ** RCS: $Id: timer.c,v 1.1 1994/02/12 03:21:45 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 18:57 1993 (hws)
 **  Oct 24 18:57 1993 (hws): reintroduce hpux7 restriction
 **  Sep  5 02:49 1993 (hws): avoid timers name conflict in OSF/1
 **  Apr 17 06:30 1993 (hws): adapt to RS6000
 ** Created: Mon Dec 10 14:25:23 1990
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include "all_.h"

#ifdef SCO
#include <sys/select.h>
#endif

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

#if ( defined(hpux) && defined(hpux7) ) || defined(__svr4__)

/* From Jean-Jacques Moreau's msg of 13 Aug 91.
 *  I have ported Sather to an HP9000s300 running HP-UX 7.05. To the best of
 *  my understanding, the port should also work on s800 and on machines running
 *  the new HP-UX 8.0.
 * 
 *  Note that the compiler includes bin.$ARCH/time_.o
 *  which has the work arounds.
 * 
 */

#include <unistd.h>

#if ( defined(__STDC__) || defined(__GNUC__) )

extern int getrusage (int who, struct rusage *rusage);
extern pid_t wait4 (pid_t pid, int *status, int options, struct rusage *rusage);

#  else

extern int getrusage (who, rusage)
     int who;
     struct rusage *rusage;

extern pid_t wait4 (pid, status, options, rusage)
     pid_t pid;
     int *status;
     int options;
     struct rusage *rusage;
#  endif
#endif

#if defined(__svr4__)
#  include <sys/rusage.h>
#endif

ptr timer_c_create();

ptr create_timer()
{
  struct rusage ru;
  double utime, stime;

  getrusage(RUSAGE_SELF, &ru);
#if defined(__svr4__)
  utime = ru.ru_utime.tv_sec + 0.000001 * ru.ru_utime.tv_nsec;
  stime = ru.ru_stime.tv_sec + 0.000001 * ru.ru_stime.tv_nsec;
#else
  utime = ru.ru_utime.tv_sec + 0.000000001 * ru.ru_utime.tv_usec;
  stime = ru.ru_stime.tv_sec + 0.000000001 * ru.ru_stime.tv_usec;
#endif
  return (timer_c_create(0, utime, stime));
}


ptr report_timer(cid)
int cid;
{
  struct rusage ru;
  int statusp;
  double utime, stime;
  int pid;
#if !defined(sgi) && ( defined(mips) || defined(hpux) || defined(sequent) || defined(rs6000) )
  pid = wait3( &statusp, 0, &ru);
#else
  pid = wait4(cid, &statusp, 0, &ru);
#endif
  if (cid == pid) {
#if defined(__svr4__)
    utime = ru.ru_utime.tv_sec + 0.000000001 * ru.ru_utime.tv_nsec;
    stime = ru.ru_stime.tv_sec + 0.000000001 * ru.ru_stime.tv_nsec;
#else
    utime = ru.ru_utime.tv_sec + 0.000001 * ru.ru_utime.tv_usec;
    stime = ru.ru_stime.tv_sec + 0.000001 * ru.ru_stime.tv_usec;
#endif
    return (timer_c_create(0, utime, stime));
  }
  return 0;
}

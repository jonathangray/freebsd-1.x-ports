/*  -*- Mode: C;  -*-
 * File: time_.c
 * Author: Stephen M. Omohundro
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
 ** FUNCTION: provider timer functions for benchmarking and time for
 **           random number generation.
 **
 ** RCS: $Id: time_.c,v 1.1 1994/02/12 03:23:17 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 23 16:13 1993 (hws)
 **  Oct  8 23:17 1993 (hws): merge SYSV fixes from alpha port
 **  Oct  5 13:07 1993 (hws): add linux
 **  Sep  5 02:52 1993 (hws): avoid ftime on alpha
 **  Apr  1 09:38 1993 (hws): add crude time_ for sgi port
 ** Created: Tue Sep 25 17:14:23 1990 (om)
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

#include "all_.h"
#ifdef sequent
#include <sys/types.h>
#endif
#ifdef mips
#include <sys/types.h>
#endif

#ifdef SCO
#include <sys/types.h>
#include <sys/select.h>
#endif

#if defined(SCO) || defined(hpux8) || defined(sgi) ||\
    defined(linux) || defined(__svr4__)
#include <sys/times.h>
#endif

#include <sys/time.h>
#include <sys/resource.h>

#if defined(sgi)
#  include <time.h>
#else 
#  include <sys/timeb.h>
#endif 


#if defined(__svr4__)
#  include <sys/rusage.h>
#endif

#if defined(hpux) || defined(SCO) || defined(sgi) || defined(__svr4__)
/* From Jean-Jacques Moreau's msg of 13 Aug 91.
 *  I have ported Sather to an HP9000s300 running HP-UX 7.05. To the best of
 *  my understanding, the port should also work on s800 and on machines running
 *  the new HP-UX 8.0.
 */

#include <unistd.h>

/*
 * Partial implementation of getrusage(2).
 */

int getrusage (who, rusage)
     int who;
     struct rusage *rusage;
{
  struct tms tusage;
  long hz;
  clock_t res;

  res = times (&tusage);
  if (res == -1) 
    return -1;

  hz = sysconf (_SC_CLK_TCK);
  if (hz == -1)
    return -1;

  switch (who) {
  case RUSAGE_SELF:
    rusage->ru_utime.tv_sec = tusage.tms_utime / hz;
#if defined(__svr4__)
    rusage->ru_utime.tv_nsec = (tusage.tms_utime % hz) * 1000;
#else
    rusage->ru_utime.tv_usec = (tusage.tms_utime % hz) * 1000000;
#endif
    rusage->ru_stime.tv_sec = tusage.tms_stime / hz;
#if defined(__svr4__)
    rusage->ru_stime.tv_nsec = (tusage.tms_stime % hz) * 1000;
#else
    rusage->ru_stime.tv_usec = (tusage.tms_stime % hz) * 1000000;
#endif
  case RUSAGE_CHILDREN:
    rusage->ru_utime.tv_sec = tusage.tms_cutime / hz;
#if defined(__svr4__)
    rusage->ru_utime.tv_nsec = (tusage.tms_cutime % hz) * 1000;
#else
    rusage->ru_utime.tv_usec = (tusage.tms_cutime % hz) * 1000000;
#endif
    rusage->ru_stime.tv_sec = tusage.tms_cstime / hz;
#if defined(__svr4__)
    rusage->ru_stime.tv_nsec = (tusage.tms_cstime % hz) * 1000;
#else
    rusage->ru_stime.tv_usec = (tusage.tms_cstime % hz) * 1000000;
#endif
  }

  return 0;
}


/* 
 * Partial implementation of wait4().
 */

pid_t wait4 (pid, status, options, rusage)
     pid_t pid;
     int *status;
     int options;
     struct rusage *rusage;
{
  pid_t res;

  res = waitpid (pid, status, options);

  if (res == -1)
    return -1;

  /*
   * This doesn't really emulate the original function, but it will do
   * for now.
   */
  if ((pid == -1) || (pid == 0) || (pid < 0) || (pid > 0)) {
    if (getrusage (RUSAGE_CHILDREN, rusage) == -1)
      return -1;
  }

  return res;
}

#endif

static struct rusage ru;
static float st;		/* holds start time */

/* Initializes st. */
extern void start_clock_()
{
  getrusage(RUSAGE_SELF,&ru);
#if defined(__svr4__) 
  st=ru.ru_utime.tv_sec+.000000001*ru.ru_utime.tv_nsec;
#else
  st=ru.ru_utime.tv_sec+.000001*ru.ru_utime.tv_usec;
#endif
}

/* Returns time since start_clock. */
extern float get_clock_()
{
  getrusage(RUSAGE_SELF,&ru);
#if defined(__svr4__)
  return(ru.ru_utime.tv_sec+.000000001*ru.ru_utime.tv_nsec-st);
#else
  return(ru.ru_utime.tv_sec+.000001*ru.ru_utime.tv_usec-st);
#endif
}


/* Time since 00:00:00 GMT, Jan. 1, 1970 in seconds. */
#if defined(sgi) || defined(alpha) || defined(__svr4__) || defined(__FreeBSD__)

double time_() 
{
  static time_t t, zerot;
  static struct tm zerotime = { 0, 0, 0, 0, 0, /* sec, min, hour, day, month */
				70,	      /* 1970 */
                                0, 0,	      /* weekday, yearday */
				-1 };	      /* daylight savings unknown */
  static struct timezone tz = { 0, 0 };       /* minutes west Greenwich, dst correction */
  static struct timeval microtime;
  time(&t);
  zerot = mktime(&zerotime);
  gettimeofday(&microtime, &tz);
  return(difftime(t,zerot)+microtime.tv_usec/1000000.);
}
#else
double time_() 
{
  static struct timeb t;
  ftime(&t);
  return(t.time+(t.millitm/1000.));
}
#endif


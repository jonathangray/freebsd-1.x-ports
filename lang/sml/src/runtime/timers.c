/* timers.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#include "ml_os.h"

#if defined (V9) || defined(HPUX)
#include <sys/times.h>
#else
#ifdef SOLARIS
#include <sys/time.h>
#include <sys/times.h>
#include <unistd.h>
#else
#ifndef THINK_C
#include <sys/time.h>
#else
#include <time.h>
#include "MacOS.dep.h"
#endif
#ifdef AUX
#include <sys/times.h>
#else
#ifndef THINK_C
#include <sys/resource.h>
#endif
#endif
#endif
#endif

#include "ml_types.h"

#ifdef THINK_C
/* 28Jan93   Kjeld & Soren */

#define TMacbaseyr      1904L
#define TUNIXbaseyr     1970L

/* number of leap days between the two years -- Mac base was a leap year! */

#define TLpD    ((TUNIXbaseyr-TMacbaseyr-1)/4) + 1

/* TimeBaseDif is the number of seconds between Mac and UNIX time (GMT) */

#define TimeBaseDif     ((((TUNIXbaseyr-TMacbaseyr)*365)+TLpD)*24*60*60)


void gettimeofday ( struct timeval *t, int huh )
{
        unsigned long now, ticks;
        
        GetDateTime(&now);
        ticks = clock();
        
        t->tv_sec = now - TimeBaseDif;
        
    t->tv_usec = ((ticks % CLOCKS_PER_SEC) * 1000000) / CLOCKS_PER_SEC;
}
#endif

/* The global clocks (as ML ints) */
ML_val_t	t_sec, t_usec,	/* total usr time */
		s_sec, s_usec,	/* total sys time */
		g_sec, g_usec;	/* garbage collection usr time */

static struct timeval gt0,	/* the startime of the most recent garbage collection */
		garbagetime;	/* the cumulative garbage collection time */

/* get_cpu_time:
 * Get the user and/or system cpu times in a system independent way.
 */
static void get_cpu_time (user_t, sys_t)
    struct timeval  *user_t, *sys_t;
{
#if defined(V9) || defined(HPUX) || defined(AUX)
    struct tms	    ts;
    times (&ts);
    if (user_t) {
	user_t->tv_usec	= ((ts.tms_utime % 60) * 1000000) / 60;
	user_t->tv_sec	= (ts.tms_utime / 60);
    }
    if (sys_t) {
	sys_t->tv_usec	= ((ts.tms_stime % 60) * 1000000) / 60;
	sys_t->tv_sec	= (ts.tms_stime / 60);
    }
#else
#ifdef SOLARIS
    struct tms	    ts;
    long tick;
    times (&ts);
    tick = sysconf(_SC_CLK_TCK);
    if (user_t) {
	user_t->tv_usec	= ((ts.tms_utime % tick) * 1000000) / tick;
	user_t->tv_sec	= (ts.tms_utime / tick);
    }
    if (sys_t) {
	sys_t->tv_usec	= ((ts.tms_stime % tick) * 1000000) / tick;
	sys_t->tv_sec	= (ts.tms_stime / tick);
    }
#else
#ifdef THINK_C
    int	ticks;
    if (user_t) {
    	ticks = (int)clock();
    	user_t->tv_sec  = ticks / CLOCKS_PER_SEC;
    	user_t->tv_usec = ((ticks % CLOCKS_PER_SEC) * 1000000) / CLOCKS_PER_SEC;
    }
    if (sys_t)  {
	sys_t->tv_usec	= 0;
	sys_t->tv_sec	= 0;
    }
#else
    struct rusage   r;
    getrusage (RUSAGE_SELF, &r);
    if (user_t) { *user_t = r.ru_utime; }
    if (sys_t) { *sys_t = r.ru_stime; }
#endif
#endif
#endif
} /* end of get_cpu_time. */


/* start_gc_timer:
 * Start timing a garbage collection.
 */
void start_gc_timer ()
{
    get_cpu_time (&gt0, 0);
}

/* check_gc_timer:
 * Return the time (in ms.) spent since the start of the most recent
 * garbage collection.
 */
int check_gc_timer ()
{
    struct timeval t1;
    int		sec, usec;

    get_cpu_time (&t1, 0);
    sec = t1.tv_sec - gt0.tv_sec;
    usec = t1.tv_usec - gt0.tv_usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    return (usec/1000 + sec*1000);
}

/* stop_gc_timer:
 * Stop the garbage collection timer and update the cumulative garbage collection
 * time.
 */
void stop_gc_timer ()
{
    int		sec, usec;
    struct timeval t1;

    get_cpu_time (&t1, 0);
    sec = (t1.tv_sec - gt0.tv_sec) + garbagetime.tv_sec;
    usec = (t1.tv_usec - gt0.tv_usec) + garbagetime.tv_usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
	sec++; usec -= 1000000;
    }
    garbagetime.tv_sec = sec;
    garbagetime.tv_usec = usec;
}

/* resettimers:
 * Clear the timers.
 */
resettimers()
{
    garbagetime.tv_sec = 0;
    garbagetime.tv_usec = 0;
}

/* timer:
 * Set the ML version of the global clock.
 */
timer()
{
    struct timeval	t, s;

    get_cpu_time (&t, &s);
    t_usec	= INT_CtoML(t.tv_usec);
    t_sec	= INT_CtoML(t.tv_sec);
    s_usec	= INT_CtoML(s.tv_usec);
    s_sec	= INT_CtoML(s.tv_sec);
    g_usec	= INT_CtoML(garbagetime.tv_usec);
    g_sec	= INT_CtoML(garbagetime.tv_sec);
}

/* $Id: timer.c,v 1.1 1994/02/23 14:40:08 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "types.h"
#include <stdio.h>
#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#include <socket.h>
#include <time.h>
#include "gettimeofday.h"
#else
#include <unistd.h>
#endif
#ifndef VMS
#include <sys/time.h>
#endif
#include <signal.h>
#include <stdlib.h>

#include "error.h"

#ifdef _SONYNEWS_SOURCE
/*
 * Sony NEWS doesn't have the sigset family.
 */
typedef unsigned int    sigset_t;
#define sigemptyset(set)        (*(set) = 0)
#define sigfillset(set)         (*(set) = ~(sigset_t)0, 0)
#define sigaddset(set,signo)    (*(set) |= sigmask(signo), 0)
#define sigdelset(set,signo)    (*(set) &= ~sigmask(signo), 0)
#define sigismember(set,signo)  ((*(set) & sigmask(signo)) != 0)

#define SIG_BLOCK       1
#define SIG_UNBLOCK     2
#define SIG_SETMASK     3

static int __sigtemp;           /* For use with sigprocmask */

#define sigprocmask(how,set,oset) \
	((__sigtemp = (((how) == SIG_BLOCK) ? \
	sigblock(0) | *(set) : (((how) == SIG_UNBLOCK) ? \
	sigblock(0) & ~(*(set)) : ((how) == SIG_SETMASK ? \
	*(set) : sigblock(0))))), ((oset) ? \
	(*(oset) = sigsetmask(__sigtemp)) : sigsetmask(__sigtemp)), 0)

/*
 * Sony NEWS doesn't have sigaction(), using sigvec() instead.
 */
#define sigaction sigvec
#define sa_handler sv_handler
#define sa_mask sv_mask
#define sa_flags sv_flags
#endif


extern int  framesPerSecond;


#ifdef VMS
#define BUSYLOOP
#else
#undef BUSYLOOP
#endif
#ifdef BUSYLOOP
void Loop_delay(void)
{
    static long		adj_sec = 0;
    static long		last_msec = 0;
    long		msec;
    struct timeval	tval;
    struct timezone	tzone;

    if (adj_sec == 0) {
	if (gettimeofday(&tval, &tzone) != 0)
	    return;

	adj_sec = tval.tv_sec;
	last_msec = (tval.tv_sec - adj_sec)*1000 + (tval.tv_usec)/1000;
	return;
    }

    do {
	if (gettimeofday(&tval, &tzone) != 0)
	    return;

	msec = (tval.tv_sec - adj_sec)*1000 + (tval.tv_usec)/1000;

    } while (msec < last_msec + 1000/framesPerSecond);

    last_msec = msec;
}

void block_timer(void)
{	/* dummy */
}

void allow_timer(void)
{	/* dummy */
}
#else

static volatile long   timer_count,	/* SIGALRMs that have occurred */
                       timers_used;	/* SIGALRMs that have been used */

/*
 * Catch the SIGALRM signal.
 * If any system calls other than pause(2) would get interrupted because
 * of the SIGALRM signal occuring, than that would be an undesirable
 * side effect or bug.  Be aware of the possibility.
 * E.g., select(2) will be interrupted.
 * If this happens to be a problem, two functions are provided to
 * temporarily block and unblock the timer interrupt.
 * Note that this is still experimental.  It is unclear to me if setitimer(2)
 * is accurate enough.  If not than this whole idea may be thrown away or
 * we have to come up with something better.  How about using gettimeofday(2)
 * to measure if any timer-interrupts are missed and adjusting `timer_count'
 * accordingly?
 */
static void catch_timer(int signum)
{
    timer_count++;
}

/*
 * Block or unblock a single signal.
 */
static void sig_ok(int signum, int flag)
{
    sigset_t    sigset;
#ifdef _SONYNEWS_SOURCE
    /* Dummy variable */
    sigset_t	osigset;
    int		flag_block;
#endif

    sigemptyset(&sigset);
    sigaddset(&sigset, signum);
#ifdef _SONYNEWS_SOURCE
    flag_block = (flag) ? SIG_UNBLOCK : SIG_BLOCK;
    if (sigprocmask(flag_block, &sigset, &osigset) == -1) {
#else
    if (sigprocmask((flag) ? SIG_UNBLOCK : SIG_BLOCK, &sigset, NULL) == -1) {
#endif
	error("sigprocmask(%d,%d)", signum, flag);
	exit(1);
    }
}

/*
 * Prevent the real-time timer from interrupting system calls.
 * Globally accessible.
 */
void block_timer(void)
{
    sig_ok(SIGALRM, 0);
}

/*
 * Unblock the real-time timer.
 * Globally accessible.
 */
void allow_timer(void)
{
    sig_ok(SIGALRM, 1);
}

/*
 * Setup the handling of the SIGALRM signal
 * and setup the real-time interval timer.
 */
static void setup_timer(void)
{
    struct itimerval itv;
    struct sigaction act;

    /*
     * Prevent SIGALRMs from disturbing the initialisation.
     */
    block_timer();

    /*
     * Install a signal handler for the alarm signal.
     */
    act.sa_handler = catch_timer;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaddset(&act.sa_mask, SIGALRM);
    if (sigaction(SIGALRM, &act, (struct sigaction *)NULL) == -1) {
	error("sigaction SIGALRM");
	exit(1);
    }

    /*
     * Install a real-time timer with a frequency of `framesPerSecond'.
     */
    if (framesPerSecond <= 0) {
	error("illegal framesPerSecond value %d", framesPerSecond);
	exit(1);
    }
    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 1000000 / framesPerSecond;
    itv.it_value = itv.it_interval;
    if (setitimer(ITIMER_REAL, &itv, NULL) == -1) {
	error("setitimer");
	exit(1);
    }

    /*
     * Prevent possible underflow later on.
     */
    timer_count = timers_used = framesPerSecond;

    /*
     * Allow the real-time timer to generate SIGALRM signals.
     */
    allow_timer();
}

/*
 * Let the caller be run in a frequency of `framesPerSecond'.
 */
void Loop_delay(void)
{
    static int  prevFramesPerSecond = -2;

    if (prevFramesPerSecond != framesPerSecond) {
	setup_timer();
	prevFramesPerSecond = framesPerSecond;
    }
    while (timers_used == timer_count) {
	/*
	 * Wait for a SIGALRM to occur.
	 */
	pause();
    }
    if (timers_used >= timer_count) {
	error("bug SIGALRM %ld >= %ld\n", timers_used, timer_count);
	timers_used = timer_count = framesPerSecond;
    }
    else if (++timers_used + 2 < timer_count) {
	/*
	 * The host, network or X servers can't keep up with our rate.
	 * Adjust, but allow an overload if it is of short duration.
	 */
	timers_used = timer_count - 2;
    }
}
#endif

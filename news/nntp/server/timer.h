/* @(#) $Header: /a/cvs/386BSD/ports/news/nntp/server/timer.h,v 1.1 1993/07/19 20:04:33 nate Exp $ */

struct timer {
	void (*subr)();		/* routine to invoke at timeout */
	int resetoninput;	/* if true, reset timer on input */
	long seconds;		/* seconds until a timeout */
	long left;		/* seconds left until next timeout */
};

void timer_init();
int timer_sleep();

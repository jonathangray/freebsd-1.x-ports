#include <stdio.h>
#include <signal.h>
#include <sys/time.h>

#define USPS    1000000         /* number of microseconds in a second */
#define TICK    10000           /* system clock resolution in microseconds */

static int ringring;
static void (*ofunc)();
    
static void
sleepx()
{
        ringring = 1;
}

void
set_timer(n)
unsigned n;
{
    struct itimerval itv;
    register struct itimerval *itp = &itv;
    if (n == 0)
    {
	ringring = 1;
	return;
    }
    timerclear(&itp->it_interval);
    itp->it_value.tv_sec = n / USPS;
    itp->it_value.tv_usec = n % USPS;
    ofunc = (void (*)())signal(SIGALRM, sleepx);

    ringring = 0;
    (void) setitimer(ITIMER_REAL, itp, (struct itimerval *)0);
}

#ifndef sigmask
#define sigmask(m)	(1 << ((m)-1))
#endif

void
wait_timer()
{
    while (!ringring)
	sigpause( ~sigmask(SIGALRM));
    signal(SIGALRM, ofunc);
}

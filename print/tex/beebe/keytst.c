/***********************************************************************
This is a simple test program for the keyboard I/O package.  It tests
each of the input modes by looping waiting for input, displaying the
counts and input bytes found, and exiting when 5 successive input
checks find nothing.
[15-Nov-87]
***********************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "keydef.h"

#ifdef OS_PCDOS
#include <time.h>

#if    ANSI_PROTOTYPES
void	sleep(int seconds);
#else
void	sleep();
#endif

#endif

void	abortjob();
void	keytst();

void
main()
{
    if (kbopen(KB_NORMAL) == EOF)
	abortjob("kbopen");

    (void)printf("\fNormal mode test\r\n");
    keytst();

    (void)printf("\fCbreak mode test\r\n");
    if (kbmode(KB_CBREAK) == EOF)
	abortjob("kbmode");
    keytst();

    (void)printf("\fRaw mode test\r\n");
    if (kbmode(KB_RAW) == EOF)
	abortjob("kbmode");
    keytst();

    (void)printf("\fNormal mode test\r\n");
    if (kbmode(KB_NORMAL) == EOF)
	abortjob("kbmode");
    keytst();

    if (kbclose() == EOF)
	abortjob("kbclose");
    exit(0);
}

void
abortjob(msg)
{
    (void)fprintf(stderr,"?%s error return\r\n",msg);
    exit(1);
}

void
keytst()
{
    register int c;
    register int count;
    register int no_input;

    no_input = 0;
    (void)printf("Test will complete when no input is found after 5 tries\n");

    for (;;)			/* "infinite" loop */
    {
	(void)sleep(1);		/* wait 1 sec */
	count = kbinput();
	if (count == EOF)
	    abortjob("kbinput");
        if (count == 0)
	    no_input++;
        else
	    no_input = 0;

	(void)printf("kbinput() = %d [",count);

	for ( ; count > 0; count--)
	{
	    if ((c = kbget()) == EOF)
	        abortjob("kbget");
	    if (isprint(c))
		(void)putchar(c);
	    else
		(void)printf("\\%03o",c);
	    fflush(stdout);
	}

	(void)printf("]\r\n");
	fflush(stdout);
	if (no_input >= 5)
	    return;
    }
}

#ifdef OS_PCDOS
void
sleep(seconds)
int seconds;
{
	long start_time;
	start_time = time((time_t*)NULL);
	if (seconds < 2) /* must wait 2 ticks to ensure passage of 1 second */
		seconds = 2;
	while ((time((time_t*)NULL) - start_time) < (long)seconds)
		/* waste time */ ;		
}
#endif


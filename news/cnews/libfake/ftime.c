/*
 *  A reimplementation of the 'deprecated' ftime
 *  system call using modern BSD 4.3 and later calls.
 *  Note that under BSDI/386, the kernel runs on UTC
 *  time so we cannot really use the time zone info
 *  from the gettimeofday system call. Therefore we
 *  use the localtime library function which does
 *  all appropriate conversions.
 *  Erik Forsberg.
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>

int ftime(tbp)
	struct timeb *tbp;
{
	struct timeval tv;
	struct tm *tmp;

	if (tbp == NULL)
		return(-1);

	if (gettimeofday(&tv, (struct timezone *) NULL) < 0)
		return(-1);

	if ((tmp = localtime(&tv.tv_sec)) == NULL)
		return(-1);

	tbp->time = tv.tv_sec;
	tbp->millitm = tv.tv_usec / 1000;

	tbp->timezone = - tmp->tm_gmtoff / 60;
	tbp->dstflag = tmp->tm_isdst;

	return(0);
}

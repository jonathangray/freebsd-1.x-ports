
static char rcsid[] ="@(#)$Id: audit.c,v 1.2 1993/08/27 00:54:10 smace Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 1.2 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein - elm@DSI.COM
 *			dsinc!elm
 *
 *******************************************************************************
 * $Log: audit.c,v $
 * Revision 1.2  1993/08/27 00:54:10  smace
 * Upgrade elm2.4 pl23beta elm2.4 pl23beta2
 *
 * Revision 5.3  1993/02/09  19:04:38  syd
 * use standard include method on time.h style includes
 *
 * Revision 5.2  1993/01/27  21:30:10  syd
 * fix include syntax
 *
 * Revision 5.1  1993/01/27  19:40:18  syd
 * Initial checkin
 *
 *
 ******************************************************************************/


/** allow date/time to be combined with username in verbose audit

**/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#ifdef I_TIME
#  include <time.h>
#endif
#ifdef I_SYSTIME
#  include <sys/time.h>
#endif

#include "defs.h"
#include "filter.h"
#include "s_filter.h"

extern int      errno;

char           *
date_n_user()
{
    static char     date[NLEN];
    static char    *stuff = NULL;
    time_t          now;

    now = time(NULL);
    if (!strftime(date, NLEN, "%c", localtime(&now)))
        return (username);
    if (stuff)
        free(stuff);
    if (!(stuff = malloc(strlen(username) + strlen(date) + 3)))
        return (username);
    sprintf(stuff, "%s %s", date, username);
	return(stuff);
}


/*
 * misc - general miscellaneous routines
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * Assorted miscellaneous routines.
 *
 * $Id: misc.c,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: misc.c,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/07  11:08:12  root
 * Added stradd function.
 *
 * Revision 1.6  1993/04/22  18:31:11  root
 * Hey, it's 1993!
 *
 * Revision 1.4  1993/02/14  14:53:27  root
 * In log_sys if any message ids are present, then submit the
 * currently open batch and write out the unretrieved message
 * ids to slurp.<hostname> along with the new time.
 *
 * Revision 1.0  1992/11/27
 * Initial coding.
 *
 */

#include "slurp.h"
#include <stdarg.h>

static int in_log_sys = FALSE;

static void log_doit (int sysflag, const char *fmt, va_list ap);


/*
 * stradd - If string1 is not NULL, then concatenate string1 and
 * string2, remallocing the space the first occupies to provide
 * enough room for them both. If string1 is null, then malloc space
 * for string2 and copy string2 to it. Returns location of string.
 */

	char *
stradd (const char *string1, const char *string2)
	{
	size_t len;
	char *new;

	if (string1 == NULL)
		{
		len = strlen (string2) + sizeof (char);
		if ((new = (char *) malloc (len)) == NULL)
			log_sys ("stradd: malloc %d bytes", len);
		(void) strcpy (new, string2);
		}
	else
		{
		len = strlen (string1) + strlen (string2) + sizeof (char);
		if ((new = (char *) realloc ((void *) string1, len)) == NULL)
			log_sys ("stradd: realloc %d bytes", len);
		(void) strcat (new, string2);
		}
	return (new);
	}


/*
 * log_ret - Log a message to stderr or syslog related to a system call
 * containing the appropriate system error message and return.
 */

	void
log_ret (const char *fmt, ...)
	{
	va_list ap;

	va_start (ap, fmt);
	log_doit (TRUE, fmt, ap);
	va_end (ap);
	return;
	}


/*
 * log_sys - Log a message to stderr or syslog related to a system call.
 * containing the appropriate system error message and exit program.
 * If any message ids in the tree then write out slurp.<hostname> file
 * and close the batch if open.
 */

	void
log_sys (const char *fmt, ...)
	{
	va_list ap;

	va_start (ap, fmt);
	log_doit (TRUE, fmt, ap);
	va_end (ap);
	if ((!in_log_sys) && (root != NULL))
		{
		in_log_sys = TRUE;
		enqueue_batch ();
		if ((!no_time_flag) && (!no_id_load_flag))
			set_ntime ();
		}
	exit (1);
	}


/*
 * log_msg - Log a message to stderr or syslog unrelated to a system call.
 */

	void
log_msg (const char *fmt, ...)
	{
	va_list ap;

	va_start (ap, fmt);
	log_doit (FALSE, fmt, ap);
	va_end (ap);
	return;
	}


/*
 * log_doit - Write an error message to stderr if debug_flag is set or
 * syslog if not set. If sysflag is true then the last system error
 * message is appended.
 */

	static void
log_doit (int sysflag, const char *fmt, va_list ap)
	{
	int errnosave;
	char buf [BUFSIZ];

	errnosave = errno;
	(void) vsprintf (buf, fmt, ap);
	if (sysflag)
		(void) sprintf (buf + strlen (buf), ": %s", strerror (errnosave));
	(void) strcat (buf, "\n");
#ifdef SYSLOG
		if (!debug_flag)
			syslog (LOG_ERR, buf);
		else
#endif
			(void) fprintf (stderr, "%s: %s", pname, buf);
	}


/* END-OF-FILE */

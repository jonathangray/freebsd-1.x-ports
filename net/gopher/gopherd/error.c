/********************************************************************
 * lindner
 * 3.5
 * 1993/07/30 17:19:53
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/error.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: error.c
 * Error handling/logging routines.
 *********************************************************************
 * Revision History:
 * error.c,v
 * Revision 3.5  1993/07/30  17:19:53  lindner
 * Fix for NeXTs
 *
 * Revision 3.4  1993/07/20  23:58:01  lindner
 * LOGGopher mods
 *
 * Revision 3.3  1993/07/10  04:21:55  lindner
 * mods for solaris2.2
 *
 * Revision 3.2  1993/07/07  19:39:09  lindner
 * fix for linux
 *
 * Revision 3.1.1.1  1993/02/11  18:02:50  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


/*
 * Error handling routines from Stevens: UNIX network programming
 * pp 722-731
 */

/*
 * Error handling routines.
 *
 * The functions in this file are independent of any application
 * variables, and may be used in any C program.  Either of the names
 * CLIENT of SERVER may be defined when compiling this function.
 * If neither are defined, we assume SERVER.
 */

#include "gopherd.h"
void my_perror();

#ifndef NULL
#define NULL ((void *) 0)
#endif

extern char *pname;

#ifndef NOSYSLOG
 /*
  * Under useful OS's, these server routines use the syslog(3) facility.
  * They don't append a newline for example.
  */

#include <syslog.h>

#else /* no syslog() */
/*
 * There really ought to be a better way to handle server logging
 * under System V
 */
#define syslog(a,b)	fprintf(stderr, "%s\n", (b))
#define openlog(a,b,c)	fprintf(stderr, "%s\n", (a))

#endif  /* NOSYSLOG */
#if defined(__STDC__) && !defined(NeXT)
#define __stdc__
#endif

#if defined(__stdc__)
#include <stdarg.h>
#else
#include <varargs.h>
#endif


char emesgstr[255] = {0};  /* used by all server routines */

/*
 * Identify ourselves, for syslog() messages.
 *
 * LOG_PID is an option that says prepend each message with our pid.
 * LOG_CONS is an option that says write to the console if unable to
 * send the message to syslogd.
 * LOG_DAEMON is our facility.
 */

void
err_init()
{
	/* have to be able to reopen, since daemon_start keeps closing us */
	closelog();	/* in case we've been closed by daemon_start */
#ifdef LOG_DAEMON
	openlog("gopherd", (LOG_PID|LOG_CONS), LOG_DAEMON);
#else
	/* old old syslog - probably Ultrix */
	openlog("gopherd", LOG_PID);
#endif
}

/*
 * Fatal error.  Print a message and terminate.
 * Don't print the system's errno value.
 *
 *	err_quit(str, arg1, arg2, ...)
 */

#ifdef __stdc__
err_quit(char *va_alist, ...)
#else
/*VARARGS1*/
void
err_quit(va_alist)
va_dcl
#endif
{
	va_list args;
	char *fmt;

#ifdef __stdc__
	fmt = va_alist;
	va_start(args, va_alist);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif

	(void) vsprintf(emesgstr, fmt, args);
	va_end(args);

	syslog(LOG_ERR, emesgstr);
	LOGGopher(-1, "%s", emesgstr);

	exit(1);
}


/*
 * Fatal error related to a system call.  Print the message and terminate.
 * Don't dump core, but do print the systems errno value and its associated
 * message.
 */

#ifdef __stdc__
void err_sys(char *va_alist, ...)
#else
/*VARARGS*/
void
err_sys(va_alist)
va_dcl
#endif
{
	va_list args;
	char *fmt;

#ifdef __stdc__
	fmt = va_alist;
	va_start(args,va_alist);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif
	(void) vsprintf(emesgstr, fmt, args);
	va_end(args);

	my_perror();
	syslog(LOG_ERR, emesgstr);
	LOGGopher(-1, "%s", emesgstr);

	exit(1);
}


/*
 * Recoverable error.  Print a message, and return to caller.
 */

#ifdef __stdc__
void err_ret(char *va_alist, ...)
#else
/*VARARGS1*/
void
err_ret(va_alist)
va_dcl
#endif
{
	va_list args;
	char *fmt;

#ifdef __stdc__
	fmt = va_alist;
	va_start(args, va_alist);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif
	(void) vsprintf(emesgstr, fmt, args);
	va_end(args);

	my_perror();
	syslog(LOG_ERR, emesgstr);
	LOGGopher(-1, "%s", emesgstr);

	return;
}


/*
 * Fatal error.  Print a message, dump core and terminate.
 */

#ifdef __stdc__
void err_dump(char *va_alist, ...)
#else
/*VARARGS1*/
void
err_dump(va_alist)
va_dcl
#endif
{
	va_list args;
	char *fmt;

#ifdef __stdc__
	fmt = va_alist;
	va_start(args, va_alist);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif
	(void) vsprintf(emesgstr, fmt, args);
	va_end(args);

	my_perror();
	syslog(LOG_ERR, emesgstr);
	LOGGopher(-1, "%s", emesgstr);

	abort();
}

/*
 * Print the UNIX errno value.
 * We just append it the the end of the emesgstr[] array.
 */

void my_perror()
{
	int len;
	char *sys_err_str();

	len = strlen(emesgstr);
	sprintf(emesgstr + len, " %s", sys_err_str());
}


extern int errno;		/* UNIX error number */
extern int sys_nerr;		/* # of error message strings in sys table */
extern char *sys_errlist[];	/* the system error message table */

#ifdef SYS5
int t_errno;
int t_nerr;
char *t_errlist[1];
#endif

/*
 * Return a string containing some additional operating system
 * dependent information.
 * Note that different versions of UNIX assign different meanings
 * to the same value of "errno".  This means that if an error
 * condition is being sent ot another UNIX system, we must interpret
 * the errno value on the system that generated the error, and not
 * just send the decimal value of errno to the other system.
 */

char *
sys_err_str()
{
	static char msgstr[200];

	if (errno != 0) {
		if (errno >0 && errno < sys_nerr)
			sprintf(msgstr, "(%s)", sys_errlist[errno]);
		else
			sprintf(msgstr, "(errno = %d)", errno);
	} else {
		msgstr[0] = '\0';
	}

#ifdef SYS5

	if (t_errno != 0) {
		char tmsgstr[100];

		if (t_errno > 0 && t_errno < sys_nerr)
			sprintf(tmsgstr, " (%s)", t_errlist[t_errno]);
		else
			sprintf(tmsgstr, ", (t_errno = %d)", t_errno);

		strcat(msgstr, tmsgstr);
	}
#endif

	return(msgstr);
}



#ifndef _regexp_h_
#define _regexp_h_

/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#define NSUBEXP  10
typedef struct regexp {
	char *startp[NSUBEXP];
	char *endp[NSUBEXP];
	char regstart;		/* Internal use only. */
	char reganch;		/* Internal use only. */
	char *regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;

/*
 * These definitions added for aXe to avoid a clash with their use
 * in Tcl (Extension language) where modified versions are used.
 */
#define regcomp Regcomp
#define regexec Regexec

extern regexp *regcomp();
extern int regexec();

/*
 * These declarations commented out for aXe; regsub not used and
 * different error handling from Tcl's is required. regerror has
 * been moved inside regexp.c and made static.
 *
 * extern void regsub(); 
 * extern void regerror(); 
 */
static void regerror();

/*
 * New (global) variable for aXe; used to communicate errors.
 */
extern char *RegError;

#endif

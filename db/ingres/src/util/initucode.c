#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <pwd.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SGTTY_H
#include <sgtty.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <version.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)initucode.c	8.5 	2/8/85)

/*
**  INITUCODE -- initialize standalone process
**
**	This function initializes a standalone process, initializing
**	a lot of global variables, scanning the argument vector for
**	some special flags (-u and +-w), seperating flags and
**	parameters, and so forth.
**
**	Every standalone program should begin with the lines:
**			i = initucode(argc, argv, ...);
**			switch (i)
**				...
**
**	On a return of 2, 3, or 4, essentially none of the processing
**	is done (particularly true with return 4).  Virtually nothing
**	can be done in the calling program except print a "usage"
**	message and exit.  The exception to this is that 'Pathname'
**	is set, so that it can be used in the error printing.  For
**	example, ingres.c cats file .../files/usage on this sort of
**	error.
**
**	If it is preferable to not lock the database at this time,
**	the 'waitmode' parameter should be passed as -1.  This still
**	causes the 'Wait_action' variable to be initialized, but the
**	database is not actually locked.  It can be locked by calling:
**		db_lock(Dbpath, M_EXCL);
**	at the proper time.
**
**	For the main effects of this routine, see the "Side Effects"
**	section below.
**
**	Parameters:
**		argc -- argc from main.
**		argv -- argv from main.
**		dbflag -- TRUE -- take the first parameter as the
**				database name.
**			FALSE -- don't take the first parameter as
**				the database name.
**		paramlist -- a pointer to an array[4] of pointers
**			to character; set to the extra fields of
**			the users file entry for the real user
**			executing the code (not the user on the
**			-u flag).  If NULL, this is ignored.
**		waitmode -- M_EXCL -- set an exclusive lock on the
**				database.
**			M_SHARE -- set a shared lock on the database.
**			-1 -- don't set a lock on the database.
**				However, other stuff (Wait_action) is
**				still set up so that the lock can be
**				placed later by calling 'db_lock'.
**
**	Returns:
**		0 -- everything is ok.
**		1(NODB) -- the database does not exist.
**		2(NOACCESS)-- you are not authorized to access this database.
**		3(INVALIDUSR)-- you are not a valid INGRES user.
**		4(NODBNAME)-- no database name was specified (only if dbflag
**			== TRUE).
**		5(INDIRECT)-- everything is ok, but there was an indirect
**			taken.
**		6(INDNODB)-- there was an indirect taken, but there was no
**			database there.
**
**		If dbflag == FALSE, you can only get returns 0 and
**			3.
**
**	Side Effects:
**		A lot of variables are set, as follows:
**
**		Dbpath -- set to the pathname of the database (only
**			if dbflag == TRUE).  It is set even if the
**			database does not exist.
**		Parmvect -- set to the parameters from argv, that is,
**			anything not beginning with '+' or '-'.
**		Flagvect -- set to the flags from argv, that is,
**			everything beginning with '+' or '-'.  The
**			flags '+w', '-w', and '-u' are stripped out,
**			however.
**		Wait_action -- set to the appropriate action (A_SLP
**			or A_RTN) based on the +-w flags and whether
**			we are running in background or not.
**			This is automatically used by 'db_lock()'.
**		Usercode -- set to the persons effective user code
**			(that is, after the -u processing).  Only
**			the INGRES user or the DBA can use the -u
**			flag.
**		Pathname -- set to the pathname of the INGRES subtree.
**		Status -- an integer set to the user status field
**			of the users file for the real user.
**		Ing_uid -- set to the user id of the INGRES user.
**
**		The rubout signal (signal 2) is caught, and refered
**		to the standard rubout processor (see rub.c); thus,
**		a routine called 'rubproc' must be defined in the
**		standalone code (which will just call exit, in the
**		normal case).
**
**		The 'adminhdr' part of the 'Admin' struct is filled
**		in.  This is not done with readadmin() and is not
**		equivalent to an 'admininit()', but it does make
**		the DBA and database status available.
**
**		This routine can also exit immediately with an
**		error message.
**
**	Defined Constants:
**		MAXPARGS -- the maximum number of parameter type
**			arguments to any standalone program.
**		MAXFARGS -- the maximum number of flag type arg-
**			uments to any standalong program (not inclu-
**			ding flags in the users file, and the +-w
**			and -u flags).
**
**	Files:
**		/etc/passwd -- to get the pathname for user "ingres".
**		.../files/users -- to get all the per-user information,
**			and to process the -u flag.
**
**	Compilation Flags:
**		none
**
**	Trace Flags:
**		none
*/


char	*Usercode;	/* the usercode of the effective user */
char	*Pathname;	/* path of INGRES subtree */
int	Rubignored;	/* set if rubouts ignored */
		/* (also in initproc for system processes) */
int	Wait_action;	/* the action on the db_lock */
char	*Dbpath;	/* the pathname of the database */
int	Ing_uid;	/* the user id of the INGRES user */
jmp_buf	Initbuf;	/* Buffer to go back to initucode with */

#ifndef gtty
#define gtty(fd, argp) ioctl(fd, TIOCGETP, argp)
#endif
#ifndef stty
#define stty(fd, argp) ioctl(fd, TIOCSETP, argp)
#endif

static char	**Parmvect;	/* the parameters from argv */
static int	parmc;

static char	**Flagvect;	/* the flags from argv */
static int	flagc;

static char *
strnsave(char *s, int n)
{
	char	*cp;

	cp = xalloc(n + 1, 0, 1);
	(void) memcpy(cp, s, n);
	cp[n] = 0;
	return(cp);
}

char *
getparmvect(int n)
{
	return((n < 0 || n >= parmc) ? (char *) NULL : Parmvect[n]);
}

static void
setparmvect(char *s)
{
	if (parmc == 0) {
		Parmvect = xalloc(32 * sizeof(char *), 1, 0);
	} else if (parmc % 32 == 0) {
		Parmvect = xrealloc(Parmvect, (parmc + 32) * sizeof(char *));
	}
	if (Parmvect == (char **) NULL) {
		printf("Too many parmameters (%d)\n", parmc);
		exit(1);
	}
	Parmvect[parmc++] = strnsave(s, strlen(s));
}

char *
getflagvect(int n)
{
	return((n < 0 || n >= flagc) ? (char *) NULL : Flagvect[n]);
}

static void
setflagvect(char *s)
{
	if (flagc == 0) {
		Flagvect = xalloc(32 * sizeof(char *), 1, 0);
	} else if (flagc % 32 == 0) {
		Flagvect = xrealloc(Flagvect, (flagc + 32) * sizeof(char *));
	}
	if (Flagvect == (char **) NULL) {
		printf("Too many flags (%d)\n", flagc);
		exit(1);
	}
	Flagvect[flagc++] = strnsave(s, strlen(s));
}

/*
**  DB_LOCK -- lock database
**
**	Locks the database.  Everyone should do this before using any
**	database.
**
**	Parameters:
**		database -- the pathname of the database.
**		mode -- M_EXCL -- get an exclusive lock.
**			M_SHARE -- get a shared lock.
**
**	Returns:
**		none
**
**	Side Effects:
**		Alockdes is opened.
*/

lock_req_t	Lock;	/* the database lock structure */

void
db_lock(int mode)
{
	if ((Admin.ad_h.adm_flags & A_DBCONCUR) == 0) {
		return;
	}
	if (Alockdes < 0) {
		Alockdes = start_up_lock_driver();
	}
	if (setdbl(Wait_action, mode) < 0) {
		printf("Database temporarily unavailable\n");
		exit(1);
	}
}

/*
**  INITDBPATH -- initialize the pathname of the database
**
**	The pathname of a specified database is created.  Indirection
**	via a file is supported, so that if the pathname is a file,
**	the first line of the file is read and used as the pathname
**	of the real database.
**
**	Parameters:
**		database -- the name of the database.  If NULL,
**			the pathname of datadir is returned.
**		dbbuf -- a buffer into which the pathname should
**			be dumped.
**		follow -- if set, follow the indirect chain of
**			database pathnames.
**
**	Returns:
**		0(DBEXIST)-- database exists in datadir
**		1(PTR2DB)-- database exists, but I followed a pointer.
**		2(NODBS)-- database doesn't exist in datadir.
**		3(PRT2NODBS)-- database doesn't exist, but I followed a pointer.
**
**	Side Effects:
**		none.
*/

int
initdbpath(char *database, char *dbpath, int follow)
{
	struct stat	ibuf;
	register char	*d;
	register FILE	*f;
	register int	phase;
	int		retval;
	int		uid;

	d = dbpath;

	if (database == NULL) {
#ifndef xDBPATH
		concat(Pathname, "/data/base/", d);
#else
		smove(xDBPATH, d);
#endif
		return (DBEXIST);
	}

	/* get the basic pathname */
	concat(ztack(Pathname, "/datadir/"), database, d);

	/*
	** Iterate looking for database.
	**	"Phase" is what we are trying:
	**	   -1 -- looking in datadir
	**	    0 -- looking in data/base
	**	    1 -- following indirect.
	*/

	retval = NODBS;
	for (phase = -1;;) {
		/* find out what sort of filesystem node this is */
		if (stat(d, &ibuf) < 0) {
			if (phase < 0) {
#ifdef xDBPATH
				concat(xDBPATH, database, d);
#else
				concat(ztack(Pathname, "/data/base/"), database, d);
#endif
				phase = 0;
				continue;
			}
			else
				return (retval);
		}
		
		/* set up the lock structure for future use */
		bmove(&ibuf, Lock.dbnode, sizeof(Lock.dbnode));

		retval -= 2;
		if ((ibuf.st_mode & S_IFMT) == S_IFDIR)
			return (retval);
		
		/* if second time through, the database must be a directory */
		if (phase > 0)
			syserr("initdbpath: not direc");
		
		/* if we shouldn't follow the chain, say it exists */
		if (!follow)
			return (PTR2NODBS);
		
		/* it's a file -- see if we can use it */
		uid = ibuf.st_uid;
		if (uid != Ing_uid || (ibuf.st_mode & 0777) != 0600)
			return (PTR2NODBS);
		
		f = fopen(d, "r");
		if (f == NULL)
			syserr("initdbpath: fopen");
	
		/* read the pathname of the database */
		if (fgets(d, MAX_LINE_SIZE, f) == NULL || d[0] != '/')
			syserr("initdbpath: bad indirect");
		*strchr(d, '\n') = '\0';
		fclose(f);

		/* prepare for next iteration */
		retval = 3;
		phase = 1;
	}
}

int
initucode(int argc, char **argv, int dbflag, char **paramlist, int waitmode)
{
	register char	*p;
	char		*q;
	char		c;
	FILE		*iop;
	static char	sbuf[MAX_LINE_SIZE * 2];
	register char	*sbufp;
	char		buf[MAX_LINE_SIZE+1];
	register int	i;
	int		npermit;
	int		rtval;
	char		*field[UF_NFIELDS];
	int		actualuid;
	int	uid;
	int		waitflag;
	char		*userflag;
	struct sgttyb	gttydummy;
	char		**avp;
	char		usr_ovrd[3];
	static short	tvect[100];
	bool		nobuffer;
	struct	passwd	*pwd;

	/*
	**  Set up interrupts.
	*/

	if ( setjmp(Initbuf) )
		exit(-1);
	if (signal(SIGINT, SIG_IGN) == SIG_DFL) {
		signal(SIGINT, rubcatch);
	}

	/*
	**  Do basic initialization, such as setting trace flags.
	*/

	nobuffer = tTrace(argv, 'T', tvect, 100);
	if (!nobuffer)
		set_so_buf();
	sbufp = sbuf;

	/*
	**  Get pathname of INGRES subtree from /etc/passwd file
	**  entry for USERINGRES (presumably "ingres") and save it
	**  in 'Pathname'.
	**
	**  This algorithm suggested by Jim Popa.
	*/

	if ( (pwd = getpwnam(USERINGRES)) == NULL )
		syserr("initucode: No user %s in password file",USERINGRES);
	Pathname = (char *)getenv("INGPATH");
	if (Pathname == NULL) {
		Pathname = sbufp;
		sbufp += smove(pwd->pw_dir, sbufp) + 1;
#ifdef PATHEXT
		sbufp += smove(PATHEXT, sbufp - 1);
#endif PATHEXT
	}

	/* create the INGRES user id */
	Ing_uid = pwd->pw_uid;
	endpwent();

	/*
	**  Scan the argument vector.  The following flags are pulled
	**  out of the vector (and argc and argv are adjusted so it
	**  looks like they never existed):
	**	+w, -w -- (don't) wait for the database to be free.
	**	-uxxx -- run as user xxx.  If first character is a
	**	colon, the format must be '-u:xx' where 'xx' is the
	**	internal user code.
	*/

	avp = argv;
	waitflag = 0;
	userflag = NULL;
	usr_ovrd[0] = 0;

	for (i = argc; --i > 0; ) {
		p = *++avp;
		if (p[0] == '+') {
			if (p[1] == 'w')
				waitflag = 1;
			else
				goto boring;
		} else if (p[0] == '-') {
			switch (p[1]) {
			  case 'w':
				waitflag = -1;
				break;
			
			  case 'u':
				if (p[2] == ':') {
					if (p[3] == 0 || p[4] == 0 || p[5] != 0) {
						printf("Bad flag %s\n", p);
						exit(-1);
					}
					smove(&p[3], usr_ovrd);
				} else {
					userflag = &p[2];
				}
				break;

			  default:
				/* not an interesting flag */
			boring:
				setflagvect(p);
				break;
			}
		} else {
			/* not a flag: save in Parmvect */
			setparmvect(p);
		}
	}

	if (parmc <= 0 && dbflag) {
		return(NODBNAME);	/* no database name specified */
	}

	/*
	**  Scan the "users" file.
	*/

	if ((iop = fopen((char *)ztack(Pathname, "/files/users"), "r")) == NULL)
		syserr("initucode: open error");
	
	/* get uid (out of loop) for test */
	actualuid = getuid();
	
	/* scan users file, one line at a time */
	rtval = INVALIDUSR;
	while ((Usercode == NULL || userflag != NULL) && fgets(buf, MAX_LINE_SIZE, iop) != NULL) {
	
		/* decode users file entry */
		i = 0;
		field[0] = buf;
		for (p = buf; *p != '\n' && *p != '\0'; p++) {
			if (*p == ':') {
				*p = 0;
				i++;
				field[i] = p + 1;
			}
		}
		*p = '\0';

		/* check for correct number of fields */
		if (i != UF_NFIELDS - 1)
			syserr("initucode: users fmt %s", buf);

		/*
		**  Check to see if this entry is the override user.
		**  If so, save his user code in usr_ovrd.
		*/

		if (userflag != NULL && strcmp(userflag, field[UF_NAME]) == 0) {
			smove(field[UF_UCODE], usr_ovrd);
			userflag = NULL;
		}

		/* don't bother with this shit if not needed */
		if (Usercode != NULL)
			continue;
		
		/*
		**  Build the user id of this entry into 'uid'
		**  and see if it is this user.
		*/

		uid = atoi(field[UF_UID]);

		if (uid != actualuid)
			continue;

		/*
		**  We now have the real user entry.
		**	Fetch the usercode, the status bits, and other
		**	fields from the users file, and save them in
		**	a safe place (sbuf).
		*/

		Usercode = sbufp;
                sbufp += smove(field[UF_UCODE], sbufp) + 1;

		setglobalint(STATUS_NAME, oatoi(field[UF_STAT]));
		if (paramlist != NULL) {
			for (i = 0; i < 4; i++) {
				paramlist[i] = sbufp;
				sbufp += smove(field[UF_FLAGS + i], sbufp) + 1;
			}
		}

		/* validate access permission */
		rtval = 0;
		if (!dbflag || (getglobalint(STATUS_NAME) & U_SUPER) != 0)
			continue;
		p = field[UF_DBLIST];
		if (*p == 0)
			continue;

		/* select permission/no-permission */
		npermit = 0;
		if (*p == '-') {
			p++;
			npermit++;
		}

		/* scan for database listed */
		if (!npermit)
			rtval = NOACCESS;
		for (c = *p; c != 0; p = q + 1) {
			for (q = p; *q != ',' && *q != 0; q++)
				continue;
			c = *q;
			*q = 0;
			if (strcmp(Parmvect[0], p) == 0) {
				rtval = npermit ? NOACCESS : 0;
				break;
			}
		}
	}
	fclose(iop);

	if (rtval != 0)
		return (rtval);

	/*
	**  Check for existance of the database.  This is done by
	**	first building the pathname of the database into
	**	'Dbpath', and then reading the admin file (just
	**	the ad_h part).
	*/

	if (dbflag) {
		Dbpath = sbufp;
		switch (i = initdbpath(Parmvect[0], Dbpath, TRUE)) {
		  case DBEXIST:
			rtval = 0;
			break;

		  case PTR2DB:
			rtval = INDIRECT;
			break;

		  case NODBS:
			rtval = NODB;
			break;

		  case PTR2NODBS:
			rtval = INDNODB;
			break;

		  default:
			syserr("initucode: initdbpath %d", i);
		}
		sbufp += strlen(Dbpath) + 1;

		if (rtval == 0 || rtval == INDIRECT) {
			i = open((char *)ztack(Dbpath, "/admin"), O_RDONLY);
			if (i < 0)
				rtval += 1;
			else {
				/* open and check admin file */
				checkadmin(i);
				close(i);
			}
		}
	}

	/*
	**  Check to see if the name on the -u flag is valid, and
	**	that this user is allowed to use it.
	*/

	if (userflag != NULL) {
		printf("Invalid user name %s\n", userflag);
		exit(-1);
	}
	if (usr_ovrd[0] != '\0') {
		if ((getglobalint(STATUS_NAME) & U_SUPER) == 0) {
			if (!dbflag ||
			   !bequal(Admin.ad_h.adm_owner, Usercode, USERCODE_SIZE)) {
				printf("You may not use the -u flag\n");
				exit(-1);
			}
		}
		bmove(usr_ovrd, Usercode, USERCODE_SIZE);
	}

	/*
	**  Process the +-w flag.
	**	First, determine the locking mode.  If +w, always
	**	wait; if -w, never wait; if unspecified, wait if in
	**	background, but print error and exit if running
	**	interactive.
	*/

	if (waitflag > 0 || (waitflag == 0 && gtty(0, &gttydummy) < 0))
		Wait_action = A_SLP;
	else
		Wait_action = A_RTN;
	if (dbflag && waitmode >= 0)
		db_lock(waitmode);
	
	/*
	**  Return authorization value.
	*/

	return (rtval);
}

/*	SCCS Id: @(#)vmsunix.c	3.1	93/05/15	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

/* This file implements things from unixunix.c, plus related stuff */

#include "hack.h"
#include "date.h"	/* generated by 'makedefs' */

#include <descrip.h>
#include <dvidef.h>
#include <jpidef.h>
#include <ssdef.h>
#include <errno.h>
#include <signal.h>
#undef off_t
#ifdef GNUC
#include <sys/stat.h>
#else
#include <stat.h>
#endif
#include <ctype.h>

#ifndef L_tmpnam
#define L_tmpnam 256
#endif

extern unsigned long SYS$SETPRV();
extern unsigned long LIB$GETDVI(), LIB$GETJPI(), LIB$SPAWN(), LIB$ATTACH();
extern unsigned long SMG$INIT_TERM_TABLE_BY_TYPE(), SMG$DEL_TERM_TABLE();
#define vms_ok(sts) ((sts) & 1) /* odd => success */

static long exe_time = BUILD_TIME;

static int FDECL(veryold, (int));
static char *NDECL(verify_term);
#if defined(SHELL) || defined(SUSPEND)
static void FDECL(hack_escape, (BOOLEAN_P,const char *));
static void FDECL(hack_resume, (BOOLEAN_P));
#endif

void
gethdate(name)		/* get the creation date & time of file 'name' */
const char *name;	/* assumed to be argv[0], full path to nethack.exe */
{
	struct stat hbuf;
	/*
	   [f]stat() won't work across a DECnet link.  Would somebody be
	   masochistic enough to try ``$ RUN node::NETHACK''?  One hopes
	   not, but don't inadvertently forbid it.  If stat() fails, use
	   the compile time provided by 'makedefs' instead of just quitting.
	   The playground must be local, otherwise all level files will
	   fail their status check, but the exe image could be remote.

	   Note that we care about the executable's creation time, not
	   its modification time, since the latter is touched by changing
	   file protection or renaming, two naive ways someone might use
	   to disable the program during prime time.
	 */
	if (stat(name, &hbuf) == 0) exe_time = hbuf.st_ctime;
}

boolean
uptodate(fd)
int fd;
{
	struct stat buf;

	if (fstat(fd, &buf) != 0)
		pline("Cannot get status of saved level? ");
	else if (buf.st_mtime < exe_time)
		pline("Saved level is out of date. ");
	else
		return TRUE;	/* passed the test */
	wait_synch();
	return FALSE;			/* failed the test */
}

static int
veryold(fd)
int fd;
{
	register int i;
	time_t date;
	struct stat buf;

	if(fstat(fd, &buf)) return(0);			/* cannot get status */
#ifndef INSURANCE
	if(buf.st_size != sizeof(int)) return(0);	/* not an xlock file */
#endif
	(void) time(&date);
	if(date - buf.st_mtime < 3L*24L*60L*60L) {	/* recent */
		int lockedpid;	/* should be the same size as hackpid */
		unsigned long status, dummy, code = JPI$_PID;

		if (read(fd, (genericptr_t)&lockedpid, sizeof(lockedpid)) !=
				sizeof(lockedpid))	/* strange ... */
			return 0;
		status = LIB$GETJPI(&code, &lockedpid, 0, &dummy);
		if (vms_ok(status) || status != SS$_NONEXPR)
			return 0;
	}
	(void) close(fd);

	/* cannot use maxledgerno() here, because we need to find a lock name
	 * before starting everything (including the dungeon initialization
	 * that sets astral_level, needed for maxledgerno()) up
	 */
	for(i = 1; i <= MAXDUNGEON*MAXLEVEL + 1; i++) {
		/* try to remove all */
		set_levelfile_name(lock, i);
		(void) delete(lock);
	}
	set_levelfile_name(lock, 0);
	if(delete(lock)) return(0);			/* cannot remove it */
	return(1);					/* success! */
}

void
getlock()
{
	register int i = 0, fd;

	/* idea from rpick%ucqais@uccba.uc.edu
	 * prevent automated rerolling of characters
	 * test input (fd0) so that tee'ing output to get a screen dump still
	 * works
	 * also incidentally prevents development of any hack-o-matic programs
	 */
	if (isatty(0) <= 0)
		error("You must play from a terminal.");

	/* we ignore QUIT and INT at this point */
	if (!lock_file(HLOCK, 10)) {
		wait_synch();
		error("Quitting.");
	}

	regularize(lock);
	set_levelfile_name(lock, 0);
	if(locknum > 25) locknum = 25;

	do {
		if(locknum) lock[0] = 'a' + i++;

		if((fd = open(lock, 0, 0)) == -1) {
			if(errno == ENOENT) goto gotlock;    /* no such file */
			perror(lock);
			unlock_file(HLOCK);
			error("Cannot open %s", lock);
		}

		if(veryold(fd))	/* if true, this closes fd and unlinks lock */
			goto gotlock;
		(void) close(fd);
	} while(i < locknum);

	unlock_file(HLOCK);
	error(locknum ? "Too many hacks running now."
		      : "There is a game in progress under your name.");

gotlock:
	fd = creat(lock, FCMASK);
	unlock_file(HLOCK);
	if(fd == -1) {
		error("cannot creat lock file.");
	} else {
		if(write(fd, (char *) &hackpid, sizeof(hackpid))
		    != sizeof(hackpid)){
			error("cannot write lock");
		}
		if(close(fd) == -1) {
			error("cannot close lock");
		}
	}
}	

void
regularize(s)	/* normalize file name */
register char *s;
{
	register char *lp;

	for (lp = s; *lp; lp++)         /* note: '-' becomes '_' */
	    if (!(isalpha(*lp) || isdigit(*lp) || *lp == '$'))
			*lp = '_';
}

#undef getuid
int
vms_getuid()
{
    return (getgid() << 16) | getuid();
}

/* return a copy of the 'base' portion of a filename */
char *
basename(name)
const char *name;
{
    unsigned len;
    char *base;
    register const char *p;

    /* skip directory/path */
    if ((p = strrchr(name, ']')) != 0) name = p + 1;
    if ((p = strrchr(name, '>')) != 0) name = p + 1;
    if ((p = strrchr(name, ':')) != 0) name = p + 1;
    if ((p = strrchr(name, '/')) != 0) name = p + 1;
    if (!*name) name = ".";		/* this should never happen */

    /* find extension/version and derive length of basename */
    if ((p = strchr(name, '.')) == 0 || p == name) p = strchr(name, ';');
    len = p && p > name ? p - name : strlen(name);

    /* copy and return name */
    base = strncpy((char *)alloc(len + 1), name, len);
    base[len] = '\0';
    return lcase(base);
}

#ifndef FAB$C_STMLF
#define FAB$C_STMLF 5
#endif
/* check whether the open file specified by `fd' is in stream-lf format */
boolean
file_is_stmlf(fd)
int fd;
{
    int rfm;
    struct stat buf;

    if (fstat(fd, &buf)) return FALSE;	/* cannot get status? */

#ifdef stat_alignment_fix	/* gcc-vms alignment kludge */
    rfm = stat_alignment_fix(&buf)->st_fab_rfm;
#else
    rfm = buf.st_fab_rfm;
#endif
    return rfm == FAB$C_STMLF;
}

/*------*/
#ifndef LNM$_STRING
#include <lnmdef.h>	/* logical name definitions */
#endif
#define ENVSIZ LNM$C_NAMLENGTH  /*255*/

#define ENV_USR 0	/* user-mode */
#define ENV_SUP 1	/* supervisor-mode */
#define ENV_JOB 2	/* job-wide entry */

/* vms_define() - assign a value to a logical name */
int
vms_define(name, value, flag)
const char *name;
const char *value;
int flag;
{
    struct dsc { int len; const char *adr; };	/* string descriptor */
    struct itm3 { short buflen, itmcode; const char *bufadr; short *retlen; };
    static struct itm3 itm_lst[] = { {0,LNM$_STRING,0,0}, {0,0} };
    struct dsc nam_dsc, val_dsc, tbl_dsc;
    unsigned long result, SYS$CRELNM(), LIB$SET_LOGICAL();

    /* set up string descriptors */
    nam_dsc.len = strlen( nam_dsc.adr = name );
    val_dsc.len = strlen( val_dsc.adr = value );
    tbl_dsc.len = strlen( tbl_dsc.adr = "LNM$PROCESS" );

    switch (flag) {
	case ENV_JOB:	/* job logical name */
		tbl_dsc.len = strlen( tbl_dsc.adr = "LNM$JOB" );
	    /*FALLTHRU*/
	case ENV_SUP:	/* supervisor-mode process logical name */
		result = LIB$SET_LOGICAL(&nam_dsc, &val_dsc, &tbl_dsc);
	    break;
	case ENV_USR:	/* user-mode process logical name */
		itm_lst[0].buflen = val_dsc.len;
		itm_lst[0].bufadr = val_dsc.adr;
		result = SYS$CRELNM(0, &tbl_dsc, &nam_dsc, 0, itm_lst);
	    break;
	default:	/*[ bad input ]*/
		result = 0;
	    break;
    }
    result &= 1;	/* odd => success (== 1), even => failure (== 0) */
    return !result;	/* 0 == success, 1 == failure */
}

/* vms_putenv() - create or modify an environment value */
int
vms_putenv(string)
const char *string;
{
    char name[ENVSIZ+1], value[ENVSIZ+1], *p;   /* [255+1] */

    p = strchr(string, '=');
    if (p > string && p < string + sizeof name && strlen(p+1) < sizeof value) {
	(void)strncpy(name, string, p - string),  name[p - string] = '\0';
	(void)strcpy(value, p+1);
	return vms_define(name, value, ENV_USR);
    } else
	return 1;	/* failure */
}

/*
   Support for VT420 was added to VMS in version V5.4, but as of V5.5-2
   VAXCRTL still doesn't handle it and puts TERM=undefined into the
   environ[] array.  getenv("TERM") will return "undefined" instead of
   something sensible.  Even though that's finally fixed in V6.0, site
   defined terminals also return "undefined" so query SMG's TERMTABLE
   instead of just checking VMS's device-type value for VT400_Series.

   Called by verify_termcap() for convenience.
 */
static
char *verify_term()
{
    char      *term = getenv("NETHACK_TERM");
    if (!term) term = getenv("HACK_TERM");
    if (!term) term = getenv("EMACS_TERM");
    if (!term) term = getenv("TERM");
    if (!term || !*term
	|| !strcmpi(term, "undefined") || !strcmpi(term, "unknown")) {
	static char smgdevtyp[31+1];	/* size is somewhat arbitrary */
	static $DESCRIPTOR(smgdsc, smgdevtyp);
	static $DESCRIPTOR(tt, "TT:");
	unsigned short dvicode = DVI$_DEVTYPE;
	unsigned long devtype = 0L, termtab = 0L;

	(void)LIB$GETDVI(&dvicode, (unsigned short *)0, &tt, &devtype,
			 (genericptr_t)0, (unsigned short *)0);

	if (devtype &&
	    vms_ok(SMG$INIT_TERM_TABLE_BY_TYPE(&devtype, &termtab, &smgdsc))) {
	    register char *p = &smgdevtyp[smgdsc.dsc$w_length];
	    /* strip trailing blanks */
	    while (p > smgdevtyp && *--p == ' ') *p = '\0';
	    /* (void)SMG$DEL_TERM_TABLE(); */
	    term = smgdevtyp;
	}
    }
    return term;
}

/*
   Figure out whether the termcap code will find a termcap file; if not,
   try to help it out.  This avoids modifying the GNU termcap sources and
   can simplify configuration for sites which don't already use termcap.
 */
#define GNU_DEFAULT_TERMCAP "emacs_library:[etc]termcap.dat"
#define NETHACK_DEF_TERMCAP "nethackdir:termcap"
#define HACK_DEF_TERMCAP    "hackdir:termcap"

char *
verify_termcap()	/* called from startup(src/termcap.c) */
{
    struct stat dummy;
    const char *tc = getenv("TERMCAP");
    if (tc) return verify_term();	/* no termcap fixups needed */
    if (!tc && !stat(NETHACK_DEF_TERMCAP, &dummy)) tc = NETHACK_DEF_TERMCAP;
    if (!tc && !stat(HACK_DEF_TERMCAP, &dummy))    tc = HACK_DEF_TERMCAP;
    if (!tc && !stat(GNU_DEFAULT_TERMCAP, &dummy)) tc = GNU_DEFAULT_TERMCAP;
    if (!tc && !stat("[]termcap", &dummy)) tc = "[]termcap"; /* current dir */
    if (!tc && !stat("$TERMCAP", &dummy))  tc = "$TERMCAP";  /* alt environ */
    if (tc) {
	/* putenv(strcat(strcpy(buffer,"TERMCAP="),tc)); */
	vms_define("TERMCAP", tc, ENV_USR);
    } else {
	/* perhaps someday we'll construct a termcap entry string */
    }
    return verify_term();
}
/*------*/

#ifdef SHELL
# ifndef CLI$M_NOWAIT
#  define CLI$M_NOWAIT 1
# endif
#endif

#if defined(CHDIR) || defined(SHELL) || defined(SECURE)
static unsigned long oprv[2];

void
privoff()
{
	unsigned long prv[2] = { ~0, ~0 }, code = JPI$_PROCPRIV;

	(void) SYS$SETPRV(0, prv, 0, oprv);
	(void) LIB$GETJPI(&code, 0, 0, prv);
	(void) SYS$SETPRV(1, prv, 0, 0);
}

void
privon()
{
	(void) SYS$SETPRV(1, oprv, 0, 0);
}
#endif	/* CHDIR || SHELL || SECURE */

#if defined(SHELL) || defined(SUSPEND)
static void
hack_escape(screen_manip, msg_str)
boolean screen_manip;
const char *msg_str;
{
	suspend_nhwindows(msg_str);	/* clear screen, reset terminal, &c */
	(void) signal(SIGQUIT,SIG_IGN);	/* ignore ^Y */
	(void) signal(SIGINT,SIG_DFL);	/* don't trap ^C (implct cnvrs to ^Y) */
}

static void
hack_resume(screen_manip)
boolean screen_manip;
{
	(void) signal(SIGINT, (SIG_RET_TYPE) done1);
# ifdef WIZARD
	if (wizard) (void) signal(SIGQUIT,SIG_DFL);
# endif
	resume_nhwindows();	/* setup terminal modes, redraw screen, &c */
}
#endif	/* SHELL || SUSPEND */

#ifdef SHELL
unsigned long dosh_pid = 0,	/* this should cover any interactive escape */
	mail_pid = 0;	/* this only covers the last mail or phone; */
/*(mail & phone commands aren't expected to leave any process hanging around)*/

int dosh()
{
	return vms_doshell("", TRUE);	/* call for interactive child process */
}

/* vms_doshell -- called by dosh() and readmail() */

/* If execstring is not a null string, then it will be executed in a spawned */
/* subprocess, which will then return.  It is for handling mail or phone     */
/* interactive commands, which are only available if both MAIL and SHELL are */
/* #defined, but we don't bother making the support code conditionalized on  */
/* MAIL here, just on SHELL being enabled.				     */

/* Normally, all output from this interaction will be 'piped' to the user's  */
/* screen (SYS$OUTPUT).  However, if 'screenoutput' is set to FALSE, output  */
/* will be piped into oblivion.  Used for silent phone call rejection.	     */

int
vms_doshell(execstring, screenoutput)
const char *execstring;
boolean screenoutput;
{
	unsigned long status, new_pid, spawnflags = 0;
	struct dsc$descriptor_s comstring, *command, *inoutfile = 0;
	static $DESCRIPTOR(nulldevice, "_NLA0:");

	/* Is this an interactive shell spawn, or do we have a command to do? */
	if (execstring && *execstring) {
		comstring.dsc$w_length = strlen(execstring);
		comstring.dsc$b_dtype = DSC$K_DTYPE_T;
		comstring.dsc$b_class = DSC$K_CLASS_S;
		comstring.dsc$a_pointer = (char *)execstring;
		command = &comstring;
	} else
		command = 0;

	/* use asynch subprocess and suppress output iff one-shot command */
	if (!screenoutput) {
		spawnflags = CLI$M_NOWAIT;
		inoutfile = &nulldevice;
	}

	hack_escape(screenoutput, command ? (const char *) 0 :
     "  \"Escaping\" into a subprocess; LOGOUT to reconnect and resume play. ");

	if (command || !dosh_pid || !vms_ok(status = LIB$ATTACH(&dosh_pid))) {
# ifdef CHDIR
		(void) chdir(getenv("PATH"));
# endif
		privoff();
		new_pid = 0;
		status = LIB$SPAWN(command, inoutfile, inoutfile, &spawnflags,
				   (struct dsc$descriptor_s *) 0, &new_pid);
		if (!command) dosh_pid = new_pid; else mail_pid = new_pid;
		privon();
# ifdef CHDIR
		chdirx((char *) 0, 0);
# endif
	}

	hack_resume(screenoutput);

	if (!vms_ok(status)) {
		pline("  Spawn failed.  (%%x%08lX) ", status);
		mark_synch();
	}
	return 0;
}
#endif	/* SHELL */

#ifdef SUSPEND
/* dosuspend() -- if we're a subprocess, attach to our parent;
 *		if not, there's nothing we can do.
 */
int
dosuspend()
{
	static long owner_pid = -1;
	unsigned long status;

	if (owner_pid == -1)	/* need to check for parent */
		owner_pid = getppid();
	if (owner_pid == 0) {
		pline(
     "  No parent process.  Use '!' to Spawn, 'S' to Save,  or 'Q' to Quit. ");
		mark_synch();
		return 0;
	}

	/* restore normal tty environment & clear screen */
	hack_escape(1,
     " Attaching to parent process; use the ATTACH command to resume play. ");

	status = LIB$ATTACH(&owner_pid);	/* connect to parent */

	hack_resume(1);	/* resume game tty environment & refresh screen */

	if (!vms_ok(status)) {
		pline("  Unable to attach to parent.  (%%x%08lX) ", status);
		mark_synch();
	}
	return 0;
}
#endif	/* SUSPEND */

/*vmsunix.c*/

/*	FILE.C:   for MicroEMACS
 *
 *	The routines in this file handle the reading, writing
 *	and lookup of disk files.  All of details about the
 *	reading and writing of the disk are in "fileio.c".
 *
 *
 * $Log: file.c,v $
 * Revision 1.1  1994/02/01 03:29:22  jkh
 * Initial revision
 *
 * Revision 1.104  1993/09/16  10:57:31  pgf
 * do an 'ls -a' when sending mail about saved files
 *
 * Revision 1.103  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.102  1993/09/06  16:25:48  pgf
 * used-before-set warning cleanup
 *
 * Revision 1.101  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.100  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.99  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.98  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.97  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.96  1993/07/07  11:32:28  pgf
 * use mlforce instead of mlwrite for write-status message
 *
 * Revision 1.95  1993/07/07  09:01:30  pgf
 * took out unnecessary subsequent window check from check_visible_modtimes()
 *
 * Revision 1.94  1993/07/06  12:34:53  pgf
 * tweaks to the check-modtime behavior -- always prompt, whether prompted
 * previously or not, when writing.  also, new routine to only check visible
 * buffers' modtimes, for use after shell escapes.
 *
 * Revision 1.93  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.92  1993/06/24  12:11:08  pgf
 * tag_for_undo() now returns void -- this was the _only_ place that
 * checked the return value anyway.
 *
 * Revision 1.91  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.90  1993/06/02  14:57:32  pgf
 * added FILEC_PROMPT to writeregion's call to mlreply_file to force
 * prompting, and folded some long lines
 *
 * Revision 1.89  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.88  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.87  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.86  1993/05/10  10:08:30  pgf
 * new buffers should not be dos-mode by default
 *
 * Revision 1.85  1993/05/06  11:02:19  pgf
 * don't try to realloc to size 0
 *
 * Revision 1.84  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.83  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.82  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.81  1993/04/21  14:36:10  pgf
 * replace spaces with dashes in unix buffernames
 *
 * Revision 1.80  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.79  1993/04/12  19:35:42  pgf
 * shortened some long lines
 *
 * Revision 1.78  1993/04/09  13:36:47  pgf
 * include sys/stat.h to try to get extern decl for mkdir
 *
 * Revision 1.77  1993/04/02  11:00:02  pgf
 * force scrolling from bottom line when reading pipe
 *
 * Revision 1.76  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.75  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.74  1993/03/16  16:04:01  pgf
 * fix 'parentheses suggested' warnings
 *
 * Revision 1.73  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.72  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.71  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.70  1993/02/23  12:04:15  pgf
 * added support for appending to file (from alistair crooks)
 *
 * Revision 1.69  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.68  1993/01/23  13:38:23  foxharp
 * changes for updating buffer list when writing files out,
 * apollo-specific change for death conditions,
 * use new exit code macros
 *
 * Revision 1.67  1993/01/16  10:32:28  foxharp
 * some lint, some macro-ization of special filenames, a couple pathname
 * manipulators (lengthen_path(), is_pathname())
 *
 * Revision 1.66  1992/12/30  19:54:56  foxharp
 * avoid inf. loop when choosing unique name for buffers that have names
 * containing "-x" in the NBUFN-1 position
 *
 * Revision 1.65  1992/12/23  09:19:26  foxharp
 * allow ":e!" with no file to default to current file, and
 * lint cleanup
 *
 * Revision 1.64  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.63  1992/12/05  13:55:06  foxharp
 * rvstrcpy is now here, and ifdef'ed
 *
 * Revision 1.62  1992/12/04  09:26:43  foxharp
 * added apollo leading '//' fix to canonpath (probably needs work elsewhere
 * as well), and deleted unused assigns
 *
 * Revision 1.61  1992/11/19  09:06:35  foxharp
 * added kdone() call to complete the ksetup() operation, and
 * fixed possible someday bug with crypt leaving open files around
 *
 * Revision 1.60  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.59  1992/08/19  22:56:50  foxharp
 * no longer need to multiply or add to NFILEN
 *
 * Revision 1.58  1992/08/06  23:55:07  foxharp
 * changes to canonical pathnames and directory changing to support DOS and
 * its drive designators
 *
 * Revision 1.57  1992/08/05  22:07:50  foxharp
 * glob() on DOS now does something -- it makes sure the drive designator
 * is uppercase
 *
 * Revision 1.56  1992/08/05  21:51:55  foxharp
 * trim trailing slashes in DOS as well as UNIX
 *
 * Revision 1.55  1992/08/05  21:02:51  foxharp
 * check return of mlyesno explicitly against TRUE, since it might return FALSE 
 * _or_ ABORT
 *
 * Revision 1.54  1992/07/30  07:29:55  foxharp
 * if a requested name has no slashes, try for it as a buffer name, then
 * canonicalize, and try for buffer and then file, like we used to
 *
 * Revision 1.53  1992/07/24  07:49:38  foxharp
 * shorten_name changes
 *
 * Revision 1.52  1992/07/22  09:16:49  foxharp
 * shorten_path now prints shell commands as-is
 *
 * Revision 1.51  1992/07/20  22:49:19  foxharp
 * prototyped code sure is picky...
 *
 * Revision 1.50  1992/07/18  13:13:56  foxharp
 * put all path-shortening in one place (shorten_path()), and took out
 * some old code now unnecessary
 *
 * Revision 1.49  1992/07/17  19:14:30  foxharp
 * deleted unused locals
 *
 * Revision 1.48  1992/07/15  08:54:40  foxharp
 * give up pretense that we can canonicalize relative pathnames -- all
 * pathnames are stored absolute now anyway.  also, make canonpath
 * deal with DOS \ path separators
 *
 * Revision 1.47  1992/07/13  20:18:51  foxharp
 * took out old ifdef BEFORE code
 *
 * Revision 1.46  1992/07/13  20:03:54  foxharp
 * the "terse" variable is now a boolean mode
 *
 * Revision 1.45  1992/07/13  19:38:03  foxharp
 * finished canonicalizing pathnames
 *
 * Revision 1.44  1992/07/13  09:28:37  foxharp
 * preliminary changes for canonical path names
 *
 * Revision 1.43  1992/07/10  22:00:32  foxharp
 * in signal handler, don't exit after calling abort, to work around
 * bug in sunos 4.1.1 on sparcs
 *
 * Revision 1.42  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.41  1992/05/27  08:32:57  foxharp
 * force screen updates while reading from pipes, otherwise bumping the
 * keyboard freezes the screen until the spawned process is done
 *
 * Revision 1.40  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.39  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.38  1992/04/14  08:51:44  pgf
 * ifdef fixups for pjr and DOS
 *
 * Revision 1.37  1992/04/02  08:28:59  pgf
 * fixed the realloc case of quickreadf()
 *
 * Revision 1.36  1992/03/20  09:00:40  pgf
 * fixed typo
 *
 * Revision 1.35  1992/03/19  23:34:53  pgf
 * set b_linecount on reads and writes of a file
 *
 * Revision 1.34  1992/03/19  23:20:22  pgf
 * SIGT for signals, linux portability
 *
 * Revision 1.33  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.32  1992/02/17  09:04:03  pgf
 * fix null filename dereference, and
 * kill registers now hold unsigned chars
 *
 * Revision 1.31  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.30  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed string, to avoid length limits
 *
 * Revision 1.29  1991/11/10  21:21:20  pgf
 * don't look for cr/nl pairs on empty lines
 *
 * Revision 1.28  1991/11/08  13:20:18  pgf
 * lint cleanup, and took out lazy filename matching, and
 * changed the DOSFILES code so it's all in this routine, and made it
 * work with quickreadf, where we don't read every line, and fixed
 * core dumps on long-lined files, and allowed for aborting file read
 * operations
 *
 * Revision 1.27  1991/11/07  03:58:31  pgf
 * lint cleanup
 *
 * Revision 1.26  1991/11/03  17:46:30  pgf
 * removed f,n args from all region functions -- they don't use them,
 * since they're no longer directly called by the user
 *
 * Revision 1.25  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.24  1991/10/31  02:35:04  pgf
 * avoid malloc(0) in quickreadf
 *
 * Revision 1.23  1991/10/27  01:44:06  pgf
 * used has_C_suffix routine to determine c-mode or not
 *
 * Revision 1.22  1991/10/20  23:07:13  pgf
 * shorten the big file buffer if we don't use it all due to long lines
 *
 * Revision 1.21  1991/10/18  01:17:34  pgf
 * don't seek too far when long lines found in quickread
 *
 * Revision 1.20  1991/10/10  12:33:33  pgf
 * changes to support "block malloc" of line text -- now for most files
 * there is are two mallocs and a single read, no copies.  previously there
 * were two mallocs per line, and two copies (stdio's and ours).  This change
 * implies that lines and line text should not move between buffers, without
 * checking that the text and line struct do not "belong" to the buffer.
 *
 * Revision 1.19  1991/09/24  01:19:14  pgf
 * don't let buffer name change needlessly in fileread() (:e!)
 *
 * Revision 1.18  1991/09/13  01:47:09  pgf
 * lone ":f" now gives same info as ^G
 *
 * Revision 1.17  1991/09/10  01:52:21  pgf
 * buffer name now changes to match filename after ":e!" command (fileread)
 *
 * Revision 1.16  1991/08/16  10:59:35  pgf
 * added WFKILLS to pipe-reading updates, so scrolling gets invoked
 *
 * Revision 1.15  1991/08/12  09:25:10  pgf
 * now store w_line in w_traits while buffer is offscreen, so reframe
 * isn't always necessary.  don't force reframe on redisplay.
 *
 * Revision 1.14  1991/08/08  13:19:08  pgf
 * fixed MDDOS processing, and don't allow writes of view-only buffers
 *
 * Revision 1.13  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.12
 * date: 1991/08/06 15:21:21;
 * global/local values
 * 
 * revision 1.11
 * date: 1991/06/25 19:52:35;
 * massive data structure restructure
 * 
 * revision 1.10
 * date: 1991/06/13 15:18:28;
 * fixed comment on glob()
 * 
 * revision 1.9
 * date: 1991/06/03 12:18:17;
 * ifdef'ed TAGS for unresolved ref
 * 
 * revision 1.8
 * date: 1991/05/31 10:58:11;
 * fixed bug in writereg, and
 * made writeregion more like writefile
 * 
 * revision 1.7
 * date: 1991/04/25 12:08:28;
 * use npopen instead of popen for globbing
 * 
 * revision 1.6
 * date: 1991/04/22 08:59:37;
 * fixed globbing to always use /bin/sh
 * 
 * revision 1.5
 * date: 1991/04/08 15:48:59;
 * only update() in readin() if no input pending
 * 
 * revision 1.4
 * date: 1991/04/04 09:36:10;
 * allow for internal callers of ifile()
 * fixed bug with non-unique buffer names at startup
 * 
 * revision 1.3
 * date: 1990/10/01 12:16:11;
 * make mkdir() stuff conditional on ifdef HAVE_MKDIR
 * 
 * revision 1.2
 * date: 1990/09/25 11:38:17;
 * took out old ifdef BEFORE code
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:16;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"

#if UNIX || defined(MDCHK_MODTIME)
#include	<sys/stat.h>  /* for mkdir() declaration */
#endif

extern int fileispipe;

static	void	readlinesmsg P(( int, int, char *, int ));
static	int	getfile2 P(( char *, int ));
#if DOSFILES
static	void	guess_dosmode P(( BUFFER * ));
#endif
static	int	writereg P(( REGION *, char *, int, BUFFER * ));
#if UNIX
static	int	slowtime P(( long * ));
#endif

/*--------------------------------------------------------------------------*/

#ifdef	MDCHK_MODTIME
static	int	PromptModtime P(( BUFFER *, char *, char *, int));

static int
PromptModtime (bp, fname, question, iswrite)
BUFFER	*bp;
char	*fname;
char	*question;
int	iswrite;
{
	int status = SORTOFTRUE;
	long current;
	char prompt[NLINE];

	if (!b_is_temporary(bp)
	 && b_val(bp, MDCHK_MODTIME)
	 && bp->b_active	/* only buffers that are loaded */
	 && same_fname(fname, bp, FALSE)
	 && get_modtime(bp, &current)) {
		long check_against;
		char *remind, *again;
		if (iswrite) {
			check_against = bp->b_modtime;
			remind = "Reminder: ";
			again = "";
		} else {
			remind = "";
			if (bp->b_modtime_at_warn) {
				check_against = bp->b_modtime_at_warn;
				again = "again ";
			} else {
				check_against = bp->b_modtime;
				again = "";
			}
		}

		if (check_against != current) {
			(void)lsprintf(prompt,
			"%sFile for buffer \"%s\" has changed %son disk.  %s",
				remind, get_bname(bp), again, question);
			if ((status = mlyesno( prompt )) != TRUE)
				mlerase();
			/* avoid reprompts */
			bp->b_modtime_at_warn = current;
		}
	}
	return status;
}

int
get_modtime (bp, the_time)
BUFFER	*bp;
long	*the_time;
{
	struct stat	statbuf;

	*the_time = 0;
	if (!isInternalName(bp->b_fname)) {
		if (stat(bp->b_fname, &statbuf) >= 0) {
			*the_time = statbuf.st_mtime;
			return TRUE;
		}
	}
	return FALSE;
}

void
set_modtime( bp, fn )
BUFFER *bp;
char *fn;
{
	long	current;

	if (same_fname(fn, bp, FALSE) && get_modtime(bp, &current)) {
		bp->b_modtime = current;
		bp->b_modtime_at_warn = 0;
	}
}

int
check_modtime( bp, fn )
BUFFER *bp;
char *fn;
{
	int status = TRUE;

	if (PromptModtime(bp, fn, "Read from disk", FALSE) == TRUE) {
		status = readin(fn, TRUE, bp, TRUE);
	}
	return status;
}

int
inquire_modtime( bp, fn )
BUFFER *bp;
char *fn;
{
	register int status;
	if ((status = PromptModtime(bp, fn, "Continue write", TRUE)) != TRUE
	 && (status != SORTOFTRUE)) {
		mlforce("[Write aborted]");
		return FALSE;
	}
	return TRUE;
}

int
check_visible_modtimes ()
{
	register WINDOW *wp;

	for_each_window(wp)
		(void)check_modtime(wp->w_bufp, wp->w_bufp->b_fname);
	return TRUE;
}
#endif	/* MDCHK_MODTIME */

#if UNIX
static	void	CleanAfterPipe P((int));

#define	CleanToPipe()	if (fileispipe) ttclean(TRUE)

static void
CleanAfterPipe (Wrote)
int	Wrote;
{
	if (fileispipe == TRUE) {
		ttunclean();	/* may clear the screen as a side-effect */
	        TTflush();
		if (Wrote) pressreturn();
	        sgarbf = TRUE;
	}
}
#else
#define	CleanToPipe()		TTkclose()
#define	CleanAfterPipe(f)	TTkopen()
#endif

/*
 * On faster machines, a pipe-writer will tend to keep the pipe full. This
 * function is used by 'slowreadf()' to test if we've not done an update
 * recently even if this is the case.
 */
#if UNIX
static int
slowtime (refp)
long	*refp;
{
	int	status = FALSE;

	if (fileispipe) {
		long	temp = time((long *)0);

		status = (!ffhasdata() || (temp != *refp));
		if (status)
			*refp = temp;
	}
	return status;
}
#else
#define	slowtime(refp)	(fileispipe && !ffhasdata())
#endif

int
no_such_file(fname)
char *	fname;
{
	mlforce("[No such file \"%s\"]", fname);
	return FALSE;
}

#if VMS
static char *
version_of(char *fname)
{
	register char	*s = strchr(fname, ';');
	if (s == 0)
		s = fname + strlen(fname);
	return s;
}

static int
explicit_version(char *version)
{
	if (*version++ == ';') {
		if (isdigit(*version))
			return TRUE;
	}
	return FALSE;
}

static char *
resolve_filename(bp)
BUFFER	*bp;
{
	char	temp[NFILEN];
	ch_fname(bp, fgetname(ffp, temp));
	return bp->b_fname;
}
#endif

/*
 * Returns true if the given filename is the same as that of the referenced
 * buffer.  The 'lengthen' parameter controls whether we assume the filename is
 * already in canonical form, since that may be an expensive operation to do in
 * a loop.
 */
int
same_fname(fname, bp, lengthen)
char	*fname;
BUFFER	*bp;
int	lengthen;
{
	char	temp[NFILEN];

	if (fname == 0
	 || bp->b_fname == 0
	 || isInternalName(fname)
	 || isInternalName(bp->b_fname))
		return FALSE;

	if (lengthen)
		fname = lengthen_path(strcpy(temp, fname));

#if VMS
	/* ignore version numbers in this comparison unless both are given */
	if (is_vms_pathname(fname, FALSE)) {
		char	*bname = bp->b_fname,
			*s = version_of(bname),
			*t = version_of(fname);

		if (!explicit_version(s)
		 || !explicit_version(t))
			if ((s-bname) == (t-fname))
				return !strncmp(fname, bname, s-bname);
	}
#endif

	return !strcmp(fname, bp->b_fname);
}

/*
 * Read a file into the current
 * buffer. This is really easy; all you do it
 * find the name of the file, and call the standard
 * "read a file into the current buffer" code.
 */
/* ARGSUSED */
int
fileread(f, n)
int f,n;
{
        register int    s;
        char bname[NBUFN];
	char fname[NFILEN];

	if ((s = mlreply_file("Replace with file: ", (TBUFF **)0, 
			FILEC_REREAD, fname)) != TRUE)
		return s;

	/* we want no errors or complaints, so mark it unchanged */
	b_clr_changed(curbp);
        s = readin(fname, TRUE, curbp, TRUE);
	curbp->b_bname[0] = EOS;	/* ...so 'unqname()' doesn't find me */
	makename(bname, fname);
	unqname(bname, TRUE);
	set_bname(curbp, bname);
	updatelistbuffers();
	return s;
}

/*
 * Select a file for editing.
 * Look around to see if you can find the
 * file in another buffer; if you can find it
 * just switch to the buffer. If you cannot find
 * the file, create a new buffer, read in the
 * text, and switch to the new buffer.
 * This is ": e"
 */
/* ARGSUSED */
int
filefind(f, n)
int f,n;
{
	register int	s;
	char fname[NFILEN];
	char *actual;
	static	TBUFF	*last;

	if ((s = mlreply_file("Find file: ", &last, FILEC_READ|FILEC_EXPAND,
			fname)) == TRUE) {
		while ((actual = filec_expand()) != 0) {
			if ((s = getfile(actual, TRUE)) != TRUE)
				break;
		}
	}
	return s;
}

/* ARGSUSED */
int
viewfile(f, n)	/* visit a file in VIEW mode */
int f,n;
{
	char fname[NFILEN];	/* file user wishes to find */
	register int s;		/* status return */
	char	*actual;
	static	TBUFF	*last;

	if ((s = mlreply_file("View file: ", &last, FILEC_READ|FILEC_EXPAND,
			fname)) == TRUE) {
		while ((actual = filec_expand()) != 0) {
			if ((s = getfile(actual, FALSE)) != TRUE)
				break;
			/* if we succeed, put it in view mode */
			make_local_b_val(curwp->w_bufp,MDVIEW);
			set_b_val(curwp->w_bufp,MDVIEW,TRUE);
			markWFMODE(curwp->w_bufp);
		}
	}
	return s;
}

/*
 * Insert a file into the current
 * buffer. This is really easy; all you do it
 * find the name of the file, and call the standard
 * "insert a file into the current buffer" code.
 */
/* ARGSUSED */
int
insfile(f, n)
int f,n;
{
        register int    s;
	char fname[NFILEN];
	static	TBUFF	*last;

	if (!calledbefore) {
	        if ((s= mlreply_file("Insert file: ", &last,
				FILEC_READ|FILEC_PROMPT, fname)) != TRUE)
	                return s;
	}
	if (ukb == 0)
	        return ifile(fname, TRUE, (FILE *)0);
	else
	        return kifile(fname);
}

static int
getfile2(fname, lockfl)
char *fname;		/* file name to find */
int lockfl;		/* check the file for locks? */
{
	register BUFFER *bp;
        register int    s;
	char bname[NBUFN+1];	/* buffer name to put file */
	char nfname[NFILEN];	/* canonical form of 'fname' */

	/* user may have renamed buffer to look like filename */
	if ((bp = find_b_name(fname)) == NULL) {

		/* It's not already here by that buffer name.
		 * Try to find it assuming we're given the file name.
		 */
		(void)lengthen_path(strcpy(nfname, fname));
		if (is_internalname(nfname)) {
			mlforce("[Buffer not found]");
			return FALSE;
		}
	        for_each_buffer(bp) {
			/* is it here by that filename? */
	                if (same_fname(nfname, bp, FALSE)) {
				(void)swbuffer(bp);
	                        curwp->w_flag |= WFMODE|WFHARD;
				if (!isShellOrPipe(fname)) {
		                        mlwrite("[Old buffer]");
				} else {
		                        if (mlyesno(
				 "Old command output -- rerun") == TRUE) {
					        return readin(fname, lockfl, 
								curbp, TRUE);
					}
				}					
	                        return TRUE;
	                }
	        }
		/* it's not here */
	        makename(bname, fname);            /* New buffer name.     */
		/* make sure the buffer name doesn't exist */
		while ((bp = find_b_name(bname)) != NULL) {
			if ( !b_is_changed(bp) && is_empty_buf(bp)) {
				/* empty and unmodified -- then it's okay 
					to re-use this buffer */
				bp->b_active = FALSE;
				return readin(fname, lockfl, bp, TRUE) &&
						swbuffer(bp);
			}
			/* old buffer name conflict code */
			unqname(bname,TRUE);
			hst_glue(' ');
			s = mlreply("Will use buffer name: ", bname, sizeof(bname));
	                if (s == ABORT)
	                        return s;
			if (s == FALSE || bname[0] == EOS)
		                makename(bname, fname);
	        }
		/* okay, we've got a unique name -- create it */
		if (bp==NULL && (bp=bfind(bname, 0))==NULL) {
			mlforce("[Cannot create buffer]");
	                return FALSE;
	        }
		/* switch and read it in. */
		ch_fname(bp, nfname);
	}
	return swbuffer(bp);
}

int
getfile(fname, lockfl)
char *fname;		/* file name to find */
int lockfl;		/* check the file for locks? */
{
        register BUFFER *bp;

	/* if there are no path delimiters in the name, then the user
		is likely asking for an existing buffer -- try for that
		first */
        if (!maybe_pathname(fname)
	 && (bp = find_b_name(fname)) != NULL) {
		return swbuffer(bp);
	}

	/* oh well.  canonicalize the name, and try again */
	return getfile2(fname, lockfl);
}

/*
 * Scan a buffer to see if it contains more lines terminated by CR-LF than by
 * LF alone.  If so, set the DOS-mode to true, otherwise false.
 */
#if DOSFILES
static void
guess_dosmode(bp)
BUFFER *bp;
{
	if (b_val(bp, MDDOS)) { /* should we check for dos files? */
		int	doslines = 0,
			unixlines = 0;
		register LINE *lp;

		make_local_b_val(bp, MDDOS);	/* keep it local, if not */
		for_each_line(lp,bp) {
			if (llength(lp) > 0
			 && lgetc(lp, llength(lp)-1) == '\r') {
				llength(lp)--;
				doslines++;
			} else {
				unixlines++;
			}
		}
		set_b_val(bp, MDDOS, doslines > unixlines);
		bp->b_bytecount -= doslines;
	}
}

/*
 * Forces the current buffer to be in DOS-mode, stripping any trailing CR's.
 */
/*ARGSUSED*/
int
set_dosmode(f,n)
int	f,n;
{
	make_local_b_val(curbp, MDDOS);
	set_b_val(curbp, MDDOS, TRUE);
	guess_dosmode(curbp);
	set_b_val(curbp, MDDOS, TRUE);
	markWFMODE(curbp);
	return TRUE;
}
#endif

/*
 *	Read file "fname" into a buffer, blowing away any text
 *	found there.  Returns the final status of the read.
 */

/* ARGSUSED */
int
readin(fname, lockfl, bp, mflg)
char    *fname;		/* name of file to read */
int	lockfl;		/* check for file locks? */
register BUFFER *bp;	/* read into this buffer */
int	mflg;		/* print messages? */
{
        register WINDOW *wp;
	register int    s;
        int    nline;
#if VMALLOC
	extern int doverifys;
	int odv;
#endif

	if (bp == 0)				/* doesn't hurt to check */
		return FALSE;

#if	FILOCK
	if (lockfl && lockchk(fname) == ABORT)
		return ABORT;
#endif
#if	CRYPT
	if ((s = resetkey(bp, fname)) != TRUE)
		return s;
#endif
        if ((s=bclear(bp)) != TRUE)             /* Might be old.        */
                return s;
	b_clr_flags(bp, BFINVS|BFCHG);
	ch_fname(bp,fname);
#if DOSFILES
	make_local_b_val(bp,MDDOS);
	set_b_val(bp, MDDOS, global_b_val(MDDOS) );
#endif
	make_local_b_val(bp,MDNEWLINE);
	set_b_val(bp, MDNEWLINE, TRUE);		/* assume we've got it */

	/* turn off ALL keyboard translation in case we get a dos error */
	TTkclose();

        if ((s = ffropen(fname)) == FIOERR) {	/* Hard file open.      */
		mlerror(fname);
        } else if (s == FIOFNF) {		/* File not found.      */
                if (mflg)
			mlwrite("[New file]");
#if DOSFILES
		set_b_val(bp, MDDOS, FALSE);
#endif
        } else {

        	if (mflg)
			mlforce("[Reading %s ]", fname);
#if VMS
		if (!isInternalName(bp->b_fname))
			fname = resolve_filename(bp);
#endif
		/* read the file in */
        	nline = 0;
#if VMALLOC
		/* we really think this stuff is clean... */
		odv = doverifys;
		doverifys = 0;
#endif
		if_OPT_WORKING(max_working = cur_working = old_working = 0)
#if ! MSDOS && !OPT_MAP_MEMORY
		if (fileispipe || (s = quickreadf(bp, &nline)) == FIOMEM)
#endif
			s = slowreadf(bp, &nline);
#if VMALLOC
		doverifys = odv;
#endif
		if_OPT_WORKING(cur_working = 0)
		if (s == FIOERR) {
			mlerror(fname);
		} else {

			b_clr_changed(bp);
#if FINDERR
			if (fileispipe == TRUE)
				set_febuff(get_bname(bp));
#endif
        		(void)ffclose();	/* Ignore errors.       */
			if (mflg)
				readlinesmsg(nline, s, fname, ffronly(fname));

			/* set read-only mode for read-only files */
			if (isShellOrPipe(fname)
#if RONLYVIEW
			 || ffronly(fname) 
#endif
			) {
				make_local_b_val(bp, MDVIEW);
				set_b_val(bp, MDVIEW, TRUE);
			}
	
			bp->b_active = TRUE;
		}

	}

	/* set C mode for C files */
	make_local_b_val(bp, MDCMOD); /* make it local for all, so that
					subsequent changes to global value
					will _not_ affect this buffer */
	set_b_val(bp, MDCMOD, (global_b_val(MDCMOD) && has_C_suffix(bp)));

	TTkopen();	/* open the keyboard again */

        for_each_window(wp) {
                if (wp->w_bufp == bp) {
                        wp->w_line.l = lFORW(bp->b_line.l);
                        wp->w_dot.l  = lFORW(bp->b_line.l);
                        wp->w_dot.o  = 0;
#ifdef WINMARK
                        wp->w_mark = nullmark;
#endif
                        wp->w_lastdot = nullmark;
                        wp->w_flag |= WFMODE|WFHARD;
                }
        }
	imply_alt(fname, FALSE, lockfl);
	updatelistbuffers();

	return (s != FIOERR);
}

#if ! MSDOS && !OPT_MAP_MEMORY
int
quickreadf(bp, nlinep)
register BUFFER *bp;
int *nlinep;
{
        register UCHAR *textp;
        UCHAR *countp;
	L_NUM nlines;
        int incomplete = FALSE;
	B_COUNT len, nbytes;

	if ((len = ffsize()) < 0)
		return FIOERR;

	/* avoid malloc(0) problems down below; let slowreadf() do the work */
	if (len == 0)
		return FIOMEM;
	if_OPT_WORKING(max_working = len)
#if     MSDOS
	/* cannot allocate more than 64K in dos */
	if (len >= 65535)
		return FIOMEM;
#endif
	/* leave an extra byte at the front, for the length of the first
		line.  after that, lengths go in place of the newline at
		the end of the previous line */
	bp->b_ltext = castalloc(UCHAR, len + 2);
	if (bp->b_ltext == NULL)
		return FIOMEM;

	if ((len = ffread((char *)&bp->b_ltext[1], len)) < 0) {
		FreeAndNull(bp->b_ltext);
		return FIOERR;
	}

#if CRYPT
	if (b_val(bp, MDCRYPT)
	 && bp->b_key[0]) {	/* decrypt the file */
	 	char	temp[NPAT];
		(void)strcpy(temp, bp->b_key);
		ue_crypt((char *)NULL, 0);
		ue_crypt(temp, (int)strlen(temp));
		ue_crypt((char *)&bp->b_ltext[1], len);
	}
#endif

	/* loop through the buffer, replacing all newlines with the
		length of the _following_ line */
	bp->b_ltext_end = bp->b_ltext + len + 1;
	countp = bp->b_ltext;
	textp = countp + 1;
	nbytes = len;
        nlines = 0;

	if (textp[len-1] != '\n') {
		textp[len++] = '\n';
		set_b_val(bp, MDNEWLINE, FALSE);
	}

	while (len--) {
		if (*textp == '\n') {
			if (textp - countp >= 255) {
				UCHAR *np;
				if_OPT_WORKING(max_working = bp->b_ltext_end - countp)
				len = (B_COUNT)(countp - bp->b_ltext);
				incomplete = TRUE;
				/* we'll re-read the rest later */
				if (len)  {
					ffseek(len);
					np = castrealloc(UCHAR, bp->b_ltext, len);
				} else {
					np = NULL;
				}
				if (np == NULL) {
					ffrewind();
					FreeAndNull(bp->b_ltext);
					return FIOMEM;
				}
				bp->b_ltext = np;
				bp->b_ltext_end = np + len + 1;
				nbytes = len;
				break;
			}
			*countp = textp - countp - 1;
			countp = textp;
			nlines++;
		}
		++textp;
	}

	if (nlines == 0) {
		ffrewind();
		FreeAndNull(bp->b_ltext);
		incomplete = TRUE;
	} else {
		/* allocate all of the line structs we'll need */
		bp->b_LINEs = typeallocn(LINE,nlines);
		if (bp->b_LINEs == NULL) {
			FreeAndNull(bp->b_ltext);
			ffrewind();
			return FIOMEM;
		}
		bp->b_LINEs_end = bp->b_LINEs + nlines;
		bp->b_bytecount = nbytes;
		bp->b_linecount = nlines;
		b_set_counted(bp);

		/* loop through the buffer again, creating
			line data structure for each line */
		{
			register LINE *lp;
#if !SMALLER
			L_NUM lineno = 0;
#endif
			lp = bp->b_LINEs;
			textp = bp->b_ltext;
			while (lp != bp->b_LINEs_end) {
#if !SMALLER
				lp->l_number = ++lineno;
#endif
				lp->l_used = *textp;
				lp->l_size = *textp + 1;
				lp->l_text = (char *)textp + 1;
				set_lforw(lp, lp + 1);
				set_lback(lp, lp - 1);
				lsetclear(lp);
				lp->l_nxtundo = null_ptr;
				lp++;
				textp += *textp + 1;
			}
			/*
			if (textp != bp->b_ltext_end - 1)
				mlwrite("BUG: textp not equal to end %d %d",
					textp,bp->b_ltext_end);
			*/
			lp--;  /* point at last line again */

			/* connect the end of the list */
			set_lforw(lp, bp->b_line.l);
			set_lback(bp->b_line.l, lp);

			/* connect the front of the list */
			set_lback(bp->b_LINEs, bp->b_line.l);
			set_lforw(bp->b_line.l, bp->b_LINEs);
		}
	}

	*nlinep = nlines;

	if (incomplete)
		return FIOMEM;
#if DOSFILES
	guess_dosmode(bp);
#endif
	return b_val(bp, MDNEWLINE) ? FIOSUC : FIOFUN;
}

#endif /* ! MSDOS */

int
slowreadf(bp, nlinep)
register BUFFER *bp;
int *nlinep;
{
	int s;
	int len;
#if DOSFILES
	int	doslines = 0,
		unixlines = 0;
#endif
#if UNIX || MSDOS	/* i.e., we can read from a pipe */
	int	flag = 0;
	int	done_update = FALSE;
#endif
#if UNIX
	long	last_updated = time((long *)0);
#endif
	b_set_counted(bp);	/* make 'addline()' do the counting */
        while ((s = ffgetline(&len)) <= FIOSUC) {
#if DOSFILES
		/*
		 * Strip CR's if we are reading in DOS-mode.  Otherwise,
		 * keep any CR's that we read.
		 */
		if (global_b_val(MDDOS)) {
			if (fline[len-1] == '\r') {
				len--;
				doslines++;
			} else {
				unixlines++;
			}
		}
#endif
		if (addline(bp,fline,len) != TRUE) {
                        s = FIOMEM;             /* Keep message on the  */
                        break;                  /* display.             */
                } 
#if UNIX || MSDOS
		else {
                	/* reading from a pipe, and internal? */
			if (slowtime(&last_updated)) {
				register WINDOW *wp;

				flag |= (WFEDIT|WFFORCE);

				if (!done_update || bp->b_nwnd > 1)
					flag |= WFHARD;
			        for_each_window(wp) {
			                if (wp->w_bufp == bp) {
			                        wp->w_line.l=
							lFORW(bp->b_line.l);
			                        wp->w_dot.l =
							lBACK(bp->b_line.l);
			                        wp->w_dot.o = 0;
						wp->w_flag |= flag;
						wp->w_force = -1;
			                }
			        }

				/* track changes in dosfile as lines arrive */
#if DOSFILES
				if (global_b_val(MDDOS))
					set_b_val(bp, MDDOS, 
						doslines > unixlines);
#endif
				curwp->w_flag |= WFMODE|WFKILLS;
				if (!update(TRUE)) {
					s = FIOERR;
					break;
				}
				done_update = TRUE;
				flag = 0;
			} else {
				flag |= WFHARD;
			}
			
		}
#endif
                ++(*nlinep);
		if (s == FIOFUN) {
			set_b_val(bp, MDNEWLINE, FALSE);
			break;
		}
        }
#if DOSFILES
	if (global_b_val(MDDOS))
		set_b_val(bp, MDDOS, doslines > unixlines);
#endif
	return s;
}

/* utility routine for no. of lines read */
static void
readlinesmsg(n,s,f,rdo)
int n;
int s;
char *f;
int rdo;
{
	char fname[NFILEN];
	char *m;
	f = shorten_path(strcpy(fname,f),TRUE);
	switch(s) {
		case FIOFUN:	m = "INCOMPLETE LINE, ";break;
		case FIOERR:	m = "I/O ERROR, ";	break;
		case FIOMEM:	m = "OUT OF MEMORY, ";	break;
		case FIOABRT:	m = "ABORTED, ";	break;
		default:	m = "";			break;
	}
	if (!global_b_val(MDTERSE))
		mlwrite("[%sRead %d line%s from \"%s\"%s]", m,
			n, PLURAL(n), f, rdo ? "  (read-only)":"" );
	else
		mlforce("[%s%d lines]",m,n);
}

/*
 * Take a (null-terminated) file name, and from it
 * fabricate a buffer name. This routine knows
 * about the syntax of file names on the target system.
 * I suppose that this information could be put in
 * a better place than a line of code.
 */

void
makename(bname, fname)
char    bname[];
char    fname[];
{
	register char *fcp;
        register char *bcp;
	register int j;

#if VMS
	if (is_vms_pathname(fname, TRUE)) {
		(void)strcpy(bname, "NoName");
		return;
	}
	if (is_vms_pathname(fname, FALSE)) {
		for (fcp = fname + strlen(fname);
			fcp > fname && !strchr(":]", fcp[-1]);
				fcp--)
				;
		(void)strncpy(bname, fcp, NBUFN);
		if (bcp = strchr(bname, ';'))	/* strip version */
			*bcp = EOS;
		(void)mklower(bname);
		return;
	}
#endif
	fcp = &fname[strlen(fname)];
	/* trim trailing whitespace */
	while (fcp != fname && (isblank(fcp[-1])
#if UNIX || MSDOS /* trim trailing slashes as well */
					 || slashc(fcp[-1])
#endif
							) )
                *(--fcp) = EOS;
	fcp = fname;
	/* trim leading whitespace */
	while (isblank(*fcp))
		fcp++;

#if     UNIX || MSDOS || VMS
	bcp = bname;
	if (isShellOrPipe(fcp)) { 
		/* ...it's a shell command; bname is first word */
		*bcp++ = SHPIPE_LEFT[0];
		do {
			++fcp;
		} while (isspace(*fcp));
		(void)strncpy(bcp, fcp, NBUFN);
		for (j = 1; j < NBUFN; j++) {
			if (isspace(*bcp)) {
				*bcp = EOS;
				break;
			}
			bcp++;
		}
		return;
	}

	(void)strncpy(bcp, pathleaf(fcp), NBUFN);

#if	UNIX
	/* UNIX filenames can have any characters (other than EOS!).  Refuse
	 * (rightly) to deal with leading/trailing blanks, but allow embedded
	 * blanks.  For this special case, ensure that the buffer name has no
	 * blanks, otherwise it is difficult to reference from commands.
	 */
	for (j = 0; j < NBUFN; j++) {
		if (*bcp == EOS)
			break;
		if (isspace(*bcp))
			*bcp = '-';
		bcp++;
	}
#endif

#else	/* !(UNIX||VMS||MSDOS) */

	bcp = fcp + strlen(fcp);
#if     AMIGA || ST520
	while (bcp!=fcp && bcp[-1]!=':' && !slashc(bcp[-1]))
                --bcp;
#endif
#if     CPM
        while (bcp!=fcp && bcp[-1]!=':')
                --bcp;
#endif
	{
		register char *cp2 = bname;

		do {
			if (cp2 == bname+NBUFN)
				break;
			*cp2 = EOS;
			if (*bcp == ';')
				break;
		} while ((*cp2++ = *bcp++) != EOS);
	}
#endif
}

void
unqname(name,ok_to_ask)	/* make sure a buffer name is unique */
char *name;	/* name to check on */
int ok_to_ask;  /* prompts allowed? */
{
	register char *sp;
	register SIZE_T	j;

	/* check to see if it is in the buffer list */
	while (find_b_name(name) != NULL) {

		/* "strnlen()", if there were such a thing */
		for (j = 0; (j < NBUFN) && (name[j] != EOS); j++)
			;
		if (j == 0)
			j = strlen(strcpy(name, "NoName"));

		sp = name+j-1;	/* point to last char of name */

		if (j > 2 && sp[-1] == '-') {
			if (*sp == '9')
				*sp = 'A';
			else
				*sp += 1;
			if (!isdigit(*sp) && !isupper(*sp))
				goto choosename;

		} else if (j < NBUFN-2)  {
			(void)strncpy(sp+1, "-1", NBUFN-j);
		} else {
		choosename:
			if (ok_to_ask) {
				do {
					hst_glue(' ');
					(void)mlreply(
					"Choose a unique buffer name: ",
						 name, NBUFN);
				} while (name[0] == EOS);
			} else { /* can't ask, just overwrite end of name */
				while (j >= NBUFN-1) {
					j--;
					sp--;
				}
				(void)strncpy(sp, "-1", NBUFN-j);
			}
		}
	}
}

/*
 * Ask for a file name, and write the
 * contents of the current buffer to that file.
 */
/* ARGSUSED */
int
filewrite(f, n)
int f,n;
{
        register int    s;
        char            fname[NFILEN];

	if (more_named_cmd()) {
	        if ((s= mlreply_file("Write to file: ", (TBUFF **)0, 
				FILEC_WRITE, fname)) != TRUE)
	                return s;
        } else
		(void) strcpy(fname, curbp->b_fname);

        if ((s=writeout(fname,curbp,TRUE)) == TRUE)
		unchg_buff(curbp, 0);
        return s;
}

/*
 * Save the contents of the current
 * buffer in its associatd file.
 * Error if there is no remembered file
 * name for the buffer.
 */
/* ARGSUSED */
int
filesave(f, n)
int f,n;
{
        register int    s;

        if (curbp->b_fname[0] == EOS) {		/* Must have a name.    */
                mlforce("[No file name]");
                return FALSE;
        }
        if ((s=writeout(curbp->b_fname,curbp,TRUE)) == TRUE)
		unchg_buff(curbp, 0);
        return s;
}

/*
 * This function performs the details of file
 * writing. Uses the file management routines in the
 * "fileio.c" package. The number of lines written is
 * displayed. Sadly, it looks inside a LINE; provide
 * a macro for this. Most of the grief is error
 * checking of some sort.
 */
int
writeout(fn,bp,msgf)
char    *fn;
BUFFER *bp;
int msgf;
{
        REGION region;

	bsizes(bp);	/* make sure we have current count */
	/* starting at the beginning of the buffer */
        region.r_orig.l = lFORW(bp->b_line.l);
        region.r_orig.o = 0;
        region.r_size   = bp->b_bytecount;
        region.r_end    = bp->b_line;
 
	return writereg(&region, fn, msgf, bp);
}

int
writeregion()
{
        REGION region;
	int status;
        char fname[NFILEN];

	if (end_named_cmd()) {
		if (mlyesno("Okay to write [possible] partial range") != TRUE) {
			mlwrite("Range not written");
			return FALSE;
		}
		(void)strcpy(fname, curbp->b_fname);
	} else {
	        if ((status = mlreply_file("Write region to file: ", 
			(TBUFF **)0, FILEC_WRITE|FILEC_PROMPT, fname)) != TRUE)
	                return status;
        }
        if ((status=getregion(&region)) == TRUE)
		status = writereg(&region, fname, TRUE, curbp);
	return status;
}


static int
writereg(rp, fn, msgf, bp)
REGION	*rp;
char    *fn;
int 	msgf;
BUFFER	*bp;
{
        register int    s;
        register LINE   *lp;
        register int    nline;
	register int i;
	char	fname[NFILEN];
	long	nchar;
	char *	ending =
#if DOSFILES
			b_val(bp, MDDOS) ? "\r\n" : "\n"
#else
#if ST520
			"\r\n"
#else	/* UNIX */
			"\n"
#endif
#endif	/* DOSFILES */
		;
	C_NUM	offset = rp->r_orig.o;

	/* this is adequate as long as we cannot write parts of lines */
	int	whole_file = (l_ref(rp->r_orig.l) == lForw(bp->b_line.l))
	        	  && (same_ptr(rp->r_end.l, bp->b_line.l));

	fn = lengthen_path(strcpy(fname, fn));
	if (same_fname(fn, bp, FALSE) && b_val(bp,MDVIEW)) {
		mlforce("[Can't write-back from view mode]");
		return FALSE;
	}

#if	CRYPT
	if ((s = resetkey(curbp, fn)) != TRUE)
		return s;
#endif
        if (is_internalname(fn)) {
        	mlforce("[No filename]");
        	return FALSE;
        }
 
#ifdef MDCHK_MODTIME
	if ( ! inquire_modtime( bp, fn ) )
		return FALSE;
#endif  
        if ((s=ffwopen(fn)) != FIOSUC)       /* Open writes message. */
                return FALSE;

	/* tell us we're writing */
	if (msgf == TRUE)
		mlwrite("[Writing...]");

	CleanToPipe();

        lp = l_ref(rp->r_orig.l);
        nline = 0;                              /* Number of lines     */
        nchar = 0;                              /* Number of chars     */

	/* first (maybe partial) line and succeeding whole lines */
        while ((rp->r_size+offset) >= llength(lp)+1) {
		register C_NUM	len = llength(lp) - offset;
		register char	*text = lp->l_text + offset;

		/* If this is the last line (and no fragment will be written
		 * after the line), allow 'newline' mode to suppress the
		 * trailing newline.
		 */
		if ((rp->r_size -= (len + 1)) <= 0
		 && !b_val(bp,MDNEWLINE))
			ending = "";
                if ((s = ffputline(text, len, ending)) != FIOSUC)
			goto out;

                ++nline;
		nchar += len + 1;
		offset = 0;
                lp = lforw(lp);
        }

	/* last line (fragment) */
	if (rp->r_size > 0) {
		for (i = 0; i < rp->r_size; i++)
		        if ((s = ffputc(lgetc(lp,i))) != FIOSUC)
		                goto out;
		nchar += rp->r_size;
		++nline;	/* it _looks_ like a line */
	}

 out:
        if (s == FIOSUC) {                      /* No write error.      */
#if VMS
		if (same_fname(fn, bp, FALSE))
			fn = resolve_filename(bp);
#endif
                s = ffclose();
                if (s == FIOSUC && msgf) {      /* No close error.      */
			if (!global_b_val(MDTERSE)) {
				char *action;
				if ((action = is_appendname(fn)) != 0) {
					fn = action;
					action = "Appended";
				} else {
					action = "Wrote";
				}
				mlforce("[%s %d line%s %ld char%s to \"%s\"]", 
					action, nline, PLURAL(nline),
					nchar, PLURAL(nchar), fn);
			} else {
				mlforce("[%d lines]", nline);
			}
                }
        } else {                                /* Ignore close error   */
                (void)ffclose();                /* if a write error.    */
	}
	if (whole_file)				/* patch: do I need this? */
		bp->b_linecount = nline;

	CleanAfterPipe(TRUE);

        if (s != FIOSUC)                        /* Some sort of error.  */
                return FALSE;

#ifdef MDCHK_MODTIME
	set_modtime(bp, fn);
#endif
	imply_alt(fn, whole_file, FALSE);
        return TRUE;
}

/*
 * This function writes the kill register to a file
 * Uses the file management routines in the
 * "fileio.c" package. The number of lines written is
 * displayed.
 */
int
kwrite(fn,msgf)
char    *fn;
int	msgf;
{
	register KILL *kp;		/* pointer into kill register */
	register int	nline;
	register int	s;
	register int	c;
	register int	i;
	register char	*sp;	/* pointer into string to insert */

	/* make sure there is something to put */
	if (kbs[ukb].kbufh == NULL) {
		if (msgf) mlforce("Nothing to write");
		return FALSE;		/* not an error, just nothing */
	}

#if	CRYPT
	if ((s = resetkey(curbp, fn)) != TRUE)
		return s;
#endif
	/* turn off ALL keyboard translation in case we get a dos error */
	TTkclose();

	if ((s=ffwopen(fn)) != FIOSUC) {	/* Open writes message. */
		TTkopen();
		return FALSE;
	}
	/* tell us we're writing */
	if (msgf == TRUE)
		mlwrite("[Writing...]");
	nline = 0;				/* Number of lines.	*/

	kp = kbs[ukb].kbufh;
	while (kp != NULL) {
		if (kp->d_next == NULL)
			i = kbs[ukb].kused;
		else
			i = KBLOCK;
		sp = (char *)kp->d_chunk;
		while (i--) {
			if ((c = *sp++) == '\n')
				nline++;
			if ((s = ffputc(c)) != FIOSUC)
				break;
		}
		kp = kp->d_next;
	}
	if (s == FIOSUC) {			/* No write error.	*/
		s = ffclose();
		if (s == FIOSUC && msgf) {	/* No close error.	*/
			if (!global_b_val(MDTERSE))
				mlwrite("[Wrote %d line%s to %s ]",
					nline, PLURAL(nline), fn);
			else
				mlforce("[%d lines]", nline);
		}
	} else	{				/* Ignore close error	*/
		(void)ffclose();		/* if a write error.	*/
	}
	TTkopen();
	if (s != FIOSUC)			/* Some sort of error.	*/
		return FALSE;
	return TRUE;
}


/*
 * The command allows the user
 * to modify the file name associated with
 * the current buffer. It is like the "f" command
 * in UNIX "ed". The operation is simple; just zap
 * the name in the BUFFER structure, and mark the windows
 * as needing an update. You can type a blank line at the
 * prompt if you wish.
 */
/* ARGSUSED */
int
filename(f, n)
int f,n;
{
        register int    s;
        char            fname[NFILEN];

	if (end_named_cmd()) {
		return showcpos(FALSE,1);
	}

        if ((s = mlreply_file("Name: ", (TBUFF **)0, FILEC_UNKNOWN, fname)) 
						== ABORT)
                return s;
        if (s == FALSE)
                ch_fname(curbp, "");
        else
                ch_fname(curbp, fname);
	make_global_b_val(curbp,MDVIEW); /* no longer read only mode */
	markWFMODE(curbp);
	updatelistbuffers();
        return TRUE;
}

/*
 * Insert file "fname" into the current
 * buffer, Called by insert file command. Return the final
 * status of the read.
 */
int
ifile(fname,belowthisline,haveffp)
char    *fname;
int	belowthisline;
FILE	*haveffp;
{
	fast_ptr LINEPTR prevp;
	fast_ptr LINEPTR newlp;
	fast_ptr LINEPTR nextp;
        register BUFFER *bp;
        register int    s;
        int    nbytes;
        register int    nline;

        bp = curbp;                             /* Cheap.               */
	b_clr_flags(bp, BFINVS);		/* we are not temporary*/
	if (!haveffp) {
	        if ((s=ffropen(fname)) == FIOERR) /* Hard file open.      */
	                goto out;
	        if (s == FIOFNF)		/* File not found.      */
			return no_such_file(fname);
#if	CRYPT
		if ((s = resetkey(curbp, fname)) != TRUE)
			return s;
#endif
	        mlwrite("[Inserting...]");
		CleanToPipe();

	} else { /* we already have the file pointer */
		ffp = haveffp;
	}
	prevp = curwp->w_dot.l;
	curwp->w_dot.o = 0;
	MK = DOT;

	nline = 0;
	nextp = null_ptr;
	while ((s=ffgetline(&nbytes)) <= FIOSUC) {
#if DOSFILES
		if (b_val(curbp,MDDOS)
		 && (nbytes > 0)
		 && fline[nbytes-1] == '\r')
			nbytes--;
#endif
		if (!belowthisline) {
			nextp = prevp;
			prevp = lBACK(prevp);
		}

		if (add_line_at(curbp, prevp, fline, nbytes) != TRUE) {
			s = FIOMEM;		/* Keep message on the	*/
			break;			/* display.		*/
		}
		newlp = lFORW(prevp);
		tag_for_undo(newlp);
		prevp = belowthisline ? newlp : nextp;
		++nline;
		if (s < FIOSUC)
			break;
	}
	if (!haveffp) {
		CleanAfterPipe(FALSE);
		(void)ffclose();		/* Ignore errors.	*/
		readlinesmsg(nline,s,fname,FALSE);
	}
out:
	/* advance to the next line and mark the window for changes */
	curwp->w_dot.l = lFORW(curwp->w_dot.l);

	/* copy window parameters back to the buffer structure */
	copy_traits(&(curbp->b_wtraits), &(curwp->w_traits));

	imply_alt(fname, FALSE, FALSE);
	chg_buff (curbp, WFHARD);

	return (s != FIOERR);
}

/*
 * Insert file "fname" into the kill register
 * Called by insert file command. Return the final
 * status of the read.
 */
int
kifile(fname)
char    *fname;
{
        register int    i;
        register int    s;
        register int    nline;
        int    nbytes;

	ksetup();
        if ((s=ffropen(fname)) == FIOERR)       /* Hard file open.      */
                goto out;
        if (s == FIOFNF)			/* File not found.      */
		return no_such_file(fname);

        nline = 0;
#if	CRYPT
	if ((s = resetkey(curbp, fname)) == TRUE)
#endif
	{
        	mlwrite("[Reading...]");
		CleanToPipe();
		while ((s=ffgetline(&nbytes)) <= FIOSUC) {
			for (i=0; i<nbytes; ++i)
				if (!kinsert(fline[i]))
					return FIOMEM;
			if ((s == FIOSUC) && !kinsert('\n'))
				return FIOMEM;
			++nline;
			if (s < FIOSUC)
				break;
		}
		CleanAfterPipe(FALSE);
	}
	kdone();
        (void)ffclose();                        /* Ignore errors.       */
	readlinesmsg(nline,s,fname,FALSE);

out:
	return (s != FIOERR);
}

#if UNIX

/* called on hangups, interrupts, and quits */
/* This code is definitely not production quality, or probably very
	robust, or probably very secure.  I whipped it up to save
	myself while debugging...		pgf */
/* on the other hand, it has worked for well over two years now :-) */
SIGT
imdying(ACTUAL_SIG_ARGS)
ACTUAL_SIG_DECL
{
#if HAVE_MKDIR
	static char dirnam[NSTRING] = "/tmp/vileDXXXXXX";
#else
	static char dirnam[NSTRING] = "/tmp";
	char temp[NFILEN];
#endif
	char filnam[NFILEN];
	char cmd[80];
	BUFFER *bp;
	char *np;
	int wrote = 0;
	int created = FALSE;
	extern char *mktemp P(( char * ));

#if APOLLO
	extern	char	*getlogin();
	static	int	i_am_dead;
#endif	/* APOLLO */

#if OPT_WORKING && defined(SIGALRM)
	(void)signal(SIGALRM, SIG_IGN);
#endif

#if APOLLO
	if (i_am_dead++)	/* prevent recursive faults */
		_exit(signo);
	(void)lsprintf(cmd,
		"(echo signal %d killed vile;/com/tb %d)| /bin/mail %s",
		signo, getpid(), getlogin());
	(void)system(cmd);
#endif	/* APOLLO */

	for_each_buffer(bp) {
		if (!b_is_invisible(bp) && 
			 bp->b_active == TRUE && 
	                 b_is_changed(bp)) {
#if HAVE_MKDIR
			if (!created) {
				(void)mktemp(dirnam);
				if(mkdir(dirnam,0700) != 0) {
					vttidy(FALSE);
					ExitProgram(BAD(1));
				}
				created = TRUE;
			}
			(void)pathcat(filnam, dirnam, get_bname(bp));
#else
			(void)pathcat(filnam, dirnam,
				strcat(strcpy(temp, "V"), get_bname(bp)));
#endif
			set_b_val(bp,MDVIEW,FALSE);
			if (writeout(filnam,bp,FALSE) != TRUE) {
				vttidy(FALSE);
				ExitProgram(BAD(1));
			}
			wrote++;
		}
	}
	if (wrote) {
		if ((np = getenv("LOGNAME")) || (np = getenv("USER"))) {
			(void)lsprintf(cmd,
#if HAVE_MKDIR
    "(echo Subject: vile died; echo Files saved: ; ls -a %s ) | /bin/mail %s",
#else
    "(echo Subject: vile died; echo Files saved: ; ls %s/V* ) | /bin/mail %s",
#endif
				dirnam, np);
			(void)system(cmd);
		}
	}
	vttidy(FALSE);
	if (signo > 2)
		abort();
	else
		ExitProgram(wrote ? BAD(wrote) : GOOD);

	/* NOTREACHED */
	SIGRET;
}
#endif

void
markWFMODE(bp)
BUFFER *bp;
{
	register WINDOW *wp;	/* scan for windows that need updating */
        for_each_window(wp) {
                if (wp->w_bufp == bp)
                        wp->w_flag |= WFMODE;
        }
}

#if	CRYPT
int
resetkey(bp, fname)	/* reset the encryption key if needed */
BUFFER	*bp;
char	*fname;
{
	register int s;	/* return status */

	/* turn off the encryption flag */
	cryptflag = FALSE;

	/* if we are in crypt mode */
	if (b_val(bp, MDCRYPT)) {
		char	temp[NFILEN];

		/* don't automatically inherit key from other buffers */
		if (bp->b_key[0] != EOS
		 && strcmp(lengthen_path(strcpy(temp, fname)), bp->b_fname)) {
			char	prompt[80];
			(void)lsprintf(prompt,
				"Use crypt-key from %s", get_bname(bp));
			s = mlyesno(prompt);
			if (s != TRUE)
				return (s == FALSE);
		}

		/* make a key if we don't have one */
		if (bp->b_key[0] == EOS) {
			s = ue_makekey(bp->b_key, sizeof(bp->b_key));
			if (s != TRUE)
				return (s == FALSE);
		}

		/* let others know... */
		cryptflag = TRUE;

		/* and set up the key to be used! */
		/* de-encrypt it */
		ue_crypt((char *)NULL, 0);
		ue_crypt(bp->b_key, (int)strlen(bp->b_key));

		/* re-encrypt it...seeding it to start */
		ue_crypt((char *)NULL, 0);
		ue_crypt(bp->b_key, (int)strlen(bp->b_key));
	}

	return TRUE;
}
#endif

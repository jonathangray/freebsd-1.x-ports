/*
 *	filec.c
 *
 *	Filename prompting and completion routines
 *
 * $Log: filec.c,v $
 * Revision 1.1  1994/02/01 03:29:22  jkh
 * Initial revision
 *
 * Revision 1.24  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.23  1993/09/06  16:36:47  pgf
 * changed glob() to doglob() to avoid symbol conflicts
 *
 * Revision 1.22  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.21  1993/07/09  19:10:27  pgf
 * changed a couple of routine names -- it looked like watcom might
 * have been doing caseless symbol compares, making trailing_slash() and
 * trailing_SLASH() match, and also force_slash/force_SLASH.
 *
 * Revision 1.20  1993/07/07  10:15:04  pgf
 * fix coredump on filecompletion, by making sure BFSIZES bit is cleared
 * when building MyBuff
 *
 * Revision 1.19  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.18  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.17  1993/06/02  14:58:17  pgf
 * folded some long lines
 *
 * Revision 1.16  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.15  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.14  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.13  1993/05/06  11:59:58  pgf
 * added ifdefs for USE_D_NAMLEN, for systems that don't have or don't
 * need it (d_name[] is null-terminated on most systems)
 *
 * Revision 1.12  1993/04/28  17:15:56  pgf
 * got rid of LOOKTAGS mode and ifdefs
 *
 * Revision 1.11  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.10  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.9  1993/04/02  11:00:40  pgf
 * ls-style directory reading moved to path.c.  thanks -- much better, tom.
 *
 * Revision 1.8  1993/04/01  15:50:34  pgf
 * for sysV machines, without POSIX or BSD dirent, we now have code
 * that enumerates directories using /bin/ls.
 *
 * Revision 1.7  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.6  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.5  1993/03/19  12:33:32  pgf
 * suppress completion chars if IsInternalName
 *
 * Revision 1.4  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.3  1993/03/16  16:04:01  pgf
 * fix 'parentheses suggested' warnings
 *
 * Revision 1.2  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.1  1993/03/05  18:45:59  pgf
 * fixes for null pointers and filenames
 *
 * Revision 1.0  1993/03/04  15:06:38  pgf
 * Initial revision
 *
 */

#include "estruct.h"
#include "edef.h"
#include "glob.h"

#define	SLASH (EOS+1) /* less than everything but EOS */

#if VMS
#define	KBD_OPTIONS	KBD_NORMAL|KBD_UPPERC
#endif

#if MSDOS
#define	KBD_OPTIONS	KBD_NORMAL|KBD_LOWERC
#endif

#ifndef	KBD_OPTIONS
#define	KBD_OPTIONS	KBD_NORMAL
#endif

static	char	**MyGlob;	/* expanded list */
static	int	in_glob;	/* index into MyGlob[] */
static	int	only_dir;	/* can only match real directories */

static void
free_expansion P(( void ))
{
	MyGlob = glob_free(MyGlob);
	in_glob = -1;
}

#if COMPLETE_DIRS || COMPLETE_FILES

#include "dirstuff.h"

static	int	trailing_slash P((char *));
static	int	force_slash P((char *));
static	int	trailing__SLASH P((char *));
static	int	force__SLASH P((char *));
static	void	conv_path P((char *, char *, int));
static	int	pathcmp P((LINE *, char *));
static	LINE *	makeString P((BUFFER *, LINE *, char *));

/*--------------------------------------------------------------------------*/

/*
 * Test if the path has a trailing slash-delimiter (i.e., can be syntactically
 * distinguished from non-directory paths).
 */
static int
trailing_slash(path)
char *	path;
{
	register int	len = strlen(path);
	if (len > 1) {
#if VMS
		if (is_vms_pathname(path, TRUE))
			return TRUE;
#endif
		return (slashc(path[len-1]));
	}
	return FALSE;
}

/*
 * Force a trailing slash on the end of the path, returns the length of the
 * resulting path.
 */
static int
force_slash(path)
char *	path;
{
	register int	len = strlen(path);

#if VMS
	if (!is_vms_pathname(path, -TRUE))	/* must be unix-style name */
#endif
	if (!trailing_slash(path)) {
		path[len++] = slash;
		path[len] = EOS;
	}
	return len;
}

/*
 * Test if the path has a trailing SLASH-delimiter
 */
static int
trailing__SLASH(path)
char *	path;
{
	register int	len = strlen(path);
	return (len > 1
	  &&	path[len-1] == SLASH);
}

/*
 * Force a trailing SLASH on the end of the path, returns the length of the
 * resulting path.
 */
static int
force__SLASH(path)
char *	path;
{
	register int	len = strlen(path);

	if (!trailing__SLASH(path)) {
		path[len++] = SLASH;
		path[len] = EOS;
	}
	return len;
}

/*
 * Because the slash-delimiter is not necessarily lexically before any of the
 * other characters in a path, provide a conversion that makes it so.  Then,
 * using 'strcmp()', we will get lexically-sorted paths.
 */
static void
conv_path(dst, src, len)
char *	dst;
char *	src;
int	len;
{
	register int	c;

	while ((len-- > 0) && (c = *src++) != EOS) {
#if VMS
		if (strchr(":[]!", c))
			c = SLASH;
#endif
		if (slashc(c))
			c = SLASH;
		*dst++ = c;
	}
	*dst = EOS;
}

/*
 * Compare two paths lexically.  The text-string is normally not a full-path,
 * so we must find an appropriate place along the lp-string to start the
 * comparison.
 */
static int
pathcmp(lp, text)
LINE *	lp;
char *	text;
{
	char	refbfr[NFILEN], *ref = refbfr,
		tstbfr[NFILEN], *tst = tstbfr;
	int	reflen = llength(lp),	/* length, less null */
		tstlen = strlen(text);
	register int	j, k;

	if (reflen <= 0)	/* cannot compare */
		return 1;

	conv_path(ref, lp->l_text, reflen);
	conv_path(tst, text,       tstlen);

#if MSDOS
	/*
	 * Check to see if the drives are the same.  If not, there is no
	 * point in further comparison.
	 */
	if ((ref = is_msdos_drive(refbfr)) != 0) {
		if ((tst = is_msdos_drive(tstbfr)) != 0) {
			if (*refbfr != *tstbfr)
				return (*refbfr - *tstbfr);
		} else
			tst = tstbfr;
	} else {
		ref = refbfr;
		if ((tst = is_msdos_drive(tstbfr)) == 0)
			tst = tstbfr;
	}
#endif

	/* If we stored a trailing slash on the ref-value, then it was known to
	 * be a directory.  Append a slash to the tst-value in that case to
	 * force a match if it is otherwise the same.
	 */
	if (trailing__SLASH(ref))
		(void)force__SLASH(tst);
	else if (trailing__SLASH(tst))
		(void)force__SLASH(ref);

	/* count the slashes embedded in text-string */
	for (j = k = 0; tst[j] != EOS; j++)
		if (tst[j] == SLASH)
			k++;

	/* skip back so we have the same number of slashes in lp-string */
	j = strlen(ref);
	if (k > 0) {
		for (; j >= 0; j--)
			if (ref[j] == SLASH)
				if (--k <= 0)
					break;
	}

	if ((k == 0) && (j > 0)) {
		/* skip back to include the leading leaf */
		if (tst[0] != SLASH) {
			j--;
			while ((j >= 0) && (ref[j] != SLASH))
				j--;
			if (ref[j] == SLASH)
				j++;	/* back to the beginning of leaf */
		}
	} else
		j = 0;	/* cannot get there */

#if APOLLO
	if (j == 1)		/* we have leading "//" on apollo */
		j = 0;
#endif
	return strcmp(ref+j, tst);
}

/*
 * Insert a pathname at the given line-pointer.
 * Allocate 2 extra bytes for EOS and (possible) trailing slash.
 */
static LINE *
makeString(bp, lp, text)
BUFFER *bp;
LINE *	lp;
char *	text;
{
	register LINE	*np;
	register int	len = strlen(text);

	if ((np = l_ref(lalloc(len+2,bp))) == NULL) {
		lp = 0;
	} else {
		(void)strcpy(np->l_text, text);
		llength(np) -= 2;	/* hide the null */

		set_lforw(lback(lp), np);
		set_lback(np, lback(lp));
		set_lback(lp, np);
		set_lforw(np, lp);
		lp = np;
	}
	return lp;
}

/*
 * Create a buffer to store null-terminated strings.
 *
 * The file (or directory) completion buffer is initialized at the beginning of
 * each command.  Wildcard expansion causes entries to be read for a given path
 * on demand.  Resetting the buffer in this fashion is a tradeoff between
 * efficiency (allows reuse of a pattern in NAMEC/TESTC operations) and speed
 * (directory scanning is slow).
 *
 * The tags buffer is initialized only once for a given tags-file.
 */
BUFFER *
bs_init(name, first)
char *	name;
int	first;
{
	register BUFFER *bp;

	if ((bp = bfind(name, BFINVS)) != 0) {
		b_clr_flags(bp, BFSCRTCH);	/* make it nonvolatile */
		if (first == -TRUE) {
			(void)bclear(bp);
			bp->b_active = TRUE;
		}
	}
	return bp;
}

/*
 * Look for or insert a pathname string into the given buffer.  Start looking
 * at the given line if non-null.
 */
int
bs_find(text, len,  bp, iflag, lpp)
char *	text;	/* pathname to find */
int	len;	/* ...its length */
BUFFER *bp;	/* buffer to search */
int	iflag;	/* true to insert if not found, -true if it is directory */
LINEPTR *lpp;	/* in/out line pointer, for iteration */
{
	register LINE	*lp;
	int	doit	= FALSE;
	char	fname[NFILEN];

	/*
	 * If we are only looking up a name (for "looktags"), keep the name as
	 * we are given it.  If told that we might insert the name, convert it
	 * to absolute form.  In the special case of inserting a directory
	 * name, append a slash on the end so that we can see this in the name
	 * completion.
	 */
	strncpy(fname, text, len)[len] = EOS;
#if VMS
	/* name should already be in canonical form */
#else
	if (iflag) {
		/* always store full paths */
		(void)lengthen_path(fname);
		if (iflag == -TRUE)
			(void)force_slash(fname);
	}
#endif

	if (lpp == NULL || (lp = l_ref(*lpp)) == NULL)
		lp = l_ref(bp->b_line.l);
	lp = lforw(lp);

	for (;;) {
		register int r = pathcmp(lp, fname);

		if (r == 0) {
			if (iflag == -TRUE
			 &&  trailing_slash(fname)
			 && !trailing_slash(lp->l_text)) {
				/* reinsert so it is sorted properly! */
				lremove(bp, l_ptr(lp));
				return bs_find(text, len,  bp, iflag, lpp);
			}
			break;
		} else if (iflag && (r > 0)) {
			doit = TRUE;
			break;
		}

		lp = lforw(lp);
		if (lp == l_ref(bp->b_line.l)) {
		 	if (!iflag)
				return FALSE;
			doit = TRUE;
			break;
		}
	}

	if (doit) {
		lp = makeString(bp, lp, fname);
		b_clr_counted(bp);
	}

	if (lpp)
		*lpp = l_ptr(lp);
	return TRUE;
}
#endif /* COMPLETE_DIRS || COMPLETE_FILES */

#if COMPLETE_DIRS || COMPLETE_FILES

static	int	already_scanned P((char *));
static	void	fillMyBuff P((char *));
static	void	makeMyList P((void));
#if NO_LEAKS
static	void	freeMyList P((void));
#endif
static	int	path_completion P(( int, char *, int * ));

static	BUFFER	*MyBuff;	/* the buffer containing pathnames */
static	char	*MyName;	/* name of buffer for name-completion */
static	char	**MyList;	/* list, for name-completion code */
static	ALLOC_T MySize;		/* length of list, for (re)allocation */

/*
 * Tests if the given path has been scanned during this prompt/reply operation
 *
 * If there is anything else in the list that we can do completion with, return
 * true.  This allows the case in which we scan a directory (for directory
 * completion and then look at the subdirectories.  Note that it may result in
 * re-scanning a directory that has no subdirectories, but this happens only
 * during directory completion, which is slow anyway.
 */
static int
already_scanned(path)
char *	path;
{
	register LINE	*lp;
	register int	len;
	char	fname[NFILEN];

	len = force_slash(strcpy(fname, path));

	for_each_line(lp,MyBuff)
		if (!strcmp(fname, lp->l_text)) {
			LINE	*np = lforw(lp);

			if (llength(np) > 0
			 && !strncmp(path, np->l_text, strlen(path)))
				return TRUE;
		}

	/* force the name in with a trailing slash */
	(void)bs_find(fname, len, MyBuff, -TRUE, (LINEPTR*)0);
	return FALSE;
}

/*
 * If the given path is not in the completion-buffer, expand it, and add the
 * expanded paths to the buffer.  Because the user may be trying to get an
 * intermediate directory-name, we must 'stat()' each name, so that we can
 * provide the trailing slash in the completion.  This is slow.
 */
static void
fillMyBuff(name)
char *	name;
{
	register char	*s;

	DIR	*dp;
	DIRENT	*de;

	char	path[NFILEN];

	int	iflag;

	(void)strcpy(path, name);
	if (!is_directory(path)) {
		*pathleaf(path) = EOS;
		if (!is_directory(path))
			return;
	}

	if (already_scanned(path))
		return;

	if ((dp = opendir(path)) != 0) {
		s = path;
#if VMS
		if (!is_vms_pathname(path, -TRUE))
#endif
		s += force_slash(path);

		while ((de = readdir(dp)) != 0) {
#if UNIX || VMS
# if USE_D_NAMLEN
			strncpy(s, de->d_name, (int)(de->d_namlen))[de->d_namlen] = EOS;
# else
			(void)strcpy(s, de->d_name);
# endif
#else
# if MSDOS
			(void)mklower(strcpy(s, de->d_name));
# else
			huh??
# endif
#endif
#if UNIX || MSDOS
			if (!strcmp(s, ".")
			 || !strcmp(s, ".."))
			 	continue;
#endif
#if VMS
			/* convert ".dir" files to "]" form */
			if ((s = strchr(path, ']')) != 0
			 && (s[1] != EOS)) {
				register char *t;
				if ((t = strchr(s, '.')) != 0
				 && !strcmp(t, ".DIR;1")) {
					*s = '.';
					*t++ = ']';
					*t = EOS;
				}
			}
			s = path;
#endif
			if (only_dir) {
				if (!is_directory(path))
					continue;
				iflag = -TRUE;
			} else {
#if COMPLETE_DIRS
				iflag = (global_g_val(GMDDIRC) && 
						is_directory(path))
					? -TRUE
					: TRUE;
#else
				iflag = TRUE;
#endif
			}
			(void)bs_find(path, (int)strlen(path), MyBuff, iflag,
					(LINEPTR*)0);
		}
		(void)closedir(dp);
	}
}

/*
 * Make the list of names needed for name-completion
 */
static void
makeMyList()
{
	register int	need, n;
	register LINE *	lp;

	bsizes(MyBuff);
	need = MyBuff->b_linecount + 2;
	if (MySize < need) {
		MySize = need * 2;
		if (MyList == 0)
			MyList = typeallocn(char *, MySize);
		else
			MyList = typereallocn(char *, MyList, MySize);
	}

	n = 0;
	for_each_line(lp,MyBuff)
		if (only_dir || !trailing_slash(lp->l_text))
			MyList[n++] = lp->l_text;
	MyList[n] = 0;
}

#if NO_LEAKS
static void
freeMyList()
{
	FreeAndNull(MyList);
	MySize = 0;
}
#else
#define	freeMyList()
#endif

/*
 * Perform the name-completion/display.  Note that we must convert a copy of
 * the pathname to absolute form so that we can match against the strings that
 * are stored in the completion table.  However, the characters that might be
 * added are always applicable to the original buffer.
 *
 * We only do name-completion if asked; if we did it when the user typed a
 * return it would be too slow.
 */
static int
path_completion(c, buf, pos)
int	c;
char	*buf;
int	*pos;
{
	int	code	= FALSE,
		action	= (c == NAMEC || c == TESTC),
		ignore	= (*buf != EOS && isInternalName(buf));

#if VMS
	if (ignore && action) {		/* resolve scratch-name conflict */
		if (is_vms_pathname(buf, -TRUE))
			ignore = FALSE;
	}
#endif
	if (ignore) {
		if (action) {
			kbd_putc(c);	/* completion-chars have no meaning */
			TTflush();
			buf[*pos] = c;
			*pos += 1;
			buf[*pos] = EOS;
		}
	} else if (action) {
		char	*s;
		char	path[NFILEN];
		int	oldlen,
			newlen;

		/* initialize only on demand */
		if (MyBuff == 0) {
			if ((MyName == 0)
			 || (MyBuff = bs_init(MyName, -TRUE)) == 0)
			 	return FALSE;
		}

		/*
		 * Copy 'buf' into 'path', making it canonical-form.
		 */
#if UNIX || MSDOS
		/* trim trailing "." if it is a "/." */
		if ((s = last_slash(buf)) != 0
		 && !strcmp(s+1, "."))
			kbd_kill_response(buf, pos, '\b');

		(void)lengthen_path(strcpy(path, buf));
#endif

#if VMS
		if (*strcpy(path, buf) == EOS) {
			(void)strcpy(path, current_directory(FALSE));
		} else {
			char	frac[NFILEN];
			*frac = EOS;
			if (is_vms_pathname(path, -TRUE)) {
				s = vms_pathleaf(path);
				(void)strcpy(frac, s);
				*s = EOS;
			}
			if (*path == EOS)
				(void)strcpy(path, current_directory(FALSE));
			else
				(void)lengthen_path(path);
			(void)strcat(path, frac);
		}
#endif

		if (!(s = is_appendname(buf)))
			s = buf;
		if ((*s == EOS) || trailing_slash(s))
			(void)force_slash(path);

		if ((s = is_appendname(path)) != NULL) {
			register char *d;
			for (d = path; (*d++ = *s++) != EOS; )
				;
		}

		newlen =
		oldlen = strlen(path);

		fillMyBuff(path);
		makeMyList();

		/* patch: should also force-dot to the matched line, as in history.c */
		/* patch: how can I force buffer-update to show? */

		code = kbd_complete(c, path, &newlen, (char *)&MyList[0], sizeof(MyList[0]));
		(void)strcat(buf, path+oldlen);
		*pos = strlen(buf);

		/* avoid accidentally picking up directory names for files */
		if ((code == TRUE)
		 && !only_dir
		 && !trailing_slash(path)
		 && is_directory(path)) {
			kbd_putc(slash);
			TTflush();
			buf[*pos] = slash;
			*pos += 1;
			buf[*pos] = EOS;
			code = FALSE;
		}
	}
	return code;
}
#else	/* no filename-completion */
#define	freeMyList()
#endif	/* filename-completion */

/******************************************************************************/
int
mlreply_file(prompt, buf, flag, result)
char *	prompt;
TBUFF **buf;
int	flag;		/* +1 to read, -1 to write, 0 don't care */
char *	result;
{
	register int	s;
	static	TBUFF	*last;
	char	Reply[NFILEN];
	int	(*complete) P(( int, char *, int *)) = no_completion;
	int	had_fname = (curbp != 0
			  && curbp->b_fname != 0
			  && curbp->b_fname[0] != EOS);
	int	do_prompt = (clexec || isnamedcmd || (flag & FILEC_PROMPT));
	int	ok_expand = (flag & FILEC_EXPAND);

	flag &= ~ (FILEC_PROMPT | FILEC_EXPAND);

#if COMPLETE_FILES
	if (do_prompt && !clexec) {
		complete = path_completion;
		MyBuff = 0;
		MyName = ScratchName(FileCompletion);
	}
#endif

	/* use the current filename if none given */
	if (buf == 0) {
		(void)tb_scopy(
			buf = &last,
			had_fname && is_pathname(curbp->b_fname)
				? shorten_path(strcpy(Reply, curbp->b_fname),
				FALSE)
				: "");
	}

	if (do_prompt) {
		char	*t1 = tb_values(*buf),
			*t2 = is_appendname(t1);

		if (t1 != 0)
			(void)strcpy(Reply, (t2 != 0) ? t2 : t1);
		else
			*Reply = EOS;

	        s = kbd_string(prompt, Reply, NFILEN,
			'\n', KBD_OPTIONS|KBD_MAYBEC, complete);
		freeMyList();

		if (s == ABORT)
			return s;
		if (s != TRUE) {
			if ((flag == FILEC_REREAD)
			 && had_fname
			 && ((s = mlyesno("Reread current buffer")) == TRUE))
				(void)strcpy(Reply, curbp->b_fname);
			else
	                	return s;
		}
        } else if (!screen_to_bname(Reply)) {
		return FALSE;
        }
	if (flag >= FILEC_UNKNOWN && is_appendname(Reply) != 0) {
		mlforce("[file is not a legal input]");
		return FALSE;
	}

	free_expansion();
	if (ok_expand) {
		if ((MyGlob = glob_string(Reply)) == 0
		 || glob_length(MyGlob) == 0) {
			mlforce("[No files found] %s", Reply);
			return FALSE;
		}
	} else if (doglob(Reply) != TRUE) {
		return FALSE;
	}

	(void)strcpy (result, Reply);
	if (flag <= FILEC_WRITE) {	/* we want to write a file */
		if (!isInternalName(Reply)
		 && !same_fname(Reply, curbp, TRUE)
		 && flook(Reply, FL_HERE)) {
			if (mlyesno("File exists, okay to overwrite") != TRUE) {
				mlwrite("File not written");
				return FALSE;
			}
		}
	}

	(void)tb_scopy(buf, Reply);
	return TRUE;
}

/******************************************************************************/
int
mlreply_dir(prompt, buf, result)
char *	prompt;
TBUFF **buf;
char *	result;
{
	register int	s;
	char	Reply[NFILEN];
	int	(*complete) P(( int, char *, int *)) = no_completion;

#if COMPLETE_DIRS
	if (isnamedcmd && !clexec) {
		complete = path_completion;
		MyBuff = 0;
		MyName = ScratchName(DirCompletion);
	}
#endif
	if (clexec || isnamedcmd) {
		if (tb_values((*buf)) != 0)
			(void)strcpy(Reply, tb_values((*buf)));
		else
			*Reply = EOS;

		only_dir = TRUE;
	        s = kbd_string(prompt, Reply, NFILEN, '\n',
			KBD_OPTIONS|KBD_MAYBEC, complete);
		freeMyList();
		only_dir = FALSE;
		if (s != TRUE)
			return s;

        } else if (!screen_to_bname(Reply)) {
		return FALSE;
        }

	(void)tb_scopy(buf, strcpy(result, Reply));
	return TRUE;
}

/******************************************************************************/

/*
 * This is called after 'mlreply_file()' to iterate over the list of files
 * that are matched by a glob-expansion.
 */
char *
filec_expand()
{
	if (MyGlob != 0) {
		if (MyGlob[++in_glob] != 0)
			return MyGlob[in_glob];
		free_expansion();
	}
	return 0;
}

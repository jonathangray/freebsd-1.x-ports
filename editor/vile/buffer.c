/*
 * Buffer management.
 * Some of the functions are internal,
 * and some are actually attached to user
 * keys. Like everyone else, they set hints
 * for the display system.
 *
 * $Log: buffer.c,v $
 * Revision 1.1  1994/02/01 03:29:12  jkh
 * Initial revision
 *
 * Revision 1.75  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.74  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.73  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.72  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.71  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.70  1993/07/07  12:31:09  pgf
 * small reformats
 *
 * Revision 1.69  1993/07/07  11:33:06  pgf
 * use mlforce instead of mlwrite for write-status message
 *
 * Revision 1.68  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.67  1993/06/23  21:31:16  pgf
 * added "undolimit" mode
 *
 * Revision 1.66  1993/06/22  10:24:21  pgf
 * new arg to freeundostacks()
 *
 * Revision 1.65  1993/06/21  14:23:41  pgf
 * protect against printing filenames with percent chars in name
 *
 * Revision 1.64  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.63  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.62  1993/05/24  15:25:41  pgf
 * tom's 3.47 changes, part b
 *
 * Revision 1.61  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.60  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.59  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.58  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.57  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.56  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.55  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.54  1993/03/16  16:04:01  pgf
 * fix 'parentheses suggested' warnings
 *
 * Revision 1.53  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.52  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.51  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.50  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.49  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.48  1993/01/23  14:25:14  foxharp
 * use strcmp against buffer-list name instead of trying to find it
 *
 * Revision 1.47  1993/01/23  13:38:23  foxharp
 * tom's changes for changed-buffers
 *
 * Revision 1.46  1993/01/18  20:25:59  foxharp
 * added missing f,n declaration
 *
 * Revision 1.45  1993/01/16  10:21:59  foxharp
 * a _lot_ of changes, to support tom dickey's autobuffer mode.  this allows
 * keeping the buffers in lru (traditional vile) order, or in command-line order
 * (as in traditional vi).  also allows auto-creation of buffers written to
 * alternate filenames.  some changes to buffer-list maintenance.
 *
 * Revision 1.44  1992/12/23  09:15:49  foxharp
 * tom dickey's changes for keeping the buffer list up to date when on screen
 *
 * Revision 1.43  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.42  1992/12/02  09:13:16  foxharp
 * changes for "c-shiftwidth"
 *
 * Revision 1.41  1992/11/19  08:48:14  foxharp
 * took out restriction against killing invisible buffers
 *
 * Revision 1.40  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.39  1992/08/05  21:52:33  foxharp
 * print filenames with DOS drive designators correctly
 *
 * Revision 1.38  1992/07/30  07:28:53  foxharp
 * don't prepend "./" to non-files in buffer list
 *
 * Revision 1.37  1992/07/24  07:49:38  foxharp
 * shorten_name changes
 *
 * Revision 1.36  1992/07/18  13:13:56  foxharp
 * put all path-shortening in one place (shorten_path()), and took out some old code now
 * unnecessary
 *
 * Revision 1.35  1992/07/15  08:52:44  foxharp
 * trim leading `pwd` from buffer names in buffer list
 *
 * Revision 1.34  1992/07/13  19:36:13  foxharp
 * show current directory in buffer list
 *
 * Revision 1.33  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.32  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.31  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.30  1992/01/03  23:33:50  pgf
 * use new ch_fname() routine to manipulate filenames
 *
 * Revision 1.29  1991/11/12  23:50:10  pgf
 * no longer allocate text for the header line -- the scanner doesn't
 * need it
 *
 * Revision 1.28  1991/11/12  23:43:23  pgf
 * pass stringend param to regexp, for end of string
 *
 * Revision 1.27  1991/11/08  13:08:44  pgf
 * ifdefed unused func
 *
 * Revision 1.26  1991/11/03  17:48:11  pgf
 * pass -1 to regexec to indicate no srchlim specified -- 0 was ambiguous
 *
 * Revision 1.25  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.24  1991/10/28  14:21:05  pgf
 * renamed curtabstopval to curtabval, initialized new b_acount field
 * in BUFFER struct
 *
 * Revision 1.23  1991/10/28  01:01:06  pgf
 * added start offset and end offset to regexec calls
 *
 * Revision 1.22  1991/10/26  00:10:35  pgf
 * use regexec for c suffixes
 *
 * Revision 1.21  1991/10/23  14:19:53  pgf
 * allocate some text for a buffer's header line, and put a -1 there as the
 * first and only character.  this helps the search engine.
 *
 * Revision 1.20  1991/10/22  14:08:23  pgf
 * took out old ifdef BEFORE code
 *
 * Revision 1.19  1991/10/10  12:33:33  pgf
 * changes to support "block malloc" of line text -- now for most files
 * there is are two mallocs and a single read, no copies.  previously there
 * were two mallocs per line, and two copies (stdio's and ours).  This change
 * implies that lines and line text should not move between buffers, without
 * checking that the text and line struct do not "belong" to the buffer.
 *
 * Revision 1.18  1991/09/30  01:47:24  pgf
 * don't allow recursive zotbuf() via delwp()
 *
 * Revision 1.17  1991/09/26  13:17:11  pgf
 * can now kill a displayed buffer, and
 * we initialize window values at creation time
 *
 * Revision 1.16  1991/09/20  13:11:53  pgf
 * protect against null curtabstopval after make_current() returns
 *
 * Revision 1.15  1991/09/10  12:30:45  pgf
 * initialize b_wline when creating buffer
 *
 * Revision 1.14  1991/09/10  01:52:05  pgf
 * fixed bug caused by re-tabbing
 *
 * Revision 1.13  1991/09/10  00:50:23  pgf
 * re-tab, and elim. unused local in swbuffer
 *
 * Revision 1.12  1991/08/16  11:03:50	pgf
 * since we now switch to popped up buffers, the numbering in the buffer
 * list was off by one -- now we number from 1
 *
 * Revision 1.11  1991/08/12  09:25:10	pgf
 * now store w_line in w_traits while buffer is offscreen, so reframe
 * isn't always necessary.  don't force reframe on redisplay.
 *
 * Revision 1.10  1991/08/08  13:15:25	pgf
 * removed some ifdef BEFORE
 *
 * Revision 1.9  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.8
 * date: 1991/08/06 15:11:31;
 * global/local values
 * and printf/list stuff
 *
 * revision 1.7
 * date: 1991/06/25 19:52:06;
 * massive data structure restructure
 *
 * revision 1.6
 * date: 1991/06/16 17:34:23;
 * added support for local values of tabstops and fillcol
 *
 * revision 1.5
 * date: 1991/04/08 15:45:31;
 * fixed readin() arg count
 *
 * revision 1.4
 * date: 1991/04/04 09:28:02;
 * line text is now separate from LINE struct
 *
 * revision 1.3
 * date: 1991/02/21 09:15:35;
 * added horizontal scroll support
 *
 * revision 1.2
 * date: 1990/12/06 19:47:56;
 * fixed list-buffer window re-use problem, and startup core dumps
 * if popupbuff was called in startup file
 *
 * revision 1.1
 * date: 1990/09/21 10:24:46;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"

#define BUFFER_LIST_NAME ScratchName(Buffer List)

/*--------------------------------------------------------------------------*/
static	BUFFER *find_BufferList P(( void ));
#if	OPT_UPBUFF
static	int	show_BufferList P(( BUFFER * ));
#define	update_on_chg(bp) (!b_is_invisible(bp) || show_all)
#endif
static	BUFFER *find_bp P(( BUFFER * ));
static	int	countBuffers P(( void ));
static	BUFFER *find_nth_created P(( int ));
static	BUFFER *find_nth_used P(( int ));
static	BUFFER *find_latest P(( void ));
static	BUFFER *find_b_file P(( char * ));
static	BUFFER *find_b_hist P(( int ));
static	BUFFER *find_b_number P(( char * ));
static	BUFFER *find_any_buffer P(( char * ));
static	int	zotwp P(( BUFFER * ));
static	void	MarkDeleted P(( BUFFER * ));
static	void	MarkUnused P(( BUFFER * ));
static	void	FreeBuffer P(( BUFFER * ));
static	void	TrackAlternate P(( BUFFER * ));
static	char *	hist_lookup P(( int ));
static	int	lookup_hist P(( BUFFER * ));
static	int	hist_show P(( void ));
#if !SMALLER
static	void	footnote P(( int ));
#endif
static	void	makebufflist P(( int, char * ));

/*--------------------------------------------------------------------------*/

static	BUFFER	*last_bp,	/* noautobuffer value */
		*this_bp,	/* '%' buffer */
		*that_bp;	/* '#' buffer */
static	int	show_all,	/* true iff we show all buffers */
		updating_list;

/*--------------------------------------------------------------------------*/

/*
 * Returns the buffer-list pointer, if it exists.
 */
static
BUFFER *find_BufferList()
{
	return find_b_name(BUFFER_LIST_NAME);
}

/*
 * Look for a buffer-pointer in the list, to see if it has been delinked yet.
 */
static
BUFFER *find_bp(bp1)
	BUFFER *bp1;
{
	register BUFFER *bp;
	for_each_buffer(bp)
		if (bp == bp1)
			return bp;
	return 0;
}

/*
 * Returns the total number of buffers in the list.
 */
static
int	countBuffers()
{
	register BUFFER *bp;
	register int	count = 0;
	for_each_buffer(bp)
		count++;
	return count;
}

/*
 * Returns the n'th buffer created
 */
static
BUFFER *find_nth_created(n)
	int	n;
{
	register BUFFER *bp;

	for_each_buffer(bp)
		if (bp->b_created == n)
			return bp;
	return 0;
}

/*
 * Returns the n'th buffer used
 */
static
BUFFER *find_nth_used(n)
	int	n;
{
	register BUFFER *bp;

	for_each_buffer(bp)
		if (bp->b_last_used == n)
			return bp;
	return 0;
}

/*
 * Returns the buffer with the largest value of 'b_last_used'.
 */
static
BUFFER *find_latest()
{
	register BUFFER *bp, *maxbp = 0;

	for_each_buffer(bp) {
		if (maxbp == 0)
			maxbp = bp;
		else if (maxbp->b_last_used < bp->b_last_used)
			maxbp = bp;
	}
	return maxbp;
}

/*
 * Look for a filename in the buffer-list
 */
static
BUFFER *find_b_file(fname)
	char *fname;
{
	register BUFFER *bp;
	char	nfname[NFILEN];

	(void)lengthen_path(strcpy(nfname, fname));
	for_each_buffer(bp)
		if (same_fname(nfname, bp, FALSE))
			return bp;
	return 0;
}

/*
 * Look for a specific buffer number
 */
static
BUFFER *find_b_hist(number)
	int number;
{
	register BUFFER *bp;

	if (number >= 0) {
		for_each_buffer(bp)
			if (!b_is_temporary(bp) && (number-- <= 0))
				break;
	} else
		bp = 0;
	return bp;
}

/*
 * Look for a buffer-number (i.e., one from the buffer-list display)
 */
static
BUFFER *find_b_number(number)
	char *number;
{
	register int	c = 0;
	while (isdigit(*number))
		c = (c * 10) + (*number++ - '0');
	if (!*number)
		return find_b_hist(c);
	return 0;
}

/*
 * Find buffer, given (possibly) filename, buffer name or buffer number
 */
static
BUFFER *find_any_buffer(name)
	char *name;
{
	register BUFFER *bp;

	if (!(bp=find_b_name(name))		/* Try buffer */
	 && !(bp=find_b_file(name))		/* ...then filename */
	 && !(bp=find_b_number(name))) {	/* ...then number */
		mlforce("[No such buffer] %s", name);
		return 0;
	}

	return bp;
}

/*
 * Delete all instances of window pointer for a given buffer pointer
 */
static
int	zotwp(bp)
	BUFFER *bp;
{
	register WINDOW *wp;
	WINDOW	dummy;
	int s = FALSE;

	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			dummy.w_wndp = wp->w_wndp;
			s = delwp(wp);
			wp = &dummy;
		}
	}
	return s;
}

/*
 * Mark a buffer's created member to 0 (unused), adjusting the other buffers
 * so that there are no gaps.
 */
static
void	MarkDeleted(bp)
	register BUFFER *bp;
{
	int	created = bp->b_created;

	if (created) {
		bp->b_created = 0;
		for_each_buffer(bp) {
			if (bp->b_created > created)
				bp->b_created -= 1;
		}
	}
}

/*
 * Mark a buffer's last-used member to 0 (unused), adjusting the other buffers
 * so that there are no gaps.
 */
static
void	MarkUnused(bp)
	register BUFFER *bp;
{
	int	used = bp->b_last_used;

	if (used) {
		bp->b_last_used = 0;
		for_each_buffer(bp) {
			if (bp->b_last_used > used)
				bp->b_last_used -= 1;
		}
	}
}

/*
 * After 'bclear()', frees remaining storage for a buffer.
 */
static
void	FreeBuffer(bp)
	BUFFER *bp;
{
	register BUFFER *bp1, *bp2;

	if (bp->b_fname != out_of_mem)
		FreeIfNeeded(bp->b_fname);

#if !WINMARK
	if (same_ptr(MK.l, bp->b_line.l)) {
		MK.l = null_ptr;
		MK.o = 0;
	}
#endif
	lfree(bp->b_line.l, bp);		/* Release header line. */
	bp1 = NULL;				/* Find the header.	*/
	bp2 = bheadp;
	while (bp2 != bp) {
		bp1 = bp2;
		bp2 = bp2->b_bufp;
	}
	bp2 = bp2->b_bufp;			/* Next one in chain.	*/
	if (bp1 == NULL)			/* Unlink it.		*/
		bheadp = bp2;
	else
		bp1->b_bufp = bp2;
	MarkUnused(bp);
	MarkDeleted(bp);
	if (bp == last_bp)
		last_bp = 0;
	free((char *) bp);			/* Release buffer block */
}

/*
 * Adjust buffer-list's last-used member to account for a new buffer.
 */
static
void	TrackAlternate(newbp)
	BUFFER *newbp;
{
	register BUFFER *bp;

	if (!updating_list) {
		MarkUnused(newbp);
		if ((bp = find_latest()) != 0) {
			if (bp != newbp)
				newbp->b_last_used = (bp->b_last_used + 1);
		} else
			newbp->b_last_used = 1;
	}
}

/* c is an index (counting only visible/nonscratch) into buffer list */
static
char *	hist_lookup(c)
	int c;
{
	register BUFFER *bp = find_b_hist(c);

	return (bp != 0) ? get_bname(bp) : NULL;
}

/* returns the buffer corresponding to the given number in the history */
static
int	lookup_hist(bp1)
	BUFFER *bp1;
{
	register BUFFER *bp;
	register int	count = -1;

	for_each_buffer(bp)
		if (!b_is_temporary(bp)) {
			count++;
			if (bp == bp1)
				return count;
		}
	return -1;	/* no match */
}

static
int	hist_show()
{
	register BUFFER *bp;
	register int i = 0;
	char line[NLINE];

	(void)strcpy(line,"");
	for_each_buffer(bp) {
		if (!b_is_temporary(bp)) {
			if (bp != curbp) {	/* don't bother with current */
				(void)lsprintf(line+strlen(line), "  %d%s%s",
					i,
					b_is_changed(bp) ? "* " : " ",
					get_bname(bp));
			}
			if (++i > 9)	/* limit to single-digit */
				break;
		}
	}
	if (strcmp(line,"")) {
		mlforce("%s",line);
		return TRUE;
	} else {
		return FALSE;
	}
}

/*
 * Given a buffer, returns any corresponding WINDOW pointer
 */
WINDOW *
bp2any_wp(bp)
BUFFER *bp;
{
	register WINDOW *wp;
	for_each_window(wp)
		if (wp->w_bufp == bp)
			break;
	return wp;
}

/*
 * Lets the user select a given buffer by its number.
 */
int
histbuff(f,n)
int f,n;
{
	register int thiscmd, c;
	register BUFFER *bp = 0;
	char *bufn;

	if (f == FALSE) {
		if (!hist_show())
			return FALSE;
		thiscmd = lastcmd;
		c = kbd_seq();
		mlerase();
		if (c == thiscmd) {
			c = lookup_hist(bp = find_alt());
		} else if (isdigit(c)) {
			c = c - '0';
		} else {
			if (!isreturn(c))
				tungetc(c);
			return FALSE;
		}
	} else {
		c = n;
	}

	if (c >= 0) {
		if ((bufn = hist_lookup(c)) == NULL) {
			TTbeep();
			mlforce("[No such buffer.]");
			return FALSE;
		}
		/* first assume its a buffer name, then a file name */
		if ((bp = find_b_name(bufn)) == NULL)
			return getfile(bufn,TRUE);

	} else if (bp == 0) {
		TTbeep();
		mlforce("[No alternate buffer]");
		return FALSE;
	}

	return swbuffer(bp);
}

/*
 * Returns the alternate-buffer pointer, if any
 */
BUFFER *find_alt()
{
	register BUFFER *bp;

	if (global_g_val(GMDABUFF)) {
		BUFFER *any_bp = 0;
		if ((bp = find_bp(curbp)) == 0)
			bp = bheadp;
		for (; bp; bp = bp->b_bufp) {
			if (bp != curbp) {
				if (b_is_scratch(bp)) {
					if (!any_bp)
						any_bp = bp;
				} else
					return bp;
			}
		}
		return any_bp;
	} else {
		register BUFFER *last = 0,
				*next = find_latest();

		for_each_buffer(bp) {
			if ((bp != next)
			 && (bp->b_active)) {
				if (last) {
					if (last->b_last_used < bp->b_last_used)
						last = bp;
				} else
					last = bp;
			}
		}
		return last;
	}
}

/* make '#' buffer in noautobuffer-mode, given filename */
void
imply_alt(fname, copy, lockfl)
char *	fname;
int	copy;
int	lockfl;
{
	register BUFFER *bp;
	register LINE	*lp;
	BUFFER *savebp;
	char bname[NBUFN];
	char nfname[NFILEN];

	if (interrupted || fname == 0)	/* didn't really have a filename */
		return;

	(void)lengthen_path(strcpy(nfname, fname));
	if (global_g_val(GMDIMPLYBUFF)
	 && curbp != 0
	 && curbp->b_fname != 0
	 && !same_fname(nfname, curbp, FALSE)
	 && !isInternalName(fname)) {
		savebp = curbp;
		if (!(bp = find_b_file(nfname))) {
			L_NUM	top, now;

			makename(bname, fname);
			unqname(bname, TRUE);

			if (!(bp=bfind(bname, 0))) {
				mlforce("[Cannot create buffer]");
				return;
			}

			/* fill the buffer */
			b_clr_flags(bp, BFINVS|BFCHG);
			b_set_flags(bp, BFIMPLY);
			bp->b_active = TRUE;
			ch_fname(bp, nfname);
			make_local_b_val(bp,MDNEWLINE);
			if (curwp != 0 && curwp->w_bufp == curbp) {
				top = line_no(curbp, curwp->w_line.l);
				now = line_no(curbp, curwp->w_dot.l);
			} else
				top = now = -1;

			if (copy) {
				for_each_line(lp, savebp) {
					if (addline(bp,lp->l_text,lp->l_used) != TRUE) {
						mlforce("[Copy-buffer failed]");
						return;
					}
				}
				set_b_val(bp, MDNEWLINE, b_val(savebp,MDNEWLINE));
			} else
				readin(fname, lockfl, bp, FALSE);

			/* setup so that buffer-toggle works as in vi (i.e.,
			 * the top/current lines of the screen are the same).
			 */
			if (now >= 0) {
				for_each_line(lp,bp) {
					if (--now == 0) {
						bp->b_dot.l = l_ptr(lp);
						bp->b_dot.o = curbp->b_dot.o;
						break;
					}
					if (--top == 0) {
						bp->b_wline.l = l_ptr(lp);
					}
				}
			}

			/* set last-used */
			make_current(bp);
			make_current(savebp);
		} else {	/* buffer already exists */
			make_current(bp);
			make_current(savebp);
		}
	}
}

/* switch back to the most recent buffer */
/* ARGSUSED */
int
altbuff(f,n)
int f,n;
{
	register BUFFER *bp = find_alt();
	if (bp == 0) {
		TTbeep();
		mlforce("[No alternate filename to substitute for #]");
		return FALSE;
	} else {
		return swbuffer(bp);
	}
}

/*
 * Attach a buffer to a window. The
 * values of dot and mark come from the buffer
 * if the use count is 0. Otherwise, they come
 * from some other window.
 */
/* ARGSUSED */
int
usebuffer(f, n)
int f,n;
{
	register BUFFER *bp;
	register int	s;
	char		bufn[NBUFN+1];

	bufn[0] = EOS;
	if ((s=mlreply("Use buffer: ", bufn, sizeof(bufn))) != TRUE)
		return s;
	if (!(bp=find_any_buffer(bufn)))	/* Try buffer */
		return FALSE;
	return swbuffer(bp);
}

/* switch back to the first buffer (i.e., ":rewind") */
/* ARGSUSED */
int
firstbuffer(f,n)
int f, n;
{
	int s = histbuff(TRUE,0);
	if (!global_g_val(GMDABUFF))
		last_bp = s ? curbp : 0;
	return s;
}

/* ARGSUSED */
int
nextbuffer(f, n)	/* switch to the next buffer in the buffer list */
int f, n;	/* default flag, numeric argument */
{
	register BUFFER *bp;	/* eligible buffer to switch to*/
	register BUFFER *stopatbp;	/* eligible buffer to switch to*/

	if (global_g_val(GMDABUFF)) {	/* go backward thru buffer-list */
		stopatbp = NULL;
		while (stopatbp != bheadp) {
			/* get the last buffer in the list */
			bp = bheadp;
			while(bp->b_bufp != stopatbp)
				bp = bp->b_bufp;
			/* if that one's invisible, back up and try again */
			if (b_is_invisible(bp))
				stopatbp = bp;
			else
				return swbuffer(bp);
		}
	} else {			/* go forward thru args-list */
		if (!(stopatbp = curbp))
			stopatbp = find_nth_created(1);
		if (last_bp == 0)
			last_bp = find_b_hist(0);
		if (last_bp != 0) {
			for (bp = last_bp->b_bufp; bp; bp = bp->b_bufp) {
				if (b_is_argument(bp))
					return swbuffer(last_bp = bp);
			}
		}
		mlforce("[No more files to edit]");
	}
	/* we're back to the top -- they were all invisible */
	return swbuffer(stopatbp);
}

/* bring nbp to the top of the list, where curbp usually lives */
void
make_current(nbp)
BUFFER *nbp;
{
	register BUFFER *bp;

	TrackAlternate(nbp);

	if (!updating_list && global_g_val(GMDABUFF)) {
		if (nbp != bheadp) {	/* remove nbp from the list */
			bp = bheadp;
			while(bp->b_bufp != nbp)
				bp = bp->b_bufp;
			bp->b_bufp = nbp->b_bufp;

			/* put it at the head */
			nbp->b_bufp = bheadp;

			bheadp = nbp;
		}
		curbp = bheadp;
	} else
		curbp = nbp;

	curtabval = tabstop_val(curbp);
	curswval = shiftwid_val(curbp);
}


int
swbuffer(bp)	/* make buffer BP current */
register BUFFER *bp;
{

	if (!bp) {
		mlforce("BUG:  swbuffer passed null bp");
		return FALSE;
	}

	if (curbp == bp
	 && curwp != 0
	 && l_ref(DOT.l) != 0
	 && curwp->w_bufp == bp)  /* no switching to be done */
		return TRUE;

	if (curbp) {
		/* if we'll have to take over this window, and it's the last */
		if (bp->b_nwnd == 0 && --(curbp->b_nwnd) == 0) {
			undispbuff(curbp,curwp);
		}
	}
	make_current(bp);	/* sets curbp */

	/* get it already on the screen if possible */
	if (bp->b_nwnd > 0)  { /* then it's on the screen somewhere */
		register WINDOW *wp = bp2any_wp(bp);
		if (!wp)
			mlforce("BUG: swbuffer: wp still NULL");
		curwp = wp;
		upmode();
#ifdef MDCHK_MODTIME
		(void)check_modtime( bp, bp->b_fname );
#endif
		if (bp != find_BufferList())
			updatelistbuffers();
		return TRUE;
	}

	/* oh well, suck it into this window */
	curwp->w_bufp  = bp;
	curwp->w_flag |= WFMODE|WFHARD;		/* Quite nasty.		*/
	if (bp->b_nwnd++ == 0) {		/* First use.		*/
		copy_traits(&(curwp->w_traits), &(bp->b_wtraits));
	}

	if (bp->b_active != TRUE) {		/* buffer not active yet*/
		/* read it in and activate it */
		(void)readin(bp->b_fname, TRUE, bp, TRUE);
		curwp->w_dot.l = lFORW(bp->b_line.l);
		curwp->w_dot.o = 0;
		bp->b_active = TRUE;
	}
#ifdef MDCHK_MODTIME
	else
		(void)check_modtime( bp, bp->b_fname );
#endif
	updatelistbuffers();
	return TRUE;
}


void
undispbuff(bp,wp)
register BUFFER *bp;
register WINDOW *wp;
{
	/* get rid of it completely if it's a scratch buffer,
		or it's empty and unmodified */
	if (b_is_scratch(bp)
	 || ( global_g_val(GMDABUFF) && !b_is_changed(bp) && is_empty_buf(bp)) ) {
		(void)zotbuf(bp);
	} else {  /* otherwise just adjust it off the screen */
		copy_traits(&(bp->b_wtraits), &(wp->w_traits));
	}
}

/* return true iff c-mode is active for this buffer */
int
cmode_active(bp)
register BUFFER *bp;
{
	if (is_local_b_val(bp,MDCMOD))
		return b_val(bp, MDCMOD);
	else
		return (b_val(bp, MDCMOD) && has_C_suffix(bp));
}


/* return the correct tabstop setting for this buffer */
int
tabstop_val(bp)
register BUFFER *bp;
{
	return b_val(bp, (cmode_active(bp) ? VAL_C_TAB : VAL_TAB));
}

/* return the correct shiftwidth setting for this buffer */
int
shiftwid_val(bp)
register BUFFER *bp;
{
	return b_val(bp, (cmode_active(bp) ? VAL_C_SWIDTH : VAL_SWIDTH));
}

int
has_C_suffix(bp)
register BUFFER *bp;
{
	int s;
	int save = ignorecase;
	ignorecase = FALSE;
	s =  regexec(global_g_val_rexp(GVAL_CSUFFIXES)->reg,
			bp->b_fname, (char *)0, 0, -1);
	ignorecase = save;
	return s;
}

/*
 * Dispose of a buffer, by name.  If this is a screen-command, pick the name up
 * from the screen.  Otherwise, ask for the name.  Look it up (don't get too
 * upset if it isn't there at all!).  Get quite upset if the buffer is being
 * displayed.  Clear the buffer (ask if the buffer has been changed).  Then
 * free the header line and the buffer header.
 *
 * If we are given a repeat-count, try to kill that many buffers.  Killing from
 * names selected from the buffer-list is a special case, because the cursor
 * may be pointing to the buffer-number column.  In that case, we must
 * recompute the buffer-contents.  Otherwise, move the cursor down one line
 * (staying in the same column), so we can pick up the names from successive
 * lines.
 */
int
killbuffer(f, n)
int f,n;
{
	register BUFFER *bp;
	register int	s;
	char bufn[NFILEN];

#if !SMALLER
	C_NUM	save_COL;
	MARK	save_DOT;
	MARK	save_TOP;
	int	animated = f
			&& (n > 1)
			&& (curbp != 0)
			&& (curbp == find_BufferList());
	int	special  = animated && (DOT.o == 2);

	if (animated && !special) {
		save_COL = getccol(FALSE);
		save_DOT = curwp->w_dot;
		save_TOP = curwp->w_line;
	} else
		save_COL = 0;	/* appease gcc */
#endif

	if (!f)
		n = 1;
	for(;;) {
		bufn[0] = EOS;
		if (clexec || isnamedcmd) {
			if ((s=mlreply("Kill buffer: ", bufn, sizeof(bufn))) != TRUE)
				break;
		} else if ((s = screen_to_bname(bufn)) != TRUE) {
			mlforce("[Nothing selected]");
			break;
		}

		if (!(bp=find_any_buffer(bufn))) {	/* Try buffer */
			s = FALSE;
			break;
		}

		if ((curbp == bp) && (find_alt() == 0)) {
			mlforce("[Can't kill that buffer]");
			s = FALSE;
			break;
		}

		(void)strcpy(bufn, get_bname(bp)); /* ...for info-message */
		if (bp->b_nwnd > 0)  { /* then it's on the screen somewhere */
			(void)zotwp(bp);
			if (find_bp(bp) == 0) { /* delwp must have zotted us */
				s = FALSE;
				break;
			}
		}
		if ((s = zotbuf(bp)) != TRUE)
			break;
		mlwrite("Buffer %s gone", bufn);
		if (--n > 0) {
#if !SMALLER
			if (special)
				(void)update(TRUE);
			else
#endif
			 if (!forwline(FALSE,1))
				break;
		} else
			break;
	}

#if !SMALLER
	if (animated && !special) {
		curgoal = save_COL;
		DOT     = save_DOT;
		DOT.o   = getgoal(l_ref(DOT.l));
		curwp->w_line = save_TOP;
		curwp->w_flag &= ~WFMOVE;
	}
#endif
	return s;
}

int
zotbuf(bp)	/* kill the buffer pointed to by bp */
register BUFFER *bp;
{
	register int	s;
	register int	didswitch = FALSE;

#define no_del
#ifdef no_del
	if (bp->b_nwnd != 0) {			/* Error if on screen.	*/
		mlforce("[Buffer is being displayed]");
		return (FALSE);
	}
#else
	if (curbp == bp) {
		didswitch = TRUE;
		if (find_alt() == 0) {
			mlforce("[Can't kill that buffer]");
			return FALSE;
		}
	}
	if (bp->b_nwnd > 0)  { /* then it's on the screen somewhere */
		(void)zotwp(bp);
		if (find_bp(bp) == 0) /* delwp must have zotted us */
			return TRUE;
	}

#endif
	/* Blow text away.	*/
	if ((s=bclear(bp)) != TRUE) {
		/* the user must have answered no */
		if (didswitch)
			(void)swbuffer(bp);
		return (s);
	}

	FreeBuffer(bp);
	updatelistbuffers();
	return (TRUE);
}

/* ARGSUSED */
int
namebuffer(f,n)		/*	Rename the current buffer	*/
int f, n;		/* default Flag & Numeric arg */
{
	register BUFFER *bp;	/* pointer to scan through all buffers */
	static char bufn[NBUFN+1];	/* buffer to hold buffer name */
	char *prompt = "New name for buffer: ";

	/* prompt for and get the new buffer name */
	do {
		if (mlreply(prompt, bufn, sizeof(bufn)) != TRUE)
			return(FALSE);
		prompt = "That name's been used.  New name: ";
		bp = find_b_name(bufn);
		if (bp == curbp)	/* no change */
			return(FALSE);
	} while (bp != 0);

	set_bname(curbp, bufn);		/* copy buffer name to structure */
	curwp->w_flag |= WFMODE;	/* make mode line replot */
	mlerase();
	updatelistbuffers();
	return(TRUE);
}

/* create or find a window, and stick this buffer in it.  when
	done, we own the window and the buffer, but they are _not_
	necessarily curwp and curbp */
int
popupbuff(bp)
BUFFER *bp;
{
	register WINDOW *wp;

	if (!curbp) {
		curbp = bp;  /* possibly at startup time */
		curwp->w_bufp = curbp;
		++curbp->b_nwnd;
	} else if (bp->b_nwnd == 0) {	/* Not on screen yet. */
		if ((wp = wpopup()) == NULL)
			return FALSE;
		if (--wp->w_bufp->b_nwnd == 0)
			undispbuff(wp->w_bufp,wp);
		wp->w_bufp  = bp;
		++bp->b_nwnd;
	}

	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			wp->w_line.l = lFORW(bp->b_line.l);
			wp->w_dot.l  = lFORW(bp->b_line.l);
			wp->w_dot.o  = 0;
#ifdef WINMARK
			wp->w_mark = nullmark;
#endif
			wp->w_lastdot = nullmark;
			wp->w_values = global_w_values;
			wp->w_flag |= WFMODE|WFHARD;
		}
	}
	(void)swbuffer(bp);
	return TRUE;
}

/*
 * Invoked after changing mode 'autobuffer', this relinks the list of buffers
 * sorted according to the mode: by creation or last-used order.
 */
void
sortlistbuffers()
{
	register BUFFER *bp, *newhead = 0;
	register int	c;

	if (global_g_val(GMDABUFF)) {
		c = 1;
		while ((bp = find_nth_used(c++)) != 0) {
			bp->b_relink = newhead;
			newhead = bp;
		}
	} else {
		c = countBuffers();
		while ((bp = find_nth_created(c--)) != 0) {
			bp->b_relink = newhead;
			newhead = bp;
		}
	}

	for (bp = newhead; bp; bp = bp->b_relink)
		bp->b_bufp = bp->b_relink;
	bheadp = newhead;

	updatelistbuffers();
}

/*
 * List all of the active buffers.  First update the special buffer that holds
 * the list.  Next make sure at least 1 window is displaying the buffer list,
 * splitting the screen if this is what it takes.  Lastly, repaint all of the
 * windows that are displaying the list.  A numeric argument forces it to list
 * invisible buffers as well.
 */
int
togglelistbuffers(f, n)
int f,n;
{
	register BUFFER *bp;

	/* if it doesn't exist, create it */
	if ((bp = find_BufferList()) == NULL)
		return listbuffers(f,n);

	/* if it does exist, delete the window, which in turn
		will kill the BFSCRTCH buffer */
	return zotwp(bp);
}

/*
 * Track/emit footnotes for 'makebufflist()', showing only the ones we use.
 */
#if !SMALLER
static
void	footnote(c)
	int	c;
{
	static	struct	{
		char	*name;
		int	flag;
	} table[] = {
		{"automatic",	0},
		{"invisible",	0},
		{"modified",	0},
		{"scratch",	0},
		{"unread",	0},
		};
	register int	j, next;

	for (j = next = 0; j < SIZEOF(table); j++) {
		if (c != 0) {
			if (table[j].name[0] == c) {
				bputc(c);
				table[j].flag = TRUE;
				break;
			}
		} else if (table[j].flag) {
			bprintf("%s '%c'%s %s",
				next ? "," : "notes:",	table[j].name[0],
				next ? ""  : " is",	table[j].name);
			next++;
			table[j].flag = 0;
		}
	}
	if (next)
		bputc('\n');
}
#define	MakeNote(c)	footnote(c)
#define	ShowNotes()	footnote(0)
#else
#define	MakeNote(c)	bputc(c)
#define	ShowNotes()
#endif

/*
 * This routine rebuilds the text in the buffer that holds the buffer list.  It
 * is called by the list buffers command.  Return TRUE if everything works.
 * Return FALSE if there is an error (if there is no memory).  The variable
 * 'show_all' (set if the command had a repeat-count) indicates whether to list
 * hidden buffers.
 */
/* ARGSUSED */
static
void	makebufflist(iflag,dummy)
	int iflag;	/* list hidden buffer flag */
	char *dummy;
{
	register BUFFER *bp;
	LINEPTR curlp;		/* entry corresponding to buffer-list */
	int nbuf = 0;		/* no. of buffers */
	int this_or_that;

	curlp = null_ptr;

	if (this_bp == 0)
		this_bp = curbp;
	if (this_bp == that_bp)
		that_bp = find_alt();

	bprintf("      %7s %*s %s\n", "Size",NBUFN,"Buffer name","Contents");
	bprintf("      %7p %*p %30p\n", '-',NBUFN,'-','-');

	/* output the list of buffers */
	for_each_buffer(bp) {
		/* skip invisible buffers and ourself if iflag is false */
		if ((b_is_temporary(bp)) && !show_all) {
			continue;
		}

		/* output status flag (e.g., has the file been read in?) */
		if (b_is_scratch(bp))
			MakeNote('s');
		else if (!(bp->b_active))
			MakeNote('u');
		else if (b_is_implied(bp))
			MakeNote('a');
		else if (b_is_invisible(bp))
			MakeNote('i');
		else if (b_is_changed(bp))
			MakeNote('m');
		else
			bputc(' ');

		this_or_that = (bp == this_bp)
			? '%'
			: (bp == that_bp)
				? '#'
				: ' ';

		if (b_is_temporary(bp))
			bprintf("   %c ", this_or_that);
		else
			bprintf(" %2d%c ", nbuf++, this_or_that);

		bsizes(bp);
		bprintf("%7ld %*S ", bp->b_bytecount, NBUFN, bp->b_bname );
		{
			char	temp[NFILEN];
			char	*p;

			if ((p = bp->b_fname) != 0)
				p = shorten_path(strcpy(temp, p), TRUE);

			if (p != 0)
				bprintf("%s",p);
		}
		bprintf("\n");
		if (bp == curbp)
			curlp = lBACK(lBACK(curbp->b_line.l));
	}
	ShowNotes();
	bprintf("             %*s %s", NBUFN, "Current dir:",
		current_directory(FALSE));

	/* show the actual size of the buffer-list */
	if (!same_ptr(curlp, null_ptr)) {
		char	temp[20];
		bsizes(curbp);
		(void)lsprintf(temp, "%7ld", curbp->b_bytecount);
		(void)memcpy(l_ref(curlp)->l_text + 6, temp, strlen(temp));
	}
}

#if	OPT_UPBUFF
/*
 * (Re)compute the contents of the buffer-list.  Use the flag 'updating_list'
 * as a semaphore to avoid adjusting the last used/created indices while
 * cycling over the list of buffers.
 */
/*ARGSUSED*/
static int
show_BufferList(bp)
BUFFER	*bp;
{
	register int	status;
	if ((status = (!updating_list++ != 0)) != FALSE) {
		this_bp = curbp;
		that_bp = find_alt();
		status = liststuff(BUFFER_LIST_NAME, makebufflist, 0, (char *)0);
		b_clr_flags(curbp, BFUPBUFF);
	}
	updating_list--;
	return status;
}

/*
 * If the list-buffers window is visible, update it after operations that
 * would modify the list.
 */
void
updatelistbuffers()
{
	register BUFFER	*bp = find_BufferList();

	if (bp != 0) {
		bp->b_upbuff = show_BufferList;
		b_set_flags(bp,BFUPBUFF);
	}
}
#endif	/* OPT_UPBUFF */

/* ARGSUSED */
int
listbuffers(f, n)
int f,n;
{
#if	OPT_UPBUFF
	register int status;

	show_all = f;	/* save this to use in automatic updating */
	if (find_BufferList() != 0) {
		updatelistbuffers();
		return TRUE;
	}
	this_bp = 0;
	that_bp = curbp;
	status  = liststuff(BUFFER_LIST_NAME, makebufflist, 0, (char *)0);
	b_clr_flags(curbp, BFUPBUFF);
	return status;
#else
	this_bp = 0;
	that_bp = curbp;
	return liststuff(BUFFER_LIST_NAME, makebufflist, 0, (char *)0);
#endif
}

/*
 * The argument "text" points to
 * a string. Append this line to the
 * buffer. Handcraft the EOL
 * on the end. Return TRUE if it worked and
 * FALSE if you ran out of room.
 */
int
addline(bp,text,len)
register BUFFER *bp;
char	*text;
int len;
{
	if (add_line_at (bp, lBACK(bp->b_line.l), text, len) == TRUE) {
		/* If "." is at the end, move it to new line  */
		if (sameline(bp->b_dot, bp->b_line))
			bp->b_dot.l = lBACK(bp->b_line.l);
		return TRUE;
	}
	return FALSE;
}

/*
 * Add a LINE filled with the given text after the specified LINE.
 */
int
add_line_at (bp, prevp, text, len)
register BUFFER	*bp;
LINEPTR	prevp;
char	*text;
int	len;
{
	fast_ptr LINEPTR newlp;
	fast_ptr LINEPTR nextp;
	register LINE *	lp;
	register int	ntext;

	nextp = lFORW(prevp);
	ntext = (len < 0) ? strlen(text) : len;
	newlp = lalloc(ntext, bp);
	if (same_ptr(newlp, null_ptr))
		return (FALSE);

	lp = l_ref(newlp);
#if OPT_MAP_MEMORY
	if (ntext > lp->l_size)
		ntext = lp->l_size;
#endif
	if (ntext > 0)
		(void)memcpy(lp->l_text, text, (SIZE_T)ntext);

	/* try to maintain byte/line counts? */
	if (b_is_counted(bp)) {
		if (same_ptr(nextp, bp->b_line.l)) {
			make_local_b_val(bp,MDNEWLINE);
			set_b_val(bp, MDNEWLINE, TRUE);
			bp->b_bytecount += (ntext+1);
			bp->b_linecount += 1;
#if !SMALLER		/* tradeoff between codesize & data */
			lp->l_number = bp->b_linecount;
#endif
		} else
			b_clr_counted(bp);
	}

	set_lFORW(prevp, newlp);	/* link into the buffer */
	set_lBACK(newlp, prevp);
	set_lBACK(nextp, newlp);
	set_lFORW(newlp, nextp);

	return (TRUE);
}

/*
 * Look through the list of
 * buffers. Return TRUE if there
 * are any changed buffers. Buffers
 * that hold magic internal stuff are
 * not considered; who cares if the
 * list of buffer names is hacked.
 * Return FALSE if no buffers
 * have been changed.
 */
int
anycb()
{
	register BUFFER *bp;
	register int cnt = 0;

	for_each_buffer(bp) {
		if (!b_is_invisible(bp) && b_is_changed(bp))
			cnt++;
	}
	return (cnt);
}

/*
 * Copies buffer-name to a null-terminated buffer (for use in code that
 * cannot conveniently use the name without a null-termination).
 */
char *
get_bname(bp)
BUFFER *bp;
{
	static	char	bname[NBUFN+1];
	strncpy(bname, bp->b_bname, NBUFN)[NBUFN] = EOS;
	return bname;
}

/*
 * Look for a buffer-name in the buffer-list
 */
BUFFER *
find_b_name(bname)
char *bname;
{
	register BUFFER *bp;

	for_each_buffer(bp)
		if (eql_bname(bp, bname))
			return bp;
	return 0;
}

/*
 * Find a buffer, by name. Return a pointer
 * to the BUFFER structure associated with it.
 * If the buffer is not found, create it. The "bflag" is
 * the settings for the flags in in buffer.
 */
BUFFER	*
bfind(bname, bflag)
int bflag;
char *bname;
{
	register BUFFER *bp;
	fast_ptr LINEPTR lp;
	register BUFFER *lastb = NULL;	/* buffer to insert after */

	for_each_buffer(bp) {
		if (eql_bname(bp, bname))
			return (bp);
		lastb = bp;
	}

	if ((bp = typealloc(BUFFER)) == NULL) {
		(void)no_memory("BUFFER");
		return (NULL);
	}
	/* set everything to 0's unless we want nonzero */
	(void)memset((char *)bp, 0, sizeof(BUFFER));

	/* set this first, to make it simple to trace */
	set_bname(bp, bname);

	lp = lalloc(0, bp);
	if (same_ptr(lp, null_ptr)) {
		free((char *) bp);
		(void)no_memory("BUFFER head");
		return (NULL);
	}

	/* and set up the other buffer fields */
	bp->b_active = FALSE;
	bp->b_dot.l  = lp;
	bp->b_dot.o  = 0;
	bp->b_wline  = bp->b_dot;
	bp->b_line   = bp->b_dot;
#if WINMARK
	bp->b_mark = nullmark;
#endif
	bp->b_lastdot = nullmark;
	bp->b_nmmarks = NULL;
	bp->b_flag  = bflag;
	bp->b_values = global_b_values;
	bp->b_wtraits.w_vals = global_w_values;
	bp->b_nwnd  = 0;
	bp->b_acount = b_val(bp, VAL_ASAVECNT);
	bp->b_fname = NULL;
	ch_fname(bp, "");
#if	CRYPT
	if (cryptkey != 0 && *cryptkey != EOS) {
		(void)strcpy(bp->b_key, cryptkey);
		make_local_b_val(bp, MDCRYPT);
		set_b_val(bp, MDCRYPT, TRUE);
	} else
		bp->b_key[0] = EOS;
#endif
	bp->b_udstks[0] = bp->b_udstks[1] = null_ptr;
#if OPT_MAP_MEMORY	/* _all_ pointers must be clean */
	bp->b_uddot[0] = bp->b_uddot[1] = nullmark;
#endif
	bp->b_udstkindx = 0;
	bp->b_ulinep = null_ptr;
	bp->b_last_used = 0;
	bp->b_udtail = null_ptr;
	bp->b_udlastsep = null_ptr;
	bp->b_udcount = 0;

	b_set_counted(bp);	/* buffer is empty */
	bp->b_bytecount = 0;
	bp->b_linecount = 0;
	set_lFORW(lp, lp);
	set_lBACK(lp, lp);

	/* append at the end */
	if (lastb)
		lastb->b_bufp = bp;
	else
		bheadp = bp;
	bp->b_bufp = NULL;
	bp->b_created = countBuffers();

	return (bp);
}

/*
 * This routine blows away all of the text
 * in a buffer. If the buffer is marked as changed
 * then we ask if it is ok to blow it away; this is
 * to save the user the grief of losing text. The
 * window chain is nearly always wrong if this gets
 * called; the caller must arrange for the updates
 * that are required. Return TRUE if everything
 * looks good.
 */
int
bclear(bp)
register BUFFER *bp;
{
	fast_ptr LINEPTR lp;

	if (!b_is_temporary(bp)		/* Not invisible or scratch */
	 &&  b_is_changed(bp)) {	/* Something changed	*/
		char ques[30+NBUFN];
		(void)strcat(strcpy(ques,"Discard changes to "), get_bname(bp));
		if (mlyesno(ques) != TRUE)
			return FALSE;
	}
	b_clr_changed(bp);		/* Not changed		*/
	freeundostacks(bp,TRUE);	/* do this before removing lines */
	while ((l_ref(lp=lFORW(bp->b_line.l))) != l_ref(bp->b_line.l)) {
		lremove(bp,lp);
		lfree(lp,bp);
	}

#if !OPT_MAP_MEMORY
	FreeAndNull(bp->b_ltext);
	bp->b_ltext_end = NULL;

	FreeAndNull(bp->b_LINEs);
	bp->b_LINEs_end = NULL;

	bp->b_freeLINEs = NULL;
#endif

	bp->b_dot  = bp->b_line;	/* Fix "."		*/
#if WINMARK
	bp->b_mark = nullmark;		/* Invalidate "mark"	*/
#endif
	bp->b_lastdot = nullmark;	/* Invalidate "mark"	*/
	FreeAndNull(bp->b_nmmarks);	/* free the named marks */

	b_set_counted(bp);
	bp->b_bytecount = 0;
	bp->b_linecount = 0;

	free_local_vals(b_valuenames, bp->b_values.bv);

	return (TRUE);
}

/*
 * Update the counts for the # of bytes and lines, to use in various display
 * commands.
 */
int
bsizes(bp)
BUFFER *bp;
{
	register LINE	*lp;		/* current line */
	register B_COUNT numchars = 0;	/* # of chars in file */
	register L_NUM   numlines = 0;	/* # of lines in file */

	if (b_is_counted(bp))
		return FALSE;

	/* count chars and lines */
	for_each_line(lp,bp) {
		++numlines;
		numchars += llength(lp) + 1;
#if !SMALLER	/* tradeoff between codesize & data */
		lp->l_number = numlines;
#endif
	}
	if (!b_val(bp,MDNEWLINE))
		numchars--;

	bp->b_bytecount = numchars;
	bp->b_linecount = numlines;
	b_set_counted(bp);
	return TRUE;
}

/*
 * Mark a buffer 'changed'
 */
void
chg_buff(bp, flag)
register BUFFER *bp;
register int	flag;
{
	register WINDOW *wp;

	b_clr_counted(bp);
	if (bp->b_nwnd != 1)		/* Ensure hard.		*/
		flag |= WFHARD;
	if (!b_is_changed(bp)) {	/* First change, so	*/
		flag |= WFMODE;		/* update mode lines.	*/
		b_set_changed(bp);
	}
#if	OPT_UPBUFF
	if (update_on_chg(bp))
		updatelistbuffers();
#endif
	for_each_window(wp)
		if (wp->w_bufp == bp) {
			wp->w_flag |= flag;
#ifdef WMDLINEWRAP
			/* The change may affect the line-height displayed
			 * on the screen.  Assume the worst-case.
			 */
			if ((flag & WFEDIT) && w_val(wp,WMDLINEWRAP))
				wp->w_flag |= WFHARD;
#endif
		}
}

/*
 * Mark a buffer 'unchanged'
 */
void
unchg_buff(bp, flag)
register BUFFER *bp;
register int	flag;
{
	register WINDOW *wp;

	if (b_is_changed(bp)) {
		if (bp->b_nwnd != 1)		/* Ensure hard.		*/
			flag |= WFHARD;
		flag |= WFMODE;			/* update mode lines.	*/
		b_clr_changed(bp);
		b_clr_counted(bp);

		for_each_window(wp) {
			if (wp->w_bufp == bp)
				wp->w_flag |= flag;
		}
#if	OPT_UPBUFF
		if (update_on_chg(bp))
			updatelistbuffers();
#endif
	}
}

/* ARGSUSED */
int
unmark(f, n)	/* unmark the current buffers change flag */
int f, n;	/* unused command arguments */
{
	unchg_buff(curbp, 0);
	return (TRUE);
}

/* write all _changed_ buffers */
int
writeall(f,n)
int f,n;
{
	register BUFFER *bp;	/* scanning pointer to buffers */
	register BUFFER *oldbp; /* original current buffer */
	register int status = TRUE;
	int count = 0;

	oldbp = curbp;				/* save in case we fail */

	for_each_buffer(bp) {
		if (b_is_changed(bp) && !b_is_invisible(bp)) {
			make_current(bp);
			mlforce("[Saving %s]",bp->b_fname);
			mlforce("\n");
			if ((status = filesave(f, n)) != TRUE)
				break;
			count++;
			mlforce("\n");
		}
	}
	if (count > 0 || status != TRUE) {
		make_current(oldbp);
		mlforce("\n");
	}
	if (status != TRUE) {
		pressreturn();
		sgarbf = TRUE;
	} else {
		if (count > 0) {
			sgarbf = TRUE;
			(void)update(TRUE);
		}
		mlforce("[Wrote %d buffer%s]",count, PLURAL(count));
	}
	return status;
}

/* For memory-leak testing (only!), releases all buffer storage. */
#if	NO_LEAKS
void	bp_leaks()
{
	register BUFFER *bp;

	while ((bp = bheadp) != 0) {
		b_clr_changed(bp);	/* discard any changes */
		bclear(bheadp);
		FreeBuffer(bheadp);
	}
}
#endif

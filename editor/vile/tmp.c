/*
 *	tmp.c
 *
 * Manages temporary file (or extended memory) to cache buffer data on systems
 * where main memory is not sufficient to hold large amounts of data.
 * Written by T.E.Dickey for vile (may 1993).
 *
 * We store the lines of all files in a cache of pages which are in-memory
 * when referenced with 'LINE *'.
 *
 * This is _not_ (yet) a recovery-file mechanism, nor does it store all dynamic
 * memory used by vile.  To make a recovery-file, we would have to store the
 * following data:
 *
 *	list of BUFFER structs
 *	buffer- and file-name associated with each BUFFER struct
 *	header-LINEPTR for each BUFFER struct.
 *
 * Dynamic memory not stored here includes
 *
 *	BUFFER structs
 *	WINDOW structs
 *	buffer- and window-mode arrays
 *	kill-buffers
 *
 * To do:
 *
 *	+ investigate whether it is feasible (or desirable) to add logic that
 *	  keeps track of modified pages so we don't write unmodified pages
 *	  during swapping.
 *
 *	+ improve temp-file page reuse by keeping track of pages that have
 *	  freespace (or have been entirely freed).  Currently, only pages that
 *	  are in-memory can have space allocated from them.
 *
 * $Log: tmp.c,v $
 * Revision 1.1  1994/02/01 03:29:40  jkh
 * Initial revision
 *
 * Revision 1.4  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.3  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.2  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.1  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.0  1993/05/11  16:26:18  pgf
 * Initial revision
 *
 */
#include "estruct.h"
#include "edef.h"

/*****************************************************************************
 *      Configuration Settings                                               *
 *****************************************************************************/

#undef	LP_TABLE
#undef	TESTING

#define TESTING 0		/* values: 0, 1,2,3 */

#define	MAX_PAGES       4	/* patch: maximum # in in-memory pages */

#define	NCHUNK          1024	/* size of page-buffers */

/*****************************************************************************
 *      Local types & data                                                   *
 *****************************************************************************/

#if OPT_MAP_MEMORY

#define	BAD_PAGE	-1

	/*
	 * Definitions for the maximum space and line-length we can have in
	 * a page-buffer.  The maximum space is forced to be a multiple of
	 * the FREE_T's size to simplify allocation.
	 */
#define	MAX_SPACE	((((NCHUNK - sizeof(PAGE_T)) / sizeof(FREE_T)) - 1) * sizeof(FREE_T))
#define	MAX_LINELEN	(MAX_SPACE - sizeof(LINE) - sizeof(FREE_T))

#define	for_each_page(p,q)	for (p = recent_pages, q = 0; p != 0; q = p, p = p->next)

#define	set_text(lp)	lp->l_text = (lp->l_size > 0) ? (char *)(lp+1) : 0

	/*
	 * We keep the FREE_T header before each allocated LINE struct to
	 * allow us to reclaim storage when a LINE is deallocated.
	 */
#define	Area2Line(a)	(LINE *)((a)+1)
#define	Line2Area(lp)	((FREE_T *)(lp)-1)
#define	Page2Line(p,o)	(LINE *)((long)p + o + sizeof(FREE_T))

	/*
	 * Linked-list type for inuse/freespace blocks within a page.
	 */
typedef	struct	free_t	{
	OFF_T	skip;		/* in-page pointer to next freespace */
	OFF_T	size;		/* size of this freespace (0 is end-mark) */
	} FREE_T;

	/*
	 * Linked-list type for pages holding LINE structs (and some freespace)
	 * The member 'space[]' points to two lists:
	 *	space[0] = offset to freespace
	 *	space[1] = offset to inuse-space
	 */
#define	PAGE_T	struct	buff_t
	PAGE_T	{
	PAGE_T	*next;		/* pointer to next in-memory page */
	BLK_T	block;		/* identifies this page */
	OFF_T	space[2];	/* offset of first freespace in page */
	};

#define	REGIONPTR	struct	regionptr
	REGIONPTR {
	REGIONPTR *next;
	REGION	*data;
	};

static	REGIONPTR *region_list;

static	PAGE_T	*recent_pages;	/* LRU list of pages of LINE structs */
static	BLK_T	intern_pagenum;	/* next page # to allocate */
static	BLK_T	extern_pagenum;	/* total # of pages written to temp-file */

static	FILE	*temp_fp;	/* page-swapping file */
static	char	*temp_is;	/* ...and its pathname */

static	int	count_truncated;/* counts truncated lines for warning message */
#endif

/*****************************************************************************
 *      Debugging (general)                                                  *
 *****************************************************************************/

#if TESTING
extern	void	WalkBack P(( void ));
extern	void	Trace P(( char *, ... ));

static	int	dumping;
	int	dump_line P(( char *, LINEPTR ));
	void	dumpBuffer P(( BUFFER * ));

#endif

#if TESTING || OPT_MAP_MEMORY
static	void	Oops P(( void ));

static	void
Oops()
{
#if TESTING
	WalkBack();
	Trace((char *)0);
#endif
	ttclean(TRUE);
	abort();
}
#endif

#if TESTING

/*----------------------------------------------------------------------------*/
int
dump_line(tag, p)
char	*tag;
LINEPTR	p;
{
	LINE	*q = l_ref(p);
	char	temp[80];

#if OPT_MAP_MEMORY && (TESTING > 2)
	(void)sprintf(temp, "%p\t%ld.%x", q, p.blk, p.off);
#else
	(void)sprintf(temp, "%p", q);
#endif

	if ((long)q > 10) {
		Trace("%s\t%20s %d:'%.*s'\n",
			temp, tag,
			llength(q),
			lisreal(q) ? llength(q) : 1,
			lisreal(q) ? q->l_text  : "");
	} else {
		Trace("%s\t%20s (null)\n",
			temp, tag);
		return FALSE;
	}
	return TRUE;
}

#if __STDC__
#define	dumpLine(p)	dump_line(#p,p)
#else
#define	dumpLine(p)	dump_line("",p)
#endif
#define	dumpMark(p)	dumpLine(p.l)

/*----------------------------------------------------------------------------*/
void
dumpBuffer(bp)
BUFFER	*bp;
{
	WINDOW	*wp;
	LINE	*p;
	LINEPTR	s;
	REGIONPTR *q;
	int	n;

	dumping = TRUE;
	Trace("dumpBuffer(%s) %d windows\n", get_bname(bp), bp->b_nwnd);
	if (bp->b_nwnd != 0) {	/* only show if displayed */
		(void)dumpMark(bp->b_dot);
#ifdef WINMARK
		(void)dumpMark(bp->b_mark);
#endif
		(void)dumpMark(bp->b_lastdot);
		(void)dumpMark(bp->b_wline);
	}

#ifndef WINMARK
	(void)dumpMark(MK);
#endif
	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			(void)dumpMark(wp->w_dot);
#ifdef WINMARK
			(void)dumpMark(wp->w_mark);
#endif
			(void)dumpMark(wp->w_lastdot);
			(void)dumpMark(wp->w_line);
		}
	}

	if (bp->b_nmmarks != 0)
		for (n = 0; n < 26; n++)
			(void)dumpMark(bp->b_nmmarks[n]);

	for (n = 0; n < 2; n++) {
		(void)dumpMark(bp->b_uddot[n]);

		(void)dumpLine(bp->b_udstks[n]);
		for (s = bp->b_udstks[n]; (p = l_ref(s)) != 0; s = p->l_nxtundo) {
			(void)dumpLine(p->l_fp);
			(void)dumpLine(p->l_bp);
			if (!dumpLine(p->l_nxtundo))	break;
		}
	}

	(void)dumpLine(bp->b_ulinep);
	for (s = bp->b_ulinep; (p = l_ref(s)) != 0; s = p->l_nxtundo) {
		(void)dumpLine(p->l_fp);
		(void)dumpLine(p->l_bp);
		if (!dumpLine(p->l_nxtundo))	break;
	}

	(void)dump_line("HEAD", bp->b_line.l);
	for_each_line(p,bp) {
		(void)dump_line("text", l_ptr(p));
	}

	for (q = region_list; q != 0; q = q->next) {
		(void)dumpMark(q->data->r_orig);
		(void)dumpMark(q->data->r_end);
	}
	dumping = FALSE;
}
#endif	/* TESTING */

/*****************************************************************************
 *      Local Definitions and Data                                           *
 *****************************************************************************/

#if OPT_MAP_MEMORY
static	BLK_T	MaxPages    P(( void ));
static	void	RecentPage  P(( PAGE_T * ));
static	void	WritePage   P(( PAGE_T * ));
static	void	ReadPage    P(( PAGE_T *, BLK_T ));
static	void	FlushPages  P(( PAGE_T * ));
static	LINE *	AdjustPtrs  P(( LINE *, LINE *, BUFFER * ));
static	OFF_T	SizeToSpace P(( SIZE_T ));
static	FREE_T *FirstSpace  P(( PAGE_T *, int ));
static	FREE_T *NextSpace   P(( FREE_T * ));
static	OFF_T	SpaceOffset P(( PAGE_T *, FREE_T * ));
static	void	ShrinkSpace P(( FREE_T *, OFF_T ));
static	void	AdjustSpace P(( PAGE_T *, FREE_T *, OFF_T, int ));
#if TESTING
static	LINE *	ValidateDst P(( char *, LINE * ));
static	LINE *	ValidateSrc P(( char *, LINE * ));
static	void	CheckSpace  P(( PAGE_T * ));
#else
#define	ValidateDst(tag,lp)	lp
#define	ValidateSrc(tag,lp)	lp
#define	CheckSpace(p)
#endif

/*****************************************************************************
 *      Debugging (map-memory)                                               *
 *****************************************************************************/

#if TESTING > 1
#define	TRACE1(s)	Trace s;
#else
#define	TRACE1(s)
#endif

#if TESTING > 2
#define	TRACE2(s)	Trace s;
#else
#define	TRACE2(s)
#endif

/*----------------------------------------------------------------------------*/

#if TESTING
static	char	*listnames[2] = {"free", "inuse"};

static	LINE *
ValidateDst(tag, lp)
char	*tag;
LINE	*lp;
{
	register PAGE_T *p;
	register FREE_T *f;
	register int	list;

	for (p = recent_pages; p != 0; p = p->next) {
		long	offset = (long)lp - (long)p;
		if (offset >= 0 && offset < NCHUNK) {
			for (f = FirstSpace(p,1); f != 0; f = NextSpace(f)) {
				if (lp == Page2Line(p,SpaceOffset(p,f)))
					return lp;
			}
			Trace("Cannot find line in %p .. %p (offset %lx)\n",
				p,
				(char *)((long)p + offset - 1),
				offset);
			for (list = 0; list < 2; list++) {
				Trace("%s:\n", listnames[list]);
				for (f = FirstSpace(p,list); f != 0; f = NextSpace(f)) {
					offset = SpaceOffset(p,f);
					Trace("\t%p (%x.%x)\n", f, f->skip, f->size);
					if (offset < 0 || offset > NCHUNK) {
						Trace("? error\n");
						break;
					}
				}
			}
			break;
		}
	}
	Trace("(%s) illegal value of lp:%p\n", tag, lp);
	if (!dumping)
		Oops();
	return lp;
}

static	LINE *
ValidateSrc(tag, lp)
char	*tag;
LINE	*lp;
{
	if (lp == (LINE *)0
	 || lp == (LINE *)1)	/* poison-value used in 'lfree()' */
		return lp;
	return ValidateDst(tag,lp);
}

static	void
CheckSpace (page)
PAGE_T	*page;
{
	register FREE_T	*temp;
	int	list;
	OFF_T	total = 0;

	TRACE2(("check PAGE %ld\n", page->block))
	for (list = 0; list < 2; list++) {
		TRACE2(("\t%s-list\n", listnames[list]))
		for (temp = FirstSpace(page, list); temp != 0; temp = NextSpace(temp)) {
			long	test = (long)temp - (long)page;
			if (test < 0
			 || test > NCHUNK) {
				Trace("bad space-pointer %p vs %p\n", temp, page);
				Oops();
			}
			if (temp->size < 0
			 || temp->size > NCHUNK
			 || temp->size % sizeof(FREE_T)) {
				Trace("bad size-value %x\n", temp->size);
				Oops();
			}
			if (temp->skip < 0
			 || temp->skip > NCHUNK) {
				Trace("bad skip-value %x\n", temp->skip);
				Oops();
			}
			TRACE2(("\t\t%p (%x.%x)\n", temp, temp->skip, temp->size))
			total += temp->size;
		}
	}
	if (total != MAX_SPACE) {
		Trace("bad total in page %ld is %x\n", page->block, total);
		Oops();
	}
	TRACE2(("\ttotal %#x\n", total))
}
#endif	/* TESTING */

/*****************************************************************************
 *      Local procedures                                                     *
 *****************************************************************************/

/*
 * The first time this is called, it determines how much memory we can
 * allocate (leaving a generous share for the non-line data).  It returns
 * the number of pages so that we can use this to limit the growth of the
 * page-list in memory.
 */
static BLK_T
MaxPages()
{
	static	BLK_T	the_limit;
	if (the_limit == 0) {
#if MSDOS
		extern long coreleft(void);
		the_limit = ((coreleft() / 3) * 2) / NCHUNK;
#else
		the_limit = MAX_PAGES;
#endif
	}
	return the_limit;
}

/*
 * Given the prev-pointer for the page, relink a page to the front of the
 * list of recent pages.
 */
static	void
RecentPage (p)
PAGE_T	*p;
{
	if (p != 0) {
		register PAGE_T	*q = p->next;
		p->next = q->next;
		q->next = recent_pages;
		recent_pages = q;
#if TESTING
		{ int	count = 0;
			TRACE2(("recent-page #%ld => %p\n", q->block, q))
			for (p = recent_pages; p != 0; p = p->next) {
				TRACE2(("... #%ld => %p\n", p->block, p))
				if (count++ > MaxPages())
					Oops();
			}
		}
#endif
	}
}

#define	PageOffset(number) ((number) * NCHUNK)

/*
 * Write the referenced page to the temporary file
 */
static	void
WritePage(this)
PAGE_T	*this;
{
	if (this->block+1 > extern_pagenum)
		extern_pagenum = this->block+1;

	TRACE1(("writing page #%ld:%ld =>%p\n", this->block, extern_pagenum, this))

	if (temp_fp == 0) {
		if ((temp_is = tempnam(TMPDIR, "vile")) == 0
		 || (temp_fp = fopen(temp_is, FOPEN_UPDATE)) == 0)
			Oops();
	}

	if (fseek(temp_fp, PageOffset(this->block), 0) < 0
	 || fwrite((char *)this, sizeof(char), NCHUNK, temp_fp) != NCHUNK) {
		Oops();
	}
}

/*
 * Read the referenced page (always into the least-recent position in the
 * page-list).
 */
static	void
ReadPage(this, number)
PAGE_T	*this;
BLK_T	number;
{
	register FREE_T	*p;

	TRACE1(("reading page #%ld:%ld =>%p\n", number, extern_pagenum, this))

	if (fseek(temp_fp, PageOffset(number), 0) < 0
	 || fread((char *)this, sizeof(char), NCHUNK, temp_fp) != NCHUNK) {
		Oops();
	}
	this->next = 0;
	for (p = FirstSpace(this,1); p != 0; p = NextSpace(p)) {
		register LINE	*lp = Page2Line(this,SpaceOffset(this,p));
		set_text(lp);
	}
}

/*
 * Write unwritten pages through the one referenced, so we can reuse it in a
 * new (or retrieved) page.  Note that the page-list is not in any particular
 * order, but that we must write initial blocks to the temp-file in order.
 */
static	void
FlushPages(thru)
PAGE_T	*thru;
{
	register PAGE_T	*p;

	while (extern_pagenum < thru->block) {
		for (p = recent_pages; p != 0; p = p->next) {
			if (p->block == extern_pagenum) {
				WritePage(p);
				break;
			}
		}
	}

	/* always write the final block, since we don't know if it changed */
	WritePage(thru);
}

/*----------------------------------------------------------------------------*/

/*
 * Adjust LINEPTR structs to reflect a reallocation.  This is necessary because
 * (unlike the normal configuration), we keep the LINE struct and its text
 * together.
 *
 * Use 'same_ptr()' for comparisons to avoid swapping unnecessarily, as well
 * as to avoid errors due to referencing obsolete (deallocated) pointers.
 */
#define	AdjustLine(ptr)	if (same_ptr(ptr, oldptr)) ptr = newptr
#define	AdjustMark(m)	AdjustLine(m.l)

static	LINE *
AdjustPtrs(oldp, newp, bp)
LINE	*oldp;
LINE	*newp;
BUFFER	*bp;
{
	register WINDOW	*wp;
	register int	n;
	LINEPTR	s;
	LINE	*p;
	LINEPTR	oldptr;
	LINEPTR	newptr;
	REGIONPTR *q;

	set_lforw(lback(oldp), newp);
	set_lback(lforw(oldp), newp);
	set_lforw(newp, lforw(oldp));
	set_lback(newp, lback(oldp));

	newptr = l_ptr(newp);
	oldptr = l_ptr(oldp);

	/* adjust pointers in buffer and its windows */
	AdjustMark(bp->b_dot);
#ifdef WINMARK
	AdjustMark(bp->b_mark);
#endif
	AdjustMark(bp->b_lastdot);
	AdjustMark(bp->b_wline);

#ifndef WINMARK
	AdjustMark(MK);
#endif
	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			AdjustMark(wp->w_dot);
#ifdef WINMARK
			AdjustMark(wp->w_mark);
#endif
			AdjustMark(wp->w_lastdot);
			AdjustMark(wp->w_line);
		}
	}

	/* adjust pointers in marks */
	if (bp->b_nmmarks != 0)
		for (n = 0; n < 26; n++)
			AdjustMark(bp->b_nmmarks[n]);

	/* adjust pointers in the undo-stacks */
	for (n = 0; n < 2; n++) {
		AdjustMark(bp->b_uddot[n]);

		AdjustLine(bp->b_udstks[n]);
		for (s = bp->b_udstks[n]; (p = l_ref(s)) != 0; s = p->l_nxtundo) {
			AdjustLine(p->l_fp);
			AdjustLine(p->l_bp);
			AdjustLine(p->l_nxtundo);
		}
	}

	AdjustLine(bp->b_ulinep);
	for (s = bp->b_ulinep; (p = l_ref(s)) != 0; s = p->l_nxtundo) {
		AdjustLine(p->l_fp);
		AdjustLine(p->l_bp);
		AdjustLine(p->l_nxtundo);
	}

	for (q = region_list; q != 0; q = q->next) {
		AdjustMark(q->data->r_orig);
		AdjustMark(q->data->r_end);
	}

	/* patch: MARK's in struct WHBLOCK? */

	return l_ref(newptr);	/* forces it back to most-recent-page */
}

/*----------------------------------------------------------------------------*/

/*
 * Convert a requested line-size to the total space required for storing in a
 * page-buffer.  Ensure that it is evenly divisible by the FREE_T struct so we
 * don't accumulate fragments.
 */
static	OFF_T
SizeToSpace (size)
SIZE_T	size;
{
	OFF_T	need = sizeof(LINE) + sizeof(FREE_T) + size;
	if (need & (sizeof(FREE_T)-1))
		need = (need | (sizeof(FREE_T)-1)) + 1;
	return need;
}

static	FREE_T *
FirstSpace (page, list)
PAGE_T	*page;
int	list;
{
	return (page->space[list] != 0) ? (FREE_T *)((long)page + page->space[list]) : 0;
}

static	FREE_T *
NextSpace (area)
FREE_T	*area;
{
	return (area->skip != 0) ? (FREE_T *)((long)area + area->skip) : 0;
}

static	OFF_T
SpaceOffset (page, area)
PAGE_T	*page;
FREE_T	*area;
{
	return (area != 0) ? (OFF_T)((long)area - (long)page) : 0;
}

static	void
ShrinkSpace (area, size)
FREE_T	*area;
OFF_T	size;
{
	if (area->skip != 0)
		area->skip -= size;	/* closer to the next space now */
	area->size -= size;		/* ...smaller by the same amount */
}

/*----------------------------------------------------------------------------*/

/*
 * Remove a given space-size at an area-pointer from the given list. Then,
 * add the area to the complementary list.
 */
static	void
AdjustSpace (page, area, size, list)
PAGE_T	*page;
FREE_T	*area;		/* area to remove or reduce */
OFF_T	size;		/* ...amount by which to reduce */
int	list;		/* ...list from which to remove the space */
{
	register FREE_T	*temp, *next;
	register FREE_T *first = FirstSpace(page, list);

	if (area == first) {
		if (area->size == size) {
			page->space[list] = SpaceOffset(page, NextSpace(area));
		} else {
			page->space[list] += size;
			*(temp = FirstSpace(page, list)) = *area;
			ShrinkSpace(temp, size);
		}
	} else {
		for (temp = first; temp != 0; temp = next) {
			next = NextSpace(temp);
			if (next == area) {
				if (area->size == size) {
					if (area->skip != 0)
						temp->skip += area->skip;
					else
						temp->skip = 0;
				} else {
					temp->skip += size;
					*(next = NextSpace(temp)) = *area;
					ShrinkSpace(next, size);
				}
				break;
			}
		}
	}

	/* add the area to the complementary list */
	list  = !list;
	first = FirstSpace(page, list);
	area->size = size;

	if (first == 0) {
		area->skip = 0;
		page->space[list] = SpaceOffset(page, area);
	} else {
		OFF_T	a = SpaceOffset(page, area),
			b = SpaceOffset(page, first);
		if (a < b) {
			/* link area before first */
			area->skip = b - a;
			page->space[list] = a;
		} else {
			for (temp = first; temp != 0; temp = next) {
				if ((next = NextSpace(temp)) == 0)
					break;
				if (SpaceOffset(page, next) > a)
					break;
			}
			/* link area after temp */
			if (next != 0)
				area->skip = SpaceOffset(page, next) - a;
			else
				area->skip = 0;
			temp->skip = a - SpaceOffset(page, temp);
		}
	}

	/* merge adjacent areas in the freespace list */
	if (!list) {
		for (temp = FirstSpace(page,0); temp != 0; temp = next) {
			next = NextSpace(temp);
			if (temp->skip != 0
			 && temp->skip == temp->size) {
				if (next->skip == 0)
					temp->skip = 0;
				else
					temp->skip += next->skip;
				temp->size += next->size;
				break;
			}
		}
	}
}

/*----------------------------------------------------------------------------*/

/* If we have opened the temp-file, close and remove it.
 */
void
tmp_cleanup()
{
	if (temp_fp != 0)
		(void)fclose(temp_fp);
	if (temp_is != 0)
		(void)unlink(temp_is);
}

/* Try to find an in-memory page with enough space to store the line.
 * If there is none, allocate a new page and return the line from that point.
 *
 */
LINEPTR
l_allocate(size)
SIZE_T	size;
{
#if TESTING > 1
	static	int	count;
#endif

	LINEPTR	ptr;
	OFF_T	need	= SizeToSpace(size);
	register PAGE_T	*this, *prev;
	register FREE_T	*q;
	register FREE_T	*q0 = 0;

	TRACE1(("\n** allocate %x #%d\n", size, ++count))
	if (size > MAX_LINELEN) {
		need = SizeToSpace(size = MAX_LINELEN);
		TRACE1(("...truncate to %x\n", size))
		count_truncated++;
	}

	/* allocate from the first in-memory page that has enough space */
	for_each_page(this, prev) {
		for (q = FirstSpace(this, 0); q != 0; q = NextSpace(q)) {
			if (q->size > need) {
				if (q0 != 0) {
					if (q0->size > q->size)
						q0 = q;
				} else
					q0 = q;
			}
		}
		if (q0 != 0 || this->next == 0)
			break;
	}

	ptr.blk = BAD_PAGE;
	ptr.off = 0;

	if (q0 == 0) {	/* allocate a new page */
		if (intern_pagenum >= MaxPages()) {
			FlushPages(this);
			RecentPage(prev);
		} else {
			if ((this = castalloc(PAGE_T, NCHUNK)) == 0)
				return ptr;
			this->next = recent_pages;
			recent_pages = this;
		}

		this->block = intern_pagenum++;
		this->space[0] = sizeof(PAGE_T);
		this->space[1] = 0;

		q0 = FirstSpace(this, 0);
		q0->skip = 0;
		q0->size = MAX_SPACE;

		TRACE1(("ALLOC-PAGE %ld =>%p (%p, %p) (%d:%d)\n",
			this->block, this, q0, q, q0->skip, q0->size))
		CheckSpace(this);
	}

	if (q0 != 0) {	/* de-link LINE from the page's freespace */
		LINE	*lp = Area2Line(q0);

		AdjustSpace(this, q0, need, 0);

		/* setup return-data */
		ptr.blk = this->block;
		ptr.off = SpaceOffset(this, q0);

		lp->l_size = size;
		set_text(lp);
		lp->l_fp   =
		lp->l_bp   =
		lp->l_nxtundo = null_ptr;

		TRACE1(("ALLOC-LINE %ld.%x (%x) =>%p\n", ptr.blk, ptr.off, size, lp))
		CheckSpace(this);
	} else
		Oops();
	return ptr;
}

/*----------------------------------------------------------------------------*/

void
l_deallocate(ptr)
LINEPTR	ptr;
{
	LINE	*lp = l_ref(ptr);	/* forces page-lookup */
	FREE_T	*f = Line2Area(ValidateDst("free",lp));
#if TESTING > 1
	static	int	count;
#endif
	TRACE1(("deallocate #%d %ld.%x %p\n", ++count, ptr.blk, ptr.off, lp))
	AdjustSpace(recent_pages, f, f->size, 1);
	CheckSpace(recent_pages);
}

/*----------------------------------------------------------------------------*/

/*
 * Reallocate a line buffer (to increase its size).
 * patch: may be able to make line grow into adjacent space
 */
LINE *
l_reallocate(lp, size, bp)
LINEPTR	lp;
SIZE_T	size;
BUFFER	*bp;
{
	LINE	*oldp	= l_ref(lp),
		*newp;

	if (size > MAX_LINELEN) {
		size = MAX_LINELEN;
		count_truncated++;
	}

	if (size > oldp->l_size)
		newp = l_ref(l_allocate(size));
	else
		newp = oldp;

	TRACE1(("reallocate %p to %p (%x bytes)\n", oldp, newp, size))
	if (newp != 0 && newp != oldp) {
		*newp = *oldp;	/* copy everything that we don't change */
		newp->l_size = size;
		set_text(newp);
		if (oldp->l_text != 0
		 && newp->l_text != 0
		 && oldp->l_used >= 1)
			(void)memcpy(newp->l_text, oldp->l_text, (SIZE_T)oldp->l_used);

		(void)AdjustPtrs(oldp, newp, bp);

		/* finally, get rid of the old line-struct */
		l_deallocate(lp);
	}
	return newp;
}

/*
 * Returns the number of lines truncated (i.e., allocation requests longer than
 * MAX_LINELEN) since the last call.
 */
int
l_truncated()
{
	int	it = count_truncated;
	count_truncated = 0;
	return it;
}

/*
 * Maintains a list of pointers to REGION structs to allow adjustment of the
 * corresponding LINEPTR's in 'AdjustPtrs()'.
 */
void
l_region(rp)
REGION	*rp;
{
	register REGIONPTR *p;

	if (rp != 0) {
		if ((p = typealloc(REGIONPTR)) != 0) {
			p->next = region_list;
			p->data = rp;
			region_list = p;
		}
	} else {	/* pop */
		if ((p = region_list) != 0) {
			region_list = p->next;
			free((char *)p);
		}
	}
}

/*
 * Ensure that the LINE referenced by the argument is in memory, return the
 * pointer to it.  If it isn't in memory, read it in and make it the most
 * recent page.
 */
LINE *
l_ref (ptr)
LINEPTR	ptr;
{
	register LINE	*lp;

	if (ptr.blk == BAD_PAGE) {
		lp = (LINE *)(ptr.off);
		return ValidateSrc("ref", lp);
	} else {
		register PAGE_T	*this, *prev;
		register int	found = FALSE;

		lp = 0;	/* make the compiler shut up */
		for_each_page(this,prev) {
			if (this->block == ptr.blk) {
				lp = Page2Line(this, ptr.off);
				TRACE2(("l_ref(%ld.%x) = %p\n", ptr.blk, ptr.off, lp))
				found = TRUE;
				break;
			} else if (this->next == 0)
				break;	/* ...setup for page-read */
		}

		if (found) {
			;
		} else if (ptr.blk < extern_pagenum) {
			FlushPages(this);
			ReadPage(this, ptr.blk);
			lp = Page2Line(this, ptr.off);
		} else {
			TRACE1(("Cannot find pointer (%ld.%x)\n", ptr.blk, ptr.off))
			Oops();
		}
		RecentPage(prev);
		return ValidateDst("ref", lp);
	}
}

LINEPTR
l_ptr (lp)
LINE *	lp;
{
	register PAGE_T *this, *prev;
	register int	found = FALSE;
	LINEPTR	ptr;

#if TESTING
	(void)ValidateSrc("ptr", lp);
#endif
	if (lp == (LINE *)0
	 || lp == (LINE *)1) {
		ptr.blk = BAD_PAGE;
		ptr.off = (OFF_T)lp;
		found = TRUE;
	} else {
		for_each_page(this,prev) {
			long	off = ((long)lp - (long)this) - sizeof(FREE_T);
			if (off >= sizeof(PAGE_T) && (off < NCHUNK)) {
				RecentPage(prev);
				ptr.blk = this->block;
				ptr.off = (OFF_T)off;
				TRACE2(("l_ptr(%p) = %ld.%x\n", lp, ptr.blk, ptr.off))
				found = TRUE;
				break;
			}
		}
	}

	if (!found) {
		TRACE1(("Cannot find line %p\n", lp))
		Oops();
		/*NOTREACHED*/
	}
	return ptr;
}

void
set_lforw (dst, src)
LINE	*dst;
LINE	*src;
{
	ValidateDst("set",dst)->l_fp = l_ptr(src);
}

void
set_lback (dst, src)
LINE	*dst;
LINE	*src;
{
	ValidateDst("set",dst)->l_bp = l_ptr(src);
}

LINE *
lforw (lp)
LINE	*lp;
{
	return l_ref(ValidateDst("get",lp)->l_fp);
}

LINE *
lback (lp)
LINE	*lp;
{
	return l_ref(ValidateDst("get",lp)->l_bp);
}

void
lsetclear (lp)
LINE	*lp;
{
#if TESTING
	(void)ValidateDst("flag", lp);
#endif
	lp->l_nxtundo = null_ptr;
	lp->l.l_flag  = 0;
}

/*****************************************************************************
 *      Variations on the LINE/LINEPTR translations                          *
 *****************************************************************************/

LINE *
lforw_p2r (lp)
LINEPTR lp;
{
	return lforw(l_ref(lp));
}

LINE *
lback_p2r (lp)
LINEPTR lp;
{
	return lback(l_ref(lp));
}

LINEPTR
lforw_p2p (lp)
LINEPTR lp;
{
	return ValidateDst("get",l_ref(lp))->l_fp;
}

LINEPTR
lback_p2p (lp)
LINEPTR lp;
{
	return ValidateDst("get",l_ref(lp))->l_bp;
}

void
set_lforw_p2r (dst, src)
LINE *	dst;
LINEPTR src;
{
	ValidateDst("set",dst)->l_fp = src;
}

void
set_lback_p2r (dst, src)
LINE *	dst;
LINEPTR src;
{
	ValidateDst("set",dst)->l_bp = src;
}

void
set_lforw_p2p (dst, src)
LINEPTR	dst;
LINEPTR src;
{
	set_lforw_p2r (l_ref(dst), src);
}

void
set_lback_p2p (dst, src)
LINEPTR	dst;
LINEPTR src;
{
	set_lback_p2r (l_ref(dst), src);
}

#endif	/* OPT_MAP_MEMORY */


/* For memory-leak testing (only!), releases all tmp-buffer storage. */
#if NO_LEAKS
void
tmp_leaks()
{
#if OPT_MAP_MEMORY
	register PAGE_T	*p;
	while ((p = recent_pages) != 0) {
		p = p->next;
		free((char *)recent_pages);
		recent_pages = p;
	}
#endif
}
#endif

/*
 *	tbuff.c
 *
 *	Manage dynamic temporary buffers.
 *	Note that some temp-buffs are never freed, for speed
 *
 *	To do:	add 'tb_ins()' and 'tb_del()' to support cursor-level command
 *		editing.
 *
 * $Log: tbuff.c,v $
 * Revision 1.1  1994/02/01 03:29:39  jkh
 * Initial revision
 *
 * Revision 1.10  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.9  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.8  1993/04/08  09:49:08  pgf
 * added tb_stuff.c
 *
 * Revision 1.7  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.6  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.5  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.4  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.3  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.2  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.1  1993/02/08  14:51:46  pgf
 * Initial revision
 *
 */

#include "estruct.h"
#include "edef.h"

#define	NCHUNK	NLINE

/*******(testing)************************************************************/
#if NO_LEAKS
typedef	struct	_tb_list	{
	struct	_tb_list	*link;
	TBUFF			*buff;
	} TB_LIST;

static	TB_LIST	*all_tbuffs;

#define	AllocatedBuffer(q)	tb_remember(q);
#define	FreedBuffer(q)		tb_forget(q);

static	void	tb_remember P(( TBUFF * ));
static	void	tb_forget P(( TBUFF * ));

static
void	tb_remember(p)
	TBUFF	*p;
{
	register TB_LIST *q = typealloc(TB_LIST);
	q->buff = p;
	q->link = all_tbuffs;
	all_tbuffs = q;
}

static
void	tb_forget(p)
	TBUFF	*p;
{
	register TB_LIST *q, *r;

	for (q = all_tbuffs, r = 0; q != 0; r = q, q = q->link)
		if (q->buff == p) {
			if (r != 0)
				r->link = q->link;
			else
				all_tbuffs = q->link;
			free((char *)q);
			break;
		}
}

void	tb_leaks()
{
	while (all_tbuffs != 0) {
		TBUFF	*q = all_tbuffs->buff;
		tb_free(&q);
		FreedBuffer(q);
	}
}

#else
#define	AllocatedBuffer(q)
#define	FreedBuffer(q)
#endif

/*******(initialization)*****************************************************/

/*
 * ensure that the given temp-buff has as much space as specified
 */
TBUFF *	tb_alloc(p, n)
	TBUFF	**p;
	ALLOC_T	n;
{
	register TBUFF *q = *p;
	if (q == 0) {
		q = *p = typealloc(TBUFF);
		q->tb_data = typeallocn(char, q->tb_size = n);
		q->tb_used = 0;
		q->tb_endc = abortc;
		AllocatedBuffer(q)
	} else if (n >= q->tb_size) {
		q->tb_data = typereallocn(char, q->tb_data, q->tb_size = (n*2));
	}
	return q;
}

/*
 * (re)initialize a temp-buff
 */
TBUFF *	tb_init(p, c)
	TBUFF	**p;
	int	c;		/* code to return if no-more-data */
{
	register TBUFF *q = *p;
	if (q == 0)
		q = tb_alloc(p, NCHUNK);
	q->tb_used = 0;
	q->tb_last = 0;
	q->tb_endc = c;
	return (*p = q);
}

/*
 * deallocate a temp-buff
 */
void	tb_free(p)
	TBUFF	**p;
{
	register TBUFF *q = *p;

	if (q != 0) {
		free(q->tb_data);
		free((char *)q);
		FreedBuffer(q)
	}
	*p = 0;
}

/*******(storage)************************************************************/

/*
 * put a character c at the nth position of the temp-buff
 */
TBUFF *	tb_put(p, n, c)
	TBUFF	**p;
	ALLOC_T	n;
	int	c;
{
	register TBUFF *q;

	if ((q = tb_alloc(p, n+1)) != 0) {
		q->tb_data[n] = c;
		q->tb_used = n+1;
	}
	return q;
}

/*
 * stuff the nth character into the temp-buff -- assumes space already there
 *  it's sort of the opposite of tb_peek
 */
void	tb_stuff(p, c)
	TBUFF	*p;
	int	c;
{
	if (p->tb_last < p->tb_used)
		p->tb_data[p->tb_last] = c;
	else
		p->tb_endc = c;
}
/*
 * append a character to the temp-buff
 */
TBUFF *	tb_append(p, c)
	TBUFF	**p;
	int	c;
{
	register TBUFF *q = *p;
	register ALLOC_T n = (q != 0) ? q->tb_used : 0;
	
	return tb_put(p, n, c);
}

/*
 * Copy one temp-buff to another
 */
TBUFF *	tb_copy(d, s)
	TBUFF	**d;
	TBUFF	*s;
{
	register TBUFF *p;

	if (s != 0) {
		if ((p = tb_init(d, s->tb_endc)) != 0)
			p = tb_bappend(d, s->tb_data, s->tb_used);
	} else
		p = tb_init(d, abortc);
	return p;
}

/*
 * append a binary data to the temp-buff
 */
TBUFF *	tb_bappend(p, s, len)
	TBUFF	**p;
	char	*s;
	ALLOC_T len;
{
	while ((len-- != 0) && tb_append(p, (int)(*s++)) != 0)
		;
	return *p;
}
/*
 * append a string to the temp-buff
 */
TBUFF *	tb_sappend(p, s)
	TBUFF	**p;
	char	*s;
{
	if (!s)
		s = "";
	return tb_bappend(p, s, (ALLOC_T)strlen(s));
}

/*
 * copy a string to the temp-buff, including a null
 */
TBUFF *	tb_scopy(p, s)
	TBUFF	**p;
	char	*s;
{
	(void) tb_init(p, EOS);
	(void) tb_sappend(p, s);
	return tb_append(p, EOS);
}

/*******(retrieval)************************************************************/

/*
 * get the nth character from the temp-buff
 */
int	tb_get(p, n)
	TBUFF	*p;
	ALLOC_T	n;
{
	register int	c = abortc;

	if (p != 0)
		c = (n < p->tb_used) ? p->tb_data[n] : p->tb_endc;

	return (c >= 128) ? (c-256) : c;	/* sign-extend */
	/* patch, should be char2int(c) */
}

/*
 * undo the last 'tb_put'
 */
void	tb_unput(p)
	TBUFF	*p;
{
	if (p != 0
	 && p->tb_used != 0)
		p->tb_used -= 1;
}

/*******(iterators)************************************************************/

/*
 * Reset the iteration-count
 */
void	tb_first(p)
	TBUFF	*p;
{
	if (p != 0)
		p->tb_last = 0;
}

/*
 * Returns true iff the iteration-count has not gone past the end of temp-buff.
 */
int	tb_more(p)
	TBUFF	*p;
{
	return (p != 0) ? (p->tb_last < p->tb_used) : FALSE;
}

/*
 * get the next character from the temp-buff
 */
int	tb_next(p)
	TBUFF	*p;
{
	if (p != 0)
		return tb_get(p, p->tb_last++);
	return abortc;
}

/*
 * get the next character from the temp-buff w/o incrementing index
 */
int	tb_peek(p)
	TBUFF	*p;
{
	if (p != 0)
		return tb_get(p, p->tb_last);
	return abortc;
}

/*******(bulk-data)************************************************************/

/*
 * returns a pointer to data, assumes it is one long string
 */
char *	tb_values(p)
	TBUFF	*p;
{
	return (p != 0) ? p->tb_data : 0;
}

/*
 * returns the length of the data
 */
ALLOC_T tb_length(p)
	TBUFF	*p;
{
	return (p != 0) ? p->tb_used : 0;
}

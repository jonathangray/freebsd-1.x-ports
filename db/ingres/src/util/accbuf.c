#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include <aux.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)accbuf.c	8.1	12/31/84)


/*
**	access method buffers and other data areas for buffer maintenance
*/

accbuf_t	Acc_buf[NACCBUFS];	/* the buffers */
accbuf_t	*Acc_head;		/* head of usage list */
accbuf_t	*Acc_tail;		/* tail of usage list */
lock_req_t	Lock;

/*
**	structs for admin file data
*/

admin_t	Admin;

/*
**	global flag indicating if access methods
**	have been initialized.
*/

int		Acc_init	=	FALSE;

char		Acclock;		/* locks enabled flag */
extern int	Alockdes;		/* file descriptor for lock device*/
int		Lockrel;		/* lock relations flag*/

/*
**	Flush the indicated page and reset all
**	important information including the name
**
**	Trace Flags:
**		20.0,1
*/
int
resetacc(accbuf_t *ap)
{
	register int		i;
	long			lvar;

	if (ap == 0) {
		ap = Acc_head;
	}
#ifdef xATR3
	if (tTf(20, 0)) {
		printf("RESETACC: %p=", ap);
		dumptid(&ap->am_rel);
	}
#endif
	i = pageflush(ap);	/* write the page if necessary */
	lvar = -1;
	(void) memcpy(&ap->am_rel, &lvar, sizeof(ap->am_rel));
	ap->am_fd = -1;
	ap->am_curpg = -1;
	ap->am_flags = 0;
	return (i);
}

/*
**	initialize access method data areas
**
**	Trace Flags:
**		20.2,3
*/
void
acc_init(int nocheck, int fake)
{
	register accbuf_t	*last;
	register accbuf_t	*b;
	struct stat		s;

#ifdef xATR3
	if (tTf(20, 2))
		printf("ACC_INIT=%d\n", Acc_init);
#endif

	if (Acc_init) {
		return;		/* already initialized */
	}
	last = 0;
	for (b = Acc_buf; b < &Acc_buf[NACCBUFS]; ) {
		resetacc(b);
		b->am_prev = last;
		last = b;
		b++;
		last->am_next = b;
	}
	last->am_next = 0;
	Acc_head = Acc_buf;
	Acc_tail = last;

	/* get the admin file */
	readadmin(fake);

	/*
	** Set up locking information. If the database has concurrency
	** control then Lockrel = TRUE and the concurrency device will
	** be opened for writing. If there is no concurrency for the
	** data base or if the lock device isn't installed, then Alockdes
	** = -1 and no locking will (or can) occure.
	*/
	Lockrel = (Admin.ad_h.adm_flags & A_DBCONCUR) != 0;
	if (Lockrel && Alockdes < 0) {
		Alockdes = start_up_lock_driver();
	}
	errno = 0;	/* clear in case /dev/lock isn't available */
	Acclock = TRUE;
	stat(".", &s);
	/* XXX - agc */
	bmove((char *) &s, (char *) Lock.dbnode, sizeof(Lock.dbnode));
	Acc_init = TRUE;
}

/*
**	place buffer at top of LRU list
*/
int
top_acc(accbuf_t *buf)
{
	register accbuf_t	*b;

	b = buf;

	if (b == Acc_head)
		return (0);
	if (b == Acc_tail)
		Acc_tail = b->am_prev;
	else
		b->am_next->am_prev = b->am_prev;
	b->am_prev->am_next = b->am_next;
	Acc_head->am_prev = b;
	b->am_next = Acc_head;
	Acc_head = b;
	b->am_prev = 0;
	return (0);
}

/*
** Flush_rel -- flush all pages associated with the relation
**	described by the descriptor. If resetflag is TRUE,
**	then the buffers are reset so the pages will not be
**	found on subsequent calls to find_page().
**
**	Returns "or'ed" result from calls to pageflush.
**
**	Trace Flags:
**		20.4-5
*/
int
flush_rel(register desc_t *d, int resetflag)
{
	register accbuf_t	*b;
	register int		i;

#ifdef xATR3
	if (tTf(20, 4))
		printf("flush_rel: rel=%.14s, reset=%d\n", d->d_r.r_id, resetflag);
#endif

	i = 0;
	for (b = Acc_head; b != NULL; b = b->am_next) {
		if (memcmp(&d->d_tid, &b->am_rel, sizeof(d->d_tid)) == 0) {
			if (resetflag)
				i |= resetacc(b);
			else
				i |= pageflush(b);
		}
	}
	return (i);
}

/*
**	CHOOSE_BUF -- Try to find an empty buffer for assignment.
**		If there is no empty buffer, pick the last buffer
**		in the LRU queue and make sure it is flushed.
**
**		Choose_buf guarantees that the buffer will be reset
**		if it was used previously for a different relation.
**
**	Choose_buf -- choose a buffer for use with the given relation on
**	the given page. The current algorithm is to allow only one buffer
**	per relation. If a relation does not have a buffer, it is given a
**	free one (if any) or else the Least Recently Used.
**
**	Trace Flags:
**		29.0,1
*/

accbuf_t *
choose_buf(desc_t *dx, long pageid)
{
	register accbuf_t	*b, *freeb;
	register desc_t		*d;
	accbuf_t		*mine;
	long			lvar;

	d = dx;
	freeb = mine = NULL;

	for (b = Acc_head; b != 0; b = b->am_next) {
		lvar = -1;
		if (memcmp(&b->am_rel, &lvar, sizeof(b->am_rel)) == 0) {
			freeb = b;
		} else if (memcmp(&d->d_tid, &b->am_rel, sizeof(d->d_tid)) == 0) {
			if (pageid == b->am_curpg) {
				if (d->d_opened < 0)
					b->am_fd = d->d_fd;
				return (b);
			}
			mine = b;
		}
	}

	/*
	** "Free" and "Mine" now reflect the current state of the buffers.
	** There is no buffer with the currently requested page
	*/

#ifdef xATR3
	if (tTf(29, 1))
		printf("choosebuf freeb %p,mine %p\n", freeb, mine);
#endif

	/* no current buffer. Choose a free one or LRU */
	if (freeb == NULL) {
		/* error if can't reset the LRU */
		freeb = resetacc(Acc_tail) ? NULL : Acc_tail;
	}
	if (freeb) {
		/* copy relevant material (in this order in case of rubout) */
		freeb->am_fd = d->d_fd;
		memcpy(&freeb->am_rel, &d->d_tid, sizeof(d->d_tid));
	}

#ifdef xATR1
	if (tTf(29, 0))
		printf("choosebuf:rets %p\n", freeb);
#endif
	return (freeb);
}

/*
**	ACC_CLOSE -- flush any buffers left around
**		and then close the files for relation & attribute.
**		The relation and attribute relation are normally left open
**		until the end of an INGRES session but must be closed
**		and re-opened in the dbu's whenever a new overlay is loaded.
*/
int
acc_close(void)
{
	register int	i;

	if ((i = pageflush((accbuf_t *) NULL)) != 0) {
		syserr("acc_close: pageflush %d", i);
	}
	close(Admin.ad_rel.d_fd);
	close(Admin.ad_attr.d_fd);
	Admin.ad_rel.d_opened = Admin.ad_attr.d_opened = 0;
	if (Alockdes >= 0) {
		close(Alockdes);
	}
	Alockdes = -1;
	Acc_init = FALSE;
	return(0);
}

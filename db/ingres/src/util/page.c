#include <sys/types.h>

#include <stdio.h>

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

SCCSID(@(#)page.c	8.3	2/8/85)



/*
**	UNIX read/write counters
*/

long	Accuread, Accuwrite;
long	Accusread;

/*
**	GETPAGE - get the page on which tid is found
**
**
**	Parameters:
**		d - descriptor for relation
**		tid - tid which specifies the page
**
**	Return Codes:
**		0 - success		
**		-1, -2 - AMREAD_ERR, AMWRITE_ERR		
**
**	Trace Flags:
**		26.9, 26.10
**
**	Called by:
**		fill_rel(), add_ovflo(), bndxsearch(), delete(),
**		get(), getequal(), ndxsearch(), noclose(), replace()
**		scan_dups()
**
*/
int
get_page(desc_t *d, tid_t *tid)
{
	register int		i;
	long			pageid;
	register accbuf_t	*b;
	struct accbuf		*b1;
	int			lk;		/* lock condition*/

#ifdef xATR3
	if (tTf(26, 9)) {
		printf("GET_PAGE: %.14s,", d->d_r.r_id);
		dumptid(tid);
	}
#endif

	pluck_page(tid, &pageid);
	if ((b = choose_buf(d, pageid)) == NULL)
		return (-1);
	top_acc(b);
	lk = Acclock && (d->d_r.r_status & S_CONCUR) && (d->d_opened < 0);
	if ((b->am_curpg != pageid) || (lk && !(b->am_flags & BUF_LOCKED))) {
		if ((i = pageflush(b)) != 0) {
			return (i);
		}

#ifdef xATR1
		if (tTf(26, 10)) {
			printf("GET_PAGE: rdg pg %ld", pageid);
			printf(",relid ");
			dumptid((tid_t *) &d->d_tid);
		}
#endif
		b->am_curpg = pageid;
		if (lk) {
			b1 = Acc_head;
			for (; b1 != 0; b1 = b1->am_next) {
				if (b1->am_flags & BUF_LOCKED) {
					pageflush(b1);  /*  */
				}
			}
			if (setpgl(b) < 0) {
				syserr("get-page: lk err");
			}
		}
		if ((lseek(d->d_fd, (off_t)(pageid * PGSIZE), 0) == -1) ||
		    (read(d->d_fd, (char *) b, PGSIZE) != PGSIZE)) {
			resetacc(b);
			return (acc_err(AMREAD_ERR));
		}
		Accuread++;
		if (d->d_r.r_status & S_CATALOG) {
			Accusread++;
		}
	}
	return (0);
}

/*
**	PAGEFLUSH - flush the buffered page to disk
**
**	Parameters:
**		buf - the buffer
**
**	Return Codes:
**		0 -- successful
**		AMWRITE_ERR - error in writing
**
**	Trace Flags:
**		29.2, 29.3
**
*/
int
pageflush(accbuf_t *ap)
{
	register int	allbufs;
	int		err;

#ifdef xATR3
	if (tTf(29, 2)) {
		printf("PAGEFLUSH: %p=", ap);
		if (ap == NULL) {
			printf("all\n");
		} else {
			dumptid(&ap->am_rel);
		}
	}
#endif
	err = FALSE;
	allbufs = FALSE;
	if (ap == (accbuf_t *) NULL) {
		ap = Acc_buf;
		allbufs = TRUE;
	}
	do {
		if (ap->am_flags & BUF_DIRTY) {
#ifdef xATR1
			if (tTf(29, 3)) {
				printf("PAGEFLUSH: wr pg %ld", ap->am_curpg);
				printf(",stat %d,relid ", ap->am_flags);
				dumptid(&ap->am_rel);
			}
#endif
			ap->am_flags &= ~BUF_DIRTY;
			if ((lseek(ap->am_fd, (off_t)(ap->am_curpg * PGSIZE), 0)== -1) ||
			    (write(ap->am_fd, (char *) ap, PGSIZE) != PGSIZE)) {
				resetacc(ap);
				err = TRUE;
			}
			Accuwrite++;

		}
		if (Acclock && ap->am_flags & BUF_LOCKED) {
			unlpg(ap);
		}
	} while (allbufs && (ap = ap->am_next) != NULL);
	return((err) ? acc_err(AMWRITE_ERR) : 0);
}

/*
**  ACC_ERR -- set global error indicator "Accerror"
**
**	Trace Flags:
**		20.4-5
*/
int
acc_err(int errnum)
{
	Accerror = errnum;
#ifdef xATR1
	tTfp(20, 4, "ACC_ERR: %d\n", Accerror);
#endif
	return (Accerror);
}

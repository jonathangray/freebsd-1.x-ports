#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)formatpg.c	8.2	2/8/85)

/*
**	FORMATPG - write n pages to a buffer
**
**
**	Parameters:
**		d - descriptor
**		n - number of pages
**
**
**	Return Codes:
**		0 - successful
**		-2 - failure when setting file ptr
**		-3, -4 - failure when writing page to buffer
**			-4 is for last page
**
**	Called by:
**		create()
**		modify()
**		resetrel()
**
*/
int
formatpg(desc_t *d, long n)
{
	accbuf_t	buf;
	register char	*p;

	if (Acc_head == 0) {
		acc_init(0, 0);
	}
	if (lseek(d->d_fd, (off_t) 0, 0) == -1) {
		return (-2);
	}
	(void) memcpy(&buf.am_rel, &d->d_tid, sizeof(d->d_tid));
	buf.am_fd = d->d_fd;
	for (p = (char *) &buf; p <= (char *) buf.am_linev; p++) {
		*p = 0;
	}
	buf.am_nextline = 0;
	buf.am_linev[0] = (int) buf.am_tup1 - (int) &buf;
	buf.am_overflowpg = 0;
	for (buf.am_mainpg = 1; buf.am_mainpg < n; (buf.am_mainpg)++) {
		if (write(buf.am_fd, (char *) &buf, PGSIZE) != PGSIZE) {
			return (-3);
		}
	}
	buf.am_mainpg = 0;
	if (write(buf.am_fd, (char *) &buf, PGSIZE) != PGSIZE) {
		return (-4);
	}
	Accuwrite += n;
	return (0);
}

#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)acc_addbuf.c	8.2	2/8/85)

/*
**	ACC_ADDBUF - add access buffers to the usage list
**
**	Parameters:
**		bufs - buffers to add
**		cnt -  number of buffers to add
**
**	Side Effects:
**		Buffers are added to the list pointed
**		to by Acc_tail
**
**	Called by:
**		init_decomp()
**
*/
void
acc_addbuf(accbuf_t *bufs, int cnt)
{
	register accbuf_t	*b, *end;

	b = bufs;
	end = &b[cnt -1];
	acc_init(0, 0);

	for ( ; b <= end; b++) {
		b->am_flags = 0;
		resetacc(b);
		Acc_tail->am_next = b;
		b->am_prev = Acc_tail;
		b->am_next = NULL;
		Acc_tail = b;
	}
}

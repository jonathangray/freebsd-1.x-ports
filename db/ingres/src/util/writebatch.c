#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)writebatch.c	8.1	12/31/84)

/*
**  FLUSHBATCH -- flush batch file
*/

void
flushbatch(void)
{
	register int	i;

	if (Batch_cnt) {
#ifdef xATR1
		if (tTf(25, 9))
			printf("flushing %d\n", Batch_cnt + IDSIZE);
#endif
		if ((i = write(Batch_fp, &Batchbuf, Batch_cnt + IDSIZE)) != Batch_cnt + IDSIZE)
			syserr("flushbatch:can't write %d", i);
		Batch_cnt = 0;
	}
}

/*
**  WRBATCH -- write batch file
*/
void
wrbatch(char *cp, int count)
{
	register char	*c;
	register int	size, cnt;

	cnt = count;
	c = cp;
#ifdef xATR1
	if (tTf(25, 8))
		printf("wrbatch:%d (%d)\n", cnt, Batch_cnt);
#endif

	while (cnt) {
		Batch_dirty = TRUE;	/* mark this buffer as dirty */
		if (cnt + Batch_cnt > BATCHSIZE) {
			size = BATCHSIZE - Batch_cnt;
		} else {
			size = cnt;
		}
		bmove(c, &Batchbuf.b_buf[Batch_cnt], size);
		c += size;
		Batch_cnt += size;
		cnt -= size;
		if (Batch_cnt == BATCHSIZE) {
			flushbatch();
		}
	}
}

#include <sys/types.h>

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)rdwrbatch.c	8.2	2/23/85)


/*
**	READBATCH - read the batch file
**
**	Return Codes:
**		returns number of bytes read
**
**	Trace Flags:
**		Z42.10
**
**	Called by:
**		update()
**
*/
int
readbatch(void)
{

	if ((Batch_lread = read(Batch_fp, &Batchbuf, BATCHSIZE+IDSIZE)) < 0) {
		syserr("readbatch:can't read %d %d", Batch_lread, Batch_fp);
	}
	Batch_cnt = 0;
#ifdef xZTR1
	if (tTf(42, 10))
		printf("read %d bytes from batch\n", Batch_lread);
#endif
	/* check file-id */
	if (strcmp(Fileset, Batchbuf.b_file) != 0) {
		syserr("readbatch:bad id '%s' '%.20s' %d", Fileset, Batchbuf.b_file, Batch_lread);
	}
	return (Batch_lread);
}

/*
**	BATCHFLUSH - flush the batch file
**
**	Return Codes:
**			0
**
**	Trace Flags:
**		Z42.4, Z42.5
**
**	Side Effects:
**		Batch_dirty gets FALSE
**
**	Called by:
**		update
**		getbatch
*/
void
batchflush(void)
{
	register int	i;
	if (Batch_cnt && Batch_dirty) {
#ifdef xZTR1
		if (tTf(42, 5))
			printf("flush:backing up %d\n", Batch_lread);
#endif
		if ((i = lseek(Batch_fp, (off_t) -Batch_lread, 1)) < 0)
			syserr("batchflush:can't seek %d", Batch_lread);
#ifdef xZTR1
		if (tTf(42, 4))
			printf("flushing %d\n", Batch_cnt + IDSIZE);
#endif
		if ((i = write(Batch_fp, &Batchbuf, Batch_cnt + IDSIZE)) != Batch_cnt + IDSIZE)
			syserr("batchflush:can't write %d", i);
		Batch_dirty = FALSE;
	}
}
/*
**	GETBATCH - retrieve data from the batch file and place in given
**			location
**
**
**	Parameters:
**		loc - address of place to put the data from the batch file
**		count - amount of data to get
**
**	Return Codes:
**			0
**
**	Side Effects:
**		loc is filled with data from batch file
**
**	Trace Flags:
**		Z42.15
**
**	Called by:
**		modupdate
**		secupdate
**		update
**		btreeupdate
**
*/
int
getbatch(void *locarg, int count)
{
	register char	*c;
	register int	cnt, size;
	char		*loc;
	int		i;

	loc = (char *) locarg;
	cnt = count;
#ifdef xZTR1
	if (tTf(42, 15))
		printf("getbatch:%d (%d)\n", cnt, Batch_cnt);
#endif
	c = loc;

	while (cnt) {
		/* see if there is anything in the buffer */
		if (Batch_cnt == BATCHSIZE)
			if ((i = readbatch()) < cnt)
				syserr("getbatch:can't read enough %d %d", i, cnt);
		if (cnt <= BATCHSIZE - Batch_cnt)
			size = cnt;
		else
			size = BATCHSIZE - Batch_cnt;
		bmove(&Batchbuf.b_buf[Batch_cnt], c, size);
		Batch_cnt += size;
		cnt -= size;
		c += size;
		/* flush the buffer if full */
		if (Batch_cnt == BATCHSIZE)
			batchflush();	/* re-write buffer if necessary */
	}
	return (0);
}

/*
**	PUTBATCH - put data in the batch file
**
**
**	Parameters:
**		cp - location of data to put in batch file
**		count - size of data
**
**	Return Codes:
**			0
**
**	Trace Flags:
**		Z42.2
**
**	Side Effects:
**		data is placed in batch file
**
**	Called by:
**		update
**
*/
void
putbatch(char *cp, int count)
{
	register char	*c;
	register int	size, cnt;
	int		i;

	cnt = count;
	c = cp;
#ifdef xZTR1
	if (tTf(42, 2))
		printf("putbatch:%d\n", cnt);
#endif

	while (cnt) {
		Batch_dirty = TRUE;	/* mark this buffer as dirty */
		if (cnt + Batch_cnt > BATCHSIZE)
			size = BATCHSIZE - Batch_cnt;
		else
			size = cnt;
		bmove(c, &Batchbuf.b_buf[Batch_cnt], size);
		c += size;
		Batch_cnt += size;
		cnt -= size;
		if (Batch_cnt == BATCHSIZE) {
			batchflush();
			/* is there is more to write, must read ahead first */
			if (cnt)
				if ((i = readbatch()) < cnt)
					syserr("putbatch:rd too small %d", i);
		}
	}
}

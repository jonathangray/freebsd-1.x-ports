#include "sccs.h"

#include "nodbuf.h"

#include "protos.h"

SCCSID(@(#)freebuf.c	8.1	12/31/84)

/*
**  FREEBUF.C -- more routines for LIFO dynamic buffer allocation [need.c]
**
**	These routines allow the deallocation of a need() type buffer,
**	and also using the same buffer for various SERIALIZED purposes
**	by marking the end of one, beginning of the next.
**
**	Defines:
**		freebuf()
**		markbuf()
**		seterr()
*/

/*
**  MARKBUF -- Mark a place in the buffer to deallocate to
**
**	Parameters:
**		bf -- buffer
**
**	Returns:
**		int >= 0 marking place in buffer (should be used in calling
**			freebuf())
**
**	Side Effects:
**		none
*/
int
markbuf(void *bf)
{
	register nodbuf_t	*buf;

	buf = (nodbuf_t *) bf;
	return (buf->nleft);
}

/*
**  FREEBUF -- frees part of a buffer
**
**	Parameters:
**		bf -- buffer
**		bytes -- a previous return from markbuf().
**
**	Returns:
**		none
**
**	Side Effects:
**		none
*/
void
freebuf(void *bf, int bytes)
{
	register nodbuf_t	*buf;
	register int			i;

	buf = (nodbuf_t *) bf;
	i = bytes - buf->nleft;
	if (i < 0)
		syserr("freebuf %d, %d", i, bytes);
	buf->xfree -= i;
	buf->nleft += i;
}
/*
**  SETERR -- change the error info for a buffer
**
**	Parameters:
**		bf -- buffer
**		errnum -- new overflow error code
**		err_func -- new error handler
**
**	Returns:
**		none
**
**	Side Effects:
**		adjusts buffer structure
*/
void
seterr(void *bf, int errnum, int (*err_func)(int, int))
{
	register nodbuf_t	*buf;

	buf = (nodbuf_t *) bf;
	buf->err_num = errnum;
	buf->err_func = err_func;
}

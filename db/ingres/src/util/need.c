#include "sccs.h"

#include "nodbuf.h"

#include "protos.h"

SCCSID(@(#)need.c	8.2	12/18/85)

/*
**  NEED.C -- general buffer allocation routines
**
**	allow buffers with LIFO de-allocation
*/

/*
**  NEED -- allocate space from a buffer
**
**	On buffer overflow, calls err_func from that field
**	in the buffer with the error code err_code from that field
**	in the buffer, then returns 0.
**	need() guarantees an even adress on return.
**
**	Parameters:
**		bf -- buffer
**		nbytes -- number of bytes desired
**
**	Returns:
**		pointer to allocated area
**		on buffer overflow returns 0.
**
**	Side Effects:
**		adjusts buffer structure to reflect allocation.
*/

char *
need(char *bf, int nbytes)
{
	register char		*x;
	register nodbuf_t	*buf;
	register int		i;

	buf = (nodbuf_t *) bf;
	i = nbytes;
	if (i > buf->nleft) {
		(*buf->err_func)(buf->err_num, 0);
		return (0);
	}
	i = (i + 3) & ~03;
	x = buf->xfree;
	buf->xfree += i;
	buf->nleft -= i;
	clrmem(x, i);
	return(x);
}

/*
**  INITBUF -- initialize a buffer
**
**	Must be called before the first need() call on the buffer.
**
**	Parameters:
**		bf -- buffer
**		size -- size fo buffer area
**		err_num -- error code for overflow
**		err_func -- function to call with err_code on error
**
**	Returns:
**		none
**
**	Side Effects:
**		initializes buffer structure
**
**	Diagnostics:
**		"initbuf : odd buffer adress 0%o" -- buffers must start
**			at an even address.
*/
void
initbuf(char *bf, int size, int err_num, int (*err_func)(int, int))
{
	register nodbuf_t	*buf;
	register 		i;

	buf = (nodbuf_t *) bf;
	i = (int) buf;
	if (i & 03)
		syserr("initbuf : odd buffer adress 0%o", buf);
	buf->nleft = size - sizeof(*buf);
	buf->xfree = buf->buffer;
	buf->err_num = err_num;
	buf->err_func = err_func;
}

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "buf.h"
#include "ingres.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)buf.c	8.1	12/31/84)


/*
**  BUFFER MANIPULATION ROUTINES
*/



/*
**  BUFPUT -- put character onto buffer
**
**	The character 'c' is put onto the buffer 'bp'.  If the buffer
**	need be extended it is.
*/
void
bufput(char c, buf_t **buffer)
{
	register buf_t	*b;
	register buf_t	*a;
	register buf_t	**bp;

	bp = buffer;
	b = *bp;
	if (b == 0 || b->ptr >= &b->buffer[BUFSIZE]) {
		/* allocate new buffer segment */
		a = (buf_t *) bufalloc(sizeof(*a));
		a->nextb = b;
		a->ptr = a->buffer;
		*bp = b = a;
	}

	*b->ptr++ = c;
}

/*
**  BUFGET -- get character off of buffer
**
**	The buffer is popped and the character is returned.  If the
**	segment is then empty, it is returned to the free list.
*/
char
bufget(buf_t **buffer)
{
	register buf_t	*b;
	register char		c;
	register buf_t	**bp;

	bp = buffer;
	b = *bp;

	if (b == 0 || b->ptr == b->buffer) {
		/* buffer is empty -- return end of file */
		return (0);
	}

	c = *--(b->ptr);

	/* check to see if we have emptied the (non-initial) segment */
	if (b->ptr == b->buffer && b->nextb != 0) {
		/* deallocate segment */
		*bp = b->nextb;
		buffree((char *) b);
	}

	return (c);
}

/*
**  BUFPURGE -- return an entire buffer to the free list
**
**	The buffer is emptied and returned to the free list.  This
**	routine should be called when the buffer is to no longer
**	be used.
*/
void
bufpurge(buf_t **buffer)
{
	register buf_t	**bp;
	register buf_t	*a;
	register buf_t	*b;

	bp = buffer;
	b = *bp;
	*bp = 0;

	/* return the segments to the free list */
	while (b != 0) {
		a = b->nextb;
		buffree((char *) b);
		b = a;
	}
}

/*
**  BUFFLUSH -- flush a buffer
**
**	The named buffer is truncated to zero length.  However, the
**	segments of the buffer are not returned to the system.
*/
void
bufflush(buf_t **buffer)
{
	register buf_t	*b;
	register buf_t	**bp;

	bp = buffer;
	b = *bp;
	if (b == 0)
		return;

	/* return second and subsequent segments to the system */
	bufpurge(&b->nextb);

	/* truncate this buffer to zero length */
	b->ptr = b->buffer;
}

/*
**  BUFCRUNCH -- flatten a series of buffers to a string
**
**	The named buffer is flattenned to a conventional C string,
**	null terminated.  The buffer is deallocated.  The string is
**	allocated "somewhere" off in memory, and a pointer to it
**	is returned.
*/

char	*Buf_flat;

char *
bufcrunch(buf_t **buffer)
{
	register char	*p;

	p = bufflatten(*buffer, 1);
	*p = 0;
	*buffer = 0;
	return (Buf_flat);
}

char *
bufflatten(buf_t *buf, int length)
{
	register buf_t	*b;
	register char		*p;
	register char		*q;

	b = buf;

	/* see if we have advanced to beginning of buffer */
	if (b != 0) {
		/* no, keep moving back */
		p = bufflatten(b->nextb, length + (b->ptr - b->buffer));
	} else {
		/* yes, allocate the string */
		Buf_flat = p = bufalloc(length);
		return (p);
	}

	/* copy buffer into string */
	for (q = b->buffer; q < b->ptr; )
		*p++ = *q++;

	/* deallocate the segment */
	buffree((char *) b);

	/* process next segment */
	return (p);
}

/*
**  BUFALLOC -- allocate clear memory
**
**	This is similar to the system malloc routine except that
**	it has no error return, and memory is guaranteed to be clear
**	when you return.
**
**	It might be nice to rewrite this later to avoid the nasty
**	memory fragmentation that malloc() tends toward.
**
**	The error processing might have to be modified if used anywhere
**	other than INGRES.
*/

char *
bufalloc(int size)
{
	return(xalloc(size, 1, 1));
}

/*
**  BUFFREE -- free memory
*/
void
buffree(char *ptr)
{
	if (ptr == (char *) NULL) {
		syserr("buffree: NULL ptr");
	}
	xfree(ptr);
}

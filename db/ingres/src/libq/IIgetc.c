#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIgetc.c	8.1	12/31/84)


/*
**  IIGETC.C -- File input routines
**
**	Defines:
**		IIfopen()
**		IIgetc()
**		IIclose()
**
**	Requires:
**		read()
**		open()
**
**	Required By:
**		IIp_err() -- to get text from error files
**		IIgetpath();
**		USER -- as Input routines
**
**	History:
**		11/21/78 -- (marc) written to free IIp_err() [IIp_err.c] from
**			depending on a single I/O package
*/



/*
**  IIFOPEN -- Buffered input file open
**
**	Entirely analogous to fopen(III).
**
**	Parameters:
**		file - file name to open for READ only
**		iobuf - iob struct to use for this file
**
**	Returns:
**		0  success
**		-1 failure (errno set by open(II) call)
**
**	Side Effects:
**		file activity
**		sets up iobuf
**
**	Requires:
**		open()
**
**	Called By:
**		IIp_err() [IIp_err.c]
**		USER
**
**	History:
**		11/21/78 -- (marc) written
*/
int
IIfopen(char *file, iob_t *iobuf)
{
	register iob_t	*b;

	b = iobuf;
	if ((b->fildes = open(file, 0)) < 0)
		return (-1);
	b->nleft = 0;
	return (0);
}

/*
**  IIGETC -- Get a character from a file using buffered input
**
**	Entirely analogous to getc(III).
**
**	Parameters:
**		iobuf -- iob struct for the file from which the character
**			is to be taken
**
**	Returns:
**		next character from file (16-bit no sign extension)
**		-1 -- EOF or error (errno set by read)
**
**	Side Effects:
**		file activity - may do a read ()
**		fuddles iobuf to reflect number of characters left after call
**
**	Requires:
**		read()
**		an fopen(III) or IIfopen() [IIgetc.c] call on iobuf before
**			being called. (It is unwise to call fopen(), the 
**			IIgetc(), because fopen() and getc(III) are both 
**			in /usr/source/s4/getc.c so the code will be 
**			duplicated).
**
**	Called By:
**		IIp_err() [IIp_err.c]
**		USER
**
**	History:
**		11/21/78 -- (marc) written
*/
int
IIgetc(iob_t *iobuf)
{
	register iob_t	*b;
	register int		i;
	register int		c;

	b = iobuf;
	if (--b->nleft >= 0) {
		c = *b->nextp++ & 0377;
		return (c);
	}
	
	/* else fill the buffer */
	i = read(b->fildes, b->buff, sizeof(b->buff));
	if (i > 0) {
		b->nextp = b->buff;
		b->nleft = --i;
		c = *b->nextp++ & 0377;
		return (c);
	}
	/* EOF or error */
	return (-1);
}

/*
**  IICLOSE -- Close a file opened with IIfopen
**
**	Parameters:
**		buf -- io buffer
**
**	Returns:
**		< 0 one error (errno set)
**
**	Side Effects:
**		closes file
**
**	Requires:
**		close(II)
**
**	Called By:
**		USER
*/
int
IIclose(iob_t *buf)
{

	return (close(buf->fildes));
}

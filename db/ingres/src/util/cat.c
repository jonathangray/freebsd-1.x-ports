#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <fcntl.h>
#include "sccs.h"
#include <useful.h>

SCCSID(@(#)cat.c	8.3	2/8/85)

/*
**  CAT -- "cat" a file
**
**	This function is essentially identical to the UNIX cat(I).
**
**	Parameters:
**		file -- the name of the file to be cat'ed
**
**	Returns:
**		zero -- success
**		else -- failure (could not open file)
**
**	Side Effects:
**		"file" is open and read once through; a copy is made
**			to the standard output.
*/
int
cat(char *file)
{
	char		buf[BLOCK_SZ];
	register int	i;
	register int	fd;

	fd = open(file, O_RDONLY);
	if (fd < 0)
		return (1);

	while ((i = read(fd, buf, BLOCK_SZ)) > 0) {
		write(1, buf, i);
	}

	return (0);
}

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)writeout.c	8.2	1/18/85)



/*
**  WRITE OUT QUERY BUFFER TO UNIX FILE
**
**	The logical buffer is written to a UNIX file, the name of which
**	must follow the \w command.
**
**	Uses trace flag 18
*/
void
writeout(void)
{
	register int	i;
	register char	*file;
	register int	source;
	int		dest;
	char		buf[512];

	file = getfilenm();
	if (file[0] == 0 || file[0] == '-') {
		printf("Bad file name \"%s\"\n", file);
		return;
	}

	if ((dest = open(file, O_CREAT | O_TRUNC | O_WRONLY, 0644)) < 0) {
		printf("Cannot create \"%s\"\n", file);
		return;
	}

	if (!Nautoclear)
		Autoclear = 1;

	if ((source = open(Qbname, O_RDONLY)) < 0)
		syserr("writeout: open(%s)\n", Qbname);

	fflush(Qryiop);

	while ((i = read(source, buf, sizeof(buf))) > 0)
		write(dest, buf, i);

	close(source);
	close(dest);
}

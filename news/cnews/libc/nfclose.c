/*
 * nfclose(stream) - flush the stream, fsync its file descriptor and
 * fclose the stream, checking for errors at all stages.  This dance
 * is needed to work around the lack of Unix file system semantics
 * in Sun's NFS.  Returns EOF on error.
 */

#include <stdio.h>

int
nfclose(stream)
register FILE *stream;
{
	register int ret = 0;

	if (fflush(stream) == EOF)
		ret = EOF;
	if (fsync(fileno(stream)) < 0)		/* may get delayed error here */
		ret = EOF;
	if (fclose(stream) == EOF)
		ret = EOF;
	return ret;
}

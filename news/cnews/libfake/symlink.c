/*
 * symlink dummy
 */

int
symlink(n1, n2)
char *n1;
char *n2;
{
	extern int errno;

	errno = 0;		/* kludge */
	return(-1);
}

/*
 * fsync(2) emulation for systems lacking it
 */

/* ARGSUSED */
int
fsync(fd)
int fd;
{
	return 0;
}

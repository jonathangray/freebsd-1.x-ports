/*
 * dostatfs - the heart of spacefor.statfs
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/mount.h>
#ifndef MNAMELEN
/* this does not seem to be a 4.4BSD, try for SunOS */
#include <sys/vfs.h>
#define	f_fsize	f_bsize		/* idiotic incompatible naming in 4.4 */
#endif

extern int debug;

extern void error(), exit();

/*
 - process - do the work
 */
long
process(filesize, fileonfs, wantspace, wantinodes, bperi)
long filesize;
char *fileonfs;
long wantspace;
long wantinodes;
long bperi;
{
	struct statfs info;
	register long n;
#	define	LOTS	10000
	register long iperfile = filesize/bperi + 1;

	if (statfs(fileonfs, &info) < 0)
		error("cannot do statfs(%s)", fileonfs);
	if (debug)
		fprintf(stderr, "bsize %ld, avail %ld, inodes %ld\n",
				info.f_fsize, info.f_bavail, info.f_ffree);

	n = LOTS;
	if (info.f_bavail <= wantspace)
		n = 0;
	else if (info.f_fsize > 0 && filesize > 0)
		n = (info.f_bavail - wantspace) / (filesize/info.f_fsize + 1);

	if (info.f_ffree < 0)		/* information unavailable */
		;			/* bypass check, and pray */
	else if (info.f_ffree <= wantinodes)
		n = 0;
	else if ((info.f_ffree - wantinodes) / iperfile < n)
		n = (info.f_ffree - wantinodes) / iperfile;

	if (n < 0)
		n = 0;
	else if (n > LOTS)
		n = LOTS;	/* to avert 16-bit trouble elsewhere */

	return(n);
}

/*
 * doultrix - the heart of spacefor.ultrix
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/mount.h>

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
	struct fs_data info;
	register long n;
#	define	LOTS	10000
#ifndef ULTRIXBS
#	define	ULTRIXBS	(1024)
#endif
	register long iperfile = filesize/bperi + 1;

	if (statfs(fileonfs, &info) <= 0)
		error("cannot do statfs(%s)", fileonfs);
	if (debug)
		fprintf(stderr, "bsize %ld, avail %ld, inodes %ld\n",
				ULTRIXBS, info.fd_req.bfreen, info.fd_req.gfree);

	n = LOTS;
	if (info.fd_req.bfreen <= wantspace)
		n = 0;
	else if (ULTRIXBS > 0 && filesize > 0)
		n = (info.fd_req.bfreen - wantspace) / (filesize/ULTRIXBS + 1);
	if (info.fd_req.gfree < 0)	/* information unavailable */
		;			/* bypass check, and pray */
	else if (info.fd_req.gfree <= wantinodes)
		n = 0;
	else if ((info.fd_req.gfree - wantinodes) / iperfile < n)
		n = (info.fd_req.gfree - wantinodes) / iperfile;

	if (n < 0)
		n = 0;
	else if (n > LOTS)
		n = LOTS;	/* to avert 16-bit trouble elsewhere */

	return(n);
}

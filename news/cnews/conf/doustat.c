/*
 * doustat - the heart of spacefor.ustat
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ustat.h>

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
	struct stat silly;
	struct ustat info;
	register long n;
#	define	LOTS	10000
#ifndef USTATBS
#	define	USTATBS	(512)
#endif
	register long iperfile = filesize/bperi + 1;

	if (stat(fileonfs, &silly) < 0)
		error("cannot do stat(%s)", fileonfs);
	if (ustat(silly.st_dev, &info) < 0)
		error("cannot do ustat(%s)", fileonfs);
	if (debug)
		fprintf(stderr, "bsize %ld, avail %ld, inodes %ld\n",
				USTATBS, info.f_tfree, (long)info.f_tinode);

	n = LOTS;
	if (info.f_tfree <= wantspace)
		n = 0;
	else if (USTATBS > 0 && filesize > 0)
		n = (info.f_tfree - wantspace) / (filesize/USTATBS + 1);

	if (info.f_tinode < 0)		/* information unavailable */
		;			/* bypass check, and pray */
	else if (info.f_tinode <= wantinodes)
		n = 0;
	else if ((info.f_tinode - wantinodes) / iperfile < n)
		n = (info.f_tinode - wantinodes) / iperfile;

	if (n < 0)
		n = 0;
	else if (n > LOTS)
		n = LOTS;	/* to avert 16-bit trouble elsewhere */

	return(n);
}

/*
 *	vms2unix.c
 *
 *	Miscellaneous routines for UNIX/VMS compatibility.
 *
 * $Log: vms2unix.c,v $
 * Revision 1.1  1994/02/01 03:29:42  jkh
 * Initial revision
 *
 * Revision 1.2  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.1  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.0  1993/03/25  19:48:11  pgf
 * Initial revision
 *
 */
#include	"estruct.h"
#include	"dirstuff.h"

#include	<unixio.h>

#define	zfab	dirp->dd_fab
#define	znam	dirp->dd_nam
#define	zrsa	dirp->dd_ret.d_name
#define	zrsl	dirp->dd_ret.d_namlen
#define	zesa	dirp->dd_esa

DIR *
opendir(char *filename)
{
	DIR	*dirp = calloc(1, sizeof(DIR));
	long	status;

	if (dirp == 0)
		return (0);

	zfab = cc$rms_fab;
	zfab.fab$l_fop = FAB$M_NAM;
	zfab.fab$l_nam = &znam;		/* FAB => NAM block	*/
	zfab.fab$l_dna = "*.*;*";	/* Default-selection	*/
	zfab.fab$b_dns = strlen(zfab.fab$l_dna);

	zfab.fab$l_fna = filename;
	zfab.fab$b_fns = strlen(filename);

	znam = cc$rms_nam;
	znam.nam$b_ess = NAM$C_MAXRSS;
	znam.nam$l_esa = zesa;
	znam.nam$b_rss = NAM$C_MAXRSS;
	znam.nam$l_rsa = zrsa;

	if (sys$parse(&zfab) != RMS$_NORMAL) {
		closedir(dirp);
		dirp = 0;
	}
	return (dirp);
}

DIRENT *
readdir(DIR *dirp)
{
	if (sys$search(&zfab) == RMS$_NORMAL) {
		zrsl = znam.nam$b_rsl;
		zrsa[znam.nam$b_rsl];
		return (&(dirp->dd_ret));
	}
	return (0);
}

int
closedir(DIR *dirp)
{
	cfree(dirp);
}

char *
tempnam(head, tail)
char	*head;
char	*tail;
{
	char	temp[NFILEN];
	char	leaf[NFILEN];
	return mktemp(
		strmalloc(
			pathcat(temp,
				head,
				strcat(strcpy(leaf, tail), "XXXXXX"))));
}

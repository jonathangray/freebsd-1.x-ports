#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <version.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)checkadmin.c	8.2	1/3/85)

/*
**  CHECKADMIN -- check admin file version, etc.
**
**	The checks for database version code and whatnot are
**	factored out into this routine.  When this routine returns,
**	the admin file should be legible to this program.
**	If the admin file is not legible, it will syserr.
**
**	Parameters:
**		fd -- open file descriptor for admin file.  Only
**			read access is required.
**
**	Returns:
**		nothing if ok.
**		not at all (or via syserr) if not ok.
**
**	Side Effects:
**		The Admin.ad_h struct will be filled in.
*/
void
checkadmin(register int fd)
{
	register int	i;
	register int	k;

	i = ((char *) &Admin.ad_h.adm_version) - ((char *) &Admin.ad_h);
	if (read(fd, (char *) &Admin.ad_h, i) != i) {
		syserr("checkadmin: admin read err 1");
	}
	if (!BITISSET(A_NEWFMT, Admin.ad_h.adm_flags)) {
		syserr("checkadmin: cannot use old databases");
	}

	/* read in remainder of admin header */
	i = sizeof(Admin.ad_h);
	if (Admin.ad_h.adm_len < i) {
		i = Admin.ad_h.adm_len;
	}
	i -= ((char *) &Admin.ad_h.adm_version) - ((char *) &Admin.ad_h);
	if (i <= 0) {
		syserr("checkadmin: adlen=%d, hdrsz=%d, ct=%d", Admin.ad_h.adm_len, sizeof(Admin.ad_h), i);
	}
	if ((k = read(fd, (char *) &Admin.ad_h.adm_version, i)) != i) {
		syserr("checkadmin: admin read err 2, i=%d k=%d", i, k);
	}

	/* check versions here */
	if (Admin.ad_h.adm_version != DBVERCODE) {
		syserr("cannot handle code %d databases (current code is %d)",
			Admin.ad_h.adm_version, DBVERCODE);
	}
	if (Admin.ad_h.adm_rellen != sizeof(Admin.ad_rel)) {
		syserr("checkadmin: descriptor size mismatch, dec=%d, actual=%d\n Run ingconv on this database.",
			Admin.ad_h.adm_rellen, sizeof(Admin.ad_rel));
	}

	/* get to beginning of descriptors */
	if (lseek(fd, (off_t) Admin.ad_h.adm_len, 0) < 0) {
		syserr("checkadmin: seek");
	}

	/* read the descriptors */
	if (read(fd, (char *) &Admin.ad_rel, Admin.ad_h.adm_rellen) != Admin.ad_h.adm_rellen) {
		syserr("checkadmin: reld read sz=%d", Admin.ad_h.adm_rellen);
	}
	if (read(fd, (char *) &Admin.ad_attr, Admin.ad_h.adm_attrlen) != Admin.ad_h.adm_attrlen) {
		syserr("checkadmin: attd read sz=%d", Admin.ad_h.adm_attrlen);
	}
}

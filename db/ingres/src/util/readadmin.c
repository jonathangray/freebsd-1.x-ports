#include <sys/types.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include <aux.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)readadmin.c	8.2	1/18/85)

/*
**  READADMIN -- read admin file into 'Admin' cache
**
**	The admin file in the current directory is opened and read
**	into the 'Admin' cache.  The admin file contains the following
**	information:
**
**	A header block, containing the owner of the database (that is,
**	the DBA), and a set of status bits for the database as a whole.
**	These bits are defined in aux.h.  This header also includes a
**	field that defines the length of the header part & a version
**	stamp.
**
**	Descriptors for the relation and attribute relations.  These
**	descriptors should be completely correct except for the
**	d_fd and d_opened fields.  These are required so that the
**	process of opening a relation is not recursive.
**
**	After the admin file is read in, the relation and attribute
**	files are opened, and the d_fd and d_opened fields in both
**	descriptors are correctly initialized.  Both catalogs are
**	opened read/write.
**
**	WARNING:
**		This routine is redefined by creatdb.  If this
**		routine is changed, check that program also!!
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		The 'Admin' struct is filled in from the 'admin' file
**			in the current directory.
**		The 'relation....xx' and 'attribute...xx' files are
**			opened.
**
**	Files:
**		./admin
**			The bootstrap description of the database,
**			described above.
**
**	Trace Flags:
**		none
*/
void
readadmin(int fake)
{
	register int	i;
	char		relname[MAX_NAME_SIZE + 4];

	/* read the stuff from the admin file */
	i = open("admin", O_RDONLY);
	if (i < 0) {
		syserr("readadmin: open admin %d", i);
	}
	checkadmin(i);
	close(i);

	/* open the physical files for 'relation' and 'attribute' */
	ingresname(Admin.ad_rel.d_r.r_id, Admin.ad_rel.d_r.r_owner, relname);
	if ((Admin.ad_rel.d_fd = open(relname, O_RDWR)) < 0) {
		syserr("readadmin: open rel %d", Admin.ad_rel.d_fd);
	}
	ingresname(Admin.ad_attr.d_r.r_id, Admin.ad_attr.d_r.r_owner, relname);
	if ((Admin.ad_attr.d_fd = open(relname, O_RDWR)) < 0) {
		syserr("readadmin: open att %d", Admin.ad_attr.d_fd);
	}
	Admin.ad_rel.d_opened = (Admin.ad_rel.d_fd + 1) * -5;
	/* we just want to read here create, modify and destroy fix it up */
	Admin.ad_attr.d_opened = (Admin.ad_attr.d_fd + 1) * 5;

}

#include <stdio.h>

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
#include <aux.h>
#include <access.h>
#include <symbol.h>
#include <catalog.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)openr.c	8.4	1/22/85)

/*
**  OPENR -- Open a relation into a descriptor
**
**	Openr will open the named relation into the given descriptor
**	according to the mode specified. When searching for a name,
**	a relation owner by the current user will be searched for first.
**	If none is found then one owned by the DBA will be search for.
**
**	There are several available modes for opening a relation. The
**	most common are
**		mode OR_READ    -- open for reading
**		mode OR_WRITE   -- open for writing.
**	Other modes which can be used to optimize performance:
**		mode OR_RELTID  -- get relation-relation tuple and tid only.
**      			Does not open the relation.
**		mode OR_AREAD   -- open relation for reading after a previous
**      			call of mode OR_RELTID.
**		mode OR_AWRITE  -- open relation for writing after a previous
**      			call of mode OR_RELTID.
**		mode OR_REREAD  -- open relation for reading. Assumes that relation
**      			was previously open (eg relation & attributed
**      			have been filled) and file was closed by closer.
**		mode OR_REWRITE -- open relation for writing. Same assumptions as
**      			mode OR_REREAD.
**
**	Parameters:
**		dx - a pointer to a desc_t (defined in ingres.h)
**		mode - can be OR_READ -> OR_REWRITE
**		name - a null terminated name (only first 12 chars looked at)
**
**	Returns:
**		1 - relation does not exist
**		0 - ok
**		<0 - error. Refer to the error codes in access.h
**
**	Side Effects:
**		Opens the physical file if required. Fill the
**		descriptor structure. Initializes the access methods
**		if necessary.
**
**	Trace Flags:
**		90
*/

int
openr(desc_t *d, int mode, char *name)
{
	int		i;
	register int	retval, filemode;
	char		filename[MAX_NAME_SIZE+3];
	char		btree[MAX_NAME_SIZE];
	char		btreefile[MAX_NAME_SIZE + 3];
#ifdef xATR1
	if (tTf(21, 0))
		printf("openr:%.12s,%d\n", name, mode);
#endif

	retval = 0;

	/* init admin */
	acc_init(0, 0);

	/* process according to mode */

	filemode = O_RDONLY;

	if (mode >= 0) {
		d->d_btree = NULL;
	}

	switch (mode) {

	  case OR_RELTID:
		retval = get_reltup(d, name);
		break;

	  case OR_WRITE:
		filemode = O_RDWR;

	  case OR_READ:
		if ((retval = get_reltup(d, name)) != 0) {
			break;
		}

	  case OR_AREAD:
	  case OR_AWRITE:
		if ((retval = get_attuples(d)) != 0) {
			break;
		}

	  case OR_REWRITE:
		if (mode == OR_AWRITE || mode == OR_REWRITE) {
			filemode = O_RDWR;
		}

	  case OR_REREAD:
		clearkeys(d);
		/* descriptor is filled. open file */
		ingresname(d->d_r.r_id, d->d_r.r_owner, filename);
		/* can't open a view */
		if (d->d_r.r_status & S_VIEW) {
			retval = acc_err(AMOPNVIEW_ERR);	/* view */
			break;
		}
		if ((d->d_fd = open(filename, filemode)) < 0) {
			retval = acc_err(AMNOFILE_ERR);	/* can't open file */
			break;
		}
		d->d_opened = (d->d_fd + 1) * 5;
		if (filemode == O_RDWR)
			d->d_opened = -d->d_opened;
		d->d_addc = 0;
		retval = 0;
		break;

	  default:
		syserr("openr:bd md=%d", mode);
	}

	if (mode == OR_RELTID && d->d_r.r_dim > 0 && !retval) {
		/* open btreesec relation */
		capital(d->d_r.r_id, btree);
		d->d_btree = xalloc(sizeof(desc_t), 1, 1);
		if ((i = openr(d->d_btree, OR_RELTID, btree)) != 0)
			syserr("opening Btreesec %s %d\n", btree, i);
	}

	if (retval == 0 && d->d_r.r_dim > 0 && mode != OR_RELTID) {
		capital(d->d_r.r_id, btree);
		if (d->d_btree == NULL) {
			d->d_btree = xalloc(sizeof(desc_t), 1, 1);
		}
		if ((i = openr(d->d_btree, mode, btree)) != 0) {
			syserr("opening Btreesec %s %d\n", btree, i);
		}
		ingresname(d->d_r.r_id, d->d_r.r_owner, filename);
		btreename(filename, btreefile);
		if ((d->d_btreefd = open(btreefile, O_RDWR)) < 0) {
			syserr("openr: can't open %s", btreefile);
		}
	}

	/* return */

#ifdef xATR1
	if (tTf(21, 4) && mode != OR_RELTID && retval != 1)
		printdesc(d);
	if (tTf(21, 0))
		printf("openr rets %d\n", retval);
#endif

	return (retval);
}
/*
**  GET_ATTUPLES -- get tuples from attribute relation for this relation
*/
int
get_attuples(desc_t *d)
{
	attr_t	attr, attkey;
	register int		i, dom;
	int			numatts;
	tid_t			tid1, tid2;

	clearkeys(&Admin.ad_attr);

	/* zero all format types */
	for (i = 0; i <= d->d_r.r_attrc; i++) {
		d->d_fmt[i] = 0;
	}

	/* prepare to scan attribute relation */
	ingres_setkey(&Admin.ad_attr, (char *) &attkey, d->d_r.r_id, ATTRELID);
	ingres_setkey(&Admin.ad_attr, (char *) &attkey, d->d_r.r_owner, ATTOWNER);
	if ((i = find(&Admin.ad_attr, EXACTKEY, &tid1, &tid2, &attkey)) != 0) {
		return (i);
	}

	numatts = d->d_r.r_attrc;

	while (numatts && !get(&Admin.ad_attr, &tid1, &tid2, &attr, TRUE)) {

		/* does this attribute belong? */
		if (bequal(&attr, &attkey, MAX_NAME_SIZE + 2)) {

			/* this attribute belongs */
			dom = attr.a_id;	/* get domain number */

			if (d->d_fmt[dom]) {
				/* duplicate attribute. force error */
				break;
			}

			numatts--;
			d->d_off[dom] = attr.a_off;
			d->d_fmt[dom] = attr.a_fmt;
			d->d_len[dom] = attr.a_len;
			d->d_iskey[dom] = attr.a_iskey;
		}
	}

	d->d_fmt[0] = INT_CONST;
	d->d_len[0] = 4;
	/* make sure all the atributes were there */
	for (dom = 1; dom <= d->d_r.r_attrc; dom++) {
		if (d->d_fmt[dom] == 0) {
			/* force an error */
			numatts = 1;
		}
	}
	if (numatts) {
		i = acc_err(AMNOATTS_ERR);
	}

	flush_rel(&Admin.ad_attr, TRUE);

#ifdef xATR1
	if (tTf(21, 3)) {
		printf("get_attr ret %d\n", i);
	}
#endif
	return (i);
}

#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <lock.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)find.c	8.1	12/31/84)

/*
** This routine will check that enough of the tuple has been specified
** to enable a key access.
*/
int
fullkey(desc_t *des)
{
	register desc_t	*d;
	register int	i;

	d = des;
	for (i = 1; i <= d->d_r.r_attrc; i++)
		if (d->d_iskey[i] && !d->d_given[i])
			return (FALSE);
	return (TRUE);
}

/*
**	Find - determine limits for scan of a relation
**
**	Find determines the values of an initial tid_t
**	and an ending tid_t for scanning a relation.
**	The possible calls to find are:
**
**	find(desc, NOKEY, lotid, hightid)
**		sets tids to scan entire relation
**
**	find(desc, EXACTKEY, lotid, hightid, key)
**		sets tids according to structure
**		of the relation. Key should have
**		been build using setkey.
**
**	find(desc, LRANGEKEY, lotid, hightid, keylow)
**		Finds lotid less then or equal to keylow
**		for isam relations. Otherwise scans whole relation.
**		This call should be followed by a call with HRANGEKEY.
**
**	find(desc, HRANGEKEY, lotid, hightid, keyhigh)
**		Finds hightid greater than or equal to
**		keyhigh for isam relations. Otherwise sets
**		hightid to maximum scan.
**
**	find(desc, FULLKEY, lotid, hightid, key)
**		Same as find with EXACTKEY and all keys
**		provided. This mode is used only by findbest
**		and replace.
**
**	returns:
**		<0 fatal error
**		 0 success
**
**	Trace Flags:
**		22.0-8
*/

int
find(desc_t *d, int mode, tid_t *lotid, tid_t *hightid, void *key)
{
	register int	ret;
	bool		keyok;
	long		pageid, lid[MAXLID], page;
	locator_t  tid_id;
	char		*tp;
	int		i;

#ifdef xATR1
	if (tTf(22, 0)) {
		printf("find: m%d,s%d,%.14s\n", mode, d->d_r.r_spec, d->d_r.r_id);
		if (mode != NOKEY)
			printup(d, key);
	}
#endif

	ret = 0;	/* assume successful return */
	keyok = FALSE;

	switch (mode) {

	  case EXACTKEY:
		keyok = fullkey(d);
		break;

	  case FULLKEY:
		keyok = TRUE;

	  case NOKEY:
	  case LRANGEKEY:
	  case HRANGEKEY:
	  case BTREEKEY:
		break;

	  default:
		syserr("FIND: bad mode %d", mode);
	}

	/* set lotid for beginning of scan */
	if (mode != HRANGEKEY) {
		pageid = 0;
		stuff_page(lotid, &pageid);
		lotid->line_id = -1;
	}

	/* set hitid for end of scan */
	if (mode != LRANGEKEY) {
		pageid = -1;
		stuff_page(hightid, &pageid);
		hightid->line_id = -1;
	}

	if (mode == BTREEKEY) {
	/* set tid to value as found using B-Tree */
		tp = key + d->d_r.r_width - LIDSIZE * d->d_r.r_dim;
		bmove(tp, lid, LIDSIZE * d->d_r.r_dim);
		setglobalint(BTREE_FD_NAME, d->d_btreefd);
		page = RT;
		for (i = 0; i < d->d_r.r_dim; ++i) {
			pageid = get_tid(page, lid[i], &tid_id);
			if (pageid < 0)
				break;
			page = pageid;
		}
		if (pageid >= 0)
			bmove(&pageid, lotid, LIDSIZE);

	} else if (mode != NOKEY) {
		switch (M_TYPEOF(d->d_r.r_spec)) {
	
		  case M_HEAP:
			break;
	
		  case M_ISAM:
			if (mode != HRANGEKEY) {
				/* compute lo limit */
				if ((ret = ndxsearch(d, lotid, key, -1, keyok)) != 0)
					break;	/* fatal error */
			}
	
			/* if the full key was provided and mode is exact, then done */
			if (keyok) {
				bmove((char *) lotid, (char *) hightid, sizeof(*lotid));
				break;
			}
	
			if (mode != LRANGEKEY)
				ret = ndxsearch(d, hightid, key, 1, keyok);
			break;
	
		  case M_HASH:
			if (!keyok)
				break;		/* can't do anything */
			pageid = rhash(d, key);
			stuff_page(lotid, &pageid);
			stuff_page(hightid, &pageid);
			break;

		  default:
			ret = acc_err(AMFIND_ERR);
		}
	}

#ifdef xATR2
	if (tTf(22, 1)) {
		printf("find: ret %d\tlow", ret);
		dumptid(lotid);
		printf("hi");
		dumptid(hightid);
	}
#endif
	return (ret);
}

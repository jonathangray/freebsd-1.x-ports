#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <pv.h>
#include <aux.h>
#include <access.h>
#include <batch.h>
#include <lock.h>
#include <func.h>
#include <version.h>
#include <symbol.h>
#include <catalog.h>
#include <btree.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#define INGRES_IUTIL
#define INGRES_CTLMOD
#include <protos.h>

SCCSID(@(#)modify.c	8.8	5/7/85)

extern	short	tTdbu[];
int	modify();

func_t ModifyFn = {
	"MODIFY",
	modify,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};

/*
**  MODIFY -- converts any relation to the specified
**		storage structure
**
**	arguments:
**	0 - relation name
**	1 - storage structure ("heap", "cheap", "hash", "chash",
**		"isam", "cisam")
**	2 - "name" for attribute names, or "num" for numbers
**	3 - key1
**	4 - key2
**	    .
**	    .
**	i - null
**	i+1 - option name (e.g., "fillfactor")
**	i+2 - option value
**	    .
**	    .
**
**	If all the options default, parameter i -> pc are omitted.
**	If no keys are provided, parameter 2 is omitted.
*/

int		F_fac, Mn_pages, Mx_pages;
char		Lid[MAXLID][MAX_NAME_SIZE];
int		NLidKeys;
int		LidKey[MAXLID];

struct modtab {
	char	*type;
	char	newrelspec;
	char	yeskeys;
	char	sortit;
	char	yes_seq;
	int	f_fac;
	int	mn_pages;
	int	mx_pages;
};


struct modtab	Modtab[] = {
	/* type		spec	keys	sort	seq	ffac	min	max */

	{ "heap",	M_HEAP,	FALSE,	FALSE,	FALSE,	0,	0,	0 },
	{ "cheap",	-M_HEAP,FALSE,	FALSE,	FALSE,	0,	0,	0 },
	{ "hash",	M_HASH,	TRUE,	TRUE,	FALSE,	50,	10,	-1 },
	{ "chash",	-M_HASH,TRUE,	TRUE,	FALSE,	75,	1,	-1 },
	{ "isam",	M_ISAM,	TRUE,	TRUE,	FALSE,	80,	0,	0 },
	{ "cisam",	-M_ISAM,TRUE,	TRUE,	FALSE,	100,	0,	0 },
	{ "heapsort",	M_HEAP,	TRUE,	TRUE,	TRUE,	0,	0,	0 },
	{ "cheapsort",	-M_HEAP,TRUE,	TRUE,	TRUE,	0,	0,	0 },
	{ "truncated",	M_TRUNC,FALSE,	FALSE,	FALSE,	0,	0,	0 },
	{ "ordered",	M_ORDER,TRUE,	FALSE,	FALSE,	0,	0,	0 },
	{ 0 }
};

struct mod_info {
	char	outfile[MAX_NAME_SIZE + 4];	/* result file filled by ksort */
	char	formfile[MAX_NAME_SIZE + 4];	/* file with descriptor for ksort */
	char	infile[MAX_NAME_SIZE + 4];	/* input file for ksort (relation itself */
	char	reltemp[MAX_NAME_SIZE + 4];	/* file holding new relation */
	char	spfile[MAX_NAME_SIZE + 4], spflag;	/* isam spool file for overflow */
	char	btree[MAX_NAME_SIZE + 4];	/* file holding temporary btree structure */
	char	temp_sort[MAX_NAME_SIZE + 4];	/* file holding result of special isam
					** required when ordering on a field
					*/
};

struct mod_info	Mod_info;

extern desc_t Btreesec;

/*
**	FILL_REL -- Fill the new relation with tuples from either
**		the old relation or the output file of KSORT.
*/
int
fill_rel(register desc_t *sdesc, register desc_t *desc, char sortit)
{
	register int	i;
	char		tup_buf[MAX_TUP_SIZE], last_tup[MAX_TUP_SIZE]; 
	char		junk[4], newreltype, anytups, chkdups;
	int		need, j, k;
	long		lnum, lid[MAXLID], page, t;
	tid_t		tid, stid, stidlim, ntid, tidpos, btid;
	FILE		*fp, *spfp;
	char		*dp, *sp;
	int		w, temp;
	locator_t	tidloc;

	fp = spfp = (FILE *) NULL;
	k = 0;
	newreltype = M_TYPEOF(desc->d_r.r_spec);
	if (sortit) {
		if ((fp = fopen(Mod_info.outfile, "r")) == NULL)
			syserr("FILL_REL: fopen %.14s", Mod_info.outfile);
	} else {
		cleanrel(sdesc);	/* make sure each page is read fresh */
		find(sdesc, NOKEY, &stid, &stidlim, (void *) NULL);
	}
	if (M_TYPEOF(newreltype) == M_ISAM &&
	    (NLidKeys > 0 || !desc->d_r.r_dim)) {
		lnum = 0;
		stuff_page(&tid, &lnum);
		tid.line_id = 0;
		get_page(desc, &tid);
		concat(ISAM_SPOOL, Fileset, Mod_info.spfile);
		/* assume that spool file is not needed */
		spfp = NULL;
		Mod_info.spflag = FALSE;
		if (F_fac == 0)
			F_fac = 100;
		/* setup d_given field for kcompare later on */
		for (i = 1; i <= desc->d_r.r_attrc; i++)
			desc->d_given[i] = desc->d_iskey[i];
		if (desc->d_r.r_dim) {
			setglobalint(BTREE_FD_NAME, desc->d_btreefd);
		}
	}
	desc->d_addc = 0;
	for (i = 0; i < desc->d_r.r_dim; lid[i++] = 0)
		continue;
	anytups = FALSE;
	chkdups = !sortit && (M_TYPEOF(newreltype) != M_ORDER);
#ifdef xZTR2
	if (tTf(36, 3)) {
		printf("  FILLREL: ");
		printdesc(sdesc);
		printdesc(desc);
	}
#endif
	for (;;) {
		w = (M_TYPEOF(newreltype) == M_ISAM) ?
			sdesc->d_r.r_width + desc->d_r.r_dim * LIDSIZE :
			sdesc->d_r.r_width;
		if (sortit) {
			i = fread(tup_buf, 1, w, fp);
			if (i == 0)
				break;
			if (i != w)
				syserr("FILL_REL: fread A %d", i);
			if (M_TYPEOF(newreltype) == M_HASH && !desc->d_r.r_dim)
				if (fread(junk, 1, 4, fp) != 4)
					syserr("FILL_REL: fread B");
		} else {
#ifdef xZTR2
			if (tTf(36, 1)) {
				printf("FILL_REL: stid ");
				dumptid(&stid);
				printf("FILL_REL: stidlim ");
				dumptid(&stidlim);
			}
#endif
			i = get(sdesc, &stid, &stidlim, tup_buf, TRUE);
#ifdef xZTR2
			if (tTf(36, 1)) {
				printf("FILLREL: get %d ", i);
				printup(sdesc, tup_buf);
			}
#endif
			if (i < 0)
				syserr("FILL_REL: get %d", i);
			if (i == 1)
				break;
		}
		if (M_TYPEOF(newreltype) != M_ISAM ||
		    (M_TYPEOF(newreltype) == M_ISAM &&
		     NLidKeys == 0 &&
		     desc->d_r.r_dim > 0)) {
			for (j = 0; j < desc->d_r.r_dim; ++j) {
				if (j < NLidKeys && j < desc->d_r.r_dim - 1) {
					dp = tup_buf + (sdesc->d_off[LidKey[j]] & I1MASK);
					sp = last_tup + (sdesc->d_off[LidKey[j]] & I1MASK);
					if (!bequal(dp, sp, sdesc->d_len[LidKey[j]]) || !lid[j]) {
						++lid[j];
						for (k = j + 1; k < desc->d_r.r_dim; ++k)
							lid[k] = 0;
						break;
					}
				} else {
					if (!lid[0]) {
						lid[0] = 1;
						if (!(desc->d_r.r_dim -1))
							break;
					}
					++lid[desc->d_r.r_dim - 1];
					break;
				}
			}
			setglobalint(BTREE_FD_NAME, desc->d_btreefd);
			if (!desc->d_r.r_dim || NLidKeys > 0) {
				/* assume unordered so btree inserts done
				** separately */
				temp = 0;
				if (desc->d_r.r_dim > 0) {
					temp = desc->d_r.r_dim;
					desc->d_r.r_dim = 0;
					desc->d_r.r_width -= temp * LIDSIZE;
				}
				if ((i = insert(desc, &tid, tup_buf, chkdups)) < 0) {
					syserr("FILL_REL: insert %d", i);
				}
				if (NLidKeys > 0) {
					bmove(&tid, &stid, LIDSIZE);
					desc->d_r.r_dim = temp;
					desc->d_r.r_width += temp * LIDSIZE;
					insert_mbtree(desc, Mod_info.btree, lid, &tid, &tidpos);
				}
			}
			if (desc->d_r.r_dim && !NLidKeys) {
				/* new relation not changed, only lids added */
				page = RT;
				for (j = 0; j < desc->d_r.r_dim - 1; ++j) {
					if (!lid[j])
						lid[j] = 1;
					if ((t = get_tid(page, lid[j], &tidloc)) < 0) {
						insert_btree(Mod_info.btree, page, lid[j], &ntid, &tidpos, j + 2);
						bmove(&ntid, &page, LIDSIZE);
					}
					else
						bmove(&t, &page, LIDSIZE);
				}
				insert_btree(Mod_info.btree, page, lid[abs(desc->d_r.r_dim) - 1], &stid, &tidpos, FALSE);
			}
			bmove(tup_buf, last_tup, sdesc->d_r.r_width);
			if (desc->d_r.r_dim > 0) {
				dp = tup_buf + desc->d_r.r_width - desc->d_r.r_dim * LIDSIZE;
				bmove(lid, dp, LIDSIZE * desc->d_r.r_dim);
			}
#ifdef xZTR2
			if (tTf(36, 2)) {
				printf("FILL_REL: insert ");
				printup(desc, tup_buf);
				printf("FILL_REL: insert ret %d at", i);
				dumptid(&tid);
			}
#endif
			continue;
		}
		if (anytups)
			i = kcompare(desc, tup_buf, last_tup);
		else {
			anytups = TRUE;
			i = 1;
		}
		bmove(tup_buf, last_tup, desc->d_r.r_width);
		need = canonical(desc, tup_buf);
		if (i == 0 && need > space_left(Acc_head)) {
			/* spool out this tuple. will go on overflow page later */
			if (spfp == NULL) {
				if ((spfp = fopen(Mod_info.spfile, "w")) == NULL)
					syserr("FILL_REL: fopen %.14s", Mod_info.spfile);
				Mod_info.spflag = TRUE;
			}
			if (fwrite(tup_buf, 1, desc->d_r.r_width, spfp) != desc->d_r.r_width)
				syserr("FILL_REL: putb spool");
			continue;
		}
		j = (100 - F_fac) * MAX_TUP_SIZE / 100;
		if (j < need)
			j = need;
		if (i != 0 && j > space_left(Acc_head)) {
			if ((i = add_prim(desc, &tid)) != 0)
				syserr("FILL_REL: force ovflo %d", i);
		}
		tid.line_id = newlino(need);
		put_tuple(&tid, Acctuple, need);
		if (NLidKeys > 0) {
			bmove(tup_buf + desc->d_r.r_width - LIDSIZE * desc->d_r.r_dim, lid, LIDSIZE * desc->d_r.r_dim);
			page = RT;
			for (j = 0; j < desc->d_r.r_dim; ++j) {
				if ((t = get_tid(page, lid[j], &tidloc)) < 0)
					syserr("get_tid error in modify isam ordered");
				page = t;
			}
			stuff_page(&btid, &tidloc.pageno);
			btid.line_id = tidloc.page.node.leafnode.tid_loc[tidloc.offset];
			/* place proper tid in tree */
			replace_btree(tid, &btid);
		}
		desc->d_addc++;
	}
	if (sortit) {
		fclose(fp);
		unlink(Mod_info.outfile);
	}
	if (M_TYPEOF(newreltype) == M_ISAM && desc->d_r.r_dim <= 0) {
		if ((i = pageflush(Acc_head)) != 0)
			syserr("fill_rel:pg clean %d", i);
		if (spfp != NULL)
			fclose(spfp);
	}
	if (!desc->d_r.r_dim || NLidKeys > 0)
		desc->d_r.r_tupc = desc->d_addc;
	desc->d_addc = 0;
	return (0);
}

/*
**	GETFILL -- Get fill factor and minimum pages parameters
**		from argument list, convert them from ascii to integer
**		and store them in global variables.  If the global
**		variable for the corresponding parameter is zero,
**		it means that that parameter is not allowed and an
**		error is generated.
*/

/*ARGSUSED*/
int
getfill(desc_t *d, register paramv_t *pv, char *rel, struct modtab *mp)
{
	register char		*p1;
	register int		err;
	char			*p2;
	int			i, j;
	int			fill_flag, min_flag, max_flag, lid_flag[MAXLID];

	err = 0;
	fill_flag = min_flag = max_flag = FALSE;
	for (i = 0; i < d->d_r.r_dim; ++i)
		lid_flag[i] = FALSE;

	while ((p1 = (pv++)->pv_val.pv_str) != NULL) {
		p2 = (pv++)->pv_val.pv_str;
		if (strcmp(p1, "fillfactor") == 0) {
			if (F_fac == 0 || fill_flag) {
				err = NOTALLOWED;
				break;
			}
			p1 = p2;
			F_fac = atoi(p1);
			if (F_fac > 100 || F_fac < 1) {
				err = FILLBOUND;
				break;
			}
			fill_flag = TRUE;
			continue;
		}
		if (strcmp(p1, "minpages") == 0) {
			if (Mn_pages == 0 || min_flag) {
				err = NOTALLOWED;
				break;
			}
			p1 = p2;
			Mn_pages = atoi(p1);
			if (Mn_pages < 1) {
				err = MINPGBOUND;
				break;
			}
			if (max_flag && (Mn_pages > Mx_pages)) {
				err = MINGTMAX;
				break;
			}
			min_flag = TRUE;
			continue;
		}
		if (strcmp(p1, "maxpages") == 0) {
			if (Mx_pages == 0 || max_flag) {
				err = NOTALLOWED;
				break;
			}
			p1 = p2;
			Mx_pages = atoi(p1);
			if (Mx_pages < 1) {
				err = MAXPGBOUND;
				break;
			}
			if (min_flag && (Mn_pages > Mx_pages)) {
				err = MINGTMAX;
				break;
			}
			max_flag = TRUE;
			continue;
		}
		for ( i  = 1; i <= d->d_r.r_dim && !err; ++i)
			if (strcmp(p1, ztack("lid", iocv(i))) == 0) {
				if (lid_flag[i-1] || *Lid[i-1] == 0) {
					err = NOTALLOWED;
					break;
				}
				for (j = 0; j < d->d_r.r_dim; ++j)
					if (i - 1 != j && strcmp(p2, Lid[j]) == 0 && lid_flag[j]) {
						err = NOTALLOWED;
						break;
					}
				p1 = p2;
				smove(p1, Lid[i - 1]);
				lid_flag[i - 1] = TRUE;
				break;
			}
			if (err)
				break;
		if (i <= d->d_r.r_dim)
			continue;
		err = NEEDFILL;
		break;
	}
	if (err) {
		closeall(0l, 0l);
		return (error(err, rel, p1, 0));
	}
	return (0);
}

/*
**	MODSEQKEY - verify that sequence specified is valid
**
**	Parameters:
**		domain - list of domains
**		relname - relation name
**		seq_ok - whether it is ok to specify the sequence
**				ascending or descending
**
**	Return Codes:
**		0 - ok
**		> 0 - error in sequence specified
**
**	Called by:
**		getkeys
*/
int
modseqkey(char *domain, char *relname, int seq_ok)
{
	register char	*cp, c;
	register int	ret;

	ret = 0;

	for (cp = domain; (c = *cp++) != 0 ; )
		if (c == ':')
			break;

	if (c != '\0') {
		/* replace ":" with null */
		*(cp - 1) = '\0';

		/* verify sequence is valid */
		if (!seq_ok) {
			closeall(0l, 0l);
			ret = error(BADSEQSPEC, relname, cp, domain, 0);
		} else if (strcmp("descending", cp) == 0 || strcmp("d", cp) == 0)
			ret = -1;
		else if (!(strcmp("ascending", cp) == 0 || strcmp("a", cp) == 0)) {
			closeall(0l, 0l);
			ret = error(INVALIDSEQ, relname, cp, domain, 0);
		}
	}

	return (ret);
}

/*
**	GETKEYS - get key domains information
**
**	Parameters:
**		ppv - parameter vector with info about keys
**		relname - relation name
**		d - new descriptor for the relation
**		mp - mod table
**
**	Return Codes:
**		0 - ok
**		>0 - error from modseqkey		
*/
int
getkeys(paramv_t **ppv, char *relname, register desc_t *d, struct modtab *mp)
{
	register paramv_t		*pv;
	register char		*cp;
	int			namemode, sort_only, as_ds;
	int			i, keyno, keywid;
	attr_t	attkey, atttup;
	tid_t			tid;
	extern desc_t		Attdes;

	pv = *ppv;	/* copy list of params */
	namemode = 0;
	if (mp->newrelspec != M_ORDER)
		/* zero key info (ordering does not change keyed fields) */
		for (i = 0; i <= d->d_r.r_attrc; i++)
			d->d_iskey[i] = 0;
	for (i = 0; i <= d->d_r.r_attrc; ++i)
		d->d_given[i] = 0;

	/* determine whether there are any keys at all */
	keywid = 0;
	keyno = 0;
	sort_only = FALSE;
	cp = pv->pv_val.pv_str;

	if (cp == NULL || *cp == 0) {
		/* no key information. default as needed */
		if (mp->yeskeys && mp->newrelspec != M_ORDER) {
			cp = "\1";	/* default to first key */
			namemode = FALSE;
		} else
			pv++;	/* point one to far */
	} else {
		/* check for name mode */
		if ((namemode = (strcmp(cp, "name") == 0)) != 0) {
			/* check attribute names, and convert them to numbers */
			opencatalog("attribute", OR_READ);
			ingres_setkey(&Attdes, &attkey, Mod_info.infile, ATTRELID);
			ingres_setkey(&Attdes, &attkey, Usercode, ATTOWNER);
		}
		pv++;	/* inc to next key */
		cp = (pv++)->pv_val.pv_str;
	}

	/* scan for attribute names */
	for (; cp != NULL; cp = (pv++)->pv_val.pv_str) {
		/* check for separator between keys & options */
		if (*cp == 0) {
			pv++;	/* point two past NULL */
			break;
		}

		if (NLidKeys >= d->d_r.r_dim && mp->newrelspec == M_ORDER) {
			/* more than one field specified as ordering key */
			closeall(0l, 0l);
			return(error(TOOMANYORDKEYS, relname, 0));
		}

		if (namemode) {
			/* check for "sort only" attribute */
			if (*cp == '#') {
				cp++;	/* inc to start of name */
				sort_only = TRUE;
			}

			/* check for ascending/descending modifier */
			if ((as_ds = modseqkey(cp, relname, mp->yes_seq)) > 0)
				return (as_ds);	/* error */

			ingres_setkey(&Attdes, &attkey, cp, ATTNAME);
			i = getequal(&Attdes, &attkey, &atttup, &tid);
			if (i < 0)
				syserr("MODIFY: geteq(att) %d", i);
			if (i > 0) {
				closeall(0l, 0l);
				return (error(INVALIDATTR, relname, cp, 0));	/* bad att name */
			}
			i = atttup.a_id;
			if (i > d->d_r.r_attrc) {
				/* attempting to key on lid field which will be
				** removed
				*/
				closeall(0l,0l);
				return(error(ATTRREMV, relname, cp, 0));
			}
		} else {
			i = *cp;
			as_ds = 0;
		}

		keyno++;
		/* add new key to descriptor */
		if (mp->newrelspec == M_ORDER)
			LidKey[NLidKeys++] = i;
		if (!sort_only && mp->newrelspec != M_ORDER) {
			d->d_iskey[i] = keyno;
			keywid += (d->d_len[i] & I1MASK);
		}
		if (d->d_given[i]) {
			closeall(0l, 0l);
			return (error(DUPKEY, relname, cp, 0));	/* duplicate attribute */
		}
		d->d_given[i] = as_ds == 0 ? keyno : -keyno;
	}
	pv--;	/* back up one to point to "-1" terminator */


	if (M_TYPEOF(d->d_r.r_spec) == M_ISAM && keywid > (MAX_TUP_SIZE / 2 - 4)) {
		closeall(0l, 0l);
		return (error(TOOWIDEISAM, relname, iocv(keywid), 0));
	}

	/* if a heap, there can be no keys */
	if (!mp->yeskeys && keyno != 0) {
		closeall(0l, 0l);
		return (error(NOKEYSHEAP, relname, mp->type, 0));	/* no keys allowed on heap */
	}
	/* fill out default sort on remainder of keys */
	if (mp->yeskeys)
		for (i = 1; i <= d->d_r.r_attrc; i++)
			if (d->d_given[i] == 0)
				d->d_given[i] = ++keyno;
	*ppv = pv;
	return (0);
}

/*
**  MAKE_NEWREL -- Create a file for the modified relation
**	and build one or more primary pages for the
**	relation based on its storage structure and the
**	number of tuples it must hold.
*/
int
make_newrel(register desc_t *desc)
{
	register int	tups_p_page;
	int		width;

	concat(MODTEMP, Fileset, Mod_info.reltemp);
	close(open(Mod_info.reltemp, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE));
	if ((desc->d_fd = open(Mod_info.reltemp, O_RDWR)) < 0)
		syserr("MAKE_NEWREL: open %.14s %d", Mod_info.reltemp, desc->d_fd);
	desc->d_opened = (desc->d_fd + 1) * -5;
	desc->d_r.r_primc = 1;
	if (M_TYPEOF(desc->d_r.r_spec) == M_HASH && F_fac > 0 && Mn_pages > 0) {
		/*
		** Determine the number of primary pages. The following
		** first determines the number of tuples/page which the
		** relation should have in order to get the requested
		** fillfactor. Then that number is divided into the
		** number of tuples to get the number of primary pages.
		** To avoid round off, it must guaranteed that the
		** number of tuples per page must be at least 1.
		**
		** primary_pages = #tuples / (#tuples/page * fillfactor)
		*/
		width = desc->d_r.r_width + 2 - LIDSIZE * desc->d_r.r_dim;
		tups_p_page = (((MAX_TUP_SIZE+2) / width) * F_fac) / 100;
		if (tups_p_page == 0)
			tups_p_page = 1;
		 /* we add one to simulate a ceiling function */
		desc->d_r.r_primc = desc->d_r.r_tupc / tups_p_page + 1;
		if (desc->d_r.r_primc < Mn_pages)
			desc->d_r.r_primc = Mn_pages;
		if (Mx_pages > 0 && desc->d_r.r_primc > Mx_pages)
			desc->d_r.r_primc = Mx_pages;
#ifdef xZTR1
		if (tTf(36, 0))
			printf("using %ld prim pages\n", desc->d_r.r_primc);
#endif
	}
	desc->d_r.r_tupc = 0;
	return (0);
}

/*
 * loadiparam - load an integer parameter -
 * doesn't matter about byte-ordering, or size of integer etc.
 */
void
loadiparam(int i)
{
	setp(PV_INT, &i, sizeof(i));
}

/*
**	SORTREL - Call KSORT to sort the given relation.  SORTREL
**		sets up the descriptor struct specifying the sort
**		keys and tells KSORT whether or not the hash key should
**		be included as a sort key.
*/
int
sortrel(desc_t *odesc, register desc_t *desc)
{
	register int	i;
	desc_t		tempdesc;
	char		*temp;
	int		len;

	concat(ISAM_SORTED, Fileset, Mod_info.outfile);
	if (close(open(Mod_info.outfile, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE)))
		syserr("SORTREL: creat %.14s", Mod_info.outfile);
	bmove(odesc, &tempdesc, sizeof(*odesc));
	for (i = 1; i <= desc->d_r.r_attrc; ++i) {
		/* set up temp descriptor for ksort with new keyed fields */
		tempdesc.d_iskey[i] = desc->d_iskey[i];
		tempdesc.d_given[i] = desc->d_given[i];
	}

	if (M_TYPEOF(desc->d_r.r_spec) == M_HASH && !desc->d_r.r_dim) {
		/* sort on hash bucket first,
		   (if ordering sort on ordering key, not bucket) */
		tempdesc.d_given[0] = 1;
		for (i = 1; i <= desc->d_r.r_attrc; i++)
			tempdesc.d_given[i]++;
	}

#ifdef xZTR2
	if (tTf(36, 4)) {
		printf("sortrel: ");
		printdesc(&tempdesc);
	}
#endif

	/* flush buffers used by modify so that ksort can't look at them */
	flush_rel(desc, TRUE);
	resetacc(NULL);

	/* copy Fileset so it can't get trashed */

	len = strlen(Fileset) + 1;
	temp = (char *) need(Qbuf, len);
	bmove(Fileset, temp, len);

	initp();
	setp(PV_STR, temp, 0);
	setp(PV_STR, Mod_info.infile, 0);
	setp(PV_STR, Mod_info.outfile, 0);

	/* Descriptor for new relation */
	setp(PV_STR, tempdesc.d_r.r_id, 0);
	setp(PV_STR, tempdesc.d_r.r_owner, 0);
	loadiparam(tempdesc.d_r.r_spec);
	loadiparam(tempdesc.d_r.r_indexed);
	loadiparam(tempdesc.d_r.r_status2);
	loadiparam(tempdesc.d_r.r_status);
	loadiparam(tempdesc.d_r.r_savetime);
	loadiparam(tempdesc.d_r.r_tupc);
	loadiparam(tempdesc.d_r.r_attrc);
	loadiparam(tempdesc.d_r.r_width);
	loadiparam(tempdesc.d_r.r_primc);
	loadiparam(tempdesc.d_r.r_free);
	loadiparam(tempdesc.d_r.r_modtime);
	loadiparam(tempdesc.d_r.r_dim);

	setp(PV_STR, tempdesc.d_rangevar, 0);
	loadiparam(tempdesc.d_fd);
	loadiparam(tempdesc.d_opened);
	loadiparam(tempdesc.d_addc);
	loadiparam(tid2int(tempdesc.d_tid));
	for (i = 0; i <= tempdesc.d_r.r_attrc; ++i) {
		loadiparam(tempdesc.d_off[i]);
		loadiparam(tempdesc.d_fmt[i]);
		loadiparam(tempdesc.d_len[i]);
		loadiparam(tempdesc.d_iskey[i]);
		loadiparam(tempdesc.d_given[i]);
	}

	if (tempdesc.d_r.r_dim > 0) {
		setp(PV_STR, odesc->d_btree->d_r.r_id, 0);
		setp(PV_STR, odesc->d_btree->d_r.r_owner, 0);
		loadiparam(odesc->d_btree->d_r.r_spec);
		loadiparam(odesc->d_btree->d_r.r_indexed);
		loadiparam(odesc->d_btree->d_r.r_status2);
		loadiparam(odesc->d_btree->d_r.r_status);
		loadiparam(odesc->d_btree->d_r.r_savetime);
		loadiparam(odesc->d_btree->d_r.r_tupc);
		loadiparam(odesc->d_btree->d_r.r_attrc);
		loadiparam(odesc->d_btree->d_r.r_width);
		loadiparam(odesc->d_btree->d_r.r_primc);
		loadiparam(odesc->d_btree->d_r.r_free);
		loadiparam(odesc->d_btree->d_r.r_modtime);
		loadiparam(odesc->d_btree->d_r.r_dim);

		setp(PV_STR, odesc->d_btree->d_rangevar, 0);
		loadiparam(odesc->d_btree->d_fd);
		loadiparam(odesc->d_btree->d_opened);
		loadiparam(odesc->d_btree->d_addc);
		loadiparam(tid2int(odesc->d_btree->d_tid));

		for (i = 0; i <= odesc->d_btree->d_r.r_attrc; ++i) {
			loadiparam(odesc->d_btree->d_off[i]);
			loadiparam(odesc->d_btree->d_fmt[i]);
			loadiparam(odesc->d_btree->d_len[i]);
			loadiparam(odesc->d_btree->d_iskey[i]);
			loadiparam(odesc->d_btree->d_given[i]);
		}
	}

	call(mdKSORT, NULL);

	/* flush buffers used by ksort so that modify can't look at them */
	flush_rel(desc, TRUE);
	resetacc(NULL);

#ifdef xZTR1
	if (tTf(36,9))
		printf("SORTREL: done calling ksort\n");
#endif
	return (0);
}

/*
**	SORT_ISAM -- Sorts an isam relation back to its original order
**	so that it will be inserted into the relation in the proper order.
**	It is presently not in order because it has been sorted according
**	to a specified field for ordering.
*/
void
sort_isam(desc_t *sdesc, desc_t *desc)
{
	long		lid[MAXLID];
	register int	i, j, k;
	char		tup_buf[MAX_TUP_SIZE], last_tup[MAX_TUP_SIZE], *dp, *sp;
	FILE		*sfp, *fp;
	tid_t		tid, tidpos;
	desc_t		tempdesc;
	int		w;

	if (desc->d_r.r_dim > 0) {
		setglobalint(BTREE_FD_NAME, desc->d_btreefd);
	}
	concat(STEMP, Fileset, Mod_info.temp_sort);
	if ((sfp = fopen(Mod_info.temp_sort, "w")) == NULL) {
		syserr("sort_isam: can't open %s", Mod_info.temp_sort);
	}
	if ((fp = fopen(Mod_info.outfile, "r")) == NULL) {
		syserr("sort_isam: can't open %s", Mod_info.outfile);
	}
	for (i = 0; i < desc->d_r.r_dim; lid[i++] = 0) {
	}
	/* create input file for sort with proper lid attached to each tuple */
	w = sdesc->d_r.r_width - LIDSIZE * sdesc->d_r.r_dim;
	for ( ; ; ) {
		i = fread(tup_buf, 1, sdesc->d_r.r_width, fp);
		if (i == 0)
			break;
		if (i != sdesc->d_r.r_width)
			syserr("sort_isam: read error in %s", Mod_info.outfile);
		for (j = 0; j < desc->d_r.r_dim; ++j)
			if (j < NLidKeys && j < desc->d_r.r_dim - 1) {
				dp = tup_buf + (sdesc->d_off[LidKey[j]] & I1MASK);
				sp = last_tup + (sdesc->d_off[LidKey[j]] & I1MASK);
				if (!bequal(dp, sp, sdesc->d_len[LidKey[j]]) || !lid[j]) {
					++lid[j];
					for (k = j + 1; k < desc->d_r.r_dim; ++k)
						lid[k] = 0;
					break;
				}
			} else {
				if (!lid[0]) {
					lid[0] = 1;
					if (!(desc->d_r.r_dim - 1))
						break;
				}
				++lid[desc->d_r.r_dim - 1];
				break;
			}
		bmove(tup_buf, last_tup, sdesc->d_r.r_width);
		/* reserve a slot in btree for tuple */
		insert_mbtree(desc, Mod_info.btree, lid, &tid, &tidpos);
		bmove(lid, tup_buf + w, LIDSIZE * desc->d_r.r_dim);
		if (fwrite(tup_buf, 1, sdesc->d_r.r_width + LIDSIZE * desc->d_r.r_dim, sfp) != sdesc->d_r.r_width + LIDSIZE * desc->d_r.r_dim)
			syserr("sort_isam: write error in %s", Mod_info.temp_sort);
	}
	fclose(fp);
	fclose(sfp);
	/* set up new descriptor accounting for lid field */
	bmove(sdesc, &tempdesc, sizeof(*sdesc));
	tempdesc.d_r.r_spec = M_ORDER;
	for (i = 0; i < desc->d_r.r_dim; ++i) {
		tempdesc.d_r.r_width += LIDSIZE;
		++tempdesc.d_r.r_attrc;
		tempdesc.d_off[tempdesc.d_r.r_attrc] = tempdesc.d_r.r_width - LIDSIZE;
		tempdesc.d_fmt[tempdesc.d_r.r_attrc] = INT_CONST;
		tempdesc.d_len[tempdesc.d_r.r_attrc] = LIDSIZE;
	}
	j = 0;
	/* use old keying attributes for specifying sort order */
	clearkeys(&tempdesc);
	for (i = 1; i <= sdesc->d_r.r_attrc; ++i)
		if (sdesc->d_iskey[i]) {
			tempdesc.d_given[i] = sdesc->d_iskey[i];
			++j;
		}
	for (i = 1; i <= tempdesc.d_r.r_attrc; ++i)
		if (!tempdesc.d_given[i])
			tempdesc.d_given[i] = ++j;
	sortfile(Mod_info.temp_sort, &tempdesc, FALSE);
	if (unlink(Mod_info.outfile) < 0)
		syserr("can't unlink %s", Mod_info.outfile);
	if (link(ztack(REPL_OUT, Fileset), Mod_info.outfile) == -1)
		syserr("can't link %s to %s", ztack(REPL_OUT, Fileset), Mod_info.outfile);
	if (unlink(Mod_info.temp_sort) < 0)
		syserr("sort_isam: can't unlink %s", Mod_info.temp_sort);
	if (unlink(ztack(REPL_OUT, Fileset)) < 0)
		syserr("sort_isam: can't unlink replout file");
}
/*
**	BLDINDEX -	
**
**	Parameters:
**		d - descriptor for relation
**
**	Return Codes:
**		0 - ok
**		<0 - error
**
**	Trace Flags:
**		Z38.7, Z38.8
**
**	Called by:
**		modify
**
*/
int
bldindex(register desc_t *d)
{
	register tid_t	*tid;
	register int	tmp;
	tid_t		tidx;
	accbuf_t	dirbuf;
	int		keywid, level, savespec, keyx[MAX_DOMAINS];
	int		offset, len;
	char		tuple[MAX_TUP_SIZE], temptup[MAX_TUP_SIZE], *key;
	long		pageid, start, stop, newstart, newstop;

	tid = &tidx;
	keywid = 0;
	for (tmp = 0; tmp < MAX_DOMAINS; tmp++)
		keyx[tmp] = 0;
	for (tmp = 1; tmp <= d->d_r.r_attrc; tmp++)
		if (d->d_iskey[tmp] > 0) {
			keyx[d->d_iskey[tmp] - 1] = tmp;
			keywid += d->d_len[tmp] & I1MASK;
		}

	/* Determine the last page of the relation. This will
	** only work if all pages have been written out. Fill_rel
	** must guarantee that all pages have been written
	*/
	level = 0;
	last_page(d, tid, 0);
	pluck_page(tid, &stop);
	start = 0;
	dirbuf.am_fd = d->d_fd;
	(void) memcpy(&dirbuf.am_rel, &d->d_tid, sizeof(d->d_tid));
	savespec = d->d_r.r_spec;
	for (;;) {
#ifdef xZTR2
		if (tTf(38, 7))
			printf("isam: level %d\n", level);
#endif
		dirbuf.am_overflowpg = start;
		dirbuf.am_mainpg = level;
		dirbuf.am_curpg = stop + 1;
		dirbuf.am_linev[0] = (short) (dirbuf.am_tup1 - (char *) &dirbuf);
		offset = dirbuf.am_linev[0];
		dirbuf.am_flags = BUF_DIRTY | BUF_DIRECT;

		dirbuf.am_nextline = 0;
		newstart = stop + 1;
		newstop = newstart;
		for (pageid = start; pageid <= stop; pageid++) {
#ifdef xZTR2
			if (tTf(38, 8))
				printf("isam:get key from %ld\n", pageid);
#endif
			stuff_page(tid, &pageid);
			tid->line_id = 0;
			if ((tmp = get(d, tid, tid, tuple, FALSE)) != 0) {
				/*
				** If the relation is empty, then page 0 will
				** return AMINVL_ERR on a get(). Form a blank tuple
				** and use it to create a one tuple directory
				*/
				if (pageid == 0 && tmp == AMINVL_ERR) {
					clr_tuple(d, tuple);
				} else {
					return (-2);
				}
			}

			/*
			** If this is the first level then form the tuple
			** from the mainpage of the relation. Otherwise
			** the tuple is the first tuple of a directory page
			** and it is already correctly formed.
			*/
			if (level == 0) {
				key = temptup;
				for (tmp = 0; keyx[tmp] != 0; tmp++) {
					len = d->d_len[keyx[tmp]] & I1MASK;
					bmove(&tuple[d->d_off[keyx[tmp]]], key, len);
					key += len;
				}
				key = temptup;
			}
			else
				key = tuple;

			if (keywid > space_left(&dirbuf)) {
				if (pageflush(&dirbuf))
					return (-3);
				dirbuf.am_curpg++;
				newstop = dirbuf.am_curpg;
				dirbuf.am_overflowpg = pageid;
				dirbuf.am_linev[0] = (short) (dirbuf.am_tup1 - (char *) &dirbuf);
				offset = dirbuf.am_linev[0];
				dirbuf.am_flags = BUF_DIRTY;
				dirbuf.am_nextline = 0;
			}
			/* copy key to directory page */
			bmove(key, (char *) &dirbuf + offset, keywid);

			/* update next line number */
			offset += keywid;
			dirbuf.am_nextline++;
			dirbuf.am_linev[-dirbuf.am_nextline] = offset;
		}
		if (pageflush(&dirbuf))
			return (-4);
		if (newstart == newstop)
			break;
		d->d_r.r_spec = M_TYPEOF(d->d_r.r_spec);
		level++;
		start = newstart;
		stop = newstop;
	}
	d->d_r.r_spec = savespec;
	d->d_r.r_primc = newstart;
	return (0);
}
/*
**	UNSPOOL -- Take tuples saved in spool file and insert them
**		in new relation.  This is only for ISAM relations.
*/
int
unspool(register desc_t *sdesc, register desc_t *desc)
{
	register int	i, j;
	tid_t		tid, btid;
	char		tup_buf[MAX_TUP_SIZE];
	FILE		*spfp;
	long		lid[MAXLID], page, t;
	int		w;
	locator_t  tidpos;

	w = sdesc->d_r.r_width + desc->d_r.r_dim * LIDSIZE;
	if (Mod_info.spflag) {
		if ((spfp = fopen(Mod_info.spfile, "r")) == NULL)
			syserr("UNSPOOL: fopen spool");
		while ((i = fread(tup_buf, 1, w, spfp)) == w) {
			if ((i = insert(desc, &tid, tup_buf, FALSE)) < 0)
				syserr("UNSPOOL: insert %.14s %d", desc->d_r.r_id, i);
			if (NLidKeys > 0) {
				bmove(tup_buf + desc->d_r.r_width - LIDSIZE * desc->d_r.r_dim, lid, LIDSIZE * desc->d_r.r_dim);
				page = RT;
				for (j = 0; j < desc->d_r.r_dim; ++j) {
					if ((t = get_tid(page, lid[j], &tidpos)) < 0)
						syserr("get_tid error in unspool");
					page = t;
				}
				stuff_page(&btid, &tidpos.pageno);
				btid.line_id = tidpos.page.node.leafnode.tid_loc[tidpos.offset];
				replace_btree(tid, &btid);
			}
		}
		if (i != 0)
			syserr("UNSPOOL: read %d", i);
		fclose(spfp);
		unlink(Mod_info.spfile);
	}
	desc->d_r.r_tupc += desc->d_addc;
	desc->d_addc = 0;
	return (0);
}
/*
**	FILL_BATCH -- Create and fill a batch file containing the
**		updates for the system catalog so that MODIFY will
**		be recoverable if the system crashes.
*/
int
fill_batch(desc_t *odesc, register desc_t *desc)
{
	register desc_t		*dessys;
	register int		i, k;
	relation_t		reltup, rkey;
	tid_t			tid, lotid, hitid;
	attr_t	atttup, akey;
	int			numatts, j;
	char			prebatch[MAX_NAME_SIZE + 4], modbatch[MAX_NAME_SIZE + 4];

	if (bequal(desc->d_r.r_id, "relation    ", 12)) {
		clearkeys(desc);
		ingres_setkey(desc, &rkey, desc->d_r.r_id, RELID);
		ingres_setkey(desc, &rkey, desc->d_r.r_owner, RELOWNER);
		if ((i = getequal(desc, &rkey, &reltup, &tid)) != 0)
			syserr("FILL_BATCH: geteq rel rel %d", i);
		bmove(&tid, &desc->d_tid, sizeof(desc->d_tid));
	}
	else
		bmove(&odesc->d_tid, &desc->d_tid, sizeof(desc->d_tid));
	resetacc(Acc_head);
	concat(MOD_PREBATCH, Fileset, prebatch);
	close(open(prebatch, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE));
	if ((Batch_fp = open(prebatch, O_RDWR)) < 0)
		syserr("FILL_BATCH: open %.14s %d", prebatch, Batch_fp);
	smove(Fileset, Batchbuf.b_file);
	Batch_cnt = 0;
	wrbatch(desc, sizeof(*desc));
	if (bequal(desc->d_r.r_id, "attribute   ", 12))
		dessys = desc;
	else
		dessys = &Admin.ad_attr;
	clearkeys(dessys);
	ingres_setkey(dessys, &akey, desc->d_r.r_id, ATTRELID);
	ingres_setkey(dessys, &akey, desc->d_r.r_owner, ATTOWNER);
	if ((i = find(dessys, EXACTKEY, &lotid, &hitid, &akey)) != 0)
		syserr("FILL_BATCH: find %d", i);

	/* if ordered relation, one of attributes is LID field */
	numatts = j = desc->d_r.r_attrc - desc->d_r.r_dim;

	while(!(i = get(dessys, &lotid, &hitid, &atttup, TRUE)) && j > 0)
		if (!kcompare(dessys, &akey, &atttup))
			if (atttup.a_id <= numatts) {
				j--;
				atttup.a_iskey = desc->d_iskey[atttup.a_id];
				wrbatch(&lotid, sizeof(lotid));
				wrbatch(&atttup, sizeof(atttup));
			}
	for (k = 1; k <= desc->d_r.r_dim; ++k) {
		/* create new tuple corresponding to LID field; LID field is the
		** last field of relation, a 4-byte integer 
		*/
		smove(desc->d_r.r_id, atttup.a_rel);
		bmove(desc->d_r.r_owner, atttup.a_owner, sizeof(desc->d_r.r_owner));
		atttup.a_id = numatts + k;
		smove(Lid[k - 1], atttup.a_name);
		pad(atttup.a_name, MAX_NAME_SIZE);
		atttup.a_off = desc->d_r.r_width - (desc->d_r.r_dim - k + 1) * LIDSIZE;
		atttup.a_fmt = INT_CONST;
		atttup.a_len = LIDSIZE;
		atttup.a_iskey = 0;
		wrbatch(&atttup, sizeof(atttup));
	}
	if (i < 0 || j > 0)
		syserr("FILL_BATCH: get att %d count %d", i, j);
	/* get rid of attribute pages */
	cleanrel(dessys);
	flushbatch();
	close(Batch_fp);
	concat(MODBATCH, Fileset, modbatch);
	if (link(prebatch, modbatch) == -1)
		syserr("FILL_BATCH: can't link %.14s %.14s",
			prebatch, modbatch);
	unlink(prebatch);
	return (0);

}

/*
**	MAKE_BSEC -- Creates the seecondary btree relation by first creating
**		a heaped relation.  The main relation tids are found by
**		scanning the leaves of the btree.  The relation is then 
**		modified to an isam relation.
*/	
void
make_bsec(char *relname, int dim)
{
	paramv_t		pv[8];
	register int	i;
	desc_t		bdesc;
	tid_t		tid, btid;
	long		mtid, page, t, next;
	char		tuple[2 * LIDSIZE], btree[MAX_NAME_SIZE], btreefile[MAX_NAME_SIZE + 4];
	locator_t	tidpos;
	extern desc_t	Reldes;

	pv[0].pv_val.pv_str = "0000002";
	capital(trim_relname(relname), btree);
	pv[1].pv_val.pv_str = btree;
	pv[2].pv_val.pv_str = "mtid";
	pv[3].pv_val.pv_str = "i4";
	pv[4].pv_val.pv_str = "btid";
	pv[5].pv_val.pv_str = "i4";
	pv[6].pv_type = PV_EOF;
	if (create(6, pv)) {
		syserr("can't create btreesec %s", pv[1].pv_val.pv_str);
	}
	
	if (noclose(&Reldes)) {
		syserr("noclose in make_bsec");
	}
	if ((i = openr(&bdesc, OR_WRITE, btree)) != 0) {
		syserr("opening bsec relation %d", i);
	}
	btreename(relname, btreefile);
	setglobalint(BTREE_FD_NAME, open(btreefile, O_RDWR));
	if (getglobalint(BTREE_FD_NAME) < 0) {
		syserr("make_bsec: can't open %s", btreefile);
	}
	page = RT;
	for (i = 0; i < dim - 1; ++i) {
		t = get_tid(page, 1, &tidpos);
		if (t < 0)
			break;	/* lid value doesn't exist */
		bmove(&t, &page, LIDSIZE);
	}
	if (t >= 0) {
		/* only do inserts if there are lids! */
		do {
			get_node(page, &tidpos.page);
			next = tidpos.page.nexttree;
			get_tid(page, 1, &tidpos);
			page = tidpos.pageno;
			for (;;) {
				/* scan through leaves of btree */
				stuff_page(&btid, &page);
				for (i = 0; i < tidpos.page.nelmts; ++i) {
					btid.line_id = tidpos.page.node.leafnode.tid_loc[i];
					mtid = tidpos.page.node.leafnode.tid_pos[(int) btid.line_id];
					/* form tuple */
					bmove(&mtid, tuple, LIDSIZE);
					bmove(&btid, tuple + LIDSIZE, LIDSIZE);
					if (insert(&bdesc, &tid, tuple, TRUE) < 0)
						syserr("insert error in btreesec");
				}
				page = tidpos.page.node.leafnode.nextleaf;
				if (page == 0) {
					break;
				} else {
					get_node(page, &tidpos.page);
				}
			}
		} while ((page = next) != 0);
	}
	close(getglobalint(BTREE_FD_NAME));
	closer(&bdesc);

	/* modify to isam on mtid */
	pv[0].pv_val.pv_str = btree;
	pv[1].pv_val.pv_str = "isam";
	pv[2].pv_val.pv_str = "name";
	pv[3].pv_val.pv_str = "mtid";
	pv[4].pv_val.pv_str = "\0";
	pv[5].pv_val.pv_str = "fillfactor";
	/* use fillfactor provided for main relation */
	if (F_fac == 0)
		pv[6].pv_val.pv_str = iocv(80);
	else
		pv[6].pv_val.pv_str = iocv(F_fac);
	pv[7].pv_type = PV_EOF;
	if (modify(7, pv))
		syserr("can't modify btreesec to isam");
}

int
modify(int pc, paramv_t *pv)
{
	register int		i, j;
	register char		*rname;
	register struct modtab	*mp;
	struct modtab		*p;
	int			sorted, dim;
	desc_t			dold, dnew;
	tid_t			temptid;
	extern int		Noupdt;
	extern desc_t		Attdes;
	attr_t			atttup, attkey;
	tid_t			tid;
	int			numatts;

	
#ifdef xZTR1
	if (tTf(34, -1)) {
		printf("enter modify\n");
		prvect(pc, pv);
	}
#endif

	pv[pc].pv_val.pv_str = NULL;

	/* check for nice parameters */
	if (pc < 2)
		syserr("MODIFY: pc %d", pc);

	/* save relation name for error messages */
	rname = (pv++)->pv_val.pv_str;	/* *pv now pointes to storage spec */

	/* check for good relation */
	i = openr(&dold, OR_READ, rname);
	if (i == AMOPNVIEW_ERR)
		return (error(NOMODVIEW, rname, 0));
	if (i > 0)
		/* reln does not exist */
		return (error(NOREL, rname, 0));	
	else if (i < 0)
		syserr("MODIFY: openr (%.14s) %d", rname, i);
	/* can only modify a relation you own and isn't a sys rel */
	
	if (!bequal(Usercode, dold.d_r.r_owner, USERCODE_SIZE)) {
		i = NOOWN;
	}
	if ((dold.d_r.r_status & S_CATALOG) && Noupdt) {
		i = NOMODSYSREL;
	}
	if (i) {
		closer(&dold);
		return (error(i, rname, 0));
	}

	/*
	** Form descriptor for new relation. Here we need to
	** separate the pages from the old and new relations.
	** Since pages are identified by the tid_t of the relation
	** relation tuple, both old and new have the same identifiers.
	** To avoid this problem, a special tid_t is hand crafted for
	** the new relation.
	*/
	bmove(&dold, &dnew, sizeof(dnew));
	dnew.d_tid.line_id = (char) -2;	/* choose impossible d_tid */
	/* assume new relation isn't ordered */
	if (dold.d_r.r_dim) {
		dnew.d_r.r_attrc -= dold.d_r.r_dim;
		dnew.d_r.r_width -= dold.d_r.r_dim * LIDSIZE;
		dnew.d_r.r_dim = 0;
	}

	/* In case of an interrupt from a previous modify,
	** there might be pages around. Get rid of them.
	*/
	cleanrel(&dnew);

	ingresname(dold.d_r.r_id, dold.d_r.r_owner, Mod_info.infile);
	dim = 0;
	NLidKeys = 0;

	/* scan for entry in relspec table */
	for (mp = Modtab; mp->type; mp++) {
		if (bequal(mp->type, pv->pv_val.pv_str, 7) &&
		    bequal("ordered", pv->pv_val.pv_str, 7)) {
			if ((dim = atoi(pv->pv_val.pv_str + 7)) <= 0 ||
			    dim > MAXLID) {
				closer(&dold);
				return(error(BADORDDIM, rname, iocv(dim), 0));
			}
			break;
		}
		if (strcmp(mp->type, pv->pv_val.pv_str) == 0) {
			break;
		}
	}

	/* if not found, error */
	if (!mp->type) {
		closer(&dold);
		return (error(BADSTORAGE, rname, pv->pv_val.pv_str, 0));
		/* bad relspec */
	}

	if (M_TYPEOF(mp->newrelspec) == M_ORDER &&
	    dold.d_r.r_indexed == SECINDEX) {
		/* can't order an index relation */
		closer(&dold);
		return(error(NOORDINDX, rname,0));
	}

	if (M_TYPEOF(mp->newrelspec) == M_ORDER) {
		dnew.d_r.r_dim = dim;
		for (i = 0; i < dim; ++i) {
			++dnew.d_r.r_attrc;
			dnew.d_iskey[dnew.d_r.r_attrc] = 0;
			dnew.d_off[dnew.d_r.r_attrc] = dnew.d_r.r_width;
			dnew.d_fmt[dnew.d_r.r_attrc] = INT_CONST;
			dnew.d_len[dnew.d_r.r_attrc] = LIDSIZE;
			dnew.d_r.r_width += LIDSIZE;
		}
		concat(BTREE, Fileset, Mod_info.btree);
		create_btree(Mod_info.btree);
		dnew.d_btreefd = getglobalint(BTREE_FD_NAME);
		/* ok to order ascending/descending */
		mp->yes_seq = TRUE;
	} else {
		dnew.d_r.r_spec = mp->newrelspec;
		if (dold.d_r.r_dim) {
			dold.d_r.r_attrc -= dold.d_r.r_dim;
			dold.d_r.r_width -= dold.d_r.r_dim * LIDSIZE;
			dold.d_r.r_dim = 0;
			closer(dold.d_btree);
			close(dold.d_btreefd);
		}
	}

	if (M_TYPEOF(dnew.d_r.r_spec) == M_TRUNC)
		dnew.d_r.r_spec = M_HEAP;

	pv++;	/* now points to first parameter */

	/* get the key domains information */
	if ((i = getkeys(&pv, rname, &dnew, mp)) > 0) {
		closer(&dold);
		return (i);	/* user error */
	}

	j = 0;
	for (i = 0; i < NLidKeys; ++i)
		if (LidKey[i] > dold.d_r.r_attrc - dold.d_r.r_dim) {
			j = 1;
			break;
		}

	if (!j && dold.d_r.r_dim) {
		/* treat old relation as if not ordered since lid field not needed */
		dold.d_r.r_attrc -= dold.d_r.r_dim;
		dold.d_r.r_width -= dold.d_r.r_dim * LIDSIZE;
		dold.d_r.r_dim = 0;
		closer(dold.d_btree);
		close(dold.d_btreefd);
	}

	if (!dnew.d_r.r_dim || !NLidKeys) {
		F_fac = mp->f_fac;
		Mn_pages = mp->mn_pages;
		Mx_pages = mp->mx_pages;
	} else {
		/* set parameters to that of storage type of relation to be ordered */
		for (p = Modtab; p->type; p++)
			if (dnew.d_r.r_spec == p->newrelspec)
				break;
		F_fac = p->f_fac;
		Mn_pages = p->mn_pages;
		Mx_pages = p->mx_pages;
	}

	if (M_TYPEOF(mp->newrelspec) != M_ORDER)
		for (i = 0; i < dnew.d_r.r_dim; ++i)
			Lid[i][0] = 0;
	else
		for (i = 1; i <= dnew.d_r.r_dim; ++i)
			concat("lid", iocv(i), Lid[i-1]);

	/* get fillfactor and other options if any */
	if ((i = getfill(&dnew, pv, rname, mp)) != 0) {
		closer(&dold);
		return (i);	/* user error */
	}

	/* check for duplicate attribute name */
	if (M_TYPEOF(mp->newrelspec) == M_ORDER) {
		opencatalog("attribute", OR_READ);
		ingres_setkey(&Attdes, &attkey, dnew.d_r.r_id, ATTRELID);
		ingres_setkey(&Attdes, &attkey, dnew.d_r.r_owner, ATTOWNER);
		numatts = dold.d_r.r_attrc - dold.d_r.r_dim;
		for (i = 0; i < dnew.d_r.r_dim; ++i) {
			ingres_setkey(&Attdes, &attkey, Lid[i], ATTNAME);
			if (getequal(&Attdes, &attkey, &atttup, &tid) == 0) {
				if (atttup.a_id <= numatts) {
					/* ok to duplicate attributes that will be removed */
					closer(&dold);
					return(error(INVALIDATTR, rname, Lid[i], 0));
				}
			}
		}
	}

	/* lock the relation relation */
	if (Lockrel) {
		get_p_tid(&dold, &temptid);
		setrll(A_SLP, &temptid, M_EXCL);
	}

	if (!dnew.d_r.r_dim || NLidKeys > 0)
		/* compute new relation parameters & build descriptor */
		make_newrel(&dnew);

	if ((sorted = ((mp->sortit || NLidKeys > 0) &&
	    (dold.d_r.r_tupc != 0))) != 0) {
		sortrel(&dold, &dnew);
		dold.d_r.r_indexed = 0;
	}

	if (!dnew.d_r.r_dim || NLidKeys > 0)
		/* physically create the new relation */
		if (formatpg(&dnew, dnew.d_r.r_primc) != 0)
			syserr("modify: formatpg");

	/* clear d_given field; if heap remove any keys */
	clearkeys(&dnew);

	if (M_TYPEOF(dnew.d_r.r_spec) == M_HEAP)
		for (i = 1; i <= dnew.d_r.r_attrc; i++)
			dnew.d_iskey[i] = 0;

	if (NLidKeys > 0 && M_TYPEOF(dnew.d_r.r_spec) == M_ISAM)
		sort_isam(&dold, &dnew);

	if (M_TYPEOF(mp->newrelspec) != M_TRUNC)
		fill_rel(&dold, &dnew, sorted);

	closer(&dold);	/* error return is impossible */
	if (M_TYPEOF(dnew.d_r.r_spec) == M_ISAM &&
	    (!dnew.d_r.r_dim || NLidKeys > 0)) {
		j = dnew.d_r.r_dim;
		dnew.d_r.r_dim = 0;
		if ((i = bldindex(&dnew)) != 0)
			syserr("bldindex: %.14s %d", dnew.d_r.r_id, i);
		dnew.d_r.r_dim = j;
		unspool(&dold, &dnew);
	}

	/*
	** New relation is now complete. The system relations need to
	** be updated. First destroy all buffers with pages from the
	** new relation.
	*/
	if ((i = cleanrel(&dnew)) != 0)
		syserr("modify:clean new %d,%.14s", i, dnew.d_r.r_id);

	fill_batch(&dold, &dnew);

	/*
	** Close the file for the new relation. This must be
	** done after the fill_batch in case we are modifing
	** the attribute relation.
	*/
	if (!dnew.d_r.r_dim || NLidKeys > 0)
		close(dnew.d_fd);
	dnew.d_opened = 0;
	ruboff("modify");
	modupdate();
	if (M_TYPEOF(mp->newrelspec) == M_ORDER) {
		close(dnew.d_btreefd);
		make_bsec(dnew.d_r.r_id, dim);
	}
	rubon();

	if (Lockrel)
		unlrl(&temptid);

	return (0);
}

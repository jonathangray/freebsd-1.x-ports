#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "endian.h"

#include <pv.h>
#include <func.h>
#include <symbol.h>
#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)index.c	8.6	5/10/86)

extern short	tTdbu[];

func_t	IndexFn = {
	"INDEX",
	indexx,
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
**	This is the DBU routine INDEX
**
**  pc = #of parameters
**  pv[0] points to primary relation name
**  pv[1] points to index relation name
**  pv[2] points to domain1
**  pv[3] points to domain2
**  .
**  .
**  .
**  pv[pc] = NULL
**
*/

struct dom {
	int	id;
	int	off;
	int	frml;
	char	frm[5];
};

int
indexx(int pc, paramv_t *pv)
{
	register int		i;
	int			j;
	register struct dom	*dom;
	register paramv_t	*p;
	char			*primary, *indx;
	int			ndoms, newpc;
	struct tup_id		tid, hitid;
	struct tup_id		xtid;
	paramv_t		newpv[MAX_2ND_KEYS * 2 + 4];
	char			primtup[MAX_TUP_SIZE], systup[MAX_TUP_SIZE];
	desc_t			desc, pridesc;
	extern desc_t		Reldes;
	extern desc_t		Attdes;
	extern desc_t		Inddes;
	relation_t		relkey, reltup;
	attr_t			attkey, atttup;
	index_t			indtup;
	struct dom		domain[MAX_2ND_KEYS];

	primary = pv[0].pv_val.pv_str;
	indx = pv[1].pv_val.pv_str;
#ifdef xZTR1
	if (tTf(33, -1))
		printf("index: (pri %s ind %s)\n", primary, indx);
#endif
	i = openr(&pridesc, OR_READ, primary);
	if (i == AMOPNVIEW_ERR)
		return (error(NOINDVIEW, primary, 0));
	if (i > 0)
		return (error(NOPRIMREL, primary, 0));
	if (i < 0)
		syserr("INDEX : openr (%.14s) %d", primary, i);

	if (!bequal(pridesc.d_r.r_owner, Usercode, USERCODE_SIZE)) {
		i = NOTOWNED;
	} else if (pridesc.d_r.r_status & S_CATALOG) {
		i = NOINDXSYSREL;
	} else if (pridesc.d_r.r_indexed == SECINDEX) {
		i = ALREADYINDX;
	}

	if (i) {
		closer(&pridesc);
		return (error(i, primary, 0));
	}
	/*
	**  GATHER INFO. ON DOMAINS
	*/
	opencatalog("attribute", OR_WRITE);
	ingres_setkey(&Attdes, &attkey, primary, ATTRELID);
	ingres_setkey(&Attdes, &attkey, pridesc.d_r.r_owner, ATTOWNER);
	pc -= 2;
	p = &pv[2];
	dom = domain;
	for (i = 0; i < pc; i++) {
		if (i >= MAX_2ND_KEYS) {
			closer(&pridesc);
			return (error(TOOMUCHDOMS, (p->pv_val).pv_str, primary, 0));	/* too many keys */
		}
		ingres_setkey(&Attdes, &attkey, (p->pv_val).pv_str, ATTNAME);
		j = getequal(&Attdes, &attkey, &atttup, &tid);
		if (j < 0)
			syserr("INDEX: geteq att %d", j);
		if (j) {
			closer(&pridesc);
			return (error(NODOM, (p->pv_val).pv_str, 0));	/* key not in relation */
		}
		if (pridesc.d_r.r_dim > 0 && atttup.a_id == pridesc.d_r.r_attrc) {
			/* attempting to use lid field as part of index */
			closer(&pridesc);
			return(error(NOINDXLID, primary, (p->pv_val).pv_str, 0));
		}
		dom->id = atttup.a_id;
		dom->off = atttup.a_off;
		dom->frml = atttup.a_len & I1MASK;
		dom->frm[0] = atttup.a_fmt;
		p++;
		dom++;
	}
	ndoms = i;
	noclose(&Attdes);

	/*
	** The "order" of the steps have been altered to improve
	** recovery possibilities
	*/
	/*
	**  STEP 1 & 2: CREATE INDEX RELATION.
	*/
	newpv[0].pv_val.pv_str = "0202";
	newpv[1].pv_val.pv_str = indx;
	newpc = 2;
	p = &pv[2];
	dom = domain;
	for (i = 0; i < pc; i++) {
		newpv[newpc++].pv_val.pv_str = (p->pv_val).pv_str;
		itoa(dom->frml, &dom->frm[1]);
		newpv[newpc++].pv_val.pv_str = dom->frm;
		dom++;
		p++;
	}
	newpv[newpc++].pv_val.pv_str = "tidp";
	newpv[newpc++].pv_val.pv_str = "i4";
	newpv[newpc].pv_type = PV_EOF;

	if (create(newpc, newpv)) {
		closer(&pridesc);
		return (-1);
	}

	/* This is done for concurrency reasons */
	if (noclose(&Reldes))
		syserr("index: noclose");

	/*
	**  STEP 5: FILL UP THE SECONDARY INDEX FILE ITSELF
	*/
	if (Lockrel) {
		/* set a shared relation lock */
		setrll(A_SLP, &pridesc.d_tid, M_SHARE);
	}
	if ((i = openr(&desc, OR_WRITE, indx)) != 0)
		syserr("INDEX: openr %.14s %d", indx, i);
	find(&pridesc, NOKEY, &tid, &hitid, (void *) NULL);
	while ((i = get(&pridesc, &tid, &hitid, primtup, TRUE)) == 0) {
		dom = domain;
		for (i = j = 0; j < ndoms; j++) {
#ifdef BIG_ENDIAN
			if (dom->frm[0] != 'c')
				i = ((i-1)|(dom->frml-1))+1;
#endif
			bmove(&primtup[dom->off], &systup[i], dom->frml);
			i += dom->frml;
			dom++;
		}
#ifdef BIG_ENDIAN
		i = ((i-1)|3)+1;
#endif
		/* move in pointer */
		bmove(&tid, &systup[i], sizeof(tid));
		if ((j = insert(&desc, &xtid, systup, TRUE)) < 0) {
			syserr("INDEX: insert %.14s %d", indx, j);
		}
	}
	if (i < 0) {
		syserr("INDEX: get %.14s %d", primary, i);
	}
	closer(&pridesc);
	closer(&desc);


	/*
	**  STEP 3: ENTRIES TO INDEX-REL
	*/
	/* mv in primary name  */
	pmove(primary, indtup.i_relname, MAX_NAME_SIZE, ' ');
	/* primary owner */
	bmove(pridesc.d_r.r_owner, indtup.i_owner, sizeof(pridesc.d_r.r_owner));
	/* index name */
	pmove(indx, indtup.i_index, MAX_NAME_SIZE, ' ');
	indtup.i_indrelspec = M_HEAP;
	for (i = 0; i < MAX_2ND_KEYS; i++) {
		indtup.i_dom[i] = (i < ndoms) ? domain[i].id : 0;
	}
	opencatalog("indices", OR_WRITE);
	if ((i = insert(&Inddes, &tid, (char *) &indtup, TRUE)) < 0)
		syserr("INDEX: insert ix %d", i);

	/*
	**  STEP 4: TURN BIT ON IN PRIMARY RELATION TO SHOW IT IS BEING INDEXED
	*/
	opencatalog("relation", OR_WRITE);
	ingres_setkey(&Reldes, &relkey, primary, RELID);
	ingres_setkey(&Reldes, &relkey, pridesc.d_r.r_owner, RELOWNER);
	if ((i = getequal(&Reldes, &relkey, &reltup, &tid)) != 0)
		syserr("INDEX: geteq rel %d", i);
	reltup.r_indexed = SECBASE;
	if ((i = replace(&Reldes, &tid, &reltup, TRUE)) < 0)
		syserr("INDEX: replace rel %d", i);

	if (Lockrel)
		unlrl(&pridesc.d_tid);	/* release relation lock */

	return (0);
}

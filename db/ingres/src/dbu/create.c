#include <stdio.h>
#include <errno.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <access.h>
#include <aux.h>
#include <catalog.h>
#include <symbol.h>
#include <lock.h>
#include <func.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)create.c	8.7	5/1/86)

extern	short	tTdbu[];

int	create(int, paramv_t *);

func_t CreateFn = {
	"CREATE",
	create,
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
**  CREATE -- create new relation
**
**	This module creates a brand new relation in the current
**	directory (database).  The relation is always created as
**	a paged heap.  It may not redefine an existing relation,
**	or rename a system catalog.
**
**	Trace Flags:
**		31
*/


struct domain {
	char	*name;
	char	frmt;
	char	frml;
};

extern desc_t   Reldes, Attdes;

/*
**  CHECK ATTRIBUTE FORMAT AND CONVERT
**
**	The string 'a' is checked for a valid attribute format
**	and is converted to internal form.
**
**	zero is returned if the format is good; one is returned
**	if it is bad.  If it is bad, the conversion into a is not
**	made.
**
**	A format of CHAR_CONST can be length zero only if this
**	create was generated internally.
*/
int
formck(char *a, struct domain *dom, bool internal)
{
	int			len;
	register int		i;
	char			c;
	register char		*p;
	register struct domain	*d;

	p = a;
	c = *p++;
	d = dom;

	len = atoi(p);
	i = len;

	 switch (c) {

	  case INT_CONST:
		if (i == 1 || i == 2 || i == 4) {
			d->frmt = INT_CONST;
			d->frml = i;
			return (0);
		}
		return (1);

	  case FLOAT_CONST:
		if (i == 4 || i == 8) {
			d->frmt = FLOAT_CONST;
			d->frml = i;
			return (0);
		}
		return (1);

	  /* note: should disallow c0 from user (but needed internally) */
	  case CHAR_CONST:
		if (i > MAX_FIELD_SIZE || i < 0 || (i == 0 && !internal))
			return (1);
		d->frmt = CHAR_CONST;
		d->frml = i;
		return (0);
	}
	return (1);
}

/*
**  DUP_ATT -- check for duplicate attribute
**
**	The attribute named 'name' is inserted into the 'attalias'
**	vector at position 'count'.  'Count' should be the count
**	of existing entries in 'attalias'.  'Attalias' is checked
**	to see that 'name' is not already present.
**
**	Parameters:
**		name -- the name of the attribute.
**		count -- the count of attributes so far.
**		domain -- 'struct domain' -- the list of domains
**			so far, names and types.
**
**	Returns:
**		-1 -- attribute name is a duplicate.
**		else -- index in 'domain' for this attribute (also
**			the a_id).
**
**	Side Effects:
**		The 'domain' vector is extended.
**
**	Trace Flags:
**		none
*/
int
dup_att(char *name, int count, struct domain *domain)
{
	register struct domain	*d;
	register int		lim;
	register int		i;

	lim = count;
	d = domain;

	for (i = 0; i < lim; i++)
		if (strcmp(name, d++->name) == 0)
			return (-1);
	if (count < MAX_DOMAINS)
		d->name = name;
	return (i);
}


/*
**  CHK_ATT -- check attribute for validity
**
**	The attribute is checked to see if
**	* it's name is ok (within MAX_NAME_SIZE bytes)
**	* it is not a duplicate name
**	* the format specified is legal
**	* there are not a ridiculous number of attributes
**	  (ridiculous being defined as anything over MAX_DOMAINS - 1)
**	* the tuple is not too wide to fit on one page
**
**	Parameters:
**		rel -- relation relation tuple for this relation.
**		attname -- tentative name of attribute.
**		format -- tentative format for attribute.
**		domain -- a 'struct domain' used to determine dupli-
**			cation, and to store the resulting name and
**			format in.
**
**	Returns:
**		zero -- OK
**		5104 -- bad attribute name.
**		5105 -- duplicate attribute name.
**		5106 -- bad attribute format.
**		5107 -- too many attributes.
**		5108 -- tuple too wide.
**
**	Side Effects:
**		'rel' has the r_attrc and r_width fields updated to
**		reflect the new attribute.
**
**	Trace Flags:
**		31
*/
int
chk_att(relation_t *rel, char *attname, char *format, struct domain *domain, bool internal)
{
	register int			i;
	register relation_t	*r;

	r = rel;

#ifdef xZTR3
	if (tTf(31, 1))
		printf("chk_att %s %s\n", attname, format);
#endif

	if (strcmp(attname, "tid") == 0)
		return (BADATTRNAME);		/* bad attribute name */
	if ((i = dup_att(attname, r->r_attrc, domain)) < 0)
		return (DUPATTRNAME);		/* duplicate attribute */
	if (formck(format, &domain[i], internal))
		return (BADATTRFORMAT);		/* bad attribute format */
	r->r_attrc++;
#ifdef ADDR_ROUNDUP
/* round the location up so that we don't get off address traps */
	if (domain[i].frmt != CHAR_CONST)
		r->r_width=((r->r_width-1)|((domain[i].frml&I1MASK)-1))+1;
#endif
	r->r_width += domain[i].frml & I1MASK;
	if (r->r_attrc >= MAX_DOMAINS)
		return (TOOMANYDOMS);		/* too many attributes */
	if (r->r_width > MAX_TUP_SIZE && (r->r_status & S_VIEW) == 0)
		return (RELTOOWIDE);		/* tuple too wide */
	return (0);
}

/*
**  INITSTRUCTS -- initialize relation and attribute tuples
**
**	Structures containing images of 'relation' relation and
**	'attribute' relation tuples are initialized with all the
**	information initially needed to do the create.  Frankly,
**	the only interesting part is the the expiration date
**	computation; longconst(9, 14976) is exactly the number
**	of seconds in one week.
**
**	Parameters:
**		att -- attribute relation tuple.
**		rel -- relation relation tuple.
**
**	Returns:
**		none
**
**	Side Effects:
**		'att' and 'rel' are initialized.
**
**	Requires:
**		time -- to get the current date.
**
**	Called By:
**		create
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		none
**
**	Syserrs:
**		none
**
**	History:
**		2/27/78 (eric) -- documented.
*/
void
initstructs(register attr_t *att, register relation_t *rel)
{
	/* setup expiration date (today + one week) */
	time(&rel->r_modtime);
	rel->r_savetime = rel->r_modtime + 604800L;
	rel->r_free = 0;
	rel->r_tupc = 0;
	rel->r_attrc = 0;
	rel->r_width = 0;
	rel->r_primc = 1;
	rel->r_spec = M_HEAP;
	rel->r_indexed = 0;
	rel->r_dim = 0;
	att->a_iskey = 0;
	att->a_id = 0;
	att->a_off = 0;
}

/*
**  INS_ATT -- insert attribute into attribute relation
**
**	Parameters:
**		des -- relation descriptor for the attribute catalog.
**		att -- attribute tuple, preinitialized with all sorts
**			of good stuff (everything except 'attname',
**			'a_fmt', and 'a_len'; 'a_id' and 'a_off'
**			must be initialized to zero before this routine
**			is called the first time.
**		dom -- 'struct domain' -- the information needed about
**			each domain.
**
**	Returns:
**		none
**
**	Side Effects:
**		The 'att' tuple is updated in the obvious ways.
**		A tuple is added to the 'attribute' catalog.
**
**	Trace Flags:
**		none currently
*/
void
ins_att(desc_t *des, attr_t *att, struct domain *dom)
{
	struct tup_id		tid;
	register struct domain	*d;

	d = dom;

	pmove(d->name, att->a_name, MAX_NAME_SIZE, ' ');
#ifdef ADDR_ROUNDUP
/* round the location up so that we don't get off address traps */
	if (d->frmt != CHAR_CONST)
		att->a_off=((att->a_off-1)|(d->frml-1))+1;
#endif
	att->a_fmt = d->frmt;
	att->a_len = d->frml;
	att->a_id++;
	if (insert(des, &tid, att, FALSE))
		syserr("ins_att: insert(att, %s)", d->name);
	att->a_off += att->a_len & I1MASK;
}


/*
**  CREATE -- create new relation
**
**	This routine is the driver for the create module.
**
**	Parameters:
**		pc -- parameter count
**		pv -- parameter vector:
**			0 -- relation status (r_status) -- stored into
**				the 'r_status' field in the relation
**				relation, and used to determine the
**				caller.  Interesting bits are:
**
**				S_INDEX -- means called by the index
**					processor.  If set, the 'r_indexed'
**					field will also be set to -1
**					(SECINDEX) to indicate that this
**					relation is a secondary index.
**				S_CATALOG -- this is a system catalog.
**					If set, this create was called
**					from creatdb, and the physical
**					file is not created.  Also, the
**					expiration date is set infinite.
**				S_VIEW -- this is a view.  Create has
**					been called by the 'define'
**					statement, rather than the
**					'create' statement.  The physical
**					file is not created.
**
**			1 -- relation name.
**			2 -- attname1
**			3 -- format1
**			4, etc -- attname, format pairs.
**
**	Returns:
**		zero -- successful create.
**		else -- failure somewhere.
**
**	Side Effects:
**		A relation is created (this is a side effect?).  This
**		means entries in the 'relation' and 'attribute' cata-
**		logs, and (probably) a physical file somewhere, with
**		one page already in it.
**
**	Trace Flags:
**		31
*/
int
create(int pc, paramv_t *pv)
{
	register paramv_t	*pp;
	register int		i;
	int			bad;
	struct domain		domain[MAX_DOMAINS];
	struct domain		*dom;
	char			*relname;
	char			tempname[MAX_NAME_SIZE+3];
	struct tup_id		tid;
	relation_t		rel;
	attr_t			att;
	desc_t			desr;
	register int		relstat;
	tid_t			temptid;
	long			npages;
	int			fdes;
	bool			internal;
	long			unusedtid;

#ifdef xZTR1
	if (tTf(31, -1)) {
		printf("creating %s\n", pv[1].pv_val.pv_str);
	}
#endif
	pp = pv;
	relstat = oatoi(pp[0].pv_val.pv_str);
	/*
	**	If this database has query modification, then default
	**	to denial on all user relations.
	**	(Since views cannot be protected, this doesn't apply to them)
	*/
	if ((Admin.ad_h.adm_flags & A_QRYMOD) &&
	    ((relstat & (S_VIEW || S_CATALOG)) == 0)) {
		relstat |= (S_PROTALL | S_PROTRET);
	}
	relname = (++pp)->pv_val.pv_str;
	internal = bequal(relname, "_SYS", 4);
	ingresname(relname, Usercode, rel.r_id);
	bmove(rel.r_id, att.a_rel, MAX_NAME_SIZE + 2);
	opencatalog("relation", OR_WRITE);

	/* check for duplicate relation name */
	if ((relstat & S_CATALOG) == 0) {
		if (openr(&desr, OR_RELTID, relname) == 0) {
			if (bequal(desr.d_r.r_owner, rel.r_owner, 2)) {
				/* bad relname */
				return (error(DUPRELNAME, relname, 0));
			}
			if (desr.d_r.r_status & S_CATALOG) {
				/* attempt to rename system catalog */
				return (error(SYSRELNAME, relname, 0));
			}
		}
	}
	opencatalog("attribute", OR_WRITE);

	/* initialize structures for system catalogs */
	initstructs(&att, &rel);
	rel.r_status = relstat;
	if ((relstat & S_CATALOG) != 0)
		rel.r_savetime = 0;
	else if ((relstat & S_INDEX) != 0)
		rel.r_indexed = SECINDEX;
	
#ifdef xZTR3
	if (tTf(31, 2)) {
		printf("\nrel->r_primc = %ld\n", rel.r_primc);
		printup(&Reldes, (char *) &rel);
	}
#endif

	/* check attributes */
	pp++;
	for (i = pc - 2; i > 0; i -= 2) {
		bad = chk_att(&rel, pp[0].pv_val.pv_str, pp[1].pv_val.pv_str, domain, internal);
		if (bad != 0) {
			return (error(bad, relname, pp[0].pv_val.pv_str, pp[1].pv_val.pv_str, 0));
		}
		pp += 2;
	}

	/*
	** Create files if appropriate. Concurrency control for
	** the create depends on the actual file. To prevent
	** to users with the same usercode from creating the
	** same relation at the same time, their is check
	** on the existence of the file. The important events are
	** (1) if a tuple exists in the relation relation then
	** the relation really exists. (2) if the file exists then
	** the relation is being created but will not exist for
	** use until the relation relation tuple is present.
	** For VIEWS, the file is used for concurrency control
	** during the create but is removed afterwards.
	*/
	if ((relstat & S_CATALOG) == 0) {
		/* for non system named temporary relations
		** set a critical section lock while checking the
		** existence of a file.  If it exists, error return(DUPRELNAME)
		** else create file.
		*/
		(void) memset(&temptid, 0, sizeof(temptid));
		if (Lockrel && (!bequal(rel.r_id,"_SYS",4))) {
			unusedtid = -1;
			(void) memcpy(&temptid, &unusedtid, sizeof(temptid));
			setcsl(&temptid);	/* set critical section lock */
			if ((fdes = open(rel.r_id,O_RDONLY)) >= 0) {
				/* file already exists */
				(void) close(fdes);
				/* release critical section lock */
				unlcs(&temptid);
				return(error(DUPRELNAME, relname, 0));
			}
			errno = 0;	/* file doesn't exist */
		}
		ingresname(rel.r_id, rel.r_owner, tempname);
		desr.d_fd = open(tempname, O_CREAT | O_TRUNC | O_WRONLY,
						FILEMODE);
		unusedtid = 0;
		if (memcmp(&temptid, &unusedtid, sizeof(temptid)) != 0)
			/* release critical section lock */
			unlcs(&temptid);
		if (desr.d_fd < 0)
			syserr("create: creat %s", rel.r_id);
		/* init d_tid to unused */
		unusedtid = -1;
		memcpy(&desr.d_tid, &unusedtid, sizeof(unusedtid));
		if ((relstat & S_VIEW) == 0) {
			npages = 1;
			if ((i = formatpg(&desr, npages)) != 0)
				syserr("create: formatpg %d", i);
		}

		close(desr.d_fd);
	}

	/* insert attributes into attribute relation */
	pp = pv + 2;
	dom = domain;
	for (i = pc - 2; i > 0; i -= 2) {
		ins_att(&Attdes, &att, dom++);
		pp += 2;
	}

	/*
	** Flush the attributes. This is necessary for recovery reasons.
	** If for some reason the relation relation is flushed and the
	** machine crashes before the attributes are flushed, then recovery
	** will not detect the error.
	** The call below cannot be a "noclose" without major changes to
	** creatdb.
	*/
	if ((i = pageflush(0)) != 0) {
		syserr("create:flush att %d", i);
	}

	if ((i = insert(&Reldes, &tid, &rel, FALSE)) != 0) {
		syserr("create: insert(rel, %.14s) %d", rel.r_id, i);
	}

	if (relstat & S_VIEW) {
		unlink(tempname);
	}

	return (0);
}


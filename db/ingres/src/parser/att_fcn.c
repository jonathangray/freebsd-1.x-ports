#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "parser.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

att_ent_t *attalloc(void);

SCCSID(@(#)att_fcn.c	8.2	2/8/85)

/*
** fake attribute stash entries for tid nodes (and sid in dist version)
**	these are provided by the system to a program only for debugging
**	the system source and do not have well defined (over time) meanings.
*/
att_ent_t	Faketid = {
	0,	INT_CONST,	4,	"tid",	0
};

#ifdef	DISTRIB
att_ent_t	Fakesid = {
	0,	INT_CONST,	4,	"sid",	0
};
#endif

att_ent_t		Attable[MAXATT];/* attrib stash space, turned into a list later */
att_ent_t		*Freeatt;	/* free list of attrib stash */

/*
**  ATT_FCN.C  --  attribute list manipulation routines
**
**	ATT_FCN ~~ trace flags = 40, 41
**
**	Defines:
**		Attable
**		Freeatt
**		attinit()
**		attlookup()
**		attadd()
**		attfnd()
**		attcheck()
**		attfree()
**		attalloc()
**		attcount()
**
**	History:
**		modified for 6.3 (jiw)
*/

/*
**   ATTINIT -- initializes attribute table
**
**	Parameters:
**		none
**
**	Trace Flags:
**		attinit ~~ 40.0
*/
void
attinit(void)
{
	register att_ent_t		*atptr;
	register int			i;

#ifdef	xPTR3
	tTfp(40, 0, "attinit");
#endif

	atptr = Attable;

	for (i = 0; i < MAXATT - 1; i++) {
		atptr->atbnext = atptr + 1;
		atptr++;
	}

	atptr->atbnext = NULL;

	Freeatt = Attable;		/* the first attribute in chain */
}

/*
**  ATTADD -- add an attribute to the list for a particular range variable
**
**	Parameters:
**		slot -- the number of the range variable to use
**		tuple -- the attribute tuple to add
**
**	Returns:
**		pointer to the attribute added
**
**	Trace Flags:
**		attadd ~~ 40.8
*/

att_ent_t *
attadd(int slot, attr_t *tuple)
{
	register att_ent_t	*current;
	register att_ent_t	*aptr;
	register att_ent_t	*bptr;
	PARRNG			*rptr;
	int			i;

	extern PARRNG		Parrng[];

#ifdef	xPTR3
	tTfp(40, 8, "attadd slot %d, %12s\n", slot, tuple->a_name);
#endif

	rptr = &Parrng[slot];

	current = attalloc();
	current->atbid = tuple->a_id;
	current->atbfrmt = tuple->a_fmt;
	current->atbfrml = tuple->a_len;
	bmove(tuple->a_name, current->atbname, MAX_NAME_SIZE);
	for (i = 0; i < MAX_NAME_SIZE; i++)
		if (current->atbname[i] == ' ')
			current->atbname[i] = '\0';

	aptr = rptr->attlist;
	bptr = 0;
	while (aptr != 0) {
		if (aptr->atbid > current->atbid)
			break;
		bptr = aptr;
		aptr = aptr->atbnext;
	}
	if (bptr == 0)
		rptr->attlist = current;
	else
		bptr->atbnext = current;

	current->atbnext = aptr;

	return (current);
}

/*
**  ATTFIND -- finds attribute in stash
**
**	Attfind looks in attribute stash to see if attrib info already there
**	return pointer to attribute in attribute table else NULL.
**
**	Parameters:
**		slot -- the number of the entry in range table
**		attrib -- the attribute name to find
**
**	Returns:
**		pointer to entry or NULL if not in stash
**
**	Trace Flags:
**		attfind ~~ 40.12
*/

att_ent_t *
attfind(int slot, register char *attrib)
{
	register att_ent_t	*aptr;

	extern PARRNG		Parrng[];

#ifdef xPTR1
	tTfp(40, 12, "attadd\n");
#endif

	aptr = Parrng[slot].attlist;

	while (aptr != NULL) {
		if (!scompare(attrib, MAX_NAME_SIZE, aptr->atbname, MAX_NAME_SIZE))
			return (aptr);

		aptr = aptr->atbnext;
	}
	return (NULL);
}

/*
**  ATTLOOKUP -- finds attribute entry
**
**	Attlookup looks up atribute 'attrib' in the att stash
**	for range table entry 'slot'.  If the attrib is not
**	in the stash it is entered.
**
**	Parameters:
**		attrib -- the name of the attribute to find
**		slot -- the number of the range variable
**
**	Returns:
**		pointer to the attstash element
**
**	Trace Flags:
**		attlookup ~~ 40.4, 40.5
*/

att_ent_t *
attlookup(int slot, char *attrib)
{
	register PARRNG			*rptr;
	register att_ent_t		*current;
	int				ik;
	attr_t		tuple;
	register attr_t	*ktuple;
	attr_t		ktup;
	tid_t				tid;

	extern desc_t			Attdes;
	extern PARRNG			Parrng[];

	rptr = &Parrng[slot];

	ktuple = &ktup;
#ifdef	xPTR2
	tTfp(40, 4, "attlookup: att = %s and rel= %s\n", attrib, trim_relname(rptr->vardesc.d_r.r_id));
#endif

	/* attribute called "tid" is phantom attribute, use fake */
	if (strcmp("tid", attrib) == 0) {
		return (&Faketid);
	}
#ifdef	DISTRIB
	if (strcmp("sid", attrib) == 0) {
		return (&Fakesid);
	}
#endif

	/* check to see if attrib is in stash */
	if ((current = attfind(slot, attrib)) != NULL)
		return (current);

#ifdef	xPTR2
	tTfp(40, 5, "getting att info from relation\n");
#endif

	/* rel name, owner, a_name is unique ident */
	clearkeys(&Attdes);
	ingres_setkey(&Attdes, ktuple, rptr->vardesc.d_r.r_id, ATTRELID);
	ingres_setkey(&Attdes, ktuple, rptr->vardesc.d_r.r_owner, ATTOWNER);
	ingres_setkey(&Attdes, ktuple, attrib, ATTNAME);
	if (!(ik = getequal(&Attdes, ktuple, &tuple, &tid))) {
		/* put attrib stuff into att stash */
		current = attadd(slot, &tuple);
		return (current);
	}

	if (ik == 1)
		/* attribute not in relation */
		par_error(NOATTRIN, WARN, attrib, trim_relname(rptr->vardesc.d_r.r_id), 0);
	else
		syserr("fatal error in getequal, ret: %d", ik);
	return((att_ent_t *) NULL);
}

/*
**  ATTALLOC -- returns a pointer to a atstash type structure
**
**	Attalloc checks the freelist (Freeatt) for attstash entries,
**	if some is there, one is removed.  If the freelist is empty,
**	attstashes are removed from range table entries until an free
**	element is found.
**
**	Parameters:
**		none
**
**	Returns:
**		a pointer to an atstash element
**
**	Requires:
**		Rngback
**		Freeatt
**		
**	Trace Flags:
**		attalloc ~~ 41.8, 41.9
*/

att_ent_t *
attalloc(void)
{
	register att_ent_t	*aptr;
	register PARRNG		*rptr;

	extern PARRNG		*Rngback;

#ifdef	xPTR3
	tTfp(41, 8, "attalloc Freeatt %d\n", Freeatt);
#endif

	/* Note: the following loop doesn't happen if Freeatt != NULL */

	for (rptr = Rngback; Freeatt == NULL; rptr = rptr->frontpt) {
		/*
		** search least recently used vbles for attrib stash space
		** until at least one entry is found
		*/

#ifdef	xPTR3
	tTfp(41, 9, "attalloc: freeing %12s\n", rptr->vardesc.d_rangevar);
#endif

		if (rptr == NULL)
			syserr("attalloc: no att space.");

		Freeatt = rptr->attlist;
		rptr->attlist = NULL;
	}
	aptr = Freeatt;
	Freeatt = Freeatt->atbnext;
	aptr->atbnext = NULL;

	return (aptr);
}

/*
**  ATTCHECK -- checks for conflicts in attributes
**
**	Attcheck checks for type conflicts in the current query domain and
**	attable entry
**
**	Parameters:
**		aptr -- pointer to current atttibute
**
**	Returns:
**		nothing
**
**	Requires:
**		Trfrmt -- for current format
**
**	Trace Flags:
**		attcheck ~~ 41.0
*/
void
attcheck(register att_ent_t *aptr)
{
	extern char		Trfrmt;

#ifdef	xPTR1
	tTfp(41, 0, "attcheck\n");
#endif

	if ((Trfrmt == CHAR_CONST) != (aptr->atbfrmt == CHAR_CONST))
		/* function type does not match attrib */
		par_error(RESTYPE, WARN, aptr->atbname, 0, 0);
}

/*
**  ATTFREE -- puts a list of attrib space back on the free list
**
**	Parameters:
**		aptr -- pointer to list of attstash entries
**
**	Returns:
**		nothing
**
**	Requires:
**		Freeatt
**
**	Trace Flags:
**		attfree ~~ 41.4
*/
void
attfree(att_ent_t *aptr)
{
	register att_ent_t	*att;

#ifdef	xPTR1
	tTfp(41, 4, "attfree\n");
#endif

	if ((att = aptr) == NULL)
		return;

	while (att->atbnext != NULL)
		att = att->atbnext;

	att->atbnext = Freeatt;
	Freeatt = aptr;
}

/*
**  ATTCOUNT -- counts atstash elems
**
**	Attcount returns a count of the number of attributes already in the
**	attrib stash.
**
**	Parameter:
**		slot -- the range table entry to count
**
**	Returns:
**		count of the attributes
**
**	Trace Flags:
**		attcount ~~ 41.12
*/

int
attcount(int slot)
{
	register int		cntr;
	register att_ent_t	*aptr;

	extern PARRNG		Parrng[];

#ifdef	xPTR1
	tTfp(41, 12, "attcount\n");
#endif

	cntr = 0;
	aptr = Parrng[slot].attlist;
	while (aptr != NULL) {
		cntr++;
		aptr = aptr->atbnext;
	}
	return (cntr);
}

#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <range.h>
#include "parser.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)range_fcn.c	8.3	2/8/85)

/*
**	Range table variables
*/

PARRNG			Parrng[MAX_RANGES]; /* table for keeping track of atts */
					  /* and allocation of range vars */
int			Resrng;		/* result reln slot */

PARRNG			*Rngfront;	/* the front of Rnga */
PARRNG			*Rngback;	/* the back of Qt.qt_rangev */


/*
**  RANGE_FCN.C -- functions for manipulating the range table
**
**	Trace Flags:
**		RANGE_FCN.C ~~ 66, 67
*/
void
ctlmod_decl(int slot)
{
	extern PARRNG	Parrng[];

	Qt.qt_rangev[slot].rngvdesc = NULL;
	if (declare(slot, &Parrng[slot].vardesc) != slot)
		syserr("declare misdeclared");
}
 
/*
**  RNGINIT
**	initializes the pointers in the range table
**	it should be called prior to starting the parsing
**	it also initializes the attrib stash stuff because
**	the attrib stash is really part of the range table
**
**	Trace Flags:
**		rnginit ~~ 66.0
*/
void
rnginit(void)
{
	register int		slot;
	register PARRNG		*parrngptr;
	register RANGEV		*rngptr;

#ifdef	xPTR2
	tTfp(66, 0, "rnginit\n");
#endif

	Rngfront = &Parrng[MAX_VARS - 1];			/* ptr to head of range table */
	parrngptr = Parrng;
	/* initialize first element */
	parrngptr->attlist = NULL;
	parrngptr->backpt = NULL;
	parrngptr->frontpt = &Parrng[1];

	rngptr = Qt.qt_rangev;

	for (slot = 0, parrngptr = &Parrng[1]; slot < MAX_VARS; slot++, parrngptr++) {
		parrngptr->attlist = NULL;

		parrngptr->frontpt = parrngptr + 1;
		parrngptr->backpt = parrngptr - 1;
	}

	Rngback = Parrng;

	parrngptr = &Parrng[MAX_VARS - 1];

	parrngptr->frontpt = NULL;

	/* MAX_VARS SLOT = Resultvar */
	(++parrngptr)->attlist = NULL;
	parrngptr->frontpt = parrngptr->backpt = NULL;

	Rngfront->frontpt = NULL;

	clrrange();

	attinit();
}

/*
** RNGLOOK
**	returns a pointer to the range table entry else -1
**	type = LOOKREL	lookup relation
**	type = LOOKVAR	lookup variable
**
**	Trace Flags:
**		rnglook ~~ 66.4, 66.5, 66.6
*/
int
rnglook(char *name, int type)
{
	register PARRNG		*rptr;

	register int		slot;

	desc_t			reldesc;

	int			i;

#ifdef	xPTR2
	tTfp(66, 4, "rnglook:\ttype = %s\tname = %s\n",
		    (type == LOOKVAR ? "variable" : "relation"), name);

	if (tTf(66, 5))
		printtable();
#endif

	rptr = Parrng;

	for (slot = 0; slot < MAX_VARS; slot++, rptr++)	/* search external vbles only */ {
		if (rptr->relvused
		    && scompare(name, MAX_NAME_SIZE,
		    (type == LOOKVAR ? rptr->vardesc.d_rangevar : rptr->vardesc.d_r.r_id),
		    MAX_NAME_SIZE) == 0) {
			Qt.qt_rangev[slot].rngvmark = 1;
			
#ifdef	xPTR2
			tTfp(66, 6, "fnd '%s' at '%d'\n", name, slot);
#endif

			rngfront(slot);
			return (slot);
		}
	}

	/*
	** We haven't been able to find the variable, assume 
	** it is the name of a relation, and try and insert
	** a new range variable under that name.
	*/
	if (type == LOOKVAR) {
		if ((i = openr(&reldesc, OR_RELTID, name)) < 0)
			syserr("relname: error in openr '%d'", i);

		/*
		** rngent will call rnglook to create var if name
		** was a relation name
		*/
		if (i == 0)
			return(rngent(R_IMPLICIT, name, &reldesc));
		else
			return(-1);
	}
	return (-1);
}

/*
**  RNGENT
**
**	Insert variable and relation in range table.
**
**	Trace Flags:
**		 rngent ~~ 66.8
*/
int
rngent(int type, char *var, register desc_t *desc)
{
	register PARRNG	*rptr;
	register int	slot;

#ifdef	xPTR2
	tTfp(66, 8, "rngent:\ttype=%s\tvar=%s\n",
		(type == R_INTERNAL ? "internal" : "external"), var);
#endif

	if (type == R_INTERNAL)
		slot = MAX_VARS;		/* the internal variable */
	else {
		if ((type == R_IMPLICIT) || (slot = rnglook(var, LOOKVAR)) < 0) {
			/* not in range table */
			slot = rngold();
		}

		rngfront(slot);
	}

	rptr = &Parrng[slot];

	if (!bequal(desc, &rptr->vardesc, sizeof(*desc))) {
		attfree(rptr->attlist);
		rptr->attlist = NULL;
	}

	rptr->relvused = 1;

	bmove(desc, &rptr->vardesc, sizeof(*desc));
	pmove(var, rptr->vardesc.d_rangevar, MAX_NAME_SIZE, ' ');

	ctlmod_decl(slot);

	return (slot);
}

/*
** RNGDEL
**	removes an entry from the range table
**	removes all variables for the relation name
**
**	Trace Flags:
**		 rngdel ~~ 66.12
*/
void
rngdel(register char *rel)
{
	register int	slot;

#ifdef	xPTR2
	tTfp(66, 12, "rngdel: %12s\n", rel);
#endif

	while ((slot = rnglook(rel, LOOKREL)) >= 0) {
		Parrng[slot].relvused = 0;
		rngback(slot);
		attfree(Parrng[slot].attlist);
		Parrng[slot].attlist = NULL;
	}
}


/*
** RNGFRONT
**	move entry 'r' to head of range table list
**
**	Trace Flags:
**		 rngfront ~~ 67.0 
*/
void
rngfront(int slot)
{
	register PARRNG		*fptr;

#ifdef	xPTR2
	tTfp(67, 0, "rngfront:\tslot %d\n", slot);
#endif


	rngget(slot);

	fptr = &Parrng[slot];

	fptr->frontpt = NULL;
	fptr->backpt = Rngfront;
	Rngfront->frontpt = fptr;

	Rngfront = fptr;
}

/*
** RNGBACK
**	move entry 'r' to back of range table list
**
**	Trace Flags:
**		rngback ~~ 67.4
*/
void
rngback(int slot)
{
	register PARRNG	*bptr;

#ifdef	xPTR2
	tTfp(67, 4, "rngback:\tslot %d\n", slot);
#endif

	rngget(slot);

	bptr = &Parrng[slot];

	bptr->backpt = NULL;
	bptr->frontpt = Rngback;
	Rngback->backpt = bptr;

	Rngback = bptr;
}

/*
**  RNGGET -- get a descriptor from range table
**
**	Trace Flags:
**		rngget ~~ 67.8
*/
void
rngget(int slot)
{
	register PARRNG	*slotptr;
	register PARRNG	*forward;
	register PARRNG	*backward;

#ifdef	xPTR2
	tTfp(67, 8, "rngget:\tslot %d\n", slot);
#endif


	slotptr = &Parrng[slot];
	forward = slotptr->frontpt;
	backward = slotptr->backpt;

	if (slotptr == Rngfront) {
		Rngfront = backward;
		backward->frontpt = NULL;
	} else if (slotptr == Rngback) {
		Rngback = forward;
		forward->backpt = NULL;
	} else {
		forward->backpt = backward;
		backward->frontpt = forward;
	}

	slotptr->backpt = slotptr->frontpt = NULL;
}

/*
**  RNGOLD -- find least recently used vble entry
**
**	Trace Flags:
**		rngold ~~ 67.9
*/
int
rngold(void)
{
#ifdef	xPTR2
	tTfp(67, 9, "rngold %d.\n", Rngback - (PARRNG *) Parrng);
#endif

	return(Rngback - (PARRNG *) Parrng);
}

/*
** RNGRESET
**	reset the used marks to '0'
**
**	Trace Flags:
**		rngreset ~~ 67.10
*/
void
rngreset(void)
{
	register int		i;
	register RANGEV		*rangevptr;

#ifdef	xPTR2
	tTfp(67, 10, "rngreset().\n");
#endif

	rangevptr = Qt.qt_rangev;

	for (i = 0; i < MAX_VARS; i++, rangevptr++) /* only do external ones */
		rangevptr->rngvmark = 0;
}

/*
** CHECKUPD
**	checks to make sure that the user can update the relation 'name1'
**	the 'open' parameter is set if 'Reldesc' contains the openr info
**	for the relation in question.
**
**	Trace Flags:
**		checkupd ~~ 67.11
*/
void
checkupd(int entnum)
{
	extern int		Noupdt;
	extern PARRNG		Parrng[];
	register PARRNG		*rptr;

#ifdef	xPTR2
	tTfp(67, 11, "checkupd(%d).\n", entnum);
#endif

	rptr = &Parrng[entnum];

	if (!Noupdt)
		return;
	if (rptr->vardesc.d_r.r_status & S_NOUPDT)
		/* no updates allowed on this relation */
		par_error(CANTUPDATE, WARN, trim_relname(rptr->vardesc.d_r.r_id), 0, 0);
}

/*
** RNGFRESH -- check the range table r_status information for accuracy
**
**	If the command specified could have changed the r_status info
**	make the appropriate adjustments to the range table
*/
void
rngfresh(int op)
{
	register PARRNG		*rptr;
	register int		slot;
	desc_t			desc;
	char			var[MAX_NAME_SIZE];

#ifdef	xPTR2
	tTfp(67, 11, "rngfresh %d.\n", op);
#endif

	/* search the entire table! */
	for (slot = 0, rptr = Parrng; slot <= MAX_VARS; slot++, rptr++) {
		if (!(rptr->relvused))
			continue;

		switch (op) {
		  case mdDESTROY:
			if ((rptr->vardesc.d_r.r_status & (S_VBASE | S_INTEG | S_PROTUPS | S_INDEX)) != 0) {
			fixordel:
				/*
				** openr the relation, if it doesn't exist make
				** sure that all range table entries are gone
				*/
				if (!openr(&desc, OR_RELTID, rptr->vardesc.d_r.r_id))
					rptr->vardesc.d_r.r_status = desc.d_r.r_status;
				else
					/* relation not there, purge table */
					rngdel(rptr->vardesc.d_r.r_id);
			}
			break;

		  case mdVIEW:
			if ((rptr->vardesc.d_r.r_status & S_VBASE) == 0) {
			fixorerr:
				/*
				** if the relation doesn't exist then it is
				** a syserr, otherwise, copy the bits.
				*/
				if (!openr(&desc, OR_RELTID, rptr->vardesc.d_r.r_id))
					rptr->vardesc.d_r.r_status = desc.d_r.r_status;
				else {
					/* not there, syserr */
					syserr("RNGFRESH: extra entry: %s", rptr->vardesc.d_r.r_id);
				}
			}
			break;

		  case mdPROT:
			if ((rptr->vardesc.d_r.r_status & S_PROTUPS) == 0)
				goto	fixorerr;
			break;

		  case mdINTEG:
			if ((rptr->vardesc.d_r.r_status & S_INTEG) == 0)
				goto	fixorerr;
			break;

		  case mdMODIFY:
			if (!openr(&desc, OR_RELTID, rptr->vardesc.d_r.r_id))
				if (!bequal(&rptr->vardesc, &desc, sizeof(desc)))
				/* relation structure may have changed, adjust range variable */ {
					bmove(rptr->vardesc.d_rangevar, var, MAX_NAME_SIZE);
					rngent(R_EXTERNAL, var, &desc);
				}
			if ((rptr->vardesc.d_r.r_status & S_INDEX) != 0)
				goto	fixordel;
			break;
		
		  default:
			return;	/* command ok, dont waste time on rest of table */
		}
	}
}

void
printtable(void)
{
	register PARRNG		*rptr;
	int			slot[MAX_RANGES];
	int			i;

	printf("Range table:\n");

	for (i = 0; i < MAX_RANGES; i++)
		slot[i] = 0;

	for (rptr = Rngfront; rptr != NULL; rptr = rptr->backpt) {
		i = rptr - (PARRNG *) Parrng;
		slot[i] = 1;
		printslot(i);
	}
	printf("\nEntries not in list:\n");
	for (i = 0; i < MAX_RANGES; i++) {
		if (!slot[i]) {
			printslot(i);
		}
	}
}

void
printslot(int slot)
{
	register RANGEV		*rptr;
	register PARRNG		*auxptr;

	rptr = &Qt.qt_rangev[slot];
	auxptr = &Parrng[slot];

	printf("slot:\t%d\n{\trvar:\t%.12s,\trelnm:\t%.12s.\n",
	    slot, auxptr->vardesc.d_rangevar,
	    auxptr->vardesc.d_r.r_id);
	printf("\tRELVUSED: %d, RELVSEND %d.\n",
	    auxptr->relvused,
	    rptr->rngvmark);


	printf("\tratts: %d, attlist: %p.\n}\n", auxptr->vardesc.d_r.r_attrc, auxptr->attlist);
}

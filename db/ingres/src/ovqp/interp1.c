#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include "../decomp/globs.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_DECOMP
#include "protos.h"

SCCSID(@(#)interp1.c	8.5	5/30/88)


/*
**	INTERP1.C
**
**	symbol I/O utility routines for OVQP interpreter.
**
*/

/*
** GETSYMBOL
**
**	Gets (type, len, value) symbols from list
**	A ptr to the list is advanced after
**	call.  Symbols are moved to a target location
**	(typically a slot on the interpreter's De.ov_stack).
**	Legality of symbol type and length is checked.
**	Returns		1  if no more symbols in list
**			0 otherwise
**
*/
int
getsymbol(sym_t *ts, sym_t ***p)
/* ts - target location (on stack) */
/* p - pointer to list */
{
	int			len;	/*length of target symbol*/
	register union symvalue	*d;	/* ptr to data for target symbol */
	register sym_t		*cp;	/* the item in the list */
	register sym_t		*tops;	/* target location on stack */
	register union symvalue	*val;
	register STRKEEPER	*seen;	/* Have we seen an s? */
	register char 		*c;	/* For debugging */
	int			start;  /* save the start pos of a string */

	seen = 0;
	tops = ts;	/* copy stack pointer */
	cp = **p;	/* get list pointer */
	tops->start = -1;	/* initialize start to impossible value */

#ifdef xOTR1
	if (tTf(84, 0)) {
		printf("GETSYM: ");
		prsym(cp);
	}
#endif

	if (tops >= (sym_t *) &De.ov_stack[MAX_STACK_SIZE]) {
		ov_err(STACKOVER);
	}

	val = &cp->value;
	/* decomp will put the s_var's value in the right place 
	 * if this is the case
	 */
	if (cp->type == VAR || cp->type == S_VAR) {
		tops->type = val->sym_var.varfrmt;
		len = tops->len = val->sym_var.varfrml;
		d = (union symvalue *) val->sym_var.valptr;
		seen = (STRKEEPER *) val->sym_var.varstr;
		val->sym_var.varstr = 0;
	} else {
		tops->type = cp->type;
		len = tops->len = cp->len;
		len &= I1MASK;
		d = &cp->value;
	}
	/* advance De.ov_qvect sequencing pointer p */
	*p += 1;

	switch(tops->type) {
	case INT_CONST:
		switch (len) {
		case 1:
			tops->value.sym_data.i1type = d->sym_data.i1type;
			break;
		case 2:
		case 4:
			bmove((char *) d, (char *) &tops->value.sym_data, len);
			break;

		default:
			syserr("getsym:bad int len %d",len);
		}
		break;

	case FLOAT_CONST:
		switch (len) {
		case 4:
			tops->value.sym_data.f4type = d->sym_data.f4type;
			break;

		case 8:
			tops->value.sym_data.f8type = d->sym_data.f8type;
			break;

		default:
			syserr("getsym:bad FLOAT_CONST len %d",len);
		}
		break;

	  case CHAR_CONST:
	  {
		if (seen) {
		    if ((c = grabstring(seen,d, &len, &start)) != NULL) {
		        tops->len = len;
			tops->value.sym_data.cptype = c;
			/*tops->leavebl = 1; */
			tops->start = start;
		    } else {
			tops->value.sym_data.cptype = "\0";
			tops->len = 0;
		    }
		} else {
		    tops->value.sym_data.cptype = (char *)d;
		/*  seen = 0; */
		}
		break;
	}

	case AOP:
	case BOP:
	case UOP:
	case COP:
		tops->value.sym_op.opno = d->sym_op.opno;
		break;

	case RESDOM:
		tops->value.sym_resdom.resno = d->sym_resdom.resno;
		break;

	case AND:
	case OR:
		break;

	case AGHEAD:
	case BYHEAD:
	case ROOT:
	case QLEND:
		return (1);	/* all these are delimitors between lists */

	default:
		syserr("getsym:bad type %d", tops->type);
	}
	return(0);
}

/*
**  TOUT
**
**	Copies a symbol value into the Output tuple buffer.
** 	Used to write target
**	list elements or aggregate values into the output tuple.
*/
void
tout(register sym_t *s, char *offp, int rlen)
{
	register int	i;
	register char	*p;
	int		slen;

#ifdef xOTR1
	if (tTf(84, 3)) {
		printf("TOUT: s=");
		prstack(s);
		printf("  offset=%d, rlen=%d\n", offp-De.ov_outtup, rlen);
	}
#endif
	if (s->type == CHAR_CONST) {
		slen = s->len & I1MASK;
		rlen &= I1MASK;
		i = rlen - slen;	/* compute difference between sizes */
		if (i <= 0) {
			bmove(s->value.sym_data.cptype, offp, rlen);
		} else {
			p = s->value.sym_data.cptype;
			bmove(p, offp, slen);
			p = &offp[slen];
			while (i--)
				*p++ = ' ';	/* blank out remainder */
		}
	} else {
		bmove((char *)&s->value, offp, rlen);
	}
}

/*
** 	RCVT -	convert a symbol to a given type 
**
**	Parameters:
**		tos -  the symbol
**		restype - the type to convert it to
**		reslen - the length of the type
**
**	Trace Flags:
**		84.6
**
**	Called by:
**		interpret()
**		setallkey()
**		typecoerce()
*/
void
rcvt(register sym_t *tos, int restype, int reslen)
{
	register int	rtype, rlen;
	int		stype, slen;

	rtype = restype;
	rlen = reslen;
	stype = tos->type;
	slen= tos->len;
#ifdef xOTR1
	if (tTf(84, 6)) {
		printf("RCVT:type=");
		xputchar(rtype);
		printf("%3d, tos=", rlen);
		prstack(tos);
	}
#endif

	if (rtype != stype) {
		if (rtype == CHAR_CONST || stype == CHAR_CONST)
			ov_err(BADCONV);	/* bad char to numeric conversion requested */
		if (rtype == FLOAT_CONST) {
			itof(tos);
			if (rlen == 4)
				f8tof4(tos);
		} else {
			if (rlen == 4)
				ftoi4(tos);
			else
				ftoi2(tos);
			if (rlen == 1)
				tos->value.sym_data.i1type =
				  tos->value.sym_data.i2type;
		}
		tos->len = rlen;
	} else {
		if (rtype != CHAR_CONST && rlen != slen) {
			if (rtype == INT_CONST) {
				if (slen == 1)
					tos->value.sym_data.i2type =
					  tos->value.sym_data.i1type;
				if (rlen == 4)
					i2toi4(tos);
				else if (slen == 4)
						i4toi2(tos);
				if (rlen == 1)
					tos->value.sym_data.i1type =
					  tos->value.sym_data.i2type;
			} else if (rlen == 4)
				tos->value.sym_data.f4type =
				  tos->value.sym_data.f8type;
			else
				tos->value.sym_data.f8type =
				  tos->value.sym_data.f4type;

			tos->len = rlen;	/* handles conversion to i1 or f4 */
		}
	}
#ifdef xOTR3
	if (tTf(84, 6)) {
		printf("RCVT rets: symbol: ");
		prsym(tos);
	}
#endif
}

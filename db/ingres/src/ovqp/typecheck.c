#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "../decomp/globs.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_DECOMP
#include "protos.h"

SCCSID(@(#)typecheck.c	8.3	5/1/86)

/*
** TYPECHECK
**
**	Performs typechecking and conversions where appropriate .
**	Prohibits mixed type expressions.
*/
void
typecheck(sym_t *pp1, sym_t *pp2, int opval)
{
	register int	i;
	register sym_t	*p1, *p2;

	p1 = pp1;
	p2 = pp2;

	i = (p1->type == CHAR_CONST & p2->type == CHAR_CONST);
	/* i is non-zero only if both are chars */
	switch (opval) {

	  case opCONCAT:
		if (!i)
			ov_err(NUMERIC);	/* numeric in a char operator */
		return;	/* else no further checking is needed */

	  
	  case opADD:
	  case opSUB:
		if (i)
		    return;       /* make opADD and opSUB legal operators with char */
	  case opMUL:
	  case opDIV:
	  case opPOW:
	  case opMOD:
		if (i)
			ov_err(BADCHAR);	/* arithmetic operation on two character fields */
	}

	/* first check for identical types of symbols */
	if (p1->type == p2->type) {
		if (p1->len == p2->len)
			return;
		/* lengths are different. make p2 point to the smaller one */
		if (p1->len < p2->len) {
			p2 = p1;
			p1 = pp2;
		}

		switch (p2->type) {
		  case INT_CONST:
			if (p1->len == 2) {
				*(i2type *)&p2->value = *(i1type *)&p2->value;
				p2->len = 2;
			}
			else if (p1->len == 4)
				if (p2->len == 1) {
					*(i4type *)&p2->value = *(i1type *)&p2->value;
					p2->len = 4;
				}
				else
					i2toi4(p2);

		  case CHAR_CONST:
			return;	/* done if char or int */

		  case FLOAT_CONST:
			f8tof4(p1);
			return;
		}
	}

	/* at least one symbol is an INT_CONST or FLOAT_CONST. The other can't be a CHAR */
	if (p1->type == CHAR_CONST || p2->type == CHAR_CONST)
		ov_err(BADMIX);	/* attempting binary operation on one CHAR_CONST field with a numeric */

	/* one symbol is an INT_CONST and the other a FLOAT_CONST */
	if (p2->type == INT_CONST) {
		/* exchange so that p1 is an INT_CONST and p2 is a FLOAT_CONST */
		p1 = p2;
		p2 = pp1;
	}

	/* p1 is an INT_CONST and p2 a FLOAT_CONST */
	itof(p1);
	if (p2->len == 4)
		f8tof4(p1);
}
/*
**	Coerce the top of stack symbol to the
**	specified type and length. If the current
**	value is a character then it must be converted
**	to numeric. A user error will occure is the
**	char is not syntaxtically correct.
*/
void
typecoerce(sym_t *tosx, int ntype, int nlen)
{
	register sym_t	*tos;
	register char	*cp;
	register int	*val;
	int		ret;
	char		temp[256];

	ret = 0;
	tos = tosx;

	if (tos->type == CHAR_CONST) {
		val = (int *) &tos->value.sym_data;
		cp = temp;
		bmove(tos->value.sym_data.cptype, cp, tos->len & I1MASK);
		cp[tos->len & I1MASK] = '\0';
		if (ntype == FLOAT_CONST)
			ret = ingres_atof(cp, val);
		else {
			if (nlen == 4)
				ret = ingres_atol(cp, val);
			else
				*val = atoi(cp);
		}
		if (ret < 0)
			ov_err(CHARCONVERT);
		tos->type = ntype;
		tos->len = nlen;
	}
	else
		rcvt(tos, ntype, nlen);
}

void
i2toi4(sym_t *pp)
{
	register sym_t	*p;

#ifdef xOTR3
	if (tTf(87, 0)) {
		printf("i2toi4: ");
		prsym(pp);
	}
#endif
	p = pp;

	*(i4type *)&p->value = *(i2type *)&p->value;
	p->len = 4;
#ifdef xOTR3
	if (tTf(87, 0)) {
		printf("i2toi4 rets: ");
		prsym(p);
	}
#endif
}

void
i4toi2(sym_t *pp)
{
	register sym_t	*p;

#ifdef xOTR3
	if (tTf(87, 1)) {
		printf("i4toi2: ");
		prsym(pp);
	}
#endif
	p = pp;

	*(i2type *)&p->value = *(i4type *)&p->value;
	p->len = 2;
#ifdef xOTR3
	if (tTf(87, 1)) {
		printf("i4toi2 rets: ");
		prsym(p);
	}
#endif
}

void
itof(register sym_t *p)
{

#ifdef xOTR3
	if (tTf(87, 2)) {
		printf("itof: ");
		prsym(p);
	}
#endif

	if (p->len == 4)
		p->value.sym_data.f8type = p->value.sym_data.i4type;
	else if (p->len == 2)
		p->value.sym_data.f8type = p->value.sym_data.i2type;
	else
		p->value.sym_data.f8type = p->value.sym_data.i1type;
	p->type = FLOAT_CONST;
	p->len= 8;
#ifdef xOTR3
	if (tTf(87, 2)) {
		printf("itof rets: ");
		prsym(p);
	}
#endif
}

void
ftoi2(register sym_t *p)
{
#ifdef xOTR3
	if (tTf(87, 3)) {
		printf("ftoi: ");
		prsym(p);
	}
#endif

	if (p->len == 4)
		p->value.sym_data.i2type = p->value.sym_data.f4type;
	else
		p->value.sym_data.i2type = p->value.sym_data.f8type;
	p->type = INT_CONST;
	p->len = 2;
#ifdef xOTR3
	if (tTf(87, 3)) {
		printf("ftoi rets: ");
		prsym(p);
	}
#endif
}

void
ftoi4(register sym_t *p)
{
#ifdef xOTR3
	if (tTf(87, 4)) {
		printf("ftoi4: ");
		prsym(p);
	}
#endif

	if (p->len == 4)
		p->value.sym_data.i4type = p->value.sym_data.f4type;
	else
		p->value.sym_data.i4type = p->value.sym_data.f8type;
	p->type = INT_CONST;
	p->len = 4;
#ifdef xOTR3
	if (tTf(87, 4)) {
		printf("ftoi4 rets: ");
		prsym(p);
	}
#endif
}

void
f8tof4(sym_t *pp)
{
	pp->value.sym_data.f4type = pp->value.sym_data.f8type;
	pp->len = 4;
}

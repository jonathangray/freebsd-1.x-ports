#include <stdio.h>
#include <math.h>

#include "endian.h"

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include <access.h>
#include "../decomp/globs.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_DECOMP
#include "protos.h"

SCCSID(@(#)interp.c	8.6	5/30/88)


/*
**	Aggregate values are stored in De.ov_outtup. De.ov_tend points
**	to the spot for the next aggregate. Aop_interp()
**	computes the value for the aggregate and leaves
**	the result in the position pointed to by De.ov_tend.
*/
void
aop_interp(int opval, register sym_t *tos)
{
	register int	i;
	int		l1;
	ANYTYPE		numb;	/* used for type conversion */

#ifdef BIG_ENDIAN
	l1 = (tos->type != CHAR_CONST);
	switch (opval) {
	case opCOUNT:
	case opCOUNTU:
	  i = 4;
	  l1 = 1;
	  break;
	case opANY:
	  i = 2;
	  l1 = 1;
	  break;
	case opAVG:
	case opAVGU:
	  i = 8;
	  break;
	default:
	  i = tos->len;
	}
	if (l1) {
		l1 = De.ov_tend - De.ov_outtup;
		De.ov_tend = De.ov_outtup + ((l1 - 1)|(i-1))+1;
	}
#endif
	bmove(De.ov_tend, (char *) &numb, 8);
				/* note: this assumes that
				there are always 8 bytes which can be moved.
				** if it moves beyond De.ov_tend, it's ok */
	switch (opval) {

	  case opSUMU:
	  case opSUM:
		if (*De.ov_counter <= 1)
			goto puta;
		switch (tos->type) {
		  case INT_CONST:
			switch(tos->len) {
			  case 1:
				tos->value.sym_data.i1type += numb.i1type;
				goto puta;

			  case 2:
				tos->value.sym_data.i2type += numb.i2type;
				goto puta;

			  case 4:
				tos->value.sym_data.i4type += numb.i4type;
				goto puta;
			}

		  case FLOAT_CONST:
			if (tos->len == 4)
				tos->value.sym_data.f4type += numb.f4type;
			else
				tos->value.sym_data.f8type += numb.f8type;
			goto puta;

		  default:
			ov_err(BADSUMC);	/* cant sum char fields */
		}

	  case opCOUNTU:
	  case opCOUNT:
		tos->type = CNTTYPE;
		tos->len = CNTLEN;
		tos->value.sym_data.i4type = *De.ov_counter;
		goto puta;

	  case opANY:
		tos->type = OANYTYPE;
		tos->len = OANYLEN;
		if (*De.ov_counter) {
			tos->value.sym_data.i2type = 1;
			if (!De.ov_bylist && (De.ov_agcount == 1))
				De.ov_targvc = 0;	/* if simple agg. stop scan */
		}
		else
			tos->value.sym_data.i2type = 0;
		goto puta;

	  case opMIN:
	  case opMAX:
		if (*De.ov_counter<=1)
			goto puta;
		switch (tos->type) {
		  case INT_CONST:
			switch (tos->len) {
			  case 1:
				i = (tos->value.sym_data.i1type < numb.i1type);
				break;

			  case 2:
				i = (tos->value.sym_data.i2type < numb.i2type);
				break;

			  case 4:
				i = (tos->value.sym_data.i4type < numb.i4type);
				break;
			}
			break;

		  case FLOAT_CONST:
			if (tos->len == 4)
				i = (tos->value.sym_data.f4type < numb.f4type);
			else
				i = (tos->value.sym_data.f8type < numb.f8type);
			break;

		  case CHAR_CONST:
			l1 = size(tos);
			i = (lexcomp(tos->value.sym_data.cptype, l1, De.ov_tend, l1,0) < 0);
			break;

		  default:	
			syserr("interp:bad op type for opMIN/MAX %d", tos->type);
		}

		/* check result of comparison */
		if (opval == opMAX)
			i = !i;	/* complement test for opMAX */
		if (i)
			goto puta;	/* condition true. new value */

		/* condition false. Keep old value */
		goto done;


	  case opAVGU:
	  case opAVG:
		if (tos->type == INT_CONST)
			itof(tos);
		else
			if (tos->type == CHAR_CONST)
				ov_err(BADAVG);
		if (tos->len == 4) {
			tos->value.sym_data.f8type = tos->value.sym_data.f4type;
		/*	tos->len = 8; */
		}
		if (*De.ov_counter > 1) {
			tos->value.sym_data.f8type = numb.f8type + (tos->value.sym_data.f8type - numb.f8type) / *De.ov_counter;
		}
		tos->len = 8;
		goto puta;

	  default:
		syserr("interp:bad agg op %d", tos->type);
	}

puta:
	tout(tos, De.ov_tend, tos->len);
done:
	De.ov_tend += tos->len & I1MASK;
}


/*
**	relop_interp interprets the relational operators
**	(ie. EQ, NE etc.) and returns true or false
**	by evaluating l1.
**
**	l1 should be greater than, equal or less than zero.
*/
int
relop_interp(int opval, int l1)
{
	register int	i;

	i = l1;

	switch (opval) {

	  case opEQ:
		return (i == 0);

	  case opNE:
		return (i != 0);

	  case opLT:
		return (i < 0);

	  case opLE:
		return (i <= 0);

	  case opGT:
		return (i > 0);

	  case opGE:
		return (i >= 0);

	  default:
		syserr("relop:bad relop or bop %d", opval);
	}
	/*NOTREACHED*/
	return(0);
}

/*
**
** INTERPRET
**
**	 Processes the retrieved tuple from the De.ov_source relation
**	 according to the symbols in the list.  Recognition
**	 of characteristic delimiters and separators initiates
**	 action appropriate to a target list or qualification list
**	 as the case may be.
**	
**	 Between delimiters, the symbol list is expected to be in
**	 Polish postfix form.  A qualification list is further
**	 expected to be in conjunctive normal form, with Boolean
**	 operators infixed.
**
*/


#define SPUSH(v)	(++((struct stacksym *) v))
#define SPOP(v)	(((struct stacksym *) v)--)

sym_t *
interpret(int istlist, sym_t **list)
/* istlist - signals a target list: used for string substitution */
/* list - ptr to list of sym pointers */
{
	register sym_t		*tos;
	sym_t			*op1,*op2;	/*operands popped off stack*/
	register ANYTYPE	*val1,*val2;	/*ptrs to values of operands*/
	int			opval, optype, l1;
	char			*s1;
	int			byflag;
	tid_t			hitid;
	extern char		*Usercode;
	int			cb_mark;
	long			lvar;

#ifdef xOTR1
	if (tTf(72, 0)) {
		printf("INTERP:  list=%p\n",list);
		printf("         istlist = %d\n", istlist);
	}
#endif


	byflag = (list == De.ov_bylist);	/* set byflag if aggregate function */
	tos = (sym_t *)(De.ov_stack-1);
	/* reset the concat and ascii operator buffer */
	seterr(De.ov_ovqpbuf, CBUFULL, ov_err);
	cb_mark = markbuf(De.ov_ovqpbuf);

loop:
#ifdef xOTR1
	if (tTf(72, 1) && tos >= (sym_t *) De.ov_stack) {
		printf("\ttops of stack=");
		prstack(tos);	/* print top of stack element */
	}
#endif
	/* check for stack overflow */
	l1 = getsymbol(SPUSH(tos), &list); 
	/* getsymbol changes the value of list */

	if (l1) {

		freebuf(De.ov_ovqpbuf, cb_mark);
		return (tos);
	}
	optype = tos->type;
	opval = tos->value.sym_data.i2type;
	op1 = tos;
	SPOP(tos);		/* assume that stack will be popped */

	switch(optype) {
	  case CHAR_CONST:
					/* do any chars have to be inserted? */
		if (istlist && (Patnum || Globnum)) {
		    insert_chars(op1);
		}
	  case INT_CONST:
	  case FLOAT_CONST:
		SPUSH(tos);		/* just leave symbol on stack */
		goto loop;

	  case COP:
		SPUSH(tos);		/* new symbol goes on stack */
		tos->type = CHAR_CONST;
		switch (opval) {

		  case opDBA:
			tos->value.sym_data.cptype = Admin.ad_h.adm_owner;
			tos->len = 2;
			goto loop;

		  case opUSERCODE:
			tos->value.sym_data.cptype = Usercode;
			tos->len = 2;
			goto loop;
		}

	  case AND:		/* if top value false return immediately */
		if (!tos->value.sym_data.i2type) {
			freebuf(De.ov_ovqpbuf, cb_mark);
			return(tos);
		}
		else
			SPOP(tos);
		freebuf(De.ov_ovqpbuf, cb_mark);
		goto loop;

	  case OR:		/* if top value is true then skip to
				** end of disjunction. */
		if (tos->value.sym_data.i2type) {
			SPUSH(tos);
			do {
				getsymbol(tos, &list); 	/* getsymbol changes the value of list */
			} while (tos->type != AND);
			optype = AND;
			SPOP(tos);
		}
		SPOP(tos);
		goto loop;

	  case RESDOM:
		freebuf(De.ov_ovqpbuf, cb_mark); /* init the concat and ascii buffer */
		if (De.ov_result) {
			if (opval)	/* if gt zero then opval represents a real domain */ {
				if (byflag)
					opval++;	/* skip over count field for ag functs */
				rcvt(tos, De.ov_result->d_fmt[opval], De.ov_result->d_len[opval]);
				tout(tos, De.ov_outtup+De.ov_result->d_off[opval], De.ov_result->d_len[opval]);
			} else {
				/* opval refers to the tid,
				 * and this is an update */
				(void) memcpy(&De.ov_uptid, &tos->value.sym_data.i4type, sizeof(De.ov_uptid));
				/* copy tid */
				if (De.de_qmode == mdREPL ||
				    (De.ov_diffrel &&
				     De.de_qmode == mdDEL &&
				     De.ov_result->d_r.r_indexed > 0 )) {
					/* De.ov_origtup must be left with the orig
					** unaltered tuple, and De.ov_outtup must
					** be initialized with the orig tuple.
					**
					** De.ov_outtup only matters with REPL.
					** Scan() sets up De.ov_origtup so when
					** De.ov_source == De.ov_result, origtup is already
					** correct.
					*/

					if (De.ov_diffrel) {
						if ((l1 = get(De.ov_result, &De.ov_uptid, &hitid, De.ov_origtup, CURTUP)) != 0) {
							(void) memcpy(&lvar, &De.ov_uptid, sizeof(lvar));
							syserr("interp:get on resdom %s, %d", locv(lvar), l1);
						}
						bmove(De.ov_origtup, De.ov_outtup, De.ov_result->d_r.r_width);
					} else {
						bmove(De.ov_intup, De.ov_outtup, De.ov_result->d_r.r_width);
					}
				}
			}
		} else {
			/*
			** This is really here for the 68k machines,
			** this works on the VAX, but nowhere else...
			*/
#ifdef BIG_ENDIAN
 			if ( tos->type == INT_CONST && tos->len == 1)
 				tos->value.sym_data.i1type = tos->value.sym_data.i2type;
#endif
			if (Equel)
				equelatt(tos);	/* send attribute to equel */
			else {
				if (tos->type == CHAR_CONST)
					s1 = tos->value.sym_data.cptype;
				else
					s1 = tos->value.sym_data.c0type;
				printatt(tos->type, tos->len & I1MASK, s1);	/* print attribute */
			}
		}
		SPOP(tos);
		goto loop;


	  case BOP:
		op2 = (sym_t *)SPOP(tos);
		op1 = (sym_t *)tos;
		typecheck(op1, op2, opval);
		val1 = &op1->value.sym_data;
		val2 = &op2->value.sym_data;

		switch (tos->type) {
		  case INT_CONST:
			switch (tos->len) {
			  case 1:
				switch (opval) {
				  case opADD:
					val1->i1type += val2->i1type;
					goto loop;

				  case opSUB:
					val1->i1type -= val2->i1type;
					goto loop;

				  case opMUL:
					val1->i1type *= val2->i1type;
					goto loop;

				  case opDIV:
					val1->i1type /= val2->i1type;
					goto loop;

				  case opMOD:
					val1->i1type %= val2->i1type;
					goto loop;

				  case opPOW:
					itof(op1);
					itof(op2);
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  /* relational operator */
				  default:
					tos->len = 2;
					if (val1->i1type > val2->i1type)
						l1 = 1;
					else
						if (val1->i1type == val2->i1type)
							l1 = 0;
						else
							l1 = -1;
					val1->i2type = relop_interp(opval, l1);
					goto loop;
				}
			  case 2:
				switch (opval) {
				  case opADD:
					val1->i2type += val2->i2type;
					goto loop;

				  case opSUB:
					val1->i2type -= val2->i2type;
					goto loop;

				  case opMUL:
					val1->i2type *= val2->i2type;
					goto loop;

				  case opDIV:
					val1->i2type /= val2->i2type;
					goto loop;

				  case opMOD:
					val1->i2type %= val2->i2type;
					goto loop;

				  case opPOW:
					itof(op1);
					itof(op2);
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  /* relational operator */
				  default:
					l1 = val1->i2type - val2->i2type;
					val1->i2type = relop_interp(opval, l1);
					goto loop;
				}

			  case 4:
				switch(opval) {
				  case opADD:
					val1->i4type += val2->i4type;
					goto loop;

				  case opSUB:
					val1->i4type -= val2->i4type;
					goto loop;

				  case opMUL:
					val1->i4type *= val2->i4type;
					goto loop;

				  case opDIV:
					val1->i4type /= val2->i4type;
					goto loop;

				  case opMOD:
					val1->i4type %= val2->i4type;
					goto loop;

				  case opPOW:
					itof(op1);
					itof(op2);
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  /* relational operator */
				  default: 
					tos->len = 2;
					if (val1->i4type > val2->i4type)
						l1 = 1;
					else
						if (val1->i4type == val2->i4type)
							l1 = 0;
						else
							l1 = -1;

					val1->i2type = relop_interp(opval, l1);
					goto loop;

				}
			}

		  case FLOAT_CONST:
			switch (tos->len) {
			  case 4:
				switch (opval) {
				  case opADD:
					val1->f4type += val2->f4type;
					goto loop;

				  case opSUB:
					val1->f4type -= val2->f4type;
					goto loop;

				  case opMUL:
					val1->f4type *= val2->f4type;
					goto loop;

				  case opDIV:
					val1->f4type /= val2->f4type;
					goto loop;

				  case opPOW:
					val1->f4type = pow(val1->f4type, val2->f4type);
					goto loop;

				  default:
					tos->type = INT_CONST;
					tos->len = 2;
					if (val1->f4type > val2->f4type)
						l1 = 1;
					else
						if (val1->f4type == val2->f4type)
							l1 = 0;
						else
							l1 = -1;
					val1->i2type = relop_interp(opval, l1);
					goto loop;
				}
			  case 8:
				switch (opval) {
				  case opADD:
					val1->f8type += val2->f8type;
					goto loop;

				  case opSUB:
					val1->f8type -= val2->f8type;
					goto loop;

				  case opMUL:
					val1->f8type *= val2->f8type;
					goto loop;

				  case opDIV:
					val1->f8type /= val2->f8type;
					goto loop;

				  case opPOW:
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  default:
					tos->type = INT_CONST;
					tos->len = 2;
					if (val1->f8type > val2->f8type)
						l1 = 1;
					else
						if (val1->f8type == val2->f8type)
							l1 = 0;
						else
							l1 = -1;
					val1->i2type = relop_interp(opval, l1);
					goto loop;
				}
			}

		case CHAR_CONST:
			switch (opval) {
				case opSUB:
				    newstring(op1, op2);
				    goto loop;
				case opADD:
				case opCONCAT:
				    concatsym(op1, op2);	/* concatenate the two symbols */
				    goto loop;
				default:
				    l1 = lexcomp(val1->cptype, size(op1), val2->cptype, op2->len & I1MASK,0);
				    tos->type = INT_CONST;
				    tos->len = 2;
				    val1->i2type = relop_interp(opval, l1);
				    goto loop;
			}
		}	/* end of BOP */
 
	   case UOP:
		val1 = &tos->value.sym_data;
		switch (opval) {
		   case opMINUS:
		   case opABS:
			if (tos->type == CHAR_CONST)
				ov_err(BADUOPC);
			l1 = opval == opMINUS;
			switch (tos->type) {
			   case INT_CONST:
				switch (tos->len) {
				  case 1:
					if (l1 || val1->i1type < 0)
						val1->i1type = -val1->i1type;
			   		goto loop;
				  case 2:
					if (l1 || val1->i2type < 0)
						val1->i2type = -val1->i2type;
			   		goto loop;

				  case 4:
					if (l1 || val1->i4type < 0)
						val1->i4type = -val1->i4type;
					goto loop;
				}

			  case FLOAT_CONST:
				switch (tos->len) {
				  case 4:
					if (l1 || val1->f4type < 0.0)
						val1->f4type = -val1->f4type;
					goto loop;
				  case 8:
					if (l1 || val1->f8type < 0.0)
						val1->f8type = -val1->f8type;
					goto loop;
				}
			}

		  case opNOT:
			val1->i2type = !val1->i2type;
		  case opPLUS:
			if (tos->type == CHAR_CONST)
				ov_err(BADUOPC);
			goto loop;

		  case opASCII:
			ascii(tos);
			goto loop;

		  case opINT1:
			typecoerce(tos, INT_CONST, 1);
			goto loop;

		  case opINT2:
			typecoerce(tos, INT_CONST, 2);
			goto loop;

		  case opINT4:
			typecoerce(tos, INT_CONST, 4);
			goto loop;

		  case opFLOAT4:
			typecoerce(tos, FLOAT_CONST, 4);
			goto loop;

		  case opFLOAT8:
			typecoerce(tos, FLOAT_CONST, 8);
			goto loop;

		  default:
			if (tos->type == CHAR_CONST)
				ov_err(BADUOPC);
			if (tos->type == INT_CONST)
				itof(tos);
			if (tos->len == 4) {
				val1->f8type = val1->f4type;
				tos->len = 8;
			}
			switch (opval) {
			  case opATAN:
				val1->f8type = atan(val1->f8type);
				goto loop;
	
			  case opLOG:
				val1->f8type = log(val1->f8type);
				goto loop;
	
			  case opSIN:
				val1->f8type = sin(val1->f8type);
				goto loop;

			  case opCOS:
				val1->f8type = cos(val1->f8type);
				goto loop;

			  case opSQRT:
				val1->f8type = sqrt(val1->f8type);
				goto loop;

			  case opEXP:
				val1->f8type = exp(val1->f8type);
				goto loop;

			  default:
				syserr("interp:bad uop %d",opval);
			}
		}


	   case AOP:
		aop_interp(opval, tos);
		SPOP(tos);		/* pop this symbol */
		goto loop;

	}
	syserr("interp: fell out");
	/*NOTREACHED*/
	return((sym_t *) NULL);
}

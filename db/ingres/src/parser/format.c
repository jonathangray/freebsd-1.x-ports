#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "parser.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)format.c	8.2	2/8/85)

/*
**  FORMAT
**	routine to compute the format of the result relation attributes
**	it is called after ATTLOOKUP so the tuple defining the current
**	attribute is already available.
**	if the element is a function of more than one attribute, the result
**	domain format is computed by the following rules:
**		- no fcns allowed on character attributes
**		- fcn of integer attribs is an integer fcn with
**		  length = MAX(length of all attributes)
**		- fcn of floating point attribs is a floating point
**		  fcn with length = MIN(length of all attribs)
**		- fcn of integer and floating attributes is a
**		  floating fcn with length = MIN(length of all floating
**		  attributes)
**
**	Trace Flags:
**		Format ~~ 52.0, 52.1
*/
void
format(qtree_t *result1)
{
	register char		rfrmt;
	register char		rfrml;
	register qtree_t		*result;
	struct constop		*cpt;

	extern struct out_arg	Out_arg;
	extern struct constop	Coptab[];
	extern char		Trfrml;
	extern char		Trfrmt;
	extern int		Qlflag;

	rfrmt = rfrml = 0;
#ifdef	xPTR2
	tTfp(52, 0, "format:.\n");
#endif

	result = result1;
	switch (result->sym.type) {
	  case VAR:
		rfrmt = result->sym.value.sym_var.varfrmt;
		rfrml = result->sym.value.sym_var.varfrml;
      		break;

	  case AOP:
		switch (result->sym.value.sym_op.opno) {
		  case opAVG:
		  case opAVGU:
			rfrmt = FLOAT_CONST;
			rfrml = 8;
			if (result->sym.value.sym_op.agfrmt == CHAR_CONST)
				/* character domain not allowed in these aggs */
				par_error(AVGTYPE, WARN, 0, 0, 0);
			break;

		  case opCOUNT:
		  case opCOUNTU:
			rfrmt = INT_CONST;
			rfrml = 4;
			break;

		  case opANY:
			rfrmt = INT_CONST;
			rfrml = 2;
			break;

		  case opSUM:
		  case opSUMU:
			rfrmt = result->sym.value.sym_op.agfrmt;
			rfrml = result->sym.value.sym_op.agfrml;
			if (rfrmt == CHAR_CONST)
				/* no char domains for these aggs */
				par_error(SUMTYPE, WARN, 0, 0, 0);
			break;

		  default:
			rfrmt = result->sym.value.sym_op.agfrmt;
			rfrml = result->sym.value.sym_op.agfrml;
			break;
		}
		break;

	  case AGHEAD:
		/*
		** can get format info from the AOP node because
		** it already has format info computed
		*/
		if (result->left->sym.type == AOP) {
			/* no by-list */
			rfrmt = result->left->sym.value.sym_op.opfrmt;
			rfrml = result->left->sym.value.sym_op.opfrml;
		} else {
			/* skip over by-list */
			rfrmt = result->left->right->sym.value.sym_resdom.resfrmt;
			rfrml = result->left->right->sym.value.sym_resdom.resfrml;
		}
		break;

	  case RESDOM:
		format(result->right);
		return;

	  case INT_CONST:
	  case FLOAT_CONST:
	  case CHAR_CONST:
		rfrmt = result->sym.type;
		rfrml = result->sym.len;
		break;

	  case COP:
		for (cpt = Coptab; cpt->copname; cpt++) {
			if (result->sym.value.sym_op.opno == cpt->copnum) {
				rfrmt = cpt->coptype;
				rfrml = cpt->coplen;
				break;
			}
		}
		if (!cpt->copname)
			syserr("bad cop in format(%d)", result->sym.value.sym_op.opno);
		break;

	  case UOP:
		switch (result->sym.value.sym_op.opno) {
		  case opATAN:
		  case opCOS:
		  case opLOG:
		  case opSIN:
		  case opSQRT:
		  case opEXP:
			format(result->left);
			if (Trfrmt == CHAR_CONST)
				/*
				** no character expr in FOP
				** if more ops are added, must change error message				*/
				par_error(FOPTYPE, WARN, 0, 0, 0);

		  case opFLOAT8:
			/* float8 is type conversion and can have char values */
			rfrmt = FLOAT_CONST;
			rfrml = 8;
			break;

		  case opFLOAT4:
			rfrmt = FLOAT_CONST;
			rfrml = 4;
			break;

		  case opINT1:
			rfrmt = INT_CONST;
			rfrml = 1;
			break;

		  case opINT2:
			rfrmt = INT_CONST;
			rfrml = 2;
			break;

		  case opINT4:
			rfrmt = INT_CONST;
			rfrml = 4;
			break;

		  case opASCII:
			format(result->left);
			rfrmt = CHAR_CONST;
			rfrml = Trfrml;
			if (Trfrmt == INT_CONST) {
				if (Trfrml == 2)
					rfrml = Out_arg.i2width;
				else if (Trfrml == 4)
					rfrml = Out_arg.i4width;
				else if (Trfrml == 1)
					rfrml = Out_arg.i1width;
				else
					syserr("bad length %d for INT_CONST", Trfrml);
				break;
			}
			if (Trfrmt == FLOAT_CONST) {
				if (Trfrml == 8)
					rfrml = Out_arg.f8width;
				else if (Trfrml == 4)
					rfrml = Out_arg.f4width;
				else
					syserr("bad length %d for FLOAT_CONST", Trfrml);
				break;
			}
			if (Trfrmt == CHAR_CONST)
				break;
			syserr("bad frmt in opASCII %d", Trfrmt);

		  case opNOT:
			if (!Qlflag)
				syserr("opNOT in targ list");
			return;

		  case opMINUS:
		  case opPLUS:
			format(result->right);
			if (Trfrmt == CHAR_CONST)
				/* no chars for these unary ops */
				par_error(UOPTYPE, WARN, 0, 0, 0);
			return;

		  case opABS:
			format(result->left);
			if (Trfrmt == CHAR_CONST)
				/* no char values in fcn */
				par_error(FOPTYPE, WARN, 0, 0, 0);
			return;

		  default:
			syserr("bad UOP in format %d", result->sym.value.sym_op.opno);
		}
		break;

	  case BOP:
		switch (result->sym.value.sym_op.opno) {

		  case opEQ:
		  case opNE:
		  case opLT:
		  case opLE:
		  case opGT:
		  case opGE:
			if (!Qlflag)
				syserr("LBOP in targ list");
			format(result->right);
			rfrmt = Trfrmt;
			format(result->left);
			if ((rfrmt == CHAR_CONST) != (Trfrmt == CHAR_CONST))
				/* type conflict on relational operator */
				par_error(RELTYPE, WARN, 0, 0, 0);
			return;

		  case opADD:
		  case opSUB:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt == FLOAT_CONST || Trfrmt == FLOAT_CONST) {
				if (rfrmt == FLOAT_CONST && Trfrmt == FLOAT_CONST) {
					if (Trfrml < rfrml)
						rfrml = Trfrml;
				}
				else if (Trfrmt == FLOAT_CONST)
					rfrml = Trfrml;
				rfrmt = FLOAT_CONST;
			}
			else
				if (Trfrml > rfrml)
					rfrml = Trfrml;
			break;

		  case opMUL:
		  case opDIV:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if ((rfrmt == CHAR_CONST || Trfrmt == CHAR_CONST))
				par_error(NUMTYPE, WARN, 0, 0, 0);
			if (rfrmt == FLOAT_CONST || Trfrmt == FLOAT_CONST) {
				if (rfrmt == FLOAT_CONST && Trfrmt == FLOAT_CONST) {
					if (Trfrml < rfrml)
						rfrml = Trfrml;
				}
				else if (Trfrmt == FLOAT_CONST)
					rfrml = Trfrml;
				rfrmt = FLOAT_CONST;
			}
			else
				if (Trfrml > rfrml)
					rfrml = Trfrml;
			break;

		  case opMOD:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt != INT_CONST || Trfrmt != INT_CONST)
				/* mod operator not defined */
				par_error(MODTYPE, WARN, 0, 0, 0);
			if (Trfrml > rfrml)
				rfrml = Trfrml;
			break;

		  case opPOW:
			format(result->right);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->left);
			if (rfrmt == CHAR_CONST || Trfrmt == CHAR_CONST)
				/* no char values for pow */
				par_error(NUMTYPE, WARN, 0, 0, 0);
			if ((rfrmt == FLOAT_CONST && rfrml == 4) || (Trfrmt == FLOAT_CONST && Trfrml == 4)) {
				rfrmt = FLOAT_CONST;
				rfrml = 4;
			} else {
				rfrmt = FLOAT_CONST;
				rfrml = 8;
			}
			break;

		  case opCONCAT:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt != CHAR_CONST || Trfrmt != CHAR_CONST)
				/* only character domains allowed */
				par_error(CONCATTYPE, WARN, 0, 0, 0);
			rfrml += Trfrml;
			break;

		  default:
			syserr("bad BOP in format %d", result->sym.value.sym_op.opno);
		}
	}
	Trfrmt = rfrmt;
	Trfrml = rfrml;
#ifdef	xPTR2
	tTfp(52, 2, "format>>: Trfrmt = %d, Trfrml = %d.\n", Trfrmt, Trfrml);
#endif
}

#include <stdio.h>

#include <ingres.h>
#include "../equel/constants.h"
#include "IIglobals.h"
#include "sccs.h"
#include <errors.h>

#include "protos.h"

SCCSID(@(#)IIw_right.c	8.2	2/8/85)


/*
**  IIw_right -- Write down to the Quel parser a string
**	for a target list of anything but a tupret.
**
**	Parameters:
**		string -- a string which contains the target list
**			of a quel statement, where values from C variables
**			to be plugged in are flagged by the construct
**			'%<ingres_type>" a la printf(). String is left 
**			unchanged. 
**			To escape the '%' mechanism use '%%'.
**
**		argv -- a vector of pointers to
**			variables from which the values flagged by the '%'
**			mechanism are taken.
**
**	Usage:
**		argv [0] = &double_raise;
**		IIw_right("dom1 = i.ifield * (1+%f8)", argv);
**
**	Required By:
**		all the parametrized statements except "tupret_p".
**
**	Error numbers:
**		1004 -- bad type in parametrized statement.
*/


void
IIw_right(char *string, char **argv)
{
	register char	*b_st, *e_st;
	register char	**av;

	if (IIdebug)
		printf("ent IIw_right : string \"%s\"\n",
		string);
	av = argv;
	for (b_st = e_st = string; *e_st; ) {
		if (*e_st != '%') {
			e_st++;
			continue;
		}

		/* provide '%%' escape mechanism */
		if (e_st [1] == '%') {
			e_st [1] = '\0';
			IIwrite(b_st);
			e_st [1] = '%';
			b_st = e_st = &e_st [2];
			continue;
		}
		*e_st = '\0';
		IIwrite(b_st);
		*e_st++ = '%';

		switch (*e_st) {

		  case 'f' :
			switch (*++e_st) {

			  case '8' :
				IIcvar(*av, opDOUBLE, 8);
				break;

			  case '4' :
				IIcvar(*av, opFLOAT, 4);
				break;

			  default :
				goto error_label;
			}
			av++;
			break;

		  case 'i' :
			switch (*++e_st) {

			  case '4' :
				IIcvar(*av, opLONG, 4);
				break;

			  case '2' :
				IIcvar(*av, opSHORT, 2);
				break;

			  case '1' :
				IIcvar(*av, opCHAR, 1);

			  default :
				goto error_label;
			}
			av++;
			break;

		  case 'c' :
			IIcvar(*av++, opSTRING, 0);
			break;
		}
		b_st = ++e_st;
	}
	IIwrite(b_st);
	return;


error_label :
	IIerror(BADSTMT, 1, &string); 
	IIerrflag = 1004;
	/* make sure that part already written down will 
	 * cause an error, and don't print it.
	 *	The old IIprint_err is restored in IIerror()
	 */
	IIwrite(",");
	IIo_print = IIprint_err;
	IIprint_err = IIno_err;
}

#include <stdio.h>
#include "constants.h"
#include "globals.h"
#include "y.tab.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)yyerror.c	8.1	12/31/84)

extern	int	Exit_val;		/* value to exit with, incremented if error found */


/*
**  YYERROR -- Yacc error reporting routine.
**	Yyerror reports on syntax errors encountered by 
**	the yacc parser, and increments the Exit_val variable. 
**
**	Parameters:
**		s -- a string explaining the error
**
**	Returns:
**		none
*/

void
yyerror(char *s)
{

	printf("\"%s\", line %d: ", Input_file_name, yyline);
	if (yychar == 0)
		printf("EOF = ");
	if (yylval.u_dn)
		printf("\"%s\" ", yylval.u_dn->d_elm);
	printf("%s\n", s);
	Exit_val++;
}

/*
**  YYSEMERR -- scanner error reporter
**		Also increments the Exit_val variable.
**	Parameters:
**		s -- string explaining the error
**		i -- if !0 a string which caused the error
**
**	Returns:
**		none
**
**	Called By:
**		lexical analysis routines -- if called from somewhere else,
**			the line number is likely to be wrong.
*/

void
yysemerr(char *s, char *i)
{
	printf("\"%s\", line %d: ", Input_file_name, yyline);
	if (i)
		printf("\"%s\": ", i);
	printf("%s\n", s);
	Exit_val++;
}

/*
**  YYSERROR -- Semantic error reportin routine
**	reports on an error on an entry in the symbol space,
**	using the line number built into the entry. Exit_val gets
**	incremented.
**
**	Parameters:
**		s -- a string explaining the error
**		d -- a symbol space node
**
**	Returns:
**		none
**
**	Called By:
**		semantic productions
*/

void
yyserror(char *s, struct disp_node *d)
{
	printf("\"%s\", line %d: ", Input_file_name, d->d_line);
	printf("\"%s\": %s\n", d->d_elm, s);
	Exit_val++;
}

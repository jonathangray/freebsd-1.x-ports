#include <stdio.h>

#include "constants.h"
#include "globals.h"
#include "y.tab.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)name.c	8.1	12/31/84)


/*
**  NAME -- Process an identifier or keyword token.
**
**	Name gets the identifier that follows in the std.
**	input, and checks if it is a keyword.
**	An identifier is defined as a sequence of
**	MAXEQNAME or fewer alphanumerics, starting with an
**	alphabetic character.
**
**	Parameters:
**		chr - the first character of the identifier
**
**	Returns:
**		Tokens.sp_name - for a user-defined name
**		Tokens.sp_struct_var -- if the name is declared 
**			a structurw variable
**		other - lexical codes for keys
**
**	Side Effects:
**		Adds a token to the symbol space.
**		yylval is set to the new node in the space.
**		If the identifier is a keyword, sets Opcode to
**		op_code from tokens.y.
*/
int
name(char chr)
{
	int			lval;
	register		i;
	char			wbuf [MAXEQNAME + 1];
	register char		*cp;
	register char		c;
	struct optab		*op;
	cvar_t		*hold;

	c = chr;
	cp = wbuf;
	for (i = 0; i <= MAXEQNAME; i++) {
		lval = equel_cmap(c);
		if (i < MAXEQNAME &&
		   (lval == ALPHA || lval == NUMBR)) {
			*cp++ = c;
			c = getch();
		} else if (lval == ALPHA || lval == NUMBR) {
			/* {i == MAXEQNAME && "c is legal" && 
			 *  cp == &wbuf [MAXEQNAME]} 
			 */
			*cp = '\0';
			yysemerr("name too long", wbuf);
			/* chomp to end of identifier */

			do {
				c = getch();
				lval = equel_cmap(c);
			}  while (lval == ALPHA || lval == NUMBR);
			backup(c);
			
			/* take first MAXEQNAME characters as IDENTIFIER 
			 * (non-key)
			 */
			yylval.u_dn = addsym(salloc(wbuf));
			return (Tokens.sp_name);
		} else {
			/* {cp <= &wbuf [MAXEQNAME] && i <= MAXEQNAME
			 * && "c is not part of id"}
			 */
			backup(c);
			*cp = '\0';
			i = 0;
			break;
		}
	}
	op = getkey(wbuf);

	/* Is it a keyword ? */
	if (op) {
		yylval.u_dn = addsym(op->op_term);
		Opcode = op->op_code;
		return (op->op_token);
	}
	/* user-defined name */
	yylval.u_dn = addsym(salloc(wbuf));
	hold = getcvar(wbuf);
	if (hold != 0 && hold->c_type == opSTRUCT)
		return(Tokens.sp_struct_var);
	return (Tokens.sp_name);
}

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include "scanner.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)s_name.c	8.2	2/8/85)

/*
** NAME
** A name is defined to be a sequence of MAX_NAME_SIZE or fewer alphanumeric
** characters, starting with an alphabetic (underscore "_" is considered
** an alphabetic).  If it is not a keyword, each name is entered into
** the symbol table, indexed by 'yylval'.  A token is then returned for
** that name.
*/
int
name(char chr)
{
	extern char		*yylval;
	char			namebuf[MAX_NAME_SIZE + 1];
	register int		hi, lo, curr;

	/* fill in the name */
	yylval = namebuf;
	*yylval = chr;
	do {
		*++yylval = get_scan(NORMAL);
		if ((yylval - namebuf) > MAX_NAME_SIZE) {
			/* name too long */
			*yylval = '\0';
			par_error(NAMELONG, WARN, namebuf, 0, 0);
		}

	}  while (parser_cmap(*yylval) == ALPHA || parser_cmap(*yylval) == NUMBR);
	backup(*yylval);
	*yylval = '\0';

	/* is it a keyword ? */
	lo = 0;
	hi = Keyent - 1;
	while (lo <= hi) {
		curr = (lo + hi) / 2;
		switch (scompare(Keyword[curr].term, MAX_NAME_SIZE, namebuf, MAX_NAME_SIZE)) {
		  case 1:
			hi = curr - 1;
			continue;

		  case -1:
			lo = curr + 1;
			continue;

		  case 0:
			Lastok.toktyp = Tokens.sconst;
			Lastok.tok = Keyword[curr].term;
			Lastok.tokop = Keyword[curr].opcode;
			yylval = (char *) Lastok.tokop;
			return (Keyword[curr].token);
		}
	}

	/* else, USER DEFINED NAME */
#ifdef	xSTR2
	tTfp(71, 0, "name: %s\n", namebuf);
#endif
	yylval = syment(namebuf, strlen(namebuf) + 1);
	Lastok.tok = yylval;
	Lastok.toktyp = Tokens.sconst;
	Lastok.tokop = 0;
	return (Tokens.name);
}

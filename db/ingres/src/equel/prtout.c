#include <stdio.h>
#include "constants.h"
#include "globals.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)prtout.c	8.1	12/31/84)


/*
**  PRTOUT.C -- output routines
**
**	Output routines for non-specific data structures
**	(i.e. w_display is in [display.c])
*/

int	Fillcnt		= FILLCNT;

/*
**  W_RAW -- Lowest level output character routine
**
**	Outputs string depending on Fillcnt and In_quote
**	and In_string and Fillmode.
**	When not in Fillmode does straight output.
**	When on Fillmode, fills lines to Fillmode.
**
**	NOTE : w_raw will feel free to output a newline after 
**		'string' if the string causes more than Fillcnt
**		characters to be output.
**		Inside strings (In_string != 0) w_raw will put
**		a '\\' before the newline issued.
**		When In_quote != 0 when the fillcnt is exceeded,
**		the IIwrite( is ended an continued on the next line
**		so that the query string won't overflow the C 
**		pre-processor's line buffer.
*/

void
w_raw(char *string)
{
	register char	*s;
	register	charcnt;

	charcnt = 0;
	for (s = string; *s; s++) {
		if (*s != '\n') {
			putc(*s, Out_file);
			charcnt++;
		} else {
			if (Fillmode == 0 ||
			   Charcnt + charcnt > Fillcnt ||
			   In_string) {
				putc(*s, Out_file);
				Lineout++;
				charcnt = 0;
				Charcnt = 0;
			} else {
				putc(' ', Out_file);
				charcnt++;
			}
		}
	}
	if ((Charcnt += charcnt) > Fillcnt && Fillmode == 1) {
		if (In_string) {
			if (In_quote) {
				fputs("\");\nIIwrite(\"", Out_file);
				Charcnt = 9;
			} else {
				fputs("\\\n", Out_file);
				Charcnt = 0;
			}
		} else {
			putc('\n', Out_file);
			Charcnt = 0;
		}
		Lineout++;
	}
}

/*
**  W_KEY -- write out a string needing a blank to
**	     separate it from other keywords.
*/
void
w_key(char *string)
{
	if (Lastc == KEYCHAR)
		w_raw(" ");
	w_raw(string);
	Lastc = KEYCHAR;
}

/*
**  W_CON -- write out a constant
**	Writes out a constant of type 'type'
**	pointed to by 'string'.
*/


void
w_con(int type, char *string)
{
	if (type == Tokens.sp_sconst)
		w_string(string, 1);
	else
		w_key(string);
}

/*
**  W_OP -- Writes out a string which doesn't need a blank
**	    to separate it from a keyword.
*/

void
w_op(char *string)
{
	w_raw(string);
	Lastc = OPCHAR;
}

/*
**  BEGIN_QUOTE -- Issue an IIwrite("
*/
void
begin_quote(void)
{
	In_string = 1;
	In_quote = 1;
	Fillmode = 1;
	w_op("IIwrite(\"");
}

/*
**  END_QUOTE -- End any pending IIwrite("
*/
void
end_quote(void)
{
	In_string = 0;
	if (In_quote)
		w_op("\");");
	In_quote = 0;
}

/*
**  W_NEW -- write out a string after getting out of
**	     any pending IIwrite's.
*/
void
w_new(char *string)
{
	end_quote();
	w_op(string);
}

/*
**  W_VAR -- writes out code to send the
**	     value of a C variable down to
**	     the Quel scanner.
**
**	Conserves the state of In_quote.
*/
void
w_var(struct display *disp, int type)
{
	register			savestat;

	savestat = In_quote;

	/* if was In_quote, then will want a space before the
	 * string written down
	 */
	if (savestat) {
		w_key("");
	}
	if (type != opIDSTRING) {
		w_new("IIcvar(");
		if (type != opSTRING)
			w_op("&");
	} else {
		w_new("IIwrite(");
	}
	w_display(disp);
	switch (type) {
	  case opSHORT:
		w_op(",1,2);");
		break;

	  case opFLOAT :
		w_op(",2,4);");
		break;

	  case opSTRING :
		w_op(",3,0);");
		break;

	  case opDOUBLE :
		w_op(",4,8);");
		break;

	  case opCHAR :
		w_op(",5,1);");
		break;

	  case opLONG :		/* also ints, since this is a VAX */
		w_op(",6,4);");
		break;

	  case opIDSTRING :
		w_op(");");
		break;

	  default :
		syserr("invalid type %d in w_var", type);
	}
	if (savestat) {
		begin_quote();
		/* if was In_quote, then will want a space 
		 * before next keyword
		 */
		w_key("");
	}
}

/*
**  EQUATE_LINES -- Make subsequent lines be output on the
**		    same line they were read (lines of C_CODE only).
**
**	Note: Because of the algorithm used, it is possible that
**		the correct line in the output has already been passed,
**		in which case equate_lines does nothing.
*/

void
equate_lines(void)
{
	Fillmode = 0;
	while (Lineout < yyline)
		w_raw("\n");
	Lastc = OPCHAR;
}

/*
**  W_FILE -- Writes out the name and line number of the
**	      input file if Rtdb is specified, else a 0.
*/
void
w_file(void)
{
	char		itemp [6];

	if (Rtdb) {
		w_string(Input_file_name, 0);
		itoa(yyline, itemp);
		w_op(",");
		w_key(itemp);
	}
	else
		w_key("0");
}

/*
**  W_SYNC -- Put out an IIsync() call
*/

void
w_sync(void)
{
	w_new("IIsync(");
	w_file();
	w_op(");");
}

/*
**  W_FLUSH -- Put out an IIflush_tup() call
*/
void
w_flush(void)
{
	w_new("IIflushtup(");
	w_file();
	w_op(");");
}

/*
**  W_STRING -- Writes out a string
**
**	String is output as a string constant if type == 0
**	otherwise writes out string inside an IIwrite(
*/

void
w_string(char *string, int type)
{
	register char	*t;
	register char	*s;

	if (type) {
		if (!In_quote)
			begin_quote();
		w_raw("\\\"");
	} else
		w_raw("\"");

	s = t = string;
	In_string += 1;
	for ( ;*t ; ) {
		if (*t == '\\') {
		  
			if (t [1] == '\n') {
				*t = '\0';
				w_raw(s);
				s = t = &t [2];
				w_raw("\\\n");
			} else {
				*t++ = '\0';
				w_raw(s);
				s = t;
				/* note that this call must be atomic,
				 * as w_raw would feel free to put newlines
				 * in if not.
				 */
				if (type)
					w_raw("\\\\");
				else
					w_raw("\\");
			}
		} else if (*t == '"') {
			w_raw("\\\"");
			s = ++t;
		} else
			t++;
	}
	w_raw(s);
	In_string -= 1;
	if (type)
		w_raw("\\\"");
	else
		w_raw("\"");
}

#include <stdio.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)ftoa.c	8.2	8/31/93)

/*
**  FLOATING POINT TO ASCII CONVERSION
**
**	'Value' is converted to an ascii character string and stored
**	into 'ascii'.  Ascii should have room for at least 'width' + 1
**	characters.  'Width' is the width of the output field (max).
**	'Prec' is the number of characters to put after the decimal
**	point.  The format of the output string is controlled by
**	'format'.
**
**	'Format' can be:
**		e or E: "E" format output
**		f or F:  "F" format output
**		g or G:  "F" format output if it will fit, otherwise
**			use "E" format.
**		n or N:  same as G, but decimal points will not always
**			be aligned.
**
**	If 'format' is upper case, the "E" comes out in upper case;
**	otherwise it comes out in lower case.
**
**	When the field width is not big enough, it fills the field with
**	stars ("*****") and returns zero.  Normal return is the width
**	of the output field (sometimes shorter than 'width').
*/
int
ftoa(double value, char *ascii, int width, int prec, char ch)
{
	char	fmt[64];

	(void) sprintf(fmt, "%%%d.%d%c", width, prec, 'f');
	(void) sprintf(ascii, fmt, value);
	return(width);
}

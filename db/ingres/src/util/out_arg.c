#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)out_arg.c	8.1	12/31/84)

/*
**  OUTPUT ARGUMENTS GLOBAL INITIALIZATION
*/

/* output arguments */
struct out_arg	Out_arg = {
	6,		/* c0width */
	6,		/* i1width */
	6,		/* i2width */
	13,		/* i4width */
	10,		/* f4width */
	10,		/* f8width */
	3,		/* f4prec */
	3,		/* f8prec */
	'n',		/* f4style */
	'n',		/* f8style */
	66,		/* linesperpage */
	'|',		/* coldelim */
};

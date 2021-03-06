/* main.c - main() and some logging routines for expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include "expect_cf.h"
#include <stdio.h>
#include "tcl.h"
#include "expect_tcl.h"

void
main(argc, argv)
int argc;
char *argv[];
{
	Tcl_Interp *interp = Tcl_CreateInterp();

	if (Tcl_Init(interp) == TCL_ERROR) {
		fprintf(stderr,"Tcl_Init failed: %s\n",interp->result);
		exit(1);
	}

	if (Exp_Init(interp) == TCL_ERROR) {
		fprintf(stderr,"Exp_Init failed: %s\n",interp->result);
		exit(1);
	}

	exp_parse_argv(interp,argc,argv);

	/* become interactive if requested or "nothing to do" */
	if (exp_interactive) (void) exp_interpreter(interp);
	else if (exp_cmdfile) exp_interpret_cmdfile(interp,exp_cmdfile);
	else if (exp_cmdfilename) exp_interpret_cmdfilename(interp,exp_cmdfilename);

	/* assert(exp_cmdlinecmds != 0) */

	exp_exit(interp,0);
}


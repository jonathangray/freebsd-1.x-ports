#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <symbol.h>
#include "parser.h"
#include <access.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)par_init.c	8.2	1/15/85)

/*
**  PAR_INIT -- initialization call for parser
**
**	Trace Flags:
**		par_init ~~ 60.0
*/
void
par_init(int argc, char **argv1)
{
	register int	rt;
	register char	**argv;

	extern int		Noupdt;
	extern int		Dcase;
	extern char		*Relspec;
	extern char		*Indexspec;
	extern admin_t	Admin;
	extern int		Qrymod;
	extern int		yydebug;

	/* set up parser */
	argv = argv1;



#ifdef	xPTR1
	if (tTf(60, 0))
		yydebug = 1;
#endif

#ifdef	xPTR2
	if (tTf(60, 1)) {
		printf("Par_init:	");
		prargs(argc, argv);
	}
#endif

	Noupdt = !setflag(argv, 'U', 0);
	Dcase = setflag(argv, 'L', 1);

	/* if param specified, set result reln storage structures */
	Relspec = "cheapsort";		/* default to cheapsort on ret into */
	Indexspec = "isam";		/* isam on index */

	for (rt = FREEFLAGS; rt < argc; rt++) {
		if (argv[rt][0] == '-') {
			if (argv[rt][1] == 'r') {
				Relspec = &argv[rt][2];
			}
			if (argv[rt][1] == 'n') {
				Indexspec = &argv[rt][2];
				continue;
			}
		}
	}
	if (strcmp(Relspec, "heap") == 0) {
		Relspec = 0;
	}
	if (strcmp(Indexspec, "heap") == 0) {
		Indexspec = 0;
	}

	rnginit();
	opencatalog("attribute", OR_READ);

	Qrymod = ((Admin.ad_h.adm_flags & A_QRYMOD) == A_QRYMOD);

#ifdef	xPTR2
	if (tTf(60, 2)) {
		printf("Par_init: Results:\n");
		printf("\tQrymod: %d\n", Qrymod);
		printf("\tIndexspec: %s\n", Indexspec);
		printf("\tRelspec: %s\n", Relspec);
		printf("\tDcase: %d\n", Dcase);
		printf("\tNoupdt: %d\n", Noupdt); 
	}
#endif
}

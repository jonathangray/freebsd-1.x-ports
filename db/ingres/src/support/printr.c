#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <lock.h>
#include <pv.h>
#include <ctlmod.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_DBU
#include "protos.h"

SCCSID(@(#)printr.c	8.2	2/8/85)

struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */
char		*Fileset;	/* a unique string to make filenames from */
char		*Database;	/* the name of the current database */
char		*Usercode;	/* the code of the current user */
char		*Pathname;	/* the pathname of the root of INGRES */
int		Equel;		/* set if running an Equel program */
int		RubLevel;	/* rubout level, -1 if ignored */
jmp_buf		CmReset;	/* restart addr on interrupt */
extern struct out_arg	Out_arg;	/* output arguments */
#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif xMONITOR


short		tTdbu[100];
desc_t		Btreesec;

void
main(int argc, char **argv)
{
	register char		*q;
	register char		*p;
	int			i;
	int			j;
	int			badf;
	char			style;
	int			mode;
	int			nc;
	paramv_t			pv[PV_MAXPC];
	paramv_t			*pp;
	extern char		*Dbpath;

	argv[argc] = NULL;

#ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif

	mode = -1;
	badf = 0;

	/*
	**  Scan the argument vector and otherwise initialize.
	*/

	i = initucode(argc, argv, TRUE, NULL, M_SHARE);
	switch (i) {
	  case 0:
	  case INDIRECT:
		break;

	  case NODB:
	  case INDNODB:
		printf("Database %s does not exist\n", getparmvect(0));
		exit(-1);

	  case NOACCESS:
		printf("You are not authorized to access this database\n");
		exit(-1);

	  case INVALIDUSR:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case NODBNAME:
		printf("No database name specified\n");
		badf++;
		break;

	  default:
		syserr("main: initucode %d", i);
	}

	for (j = 0 ; (p = getflagvect(j)) != (char *) NULL ; j++) {
		if (p[0] != '-')
			goto badflag;
		switch (p[1]) {
		  case 'h':		/* do headers on each page */
			mode = 0;
			if (p[2] == 0)
				break;
			Out_arg.linesperpage = atoi(&p[2]);
			break;

		  case 's':		/* supress headers and footers */
			mode = 1;
			if (p[2] != 0)
				goto badflag;
			break;

		  case 'c':		/* set cNwidth */
			Out_arg.c0width = atoi(&p[2]);
			break;
			{
			badflag:
				printf("bad flag %s\n", p);
				badf++;
				continue;
			}
			break;

		  case 'i':		/* set iNwidth */
			switch (p[2]) {

			  case '1':
				Out_arg.i1width = atoi(&p[3]);
				break;

			  case '2':
				Out_arg.i2width = atoi(&p[3]);
				break;

			  case '4':
				Out_arg.i4width = atoi(&p[3]);
				break;

			  default:
				goto badflag;

			}
			break;

		  case 'f':		/* set fNwidth */
			style = p[3];
			switch (style) {

			  case 'e':
			  case 'E':
			  case 'f':
			  case 'F':
			  case 'g':
			  case 'G':
			  case 'n':
			  case 'N':
				break;

			  default:
				goto badflag;

			}
			for (q = &p[4]; *q != '.'; q++)
				if (*q == 0)
					goto badflag;
			*q++ = 0;
			switch (p[2]) {

			  case '4':
				Out_arg.f4width = atoi(&p[4]);
				Out_arg.f4prec = atoi(q);
				Out_arg.f4style = style;
				break;

			  case '8':
				Out_arg.f8width = atoi(&p[4]);
				Out_arg.f8prec = atoi(q);
				Out_arg.f8style = style;
				break;

			  default:
				goto badflag;

			}
			break;

		  case 'v':
			if (p[2] == 0 || p[3] != 0)
				goto badflag;
			Out_arg.coldelim = p[2];
			break;

		  default:
			goto badflag;
		}
	}

	/*
	**  Build parameter vector for print call
	*/

	for (nc = 1, pp = pv; getparmvect(nc) != NULL; nc++)
		((pp++)->pv_val).pv_str = getparmvect(nc);
	if (mode != -1)
		((pp++)->pv_val).pv_int = mode;
	pp->pv_type = PV_EOF;

	/*
	**  Check for usage errors.
	*/

	if (nc < 2) {
		badf++;
		printf("usage:  printr [flags] database relation ...\n");
	}
	if (badf) {
		fflush(stdout);
		exit(-1);
	}

	p = getparmvect(0);	/* data base is first parameter */
	if (chdir(Dbpath) < 0) {
		syserr("cannot access data base %s", p);
	}
#ifdef xTTR2
	if (tTf(1, 0)) {
		printf("entered database %s\n", Dbpath);
	}
#endif

	/* initialize access methods (and Admin struct) for user_ovrd test */
	acc_init(0, 0);

	set_so_buf();
#ifdef xTTR1
	if (tTf(1, 1)) {
		printf("printing %s\n", p);
	}
#endif

	print(nc - 1, pv);
	fflush(stdout);
	exit(0);
}

void
rubproc(void)
{
	fflush(stdout);
	exit(0);
}

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
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

SCCSID(@(#)helpr.c	8.2	2/8/85)

struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */
jmp_buf		CmReset;	/* restart addr on interrupt */

#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */



short		tTdbu[100];
desc_t		Btreesec;
char		*Fileset;

char *
qmtest(register char *p)
{
	if (strcmp("view", p) == 0)
		return ("4");
	else if (strcmp("permit", p) == 0)
		return ("5");
	else if (strcmp("integrity", p) == 0)
		return ("6");
	else
		return (NULL);
}

void
main(int argc, char **argv)
{
	register int		i;
	int			j;
	register char		*p;
	extern char		*Dbpath;
	int			nc;
	paramv_t			newpv[PV_MAXPC];
	paramv_t			*nv;
	char			*qm;

	p = (char *) NULL;
	argv[argc] = NULL;

#ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif

	i = initucode(argc, argv, TRUE, NULL, M_SHARE);
#ifdef xSTR2
	if (tTf(0, 1))
		printf("initucode=%d, Dbpath='%s'\n", i, Dbpath);
#endif
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
		printf("usage: helpr database [relname ...]\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}

#if 0
	if (getflagvect(0) != (char *) NULL) {
		printf("No flags are allowed for this command\n");
		goto usage;
	}
#endif

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
#ifdef xTTR3
	if (tTf(2, 0)) {
		printf("access methods initialized\n");
	}
#endif

	set_so_buf();

	p = getparmvect(j = 1);
	if (p == NULL) {
		/* special case of no relations specified */
		newpv[0].pv_type = PV_INT;
		newpv[0].pv_val.pv_int = HELP_RELLIST;
		newpv[1].pv_type = PV_EOF;
#ifdef xTTR3
		if (tTf(3, 0))
			printf("calling help, no relations specified\n");
#endif
		help(1, newpv);

	} else {
		do {
			nc = 0;
			nv = newpv;

			if ((qm = qmtest(p)) != NULL) {
				/* either help view, integrity or protect */
				for (j++ ; (p = getparmvect(j)) != NULL ; j++) {
					if ((i = (int) qmtest(p)) != 0) {
						/* change of qmtest result */
						qm = (char *) i;
						continue;
					}
					(nv)->pv_type = PV_STR;
					(nv++)->pv_val.pv_str = qm;
					(nv)->pv_type = PV_STR;
					(nv++)->pv_val.pv_str = p;
					nc += 2;
				}
#ifdef xTTR3
				if (tTf(3, 0))
					printf("calling display\n");
#endif
				nv->pv_type = PV_EOF;
				/*
				display(nc, newpv);
				*/
			} else {
				/* help relname */
				while ((p = getparmvect(j++)) != NULL &&
				       qmtest(p) == NULL) {
					if (strcmp("all", p) == 0) {
						(nv)->pv_type = PV_INT;
						(nv++)->pv_val.pv_int = HELP_ALLRELINFO;
						nc++;
					} else {
						(nv)->pv_type = PV_INT;
						(nv++)->pv_val.pv_int = HELP_RELINFO;
						(nv)->pv_type = PV_STR;
						(nv++)->pv_val.pv_str = p;
						nc += 2;
					}
				}
				nv->pv_type = PV_EOF;
#ifdef xTTR3
				if (tTf(3, 0))
					printf("calling help\n");
#endif
				help(nc, newpv);
				/* this backs j up one step, so 
				 * that it points at the keywords (permit,
				 * integrity, view) or the NULL
				 */
				--j;
			}
		} while (p != NULL);
	}
	fflush(stdout);
	exit(0);
}

void
rubproc(void)
{
	exit(1);
}

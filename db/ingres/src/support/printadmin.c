#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)printadmin.c	8.2	2/8/85)

short		tTdbu[100];
admin_t	Admin;

void
main(int argc, char **argv)
{
	register int	i;
	register char	*db;
	extern char	*Dbpath;

	argv[argc] = NULL;
#ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif

	i = initucode(argc, argv, TRUE, NULL, -1);
	db = getparmvect(0);
	switch (i) {
	  case 0:
	  case INDIRECT:
		break;

	  case NODB:
	  case INDNODB:
		printf("Database %s does not exist\n", db);
		exit(-1);

	  case NOACCESS:
		printf("You are not authorized to access this database\n");
		exit(-1);

	  case INVALIDUSR:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case NODBNAME:
		printf("No database name specified\n");
	usage:
		printf("usage: printadmin database\n");
		exit(-1);
	  default:
		syserr("initucode %d", i);
	}

	if (getflagvect(0) != NULL) {
		printf("No flags are allowed for this command\n");
		goto usage;
	}

	if (getparmvect(1) != (char *) NULL)
		goto usage;

	if (chdir(Dbpath) < 0)
		syserr("cannot access database %s", db);
#ifdef xTTR2
	if (tTf(1, 0))
		printf("entered database %s\n", Dbpath);
#endif

	/* Admin struct has been filled in by initucode */
	printf("Database %s, Dba %.2s, Adflags %o\n",
		db, Admin.ad_h.adm_owner, Admin.ad_h.adm_flags);
	printf("Code %d, adlen %d, adm_rellen %d, adm_attrlen %d\n",
	       Admin.ad_h.adm_version, Admin.ad_h.adm_len,
	       Admin.ad_h.adm_rellen, Admin.ad_h.adm_attrlen);

	printf("\n\n");
	printdesc(&Admin.ad_rel);

	printf("\n\n");
	printdesc(&Admin.ad_attr);
}

void
rubproc(void)
{
	exit(1);
}

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

/* unistd.h defines _POSIX_VERSION on POSIX.1 systems.  */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_DBU
#include "protos.h"

SCCSID(@(#)subs.c	8.3	1/31/86)

/*
** These are subroutines common to RESTORE and PURGE.
*/

char		All;
char		Qrymod;
char		Superuser;
char		Ask;
char		Purge;
char		Clean;
char		Lastflag;
DIR		*Direc;
extern char	*Usercode;
static int	Dblistc;


/*
**  CHECK FOR SUPERUSER
**
**	The user has requested the -s flag.  Can he do it?  Will Martha
**	recover from cancer?  Will Dick get the girl?  Stay tuned for
**	"sucheck".
**
**	Permission is based on the U_SUPER bit in the status field
**	in the users file.
*/
int
sucheck(void)
{
	return (getglobalint(STATUS_NAME) & U_SUPER);
}

/*
**  INITIALIZE GLOBALS
**
**	Set up Usercode and Status
*/
int
initialize(int argc, char **argv)
{
	register int	i;
	int		j;
	register char	*p;
	char		datadir[MAX_LINE_SIZE];

#ifdef	xTTR2
	tTfp(40, 0, "entered initialize\n");
#endif
	i = initucode(argc, argv, FALSE, NULL, -1);
#ifdef	xTTR2
	tTfp(40, 1, "initucode ret:%d\n", i);
#endif
	switch (i) {
	  case 0:
		break;

	  case INVALIDUSR:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}
	initdbpath(NULL, datadir, FALSE);

	/* scan flags */
#ifdef	xTTR2
	tTfp(40, 2, "scanning flags\n");
#endif
	for (j = 0 ; (p = getflagvect(j)) != (char *) NULL ; j++) {
		if (p[0] != '-') {
		badflag:
			printf("Bad flag: %s\n", p);
			return (-1);
		}
		switch (p[1]) {
		  case 'a':
			Ask++;
			break;

		  case 'p':
			Purge++;
			break;

		  case 's':
			if (sucheck()) {
				Superuser++;
			} else {
				printf("You may not use the -s flag\n");
				exit(-1);
			}
			break;

		  case 'f':
			Clean++;
			break;

		  case 'T':
			break;

		  default:
			goto badflag;
		}
	}
	Dblistc = 0;
	if (getparmvect(Dblistc) == (char *) NULL) {
#ifdef	xTTR2
		tTfp(40, 3, "doing all\n");
#endif
		All++;
		if ((Direc = opendir(datadir)) == (DIR *) NULL) {
			syserr("cannot read .../data/base");
		}
	}
#ifdef	xTTR2
	tTfp(40, 0, "leaving initialize\n");
#endif
	return(0);
}


/*
**  GET NEXT DATABASE
**
**	The next database to be purged is selected.  It comes from
**	either the directory or the database list.
**
**	Getnxtdb() leaves the user in the database directory.
*/
char *
getnxtdb(void)
{
	struct dirent		*dp;
	register char		*db;
	register FILE		*fd;
	register int		i;
	extern admin_t	Admin;
	static char		dbpbuf[MAX_LINE_SIZE];

#ifdef	xTTR2
	tTfp(41, 0, "entered getnxtdb\n");
#endif
	for (;;) {
		if (All) {
			dp = readdir(Direc);
			if (dp == NULL) {
				db = NULL;
			} else {
				if (strcmp(dp->d_name, ".") == 0 ||
				    strcmp(dp->d_name, "..") == 0) {
					continue;
				}
				db = dp->d_name;
			}
		} else {
			db = getparmvect(Dblistc++);
		}
		if (db == NULL)
			return (NULL);
#ifdef	xTTR2
		tTfp(41, 1, "using %s as Database\n", db);
#endif
		i = initdbpath(db, dbpbuf, TRUE);
#ifdef	xTTR2
		tTfp(41, 3, "initdbpath ret: %d, %s\n", i, dbpbuf);
#endif
		switch (i) {
		  case 0:
		  case 1:
			break;

		  case 2:
		  case 3:
			printf("Database %s does not exist\n", db);
			continue;

		  default:
			syserr("initdbpath %d", i);
		}
		if (chdir(dbpbuf) < 0) {
			printf("Cannot enter %s", dbpbuf);
			continue;
		}
#ifdef	xTTR2
		tTfp(41, 4, "chdir ok, Superuser: %d\n", Superuser);
#endif
		fd = fopen("admin", "r");
		if (fd == NULL) {
			printf("Cannot open %s/admin\n", dbpbuf);
			continue;
		}
		fread(&Admin.ad_h, sizeof(Admin.ad_h), 1, fd);
		fclose(fd);
#ifdef	xTTR2
		tTfp(41, 5, "user: %.2s\n", Admin.ad_h.adm_owner);
#endif

		/* set qrymod flag from database status */
		Qrymod = ((Admin.ad_h.adm_flags & A_QRYMOD) == A_QRYMOD);

		/* check for dba of database if not superuser */ 
		if (Superuser || bequal(Admin.ad_h.adm_owner, Usercode, 2))
			break;

		/*
		** not dba isn't an error if running in all mode since user
		** couln't have specified the database
		*/
		if (All)
			continue;
		printf("You are not the dba for %s\n", db);
	}
#ifdef	xTTR2
	tTfp(41, 6, "leaving getnxtdb, %s ok\n", db);
#endif
	return (db);
}

/*
** ASK
**	If Ask is set desplay prompt and look for 'y' and return TRUE
**	If Ask is not set return TRUE
*/
int
ask(char *prompt)
{
	register char	*p;
	char		line[MAX_LINE_SIZE];
	extern char	Ask;

	if (!Ask)
		return (TRUE);
	p = prompt;

	while (*p) {
		putchar(*p);
		p++;
	}

	if (fgets(line, MAX_LINE_SIZE, stdin) == NULL)
		return(FALSE);

	return (line[0] == 'y');
}

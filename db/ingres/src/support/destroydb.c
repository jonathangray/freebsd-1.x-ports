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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <ctlmod.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)destroydb.c	8.3	1/31/86)

/*
**  DESTROY DATA BASE
**
**	This program destroys an existing database.  To be able
**	to wield this awesome power, you must be the dba for
**	the database.  Also, anyone has this power if the admin
**	the database, or the ingres superuser, and have the "-s"
**	flag requested.  If admin is trashed, the INGRES superuser
**	must either destroy the database or recover it.
**
**	If -m is specified, then the directory is not removed.
**	This is useful if the directory is a mounted file system.
*/

struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */

jmp_buf		CmReset;	/* restart addr on interrupt */

#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */


extern char	*Usercode;
extern char	*Pathname;
extern char	*Dbpath;
admin_t	Admin;
short		tTdbu[100];

char	*Fileset;

int
clean(register DIR *dirp)
{
	struct	dirent	*dp;

#ifdef xSTR1
	if (tTf(2, 0))
		printf("clean: ");
#endif

	while ((dp = readdir(dirp)) != (struct dirent *) NULL) {
		if ( !strcmp(".",dp->d_name) || !strcmp("..",dp->d_name) )
			continue;
#ifdef xSTR1
		if (tTf(2, 1))
			printf("unlinking %s\n", dp->d_name);
#endif
		unlink(dp->d_name);
	}
	return(0);
}/* clean */
		

void
main(int argc, char **argv)
{
	register int	i;
	int		j;
	register char	*dbase;
	int		superuser, mounted;
	register char	*p;
	char		*q;
	DIR		*dirp;

	argv[argc] = NULL;
#ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif
	
	i = initucode(argc, argv, TRUE, NULL, -1);
	dbase = getparmvect(0);
#ifdef xSTR1
	if (tTf(1, 0)) {
		printf("after initcode %d: ", i);
		prargs(argc, argv);
	}
#endif
	switch (i) {
	  case 0:
	  case 5:
		break;

	  case 1:
	  case 6:
		printf("Database %s does not exist\n", dbase);
		exit(-1);

	  case 2:
		printf("You are not authorized to access database %s\n", dbase);
		exit(-1);

	  case 3:
		printf("You are not an authorized INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
	usage:
		printf("Usage: destroydb [-s] [-m] dbname\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}

	mounted = superuser = 0;
	for (j = 0 ; (p = getflagvect(j)) != (char *) NULL ; j++) {
#ifdef xSTR1
		if (tTf(1, 1))
			printf("p = *av (\"%s\")\n", p);
#endif
		if (p[0] != '-') {
		badflag:
			printf("Bad flag %s\n", p);
			goto usage;
		}
		switch (p[1]) {

		  case 's':
			superuser++;
			break;

		  case 'm':
			mounted++;
			break;

		  default:
			goto badflag;
		}
	}

	if (getparmvect(1) != NULL) {
		printf("Too many parameters to destroydb\n");
		goto usage;
	}
	if (strlen(dbase) > MAXFILENAMESIZ)
		syserr(0, "invalid dbname %s", dbase);
	if (superuser && (getglobalint(STATUS_NAME) & U_SUPER) == 0)
		syserr(0, "you may not use the -s flag");

	if (!superuser) {
		if (!bequal(Admin.ad_h.adm_owner, Usercode, USERCODE_SIZE)) {
			printf("You are not the DBA for %s\n", dbase);
			exit(-1);
		}
	}

	if (chdir(Dbpath) < 0)
		syserr("chdir %s", Dbpath);

	if ( (dirp = opendir(".")) == NULL )
		syserr("Can't open . in %s",Dbpath);
	clean(dirp);
	closedir(dirp);

	if (!mounted) {
		/* find end of Dbpath and trim it off. */
		for (p = q = Dbpath; *p != '\0'; p++) {
			if (*p == '/') {
				q = p;
			}
		}
		*q++ = '\0';
		if (chdir(Dbpath) < 0)
			syserr("chdir(%s)", Dbpath);
		if ( i == 5 )
			if ( unlink(ztack(ztack(Pathname,"/data/base/"),dbase)) == -1 )
				syserr("Can't unlink the indirect file %s",dbase);
		execl("/bin/rmdir", "/bin/rmdir", q, 0);
		ingres_perror("/bin/rmdir");
	}
}


/*
**  Rubout processing.
*/

void
rubproc(void)
{
	exit(-2);
}

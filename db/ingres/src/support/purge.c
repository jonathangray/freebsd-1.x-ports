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
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#define DIRENT	struct dirent
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <lock.h>
#include <pv.h>
#include "sccs.h"
#include <ctlmod.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_DBU
#include "protos.h"

SCCSID(@(#)purge.c	8.3	1/31/86)

/*
**  PURGE DATABASE
**
**	This stand-alone routine cleans up a database.  This includes:
**
**	- Destroy temporary relations, i.e., relations with names
**		beginning with "_SYS".
**	- Destroy expired relations
**	- Clean out junk files, such as core, etc.
**	- As a suggested future expansion, reformat relations which
**		have many overflow pages, or heaps with lots of old
**		deleted tuples, etc.
**
**	It may be called by the ingres superuser or by the dba of
**	a database.  There are two modes.  The first is where databases
**	to be purged are explicitly named.  If none are named, then
**	all databases owned by the particular user (or all databases
**	if the INGRES superuser) are purged.
**
**	Flags:
**	-p	enable the purge feature, i.e., clean out expired
**		relations as well as temporary relations.
**	-s	attempt to run in superuser mode.  The user must be
**		login "ingres" for this to succeed.
**	-a	ask the user before each database.
**	-f	clean out rather than report junk files.
**
**	(8/2/82 peter@lbl-unix)
**		allow dba to destroy user relations by suitable manipulation
**		of Usercode
**		added ExitFn = nullfn() to avoid untimely aborts on syserrs.
**	(5/13/83 peter@lbl-csam)
**		folded in new directory rootines for 4.1c-bsd
*/

struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */
jmp_buf		CmReset;	/* restart addr on interrupt */

#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */

char		All;
char		Superuser;
char		Ask;
char		Purge;
char		Clean;
long		Today;
short		tTdbu[100];
extern int	Wait_action;
extern char	*Usercode;
extern void	(*ExitFn)();

/*
**  PURGE DATABASE
**
**	The database is purged of temporaries, expired relations, and
**	junk.
*/

extern desc_t	Reldes;
desc_t		Btreesec;
char		*Fileset;

void
purgedb(register char *db)
{
	relation_t		rel, key;
	tid_t			rtid, rlimtid;
	register int		i;
	DIR			*dirp;
	struct dirent		*dp;
	paramv_t		pv[2];
	char			pbuff[MAX_NAME_SIZE + 1];
	char			*fname;

#ifdef	xTTR2
	tTfp(11, 0, "entered purgedb(%s)\n", db);
#endif
	printf("Database %s", db);
	if (!ask("? ")) {
		return;
	}
	if (!Ask) {
		printf(":\n");
	}
	acc_init(0, 0);

	/* set exclusive lock on data base */
#ifdef	xTTR2
	tTfp(11, 1, "calling db_lock(%d)\n", M_EXCL);
#endif
	db_lock(M_EXCL);

	/* open the relation relation for read-write */
	opencatalog("relation", OR_WRITE);

	if (find(&Reldes, NOKEY, &rtid, &rlimtid, (void *) NULL)) {
		printf("\tcannot find in %s\n", db);
		closecatalog(TRUE);	/* really close cache */
		unldb();		/* unlock the database */
		acc_close();
		return;
	}

	while (get(&Reldes, &rtid, &rlimtid, &rel, 1) == 0) {
		i = 0;

		/* check for temp rel */
		if (bequal(rel.r_id, "_SYS", 4)) {
			printf("\t%.14s: temporary", rel.r_id);
			i++;
		}
		else if (rel.r_savetime < Today && rel.r_savetime != 0) {
			printf("\t%.14s: expired", rel.r_id);
			if (Purge)
				if (ask("\n\t\tPURGE? "))
					i++;
		}
		else
			i = -1;

		/* if this relation should be purged -- call (local) destroy */
		if (i > 0) {
			char	*usave;
			printf("\tpurging\n");

			/* allow DBA to purge other users relations */
			usave = 0;
			if (!bequal(rel.r_owner, Usercode, 2)) {
				usave = Usercode;
				Usercode = rel.r_owner;
			}

			/* set up parameter vector for destroy */
			bmove(rel.r_id, pbuff, MAX_NAME_SIZE);
			pbuff[MAX_NAME_SIZE] = '\0';
			pv[0].pv_type = PV_STR;
			pv[0].pv_val.pv_str = pbuff;
			pv[1].pv_type = PV_EOF;
			pv[1].pv_val.pv_str = NULL;
			if (destroy(1, pv) != 0) {
				syserr("cannot destroy %s\n", pbuff);
			}
			if (usave) {
				Usercode = usave;
			}
			closecatalog(FALSE);	/* to flush */
		} else if (i == 0) {
			printf("\t\t(not purged)\n");
		}
	}

	/* open the directory to check for extra files */
	if ((dirp = opendir(".")) == NULL) {
		printf("\tcannot open .\n");
		closecatalog(TRUE);		/* really */
		unldb();		/* unlock the database */
		acc_close();
		return;
	}

	/* scan the directory */
	while ((dp = readdir(dirp)) != (struct dirent *) NULL) {
		fname = dp->d_name;
		if (dp->d_namlen <= 2)
			continue;
		/* throw out legitimate files */
		if (strcmp(fname, "admin") == 0) {
			continue;
		}

		/* always purge _SYS files */
		if (!bequal(fname, "_SYS", 4)) {
			if (fname[13] != 0) {
				/* it might be a relation */
				clearkeys(&Reldes);
				ingres_setkey(&Reldes, &key, fname, RELID);
				ingres_setkey(&Reldes, &key, &fname[MAX_NAME_SIZE], RELOWNER);
				if (getequal(&Reldes, &key, &rel, &rtid) <= 0) {
					/* it is a relation (or should be saved) */
					continue;
				}
			}

			/* it is a funny file!!! */
			if (!Clean) {
				printf("\t%s: file (not unlinked)\n", fname);
				continue;
			}
		}

		/* purge the file */
		printf("\tunlinking %s\n", fname);
		if (unlink(fname))
			printf("\tcannot unlink\n");
	}
	closecatalog(TRUE);	/* close catalogs */
	unldb();		/* unlock the database */
	acc_close();
	closedir(dirp);
}

void
main(int argc, char **argv)
{
	register char	*db;

	argv[argc] = NULL;
#ifdef xTTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif

	/* set up arguments and operating modes */
	initialize(argc, argv);
	time(&Today);
#ifdef	xTTR2
	tTfp(10, 2, "Usercode: %.2s\n", Usercode);
#endif

	/* Assign ExitFn so syserr (e.g. from destroy) does not cause aborts */
	ExitFn = null_fn;

	while ((db = getnxtdb()) != 0) {
		purgedb(db);
	}
	printf("\npurge completed\n");
}

void
rubproc(void)
{
	unldb();
	exit(-1);
}


#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include <sys/param.h>

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
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <batch.h>
#include <lock.h>
#include <symbol.h>
#include <resp.h>
#include "sccs.h"
#include <ctlmod.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_DBU
#include "protos.h"

SCCSID(@(#)restore.c	8.5	1/31/86)

/*
** INGRES crash recovery processor
**	to recover a database you must be the dba or the ingres superuser
**	RESTORE attempts to complete updates from batch files left in a
**	database.  After finishing all the batch files it calls PURGE.
*/


struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */
jmp_buf		CmReset;	/* restart addr on interrupt */

#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */

#ifndef PURGE
#define	PURGE		"purge"
#endif /* PURGE */

/* first file to close on error */
#define	CLOSEFILES 	3

extern char	*Usercode;
char		Utemp[USERCODE_SIZE];
char		*Fileset;
char		Berror;		/* batch error */
char		Error;
extern char	Ask;
extern char	Superuser;
extern char	All;
extern char	Qrymod;
int		Direc		= CLOSEFILES - 1;
extern int	Wait_action;
short		tTvect[100];
short		tTdbu[100];
desc_t		Btreesec;
jmp_buf		Jmpbuffer;	/* buffer for nonlocal goto's on an error condition */

/*
** looks up user by usercode in users file
*/
char *
lookucode(char *ucode)
{
	static char	buf[MAX_LINE_SIZE + 1];
	register char	*p;

	if (getuser(ucode, buf))
		syserr("cannot identify user %.2s", ucode);
	for (p = buf; *p != ':'; p++) {
	}
	*p = 0;
	return (buf);
}

/*
** handles syserr's in the update processor
*/
void
uperr(void)
{

	if (Batch_fp)
		close(Batch_fp);
	Berror++;
	longjmp(Jmpbuffer,1);
}

/*
** RESTORE -- find the batch files and process them
*/
void
restore(void)
{
	desc_t			descr;
	register DIR		*dirp;
	register struct dirent	*dp;
	register int		i;
	extern char		*Fileset;
	extern void		(*ExitFn)();
	void			(*tmpfn)();
	char			*fname;

	if ( (dirp = opendir(".")) == NULL )
		syserr("Can't open data base directory");
	bmove(Usercode, Utemp, USERCODE_SIZE);
	Batch_recovery = 1;
	tmpfn = ExitFn;
	ExitFn = uperr;

	/* restart point */
	setjmp(Jmpbuffer);
	while ((dp = readdir(dirp)) != (struct dirent *) NULL) {
		fname = dp->d_name;
		if ( strcmp(".",fname) == 0 || strcmp("..",fname) == 0 )
			continue;
		if (bequal("_SYSbatch", fname, 9)) {
			Fileset = &fname[9];
			Batch_fp = open(batchname(), O_RDONLY);
			Batch_cnt = BATCHSIZE;
			getbatch(&Batchhd, sizeof(Batchhd));
			printf("\tFound batch file:  %s\n", fname);
			printf("\tRelation: %s\tUser: %s\n", Batchhd.b_relname,
				lookucode(Batchhd.b_user));
			close(Batch_fp);
			bmove(Batchhd.b_user, Usercode, USERCODE_SIZE);
			if(ask("\tUpdate? "))
				update(0, (paramv_t *) NULL);
		}
		if (bequal(MODBATCH, fname, sizeof(MODBATCH) - 1)) {
			Fileset = &fname[sizeof(MODBATCH) - 1];
			if ((Batch_fp = open(dp->d_name, O_RDONLY)) < 0)
				syserr("Can't open %s", dp->d_name);
			Batch_cnt = 0;
			if((i = getbatch(&descr, sizeof(descr))) != sizeof(descr))
				syserr(" cant read %d",i);
			printf("\tFound incomplete modify of %.12s, user = %s\n",
				descr.d_r.r_id, lookucode(descr.d_r.r_owner));

			bmove(descr.d_r.r_owner, Usercode, sizeof(descr.d_r.r_owner));
			close(Batch_fp);
			if (ask("\tComplete? "))
				modupdate();
		}
	}
	bmove(Utemp, Usercode, USERCODE_SIZE);
	ExitFn = tmpfn;
	closedir(dirp);
}

/*
** Catch errors in other places
*/
void
rubproc(void)
{
	register int		i;
	extern int		Acc_init;

	Error++;
	printf("Unable to restore!\n");
	noise(1);

	/* restore user code */
	bmove(Utemp, Usercode, sizeof(Utemp));

	/* close all possible files */
	if (Acc_init) {
		closecatalog(TRUE);
		unldb();
		acc_close();
	}

	/* close users file */
	getuser((char *) 0, (char *) NULL);

	/* get everything else */
	for (i = Direc + 1; i <= NOFILE; i++)
		close(i);
}
/*
** CHECKATTS
**	Checks that all attributes are in a relation
*/
void
checkatts(void)
{
	extern desc_t		Reldes, Attdes;
	register int		i;
	register int		once;
	tid_t			tid, limtid, reltid;
	char			key[MAX_TUP_SIZE];
	attr_t	atttup;
	relation_t		reltup;
	char			lastrel[MAX_NAME_SIZE + 2];

	once = 0;
	opencatalog("relation", OR_WRITE);
	opencatalog("attribute", OR_WRITE);
	clearkeys(&Attdes);
	lastrel[0] = '\0';
	if (find(&Attdes, NOKEY, &tid, &limtid, (void *) NULL))
		syserr("CHECKATT: find");

	while (!(i = get(&Attdes, &tid, &limtid, &atttup, TRUE))) {
		if (bequal(atttup.a_rel, lastrel, MAX_NAME_SIZE + 2))
			continue;

		clearkeys(&Reldes);
		ingres_setkey(&Reldes, key, atttup.a_rel, ATTRELID);
		ingres_setkey(&Reldes, key, atttup.a_owner, ATTOWNER);

		if ((i = getequal(&Reldes, key, &reltup, &reltid)) != 0) {
			if (i < 0)
				syserr("ATTCHECK: getequal");
			if (!once++)
				printf("\tNo relation for attribute(s):\n");
			printf("\t");
			printup(&Attdes, &atttup);
			if (ask("\tDelete?"))
				if ((i = delete(&Attdes, &tid)) != 0)
					syserr("ATTCHECK: delete=%d", i);
		}
		else
			bmove(atttup.a_rel, lastrel, MAX_NAME_SIZE + 2);
	}

	if (i < 0)
		syserr("ATTCHECK: get=%d", i);
}

/*
** HASNDX -- the relation indicated an index, check it out
**
**	will search the index relation for all secondary indices
**	and check to see that each secondary index named has an
**	entry in the relation relation.
*/
int
hasndx(char *id, char *own)
{
	register int	hasindices;
	register int	i, j;
	extern desc_t	Reldes, Inddes;
	tid_t		rtid;
	relation_t	rkey, rel;
	tid_t		itid, ihitid;
	index_t	ikey, ind;

	/* presume that answer is negative */
	hasindices = FALSE;

	/* set search for all tuples with 'id' and 'own' in indices */
	opencatalog("indices", OR_WRITE);
	clearkeys(&Inddes);
	ingres_setkey(&Inddes, &ikey, id, IRELIDP);
	ingres_setkey(&Inddes, &ikey, own, IOWNERP);
	if (find(&Inddes, EXACTKEY, &itid, &ihitid, &ikey))
		syserr("HASNDX: find");

	/* for each possible tuple in the indices relation */
	for (;;) {
		i = get(&Inddes, &itid, &ihitid, &ind, TRUE);

		/* check return values */
		if (i < 0)
			syserr("HASNDX: get=%d\n", i);
		if (i > 0)
			break;	/* finished */

		/* if key doesn't match, skip to next tuple */
		if(kcompare(&Inddes, &ikey, &ind))
			continue;
		hasindices = TRUE;

		/* verify that primary entry for sec index exists */
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &rkey, ind.i_index, RELID);
		ingres_setkey(&Reldes, &rkey, ind.i_owner, RELOWNER);
		if ((j = getequal(&Reldes, &rkey, &rel, &rtid)) != 0) {
			/* one doesn't exist, should we ignore it */
			if (j < 0)
				syserr("HASNDX: getequal=%d", j);
			printf("\tNo secondary index for indices entry:\n\t");
			printup(&Inddes, &ind);
			if (ask("\tDelete? ")) {
				/* get rid of bad entry in indices relation */
				if ((j = delete(&Inddes, &itid)) != 0)
					syserr("HASNDX: delete=%d", j);
				hasindices = FALSE;
			}
		}
	}
	return (hasindices);
}

/*
** ISNDX -- so you think that you're a secondary index, I'll check it out.
**
**	searches the indices relation for the name of the primary relation
**	and check to see if the primary is real.  Will also update the
**	'r_indexed' field of the primary if it isn't correct.
*/
int
isndx(char *id, char *own)
{
	register int	isindex;
	register int	i;
	extern desc_t	Inddes;
	tid_t		itid;
	index_t	ind, ikey;
	extern desc_t	Reldes;
	tid_t		rtid;
	relation_t	rel, rkey;

	/* search for tuple in index relation, should only be one */
	opencatalog("indices", OR_WRITE);
	clearkeys(&Inddes);
	ingres_setkey(&Inddes, &ikey, id, IRELIDI);
	ingres_setkey(&Inddes, &ikey, own, IOWNERP);
	if ((i = getequal(&Inddes, &ikey, &ind, &itid)) != 0) {
		/* there isn't a tuple in the indices relation */
		if (i < 0)
			syserr("ISNDX: getequal=%d", i);
		isindex = FALSE;
	} else {
		isindex = TRUE;

		/* there is a tuple in the indices relation */
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &rkey, ind.i_relname, RELID);
		ingres_setkey(&Reldes, &rkey, ind.i_owner, RELOWNER);

		/* see if the primary relation exists */
		if ((i = getequal(&Reldes, &rkey, &rel, &rtid)) != 0) {
			/* no it doesn't */
			if (i < 0)
				syserr("ISNDX: getequal=%d", i);

			/* what should be done about it */
			printf("\tNo primary relation for index:\n\t");
			printup(&Inddes, &ind);
			if (ask("\tDelete?")) {
				/*
				** get rid of indices tuple,
				** a FALSE return will also get rid
				** of the relation tuple
				*/
				if ((i = delete(&Inddes, &itid)) != 0)
					syserr("ISNDX: delete=%d", i);
				isindex = FALSE;
			}
		} else if (!(rel.r_indexed > 0) || (rel.r_status & S_INDEX) == S_INDEX) {
			/*
			** the primary tuple exists but isn't marked correctly
			*/
			printf("\t%.12s is index for:\n\t", rel.r_id);
			printup(&Reldes, &rel);
			if (ask("\tMark as indexed? ")) {
				rel.r_status |= S_INDEX;
				rel.r_indexed = SECBASE;
				if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
					syserr("ISNDX: replace=%d", i);
			}
		}
	}
	return (isindex);
}

/*
** HAVETREE -- check tree catalog for an entry with right name and owner
**
**	The 'id' and 'own' parameters are used to look in the tree catalog
**	for at least on tuple that also has a 'treetype' of 'mdvalue'.
**
**	If any tuples are found, havetree returns TRUE, else FALSE
*/
int
havetree(char *id, char *own, int mdvalue)
{
	extern desc_t	Treedes;
	register int	i;
	struct tree	tkey, trent;
	tid_t		ttid, thitid;

	/* search tree relation for tuple that matches */
	opencatalog("tree", OR_WRITE);
	clearkeys(&Treedes);
	ingres_setkey(&Treedes, &tkey, id, TREERELID);
	ingres_setkey(&Treedes, &tkey, own, TREEOWNER);
	ingres_setkey(&Treedes, &tkey, &mdvalue, TREETYPE);

	/* set search limit tids from the key */
	if ((i = find(&Treedes, EXACTKEY, &ttid, &thitid, &tkey)) != 0)
		syserr("HAVETREE: find=%d", i);

	for (;;) {
		i = get(&Treedes, &ttid, &thitid, &trent, TRUE);

		if (i < 0)
			syserr("HAVETREE: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Treedes, &tkey, &trent) == 0)
			return (TRUE);
	}
	return (FALSE);
}

/*
** ISPROT -- check in the 'protect' catalog for a tuple with right name, owner
**
**	search the 'protect' catalog for at least on tuple with matches the
**	values in the parameters. If 'treeid' is >= 0 then it is not used as
**	a key.
**
**	if one is found, returns TRUE, otherwise, returns FALSE
*/
int
isprot(char *id, char *own, int treeid)
{
	extern desc_t	Prodes;
	register int	i;
	protect_t	pkey, pent;
	tid_t		ptid, phitid;

	/* search the protect relation for at least on matching tuple */
	opencatalog("protect", OR_WRITE);
	clearkeys(&Prodes);
	ingres_setkey(&Prodes, &pkey, id, PRORELID);
	ingres_setkey(&Prodes, &pkey, own, PRORELOWN);
	if (treeid >= 0)
		ingres_setkey(&Prodes, &pkey, &treeid, PROTREE);

	/* set search limit tids from the keys */
	if ((i = find(&Prodes, EXACTKEY, &ptid, &phitid, &pkey)) != 0)
		syserr("ISPROT: find=%d", i);

	for (;;) {
		i = get(&Prodes, &ptid, &phitid, &pent, TRUE);

		if (i < 0)
			syserr("ISPROT: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Prodes, &pkey, &pent) == 0)
			return (TRUE);
	}
	return (FALSE);
}

/*
** ISINTEG -- check for a tuple in 'integrities'
**
**	searches the integrities relation for 'id' and 'own'.
**
**	returns TRUE if one is found, else FALSE
*/
int
isinteg(char *id, char *own, int treeid)
{
	extern desc_t		Intdes;
	register int		i;
	struct integrity	inkey, integ;
	tid_t			intid, inhitid;

	/* search the entire relation for a tuple that matches */
	opencatalog("integrities", OR_WRITE);
	clearkeys(&Intdes);
	ingres_setkey(&Intdes, &inkey, id, INTRELID);
	ingres_setkey(&Intdes, &inkey, own, INTRELOWNER);
	if (treeid >= 0)
		ingres_setkey(&Intdes, &inkey, &treeid, INTTREE);

	/* set the search limit tids from the key */
	if ((i = find(&Intdes, EXACTKEY, &intid, &inhitid, &inkey)) != 0)
		syserr("ISINTEG: find=%d", i);
	
	for (;;) {
		i = get(&Intdes, &intid, &inhitid, &integ, TRUE);

		if (i < 0)
			syserr("ISINTEG: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Intdes, &inkey, &integ) == 0)
			return (TRUE);
	}
	return (FALSE);
}

/*
** CHECKREL -- check relation relation against every thing else
**
**	Each tuple in the relation relation is read and each verifiable
**	characteristic is checked for accuracy.  Including the existence
**	of the physical file (if not a view), the qrymod definition if
**	appropriate and the secondary indexing.
*/
void
checkrel(void)
{
	extern desc_t	Reldes;
	register int	i, j;
	relation_t	rel;
	tid_t		rtid, limtid;
	char		fname[MAX_NAME_SIZE + 3];

	/* setup for search of entire relation */
	opencatalog("relation", OR_WRITE);
	clearkeys(&Reldes);
	if (find(&Reldes, NOKEY, &rtid, &limtid, (void *) NULL))
		syserr("CHECKREL: find");

	/* loop until all tuples checked */
	for (;;) {
		/* for each tuple in the rel-rel */
		i = get(&Reldes, &rtid, &limtid, &rel, TRUE);
		if (i > 0)
			break;	/* have finished */
		if (i < 0)
			syserr("CHECKREL: get=%d", i);

		/* if not a view, check for the file */
		if ((rel.r_status & S_VIEW) != S_VIEW) {
			ingresname(rel.r_id, rel.r_owner, fname);
			if ((j = open(fname, O_RDWR)) == -1) {
				printf("\tNo file for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tDelete tuple? ")) {
					if ((j = delete(&Reldes, &rtid)) != 0)
						syserr("CHECKREL: delete=%d", j);
					continue;
				}
				else
					/* don't call purge the file might still be there */
					Error++;
			}
			else
				close(j);
		}

		/* does it think that it has a secondary index */
		if (rel.r_indexed > 0) {
			/* does it really have an index? */
			if (!hasndx(rel.r_id, rel.r_owner)) {
				/* no, should it be fixed */
				printf("\tNo indices entry for primary relation:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? ")) {
					/* fix up relation relation entry */
					rel.r_indexed = 0;
					if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
						syserr("CHECKREL: replace=%d", i);
				}
			}
		}

		/* does it think that it is a secondary index */
		if (rel.r_indexed == SECINDEX) {
			/* check to make sure */
			if (!isndx(rel.r_id, rel.r_owner)) {
				/* none, what should be done? */
				printf("\tNo indices entry for index:\n\t");
				printup(&Reldes, &rel);
				if(ask("\tDelete? ")) {
					/*
					** get rid of rel-rel tuple for
					** secondary index,
					** purge will do rest of
					** removal if necessary
					*/
					if ((i = delete(&Reldes, &rtid)) != 0)
						syserr("CHECKREL: delete=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}

		/* if qrymod on in the database, check those catalogs too */
		if (Qrymod) {
			/*
			** cannot deal with S_VBASE since there is no way to
			** find the tree catalog entries without decoding the
			** 'treetree' fields.
			**
			** check to see if this is a view
			*/
			if ((rel.r_status & S_VIEW) &&
			    !havetree(rel.r_id, rel.r_owner, mdVIEW)) {
				/* no entry, should it be fixed? */
				printf("\tNo tree entry for this view:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tDelete tuple? ")) {
					/* delete relation entry */
					if ((i = delete(&Reldes, &rtid)) != 0)
						syserr("CHECKREL: delete=%d", i);
					continue;	/* skip to next entry in rel-rel */
				}
			}

			/* check to see if has 'protect' entry */
			if ((rel.r_status & S_PROTUPS) &&
			    !isprot(rel.r_id, rel.r_owner, -1)) {
				/* no entry, should the bit be reset */
				printf("\tNo protect entry for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? ")) {
					/* fix the bit */
					rel.r_status &= ~S_PROTUPS;
					if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
						syserr("CHECKREL: replace=%d", i);
				}
			}

			/* check to see if has 'integrities entry */
			if ((rel.r_status & S_INTEG) &&
			    !isinteg(rel.r_id, rel.r_owner, -1)) {
				/* no entry, should bit be reset */
				printf("\tNo integrities entry for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? ")) {
					/* fix up the bit */
					rel.r_status &= ~S_INTEG;
					if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
						syserr("CHECKREL: replace=%d", i);
				}
			}
		}
	}
}

/*
** CHECKTREE -- check the tree catalog against the others
*/
void
checktree(void)
{ 
	extern desc_t	Treedes, Reldes;
	register int	i;
	struct tree	trent;
	tid_t		ttid, thitid;
	relation_t	rkey, rel;
	tid_t		rtid;

	/* search the entire tree catalog */
	opencatalog("tree", OR_WRITE);
	clearkeys(&Treedes);
	if ((i = find(&Treedes, NOKEY, &ttid, &thitid, (void *) NULL)) != 0)
		syserr("CHECKTREE: find=%d", i);
	
	/* for each tuple in 'tree' */
	for (;;) {
		i = get(&Treedes, &ttid, &thitid, &trent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKTREE: get=%d", i);
		
		/* verify that a tuple exists in the relation relation */
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &rkey, trent.treerelid, RELID);
		ingres_setkey(&Reldes, &rkey, trent.treeowner, RELOWNER);

		/* fetch the tuple */
		if ((i = getequal(&Reldes, &rkey, &rel, &rtid)) != 0) {
			/*
			** Oops, a tuple doesn't exist in the relation
			** relation.
			**
			** maybe it's just a fatal error
			*/
			if (i < 0)
				syserr("CHECKTREE: getequal=%d", i);

			/* not a fatal error, what to do about it? */
			printf("\tNo relation tuple for:\n\t");
			printup(&Treedes, &trent);
			if (ask("\tDelete? ")) {
				if ((i = delete(&Treedes, &ttid)) != 0)
					syserr("CHECKTREE: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		} else {
			/*
			** Ah. A tuple does exist.
			**
			** If the r_status bits are correct then we can stop
			** here since elsewhere the 'protect' and 'integrity'
			** entries were verified.
			*/
			switch (trent.treetype) {
			  case mdVIEW:
				/* mere existence is sufficient */
				break;

			  case mdPROT:
				if ((rel.r_status & S_PROTUPS) != S_PROTUPS) {
					printf("\tNo 'protect' entry for:\n\t");
				deltup:
					printup(&Treedes, &trent);
					if (ask("\tDelete? ")) {
						if ((i = delete(&Treedes, &ttid)) != 0)
							syserr("CHECKTREE: delete=%d", i);
						continue;
					}
				}
				break;

			  case mdINTEG:
				if ((rel.r_status & S_INTEG) != S_INTEG) {
					printf("\tNo 'integrities' entry for:\n\t");
					goto	deltup;
				}
				break;

			  default:
				syserr("Unknown treetype: %d\n", trent.treetype);
			}
		}
	}
}

/*
**  CHECKPROTECT
*/
void
checkprotect(void)
{
	register int	i;
	extern desc_t	Reldes, Prodes;
	protect_t	pent;
	tid_t		ptid, phitid;
	relation_t	rkey, rel;
	tid_t		rtid;

	/* for each entry in the 'protect' relation */
	opencatalog("protect", OR_WRITE);
	clearkeys(&Prodes);
	if ((i = find(&Prodes, NOKEY, &ptid, &phitid, (void *) NULL)) != 0)
		syserr("CHECKPROTECT: find=%d", i);
	
	for (;;) {
		i = get(&Prodes, &ptid, &phitid, &pent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKPROTECT: get=%d", i);

		/* verify that a tuple exists in 'relation' */
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &rkey, pent.p_rel, RELID);
		ingres_setkey(&Reldes, &rkey, pent.p_owner, RELOWNER);

		/* fetch the tuple if possible */
		if ((i = getequal(&Reldes, &rkey, &rel, &rtid)) != 0) {
			/*
			** Oops.  A tuple doesn't exits in 'relation'
			**
			** Maybe it's just a fatal error.
			*/
			if (i < 0)
				syserr("CHECKPROTECT: getequal=%d", i);
			
			/* not a fatal error, what to do? */
			printf("\tNo relation for 'protect' entry:\n\t");
			printup(&Prodes, &pent);
			if (ask("\tRemove 'protect' entry? ")) {
				if ((i = delete(&Prodes, &ptid)) != 0)
					syserr("CHECKPROTECT: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		} else {
			/* 'relation' entry exists, check for the tree entry */
			if (pent.p_tree >= 0) {
				if (!havetree(pent.p_rel, pent.p_owner, mdPROT)) {
					/* no tuples in 'tree' */
					printf("\tNo tree for:\n\t");
					printup(&Prodes, &pent);
					if (ask("\tDelete entry and fix relation status bits? ")) {
						if ((i = delete(&Prodes, &pent)) != 0)
							syserr("CHECKPROTECT: delete=%d", i);
						rel.r_status &= ~S_PROTUPS;
						if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
							syserr("CHECKPROTECT: replace=%d", i);
						continue;	/* go on to next tuple */
					}
				}
			}
			if ((rel.r_status & S_PROTUPS) != S_PROTUPS) {
				/* bits not set correctly */
				printf("\tIncorrect relation status bits for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? ")) {
					rel.r_status |= S_PROTUPS;
					if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
						syserr("CHECKPROTECT: replace=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}
	}
}

/*
**  CHECKINTEG
*/
void
checkinteg(void)
{
	register int		i;
	extern desc_t		Reldes, Intdes;
	struct integrity	inent;
	tid_t			intid, inhitid;
	relation_t		rkey, rel;
	tid_t			rtid;

	/* for each entry in 'integrities' */
	opencatalog("integrities", OR_WRITE);
	clearkeys(&Intdes);
	if ((i = find(&Intdes, NOKEY, &intid, &inhitid, (void *) NULL)) != 0)
		syserr("CHECKINTEG: find=%d", i);
	
	for (;;) {
		i = get(&Intdes, &intid, &inhitid, &inent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKINTEG: get=%d", i);

		/* verify that a tuple exists in 'relation' */
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &rkey, inent.intrelid, RELID);
		ingres_setkey(&Reldes, &rkey, inent.intrelowner, RELOWNER);

		/* fetch the tuple if possible */
		if ((i = getequal(&Reldes, &rkey, &rel, &rtid)) != 0) {
			/*
			** Oops.  A tuple doesn't exits in 'relation'
			**
			** Maybe it's just a fatal error.
			*/
			if (i < 0)
				syserr("CHECKINTEG: getequal=%d", i);
			
			/* not a fatal error, what to do? */
			printf("\tNo relation for 'integrities' entry:\n\t");
			printup(&Intdes, &inent);
			if (ask("\tRemove 'integrities' entry? ")) {
				if ((i = delete(&Intdes, &intid)) != 0)
					syserr("CHECKINTEG: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		} else {
			/* 'relation' entry exists, check for the tree entry */
			if (inent.inttree >= 0) {
				if (!havetree(inent.intrelid, inent.intrelowner, mdINTEG)) {
					/* no tuples in 'tree' */
					printf("\tNo tree for:\n\t");
					printup(&Intdes, &inent);
					if (ask("\tDelete entry and fix relation status bits? ")) {
						if ((i = delete(&Intdes, &inent)) != 0)
							syserr("CHECKINTEG: delete=%d", i);
						rel.r_status &= ~S_INTEG;
						if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
							syserr("CHECKINTEG: replace=%d", i);
						continue;	/* go on to next tuple */
					}
				}
			}
			if ((rel.r_status & S_INTEG) != S_INTEG) {
				/* bits not set correctly */
				printf("\tIncorrect relation status bits for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? ")) {
					rel.r_status |= S_INTEG;
					if ((i = replace(&Reldes, &rtid, &rel, FALSE)) != 0)
						syserr("CHECKINTEG: replace=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}
	}
}

void
main(int argc, char **argv)
{
	register int	i;
	int		j;
	register char	*dbname;
	int	status;
	extern	void	(*ExitFn)();
	char		*nargv[20];
	char		**avp;
	char		*cp;

	setprocname("RESTORE");

	/* check param list */
	argv[argc] = NULL;
#ifdef xTTR1
	tTrace(argv, 'T', tTvect, 100);
	tTrace(argv, 'Z', tTdbu, 100);
#endif

	initialize(argc, argv);

	/* do it to it */
	ExitFn = rubproc;
	signal(SIGQUIT, exit);
	while ((dbname = getnxtdb()) != 0) {
		Berror = Error = 0;

		/* first restart point for this database */
		setjmp(Jmpbuffer);
		if (Error)	/* if set, will cause skip to next database */
			continue;
		printf("\nRestoring database: %s\t", dbname);

		acc_init(0, 0);
		printf("owner: %s\n", lookucode(Admin.ad_h.adm_owner));

		/* set exclusive lock on data base */
		db_lock(M_EXCL);

		restore();	/* recover batch update and modify files */
		printf("\tRecovery of batch files complete.\n");

		/*
		** second restart point for this database
		**	the batch files are completed and now the system
		**	relations need checking
		*/
		setjmp(Jmpbuffer);
		if (Error)		/* again, may cause skipping to next database */
			continue;
		printf("\tChecking system relations\n");


		/*
		** check the relation relation
		**	this will mean checking for file existence,
		**	and whether the r_status bits are supported by
		**	the information in the other catalogs.
		*/
		checkrel();

		/*
		** check the attribute relation
		**	for each tuple in the attribute relation, there must
		**	be a tuple in the relation relation.
		**	the indices relation doesn't need to be reverse checked
		**	into the relation relation since the order things are
		**	handled else where in the system is in the correct
		**	order.  All the other catalogs need to be reverse checked.
		*/
		checkatts();

		/* only check the qrymod catalogs if qrymod is turned on */
		if (Qrymod) {
			/* check the protect relation */
			checkprotect();

			/* check the integrities relation */
			checkinteg();

			/*
			** check the tree relation
			** must be done last since it depends upon
			** a state of the system relations provided
			** by the other check... routines.
			*/
			checktree();
		}

		/* finished, close up the database and go on to the next */
		closecatalog(TRUE);
		unldb();
		acc_close();

		/* call PURGE if no errors */
		if (!Berror && !Error) {
			printf("\tCalling purge: ");
			fflush(stdout);
			if ((i = fork()) == -1)
				printf("Can't fork\n");
			else if (!i) {
				avp = nargv;
				*avp++ = "Purge";
				for (j = 0 ; (cp = getflagvect(j)) != NULL ; j++) {
					*avp++ = cp;
				}
				*avp++ = dbname;
				*avp++ = 0;
#ifdef xTTR2
				if (tTf(0, 1))
					for (avp = nargv, i = 0; *avp != NULL; avp++, i++)
						printf("%d %s\n", i, *avp);
#endif
				for (i=3; i <= NOFILE; i++)
					close(i);
				execv(ztack(Pathname, "/bin/purge"), nargv);
				execvp(PURGE, nargv);
				printf("Cannot exec %s\n", PURGE);
				exit(-1);
			} else
				wait(&status);
		}
	}
}

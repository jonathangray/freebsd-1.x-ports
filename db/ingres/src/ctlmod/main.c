#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <signal.h>

#include "tree.h"
#include "ctlmod.h"
#include "pipes.h"
#include <resp.h>
#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)main.c	8.1	12/31/84)

/*
**  MAIN -- initialize control module
**
**	Called only once, this routine sets everything up.
**
**	The format of the argv is as follows:
**		argv[0] -- pathname
**		argv[1] -- file descriptor for input pipe or'ed with
**			0100 to make it printable in a 'ps'.
**		argv[2] -- Fileset
**		argv[3] -- Usercode
**		argv[4] -- Database
**		argv[5] -- Pathname
**
**	Parameters:
**		argc, argv -- as usual
**
**	Returns:
**		none
**
**	Side Effects:
**		Many; see code.
**		Proc_name is set to the working process name.
**		Fileset, Database, Usercode -- set from argv.
**		Trace vectors are initialized.
**
**	Requires:
**		markopen -- to mark the open files.
**		The input pipe must have the Cm struct in it as
**			the first data.
**
**	Trace Flags:
**		1
**
*/

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
extern struct out_arg	Out_arg;	/* output arguments */
jmp_buf		CmReset;	/* restart addr on interrupt */
#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */

static pb_t	*MonPpb;

pb_t *
getmonppb(void)
{
	return(MonPpb);
}

void
setmonppb(pb_t *pp)
{
	MonPpb = pp;
}

extern long	CmOfiles;	/* defined in markopen.c */

void
ctlmod_main(int argc, char **argv, int nocheck)
{
	register int	i;
	func_t		*f;
	register char	**q;
	register char	*p;
	pb_t		pb;
	static int	reenter;
	static bool	nobuffer;
	static int	lock_type = -1;	/* type of data base lock to request */
	static int	wait_action = -1;
			/* type of wait action on the data base */

	setprocname(Ctx.ctx_name = argv[0]);
	argv[argc] = NULL;
	nobuffer = tTrace(argv, argv[1][1], FuncVect[0]->fn_tvect, 30);
	Ctx.ctx_tvect = tT;
	reenter = 0;
	setjmp(CmReset);
	if (reenter++) {
		exit(-1);
	}
	if (signal(SIGINT, SIG_IGN) == SIG_DFL) {
		signal(SIGINT, rubcatch);
	} else {
		RubLevel = -1;
	}
	setmonppb(&pb);

	/* mark all currently open files */
	if (!nocheck) {
		acc_init(0, 0);
	}
	markopen(&CmOfiles);

	/*
	**  Process argument vector.
	**	The easy ones just involve saving a pointer.
	**	argv[1] is used to get a file descriptor; this
	**		becomes the initial input.  This file
	**		is read to fill in the Cm (configuration)
	**		structure.
	*/

	if (tTf(1, 0) || argc < 6) {
		prargs(argc, argv);
	}
		
	if (argc < 6) {
		syserr("main: argc=%d", argc);
	}

	q = &argv[2];
	Fileset = *q++;
	Usercode = *q++;
	Database = *q++;
	Pathname = *q++;

	i = read(argv[1][0] & 077, (char *) &Cm, sizeof(Cm));
	if (i != sizeof(Cm)) {
		syserr("main: read %d", i);
	}


	/* set up other globals */
	setprocname(Ctx.ctx_name = Cm.cm_myname);
	initbuf(Qbuf, QbufSize, ERR_QBUF, error);
	Ctx.ctx_cmark = Ctx.ctx_pmark = markbuf(Qbuf);

	
	if (strcmp(getprocname(), "SYSMOD") != 0) {
		/* process flags */
		for (; (p = *q) != NULL; q++) {
			if (p[0] != '-')
				continue;
			switch (p[1]) {
		  	case 'l':	/* Lock type */
				if ( Alockdes < 0 )
					break;
				if (!isdigit(p[2]))
					syserr("Illegal lock number %s",&p[2]);
				lock_type = atoi(&p[2]);
				if ( wait_action < 0 )
					break;
                                if ( setdbl(wait_action,lock_type) < 0 ) {
					syserr("Data base temporarily unavailable");
				}
				break;
			  case 'W':
				/*
				** type of data base wait to preform
				*/
				if ( Alockdes < 0 )
					break;
				if (!isdigit(p[2]))
					syserr("Illegal wait action %s",&p[2]);
				wait_action = atoi(&p[2]);
				if ( lock_type < 0 )
					break;
				if ( setdbl(wait_action,lock_type) < 0 ) {
					syserr("Data base temporarily unavailable");
				}
				break;
			  case '&':	/* equel program */
				Equel = 1;
				if (p[6] != '\0')
					Equel = 2;
				break;
	
			  case 'c':	/* c0 sizes */
				Out_arg.c0width = atoi(&p[2]);
				break;
	
			  case 'i':	/* iNsizes */
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
	
				}
				break;
	
			  case 'f':	/* fN sizes */
				p = &p[3];
				i = *p++;
				while (*p != '.')
					p++;
				*p++ = 0;
				if ((*q)[2] == '4') {
					Out_arg.f4width = atoi(&(*q)[4]);
					Out_arg.f4prec = atoi(p);
					Out_arg.f4style = i;
				} else {
					Out_arg.f8width = atoi(&(*q)[4]);
					Out_arg.f8prec = atoi(p);
					Out_arg.f8style = i;
				}
				*--p = '.';	/* restore parm for dbu's */
				break;
	
			  case 'v':	/* vertical seperator */
				Out_arg.coldelim = p[2];
				break;
	
			}
		}
	
	}
		/* set up trace flags */
		for (i = 0; i < NumFunc; i++) {
			f = FuncVect[i];
			if (f->fn_tflag != '\0')
				nobuffer |= tTrace(argv, f->fn_tflag, f->fn_tvect, f->fn_tsize);
			setprocname(Ctx.ctx_name = f->fn_name);
			(*f->fn_initfn)(argc, argv);
		}
	/* end of ingres flags */

	/*
	**  Buffer standard output
	**
	**	Since VM/UNIX always buffers, we force non-buffered
	**	output if any trace flags are set.
	*/

	if (!nobuffer)
		set_so_buf();
	else
		setbuf(stdout, NULL);

	/* if Equel, tell the program to go ahead */
	if (Equel && Cm.cm_myproc == 1) {
		pb_prime(&pb, PB_REG);
		pb.pb_st = PB_FRONT;
		pb_flush(&pb);
	}

	/*
	**  Start executing routines.
	**
	**	Do_seq knows to exit if we get an EOF on the input pipe.
	*/

	i = setjmp(CmReset);
#ifdef xMONITOR
	markperf(&CmMonBuf);
#endif /* xMONITOR */
	initbuf(Qbuf, QbufSize, ERR_QBUF, error);
	clrmem((char *) &Ctx, sizeof(Ctx));
	Ctx.ctx_cmark = Ctx.ctx_pmark = markbuf(Qbuf);
	setprocname(Ctx.ctx_name = Cm.cm_myname);
	Ctx.ctx_tvect = tT = FuncVect[0]->fn_tvect;
#ifdef xCTR2
	if (tTf(1, 1))
		lprintf("main: setjmp: %d\n", i);
#endif
	if (RubLevel >= 0) {
		signal(SIGINT, rubcatch);
	}
	closeall(FALSE, CmOfiles);
	for (;;) {
		Cm.cm_input = Cm.cm_rinput;
		pb.pb_st = PB_UNKNOWN;
		do_seq(&pb);
	}
}
/*
**  RUBPROC -- process rubout signals
**
**	This routine does the processing needed on rubouts
**	when running with the control module.  It basically
**	flushes pipes.
**
**	Parameters:
**		none.
**
**	Returns:
**		never
**
**	Side Effects:
**		Flushes pipes, etc.
**
**      This module should be renamed to compile the stuffs in the ../support
**          directory, because the latter modules also define rubproc() in
**          a different manner.
*/

void
ctlmod_rubproc(void)
{
	register int	i;
	pb_t		pb;
	register int	stat;

	/*
	**  Update the world for consistency.
	*/

	fflush(stdout);
	closecatalog(FALSE);
	i = pageflush(NULL);
	if (i != 0) {
		syserr("rubproc: pageflush %d", i);
	}

	/*
	**  Send SYNC blocks to all processes that are adjacent
	**	in the write direction.
	**  Arrange to ignore blocks from all processes that
	**	are adjacent in the read direction.
	*/

	pb_prime(&pb, PB_SYNC);
	for (i = 0; i < CM_MAXPROC; i++) {
		stat = Cm.cm_proc[i].pr_stat;
		if ((stat & PR_RADJCT) != 0) {
			Syncs[i]++;
		}
		if ((stat & PR_WADJCT) != 0) {
			pb.pb_proc = i;
			pb_write(&pb);
		}
	}

	/*
	**  Cleanup and exit.
	*/

	cm_cleanup(2);
}

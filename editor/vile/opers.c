/*
 * This file contains the command processing functions for the commands
 * that take motion operators.
 * written for vile by Paul Fox, (c)1990
 *
 * $Log: opers.c,v $
 * Revision 1.1  1994/02/01 03:29:36  jkh
 * Initial revision
 *
 * Revision 1.34  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.33  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.32  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.31  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.30  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.29  1993/06/02  14:58:17  pgf
 * folded some long lines
 *
 * Revision 1.28  1993/04/21  13:55:27  pgf
 * consolidate repeat count processing into single routine in main.c, to
 * make them multiply correctly
 *
 * Revision 1.27  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.26  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.25  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.24  1993/01/23  14:28:07  foxharp
 * report failed motions with message
 *
 * Revision 1.23  1992/11/19  09:15:37  foxharp
 * be sure to turn doingopcmd off if the motion fails or is aborted.
 * also, allow null regions -- i don't know why i didn't used to allow them
 *
 * Revision 1.22  1992/07/16  22:18:54  foxharp
 * ins() takes an argument -- whether or not to playback, usually FALSE
 *
 * Revision 1.21  1992/05/25  21:07:48  foxharp
 * extern func declarations moved to header
 *
 * Revision 1.20  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.19  1992/01/06  23:09:33  pgf
 * error message if bad function specified for motion
 *
 * Revision 1.18  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.17  1992/01/03  23:26:48  pgf
 * make the "pre-operator" position, pre_op_dot, a global so that it can
 * be adjust slightly by some motions -- paragraph motions in particular
 *
 * Revision 1.16  1991/11/03  17:46:30  pgf
 * removed f,n args from all region functions -- they don't use them,
 * since they're no longer directly called by the user
 *
 * Revision 1.15  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.14  1991/10/29  14:35:29  pgf
 * implemented the & commands: substagain
 *
 * Revision 1.13  1991/09/10  00:51:05  pgf
 * only restore dot with swapmark if the buffer hasn't switched on us
 *
 * Revision 1.12  1991/08/13  02:50:52  pgf
 * fixed chgreg case of butting against top of buffer
 *
 * Revision 1.11  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.10
 * date: 1991/07/19 17:23:06;
 * added status return to chgreg()
 * 
 * revision 1.9
 * date: 1991/07/19 17:16:41;
 * fix null pointer de-ref
 * 
 * revision 1.8
 * date: 1991/06/25 19:53:04;
 * massive data structure restructure
 * 
 * revision 1.7
 * date: 1991/06/16 17:36:47;
 * added entab, detab, and trim operator routines
 * 
 * revision 1.6
 * date: 1991/06/15 09:11:23;
 * hardened chgreg[ion] against motions that don't succeed, leaving
 * the mark unset, so swapmark fails
 * 
 * revision 1.5
 * date: 1991/06/03 10:26:34;
 * cleanup, and
 * pulled some code out of execute() into here
 * 
 * revision 1.4
 * date: 1991/05/31 11:38:43;
 * syntax error from bad merge
 * 
 * revision 1.3
 * date: 1991/05/31 11:17:31;
 * lot of cleanup, some new operators -- now use godotplus() instead of
 * stutterfunc().  much better
 * 
 * revision 1.2
 * date: 1991/04/04 09:39:56;
 * added operfilter (!) command
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:52;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"

extern CMDFUNC f_godotplus;

typedef	int	(*OpsFunc) P((void));

static	int	chgreg P(( void ));
static	int	shift_n_times P(( int, int, OpsFunc, char * ));


/* For the "operator" commands -- the following command is a motion, or
 *  the operator itself is repeated.  All operate on regions.
 */
int
operator(f,n,fn,str)
int f,n;
OpsFunc fn;
char *str;
{
	int c;
	int this1key;
	int status;
	CMDFUNC *cfp;			/* function to execute */
	char tok[NSTRING];		/* command incoming */
	BUFFER *ourbp;

	doingopcmd = TRUE;

	pre_op_dot = DOT;
	ourbp = curbp;

	if (havemotion != NULL) {
		cfp = havemotion;
		havemotion = NULL;
	} else {
		mlwrite("%s operation pending...",str);
		(void)update(FALSE);

		/* get the next command from the keyboard */
		/* or a command line, as approp. */
		if (clexec) {
			macarg(tok);	/* get the next token */
			if (!strcmp(tok,"lines"))
				cfp = &f_godotplus;
			else
				cfp = engl2fnc(tok);
		} else {
			this1key = last1key;
			c = kbd_seq();

			/* allow second chance for entering counts */
			do_repeats(&c,&f,&n);

			if (this1key == last1key)
				cfp = &f_godotplus;
			else
				cfp = kcod2fnc(c);

		}
		if (cfp)
			mlerase();
		else
			mlforce("[No such function]");
	}
	if (!cfp) {
		doingopcmd = FALSE;
		return FALSE;
	}

	if ((cfp->c_flags & MOTION) == 0) {
		TTbeep();
		doingopcmd = FALSE;
		return(ABORT);
	}

	/* motion is interpreted as affecting full lines */
	if (cfp->c_flags & FL)
		fulllineregions = TRUE;

	/* and execute the motion */
	status = execute(cfp, f,n);

	if (status != TRUE) {
		doingopcmd = FALSE;
		fulllineregions = FALSE;
		mlforce("[Motion failed]");
		return FALSE;
	}

	opcmd = 0;

	MK = pre_op_dot;

	/* we've successfully set up a region */
	if (!fn) { /* be defensive */
		mlforce("BUG -- null func pointer in operator");
		status = FALSE;
	} else {
		status = (fn)();
	}

	if (ourbp == curbp) /* in case the func switched buffers on us */
		swapmark();

	if (fulllineregions) {
		fulllineregions = FALSE;
		(void)firstnonwhite(FALSE,1);
	}

	doingopcmd = FALSE;
	return status;
}

int
operdel(f,n)
int f,n;
{
	int	status;

	opcmd = OPDEL;
	lines_deleted = 0;
	status = operator(f, n, killregion,
		fulllineregions
			? "Delete of full lines"
			: "Delete");
	if (do_report(lines_deleted))
		mlforce("[%d lines deleted]", lines_deleted);
	return status;
}

int
operlinedel(f,n)
int f,n;
{
	fulllineregions = TRUE;
	return operdel(f,n);
}

static int
chgreg()
{
	killregion();
	if (fulllineregions) {
		if (backline(FALSE,1) == TRUE) /* returns FALSE at top of buf */
			return opendown(TRUE,1);
		else
			return openup(TRUE,1);
	}
	return ins();
}

int
operchg(f,n)
int f,n;
{
	int s;

	opcmd = OPOTHER;
	s = operator(f,n,chgreg,"Change");
	if (s == TRUE) swapmark();
	return s;
}

int
operlinechg(f,n)
int f,n;
{
	int s;

	fulllineregions = TRUE;
	opcmd = OPOTHER;
	s = operator(f,n,chgreg,"Change of full lines");
	if (s == TRUE) swapmark();
	return s;
}

int
operjoin(f,n)
int f,n;
{
	opcmd = OPOTHER;
	return operator(f,n,joinregion,"Join");
}

int
operyank(f,n)
int f,n;
{
	opcmd = OPOTHER;
	return operator(f,n,yankregion,"Yank");
}

int
operlineyank(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,yankregion,"Yank of full lines");
}

int
operflip(f,n)
int f,n;
{
	opcmd = OPOTHER;
	return operator(f,n,flipregion,"Flip case");
}

int
operupper(f,n)
int f,n;
{
	opcmd = OPOTHER;
	return operator(f,n,upperregion,"Upper case");
}

int
operlower(f,n)
int f,n;
{
	opcmd = OPOTHER;
	return operator(f,n,lowerregion,"Lower case");
}

/*
 * The shift commands are special, because vi allows an implicit repeat-count
 * to be specified by repeating the '<' or '>' operators.
 */
static int
shift_n_times(f,n, func, msg)
int	f,n;
OpsFunc	func;
char	*msg;
{
	register int status = FALSE;

	fulllineregions = TRUE;
	opcmd = OPOTHER;

	if (havemotion != NULL) {
		CMDFUNC *cfp = havemotion;
		while (n-- > 0) {
			havemotion = cfp;
			if ((status = operator(FALSE,1, func, msg)) != TRUE)
				break;
		}
	} else
		status = operator(f,n, func, msg);
	return status;
}

int
operlshift(f,n)
int f,n;
{
	return shift_n_times(f,n,shiftlregion,"Left shift");
}

int
operrshift(f,n)
int f,n;
{
	return shift_n_times(f,n,shiftrregion,"Right shift");
}

int
operwrite(f,n)
int f,n;
{
        register int    s;
        char fname[NFILEN];

	if (ukb != 0) {
	        if ((s=mlreply_file("Write to file: ", (TBUFF **)0,
				FILEC_WRITE|FILEC_PROMPT, fname)) != TRUE)
	                return s;
		return kwrite(fname,TRUE);
	} else {
		opcmd = OPOTHER;
		return operator(f,n,writeregion,"File write");
	}
}

int
operformat(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,formatregion,"Format");
}

int
operfilter(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,filterregion,"Filter");
}


int
operprint(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,plineregion,"Line print");
}

int
operlist(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,llineregion,"Line list");
}

int
opersubst(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,substregion,"Substitute");
}

int
opersubstagain(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,subst_again_region,"Substitute-again");
}

int
operentab(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,entab_region,"Spaces-->Tabs");
}

int
operdetab(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,detab_region,"Tabs-->Spaces");
}

int
opertrim(f,n)
int f,n;
{
	fulllineregions = TRUE;
	opcmd = OPOTHER;
	return operator(f,n,trim_region,"Trim whitespace");
}

#ifndef lint
static char yysccsid[] = "@(#)yaccpar	1.8 (Berkeley) 01/20/91";
#endif
#define YYBYACC 1
#line 2 "ingres.y"
#include <sys/types.h>
#include <sys/param.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <stdarg.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <version.h>
#include <access.h>
#include <lock.h>
#include <ctlmod.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_EQUEL
#include "protos.h"

/*
**  INGRES -- INGRES startup
**
**	This program starts up the entire system.
**
**	Parameters:
**		1 -- database name
**		2 -- optional process table name
**		x -- flags of the form +x or -x may be freely inter-
**			sperced in the argument list.
**
**	Return:
**		none if successful
**		1 -- user error (no database, etc)
**		-1 -- system error
**
**	Flags:
**		-&xxxx -- EQUEL flag: xxxx are file descriptors for the
**			status return pipe, the command write pipe, the
**			data return pipe, and the data transfer pipe
**			respectively.
**		-@xxxx -- xxxx is same as EQUEL flag, but no flags
**			are set.
**		-*?? -- Masterid flag. Gives the siteid of the master
**			site in a distributed ingres. (Used in dist.
**			ingres' initproc() function.)
**		-|xxxx -- Network flag.  This flag is just passed to
**			the other processes, to be processed by the
**			DBU's.
**		-uusername -- set effective user to be username.  You
**			must be INGRES or the DBA for the database to
**			use this option.
**		-cN -- set minimum character field output width to be
**			N, default 6.  This is the fewest number of
**			characters which may be output in any "c" type
**			field.
**		-inN -- integer output width.  this is the width of
**			an integer field.  The small "n" is the size
**			of the internal field ("1", "2", or "4") and
**			N is the width of the field for that flag.
**			The defaults are -i16, -i26, and -i413.
**		-fnxN.M -- floating point output width and precision.
**			Small "n" is the internal width in bytes ("4"
**			or "8"), x is the format (f, F, g, G, e, E,
**			n, or N), N is the field width, and M is the
**			precision (number of digits after the decimal
**			point).  The formats are:
**			"f" or "F": FORTRAN-style F format: digits,
**				decimal point, digits, no exponent.
**			"e" or "E": FORTRAN-style E format: digits,
**				decimal point, digits, letter "e" (or
**				"E", depending on "x" in the param-
**				eter), and an exponent.  The scaling
**				factor is always one, that is, there
**				is always one digit before the decimal
**				point.
**			"g" or "G": F format if it will fit in the
**				field, otherwise E format.  Space is
**				always left at the right of the field
**				for the exponent, so that decimal
**				points will align.
**			"n" or "N": like G, except that space is not
**				left for the decimal point in F style
**				format (useful if you expect everything
**				to fit, but you're not sure).
**			The default is -fn10.3.
**		-vx -- set vertical seperator for print operations
**			and retrieves to the terminal to be "x".  The
**			default is vertical bar ("|").
**		+w -- database wait state.  If set ("+w"), you will
**			wait until the database is not busy.  If clear,
**			you will be informed if the database is busy.
**			If not specified, the same operations take
**			place depending on whether or not you are
**			running in background (determined by whether
**			or not your input is a teletype).  If in fore-
**			ground, you are informed; if in background,
**			you wait.
**		-M -- monitor trace flag
**		-P -- parser trace flag
**		-O -- ovqp trace flag
**		-Q -- qrymod trace flag
**		-D -- decomp trace flag
**		-Z -- dbu trace flag.  These flags require the 020 bit
**			in the status field of the users file to be
**			set.  The syntax is loose and is described
**			elsewhere.  Briefly, "-Z" sets all flags except
**			the last 20, "-Z4" sets flag 4, and "-Z5/7"
**			sets all flags from 5 through 7.
**		+L -- enable/disable upper to lower case mapping in the
**			parser.  Used for debugging.
**		-rmode -- retrieve into mode
**		-nmode -- index mode.  These flags give the default
**			modify mode for retrieve into and index.  They
**			default to cheapsort and isam.  "Mode" can be
**			any mode to modify except "truncated".
**		+a -- enable/disable autoclear function in monitor.
**			Default on.
**		+b -- enable/disable batch update.  Default on.
**			The 02 bit is needed to clear this flag.
**		+d -- enable/disable printing of the dayfile.  Default
**			on.
**		+s -- enable/disable printing of almost everything from
**			the monitor.
**		+U -- enable/disable direct update of system catalogs.
**			Default off.  The 04 bit is needed to set this
**			option.
**
**	Files:
**		.../files/usage -- to print a "usage: ..." message.
**		.../data/base/<database>/admin -- to determine
**			existance and some info about <database>.
**		.../files/dayfile<MAJOR_VERSION> -- dayfile (printed by
**			monitor).
**		.../files/users -- file with UNIX uid -> INGRES code
**			mapping, plus a pile of other information about
**			the user.
**		.../files/proctab<MAJOR_VERSION> -- default process table
**
**
**	Note:
**		this is also the startup program for sysmod.
**		different flags are expected and the default process table
**		is different from that of ingres.
**
*/

SCCSID(@(#)ingres.y	8.4	12/18/85)

#define	MAXOPTNS	10		/* maximum number of options you can specify */
#define	SYSMAXOPTNS	6		/* maximum number of options to sysmod */
#define	MAXPROCS	10		/* maximum number of processes in the system */
#define	EQUELFLAG	'&'
#define	NETFLAG		'|'		/* network slave flag */
#define	CLOSED		'?'

char		Fileset[10];
char		*Database;
extern char	*Dbpath;		/* defined in initucode */
admin_t	Admin;			/* set in initucode */
lock_req_t	Lock;
FILE		*ProcFile;		/* fildes for the process table */
char		*DefProcTab = NULL;	/* default process table name */
char		*Opt[MAXOPTNS + 1];
int		Nopts;
int		No_exec;		/* if set, don't execute */
int		NumProcs;		/* number of processes this system */

/*
**  Internal form of process descriptions.
*/

struct proc {
	short		prstat;		/* status bits, see below */
	char		prmpipe;	/* initial pipe to this process */
	char		prtflag;	/* trace flag for CM this proc */
	char		prpath[50];	/* pathname of this process */
	struct _cm_t	prcm;		/* cm info passed to this proc */
};

/* bits for prstat */
#define PR_REALUID	0001		/* run as the user, not INGRES */
#define PR_NOCHDIR	0002		/* don't chdir into database */
#define PR_CLSSIN	0004		/* close standard input */
#define PR_CLSDOUT	0010		/* close diagnostic output */

struct proc	ProcTab[CM_MAXPROC];


/*
**  Open pipe info.
*/

struct pipeinfo {
	char	pip_rfd;	/* read file descriptor */
	char	pip_wfd;	/* write file descriptor */
	short	pip_rcnt;	/* read reference count */
	short	pip_wcnt;	/* write reference count */
};

struct pipeinfo Pipe[128];


/*
**  Macro definitions
*/

char	Macro[26][80];


/* globals used by the grammar. */
struct proc	*Proc;
state_t		*StateP;
proc_t		*ProcP;
int		ProcNo;
int		RemStat;

#define BOLSTATE	0	/* beginning of line */
#define NORMSTATE	1	/* normal token */
#define EOFSTATE	2	/* end of file */

int	LineNo;			/* current line number */

void
usrerr(char *f, ...)
{
	va_list	vp;

	va_start(vp, f);
	printf("Line %d: ", LineNo);
	vfprintf(stdout, f, vp);
	printf("\n");
	va_end(vp);
}

#line 257 "ingres.y"
typedef union
{
	int	yyint;		/* integer */
	char	*yystr;		/* string */
	char	yypip;		/* pipe id */
	char	yychar;		/* single character */
} YYSTYPE;
#line 268 "y.tab.c"
#define INT_CONST 257
#define STR 258
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,   10,   10,   11,   11,   12,   13,   13,   15,   15,
   16,   17,   18,   19,   19,   20,   14,    1,    2,    3,
    5,    6,    7,    9,    4,    8,
};
short yylen[] = {                                         2,
    1,    1,    2,    2,    1,    7,    1,    2,    1,    1,
    5,    2,    6,    0,    2,    1,    3,    1,    1,    1,
    1,    1,    1,    1,    1,    1,
};
short yydefred[] = {                                      0,
    0,    0,    0,    0,    2,    0,    5,   18,    0,   26,
    0,    3,    0,    0,    0,    7,    9,   10,   14,   21,
    0,   17,   19,    0,    0,    8,    0,   22,    0,   25,
    0,    0,   16,   15,   23,    0,   20,    0,    0,    0,
   11,    0,   24,    6,   13,
};
short yydgoto[] = {                                       3,
    9,   24,   38,   31,   21,   29,   36,   11,   44,    4,
    5,    6,   15,    7,   16,   17,   18,   19,   27,   34,
};
short yysindex[] = {                                    -65,
 -250, -249,    0,  -65,    0,  -72,    0,    0, -246,    0,
 -244,    0, -239, -250,  -72,    0,    0,    0,    0,    0,
 -238,    0,    0, -236, -236,    0, -239,    0, -235,    0,
 -233, -235,    0,    0,    0, -236,    0, -239, -235, -232,
    0, -236,    0,    0,    0,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    8,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    2,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    1,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
    5,  -22,    0,  -25,    0,    0,  -26,    0,    0,    0,
   18,    0,    0,    0,   10,    0,    0,    0,    0,    0,
};
#define YYTABLESIZE 83
short yytable[] = {                                      32,
   12,    4,    2,   13,   33,   39,    8,    1,   10,   14,
   40,   20,   42,   22,    1,   41,   45,   23,   25,   28,
   30,   12,   35,   37,   26,   43,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   12,    4,
    0,    0,    0,    0,    0,    0,   12,    0,    0,    0,
   12,    4,   12,
};
short yycheck[] = {                                      25,
    0,    0,   68,   76,   27,   32,  257,    0,  258,   82,
   36,  258,   39,  258,   80,   38,   42,  257,   14,  258,
  257,    4,  258,  257,   15,  258,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   68,   68,
   -1,   -1,   -1,   -1,   -1,   -1,   76,   -1,   -1,   -1,
   80,   80,   82,
};
#define YYFINAL 3
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 258
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'D'",0,0,0,
0,0,0,0,"'L'",0,0,0,"'P'",0,"'R'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"INT_CONST","STR",
};
char *yyrule[] = {
"$accept : proctab",
"proctab : spec_list",
"spec_list : spec",
"spec_list : spec_list spec",
"spec : proc_line states",
"spec : macro_defn",
"proc_line : 'P' procno pathname printname pipe_id flags tflag",
"states : state_line",
"states : states state_line",
"state_line : local_line",
"state_line : remote_line",
"local_line : 'L' stateno flags funcno stateno",
"remote_line : remote_header stateno_list",
"remote_header : 'R' procno flags pipe_id pipe_id flags",
"stateno_list :",
"stateno_list : stateno_list remote_stateno",
"remote_stateno : stateno",
"macro_defn : 'D' macro_id STR",
"procno : INT_CONST",
"stateno : INT_CONST",
"funcno : INT_CONST",
"pathname : STR",
"printname : STR",
"pipe_id : STR",
"tflag : STR",
"flags : INT_CONST",
"macro_id : STR",
};
#endif
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#ifdef YYSTACKSIZE
#ifndef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#endif
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#line 459 "ingres.y"

/*
**  FLAGVAL -- return value of flag
**
**	Parameter:
**		flag -- the name of the flag
**
**	Return:
**		-1 -- flag is de-asserted (-x)
**		0 -- flag is not specified
**		1 -- flag is asserted (+x)
**
**	Requires:
**		Opt -- to scan the flags
**
**	Defines:
**		flagval
**
**	Called by:
**		buildint
**		dolocks
**
**	History:
**		3/27/78 (eric) -- changed to handle EQUEL flag
**			normally.
**		1/4/78 -- written by eric
*/
int
flagval(char flag)
{
	register char	f;
	register char	**p;
	register char	*o;

	f = flag;

	/* start scanning option list */
	for (p = Opt; (o = *p) != 0; p++)
	{
		if (o[1] == f)
			if (o[0] == '+')
				return (1);
			else
				return (-1);
	}
	return (0);
}

/*
**  DOFLAG -- process flag
**
**	Parameters:
**		flag -- the flag (as a string)
**		where -- where it is called from
**			-1 -- internally inserted
**			0 -- on user command line
**			1 -- from users file
**
**	Return:
**		none
**
**	Side effects:
**		All flags are inserted on the end of the
**		"Flaglist" vector for passing to the processes.
**		The "No_exec" flag is set if the flag is bad or you
**		are not authorized to use it.
**
**	Requires:
**		Status -- to get the status bits set for this user.
**		syserr -- for the obvious
**		printf -- to print errors
**		atoi -- to check syntax on numerically-valued flags
**
**	Defines:
**		doflag()
**		Flagok -- a list of legal flags and attributes (for
**			local use only).
**		Relmode -- a list of legal relation modes.
**
**	Called by:
**		main
**
**	History:
**		11/6/79 (6.2/8) (eric) -- -u flag processing dropped,
**			since initucode does it anyhow.  -E flag
**			removed (what is it?).  F_USER code dropped.
**			F_DROP is still around; we may need it some-
**			day.  Also, test of U_SUPER flag and/or DBA
**			status was wrong.
**		7/5/78 (eric) -- NETFLAG added to list.
**		3/27/78 (eric) -- EQUELFLAG added to the list.
**		1/29/78 -- do_u_flag broken off by eric
**		1/4/78 -- written by eric
*/

struct flag {
	char	flagname;	/* name of the flag */
	char	flagstat;	/* status of flag (see below) */
	int	flagsyntx;	/* syntax code for this flag */
	int	flagperm;	/* status bits needed to use this flag */
	char	*flagpt;	/* default proctab to use with this flag */
};

/* status bits for flag */
#define	F_PLOK		01	/* allow +x form */
#define	F_PLD		02	/* defaults to +x */
#define	F_DBA		04	/* must be the DBA to use */
#define	F_DROP		010	/* don't save in Flaglist */

/* syntax codes */
#define	F_ACCPT		1	/* always accept */
#define	F_C_SPEC	3	/* -cN spec */
#define	F_I_SPEC	4	/* -inN spec */
#define	F_F_SPEC	5	/* -fnxN.M spec */
#define	F_CHAR		6	/* single character */
#define	F_MODE		7	/* a modify mode */
#define	F_INTERNAL	8	/* internal flag, e.g., -q */
#define	F_EQUEL		9	/* EQUEL flag */

struct flag	Flagok[] = {
	{ 'a',	F_PLD|F_PLOK,	F_ACCPT,	0,		NULL },
	{ 'b',	F_PLD|F_PLOK,	F_ACCPT,	U_DRCTUPDT,	NULL },
	{ 'c',	0,		F_C_SPEC,	0,		NULL },
	{ 'd',	F_PLD|F_PLOK,	F_ACCPT,	0,		NULL },
	{ 'f',	0,		F_F_SPEC,	0,		NULL },
	{ 'i',	0,		F_I_SPEC,	0,		NULL },
	{ 'l',	F_PLOK,		F_INTERNAL,	0,		NULL },
	{ 'n',	0,		F_MODE,		0,		NULL },
	{ 'q',	F_PLD|F_PLOK,	F_INTERNAL,	0,		NULL },
	{ 'r',	0,		F_MODE,		0,		NULL },
	{ 's',	F_PLD|F_PLOK,	F_ACCPT,	0,		NULL },
	{ 'v',	0,		F_CHAR,		0,		NULL },
	{ 'w',	F_PLOK|F_DROP,	F_ACCPT,	0,		NULL },
	{ 'D',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'L',	F_PLOK,		F_ACCPT,	0,		NULL },
	{ 'M',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'O',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'P',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'Q',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'T',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ 'U',	F_PLOK,		F_ACCPT,	U_UPSYSCAT,	NULL },
	{ 'W',	0,		F_INTERNAL,	0,		NULL },
	{ 'Z',	0,		F_ACCPT,	U_TRACE,	NULL },
	{ EQUELFLAG, 0,		F_EQUEL,	0,		"=equel" },
	{ NETFLAG, 0,		F_EQUEL,	0,		"=slave" },
	{ '@',	0,		F_EQUEL,	0,		NULL },
	{ '*',	0,		F_ACCPT,	0,		NULL },
	{ 0,	0,		0,		0,		NULL }
};

/* list of valid retrieve into or index modes */
char	*Relmode[] = {
	"isam",
	"cisam",
	"hash",
	"chash",
	"heap",
	"cheap",
	"heapsort",
	"cheapsort",
	NULL
};

void
doflag(char *flag, int where)
{
	register char		*p;
	register struct flag	*f;
	int		intxx;
	register char		*ptr;
	int			i;
	int			j;

	p = flag;

	/* check for valid flag format (begin with + or -) */
	if (p[0] != '+' && p[0] != '-')
		goto badflag;

	/* check for flag in table */
	for (f = Flagok; f->flagname != p[1]; f++)
	{
		if (f->flagname == 0)
			goto badflag;
	}

	/* check for +x form allowed */
	if (p[0] == '+' && (f->flagstat & F_PLOK) == 0)
		goto badflag;

	/* check for permission to use the flag */
	if ((f->flagperm != 0 && (getglobalint(STATUS_NAME) & f->flagperm) == 0 &&
	     (((f->flagstat & F_PLD) == 0) ? (p[0] == '+') : (p[0] == '-'))) ||
	    ((f->flagstat & F_DBA) != 0 && (getglobalint(STATUS_NAME) & U_SUPER) == 0 &&
	     !bequal(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE)))
	{
		printf("You are not authorized to use the %s flag\n", p);
		No_exec++;
	}

	/* check syntax */
	switch (f->flagsyntx)
	{
	  case F_ACCPT:
		break;

	  case F_C_SPEC:
		if ((intxx = atoi(&p[2])) > MAX_FIELD_SIZE)
			goto badflag;
		break;

	  case F_I_SPEC:
		if (p[2] != '1' && p[2] != '2' && p[2] != '4')
			goto badflag;
		if ((intxx = atoi(&p[3])) > MAX_FIELD_SIZE)
			goto badflag;
		break;

	  case F_F_SPEC:
		if (p[2] != '4' && p[2] != '8')
			goto badflag;
		switch (p[3])
		{
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
		ptr = &p[4];
		while (*ptr != '.')
			if (*ptr == 0)
				goto badflag;
			else
				ptr++;
		*ptr = 0;
		if ((intxx = atoi(&p[4])) > MAX_FIELD_SIZE)
			goto badflag;
		*ptr++ = '.';
		if ((intxx = atoi(ptr)) > MAX_FIELD_SIZE)
			goto badflag;
		break;

	  case F_CHAR:
		if (p[2] == 0 || p[3] != 0)
			goto badflag;
		break;

	  case F_MODE:
		for (i = 0; (ptr = Relmode[i]) != NULL; i++)
		{
			if (strcmp(&p[2], ptr) == 0)
				break;
		}
		if (ptr == NULL)
			goto badflag;
		break;

	  case F_INTERNAL:
		if (where >= 0) {
			goto badflag;
		}
		break;

	  case F_EQUEL:
		ptr = &p[2];
		for (i = 0; i < 20; i++, ptr++) {
			if (*ptr == CLOSED) {
				continue;
			}
			if (*ptr < 0100) {
				break;
			}
#if 0
			if (*ptr >= 0100 + NOFILE) {
				break;
			}
#endif
			j = (i / 2) + '0';
			if ((i & 01) == 0) {
				Pipe[j].pip_rfd = *ptr & 077;
			} else {
				Pipe[j].pip_wfd = *ptr & 077;
			}
		}
		break;

	  default:
		syserr("doflag: syntx %d", f->flagsyntx);

	}

	/* save flag */
	if (Nopts >= MAXOPTNS)
	{
		printf("Too many options to INGRES\n");
		exit(1);
	}
	if ((f->flagstat & F_DROP) == 0)
		Opt[Nopts++] = p;

	/* change to new process table as appropriate */
	if (f->flagpt != NULL)
		DefProcTab = f->flagpt;

	return;

  badflag:
	printf("Bad flag format: %s\n", p);
	No_exec++;
	return;
}

/*
**  SYSFLAG -- process flags for sysmod
**
**	Parameters:
**		flag -- the flag (as a string)
**		may be one of: -s, -S, -T? 
**
**	Return:
**		none
**
**	Side effects:
**		All flags are inserted on the end of the
**		"Flaglist" vector for passing to the processes.
**		The "No_exec" flag is set if the flag is bad or you
**		are not authorized to use it.
**
**	Called by:
**		main
**
*/
void
sysflag(char *flag)
{
	register char 	*p;

	p = flag;

	if (p[0] != '-')
		goto sysbadflag;

	switch (p[1])
	{
  	  case 's':
		if ((getglobalint(STATUS_NAME) & U_SUPER) == 0) {
			printf("Only INGRES can use the -s flag\n");
			No_exec++;
			return;
		}
		bmove(Admin.ad_h.adm_owner, Usercode, USERCODE_SIZE);
		return;

  	  case 'R':
	  case 'S':
		break;
	
	  default:
		goto sysbadflag;
	}

	if (!bequal(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE))
	{
		printf("You are not the dba for %s\n", Database);
		No_exec++;
		return;
	}


	/* save flag */
	if (Nopts >= SYSMAXOPTNS)
	{
		printf("Too many options to SYSMOD\n");
		exit(1);
	}

	Opt[Nopts++] = p;


	return;


    sysbadflag:
	printf("Sysmod: bad flag format: %s \n", p);
	No_exec++;

}



/*
**  Process rubouts (just exit)
*/
void
rubproc(void)
{
	exit(2);
}

/*
**  DOLOCKS -- set database lock
**
**	A lock is set on the database.
*/

void
dolocks(void)
{
	db_lock(flagval('E') > 0 ? M_EXCL : M_SHARE);
}

/*
**  PIPEOPEN -- open pipe if necessary.
*/
void
pipeopen(char pipid, int rw)
{
	register struct pipeinfo *pi;
	int pipex[2];

	if (pipid == '\0')
		return;

	pi = &Pipe[(int) pipid];

	if ((rw ? pi->pip_wfd : pi->pip_rfd) >= 0)
		return;
	if (pi->pip_rfd >= 0 || pi->pip_wfd >= 0)
		syserr("pipeopen %o %d: rfd=%d, wfd=%d", pipid, rw, pi->pip_rfd, pi->pip_wfd);
	if (pipid == CLOSED)
		pi->pip_rfd = pi->pip_wfd = CLOSED;
	else
	{
		if (pipe(pipex) < 0)
			syserr("pipeopen: pipe");
		pi->pip_rfd = pipex[0];
		pi->pip_wfd = pipex[1];
	}
}

/*
**  INGEXEC -- execute INGRES process
**
**	This routine handles all the setup of the argument vector
**	and then executes a process.
**
**	Parameters:
**		process -- a pointer to the process table entry which
**			describes this process.
**
**	Returns:
**		never
**
**	Side Effects:
**		never returns, but starts up a new overlay.  Notice
**			that it does NOT fork.
**
**	Requires:
**		none
**
**	Called By:
**		satisfypt
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		none
**
**	Syserrs:
**		chdir %s -- could not change directory into the data-
**			base.
**		creat %s -- could not create the redirected standard
**			output file.
**		%s not executable -- could not execute the process.
**
**	History:
**		8/9/78 (eric) -- changed "prparam" to be a colon-
**			separated list of parameters (so the number
**			is variable); also, moved parameter expansion
**			into this routine from buildint() so that
**			the colons in the dbu part of the proctab
**			would not confuse things.
**		7/24/78 (eric) -- changed the technique of closing
**			files 0 & 2 so that they will never be closed
**			(even if requested in the status field)
**			if they are mentioned in the pipe vector.
**			Also, some fiddling is done to handle the
**			0100 bit on file descriptors correctly.
*/
void
ingexec(int procno)
{
	char			*vect[30];
	register char		**v;
	char			**opt;
	int			i;
	register struct proc	*pr;
	register proc_t 	*pp;
	char			closeit[NOFILE];
	char			fdbuf[3];

	v = vect;
	pr = &ProcTab[procno];

	*v++ = pr->prpath;
	fdbuf[0] = pr->prcm.cm_rinput | 0100;
	fdbuf[1] = pr->prtflag;
	fdbuf[2] = '\0';
	*v++ = fdbuf;
	*v++ = Fileset;
	*v++ = Usercode;
	*v++ = Database;
	*v++ = Pathname;

	/* insert flag parameters */
	for (opt = Opt; *opt; opt++)
		*v++ = *opt;
	*v = 0;

	/* set up 'closeit' to tell which pipes to close */
	for (i = 0; i < NOFILE; i++)
		closeit[i] = TRUE;
	closeit[pr->prmpipe & 077] = FALSE;
	closeit[pr->prcm.cm_input & 077] = FALSE;
	closeit[pr->prcm.cm_rinput & 077] = FALSE;
	for (i = 0; i < CM_MAXPROC; i++)
	{
		pp = &pr->prcm.cm_proc[i];
		if (pp->pr_ninput != CLOSED)
			closeit[pp->pr_ninput & 077] = FALSE;
		if (pp->pr_file != CLOSED)
			closeit[pp->pr_file & 077] = FALSE;
	}
	closeit[1] = FALSE;
	if ((pr->prstat & PR_CLSSIN) == 0)
		closeit[0] = FALSE;
	if ((pr->prstat & PR_CLSDOUT) == 0)
		closeit[2] = FALSE;

	/* close extra pipes (those not used by this process) */
	for (i = 0; i < NOFILE; i++)
	{
		if (closeit[i])
			close(i);
	}

	/* change to the correct directory */
	if ((pr->prstat & PR_NOCHDIR) == 0)
	{
		if (chdir(Dbpath))
			syserr("ingexec: chdir %s", Dbpath);
	}

	/* change to normal userid/groupid if a non-dangerous process */
	if ((pr->prstat & PR_REALUID) != 0)
	{
		setuid(getuid());
		setgid(getgid());
	}

#ifdef LEAVEOUT
	/* change standard output if specified in proctab */
	p = pr->prstdout;
	if (*p != 0) {
		/* chew up fd 0 (just in case) */
		outfd = dup(1);
		close(1);
		if (open(p, O_CREAT | O_TRUNC | O_WRONLY, 0666) != 1) {
			/* restore standard output and print error */
			close(1);
			dup(outfd);	/* better go into slot 1 */
			syserr("ingexec: creat %s", p);
		}
		close(outfd);
	}
#endif LEAVEOUT

	/*
	**  PLEASE NOTE THE TRICKERY USED HERE.
	**	In this code I depend on UNIX buffering pipes at least
	**	enough to handle one "CM" struct.  If not, the following
	**	write will hang before the exec will call the process
	**	that will read it.
	**
	**	The "correct" way to do this is to fork & have the
	**	parent write the CM struct.  But how do I handle the
	**	last one (that does not fork)?  I could also do an
	**	extra fork of a process to do the write.  But some
	**	systems have a limit on processes, and besides, it
	**	seems like a lot of overhead for such a little thing.
	**
	**	Perhaps I should encode the CM struct into argv
	**	instead & do it "right".
	*/

	/* output the control structure to the awaiting process... */
	write(pr->prmpipe & 077, &pr->prcm, sizeof(pr->prcm));
	close(pr->prmpipe & 077);

	/* give it the old college (or in this case, University) try */
	execv(vect[0], vect);
	syserr("\"%s\" not executable", vect[0]);
}


void
pipexlat(char *ppip, int rw)
{
	register struct pipeinfo *pi;
	int cnt;
	int fd;

	if (*ppip == '\0' || *ppip == CLOSED)
		return;
	pi = &Pipe[(int) *ppip];

	if (rw)
	{
		cnt = --(pi->pip_wcnt);
		fd = pi->pip_wfd;
	}
	else
	{
		cnt = --(pi->pip_rcnt);
		fd = pi->pip_rfd;
	}

	if (cnt < 0)
		syserr("pipexlat: cnt=%d: %o %d", cnt, *ppip, rw);
	if (fd < 0 || fd > NOFILE)
		syserr("pipexlat: fd=%d: %o %d", fd, *ppip, rw);

	*ppip = fd;
}

/*
**  SATISFYPT -- satisfy the process table
**
**	Well folks, now that you've read this far, this is it!!!  I
**	mean, this one really does it!!!  It takes the internal form
**	built by the parser and creates pipes as necessary, forks, and
**	execs the INGRES processes.  Isn't that neat?
**
**	Parameters:
**		none
**
**	Returns:
**		never
**
**	Requires:
**		Proctab -- the internal form
**		ingexec -- to actually exec the process
**		pipe -- to create the pipe
**		syserr -- for the obvious
**		fillpipe -- to extend a newly opened pipe through all
**			further references to it.
**		checkpipes -- to see if a given pipe will ever be
**			referenced again.
**		fork -- to create a new process
**
**	Defines:
**		satisfypt
**
**	Called by:
**		main
**
**	History:
**		3/14/80 (eric) -- changed for version 7.0.
**		7/24/78 (eric) -- Actual file descriptors stored in
**			'prpipes' are changed to have the 0100 bit
**			set internally (as well as externally), so
**			fd 0 will work correctly.
**		1/4/78 -- written by eric
*/


void
satisfypt(int callsysmod)
{
	register struct proc	*pr;
	register proc_t  	*pp;
	register int		i;
	int			procno;

	/* scan the process table */
	for (procno = CM_MAXPROC - 1; procno >= 0; procno--)
	{

		pr = &ProcTab[procno];
		if (pr->prpath[0] == '\0')
			continue;

		/* scan pipe vector, creating new pipes as needed */
		pipeopen(pr->prmpipe, TRUE);
		pipeopen(pr->prcm.cm_input, FALSE);
		pipeopen(pr->prcm.cm_rinput, FALSE);
		for (i = 0; i < CM_MAXPROC; i++)
		{
			pp = &pr->prcm.cm_proc[i];
			pipeopen(pp->pr_file, TRUE);
			pipeopen(pp->pr_ninput, FALSE);
		}

		/* substitute real file descriptors throughout */
		pipexlat(&pr->prmpipe, TRUE);
		pipexlat(&pr->prcm.cm_input, FALSE);
		pipexlat(&pr->prcm.cm_rinput, FALSE);
		for (i = 0; i < CM_MAXPROC; i++)
		{
			pp = &pr->prcm.cm_proc[i];
			pipexlat(&pp->pr_file, TRUE);
			pipexlat(&pp->pr_ninput, FALSE);
		}

		/* fork if necessary */
		if (--NumProcs <= 0  || (i = fork()) == 0 )
		{
			/* child!! */
			ingexec(procno);
		}

		/* parent */
		if (i < 0)
			syserr("satisfypt: fork");

		/* scan pipes.  close all not used in the future */
		for (i = 0; i < 128; i++)
		{
			if (i == CLOSED)
				continue;
			if (Pipe[i].pip_rcnt <= 0 && Pipe[i].pip_rfd >= 0)
			{
				if (close(Pipe[i].pip_rfd) < 0)
					syserr("satisfypt: close-r(%d)", Pipe[i].pip_rfd);
				Pipe[i].pip_rfd = -1;
			}
			if (Pipe[i].pip_wcnt <= 0 && Pipe[i].pip_wfd >= 0)
			{
				if (close(Pipe[i].pip_wfd) < 0)
					syserr("satisfypt: close-w(%d)", Pipe[i].pip_wfd);
				Pipe[i].pip_wfd = -1;
			}
		}
	}
	syserr("satisfypt: fell out");
}


/*
**  CHECKPIPES -- check for pipe referenced in the future
**
**	Parameters:
**		proc -- point in the process table to start looking
**			from.
**		fd -- the file descriptor to look for.
**
**	Return:
**		zero -- it will be referenced in the future.
**		one -- it is never again referenced.
**
**	Requires:
**		nothing
**
**	Defines:
**		checkpipes
**
**	Called by:
**		satisfypt
**
**	History:
**		7/24/78 (eric) -- 0100 bit on file descriptors handled.
**		1/4/78 -- written by eric
*/
int
checkpipes(struct proc *proc, register int fd)
{
	register struct proc	*pr;
	register proc_t 	*pp;
	register int		i;

	for (pr = proc; pr < &ProcTab[CM_MAXPROC]; pr++)
	{
		if (pr->prpath[0] == '\0')
			continue;
		for (i = 0; i < CM_MAXPROC; i++)
		{
			pp = &pr->prcm.cm_proc[i];
			if (pp->pr_file == fd || pp->pr_ninput == fd)
				return (0);
		}
	}
	return (1);
}

/*
**  YYLEX -- Return next token from proctab
**
**	Parameters:
**		none
**
**	Returns:
**		Next token
**
**	Side Effects:
**		Input from proctab
*/

int
yylex(void)
{
	static int state;
	static char *ptp;
	int ix;
	static char line[MAX_LINE_SIZE];
	register int c;
	register char *p;

	switch (state)
	{
	  case EOFSTATE:
		return (0);

	  case BOLSTATE:
		ptp = line;
		for (;;)
		{
			LineNo++;
			c = getc(ProcFile);
			if (c < 0)
			{
				state = EOFSTATE;
				return (0);
			}
			switch (c)
			{
			  case '*':
			  case '#':
			  case '\n':
				while (c != '\n' && (c = getc(ProcFile)) > 0)
					continue;
				break;

			  case ':':
				while (c != '\n' && (c = getc(ProcFile)) > 0)
					putchar(c);
				break;

			  default:
				/* regular line, return header */
				state = NORMSTATE;
				return (c);
			}
		}
	
	  case NORMSTATE:
		yylval.yystr = ptp;
		while ((c = getc(ProcFile)) != ':' && c != '\n' && c > 0)
		{
			*ptp++ = c;
			if (c == '$')
			{
				c = getc(ProcFile);
				if (c < 'A' || c > 'Z')
					*ptp++ = c;
				else
				{
					ptp--;
					for (p = Macro[c - 'A']; (*ptp++ = *p++) != '\0'; )
						continue;
					ptp--;
				}
			}
		}
		
		/* compute next state */
		if (c != ':')
			state = BOLSTATE;

		*ptp++ = '\0';
		ix = atoi(yylval.yystr);
		if ( *yylval.yystr <= '9' && *yylval.yystr >= '0')
		{
			if (yylval.yystr[0] == '0')
				ix = oatoi(yylval.yystr);
			yylval.yyint = ix;
			return (INT_CONST);
		}
		else
			return (STR);
	
	  default:
		syserr("yylex: state %d", state);
	}
	return(0);
}

void
yyerror(char *s)
{
	syserr("Line %d: Yacc error: %s", LineNo, s);
}

void
main(int argc, char **argv)
{
	register int	i;
	register int	j;
	char		*proctab;
	register char	*p;
	char		*ptr;
	char		*uservect[4];
	char		buf[MAX_LINE_SIZE+1];
	char		str[MAX_LINE_SIZE+1];	/* a string to put the Alockdes value into */
	char		str2[MAX_LINE_SIZE+1];
	extern	int	Wait_action;	/* action on lock driver */
	int		CallSysmod = FALSE;
	int		len;
	char		*sysptr;

	/*
	** sysmod is linked to ingres.
	** check whether ingres or sysmod was called.
	*/
	len =  strlen(argv[0]);
	sysptr = &argv[0][len - 6];
	if (strcmp( sysptr, "sysmod") == 0) {
		CallSysmod = TRUE;
		setprocname("SYSMOD");
	} else {
		setprocname("INGRES");
	}

	itoa(getpid(), Fileset);
	proctab = NULL;
	Database = NULL;

	/*
	**  Initialize everything, like Flagvect, Usercode,
	**	etc.
	*/

	for (i = 0; i < 128; i++)
		Pipe[i].pip_rfd = Pipe[i].pip_wfd = -1;

	i = initucode(argc, argv, TRUE, uservect, CallSysmod?M_EXCL:-1);
	switch (i) {
	  case 0:	/* ok */
	  case INDIRECT:
		break;

	  case NODB:	/* database does not exist */
	  case INDNODB:
		printf("Database %s does not exist\n", getparmvect(0));
		if (CallSysmod)
			goto sysusage;
		else
			goto usage;

	  case NOACCESS:	/* you are not authorized */
		printf("You may not access database %s\n", Database);
		if (CallSysmod)
			goto sysusage;
		else
			goto usage;

	  case INVALIDUSR:	/* not a valid user */
		printf("You are not a valid INGRES user\n");
		if (CallSysmod)
			goto sysusage;
		else
			goto usage;

	  case NODBNAME:	/* no database name specified */
		printf("No database name specified\n");
		if (CallSysmod)
			goto sysusage;
		else
			goto usage;

	  default:
		syserr("initucode %d", i);
	}

	/*
	**  Extract database name and process table name from
	**	parameter vector.
	**  Initialize the $P macro.
	*/

	Database = getparmvect(0);
	proctab = getparmvect(1);
	smove(Pathname, Macro['P' - 'A']);

	if (!CallSysmod)
	{
		/* scan flags in users file */
		for (p = uservect[0]; *p != '\0'; p++)
		{
			/* skip initial blanks and tabs */
			if (*p == ' ' || *p == '\t')
				continue;
			ptr = p;

			/* find end of flag and null-terminate it */
			while (*p != '\0' && *p != ' ' && *p != '\t')
			p++;
			i = *p;
			*p = '\0';

			/* process the flag */
			doflag(ptr, 1);
			if (i == '\0')
				break;
		}

		/* scan flags on command line */
		for (i = 0; (p = getflagvect(i)) != (char *) NULL; i++)
			doflag(p, 0);

		/* check for query modification specified for this database */
		if ((Admin.ad_h.adm_flags & A_QRYMOD) == 0)
			doflag("-q", -1);
	}
	/* special routine for handling sysmod flags */
	else		
	{
		DefProcTab = "=sysmod";
		if ( getflagvect(0) == NULL) {
			if (!bequal(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE)) {
				printf("You are not the dba for %s\n", Database);
				No_exec++;
			}
		}
		for (i = 0 ; (p = getflagvect(i)) != (char *) NULL ; i++) {
			sysflag(p);
		}
	}


	/* close any extraneous files, being careful not to close anything we need */
	for (i = 3; i < NOFILE; i++) {
		for (j = '0'; j <= '9'; j++) {
			if (Pipe[j].pip_wfd == i || Pipe[j].pip_rfd == i)
				break;
		}
		if (j > '9')
			close(i);
	}

	/* determine process table */
	if (proctab == NULL)
	{
		/* use default proctab */
		if (DefProcTab == NULL) {
			if (argv[0][strlen(argv[0]) - 1] == 'x')
				DefProcTab = "=procx";
			else
				DefProcTab = "=proctab";
			proctab = uservect[1];
		}
		if (proctab == NULL || proctab[0] == 0) {
			/* no proctab in users file */
			concat(DefProcTab, MAJOR_VERSION, buf);
			proctab = buf;
		}
	} else {
		/* proctab specified; check permissions */
		if ((getglobalint(STATUS_NAME) & (proctab[0] == '=' ? U_EPROCTAB : U_APROCTAB)) == 0) {
			printf("You may not specify this process table\n");
			if (CallSysmod)
				goto sysusage;
			else
				goto usage;
		}
	}

	/* expand process table name */
	if (proctab[0] == '=') {
		smove(ztack(ztack(Pathname, "/files/"), &proctab[1]), buf);
		proctab = buf;
	}

	/* open and read the process table */
	if ((ProcFile = fopen(proctab, "r")) == NULL) {
		printf("Proctab %s: %s\n", proctab, strerror(errno));
		if (CallSysmod)
			goto sysusage;
		else
			goto usage;
	}

	/* build internal form of the process table */
	if (yyparse())
		No_exec++;

	/* don't bother executing if we have found errors */
	if (No_exec) {
		if (!CallSysmod) {
	  	usage:
			/* cat .../files/usage */
			cat(ztack(Pathname, "/files/usage"));
			exit(1);
		} else {
		sysusage:
			cat(ztack(Pathname, "/files/sysusage"));
			exit(1);
		}
	}

	fclose(ProcFile);

	/* set locks on the database */
	if (!CallSysmod) {
		dolocks();
		if ( Alockdes >= 0 ) {
			sprintf(str,"-l%d",(flagval('E') >0 ?M_EXCL : M_SHARE));
			doflag(str,-1);
			sprintf(str2,"-W%d",Wait_action);
			doflag(str2,-1);
			if(Alockdes >= 3)      /*  added by  K.Okamoto   */
				close(Alockdes);
		}
	}

	/* satisfy process table (never returns) */
	satisfypt(CallSysmod);
}
#line 1542 "y.tab.c"
#define YYABORT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse()
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, reading %d (%s)\n", yystate,
                    yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: state %d, shifting to state %d\n",
                    yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#ifdef lint
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: state %d, error recovery shifting\
 to state %d\n", *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: error recovery discarding state %d\n",
                            *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, error recovery discards token %d (%s)\n",
                    yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("yydebug: state %d, reducing by rule %d (%s)\n",
                yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 6:
#line 307 "ingres.y"
{
			NumProcs++;
			Proc = &ProcTab[yyvsp[-5].yyint];
			smove(yyvsp[-4].yystr, Proc->prpath);
			Proc->prmpipe = Proc->prcm.cm_input = Proc->prcm.cm_rinput
			    = Proc->prcm.cm_proc[yyvsp[-5].yyint].pr_ninput = yyvsp[-2].yypip;
			smove(yyvsp[-3].yystr, Proc->prcm.cm_myname);
			Proc->prcm.cm_myproc = yyvsp[-5].yyint;
			Proc->prstat = yyvsp[-1].yyint;
			Proc->prtflag = yyvsp[0].yychar;
			Pipe[(int) yyvsp[-2].yypip].pip_rcnt += 3;
			Pipe[(int) yyvsp[-2].yypip].pip_wcnt++;
		}
break;
case 11:
#line 331 "ingres.y"
{
			StateP = &Proc->prcm.cm_state[yyvsp[-3].yyint];
			StateP->st_type = ST_LOCAL;
			StateP->st_stat = yyvsp[-2].yyint;
			StateP->st_v.st_loc.st_funcno = yyvsp[-1].yyint;
			StateP->st_v.st_loc.st_next = yyvsp[0].yyint;
		}
break;
case 13:
#line 344 "ingres.y"
{
			ProcNo = yyvsp[-4].yyint;
			ProcP = &Proc->prcm.cm_proc[ProcNo];
			RemStat = yyvsp[-3].yyint;
			ProcP->pr_file = yyvsp[-2].yypip;
			ProcP->pr_ninput = yyvsp[-1].yypip;
			ProcP->pr_stat = yyvsp[0].yyint;
			Pipe[(int) yyvsp[-2].yypip].pip_wcnt++;
			Pipe[(int) yyvsp[-1].yypip].pip_rcnt++;
		}
break;
case 16:
#line 361 "ingres.y"
{
			StateP = &Proc->prcm.cm_state[yyvsp[0].yyint];
			StateP->st_type = ST_REMOT;
			StateP->st_stat = RemStat;
			StateP->st_v.st_rem.st_proc = ProcNo;
		}
break;
case 17:
#line 374 "ingres.y"
{
			smove(yyvsp[0].yystr, Macro[yyvsp[-1].yychar - 'A']);
		}
break;
case 18:
#line 383 "ingres.y"
{
			if (yyvsp[0].yyint < 0 || yyvsp[0].yyint >= CM_MAXPROC)
			{
				usrerr("Illegal proc number %d", yyvsp[0].yyint);
				YYERROR;
			}
		}
break;
case 19:
#line 393 "ingres.y"
{
			if (yyvsp[0].yyint < 0 || yyvsp[0].yyint >= CM_MAXST)
			{
				usrerr("Illegal state number %d", yyvsp[0].yyint);
				YYERROR;
			}
		}
break;
case 20:
#line 403 "ingres.y"
{
			if (yyvsp[0].yyint < 0)
			{
				usrerr("Illegal funcno %d", yyvsp[0].yyint);
				YYERROR;
			}
		}
break;
case 23:
#line 419 "ingres.y"
{
			if ((islower(yyvsp[0].yystr[0]) || yyvsp[0].yystr[0] == CLOSED) && yyvsp[0].yystr[1] == '\0')
				yyval.yypip = yyvsp[0].yystr[0];
			else if (yyvsp[0].yystr[0] == '|' && isdigit(yyvsp[0].yystr[1]) && yyvsp[0].yystr[2] == '\0')
				yyval.yypip = yyvsp[0].yystr[1];
			else
			{
				usrerr("Invalid pipe id \"%s\"", yyvsp[0].yystr);
				YYERROR;
			}
		}
break;
case 24:
#line 433 "ingres.y"
{
			if (yyvsp[0].yystr[1] != '\0')
			{
				usrerr("Invalid trace flag \"%s\"", yyvsp[0].yystr);
				YYERROR;
			}
			else
				yyval.yychar = yyvsp[0].yystr[0];
		}
break;
case 26:
#line 448 "ingres.y"
{
			/* XXX - ctype */
			if (yyvsp[0].yystr[0] < 'A' || yyvsp[0].yystr[0] > 'Z' || yyvsp[0].yystr[1] != '\0') {
				usrerr("Invalid macro name \"%s\"", yyvsp[0].yystr);
				YYERROR;
			} else {
				yyval.yychar = yyvsp[0].yystr[0];
			}
		}
break;
#line 1804 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: after reduction, shifting from state 0 to\
 state %d\n", YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("yydebug: state %d, reading %d (%s)\n",
                        YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("yydebug: after reduction, shifting from state %d \
to state %d\n", *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}

/*	EVAL.C:	Expression evaluation functions for
		MicroEMACS

	written 1986 by Daniel Lawrence
 *
 * $Log: eval.c,v $
 * Revision 1.1  1994/02/01 03:29:19  jkh
 * Initial revision
 *
 * Revision 1.67  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.66  1993/09/06  16:36:47  pgf
 * changed glob() to doglob() to avoid symbol conflicts
 *
 * Revision 1.65  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.64  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.63  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.62  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.61  1993/07/20  18:08:18  pgf
 * add pyr and AIX to the list of non-constant case identifer machines
 *
 * Revision 1.60  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.59  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.58  1993/06/28  20:10:27  pgf
 * new variable
 *
 * Revision 1.57  1993/06/24  18:02:13  pgf
 * added AUX as system needing to use if/else instead of switch
 *
 * Revision 1.56  1993/06/18  16:20:37  pgf
 * tom's 3.49 changes
 *
 * Revision 1.55  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.54  1993/05/24  15:25:41  pgf
 * tom's 3.47 changes, part b
 *
 * Revision 1.53  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.52  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.51  1993/04/21  15:43:05  pgf
 * folded long line
 *
 * Revision 1.50  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.49  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.48  1993/03/29  11:18:15  pgf
 * $pagewid was returning length instead of width
 *
 * Revision 1.47  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.46  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.45  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.44  1993/02/16  20:52:49  pgf
 * eliminate putenv call, which isn't always available.  real vi doesn't
 * push the "shell" variable to the environment anyway.
 *
 * Revision 1.43  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.42  1993/01/23  13:38:23  foxharp
 * now include nevars.h, which is auto-created by mktbls
 *
 * Revision 1.41  1993/01/16  10:30:58  foxharp
 * makevarslist() and listvars()
 *
 * Revision 1.40  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.39  1992/11/19  09:03:56  foxharp
 * fix identification of single character tokens that happen to be prefixes
 * for special constructs, like '~', '*', etc.
 *
 * Revision 1.38  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.37  1992/07/24  18:22:51  foxharp
 * deleted local atoi() routine -- now we use the system's copy
 *
 * Revision 1.36  1992/07/13  20:09:57  foxharp
 * "terse" is no longer a variable
 *
 * Revision 1.35  1992/07/13  09:26:16  foxharp
 * added "force" argument to current_directory()
 *
 * Revision 1.34  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.33  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.32  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.30  1992/04/10  18:48:17  pgf
 * change abs to absol to get rid of name conflicts
 *
 * Revision 1.29  1992/03/24  09:02:18  pgf
 * need to glob() filenames for &rd and &wr
 *
 * Revision 1.28  1992/03/24  07:36:23  pgf
 * added &rd and &wr functions, for file access, and fixed off-by-one in $curcol
 *
 * Revision 1.27  1992/03/19  23:19:55  pgf
 * linux prototyped portability
 *
 * Revision 1.26  1992/03/19  23:07:47  pgf
 * variable access cleanup/rearrangement
 *
 * Revision 1.25  1992/03/05  09:17:21  pgf
 * added support for new "terse" variable, to control unnecessary messages
 *
 * Revision 1.24  1992/03/03  09:35:52  pgf
 * added support for getting "words" out of the buffer via variables --
 * needed _nonspace character type
 *
 * Revision 1.23  1992/02/17  09:01:16  pgf
 * took out unused vars for saber
 *
 * Revision 1.22  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.21  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed sting, to avoid length limits
 *
 * Revision 1.20  1991/12/24  09:18:47  pgf
 * added current/change directory support  (Dave Lemke's changes)
 *
 * Revision 1.19  1991/11/27  10:09:09  pgf
 * bug fix, from pete
 *
 * Revision 1.18  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.17  1991/11/08  13:19:01  pgf
 * lint cleanup
 *
 * Revision 1.16  1991/11/04  14:18:09  pgf
 * use lsprintf in itoa
 *
 * Revision 1.15  1991/11/03  17:36:13  pgf
 * picky saber change
 *
 * Revision 1.14  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.13  1991/10/28  14:25:06  pgf
 * eliminated some variables that are now buffer-values
 *
 * Revision 1.12  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.11  1991/10/22  14:10:07  pgf
 * more portable #if --> #ifdef
 *
 * Revision 1.10  1991/09/26  13:12:04  pgf
 * new arg. to kbd_string to enable backslash processing
 *
 * Revision 1.9  1991/09/16  23:46:55  pgf
 * more hardening
 *
 * Revision 1.8  1991/09/13  03:27:06  pgf
 * attempt to harden against bad variable names (like lone %)
 *
 * Revision 1.7  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.6
 * date: 1991/08/06 15:13:27;
 * global/local values
 * 
 * revision 1.5
 * date: 1991/06/25 19:52:23;
 * massive data structure restructure
 * 
 * revision 1.4
 * date: 1991/06/03 10:19:11;
 * newscreensize() is now named newlength()
 * 
 * revision 1.3
 * date: 1990/10/01 11:05:54;
 * progname --> prognam
 * 
 * revision 1.2
 * date: 1990/09/25 11:38:13;
 * took out old ifdef BEFORE code
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:11;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"
#include	"nevars.h"
#include	"glob.h"

#define	FUNC_NAMELEN	4

#define	ILLEGAL_NUM	-1
#define	MODE_NUM	-2
#define	USER_NUM	-3

/* When the command interpretor needs to get a variable's name, rather than its
 * value, it is passed back as a VDESC variable description structure.  The
 * v_num field is an index into the appropriate variable table.
 */

	/* macros for environment-variable switch */
	/*  (if your compiler balks with "non-constant case expression" */
#if VMS || AUX2 || pyr || AIX
#define	If(N)		if (vnum == N) {
#define	ElseIf(N)	} else If(N)
#define	Otherwise	} else {
#define	EndIf		}
#else			/* more compact if it works... */
#define	If(N)		switch (vnum) { case N: {
#define	ElseIf(N)	break; } case N: {
#define	Otherwise	break; } default: {
#define	EndIf		}}
#endif

#if OPT_EVAL
typedef struct	{
	int	v_type;	/* type of variable */
	int	v_num;	/* index, if it's an environment-variable */
	UVAR *	v_ptr;	/* pointer, if it's a user-variable */
	} VDESC;

#if ENVFUNC
static	char *	GetEnv P(( char * ));
static	char *	DftEnv P(( char *, char * ));
static	void	SetEnv P(( char **, char * ));
#endif
#if !SMALLER
static	void	makevarslist P(( int, char *));
static	int	is_mode_name P(( char *, int, VALARGS * ));
static	char *	get_listvalue P(( char *, int ));
#endif
static	int	gtlbl P(( char * ));
static	char *	gtfun P(( char * ));
static	void	FindVar P(( char *, VDESC * ));
static	int	vars_complete P(( int, char *, int * ));
static	int	PromptAndSet P(( char *, int, int ));
static	int	SetVarValue P(( VDESC *, char * ));
static	char *	getkill P(( void ));
#endif

/*--------------------------------------------------------------------------*/

#if	ENVFUNC && OPT_EVAL
static char *
GetEnv(s)
	char	*s;
{
	register char *v = getenv(s);
	return v ? v : "";
}

static char *
DftEnv(name, dft)
register char	*name;
register char	*dft;
{
	name = GetEnv(name);
	return (name == 0) ? dft : name;
}

static void
SetEnv(namep, value)
char	**namep;
char	*value;
{
	FreeIfNeeded(*namep);
	*namep = strmalloc(value);
}
#else
#define	GetEnv(s)	""
#define	DftEnv(s,d)	d
#define	SetEnv(np,s)	(*(np) = strmalloc(s))
#endif

#if OPT_EVAL
static char *shell;	/* $SHELL environment is "$shell" variable */
static char *directory;	/* $TMP environment is "$directory" variable */
#endif

#if ! SMALLER
/* list the current vars into the current buffer */
/* ARGSUSED */
static void
makevarslist(dum1,ptr)
int dum1;
char *ptr;
{
	register UVAR *p;
	register int j;

	bprintf("--- Environment variables %*P\n", term.t_ncol-1, '-');
	bprintf("%s", ptr);
	for (p = user_vars, j = 0; p != 0; p = p->next) {
		if (!j++)
			bprintf("--- User variables %*P", term.t_ncol-1, '-');
		bprintf("\n%%%s = %s", p->u_name, p->u_value);
	}
}

/* Test a name to ensure that it is a mode-name, filtering out modish-stuff
 * such as "all" and "noview"
 */
static int
is_mode_name(name, showall, args)
char *name;
int showall;
VALARGS	*args;
{

	if (strncmp(name, "no", 2)
	 && strcmp(name, "all")) {
		if (find_mode(name, -TRUE, args)) {
			if (!showall || !strcmp(name, args->names->shortname))
				return FALSE;
			return TRUE;
		}
		return SORTOFTRUE;
	}
	return FALSE;
}

/* filter out mode-names that apply only to abbreviations */
static char *
get_listvalue(name, showall)
char *name;
int showall;
{
	VALARGS args;
	register int	s;

	if ((s = is_mode_name(name, showall, &args)) == TRUE)
		return string_mode_val(&args);
	else if (s == SORTOFTRUE)
		return gtenv(name);
	return 0;
}
#endif /* !SMALLER */

/* ARGSUSED */
#if !SMALLER
int
listvars(f, n)
int f,n;
{
	char *values;
	register WINDOW *wp = curwp;
	register int s;
	register ALLOC_T t;
	register char *v, *vv;
	static	char *fmt = "$%s = %*S\n";
	char	**Names = f ? all_modes : envars;
	int	showall = f ? (n > 1) : FALSE;

	/* collect data for environment-variables, since some depend on window */
	for (s = t = 0; Names[s] != 0; s++) {
		if ((vv = get_listvalue(Names[s], showall)) != 0)
			t += strlen(Names[s]) + strlen(fmt) + strlen(vv);
	}
	if (!(values = malloc(t)))
		return FALSE;

	for (s = 0, v = values; Names[s] != 0; s++) {
		if ((vv = get_listvalue(Names[s], showall)) != 0) {
			t = strlen(vv);
			if (t == 0) {
				t = 1;
				vv = "";
			} else if (vv[t-1] == '\n')
				t--;	/* suppress trailing newline */
			v = lsprintf(v, fmt, Names[s], t, vv);
		}
	}
	s = liststuff(ScratchName(Variables), makevarslist, 0, (char *)values);
	free(values);

	/* back to the buffer whose modes we just listed */
	swbuffer(wp->w_bufp);
	return s;
}
#endif /* !SMALLER */

#if OPT_EVAL
static
char *gtfun(fname)	/* evaluate a function */
char *fname;		/* name of function to evaluate */
{
	register int fnum;		/* index to function to eval */
	char arg1[NSTRING];		/* value of first argument */
	char arg2[NSTRING];		/* value of second argument */
	char arg3[NSTRING];		/* value of third argument */
	static char result[2 * NSTRING];	/* string result */

	if (!fname[0])
		return(errorm);

	/* look the function up in the function table */
	mklower(fname)[FUNC_NAMELEN-1] = EOS; /* case-independent */
	for (fnum = 0; fnum < NFUNCS; fnum++)
		if (strcmp(fname, funcs[fnum].f_name) == 0)
			break;

	/* return errorm on a bad reference */
	if (fnum == NFUNCS)
		return(errorm);

	/* if needed, retrieve the first argument */
	if (funcs[fnum].f_type >= MONAMIC) {
		if (macarg(arg1) != TRUE)
			return(errorm);

		/* if needed, retrieve the second argument */
		if (funcs[fnum].f_type >= DYNAMIC) {
			if (macarg(arg2) != TRUE)
				return(errorm);

			/* if needed, retrieve the third argument */
			if (funcs[fnum].f_type >= TRINAMIC)
				if (macarg(arg3) != TRUE)
					return(errorm);
		}
	}

	/* and now evaluate it! */
	switch (fnum) {
		case UFADD:	return(l_itoa(atoi(arg1) + atoi(arg2)));
		case UFSUB:	return(l_itoa(atoi(arg1) - atoi(arg2)));
		case UFTIMES:	return(l_itoa(atoi(arg1) * atoi(arg2)));
		case UFDIV:	return(l_itoa(atoi(arg1) / atoi(arg2)));
		case UFMOD:	return(l_itoa(atoi(arg1) % atoi(arg2)));
		case UFNEG:	return(l_itoa(-atoi(arg1)));
		case UFCAT:	return(strcat(strcpy(result, arg1), arg2));
		case UFLEFT:	return(strncpy(result, arg1, atoi(arg2)));
		case UFRIGHT:	return(strcpy(result, &arg1[atoi(arg2)-1]));
		case UFMID:	return(strncpy(result, &arg1[atoi(arg2)-1],
					atoi(arg3)));
		case UFNOT:	return(ltos(stol(arg1) == FALSE));
		case UFEQUAL:	return(ltos(atoi(arg1) == atoi(arg2)));
		case UFLESS:	return(ltos(atoi(arg1) < atoi(arg2)));
		case UFGREATER:	return(ltos(atoi(arg1) > atoi(arg2)));
		case UFSEQUAL:	return(ltos(strcmp(arg1, arg2) == 0));
		case UFSLESS:	return(ltos(strcmp(arg1, arg2) < 0));
		case UFSGREAT:	return(ltos(strcmp(arg1, arg2) > 0));
		case UFIND:	return(tokval(arg1));
		case UFAND:	return(ltos(stol(arg1) && stol(arg2)));
		case UFOR:	return(ltos(stol(arg1) || stol(arg2)));
		case UFLENGTH:	return(l_itoa((int)strlen(arg1)));
		case UFUPPER:	return(mkupper(arg1));
		case UFLOWER:	return(mklower(arg1));
				/* patch: why 42? (that's an '*') */
		case UFTRUTH:	return(ltos(atoi(arg1) == 42));
		case UFASCII:	return(l_itoa((int)arg1[0]));
		case UFCHR:	result[0] = atoi(arg1);
				result[1] = EOS;
				return(result);
		case UFGTKEY:	result[0] = tgetc(TRUE);
				result[1] = EOS;
				return(result);
		case UFRND:	return(l_itoa((ernd() % absol(atoi(arg1))) + 1));
		case UFABS:	return(l_itoa(absol(atoi(arg1))));
		case UFSINDEX:	return(l_itoa(sindex(arg1, arg2)));
		case UFENV:	return(GetEnv(arg1));
		case UFBIND:	return(prc2engl(arg1));
		case UFREADABLE:return(ltos(doglob(arg1)
				  &&   flook(arg1, FL_HERE) != NULL));
		case UFWRITABLE:return(ltos(doglob(arg1)
				  &&   !ffronly(arg1)));
	}
	return errorm;
}

char *gtusr(vname)	/* look up a user var's value */
char *vname;		/* name of user variable to fetch */
{
	register UVAR	*p;

	if (vname[0] != EOS) {
		for (p = user_vars; p != 0; p = p->next)
			if (!strcmp(vname, p->u_name))
				return p->u_value;
	}
	return (errorm);
}

char *gtenv(vname)
char *vname;		/* name of environment variable to retrieve */
{
	register int vnum;	/* ordinal number of var referenced */
	register char *value = errorm;

	if (vname[0] != EOS) {

		/* scan the list, looking for the referenced name */
		for (vnum = 0; envars[vnum] != 0; vnum++)
			if (strcmp(vname, envars[vnum]) == 0)
				break;

		/* return errorm on a bad reference */
		if (envars[vnum] == 0) {
#if !SMALLER
			VALARGS	args;

			if (is_mode_name(vname, TRUE, &args) == TRUE)
				value = string_mode_val(&args);
#endif
			return (value);
		}


		/* otherwise, fetch the appropriate value */

		/* NOTE -- if you get a compiler error from this code, find
			the definitions of If and ElseIf up above, and add your
			system to the set of those with broken compilers that need
			to use ifs instead of a switch statement */

		If( EVPAGELEN )		value = l_itoa(term.t_nrow + 1);
		ElseIf( EVCURCOL )	value = l_itoa(getccol(FALSE) + 1);
		ElseIf( EVCURLINE )	value = l_itoa(getcline());
#if RAMSIZE
		ElseIf( EVRAM )		value = l_itoa((int)(envram / 1024l));
#endif
		ElseIf( EVFLICKER )	value = ltos(flickcode);
		ElseIf( EVCURWIDTH )	value = l_itoa(term.t_ncol);
		ElseIf( EVCBUFNAME )	value = get_bname(curbp);
		ElseIf( EVCFNAME )	value = curbp->b_fname;
		ElseIf( EVSRES )	value = sres;
		ElseIf( EVDEBUG )	value = ltos(macbug);
		ElseIf( EVSTATUS )	value = ltos(cmdstatus);
		ElseIf( EVPALETTE )	value = palstr;
		ElseIf( EVLASTKEY )	value = l_itoa(lastkey);
		ElseIf( EVCURCHAR )
			value = is_at_end_of_line(DOT)
				? l_itoa('\n')
				: l_itoa(char_at(DOT));

		ElseIf( EVDISCMD )	value = ltos(discmd);
		ElseIf( EVVERSION )	value = version;
		ElseIf( EVPROGNAME )	value = prognam;
		ElseIf( EVSEED )	value = l_itoa(seed);
		ElseIf( EVDISINP )	value = ltos(disinp);
		ElseIf( EVWLINE )	value = l_itoa(curwp->w_ntrows);
		ElseIf( EVCWLINE )	value = l_itoa(getwpos());
		ElseIf( EVSEARCH )	value = pat;
		ElseIf( EVREPLACE )	value = rpat;
		ElseIf( EVMATCH )	value = (patmatch == NULL) ? 
							"" : patmatch;
		ElseIf( EVMODE )	value = current_modename();
		ElseIf( EVKILL )	value = getkill();
		ElseIf( EVTPAUSE )	value = l_itoa(term.t_pause);
		ElseIf( EVPENDING )
#if	TYPEAH
					value = ltos(typahead());
#else
					value = falsem;
#endif
		ElseIf( EVLLENGTH )	value = l_itoa(lLength(DOT.l));
		ElseIf( EVLINE )	value = getctext(0);
		ElseIf( EVWORD )	value = getctext(_nonspace);
		ElseIf( EVIDENTIF )	value = getctext(_ident);
		ElseIf( EVQIDENTIF )	value = getctext(_qident);
		ElseIf( EVPATHNAME )	value = getctext(_pathn);
		ElseIf( EVCWD )		value = current_directory(FALSE);
#if X11
		ElseIf( EVFONT )	value = x_current_fontname();
#endif
		ElseIf( EVSHELL )
			if (shell == 0)
				SetEnv(&shell, DftEnv("SHELL", "/bin/sh"));
			value = shell;

		ElseIf( EVDIRECTORY )
			if (directory == 0)
				SetEnv(&directory, DftEnv("TMP", P_tmpdir));
			value = directory;

		EndIf
	}
	return value;
}

static char *
getkill()		/* return some of the contents of the kill buffer */
{
	register int size;	/* max number of chars to return */
	static char value[NSTRING];	/* temp buffer for value */

	if (kbs[0].kbufh == NULL)
		/* no kill buffer....just a null string */
		value[0] = EOS;
	else {
		/* copy in the contents... */
		if (kbs[0].kused < NSTRING)
			size = kbs[0].kused;
		else
			size = NSTRING - 1;
		(void)strncpy(value, (char *)(kbs[0].kbufh->d_chunk), size);
	}

	/* and return the constructed value */
	return(value);
}

static void
FindVar(var, vd)	/* find a variables type and name */
char *var;	/* name of var to get */
VDESC *vd;	/* structure to hold type and ptr */
{
	register UVAR *p;
	register int vnum;	/* subscript in variable arrays */
	register int vtype;	/* type to return */

fvar:
	vtype = vnum = ILLEGAL_NUM;
	vd->v_ptr = 0;

	if (!var[1]) {
		vd->v_type = vtype;
		return;
	}
	switch (var[0]) {

		case '$': /* check for legal enviromnent var */
			for (vnum = 0; envars[vnum] != 0; vnum++)
				if (strcmp(&var[1], envars[vnum]) == 0) {
					vtype = TKENV;
					break;
				}
#if !SMALLER
			if (vtype == ILLEGAL_NUM) {
				VALARGS	args;

				if (is_mode_name(&var[1], TRUE, &args) == TRUE) {
					vnum  = MODE_NUM;
					vtype = TKENV;
				}
			}
#endif
			break;

		case '%': /* check for existing legal user variable */
			for (p = user_vars; p != 0; p = p->next)
				if (!strcmp(var+1, p->u_name)) {
					vtype = TKVAR;
					vnum  = USER_NUM;
					vd->v_ptr = p;
					break;
				}
			if (vd->v_ptr == 0) {
				if ((p = typealloc(UVAR)) != 0
				 && (p->u_name = strmalloc(var+1)) != 0) {
					p->next    = user_vars;
					p->u_value = 0;
					user_vars  = vd->v_ptr = p;
					vtype = TKVAR;
					vnum  = USER_NUM;
				}
			}
			break;

		case '&':	/* indirect operator? */
			var[FUNC_NAMELEN] = EOS;
			if (strcmp(&var[1], "ind") == 0) {
				/* grab token, and eval it */
				execstr = token(execstr, var, EOS);
				(void)strcpy(var, tokval(var));
				goto fvar;
			}
	}

	/* return the results */
	vd->v_num = vnum;
	vd->v_type = vtype;
}

/*
 * Handles name-completion for variables.
 */
static int
vars_complete(c, buf, pos)
int	c;
char	*buf;
int	*pos;
{
	if (buf[0] == '$') {
		int	cpos = *pos;
		*pos -= 1;	/* account for leading '$', not in tables */
		if (kbd_complete(c, buf+1, pos, (char *)&all_modes[0], sizeof(all_modes[0])))
			*pos += 1;
		else {
			*pos = cpos;
			return FALSE;
		}
	} else if (c != NAMEC) /* cancel the unget */
		(void)tungetc(c);
	return TRUE;
}

int
setvar(f, n)		/* set a variable */
int f;		/* default flag */
int n;		/* numeric arg (can overide prompted value) */
{
	register int status;	/* status return */
	static char var[NLINE+3];	/* name of variable to fetch */

	/* first get the variable to set.. */
	status = kbd_string("Variable to set: ",
		var, NLINE,
		'=', KBD_NOEVAL|KBD_LOWERC, vars_complete);
	if (status != TRUE)
		return(status);
	return PromptAndSet(var, f, n);
}

static int
PromptAndSet(var, f, n)
char	*var;
int	f,n;
{
	register int status;	/* status return */
	VDESC vd;		/* variable num/type */
	char prompt[NLINE];
	char value[NLINE];	/* value to set variable to */

	/* check the legality and find the var */
	FindVar(var, &vd);

	/* if its not legal....bitch */
	if (vd.v_type == ILLEGAL_NUM) {
		mlforce("[No such variable as '%s']", var);
		return(FALSE);
	} else if (vd.v_type == TKENV && vd.v_num == MODE_NUM) {
		VALARGS	args;
		(void)find_mode(var+1, -TRUE, &args);
		return adjvalueset(var+1, TRUE, -TRUE, &args);
	}

	/* get the value for that variable */
	if (f == TRUE)
		(void)strcpy(value, l_itoa(n));
	else {
		value[0] = EOS;
		(void)lsprintf(prompt, "Value of %s: ", var);
		status = mlreply(prompt, value, sizeof(value));
		if (status != TRUE)
			return(status);
	}

	/* and set the appropriate value */
	status = SetVarValue(&vd, value);

	if (status == ABORT)
		mlforce("[Variable %s is readonly]", var);
	else if (status != TRUE)
		mlforce("[Cannot set %s to %s]", var, value);
	/* and return it */
	return(status);
}

/* entrypoint from modes.c, used to set environment variables */
#if OPT_EVAL
int
set_variable(name)
char	*name;
{
	char	temp[NLINE];
	if (*name != '$')
		name = strcat(strcpy(temp, "$"), name);
	return PromptAndSet(name, FALSE, 0);
}
#endif

static int
SetVarValue(var, value)	/* set a variable */
VDESC *var;	/* variable to set */
char *value;	/* value to set to */
{
	register UVAR *vptr;
	register int vnum;	/* ordinal number of var referenced */
	register int vtype;	/* type of variable to set */
	register int status;	/* status return */
	register int c;		/* translated character */

	/* simplify the vd structure (we are gonna look at it a lot) */
	vptr = var->v_ptr;
	vnum = var->v_num;
	vtype = var->v_type;

	/* and set the appropriate value */
	status = TRUE;
	switch (vtype) {
	case TKVAR: /* set a user variable */
		FreeIfNeeded(vptr->u_value);
		if ((vptr->u_value = strmalloc(value)) == 0)
			status = FALSE;
		break;

	case TKENV: /* set an environment variable */
		status = TRUE;	/* by default */
		If( EVCURCOL )
			status = gotocol(TRUE,atoi(value));

		ElseIf( EVCURLINE )
			status = gotoline(TRUE, atoi(value));

		ElseIf( EVFLICKER )
			flickcode = stol(value);

		ElseIf( EVCURWIDTH )
			status = newwidth(TRUE, atoi(value));

		ElseIf( EVPAGELEN )
			status = newlength(TRUE,atoi(value));

		ElseIf( EVCBUFNAME )
			set_bname(curbp, value);
			curwp->w_flag |= WFMODE;

		ElseIf( EVCFNAME )
			ch_fname(curbp, value);
			curwp->w_flag |= WFMODE;

		ElseIf( EVSRES )
			status = TTrez(value);

		ElseIf( EVDEBUG )
			macbug = stol(value);

		ElseIf( EVSTATUS )
			cmdstatus = stol(value);

		ElseIf( EVPALETTE )
			(void)strncpy(palstr, value, 48);
			spal(palstr);

		ElseIf( EVLASTKEY )
			lastkey = atoi(value);

		ElseIf( EVCURCHAR )
			if (b_val(curbp,MDVIEW))
				status = rdonly();
			else {
				(void)ldelete(1L, FALSE); /* delete 1 char */
				c = atoi(value);
				if (c == '\n')
					status = lnewline();
				else
					status = linsert(1, c);
				(void)backchar(FALSE, 1);
			}

		ElseIf( EVDISCMD )
			discmd = stol(value);

		ElseIf( EVSEED )
			seed = atoi(value);

		ElseIf( EVDISINP )
			disinp = stol(value);

		ElseIf( EVWLINE )
			status = resize(TRUE, atoi(value));

		ElseIf( EVCWLINE )
			status = forwline(TRUE, atoi(value) - getwpos());

		ElseIf( EVSEARCH )
			(void)strcpy(pat, value);
			FreeIfNeeded(gregexp);
			gregexp = regcomp(pat, b_val(curbp, MDMAGIC));

		ElseIf( EVREPLACE )
			(void)strcpy(rpat, value);

		ElseIf( EVTPAUSE )
			term.t_pause = atoi(value);

		ElseIf( EVLINE )
			if (b_val(curbp,MDVIEW))
				status = rdonly();
			else
				status = putctext(value);

		ElseIf( EVCWD )
			status = set_directory(value);
#if X11
		ElseIf( EVFONT )
			status = x_setfont(value);
#endif
		ElseIf( EVSHELL )
			SetEnv(&shell, value);

		ElseIf( EVDIRECTORY )
			SetEnv(&directory, value);

		Otherwise
			/* EVPROGNAME */
			/* EVVERSION */
			/* EVMATCH */
			/* EVKILL */
			/* EVPENDING */
			/* EVLLENGTH */
			/* EVRAM */
			/* EVMODE */
			status = ABORT;	/* must be readonly */
		EndIf
		break;
	}
#if	DEBUGM
	/* if $debug == TRUE, every assignment will echo a statment to
	   that effect here. */

	if (macbug) {
		char	outline[NLINE];
		(void)strcpy(outline, "(((");

		/* assignment status */
		(void)strcat(outline, ltos(status));
		(void)strcat(outline, ":");

		/* variable name */
		(void)strcat(outline, var);
		(void)strcat(outline, ":");

		/* and lastly the value we tried to assign */
		(void)strcat(outline, value);
		(void)strcat(outline, ")))");


		/* write out the debug line */
		mlforce("%s",outline);
		(void)update(TRUE);

		/* and get the keystroke to hold the output */
		if (kbd_key() == abortc) {
			mlforce("[Macro aborted]");
			status = FALSE;
		}
	}
#endif
	return(status);
}
#endif

/*	l_itoa:	integer to ascii string.......... This is too
		inconsistent to use the system's	*/

char *l_itoa(i)
int i;	/* integer to translate to a string */
{
	static char result[INTWIDTH+1];	/* resulting string */
	(void)lsprintf(result,"%d",i);
	return result;
}

int toktyp(tokn)	/* find the type of a passed token */
char *tokn;	/* token to analyze */
{

	/* no blanks!!! */
	if (tokn[0] == '\0')
		return(TKNUL);

	/* a numeric literal? */
	if (isdigit(tokn[0]))
		return(TKLIT);

	if (tokn[0] == '"')
		return(TKSTR);

	/* if it's any other single char, it must be itself */
	if (tokn[1] == '\0')
		return(TKCMD);

#if ! SMALLER
	switch (tokn[0]) {
		case '"':	return(TKSTR);

		case '~':	return(TKDIR);
		case '@':	return(TKARG);
		case '#':	return(TKBUF);
		case '$':	return(TKENV);
		case '%':	return(TKVAR);
		case '&':	return(TKFUN);
		case '*':	return(TKLBL);

		default:	return(TKCMD);
	}
#else
	return(TKCMD);
#endif
}

char *
tokval(tokn)	/* find the value of a token */
char *tokn;		/* token to evaluate */
{
#if OPT_EVAL
	register int status;	/* error return */
	register BUFFER *bp;	/* temp buffer pointer */
	register int blen;	/* length of buffer argument */
	register int distmp;	/* temporary discmd flag */
	int	oclexec;
	static char buf[NSTRING];/* string buffer for some returns */

	switch (toktyp(tokn)) {
		case TKNUL:	return("");

		case TKARG:	/* interactive argument */
				oclexec = clexec;

				(void)strcpy(tokn, tokval(&tokn[1]));
				distmp = discmd;	/* echo it always! */
				discmd = TRUE;
				clexec = FALSE;
				buf[0] = EOS;
				status = kbd_string(tokn, buf,
						sizeof(buf), '\n',
						KBD_EXPAND|KBD_QUOTES,
						no_completion);
				discmd = distmp;
				clexec = oclexec;

				if (status == ABORT)
					return(errorm);
				return(buf);

		case TKBUF:	/* buffer contents fetch */

				/* grab the right buffer */
				(void)strcpy(tokn, tokval(&tokn[1]));
				if ((bp = find_b_name(tokn)) == NULL)
					return(errorm);

				/* if the buffer is displayed, get the window
				   vars instead of the buffer vars */
				if (bp->b_nwnd > 0) {
					curbp->b_dot = curwp->w_dot;
				}

				/* make sure we are not at the end */
				if (is_header_line(bp->b_dot,bp))
					return(errorm);

				/* grab the line as an argument */
				blen = lLength(bp->b_dot.l) - bp->b_dot.o;
				if (blen > NSTRING)
					blen = NSTRING;
				(void)strncpy(buf,
					l_ref(bp->b_dot.l)->l_text + bp->b_dot.o,
					blen);
				buf[blen] = EOS;

				/* and step the buffer's line ptr
					ahead a line */
				bp->b_dot.l = lFORW(bp->b_dot.l);
				bp->b_dot.o = 0;

				/* if displayed buffer, reset window ptr vars*/
				if (bp->b_nwnd > 0) {
					curwp->w_dot.l = curbp->b_dot.l;
					curwp->w_dot.o = 0;
					curwp->w_flag |= WFMOVE;
				}

				/* and return the spoils */
				return(buf);

		case TKVAR:	return(gtusr(tokn+1));
		case TKENV:	return(gtenv(tokn+1));
		case TKFUN:	return(gtfun(tokn+1));
		case TKDIR:	return(errorm);
		case TKLBL:	return(l_itoa(gtlbl(tokn)));
		case TKLIT:	return(tokn);
		case TKSTR:	return(tokn+1);
		case TKCMD:	return(tokn);
	}
	return errorm;
#else
	return tokn;
#endif
}

/*
 * Return true if the argument is one of the strings that we accept as a
 * synonym for "true".
 */
int
is_truem(val)
char *val;
{
	char	temp[8];
	(void)mklower(strncpy(temp, val, sizeof(temp)));
	return (!strcmp(temp, "yes")
	   ||   !strcmp(temp, "true")
	   ||   !strcmp(temp, "t")
	   ||   !strcmp(temp, "y")
	   ||   !strcmp(temp, "on"));
}

/*
 * Return true if the argument is one of the strings that we accept as a
 * synonym for "false".
 */
int
is_falsem(val)
char *val;
{
	char	temp[8];
	(void)mklower(strncpy(temp, val, sizeof(temp)));
	return (!strcmp(temp, "no")
	   ||   !strcmp(temp, "false")
	   ||   !strcmp(temp, "f")
	   ||   !strcmp(temp, "n")
	   ||   !strcmp(temp, "off"));
}

#if OPT_EVAL || X11
int stol(val)	/* convert a string to a numeric logical */
char *val;	/* value to check for stol */
{
	/* check for logical values */
	if (is_falsem(val))
		return(FALSE);
	if (is_truem(val))
		return(TRUE);

	/* check for numeric truth (!= 0) */
	return((atoi(val) != 0));
}
#endif

#if OPT_EVAL

/* ARGSUSED */
static int
gtlbl(tokn)	/* find the line number of the given label */
char *tokn;	/* label name to find */
{
	return(1);
}

char *ltos(val)		/* numeric logical to string logical */
int val;	/* value to translate */
{
	if (val)
		return(truem);
	else
		return(falsem);
}
#endif

#if OPT_EVAL || !SMALLER
char *mkupper(str)	/* make a string upper case */
char *str;		/* string to upper case */
{
	char *sp;

	sp = str;
	while (*sp) {
		if (islower(*sp))
			*sp += 'A' - 'a';
		++sp;
	}
	return(str);
}
#endif

char *mklower(str)	/* make a string lower case */
char *str;		/* string to lower case */
{
	char *sp;

	sp = str;
	while (*sp) {
		if (isupper(*sp))
			*sp += 'a' - 'A';
		++sp;
	}
	return(str);
}

int absol(x)	/* take the absolute value of an integer */
int x;
{
	return(x < 0 ? -x : x);
}

#if OPT_EVAL
int ernd()	/* returns a random integer */
{
	seed = absol(seed * 1721 + 10007);
	return(seed);
}

int sindex(sourc, pattern)	/* find pattern within source */
char *sourc;	/* source string to search */
char *pattern;	/* string to look for */
{
	char *sp;	/* ptr to current position to scan */
	char *csp;	/* ptr to source string during comparison */
	char *cp;	/* ptr to place to check for equality */

	/* scanning through the source string */
	sp = sourc;
	while (*sp) {
		/* scan through the pattern */
		cp = pattern;
		csp = sp;
		while (*cp) {
			if (!eq(*cp, *csp))
				break;
			++cp;
			++csp;
		}

		/* was it a match? */
		if (*cp == 0)
			return((int)(sp - sourc) + 1);
		++sp;
	}

	/* no match at all.. */
	return(0);
}

#endif /* OPT_EVAL */

#if NO_LEAKS
void ev_leaks()
{
#if OPT_EVAL
	FreeAndNull(shell);
	FreeAndNull(directory);
#endif
}
#endif

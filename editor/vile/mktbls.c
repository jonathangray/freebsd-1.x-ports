/* This standalone utility program constructs the function, key and command
 *	binding tables for vile.  The input is a data file containing the
 *	desired default relationships among the three entities.  Output
 *	is nebind.h, nefunc.h, and nename.h, all of which are then included
 *	in main.c
 *	This code written by Paul Fox, (c)1990
 *
 *	See the file "cmdtbls" for input data formats, and "estruct.h" for
 *	the output structures.
 *
 * Heavily modified/enhanced to also generate the table of mode and variable
 * names and their #define "bindings", based on input from the file modetbl,
 * by Tom Dickey, 1993.    -pgf
 *
 *
 * $Log: mktbls.c,v $
 * Revision 1.1  1994/02/01 03:29:32  jkh
 * Initial revision
 *
 * Revision 1.37  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.36  1993/09/06  16:30:01  pgf
 * suppress some warnings under linux
 *
 * Revision 1.35  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.34  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.33  1993/07/09  19:30:13  pgf
 * changed name of all_FUNCs to not be caseless-compared identical to
 * all_funcs, for the watcom compiler.  (there's probably a compiler option
 * i'm missing somewhere for this)
 *
 * Revision 1.32  1993/07/09  19:13:52  pgf
 * adde commentary, to mention the new role of modetbl, and to give tom
 * credit....
 *
 * Revision 1.31  1993/07/09  14:02:50  pgf
 * djgcc doesn't like the fprintf prototype
 *
 * Revision 1.30  1993/07/09  09:45:44  pgf
 * be sure we malloc enough space for the longer symbol names
 *
 * Revision 1.29  1993/07/08  14:00:41  pgf
 * last change didn't work -- let's try "s_name__" instead
 *
 * Revision 1.28  1993/07/08  13:52:22  pgf
 * make generated symbols more unique: _s_name_ instead of s_name.  there
 * was a conflict with a typedef of signed char to s_char on mips/osf1.
 *
 * Revision 1.27  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.26  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.25  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.24  1993/06/23  21:29:29  pgf
 * ifdefs for linux -- our ifdefs are obviously not right yet
 *
 * Revision 1.23  1993/06/18  16:16:34  pgf
 * alistair crooks lint/NeXt changes
 *
 * Revision 1.22  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.21  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.20  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.19  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.18  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.17  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.16  1993/02/23  12:03:49  pgf
 * moved nested static func decl -- it's not ansi
 *
 * Revision 1.15  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.14  1993/02/11  19:08:32  pgf
 * fixups for shadowed variable warnings
 *
 * Revision 1.13  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.12  1993/01/26  22:46:20  foxharp
 * fixed bad octal character constant
 *
 * Revision 1.11  1993/01/23  14:44:08  foxharp
 * local copies of isspace and isprint, since not everyone has them
 * in a standard library.
 *
 * Revision 1.10  1993/01/23  13:38:23  foxharp
 * tom's changes, for lint, cleanup, variables, and functions
 *
 * Revision 1.9  1992/07/17  19:12:44  foxharp
 * explicit int return on func
 *
 * Revision 1.8  1992/07/08  08:35:09  foxharp
 * don't report duplicate keymaps if the pre-processor conditionals are
 * both present, and they don't match -- in that case, they're unlikely
 * to both compile in at the same time.
 *
 * also, be sure to emit backslash escapes when printing ' and \ character
 * constants, i.e. '\'' and '\\'
 *
 * Revision 1.7  1992/05/29  08:34:01  foxharp
 * prototypes, gcc -W cleanups
 *
 * Revision 1.6  1991/11/07  03:29:12  pgf
 * lint cleanup
 *
 * Revision 1.5  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.4  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.3
 * date: 1991/06/03 17:34:57;
 * switch from "meta" etc. to "ctla" etc.
 * 
 * revision 1.2
 * date: 1991/06/03 10:26:03;
 * commentary change
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:40;
 * initial vile RCS revision
 */

#define	OPT_IFDEF_MODES	1	/* true iff we can ifdef modes */

/* stuff borrowed/adapted from estruct.h */
#if defined(__TURBOC__) || defined(__WATCOMC__) || defined(__GO32__) || (defined(__GNUC__) && (defined(sun) || defined(linux)))
#include <stdlib.h>
#define P(param) param
#else
#define P(param) ()
extern char *malloc();
#endif

#include <stdio.h>
#include <string.h>

/* argument for 'exit()' or '_exit()' */
#if	VMS
#include	<stsdef.h>
#define GOOD	(STS$M_INHIB_MSG | STS$K_SUCCESS)
#define BAD(c)	(STS$M_INHIB_MSG | STS$K_ERROR)
#endif

#ifndef GOOD
#define GOOD	0
#define BAD(c)	(c)
#endif

/*--------------------------------------------------------------------------*/

#define	MAX_BIND	4	/* total # of binding-types */
#define	MAX_PARSE	5	/* maximum # of tokens on line */
#define	LEN_BUFFER	50	/* nominal buffer-length */
#define	MAX_BUFFER	(LEN_BUFFER*10)
#define	LEN_CHRSET	128	/* total # of chars in set (ascii) */

	/* patch: why not use <ctype.h> ? */
#define	DIFCNTRL	0x40
#define tocntrl(c)	((c)^DIFCNTRL)
#define toalpha(c)	((c)^DIFCNTRL)
#define	DIFCASE		0x20
#define	isupper(c)	((c) >= 'A' && (c) <= 'Z')
#define	islower(c)	((c) >= 'a' && (c) <= 'z')
#define toupper(c)	((c)^DIFCASE)
#define tolower(c)	((c)^DIFCASE)

#ifndef	TRUE
#define	TRUE	(1)
#define	FALSE	(0)
#endif

#define	L_CURL	'{'
#define	R_CURL	'}'

#define	Fprintf	(void)fprintf
#define	Sprintf	(void)sprintf

#if defined(lint) || (defined(__GNUC__) && !defined(__GO32__))
#if !LINUX && !defined(__NetBSD__) && !defined(__386BSD__) && defined(__STDC__)		/* this ifdef needs work, clearly */
extern       int     fprintf(FILE *, char *, ... );
#else
extern       int     fprintf();
#endif
#endif /* lint || __GNUC__ */

#define	SaveEndif(head)	InsertOnEnd(&head, "#endif")

/*--------------------------------------------------------------------------*/

typedef	struct stringl {
	char *Name;	/* stores primary-data */
	char *Func;	/* stores secondary-data */
	char *Data;	/* associated data, if any */
	char *Cond;	/* stores ifdef-flags */
	char *Note;	/* stores comment, if any */
	struct stringl *nst;
} LIST;

static	LIST	*all_names,
		*all_funcs,	/* data for extern-lines in nefunc.h */
		*all__FUNCs,	/* data for {}-lines in nefunc.h */
		*all_envars,
		*all_ufuncs,
		*all_modes,	/* data for name-completion of modes */
		*all_gmodes,	/* data for GLOBAL modes */
		*all_bmodes,	/* data for BUFFER modes */
		*all_wmodes;	/* data for WINDOW modes */

	int	main P(( int, char ** ));

static	int	isspace P((int));
static	int	isprint P((int));

static	char *	StrAlloc P((char *));
static	LIST *	ListAlloc P((void));

static	void	badfmt P((char *));
static	void	badfmt2 P((char *, int));

static	void	WriteLines P((FILE *, char **, int));
static	FILE *	OpenHeader P((char *, char **));

static	void	InsertSorted P((LIST **, char *, char *, char *, char *, char *));
static	void	InsertOnEnd P((LIST **, char *));

static	char *	append P((char *, char *));
static	char *	formcond P((char *, char *));
static	int	LastCol P((char *));
static	char *	PadTo P((int, char *));
static	int	two_conds P((int, int, char *));
static	int	Parse P((char *, char **));

static	void	BeginIf P((void));
static	void	WriteIf P((FILE *, char *));
static	void	FlushIf P((FILE *));

static	char *	AbbrevMode P((char *));
static	char *	NormalMode P((char *));
static	char *	c2TYPE P((int));
static	void	CheckModes P(( char * ));
static	char *	Mode2Key P((char *, char *, char *));
static	char *	Name2Symbol P((char *));
static	char *	Name2Address P((char *, char *));

static	void	DefineOffset P((FILE *));
static	void	WriteIndexStruct P((FILE *, LIST *, char *));
static	void	WriteModeDefines P((LIST *, char *));
static	void	WriteModeSymbols P((LIST *));

static	void	save_all_modes P((char *, char *, char *, char *));
static	void	dump_all_modes P((void));

static	void	save_bindings P((char *, char *, char *));
static	void	dump_bindings P((void));

static	void	save_bmodes P((char *, char **));
static	void	dump_bmodes P((void));

static	void	start_evar_h P((char **));
static	void	finish_evar_h P((void));
static	void	init_envars P((void));
static	void	save_envars P((char **));
static	void	dump_envars P((void));

static	void	save_funcs P((char *, char *, char *, char *));
static	void	dump_funcs P((LIST *));

static	void	save_gmodes P((char *, char **));
static	void	dump_gmodes P((void));

static	void	save_names P((char *, char *, char *));
static	void	dump_names P((void));

static	void	init_ufuncs P((void));
static	void	save_ufuncs P((char **));
static	void	dump_ufuncs P((void));

static	void	save_wmodes P((char *, char **));
static	void	dump_wmodes P((void));

	/* definitions for sections of cmdtbl */
#define	SECT_CMDS 0
#define	SECT_FUNC 1
#define	SECT_VARS 2
#define	SECT_GBLS 3
#define	SECT_BUFF 4
#define	SECT_WIND 5

	/* definitions for [MAX_BIND] indices */
#define ASCIIBIND 0
#define CTLXBIND 1
#define CTLABIND 2
#define SPECBIND 3

static	char *bindings  [MAX_BIND][LEN_CHRSET];
static	char *conditions[MAX_BIND][LEN_CHRSET];
static	char *tblname   [MAX_BIND] = {"asciitbl", "ctlxtbl", "metatbl", "spectbl" };
static	char *prefname  [MAX_BIND] = {"",         "CTLX|",   "CTLA|",   "SPEC|" };

static	char *inputfile;
static	int l = 0;
static	FILE *nebind, *nefunc, *nename, *cmdtbl;
static	FILE *nevars, *nemode;

/******************************************************************************/
static int
isspace(c)
int c;
{
	return c == ' ' || c == '\t' || c == '\n';
}

static int
isprint(c)
int c;
{
	return c >= ' ' && c < '\177';
}

/******************************************************************************/
static char *
StrAlloc(s)
char	*s;
{
	return strcpy(malloc((unsigned)strlen(s)+1), s);
}

static LIST *
ListAlloc()
{
	return (LIST *)malloc(sizeof(LIST));
}

/******************************************************************************/
static void
badfmt(s)
char *s;
{
	Fprintf(stderr,"\"%s\", line %d: bad format:", inputfile, l);
	Fprintf(stderr,"\t%s\n",s);
	exit(BAD(1));
}

static void
badfmt2(s,col)
char *s;
int col;
{
	char	temp[BUFSIZ];
	Sprintf(temp, "%s (column %d)", s, col);
	badfmt(temp);
}

/******************************************************************************/
static void
WriteLines(fp, list, count)
FILE	*fp;
char	**list;
int	count;
{
	while (count-- > 0)
		Fprintf(fp, "%s\n", *list++);
}
#define	write_lines(fp,list) WriteLines(fp, list, sizeof(list)/sizeof(list[0]))

/******************************************************************************/
static FILE *
OpenHeader(name, argv)
char	*name;
char	**argv;
{
	register FILE *fp;
	static char *progcreat =
"/* %s: this header file was produced automatically by\n\
 * the %s program, based on input from the file %s\n */\n";

	if (!(fp = fopen(name, "w"))) {
		Fprintf(stderr,"mktbls: couldn't open header file %s\n", name);
		exit(BAD(1));
	}
	Fprintf(fp, progcreat, name, argv[0], argv[1]);
	return fp;
}

/******************************************************************************/
static void
InsertSorted(headp, name, func, data, cond, note)
LIST	**headp;
char	*name;
char	*func;
char	*data;
char	*cond;
char	*note;
{
	register LIST *n, *p, *q;
	register int  r;

	n = ListAlloc();
	n->Name = StrAlloc(name);
	n->Func = StrAlloc(func);
	n->Data = StrAlloc(data);
	n->Cond = StrAlloc(cond);
	n->Note = StrAlloc(note);

	for (p = *headp, q = 0; p != 0; q = p, p = p->nst) {
		if ((r = strcmp(n->Name, p->Name)) < 0)
			break;
		else if (r == 0)
			badfmt("duplicate name");
	}
	n->nst = p;
	if (q == 0)
		*headp = n;
	else
		q->nst = n;
}

static void
InsertOnEnd(headp, name)
LIST	**headp;
char	*name;
{
	register LIST *n, *p, *q;

	n = ListAlloc();
	n->Name = StrAlloc(name);
	n->Func = "";
	n->Cond = "";
	n->Note = "";

	for (p = *headp, q = 0; p != 0; q = p, p = p->nst)
		;

	n->nst = 0;
	if (q == 0)
		*headp = n;
	else
		q->nst = n;
}

/******************************************************************************/
static char *
append(dst, src)
char	*dst;
char	*src;
{
	(void)strcat(dst, src);
	return (dst + strlen(dst));
}

static char *
formcond(c1,c2)
char *c1, *c2;
{
	static char cond[LEN_BUFFER];
	if (c1[0] && c2[0])
		Sprintf(cond, "(%s) & (%s)", c1, c2);
	else if (c1[0] || c2[0])
		Sprintf(cond, "(%s%s)", c1, c2);
	else
		cond[0] = '\0';
	return cond;
}

static int
LastCol(buffer)
char	*buffer;
{
	register int	col = 0,
			c;
	while ((c = *buffer++) != 0) {
		if (isprint(c))
			col++;
		else if (c == '\t')
			col = (col | 7) + 1;
	}
	return col;
}

static char *
PadTo(col, buffer)
int col;
char	*buffer;
{
	int	any	= 0,
		len	= strlen(buffer),
		now,
		with;

	for (;;) {
		if ((now = LastCol(buffer)) >= col) {
			if (any)
				break;
			else
				with = ' ';
		} else if (col-now > 1)
			with = '\t';
		else
			with = ' ';

		buffer[len++] = with;
		buffer[len]   = '\0';
		any++;
	}
	return buffer;
}

static int
two_conds(btype,c,cond)
int btype,c;
char *cond;
{
	/* return true if both bindings have different
	 conditions associated with them */
	return (cond[0] != 0 &&
		conditions[btype][c] != NULL &&
		strcmp(cond, conditions[btype][c]) != 0);
}

/******************************************************************************/
	/* returns the number of non-comment tokens parsed, with a list of
	 * tokens (0=comment) as a side-effect.  Note that quotes are removed
	 * from the token, so we have to have them only in the first token! */
static int
Parse(input, vec)
char	*input;
char	**vec;
{
	char	*base = input;
	register int	expecting = TRUE,
			count = 0,
			quote = 0,
			c;

	for (c = 0; c < MAX_PARSE; c++)
		vec[c] = "";
	for (c = strlen(input); c > 0 && isspace(input[c-1]); c--)
		input[c-1] = '\0';

	while ((c = *input++) != 0) {
		if (quote) {
			if (c == quote) {
				quote = 0;
				if (*input && !isspace(*input))
					badfmt2("expected blank", input-base);
				input[-1] = '\0';
			}
		} else {
			if ((c == '"') || (c == '\'')) {
				quote = c;
			} else if (isspace(c)) {
				input[-1] = '\0';
				expecting = TRUE;
			} else if ((c == '#')) {
				while (isspace(*input))
					input++;
				vec[0] = input;
				break;
			}
			if (expecting && !isspace(c)) {
				if (count+1 >= MAX_PARSE)
					break;
				vec[++count] = input - (c != quote);
				expecting = FALSE;
			}
		}
	}
	return count;
}

/******************************************************************************/
static	char	*lastIfdef;

static void
BeginIf()
{
	lastIfdef = 0;
}

static void
WriteIf(fp, cond)
FILE	*fp;
char	*cond;
{
	if (cond == 0)
		cond = "";
	if (cond[0] != '\0') {
		if (lastIfdef != 0) {
			if (!strcmp(lastIfdef, cond))
				return;
			FlushIf(fp);
		}
		Fprintf(fp, "#if %s\n", lastIfdef = cond);
	} else
		FlushIf(fp);
}

static void
FlushIf(fp)
FILE	*fp;
{
	if (lastIfdef != 0) {
		Fprintf(fp, "#endif\n");
		lastIfdef = 0;
	}
}

/******************************************************************************/
/* get abbreviation by taking the uppercase chars only */
static char *
AbbrevMode(src)
char *src;
{
	char *dst = StrAlloc(src);
	register char	*s = src,
			*d = dst;
	while (*s) {
		if (isupper(*s))
			*d++ = tolower(*s);
		s++;
	}
	*d = '\0';
	return dst;
}

/* get name, converted to lowercase */
static char *
NormalMode(src)
char *src;
{
	char *dst = StrAlloc(src);
	register char *s = dst;
	while (*s) {
		if (isupper(*s))
			*s = tolower(*s);
		s++;
	}
	return dst;
}

/* given single-char type-key (cf: Mode2Key), return define-string */
static char *
c2TYPE(c)
int	c;
{
	char	*value;
	switch (c) {
	case 'b':	value	= "BOOL";	break;
	case 'c':	value	= "COLOR";	break;
	case 'i':	value	= "INT";	break;
	case 's':	value	= "STRING";	break;
	case 'x':	value	= "REGEX";	break;
	default:	value	= "?";
	}
	return value;
}

/* check that the mode-name won't be illegal */
static void
CheckModes(name)
char	*name;
{
	if (!strncmp(name, "no", 2))
		badfmt("illegal mode-name");
}

/* make a sort-key for mode-name */
static char *
Mode2Key(type, name, cond)
char	*type, *name, *cond;
{
	int	c;
	char	*abbrev = AbbrevMode(name),
		*normal = NormalMode(name),
		*tmp = malloc((unsigned)(4 + strlen(normal) + strlen(abbrev)));

	CheckModes(normal);
	CheckModes(abbrev);

	switch (c = *type) {
	case 'b':
	case 'c':
	case 'i':
	case 's':	break;
	case 'r':	c = 'x';	/* make this sort after strings */
	}

	save_all_modes(type, normal, abbrev, cond);

	(void)sprintf(tmp, "%s\n%c\n%s", normal, c, abbrev);
	return tmp;
}

/* converts a mode-name to a legal (hopefully unique!) symbol */
static char *
Name2Symbol(name)
char	*name;
{
	char	*base, *dst;
	register char c;

	/* allocate enough for adjustment in 'Name2Address()' */
	/*   "+ 10" for comfort */
	base = dst = malloc((unsigned)(strlen(name) + 10));

	*dst++ = 's';
	*dst++ = '_';
	while ((c = *name++) != 0) {
		if (c == '-')
			c = '_';
		*dst++ = c;
	}
	*dst++ = '_';
	*dst++ = '_';
	*dst = '\0';
	return base;
}

/* converts a mode-name & type to a reference to string-value */
static char *
Name2Address(name, type)
char	*name;
char	*type;
{
	/*  "+ 10" for comfort */
	char	*base = malloc((unsigned)(strlen(name) + 10));
	char	*temp;

	temp = Name2Symbol(name);
	(void)strcpy(base, temp);
	if (*type == 'b')
		(void)strcat(strcat(strcpy(base+2, "no"), temp+2), "+2");
	free(temp);
	return base;
}

/* define OFFSETOF macro, used in index-definitions */
static void
DefineOffset(fp)
FILE	*fp;
{
#if	OPT_IFDEF_MODES
	Fprintf(fp,
"#ifndef\tOFFSETOF\n\
#define\tOFFSETOF(T, M)\t(((int)&(((T*)0)->M))/\\\n\
\t\t\t\t ((int)&(((T*)0)->Q1) - (int)&(((T*)0)->s_MAX)))\n\
#endif\n");
#endif
}

/* generate the index-struct (used for deriving ifdef-able index definitions) */
static void
WriteIndexStruct(fp, p, ppref)
FILE	*fp;
LIST	*p;
char	*ppref;
{
#if	OPT_IFDEF_MODES
	char	*s,
		temp[MAX_BUFFER],
		line[MAX_BUFFER],
		*vec[MAX_PARSE];

	BeginIf();
	Fprintf(fp, "typedef\tstruct\t%c\n", L_CURL);
	while (p != 0) {
		WriteIf(fp, p->Cond);
		(void)Parse(strcpy(line, p->Name), vec);
		Sprintf(temp, "\tchar\t%s;", s = Name2Symbol(vec[1]));
		free(s);
		if (p->Note[0]) {
			(void)PadTo(32, temp);
			Sprintf(temp+strlen(temp), "/* %s */", p->Note);
		}
		Fprintf(fp, "%s\n", temp);
		p = p->nst;
	}

	FlushIf(fp);
	Fprintf(fp, "\tchar\ts_MAX;\n");
	Fprintf(fp, "\tchar\tQ1;\n");
	Fprintf(fp, "\t%c Index%s;\n\n", R_CURL, ppref);
#endif	/* OPT_IFDEF_MODES */
}

/* generate the index-definitions */
static void
WriteModeDefines(p, ppref)
LIST	*p;
char	*ppref;
{
	char	temp[MAX_BUFFER],
		line[MAX_BUFFER],
		*vec[MAX_PARSE];
	int	count	= 0;
#if OPT_IFDEF_MODES
	char	*s;
	BeginIf();
#endif

	for (; p != 0; p = p->nst, count++) {
		(void)Parse(strcpy(line, p->Name), vec);
		Sprintf(temp, "#define %.1s%s%s ",
			(*ppref == 'B') ? "" : ppref,
			(*vec[2] == 'b') ? "MD" : "VAL_",
			p->Func);
		(void)PadTo(24, temp);
#if OPT_IFDEF_MODES
		WriteIf(nemode, p->Cond);
		Sprintf(temp+strlen(temp), "OFFSETOF(Index%s, %s)",
			ppref, s = Name2Symbol(vec[1]));
		free(s);
#else
		Sprintf(temp+strlen(temp), "%d", count);
		if (p->Note[0]) {
			(void)PadTo(32, temp);
			Sprintf(temp+strlen(temp), "/* %s */", p->Note);
		}
#endif /* OPT_IFDEF_MODES */
		Fprintf(nemode, "%s\n", temp);
	}

	Fprintf(nemode, "\n");
#if OPT_IFDEF_MODES
	FlushIf(nemode);
	Sprintf(temp, "#define NUM_%c_VALUES\tOFFSETOF(Index%s, s_MAX)",
		*ppref, ppref);
#else
	Sprintf(temp, "#define NUM_%c_VALUES\t%d", *ppref, count);
#endif /* OPT_IFDEF_MODES */

	(void)PadTo(32, temp);
	Sprintf(temp+strlen(temp), "/* SIZEOF(%c_valuenames) -- %s */\n",
		tolower(*ppref), ppref);
	Fprintf(nemode, "%s", temp);
	Fprintf(nemode, "#define MAX_%c_VALUES\t%d\n\n", *ppref, count);
}

static void
WriteModeSymbols(p)
LIST	*p;
{
	char	temp[MAX_BUFFER],
		line[MAX_BUFFER];
	char	*vec[MAX_PARSE],
		*s;

	/* generate the symbol-table */
	BeginIf();
	while (p != 0) {
#if OPT_IFDEF_MODES
		WriteIf(nemode, p->Cond);
#endif
		(void)Parse(strcpy(line, p->Name), vec);
		Sprintf(temp, "\t%c %s,",
			L_CURL, s = Name2Address(vec[1], vec[2]));
		(void)PadTo(32, temp);
		free(s);
		s = 0;

		Sprintf(temp+strlen(temp), "%s,",
			*vec[3] ? (s = Name2Address(vec[3], vec[2])) : "\"X\"");
		(void)PadTo(48, temp);
		if (s != 0)
			free(s);

		Sprintf(temp+strlen(temp), "VALTYPE_%s,", c2TYPE(*vec[2]));
		(void)PadTo(64, temp);
		Sprintf(temp+strlen(temp), "%s },", p->Data);
		Fprintf(nemode, "%s\n", temp);
		p = p->nst;
	}
	FlushIf(nemode);

}

/******************************************************************************/
static void
save_all_modes(type, normal, abbrev, cond)
char	*type;
char	*normal;
char	*abbrev;
char	*cond;
{
	if (*type == 'b') {
		char	t_normal[LEN_BUFFER],
			t_abbrev[LEN_BUFFER];
		save_all_modes("Bool",
			strcat(strcpy(t_normal, "no"), normal),
			*abbrev
				? strcat(strcpy(t_abbrev, "no"), abbrev)
				: "",
			cond);
	}
	InsertSorted(&all_modes, normal, type, "", cond, "");
	if (*abbrev)
		InsertSorted(&all_modes, abbrev, type, "", cond, "");
}

static void
dump_all_modes()
{
	static	char	*top[] = {
		"",
		"#ifdef realdef",
		"/*",
		" * List of strings shared between all_modes, b_valnames and w_valnames",
		" */",
		"static char",
		};
	static	char	*middle[] = {
		"#endif /* realdef */",
		"",
		"#ifdef realdef",
		"char *all_modes[] = {",
		};
	static	char	*bottom[] = {
		"\tNULL\t/* ends table */",
		"};",
		"#else",
		"extern char *all_modes[];",
		"#endif /* realdef */",
		};
	char	temp[MAX_BUFFER], *s;
	register LIST *p, *q;

	InsertSorted(&all_modes, "all", "?", "", "", "");
	write_lines(nemode, top);
	BeginIf();
	for (p = all_modes; p; p = p->nst) {
		if (p->Func[0] != 'b') {
			for (q = p->nst; q != 0; q = q->nst)
				if (q->Func[0] != 'b')
					break;
#if OPT_IFDEF_MODES
			WriteIf(nemode, p->Cond);
#endif
			Sprintf(temp, "\t%s[]", s = Name2Symbol(p->Name));
			(void)PadTo(32, temp);
			free(s);
			Sprintf(temp+strlen(temp), "= \"%s\"%c",
				p->Name, (q != 0) ? ',' : ';');
			(void)PadTo(64, temp);
			Fprintf(nemode, "%s/* %s */\n", temp, p->Func);
		}
	}
	FlushIf(nemode);

	write_lines(nemode, middle);
	for (p = all_modes; p; p = p->nst) {
#if OPT_IFDEF_MODES
		WriteIf(nemode, p->Cond);
#endif
		Fprintf(nemode, "\t%s,\n", s = Name2Address(p->Name, p->Func));
		free(s);
	}
	FlushIf(nemode);

	write_lines(nemode, bottom);
}

/******************************************************************************/
static void
save_bindings(s,func,cond)
char *s, *func, *cond;
{
	int btype, c;

	btype = ASCIIBIND;

	if (*s == '^' && *(s+1) == 'A'&& *(s+2) == '-') {
		btype = CTLABIND;
		s += 3;
	} else if (*s == 'F' && *(s+1) == 'N' && *(s+2) == '-') {
		btype = SPECBIND;
		s += 3;
	} else if (*s == '^' && *(s+1) == 'X'&& *(s+2) == '-') {
		btype = CTLXBIND;
		s += 3;
	}

	if (*s == '\\') { /* try for an octal value */
		c = 0;
		while (*++s < '8' && *s >= '0')
			c = (c*8) + *s - '0';
		if (c >= LEN_CHRSET)
			badfmt("octal character too big");
		if (bindings[btype][c] != NULL && !two_conds(btype,c,cond))
			badfmt("duplicate key binding");
		bindings[btype][c] = StrAlloc(func);
	} else if (*s == '^' && (c = *(s+1)) != '\0') { /* a control char? */
		if (c > 'a' &&  c < 'z')
			c = toupper(c);
		c = tocntrl(c);
		if (bindings[btype][c] != NULL && !two_conds(btype,c,cond))
			badfmt("duplicate key binding");
		bindings[btype][c] = StrAlloc(func);
		s += 2;
	} else if ((c = *s) != 0) {
		if (bindings[btype][c] != NULL && !two_conds(btype,c,cond))
			badfmt("duplicate key binding");
		bindings[btype][c] = StrAlloc(func);
		s++;
	} else {
		badfmt("getting binding");
	}
	if (cond[0]) {
		conditions[btype][c] = StrAlloc(cond);
	} else {
		conditions[btype][c] = NULL;
	}

	if (*s != '\0')
		badfmt("got extra characters");
}

static void
dump_bindings()
{
	char	temp[MAX_BUFFER];
	char *sctl;
	int i, c, btype;

	btype = ASCIIBIND;

	Fprintf(nebind,"\nCMDFUNC *%s[%d] = %c\n",
		tblname[btype], LEN_CHRSET, L_CURL);

	BeginIf();
	for (i = 0; i < LEN_CHRSET; i++) {
		WriteIf(nebind, conditions[btype][i]);

		if (i < ' ' || i > '~' ) {
			sctl = "ctrl-";
			c = toalpha(i);
		} else {
			sctl = "";
			c = i;
		}

		if (bindings[btype][i])
			Sprintf(temp, "\t&f_%s,", bindings[btype][i]);
		else
			Sprintf(temp, "\tNULL,");

		Fprintf(nebind, "%s/* %s%c */\n", PadTo(32, temp), sctl, c);

		if (conditions[btype][i] != 0)
			Fprintf(nebind,"#else\n\tNULL,\n");
		FlushIf(nebind);

	}
	Fprintf(nebind, "%c;\n", R_CURL);

	Fprintf(nebind,"\nKBIND kbindtbl[NBINDS] = %c\n", L_CURL);
	for (btype = 1; btype <= 3; btype++) {
		BeginIf();
		for (i = 0; i < LEN_CHRSET; i++) {
			if (bindings[btype][i]) {
				WriteIf(nebind, conditions[btype][i]);
				if (i < ' ') {
					Sprintf(temp,
					"\t%c %stocntrl('%c'),",
						L_CURL,
						prefname[btype],
						toalpha(i));
				} else {
					Sprintf(temp,
					"\t%c %s'%s%c',",
						L_CURL,
						prefname[btype],
						(i == '\'' || i == '\\') ?
							"\\":"",
						i);
				}
				Fprintf(nebind, "%s&f_%s %c,\n",
					PadTo(32, temp), bindings[btype][i], R_CURL);
			}
		}
		FlushIf(nebind);
	}
	Fprintf(nebind,"\t{ 0, NULL }\n");
	Fprintf(nebind,"%c;\n", R_CURL);
}

/******************************************************************************/
static void
save_bmodes(type, vec)
char	*type;
char	**vec;
{
	InsertSorted(&all_bmodes, Mode2Key(type,vec[1],vec[4]), vec[2], vec[3], vec[4], vec[0]);
}

static void
dump_bmodes()
{
	static	char	*top[] = {
		"",
		"/* buffer mode flags\t*/",
		"/* the indices of B_VALUES.v[] */",
		};
	static	char	*middle[] = {
		"",
		"typedef struct B_VALUES {",
		"\t/* each entry is a val, and a ptr to a val */",
		"\tstruct VAL bv[MAX_B_VALUES+1];",
		"} B_VALUES;",
		"",
		"#ifdef realdef",
		"struct VALNAMES b_valuenames[] = {",
		};
	static	char	*bottom[] = {
		"",
		"\t{ NULL,\tNULL,\tVALTYPE_INT, 0 }",
		"};",
		"#else",
		"extern struct VALNAMES b_valuenames[];",
		"#endif",
		};

	write_lines(nemode, top);
	WriteIndexStruct(nemode, all_bmodes, "Buffers");
	WriteModeDefines(all_bmodes, "Buffers");
	write_lines(nemode, middle);
	WriteModeSymbols(all_bmodes);
	write_lines(nemode, bottom);
}

/******************************************************************************/
static void
start_evar_h(argv)
char	**argv;
{
	static	char	*head[] = {
		"",
		"#if OPT_EVAL",
		"",
		"/*\tstructure to hold user variables and their definitions\t*/",
		"",
		"typedef struct UVAR {",
		"\tstruct UVAR *next;",
		"\tchar *u_name;\t\t/* name of user variable */",
		"\tchar *u_value;\t\t\t\t/* value (string) */",
		"} UVAR;",
		"",
		"decl_uninit( UVAR *user_vars );\t/* user variables */",
		"",
		};

	if (!nevars) {
		nevars = OpenHeader("nevars.h", argv);
		write_lines(nevars, head);
	}
}

static void
finish_evar_h()
{
	if (nevars)
		Fprintf(nevars, "\n#endif /* OPT_EVAL */\n");
}

/******************************************************************************/
static void
init_envars()
{
	static	char	*head[] = {
		"",
		"/*\tlist of recognized environment variables\t*/",
		"",
		"#ifdef realdef",
		"char *envars[] = {"
		};
	static	int	done;

	if (!done++)
		write_lines(nevars, head);
}

static void
save_envars(vec)
char	**vec;
{
	/* insert into 'all_modes' to provide for common name-completion
	 * table, and into 'all_envars' to get name/index correspondence.
	 */
	InsertSorted(&all_modes,  vec[1], "env",  "", formcond("OPT_EVAL", vec[3]), "");
	InsertSorted(&all_envars, vec[1], vec[2], "", vec[3], vec[0]);
}

static void
dump_envars()
{
	static	char *middle[] = {
		"\tNULL\t/* ends table for name-completion */",
		"};",
		"#else",
		"extern char *envars[];",
		"#endif",
		"",
		"/* \tand its preprocesor definitions\t\t*/",
		""
		};
	char	temp[MAX_BUFFER];
	register LIST *p;
	register int count;
#if OPT_IFDEF_MODES
	char	*s;
#endif

	BeginIf();
	for (p = all_envars, count = 0; p != 0; p = p->nst) {
		if (!count++)
			init_envars();
#if OPT_IFDEF_MODES
		WriteIf(nevars, p->Cond);
#endif
		Fprintf(nevars, "\t%s,\n", s = Name2Symbol(p->Name));
		free(s);
	}
	FlushIf(nevars);

	for (p = all_envars, count = 0; p != 0; p = p->nst) {
		if (!count++) {
			write_lines(nevars, middle);
			BeginIf();
			WriteIndexStruct(nevars, all_envars, "Vars");
		}
#if OPT_IFDEF_MODES
		WriteIf(nevars, p->Cond);
		Sprintf(temp, "#define\tEV%s", p->Func);
		(void)PadTo(24, temp);
		Sprintf(temp + strlen(temp), "OFFSETOF(IndexVars, %s)",
			s = Name2Symbol(p->Name));
		free(s);
		Fprintf(nevars, "%s\n", temp);
#else
		Sprintf(temp, "#define\tEV%s", p->Func);
		Fprintf(nevars, "%s%d\n", PadTo(24, temp), count-1);
#endif
	}
	FlushIf(nevars);
}

/******************************************************************************/
static void
save_funcs(func, flags, cond, old_cond)
char	*func;
char	*flags;
char	*cond;
char	*old_cond;
{
	char	temp[MAX_BUFFER];
	register char	*s;

	if (strcmp(cond, old_cond)) {
		if (*old_cond) {
			SaveEndif(all_funcs);
			SaveEndif(all__FUNCs);
		}
		if (*cond) {
			Sprintf(temp, "#if %s", cond);
			InsertOnEnd(&all_funcs, temp);
			InsertOnEnd(&all__FUNCs, temp);
		}
		(void)strcpy(old_cond, cond);
	}
	Sprintf(temp, "extern int %s P(( int, int ));", func);
	InsertOnEnd(&all_funcs, temp);

	s = append(strcpy(temp, "\tCMDFUNC f_"), func);
	(void)PadTo(32, temp);
	s = append(append(append(s, "= { "), func), ",");
	(void)PadTo(56, temp);
	(void)append(append(s, flags), " };");
	InsertOnEnd(&all__FUNCs, temp);
}

static void
dump_funcs(head)
LIST	*head;
{
	register LIST *p;
	for (p = head; p != 0; p = p->nst)
		Fprintf(nefunc, "%s\n", p->Name);
}

/******************************************************************************/
static void
save_gmodes(type, vec)
char	*type;
char	**vec;
{
	InsertSorted(&all_gmodes, Mode2Key(type,vec[1],vec[4]), vec[2], vec[3], vec[4], vec[0]);
}

static void
dump_gmodes()
{
	static	char	*top[] = {
		"",
		"/* global mode flags\t*/",
		"/* the indices of G_VALUES.v[] */",
		};
	static	char	*middle[] = {
		"",
		"typedef struct G_VALUES {",
		"\t/* each entry is a val, and a ptr to a val */",
		"\tstruct VAL gv[MAX_G_VALUES+1];",
		"} G_VALUES;",
		"",
		"#ifdef realdef",
		"struct VALNAMES g_valuenames[] = {",
		};
	static	char	*bottom[] = {
		"",
		"\t{ NULL,\tNULL,\tVALTYPE_INT, 0 }",
		"};",
		"#else",
		"extern struct VALNAMES g_valuenames[];",
		"#endif",
		};

	write_lines(nemode, top);
	WriteIndexStruct(nemode, all_gmodes, "Globals");
	WriteModeDefines(all_gmodes, "Globals");
	write_lines(nemode, middle);
	WriteModeSymbols(all_gmodes);
	write_lines(nemode, bottom);
}

/******************************************************************************/
static void
save_names(name,func,cond)
char *name, *func, *cond;
{
	char tmpline[80];

	Sprintf(tmpline,"\t{ \"%s\",\t&f_%s },\n", name, func);
	InsertSorted(&all_names, name, func, "", cond, "");
}

static void
dump_names()
{
	register LIST *m;
	char	temp[MAX_BUFFER];

	Fprintf(nename,"\n/* if you maintain this by hand, keep it in */\n");
	Fprintf(nename,"/* alphabetical order!!!! */\n\n");
	Fprintf(nename,"NTAB nametbl[] = {\n");

	BeginIf();
	for (m = all_names; m != NULL; m = m->nst) {
		WriteIf(nename, m->Cond);
		Sprintf(temp, "\t{ \"%s\",", m->Name);
		Fprintf(nename, "%s&f_%s },\n", PadTo(40, temp), m->Func);
	}
	FlushIf(nename);
	Fprintf(nename,"\t{ NULL, NULL }\n};\n");
}

/******************************************************************************/
static void
init_ufuncs()
{
	static	char	*head[] = {
		"",
		"/*\tlist of recognized user functions\t*/",
		"",
		"typedef struct UFUNC {",
		"\tchar *f_name;\t/* name of function */",
		"\tint f_type;\t/* 1 = monamic, 2 = dynamic */",
		"} UFUNC;",
		"",
		"#define\tNILNAMIC\t0",
		"#define\tMONAMIC\t\t1",
		"#define\tDYNAMIC\t\t2",
		"#define\tTRINAMIC\t3",
		"",
		"#ifdef realdef",
		"UFUNC funcs[] = {",
		};
	static	int	done;

	if (!done++)
		write_lines(nevars, head);
}

static void
save_ufuncs(vec)
char	**vec;
{
	InsertSorted(&all_ufuncs, vec[1], vec[2], vec[3], "", vec[0]);
}

static void
dump_ufuncs()
{
	static	char	*middle[] = {
		"};",
		"#else",
		"extern UFUNC funcs[];",
		"#endif",
		"",
		"/* \tand its preprocesor definitions\t\t*/",
		"",
		};
	char	temp[MAX_BUFFER];
	register LIST *p;
	register int	count;

	for (p = all_ufuncs, count = 0; p != 0; p = p->nst) {
		if (!count++)
			init_ufuncs();
		Sprintf(temp, "\t{\"%s\",", p->Name);
		(void)PadTo(15, temp);
		Sprintf(temp+strlen(temp), "%s},", p->Data);
		if (p->Note[0]) {
			(void)PadTo(32, temp);
			Sprintf(temp+strlen(temp), "/* %s */", p->Note);
		}
		Fprintf(nevars, "%s\n", temp);
	}
	for (p = all_ufuncs, count = 0; p != 0; p = p->nst) {
		if (!count)
			write_lines(nevars, middle);
		Sprintf(temp, "#define\tUF%s", p->Func);
		Fprintf(nevars, "%s%d\n", PadTo(24, temp), count++);
	}
	Fprintf(nevars, "\n#define\tNFUNCS\t\t%d\n", count);
}

/******************************************************************************/
static void
save_wmodes(type, vec)
char	*type;
char	**vec;
{
	InsertSorted(&all_wmodes, Mode2Key(type,vec[1],vec[4]), vec[2], vec[3], vec[4], vec[0]);
}

static void
dump_wmodes()
{
	static	char	*top[] = {
		"",
		"/* these are the boolean, integer, and pointer value'd settings that are",
		"\tassociated with a window, and usually settable by a user.  There",
		"\tis a global set that is inherited into a buffer, and its windows",
		"\tin turn are inherit the buffer's set. */",
		};
	static	char	*middle[] = {
		"",
		"typedef struct W_VALUES {",
		"\t/* each entry is a val, and a ptr to a val */",
		"\tstruct VAL wv[MAX_W_VALUES+1];",
		"} W_VALUES;",
		"",
		"#ifdef realdef",
		"struct VALNAMES w_valuenames[] = {",
		};
	static	char	*bottom[] = {
		"",
		"\t{ NULL,\tNULL,\tVALTYPE_INT, 0 }",
		"};",
		"#else",
		"extern struct VALNAMES w_valuenames[];",
		"#endif",
		};

	write_lines(nemode, top);
	WriteIndexStruct(nemode, all_wmodes, "Windows");
	WriteModeDefines(all_wmodes, "Windows");
	write_lines(nemode, middle);
	WriteModeSymbols(all_wmodes);
	write_lines(nemode, bottom);
}

/******************************************************************************/
int
main(argc, argv)
int	argc;
char    *argv[];
{
	char *vec[MAX_PARSE];
	char line[MAX_BUFFER];
	char func[LEN_BUFFER];
	char flags[LEN_BUFFER];
	char old_fcond[LEN_BUFFER],	fcond[LEN_BUFFER];
	char modetype[LEN_BUFFER];
	int section = SECT_CMDS;
	int r;

	if (argc != 2) {
		Fprintf(stderr, "usage: mktbls cmd-file\n");
		exit(BAD(1));
	}

	if ((cmdtbl = fopen(inputfile = argv[1],"r")) == NULL ) {
		Fprintf(stderr,"mktbls: couldn't open cmd-file\n");
		exit(BAD(1));
	}

	*old_fcond = '\0';

	/* process each input line */
	while (fgets(line, sizeof(line), cmdtbl) != NULL) {
		char	col0	= line[0],
			col1	= line[1];

		l++;
		r = Parse(line, vec);

		switch (col0) {
		case '#':		/* comment */
		case '\n':		/* empty-list */
			break;

		case '.':		/* a new section */
			switch (col1) {
			case 'c':
				section = SECT_CMDS;
				break;
			case 'e':
				section = SECT_VARS;
				start_evar_h(argv);
				break;
			case 'f':
				section = SECT_FUNC;
				start_evar_h(argv);
				break;
			case 'g':
				section = SECT_GBLS;
				break;
			case 'b':
				section = SECT_BUFF;
				break;
			case 'w':
				section = SECT_WIND;
				break;
			default:
				badfmt("unknown section");
			}
			break;

		case '\t':		/* a new function */
			switch (section) {
			case SECT_CMDS:
				switch (col1) {
				case '"':	/* then it's an english name */
					if (r < 1 || r > 2)
						badfmt("looking for english name");

					save_names(vec[1], func, formcond(fcond,vec[2]));
					break;

				case '\'':	/* then it's a key */
					if (r < 1 || r > 2)
						badfmt("looking for key binding");

					save_bindings(vec[1], func, formcond(fcond,vec[2]));
					break;

				default:
					badfmt("bad line");
				}
				break;

			case SECT_GBLS:
				if (r < 2 || r > 4)
					badfmt("looking for GLOBAL modes");
				save_gmodes(modetype, vec);
				break;

			case SECT_BUFF:
				if (r < 2 || r > 4)
					badfmt("looking for BUFFER modes");
				save_bmodes(modetype, vec);
				break;

			case SECT_WIND:
				if (r < 2 || r > 4)
					badfmt("looking for WINDOW modes");
				save_wmodes(modetype, vec);
				break;

			default:
				badfmt("did not expect a tab");
			}
			break;

		default:		/* cache information about funcs */
			switch (section) {
			case SECT_CMDS:
				if (r < 2 || r > 3)
					badfmt("looking for new function");

				save_funcs(
					strcpy(func,  vec[1]),
					strcpy(flags, vec[2]),
					strcpy(fcond, vec[3]),
					old_fcond);
				break;

			case SECT_VARS:
				if (r < 2 || r > 3)
					badfmt("looking for char *envars[]");
				save_envars(vec);
				break;

			case SECT_FUNC:
				if (r < 2 || r > 3)
					badfmt("looking for UFUNC func[]");
				save_ufuncs(vec);
				break;

			case SECT_GBLS:
			case SECT_BUFF:
			case SECT_WIND:
				if (r != 1
				 || (!strcmp(vec[1], "bool")
				  && !strcmp(vec[1], "color")
				  && !strcmp(vec[1], "int")
				  && !strcmp(vec[1], "string")
				  && !strcmp(vec[1], "regex")))
					badfmt("looking for mode datatype");
				(void)strcpy(modetype, vec[1]);
				break;

			default:
				badfmt("section not implemented");
			}
		}
	}

	if (*old_fcond) {
		SaveEndif(all_funcs);
		SaveEndif(all__FUNCs);
	}

	if (all_names) {
		nebind = OpenHeader("nebind.h", argv);
		nefunc = OpenHeader("nefunc.h", argv);
		nename = OpenHeader("nename.h", argv);
		dump_names();
		dump_bindings();
		dump_funcs(all_funcs);
		dump_funcs(all__FUNCs);
	}

	if (all_envars) {
		dump_envars();
		dump_ufuncs();
		finish_evar_h();
	}

	if (all_wmodes || all_bmodes) {
		nemode = OpenHeader("nemode.h", argv);
		DefineOffset(nemode);
		dump_all_modes();
		dump_gmodes();
		dump_wmodes();
		dump_bmodes();
	}

	return(GOOD);
	/*NOTREACHED*/
}

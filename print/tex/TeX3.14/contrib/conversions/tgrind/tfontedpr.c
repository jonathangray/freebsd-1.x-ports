#ifndef lint
static char sccsid[] = "@(#)@(#)tfontedpr.c	1.3 (LBL) 4/12/85";
#endif

/* tfontedpr - general purpose "pretty printer" for use with TeX.
 *
 * Copyright (C) 1985 by Van Jacobson, Lawrence Berkeley Laboratory.
 * This program may be freely used and copied but may not be sold
 * without the author's written permission.  This notice must remain
 * in any copy or derivative.
 *
 * This program is used as part of the "tgrind" shell script.  It
 * converts program source file(s) to TeX input files.
 *
 * This program is an adaptation of "vfontedpr" v4.2 (12/11/84) from
 * the 4.2bsd Unix distribution.  Vfontedpr was written by Dave
 * Presotto (based on an earlier program of the same name written by
 * Bill Joy).
 *
 * I would welcome comments, enhancements, bug fixes, etc.  Please 
 * mail them to:
 *	van@lbl-rtsg.arpa	(from arpanet, milnet, csnet, etc.)
 *	..!ucbvax!lbl-csam!van	(from Usenet/UUCP)
 *
 * Modifications.
 * --------------
 * 30Mar85  Chris & Van: Fixed "\C" & "\S" (comment & string start indicators)
 *			to really appear at the start of comments & strings.
 *			Changes for speeded-up expmatch.
 * 29Mar85  Chris Torek (chris@maryland):  Bug fixes for '~' and '^L'
 *			output.  Most cpu-time eaters recoded to improve
 *			efficiency.
 * 10Feb85  Van		Written.
 */

#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define boolean int
#define TRUE 1
#define FALSE 0
#define NIL 0
#define STANDARD 0
#define ALTERNATE 1

#define STRLEN 10		/* length of strings introducing things */
#define PNAMELEN 80		/* length of a function/procedure name */
#define PSMAX 20		/* size of procedure name stacking */

/* regular expression routines */

char	*expmatch();		/* match a string to an expression */
char	*convexp();		/* convert expression to internal form */
char	*tgetstr();

boolean	isproc();


char	*ctime();

/*
 *	The state variables
 */

boolean	incomm;			/* in a comment of the primary type */
boolean	instr;			/* in a string constant */
boolean	inchr;			/* in a string constant */
boolean	nokeyw = FALSE;		/* no keywords being flagged */
boolean prccont;		/* continue last procedure */
int	comtype;		/* type of comment */
int	psptr;			/* the stack index of the current procedure */
char	pstack[PSMAX][PNAMELEN+1];	/* the procedure name stack */
int	plstack[PSMAX];		/* the procedure nesting level stack */
int	blklevel;		/* current nesting level */
char	*defsfile = DEFSFILE;	/* name of language definitions file */
char	pname[BUFSIZ+1];

/*
 *	The language specific globals
 */

char	*language = "c";	/* the language indicator */
char	*l_keywds[BUFSIZ/2];	/* keyword table address */
char	*l_prcbeg;		/* regular expr for procedure begin */
char	*l_combeg;		/* string introducing a comment */
char	*l_comend;		/* string ending a comment */
char	*l_acmbeg;		/* string introducing a comment */
char	*l_acmend;		/* string ending a comment */
char	*l_blkbeg;		/* string begining of a block */
char	*l_blkend;		/* string ending a block */
char    *l_strbeg;		/* delimiter for string constant */
char    *l_strend;		/* delimiter for string constant */
char    *l_chrbeg;		/* delimiter for character constant */
char    *l_chrend;		/* delimiter for character constant */
char	l_escape;		/* character used to escape characters */
boolean	l_toplex;		/* procedures only defined at top lex level */
boolean	l_onecase;		/* upper & lower case equivalent */

/*
 *  global variables also used by expmatch
 */
extern	boolean	_escaped;	/* if last character was an escape */
extern	char *_start;		/* start of the current string */

int	(*re_strncmp)();	/* function to do string compares */
extern	int strncmp();
extern	int lc_strncmp();

/*
 * The following table converts ASCII characters to a printed
 * representation, taking care of all the TeX quoting.  N.B.: all
 * single-character strings are assumed to be equivalent to the
 * character for that index (i.e., printtab['c'] can't be "f").
 * (This is purely for efficiency hacking.)
 */
char *printtab[128] = {
    "\0x",   "\\^A",  "\\^B",  "\\^C",  "\\^D",  "\\^E",  "\\^F",  "\\^G",
    "\\^H",  "\t",    "}}\n",  "\\^K",  "\0x",   "\\^M",  "\\^N",  "\\^O",
    "\\^P",  "\\^Q",  "\\^R",  "\\^S",  "\\^T",  "\\^U",  "\\^V",  "\\^W",
    "\\^X",  "\\^Y",  "\\^Z",  "\\^[",  "\\^\\!","\\^]",  "\\^\\^","\\^_",
    " ",     "!",     "\\\"",  "\\#",   "\\$",   "\\%",   "\\&",   "\\'",
    "(",     ")",     "*",     "+",     ",",     "\\-",   ".",     "\\/",
    "0",     "1",     "2",     "3",     "4",     "5",     "6",     "7",
    "8",     "9",     ":",     ";",     "\\<",   "=",     "\\>",   "?",
    "@",     "A",     "B",     "C",     "D",     "E",     "F",     "G",
    "H",     "I",     "J",     "K",     "L",     "M",     "N",     "O",
    "P",     "Q",     "R",     "S",     "T",     "U",     "V",     "W",
    "X",     "Y",     "Z",     "[",     "\\!",   "]",     "\\^",   "\\_",
    "`",     "a",     "b",     "c",     "d",     "e",     "f",     "g",
    "h",     "i",     "j",     "k",     "l",     "m",     "n",     "o",
    "p",     "q",     "r",     "s",     "t",     "u",     "v",     "w",
    "x",     "y",     "z",     "\\{",   "\\|",   "\\}",   "\\~",   "\\^?",
};

/* Output a character, with translation.  Avoid side effects with this
   macro! */
#define outchar(c) (printtab[c][1] ? printf("%s", printtab[c]) : putchar(c))

/*
 * Output a TeX command to tab to column "col" (see tgrindmac.tex for a 
 * partial explanation of the bizarre brace arrangement).
 */
#define tabto(col) printf("}\\Tab{%d}{", col);

main(argc, argv)
    int argc;
    register char *argv[];
{
    char *fname = "", *p;
    struct stat stbuf;
    char buf[BUFSIZ];
    char strings[2 * BUFSIZ];
    char defs[2 * BUFSIZ];

    printf("\\input tgrindmac\n");
    argc--, argv++;
    do {
	register char *cp;
	register int i;

	if (argc > 0) {
	    if (!strcmp(argv[0], "-h")) {
		if (argc == 1) {
		    printf("\\Head{}\n");
		    argc = 0;
		    goto rest;
		}
		printf("\\Head{");
		putstr( argv[1] );
		printf( "}\n" );
		argc--, argv++;
		argc--, argv++;
		if (argc > 0)
		    continue;
		goto rest;
	    }

	    /* take input from the standard place */
	    if (!strcmp(argv[0], "-")) {
		argc = 0;
		goto rest;
	    }

	    /* indicate no keywords */
	    if (!strcmp(argv[0], "-n")) {
		nokeyw++;
		argc--, argv++;
		continue;
	    }

	    /* specify the language */
	    if (!strncmp(argv[0], "-l", 2)) {
		language = argv[0]+2;
		argc--, argv++;
		continue;
	    }

	    /* specify the language description file */
	    if (!strncmp(argv[0], "-d", 2)) {
		defsfile = argv[1];
		argc--, argv++;
		argc--, argv++;
		continue;
	    }

	    /* open the file for input */
	    if (freopen(argv[0], "r", stdin) == NULL) {
		perror(argv[0]);
		exit(1);
	    }

	    fname = argv[0];
	    argc--, argv++;
	}
    rest:

	/*
	 *  get the  language definition from the defs file
	 */
	i = tgetent (defs, language, defsfile);
	if (i == 0) {
	    fprintf (stderr, "no entry for language %s\n", language);
	    exit (0);
	} else  if (i < 0) {
	    fprintf (stderr,  "cannot find vgrindefs file %s\n", defsfile);
	    exit (0);
	}
	p = strings;
	if (tgetstr ("kw", &p) == NIL)
	    nokeyw = TRUE;
	else  {
	    char **cpp;

	    cpp = l_keywds;
	    cp = strings;
	    while (*cp) {
		while (*cp == ' ' || *cp =='\t')
		    *cp++ = NULL;
		if (*cp)
		    *cpp++ = cp;
		while (*cp != ' ' && *cp  != '\t' && *cp)
		    cp++;
	    }
	    *cpp = NIL;
	}
	p = buf;
	l_prcbeg = convexp (tgetstr ("pb", &p));
	p = buf;
	l_combeg = convexp (tgetstr ("cb", &p));
	p = buf;
	l_comend = convexp (tgetstr ("ce", &p));
	p = buf;
	l_acmbeg = convexp (tgetstr ("ab", &p));
	p = buf;
	l_acmend = convexp (tgetstr ("ae", &p));
	p = buf;
	l_strbeg = convexp (tgetstr ("sb", &p));
	p = buf;
	l_strend = convexp (tgetstr ("se", &p));
	p = buf;
	l_blkbeg = convexp (tgetstr ("bb", &p));
	p = buf;
	l_blkend = convexp (tgetstr ("be", &p));
	p = buf;
	l_chrbeg = convexp (tgetstr ("lb", &p));
	p = buf;
	l_chrend = convexp (tgetstr ("le", &p));
	l_escape = '\\';
	l_onecase = tgetflag ("oc");
	if ( l_onecase )
	    re_strncmp = lc_strncmp;
	else
	    re_strncmp = strncmp;
	l_toplex = tgetflag ("tl");

	/* initialize the program */

	incomm = FALSE;
	instr = FALSE;
	inchr = FALSE;
	_escaped = FALSE;
	blklevel = 0;
	for (psptr=0; psptr<PSMAX; psptr++) {
	    pstack[psptr][0] = NULL;
	    plstack[psptr] = 0;
	}
	psptr = -1;
	fstat(fileno(stdin), &stbuf);
	cp = ctime(&stbuf.st_mtime);
	cp[10] = '\0';
	cp[16] = '\0';
	cp[24] = '\0';
	printf("\\File{");
	putstr( fname );
	printf("},{%s},{%s %s}\n", cp+11, cp+4, cp+20);

	/*
	 *	MAIN LOOP!!!
	 */
	while (fgets(buf, sizeof buf, stdin) != NULL) {
	    cp = buf;
	    if (*cp == '\f') {
		printf("\\NewPage\n");
		cp++;
		if (*cp == '\n')/* some people like ^Ls on their own line */
		    continue;
	    }
	    prccont = FALSE;
	    printf("\\L{\\LB{");
	    putScp(cp);
	    if (prccont && (psptr >= 0)) {
	    	printf("\\ProcCont{");
		putstr( pstack[psptr] );
		printf("}");
	    }
#ifdef DEBUG
	    printf ("com %o str %o chr %o ptr %d\n", incomm, instr, inchr, psptr);
#endif
	}
    } while (argc > 0);
    printf("\\vfill\\eject\\end\n");
    exit(0);
}

#define isidchr(c) (isalnum(c) || (c) == '_')

putScp(os)
    char *os;
{
    register char *s = os;		/* pointer to unmatched string */
    register char *s1;			/* temp. string */
    char *comptr;			/* start of a comment delimiter */
    char *comendptr;			/* end of a comment delimiter */
    char *acmptr;			/* start of an alt. comment delimiter */
    char *acmendptr;			/* end of an alt. comment delimiter */
    char *strptr;			/* start of a string delimiter */
    char *strendptr;			/* end of a string delimiter */
    char *chrptr;			/* start of a char. const delimiter */
    char *chrendptr;			/* end of a char. const delimiter */
    char *blksptr;			/* start of a lexical block start */
    char *blksendptr;			/* end of a lexical block start */
    char *blkeptr;			/* start of a lexical block end */
    char *blkeendptr;			/* end of a lexical block end */

    _start = os;			/* remember the start for expmatch */
    _escaped = FALSE;
    if (nokeyw || incomm || instr)
	goto skip;
    if (isproc(s)) {
	printf("\\Proc{");
	putstr(pname);
	printf("}");
	if (psptr < PSMAX) {
	    ++psptr;
	    strncpy (pstack[psptr], pname, PNAMELEN);
	    pstack[psptr][PNAMELEN] = NULL;
	    plstack[psptr] = blklevel;
	}
    } 
skip:
    do {
	/* check for string, comment, blockstart, etc */
	if (!incomm && !instr && !inchr) {

	    blkeendptr = expmatch (s, l_blkend, &blkeptr, NIL);
	    blksendptr = expmatch (s, l_blkbeg, &blksptr, NIL);
	    comendptr = expmatch (s, l_combeg, &comptr, NIL);
	    acmendptr = expmatch (s, l_acmbeg, &acmptr, NIL);
	    strendptr = expmatch (s, l_strbeg, &strptr, NIL);
	    chrendptr = expmatch (s, l_chrbeg, &chrptr, NIL);

	    /* start of a comment? */
	    if (comptr != NIL
		  && (strptr  == NIL || comptr < strptr)
		  && (acmptr  == NIL || comptr < acmptr)
		  && (chrptr  == NIL || comptr < chrptr)
		  && (blksptr == NIL || comptr < blksptr)
		  && (blkeptr == NIL || comptr < blkeptr)) {
		    putKcp (s, comptr-1, FALSE);
		    printf("\\C{}");
		    s = comendptr;
		    putKcp (comptr, comendptr-1, FALSE);
		    incomm = TRUE;
		    comtype = STANDARD;
		    continue;
		}

	    /* start of an alternate-form comment? */
	    if (acmptr != NIL
		  && (strptr  == NIL || acmptr < strptr)
		  && (chrptr  == NIL || acmptr < chrptr)
		  && (blksptr == NIL || acmptr < blksptr)
		  && (blkeptr == NIL || acmptr < blkeptr)) {
		    putKcp (s, acmptr-1, FALSE);
		    printf("\\C{}");
		    s = acmendptr;
		    putKcp (acmptr, acmendptr, FALSE);
		    incomm = TRUE;
		    comtype = ALTERNATE;
		    continue;
		}

	    /* start of a string? */
	    if (strptr != NIL
		  && (chrptr  == NIL || strptr < chrptr)
		  && (blksptr == NIL || strptr < blksptr)
		  && (blkeptr == NIL || strptr < blkeptr)) {
		    putKcp (s, strptr-1, FALSE);
		    printf("\\S{}");
		    s = strendptr;
		    putKcp (strptr,strendptr-1, FALSE);
		    instr = TRUE;
		    continue;
		}

	    /* start of a character string? */
	    if (chrptr != NIL
		  && (blksptr == NIL || chrptr < blksptr)
		  && (blkeptr == NIL || chrptr < blkeptr)) {
		    putKcp (s, chrptr-1, FALSE);
		    printf("\\S{}");
		    s = chrendptr;
		    putKcp (chrptr, chrendptr-1, FALSE);
		    inchr = TRUE;
		    continue;
		}

	    /* end of a lexical block */
	    if (blkeptr != NIL) {
		if (blksptr == NIL || blkeptr < blksptr) {
		    putKcp (s, blkeendptr - 1, FALSE);
		    s = blkeendptr;
		    blklevel--;
		    if (psptr >= 0 && plstack[psptr] >= blklevel) {

			/* end of current procedure */
			blklevel = plstack[psptr];

			/* see if we should print the last proc name */
			if (--psptr >= 0)
			    prccont = TRUE;
			else
			    psptr = -1;
		    }
		    continue;
		}
	    }

	    /* start of a lexical block */
	    if (blksptr != NIL) {
		putKcp (s, blksendptr - 1, FALSE);
		s = blksendptr;
		blklevel++;
		continue;
	    }

	/* check for end of comment */
	} else if (incomm) {
	    if ((comendptr = expmatch( s,
				       comtype==STANDARD? l_comend : l_acmend,
			               NIL, NIL)) != NIL) {
		putKcp (s, comendptr-1, TRUE);
		s = comendptr;
		incomm = FALSE;
		printf("\\CE{}");
	    } else {
		comptr = s;
		s += strlen(s);
		putKcp (comptr, s-1, TRUE);
	    }
	    continue;

	/* check for end of string */
	} else if (instr) {
	    if ((strendptr = expmatch (s, l_strend, NIL, NIL)) != NIL) {
		putKcp (s, strendptr-1, TRUE);
		s = strendptr;
		instr = FALSE;
		printf("\\SE{}");
	    } else {
		strptr = s;
		s += strlen(s);
		putKcp (strptr, s-1, TRUE);
	    }
	    continue;

	/* check for end of character string */
	} else if (inchr) {
	    if ((chrendptr = expmatch (s, l_chrend, NIL, NIL)) != NIL) {
		putKcp (s, chrendptr-1, TRUE);
		s = chrendptr;
		inchr = FALSE;
		printf("\\SE{}");
	    } else {
		chrptr = s;
		s += strlen(s);
		putKcp (chrptr, s-1, TRUE);
	    }
	    continue;
	}

	/* print out the line */
	chrptr = s;
	s += strlen(s);
	putKcp (chrptr, s-1, FALSE);

    } while (*s);
}

putKcp(start, end, nix)
	register char *start;	/* start of string to write */
	register char *end;	/* end of string to write */
	register boolean nix;	/* true if we should force nokeyw */
{
	register int i, c;

	if (nokeyw)
		nix = TRUE;

	while (start <= end) {
		c = *start++;
		/* take care of nice tab stops */
		if (c == '\t') {
			while (start <= end && *start == '\t')
				start++;
			tabto(width(_start, start));
			continue;
		}
		if (!nix && (isidchr(c) ||
		 	     c == '#'   || c == '%' || c == '!' || c == '$')) {
			/* potential keyword */
			start--;
			if (start == _start || !isidchr(start[-1])) {
				i = iskw(start);
				if (i > 0) {
					printf("\\K{");
					while (--i >= 0) {
						c = *start++;
						outchar(c);
					}
					putchar('}');
					continue;
				}
			}
			start++;
		}
		outchar(c);
	}
}


width(s, os)
	register char *s, *os;
{
	register int i = 0, c;

	while (s < os) {
		c = *s++;
		if (c == '\t') {
			i = (i + 8) &~ 7;
			continue;
		}
		if (c < ' ')
			i += 2;
		else
			i++;
	}
	return (i);
}

/* output a string, escaping special characters */
putstr(cp)
	register char *cp;
{
	register int c;

	if (cp == NULL)
		return;
	while ((c = *cp++) != 0)
		outchar(c);
}

/*
 *	look for a process beginning on this line
 */
boolean
isproc(s)
	char *s;
{
	pname[0] = NULL;
	if ((!l_toplex || blklevel == 0)
	    && expmatch(s, l_prcbeg, NIL, pname) != NIL)
		return (TRUE);
	return (FALSE);
}


/*  iskw -	check to see if the next word is a keyword
 */

iskw(s)
	register char *s;
{
	register char **ss = l_keywds;
	register int i = 1;
	register char *cp = s;
	register int firstc = *s;

	while (++cp, isidchr(*cp))
		i++;
	while (cp = *ss++) {
		if (!l_onecase && firstc != *cp)
			continue;
		if ((*re_strncmp)(s, cp, i) == 0 && !isidchr(cp[i]))
			return (i);
	}
	return (0);
}

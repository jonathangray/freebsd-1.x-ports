
/*  A Bison parser, made from dbg.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	CONT	258
#define	QUIT	259
#define	HELP	260
#define	BACKTRACE	261
#define	INFO	262
#define	STACK	263
#define	REG	264
#define	REGS	265
#define	NUM	266
#define	ENABLE	267
#define	DISABLE	268
#define	BREAK	269
#define	SET	270
#define	MODE	271
#define	PRINT	272
#define	IDENTIFIER	273
#define	NO_SYMBOL	274
#define	SYMBOLFILE	275
#define	DEFINE	276
#define	ABORT	277

#line 2 "dbg.y"


/* Parser for command lines in the Wine debugger
 *
 * Version 1.0
 * Eric Youngdale
 * 9/93
 */

#include <stdio.h>
#include <signal.h>

#define YYSTYPE int

#include "regpos.h"
extern FILE * yyin;
unsigned int * regval = NULL;
unsigned int dbg_mask = 0;
unsigned int dbg_mode = 0;

void issue_prompt(void);
void mode_command(int);

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		103
#define	YYFLAG		-32768
#define	YYNTBASE	40

#define YYTRANSLATE(x) ((unsigned)(x) <= 277 ? yytranslate[x] : 50)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    23,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    38,
    39,    26,    36,     2,    37,     2,    29,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    27,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    35,    25,    31,
     2,     2,     2,     2,    32,     2,     2,     2,     2,     2,
     2,    30,    24,     2,    34,     2,     2,     2,    33,    28,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     1,     4,     6,     9,    12,    15,    18,    21,    24,
    27,    30,    34,    39,    42,    45,    48,    52,    54,    57,
    59,    61,    67,    74,    80,    84,    90,    97,    99,   101,
   105,   111,   118,   120,   122,   124,   126,   128,   130,   132,
   134,   136,   138,   140,   144,   148,   152,   155,   158,   161
};

static const short yyrhs[] = {    -1,
    40,    41,     0,    23,     0,    49,    23,     0,     1,    23,
     0,     4,    23,     0,    24,    23,     0,     5,    23,     0,
     3,    23,     0,    25,    23,     0,    22,    23,     0,    20,
    18,    23,     0,    21,    18,    48,    23,     0,    16,    11,
     0,    12,    11,     0,    13,    11,     0,    14,    26,    48,
     0,    43,     0,     6,    23,     0,    45,     0,    42,     0,
    15,     9,    27,    48,    23,     0,    15,    26,    48,    27,
    48,    23,     0,    15,    47,    27,    48,    23,     0,    28,
    48,    23,     0,    28,    29,    46,    48,    23,     0,    28,
    29,    11,    46,    48,    23,     0,    30,     0,    44,     0,
    44,    48,    23,     0,    44,    29,    46,    48,    23,     0,
    44,    29,    11,    46,    48,    23,     0,    28,     0,    31,
     0,    32,     0,    33,     0,    34,     0,    25,     0,    35,
     0,    18,     0,    11,     0,     9,     0,    47,     0,    48,
    36,    11,     0,    48,    37,    11,     0,    38,    48,    39,
     0,    26,    48,     0,     7,    10,     0,     7,     8,     0,
     7,    14,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    50,    51,    53,    54,    55,    56,    57,    58,    59,    60,
    61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
    71,    73,    75,    76,    79,    81,    82,    84,    86,    88,
    90,    91,    93,    94,    95,    96,    97,    98,    99,   101,
   108,   109,   110,   111,   112,   113,   114,   116,   117,   118
};

static const char * const yytname[] = {   "$","error","$illegal.","CONT","QUIT",
"HELP","BACKTRACE","INFO","STACK","REG","REGS","NUM","ENABLE","DISABLE","BREAK",
"SET","MODE","PRINT","IDENTIFIER","NO_SYMBOL","SYMBOLFILE","DEFINE","ABORT",
"'\\n'","'q'","'c'","'*'","'='","'x'","'/'","'p'","'d'","'i'","'w'","'s'","'b'",
"'+'","'-'","'('","')'","input","line","deposit_command","x_command","print",
"print_command","fmt","symbol","expr","infocmd",""
};
#endif

static const short yyr1[] = {     0,
    40,    40,    41,    41,    41,    41,    41,    41,    41,    41,
    41,    41,    41,    41,    41,    41,    41,    41,    41,    41,
    41,    42,    42,    42,    43,    43,    43,    44,    44,    45,
    45,    45,    46,    46,    46,    46,    46,    46,    46,    47,
    48,    48,    48,    48,    48,    48,    48,    49,    49,    49
};

static const short yyr2[] = {     0,
     0,     2,     1,     2,     2,     2,     2,     2,     2,     2,
     2,     3,     4,     2,     2,     2,     3,     1,     2,     1,
     1,     5,     6,     5,     3,     5,     6,     1,     1,     3,
     5,     6,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     3,     3,     3,     2,     2,     2,     2
};

static const short yydefact[] = {     1,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     3,     0,     0,     0,    28,
     2,    21,    18,     0,    20,     0,     5,     9,     6,     8,
    19,    49,    48,    50,    15,    16,     0,     0,    40,     0,
     0,    14,     0,     0,    11,     7,    10,    42,    41,     0,
     0,     0,    43,     0,     0,     0,     4,    17,     0,     0,
     0,    12,     0,    47,     0,    38,    33,    34,    35,    36,
    37,    39,     0,     0,    25,     0,     0,     0,     0,    30,
     0,     0,     0,    13,     0,     0,    46,    44,    45,     0,
     0,    22,     0,    24,     0,    26,     0,    31,    23,    27,
    32,     0,     0
};

static const short yydefgoto[] = {     1,
    21,    22,    23,    24,    25,    73,    53,    54,    26
};

static const short yypact[] = {-32768,
    18,   -17,    -8,    -6,     6,    13,    -7,    33,    60,    47,
    82,    61,    76,    84,    89,-32768,    94,    98,    36,-32768,
-32768,-32768,-32768,    41,-32768,   105,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    42,   108,-32768,    42,
   110,-32768,   115,    42,-32768,-32768,-32768,-32768,-32768,    42,
    53,    42,-32768,    40,    64,    46,-32768,    20,    42,    83,
    42,-32768,    67,    20,   -23,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    42,    97,-32768,   128,   129,   -23,    42,-32768,
    70,    42,    78,-32768,    42,    86,-32768,-32768,-32768,    42,
    88,-32768,    90,-32768,    93,-32768,    95,-32768,-32768,-32768,
-32768,   141,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,   -51,   131,   -24,-32768
};


#define	YYLAST		142


static const short yytable[] = {    56,
    32,    66,    33,    79,    67,    27,    34,    68,    69,    70,
    71,    72,    58,    85,    28,    60,    29,   102,     2,    63,
     3,     4,     5,     6,     7,    64,    90,    74,    30,     8,
     9,    10,    11,    12,    81,    31,    83,    13,    14,    15,
    16,    17,    18,    35,    48,    19,    49,    20,    86,    48,
    48,    49,    49,    39,    91,    76,    77,    93,    39,    39,
    95,    50,    75,    65,    51,    97,    50,    50,    80,    55,
    36,    42,    37,    52,    78,    76,    77,    66,    52,    52,
    67,    76,    77,    68,    69,    70,    71,    72,    66,    84,
    38,    67,    92,    43,    68,    69,    70,    71,    72,    39,
    94,    44,    76,    77,     0,    76,    77,    40,    96,    82,
    98,    45,    99,    76,    77,   100,    46,   101,    76,    77,
    47,    76,    77,    76,    77,    76,    77,    57,    76,    77,
    76,    77,    76,    77,    59,    87,    61,    62,    88,    89,
   103,    41
};

static const short yycheck[] = {    24,
     8,    25,    10,    55,    28,    23,    14,    31,    32,    33,
    34,    35,    37,    65,    23,    40,    23,     0,     1,    44,
     3,     4,     5,     6,     7,    50,    78,    52,    23,    12,
    13,    14,    15,    16,    59,    23,    61,    20,    21,    22,
    23,    24,    25,    11,     9,    28,    11,    30,    73,     9,
     9,    11,    11,    18,    79,    36,    37,    82,    18,    18,
    85,    26,    23,    11,    29,    90,    26,    26,    23,    29,
    11,    11,    26,    38,    11,    36,    37,    25,    38,    38,
    28,    36,    37,    31,    32,    33,    34,    35,    25,    23,
     9,    28,    23,    18,    31,    32,    33,    34,    35,    18,
    23,    18,    36,    37,    -1,    36,    37,    26,    23,    27,
    23,    23,    23,    36,    37,    23,    23,    23,    36,    37,
    23,    36,    37,    36,    37,    36,    37,    23,    36,    37,
    36,    37,    36,    37,    27,    39,    27,    23,    11,    11,
     0,    11
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 51 "dbg.y"
{ issue_prompt(); ;
    break;}
case 5:
#line 55 "dbg.y"
{ yyerrok; ;
    break;}
case 6:
#line 56 "dbg.y"
{ exit(0); ;
    break;}
case 7:
#line 57 "dbg.y"
{ exit(0); ;
    break;}
case 8:
#line 58 "dbg.y"
{ dbg_help(); ;
    break;}
case 9:
#line 59 "dbg.y"
{ return; ;
    break;}
case 10:
#line 60 "dbg.y"
{ return; ;
    break;}
case 11:
#line 61 "dbg.y"
{ kill(getpid(), SIGABRT); ;
    break;}
case 12:
#line 62 "dbg.y"
{ read_symboltable(yyvsp[-1]); ;
    break;}
case 13:
#line 63 "dbg.y"
{ add_hash(yyvsp[-2], yyvsp[-1]); ;
    break;}
case 14:
#line 64 "dbg.y"
{ mode_command(yyvsp[0]); ;
    break;}
case 15:
#line 65 "dbg.y"
{ enable_break(yyvsp[0]); ;
    break;}
case 16:
#line 66 "dbg.y"
{ disable_break(yyvsp[0]); ;
    break;}
case 17:
#line 67 "dbg.y"
{ add_break(yyvsp[0]); ;
    break;}
case 19:
#line 69 "dbg.y"
{ dbg_bt(); ;
    break;}
case 22:
#line 74 "dbg.y"
{ if(regval) regval[yyvsp[-3]] = yyvsp[-1]; else application_not_running();;
    break;}
case 23:
#line 75 "dbg.y"
{ *((unsigned int *) yyvsp[-3]) = yyvsp[-1]; ;
    break;}
case 24:
#line 76 "dbg.y"
{ *((unsigned int *) yyvsp[-3]) = yyvsp[-1]; ;
    break;}
case 25:
#line 80 "dbg.y"
{ examine_memory(yyvsp[-1], 1, 'x'); ;
    break;}
case 26:
#line 81 "dbg.y"
{ examine_memory(yyvsp[-1], 1, yyvsp[-2]); ;
    break;}
case 27:
#line 82 "dbg.y"
{ examine_memory(yyvsp[-1], yyvsp[-3], yyvsp[-2]); ;
    break;}
case 30:
#line 89 "dbg.y"
{ examine_memory(((unsigned int) &yyvsp[-1] ), 1, 'x'); ;
    break;}
case 31:
#line 90 "dbg.y"
{ examine_memory((unsigned int) &yyvsp[-1], 1, yyvsp[-2]); ;
    break;}
case 32:
#line 91 "dbg.y"
{ examine_memory((unsigned int) &yyvsp[-1], yyvsp[-3], yyvsp[-2]); ;
    break;}
case 33:
#line 93 "dbg.y"
{ yyval = 'x'; ;
    break;}
case 34:
#line 94 "dbg.y"
{ yyval = 'd'; ;
    break;}
case 35:
#line 95 "dbg.y"
{ yyval = 'i'; ;
    break;}
case 36:
#line 96 "dbg.y"
{ yyval = 'w'; ;
    break;}
case 37:
#line 97 "dbg.y"
{ yyval = 's'; ;
    break;}
case 38:
#line 98 "dbg.y"
{ yyval = 'c'; ;
    break;}
case 39:
#line 99 "dbg.y"
{ yyval = 'b'; ;
    break;}
case 40:
#line 101 "dbg.y"
{ yyval = find_hash(yyvsp[0]);
			           if(yyval == 0xffffffff) {
					   fprintf(stderr,"Symbol %s not found\n", yyvsp[0]);
					   YYERROR;
				   }
			        ;
    break;}
case 41:
#line 108 "dbg.y"
{ yyval = yyvsp[0];	;
    break;}
case 42:
#line 109 "dbg.y"
{ if(regval) yyval = regval[yyvsp[0]]; else application_not_running();;
    break;}
case 43:
#line 110 "dbg.y"
{ yyval = *((unsigned int *) yyvsp[0]); ;
    break;}
case 44:
#line 111 "dbg.y"
{ yyval = yyvsp[-2] + yyvsp[0]; ;
    break;}
case 45:
#line 112 "dbg.y"
{ yyval = yyvsp[-2] - yyvsp[0]; ;
    break;}
case 46:
#line 113 "dbg.y"
{ yyval = yyvsp[-1]; ;
    break;}
case 47:
#line 114 "dbg.y"
{ yyval = *((unsigned int *) yyvsp[0]); ;
    break;}
case 48:
#line 116 "dbg.y"
{ info_reg(); ;
    break;}
case 49:
#line 117 "dbg.y"
{ info_stack(); ;
    break;}
case 50:
#line 118 "dbg.y"
{ info_break(); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 121 "dbg.y"


void 
issue_prompt(){
#ifndef USE_READLINE
	fprintf(stderr,"Wine-dbg>");
#endif
}

void mode_command(int newmode)
{
  if(newmode == 16){
    dbg_mask = 0xffff;
    dbg_mode = 16;
    return;
  }
  if(newmode == 32){ 
    dbg_mask = 0xffffffff;
    dbg_mode = 32;
    return;
  }
  fprintf(stderr,"Invalid mode (use 16 or 32)\n");
}

static int loaded_symbols = 0;

void
wine_debug(int signal, int * regs)
{
	int i;
#ifdef YYDEBUG
	yydebug = 0;
#endif
	static int dummy_regs[32];

	yyin = stdin;
	regval = regs ? regs : dummy_regs;

#ifdef linux        
	if((SC_CS & 7) != 7) {
		dbg_mask = 0xffffffff;
		dbg_mode = 32;
	} else {
		dbg_mask = 0xffff;
		dbg_mode = 16;
	}
#endif
#ifdef __NetBSD__
	if(SC_CS == 0x1f) {
		dbg_mask = 0xffffffff;
		dbg_mode = 32;
	} else {
		dbg_mask = 0xffff;
		dbg_mode = 16;
	}
#endif
	fprintf(stderr,"In %d bit mode.\n", dbg_mode);

	/* This is intended to read the entry points from the Windows image, and
	   insert them in the hash table.  It does not work yet, so it is commented out. */
#if 0
	if(!loaded_symbols){
		loaded_symbols++;
		load_entrypoints();
	}
#endif

	/* Remove the breakpoints from memory... */
	insert_break(0);

	/* If we stopped on a breakpoint, report this fact */
	if(signal == SIGTRAP)
	  {
	    unsigned int addr;
	    addr = SC_EIP(dbg_mask);
	    if((addr & 0xffff0000) == 0 && dbg_mode == 16)
	      addr |= SC_CS << 16;
	    fprintf(stderr,"Stopped on breakpoint %d\n", get_bpnum(addr));
	  }

	/* Show where we crashed */
	if(regs)
	  examine_memory(SC_EIP(dbg_mask), 1, 'i');

	issue_prompt();

	yyparse();
	flush_symbols();

	/* Re-insert the breakpoints from memory... */
	insert_break(1);

	fprintf(stderr,"Returning to Wine...\n");

}


yyerror(char * s){
	fprintf(stderr,"%s\n", s);
}


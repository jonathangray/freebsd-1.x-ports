/*
**  GRAMMAR.Y
**
**	This file contains a grammar for ingres parsing.  It is setup
**	for the (7-31-78) version of yacc.
**
**	Use:
**		to use for non-distributed ingres:
**			grep -v "DDD" > grammar.y
**
**	Trace Flags:
**		Grammar.y ~~ 38, 39
*/

%{
/* SCANNER/PARSER GLOBALS & TABLES */
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <pv.h>
#include "parser.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)grammar.y	8.6	5/30/88)

#ifdef xPTR1
#define	YYDEBUG	1
#endif

int				i;
att_ent_t			*aptr;
char				permbuf[3];
/* space for two names, their null bytes and the seperator */
char				modbuf[(2 * (MAX_NAME_SIZE + 1)) + 1];
static char			hqmbuf[2];

extern desc_t			Reldesc;
extern int			Opflag;
extern qtree_t			*Lastree;
extern qtree_t			*Tidnode;
extern int			Rsdmno;
extern int			Resrng;
extern int			Qrymod;
extern int			Permcomd;
extern char			*Trname;
extern int			Qlflag;
extern att_ent_t 		Faketid;
extern char			*Tuple;
extern tid_t			tid;
extern int                      Err_current;

#ifdef	DISTRIB
extern att_ent_t		Fakesid;
#endif

extern char			*Indexname;

extern PARRNG			Parrng[];
%}

/* NEW BEGIN-END THINGS */
%start		program

/* UNION YYSTYPE DEFINED */
%union
{
	int				type_type;	/* OPERATOR TYPES ETC. */
	qtree_t				*tree_type;
	int				rng_type;
	char				char_type;
	int				int_type;
	short				*I2_type;
	long				*I4_type;
	float				*F4_type;
	double				*F8_type;
	char				*string_type;
	STRKEEPER			*substr_type;
}

/* COMMANDS */
%token		APPEND	COPY	CREATE	DELETE	DESTROY	HELP	INDEX	MODIFY
%token		PRINT	RANGE	REPLACE	RETRIEVE	SAVE		
%token		DEFINE	PERMIT	VIEW	INTEGRITY
%token		DELIM	USE	UNUSE
/*DDD*/%token	DISTRIBUTE

/* 'NOISE' WORDS */
%token		ALL	BY	FROM	IN	INTO	UNIQUE	AT
%token		IS	OF	ON	ONTO	TO	UNTIL	WHERE
/*DDD*/%token	DISTRD

/* CONSTANTS */
%token		NAME	SCONST	I2CONST	I4CONST F4CONST	F8CONST

/* PUNCTUATION */
%token		COMMA	LPAREN	PERIOD	RPAREN	COLON	BGNCMNT	ENDCMNT
%token		LBRAC	RBRAC	DOLLAR	PCT

/* UNARY ARITHMETIC OPERATORS */
%token		UAOP

/* BINARY ARITHMETIC OPERATORS */
%token		BAOP	BAOPH

/* BOUNDS OPERATORS */
%token		BDOP

/* EQUALITY OPERATORS */
%token		EOP

/* LOGICAL OPERATORS */
%token		LBOP	LUOP

/* FUNCTIONAL OPERATORS */
%token		FOP	FBOP

/* AGGREGATE OPERATORS */
%token		AGOP

/* TYPES FOR INGRES TOKENS */
%type	<type_type>	IS
%type	<string_type>	NAME	SCONST
%type	<I2_type>	I2CONST
%type	<I4_type>	I4CONST
%type	<F4_type>	F4CONST
%type	<F8_type>	F8CONST
%type	<type_type>	UAOP
%type	<type_type>	BAOP	BAOPH
%type	<type_type>	BDOP
%type	<type_type>	EOP
%type	<type_type>	LBOP	LUOP
%type	<type_type>	FOP	FBOP
%type	<type_type>	AGOP
%type   <I4_type>	LPAREN	RPAREN	LBRAC	RBRAC  PCT

/* TYPES FOR INGRES NON-TERMINALS */
%type	<tree_type>	permtarg	permtlist	permtlelm
/*DDD*/%type	<tree_type>	distribute	distcrits	dcriterion
%type	<tree_type>	tlclause	tlist	tlelm
%type	<tree_type>	qualclause	qual	clause	afcn	aggrfcn
%type	<type_type>	relop
%type	<tree_type>	domseq	targdom	attrib
%type	<rng_type>	var
%type	<tree_type>	attribfcn
%type	<type_type>	uop
%type	<string_type>	alias
%type   <substr_type>   subelm	stringpart	grpelm	nameprt
%type	<I4_type>	leftclose	rightclose

/* DEFINE ASCENDING PRECEDENCE FOR OPERATORS */
%left		LBOP
%left		LUOP
%left		UAOP
%left		BAOP
%left		BAOPH
%nonassoc	unaryop

%%
program:	program stmnt =
		{
#ifdef	xPTR1
			tTfp(38, 0, "*** [program stmnt] parsed.\n");
#endif

			if (endquelst(Opflag) < 0)
				return (-1);
		}
	|	stmnt =
		{
#ifdef	xPTR1
			tTfp(38, 1, "*** [stmnt] parsed.\n");
#endif

			if (endquelst(Opflag) < 0)
				return (-1);
		}
	|
		{
#ifdef	xPTR1
			tTfp(38, 2, "*** [(NULL)] parsed.\n");
#endif
		}
;
stmnt:		append
	|	copy
	|	create
	|	delete 	
	|	destroy
/*DDD*/	|	distribute
	|	help
	|	index	
	|	integrity
	|	modify
	|	permit
	|	print
	|	range
	|	replace	
	|	retrieve 
	|	save
	|	view
	|	use
	|	unuse
	|	delim	
	|	error
		{
#ifdef	xPTR1
			tTfp(38, 0, "*** [error] parsed.\n");
#endif
		}

;
range:		rngstmnt OF NAME IS NAME =
		{
			if ((i = openr(&Reldesc, OR_RELTID, $5)) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, $5, 0, 0);
				YYERROR;
			}
			else
				rngent(R_EXTERNAL, $3, &Reldesc);
		}
;
rngstmnt:	RANGE =
		{
			Opflag = mdRANGE;
		}
;
append:		apstmnt apto relation tlclause qualclause =
		{
			/* make root node */
			Lastree = par_tree($4, $5, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
apstmnt:	APPEND =
		{
			Opflag = mdAPP;
		}
;
apto:		INTO
	|	ONTO
	|	TO
	|	ON
	|	;
;
delete:		delstmnt delwd relation qualclause =
		{
			/* make root node for delete, with a TIDNODE at leftmost */
			Lastree = par_tree(par_tree(NULL, Tidnode, RESDOM, sizeof(resdomnode_t), NULL, 0), $4, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
delstmnt:	DELETE =
		{
			Opflag = mdDEL;
		}
;
delwd:		IN
	|	ON
	|	FROM
	|	;
;
replace:	repstmnt repkwd relation tlclause qualclause =
		{
			/* make root node for replace */
			Lastree = par_tree($4, $5, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
repstmnt:	REPLACE =
		{
			Opflag = mdREPL;
		}
;
repkwd:		INTO
	|	IN
	|	ON
	|	;
;
retrieve:	retstmnt retclause tlclause qualclause =
		{
			/* make root node for retrieve */
			Lastree = par_tree($3, $4, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
retstmnt:	RETRIEVE =
		{
			Opflag = mdRETR;
		}
;
retclause:	retkwd relation =
		{
			/* set up pipe block and save relname for create */
#ifdef	xPTR2
			tTfp(38, 4, "retclause: Rsdmno %d", Rsdmno);
#endif
			Rsdmno = 0;
			setp(PV_STR, "0", 0);	/* r_status = nil */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
	|	=
		{
			/* no result relation, output to terminal */
			Rsdmno = 0;
			Resrng = -1;
		}
	|	UNIQUE =
		{
			Opflag = mdRET_UNI;
			Rsdmno = 0;
			Resrng = -1;
		}
;
retkwd:		INTO
	|	TO
	|	;
;

delim:		DEFINE DELIM NAME LPAREN NAME COMMA SCONST RPAREN = 
		{
			Opflag = mdSTOP;
			if ((i = openr(&Reldesc, OR_WRITE, "rdelim")) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, "rdelim", 0, 0);
				YYERROR;
			}
			else
			{
				if (( i = make_tuples(&Reldesc, $3, $5, $7)) < 0)
				{
					closer(&Reldesc);
					par_error(BADBNF, WARN, "rdelim", 0, 0);
				}
			}
			closer(&Reldesc);
		}
;
use:		USE NAME =
		{
			Opflag = mdSTOP;
			if ((i = openr(&Reldesc, OR_WRITE, "rdelim")) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, "rdelim", 0, 0);
				YYERROR;
			}
			else
			{
				if ((i = make_list(&Reldesc, $2)) < 0)
				{
					closer(&Reldesc);
					par_error(DELEXIST, WARN, 0, 0, 0);
				}
			}
			closer(&Reldesc);
		}
;
unuse:		UNUSE NAME =
		{
			Opflag = mdSTOP;
			if (( i = shrink_list($2)) < 0)
			{
				par_error(NOGRP,WARN, 0, 0, 0);
			}
		}
;

view:		viewclause tlclause qualclause =
		{
			Lastree = par_tree($2, $3, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
viewclause:	viewstmnt relation =
		{
			Rsdmno = 0;
			setp(PV_STR, "0040", 0);	/* r_status = S_VIEW */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
;
viewstmnt:	DEFINE VIEW =
		{
			Opflag = mdVIEW;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
;
permit:		permstmnt permlist permrel permtarg permwho permplace permtd qualclause =
		{
			Lastree = par_tree($4, $8, ROOT, sizeof(struct rootnode), 1, 0);
		}
;
permstmnt:	DEFINE PERMIT =
		{
			Opflag = mdPROT;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
;
permlist:	permxlist
	|	permlist COMMA permxlist
;
permxlist:	ALL =
		{
			permcom(-1);	/* means 'all' commands */
		}
	|	RETRIEVE =
		{
			permcom(mdRETR);
		}
	|	DELETE =
		{
			permcom(mdDEL);
		}
	|	APPEND =
		{
			permcom(mdAPP);
		}
	|	REPLACE =
		{
			permcom(mdREPL);
		}
;
permrel:	permword relation =
		{
			/* put command vector into list now since this always happens */
			setp(PV_INT, &Permcomd, sizeof(Permcomd));
			Permcomd = 0;		/* reset command map */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
			bmove(Parrng[Resrng].vardesc.d_r.r_owner, permbuf, sizeof(Parrng[Resrng].vardesc.d_r.r_owner));
			permbuf[2] = 0;
			setp(PV_STR, permbuf, 0);
		}
;
permword:	ON
	|	OF
	|	TO
;
permtarg:	LPAREN permtlist RPAREN =
		{
			$$ = $2;
		}
	|	=
		{
			$$ = NULL;
		}
;
permtlist:	permtlelm
	|	permtlist COMMA permtlelm =
		{
			/*
			** attach bulk of permit tl to leftmost node of new elem
			*/
			if (!Err_current)
				$$ = tlprepend($1, $3);
		}
;
permtlelm:	NAME =
		{
			/* Resrng is set by the "relation" production */
			if (!Err_current)
			{
				Trname = $1;
				aptr = attlookup(Resrng, Trname);
				$$ = par_tree(NULL, NULL, VAR, sizeof(varnode_t), Resrng, aptr);
				$$ = addresdom(NULL, $$);
			}
		}
;
permwho:	TO NAME =
		{
			setp(PV_STR, $2, 0);
		}
	|	TO ALL =
		{
			setp(PV_STR, "all", 0);
		}
;
permplace:	AT NAME =
		{
			setp(PV_STR, $2, 0);
		}
	|	AT ALL =
		{
			setp(PV_STR, "all", 0);
		}
	|	=
		{
			setp(PV_STR, "all", 0);		/* default is all */
		}
;
permtd:		permtime permday
	|	permdeftime permday
	|	permtime permdefday
	|	permdeftime permdefday
;
permdeftime:	=
		{
			int i;

			i = 0;
			setp(PV_INT, &i, sizeof(i));
			i = 1440;
			setp(PV_INT, &i, sizeof(i));
		}
;
permdefday:	=
		{
			setp(PV_STR, "sun", 0);
			setp(PV_STR, "sat", 0);
		}
;
permtime:	FROM I2CONST COLON I2CONST TO I2CONST COLON I2CONST =
		{
			int	i;

			i = timeofday($2, $4);
			setp(PV_INT, &i, sizeof(i));
			i = timeofday($6, $8);
			setp(PV_INT, &i, sizeof(i));
		}
;
permday:	ON NAME TO NAME =
		{
			setp(PV_STR, $2, 0);
			setp(PV_STR, $4, 0);
		}
;
integrity:	integstmnt integnoise relation integis qual =
		{
			Lastree = par_tree(NULL, norml($5), ROOT, sizeof(struct rootnode), 1, 0);
			Qlflag--;	/* turn off here */
		}
;
integstmnt:	DEFINE INTEGRITY =
		{
			Opflag = mdINTEG;
			Qlflag++;	/* OK to turn on here because integrity doesn't have a targ list */
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
;
integnoise:	ON
	|	ONTO
	|	IN
	|	OF
	|	/* null */
;
integis:	IS
	|	/* null*/
;
/*DDD*/distribute:	diststmnt relation AT distcrits =
/*DDD*/		{
/*DDD*/			if (!Err_current)
/*DDD*/			{
/*DDD*/				$$ = par_tree(NULL, NULL, QLEND, 0, 0, 0);
/*DDD*/				Lastree = par_tree($4, $$, ROOT, sizeof(struct rootnode), 1, 0);
/*DDD*/			}
/*DDD*/		}
/*DDD*/;
/*DDD*/diststmnt:	DISTRIBUTE =
/*DDD*/				Opflag = mdDISTRIB;
/*DDD*/;
/*DDD*/distcrits:	dcriterion =
/*DDD*/		{
/*DDD*/			$$ = $1;
/*DDD*/		}
/*DDD*/	|	distcrits dcriterion =
/*DDD*/		{
/*DDD*/			$$ = tlprepend($1, $2);
/*DDD*/		}
/*DDD*/;
/*DDD*/dcriterion:	NAME where qual =
/*DDD*/		{
/*DDD*/			Qlflag--;
/*DDD*/			syserr("Warning this node may be the wrong size\n");
/*DDD*/			if (!Err_current)
/*DDD*/				$$ = par_tree(NULL, norml($3), SITE, 2, $1, 0);
/*DDD*/		}
/*DDD*/;
relation:	NAME =
		{
#ifdef	xPTR2
			tTfp(38, 3, "res rel name/var: '%s'\n", $1);
#endif
			switch (Opflag)
			{
			  case mdRETR:
			  case mdVIEW:
				/* result better not be a rel name */
				if ((i = openr(&Reldesc, OR_RELTID, $1)) < 0)
					syserr("relation: err openr '%d'", i);
				if (i == 0)
				{
					/* reln exists */
					if (bequal(Reldesc.d_r.r_owner, Usercode, USERCODE_SIZE))
					{
						/* same owner, can't duplicate name */
						par_error(RESEXIST, WARN, $1, 0, 0);
						YYERROR;
					}
					else if (!Err_current)
					{
						/* owned by dba -- purge range table */
						rngdel($1);
					}
				}
				if (!Err_current)
				{
					bmove(Usercode, Reldesc.d_r.r_owner, USERCODE_SIZE);
					pmove($1, Reldesc.d_r.r_id, MAX_NAME_SIZE, ' ');
					Resrng = rngent(R_INTERNAL, "", &Reldesc);
				}
				break;

			  case mdAPP:
				/* result is a rel name */
				if (!Err_current)
				{
					Resrng = rnglook($1, LOOKREL);
					if (Resrng < 0)
					{
						if ((i = openr(&Reldesc, OR_RELTID, $1)) < 0)
							syserr("relation: err openr '%d'", i);
						if (i)
						{
							/* invalid relation name */
							par_error(RESAPPEX, WARN, $1, 0, 0);
							YYERROR;
						}
						Resrng = rngent(R_INTERNAL, "", &Reldesc);
					}
					else
						ctlmod_decl(Resrng);
					checkupd(Resrng);
				}
				break;

			  case mdPROT:
			  case mdINTEG:
#ifdef	DISTRIB
			  case mdDISTRIB:
#endif
				/* the result is a tuple variable */
				Resrng = rnglook($1, LOOKVAR);
				if (Resrng < 0)
				{
					/* variable not declared */
					par_error(NOVBLE, WARN, $1, 0, 0);
					YYERROR;
				}
				else
					ctlmod_decl(Resrng);
				break;

			  case mdREPL:
			  case mdDEL:
				/* the result is a tuple variable */
				Resrng = rnglook($1, LOOKVAR);
				if (Resrng < 0)
					/* variable not declared */
				{
					par_error(NOVBLE, WARN, $1, 0, 0);
					YYERROR;
				}
				else
					ctlmod_decl(Resrng);

				checkupd(Resrng);
				Tidnode = par_tree(NULL, NULL, VAR, sizeof(varnode_t), Resrng, &Faketid);
				break;
			  default:
			    ;
			}
		}
;
tlclause:	LPAREN tlist RPAREN =
		{
			$$ = $2;

			/*
			** replace must have tid node as left branch
			**	(so does delete but it doesn't have a targ list)
			*/
			if (Opflag == mdREPL && !Err_current)
			{
				$$ = tlprepend(par_tree(NULL, Tidnode, RESDOM, sizeof(resdomnode_t), 0, 0), $$);
			}
		}
;
tlist:		tlelm
	|	tlist COMMA tlelm =
		{
			/*
			** attach bulk of targ list to leftmost node
			** of new element
			*/
			if (!Err_current)
				$$ = tlprepend($1, $3);
		}
;
tlelm:		NAME is afcn =
		{
			Trname = $1;
			/* make a new resdom entry for targ list */
			if (!Err_current)
				$$ = addresdom(NULL, $3);
		}
	|	attrib =
		{
		/* makes a new resdom entry for targ list */
			if (!Err_current)
				$$ = addresdom(NULL, $1);
		}
	|	var PERIOD ALL =
		{
			if (Opflag == mdREPL)
			{
				/* ALL not defined for REPLACE */
				par_error(REPALL, WARN,
				    trim_relname(Qt.qt_rangev[$1].rngvdesc->d_rangevar), 0, 0);
				YYERROR;
			}
			/* makes set of new resdom entries for targ list */
			else if (!Err_current)
				$$ = xdot($1);
		}
;
is:		IS
	|	BY
;
qualclause:	where qual =
		{
			$$ = norml($2);
			Qlflag--;
			if (Opflag == mdREPL)
			    qualindex();
		}
	|	=
		{
			/* null qualification */
			if (Opflag == mdREPL)
			    qualindex();
			$$ = norml(NULL);
		}
;
where:		WHERE =
		{
			Qlflag++;
		}
;
qual:		LPAREN qual RPAREN =
		{
			$$ = $2;
		}
	|	LUOP qual =
		{
			$$ = par_tree(NULL, $2, UOP, 2, $1, 0);
		}
	|	qual LBOP qual =
		{
			$$ = par_tree($1, $3, $2, sizeof (struct rootnode) -2, 0, 0);
		}
	|	clause
;
clause:		afcn relop afcn =
		{
			$$ = par_tree($1, $3, BOP, 2, $2, 0);
		}
;
relop:		EOP
	|	IS
	|	BDOP
;
afcn:		aggrfcn
	|	attribfcn
	|	afcn BAOPH afcn =
		{
			$$ = par_tree($1, $3, BOP, 2, $2, 0);
		}
	|	afcn BAOP afcn =
		{
			$$ = par_tree($1, $3, BOP, 2, $2, 0);
		}
	|	afcn UAOP afcn =
		{
			$$ = par_tree($1, $3, BOP, 2, $2, 0);
		}
	|	LPAREN afcn RPAREN =
		{
			$$ = $2;
		}
	|	uop afcn	%prec unaryop	=
		{
			$$ = par_tree(NULL, $2, UOP, 2, $1, 0);
		}
	|	FOP LPAREN afcn RPAREN =
		{
			$$ = par_tree($3, NULL, UOP, 2, $1, 0);
		}
	|	FBOP LPAREN afcn COMMA afcn RPAREN =
		{
			$$ = par_tree($3, $5, BOP, 2, $1, 0);
		}
;
aggrfcn:	AGOP LPAREN afcn BY domseq qualclause RPAREN =
		{
#ifdef	xPTR2
			tTfp(39, 0, "agg func\n");
#endif
			windup($5);
			$$ = par_tree(par_tree($5, par_tree(NULL, $3, AOP, 6, $1, 0), BYHEAD, sizeof(resdomnode_t), 0, 0), $6, AGHEAD, sizeof(struct rootnode), 0, 0);
			tlprepend(par_tree(NULL, NULL, TREE, 0, 0, 0), $$);
		}
	|	AGOP LPAREN afcn qualclause RPAREN =
		{
			$$ = par_tree(par_tree(NULL, $3, AOP, 6, $1, 0), $4,  AGHEAD, sizeof(struct rootnode), 0, 0);
		}
;
domseq:		targdom
	|	domseq COMMA targdom =
		{
			$$ = tlprepend($1, $3);
		}
;
targdom:	afcn =
		{
			$$ = par_tree(NULL, $1, RESDOM, sizeof(resdomnode_t), Rsdmno, 0);
		}
;
nameprt:	NAME =
		{
			$$ = substring($1,1);
		}
	|	SCONST =
		{
			$$ = substring($1,0);
		}
;
subelm:		DOLLAR =
		{ 
			$$ = substring(NULL,0);
		}
	|	nameprt DOLLAR =
		{
			$1->flag[0] |= 2;
			$$ = $1;
		}
	|	nameprt =
		{
			$$ = $1;
		}
	|	I2CONST subelm =
		{
			setnumber($2,$1);
			$$ = $2;
		}
;
grpelm:		subelm COMMA subelm =
		{
			groupstrings($1,$3);
			$$ = $1;
		}	
;
leftclose:	PCT =
		{
			$$ = $1;
		}
	|	LPAREN =
		{
			$$ = $1;
		}
;
rightclose:	PCT =
		{
			$$ = $1;
		}
	|	RPAREN =
		{
			$$ = $1;
		}
;
stringpart:	leftclose subelm rightclose =
		{
			$$ = endvals($2,$1,$3);
		}
	|	leftclose grpelm rightclose =
		{
			$$ = endvals($2,$1,$3);
		}
;
attrib:		var PERIOD NAME =
		{
#ifdef	xPTR2
			tTfp(39, 1, "attrib %12s.%12s found\n",
			Qt.qt_rangev[$1].rngvdesc->d_rangevar, $3);
#endif

			/* remember attribute name */
			Trname = $3;

			/* look up attribute */
			aptr = attlookup($1, Trname);
			$$ = par_tree(NULL, NULL, VAR, sizeof(varnode_t), $1, aptr);
		}
	|   	attrib stringpart =
		{
			$1->sym.value.sym_var.varstr = $2;
			$$ = $1;
		}
;
var:		NAME =
		{
			$$ = rnglook($1, LOOKVAR);
			if ($$ < 0)
			{
				/* variable not declared */
				par_error(NOVBLE, WARN, $1, 0, 0);
				YYERROR;
			}
			else
				ctlmod_decl($$);
		}
;
attribfcn:	I2CONST =
		{
			$$ = par_tree(NULL, NULL, INT_CONST, 2, $1, 0);
		}
	|	I4CONST =
		{
			$$ = par_tree(NULL, NULL, INT_CONST, 4, $1, 0);
		}
	|	F4CONST =
		{
			$$ = par_tree(NULL, NULL, FLOAT_CONST, 4, $1, 0);
		}
	|	F8CONST =
		{
			$$ = par_tree(NULL, NULL, FLOAT_CONST, 8, $1, 0);
		}
	|	SCONST =
		{
			patmat($1);
			$$ = par_tree(NULL, NULL, CHAR_CONST, strlen($1), $1, 0);
		}
	|	NAME =
		{
			$$ = par_tree(NULL, NULL, COP, 2, $1, 0);
		}
	|	attrib
;
uop:		UAOP	%prec unaryop	=
		{
			if ($1 == opADD)
				$$ = opPLUS;
			else
				if ($1 == opSUB)
					$$ = opMINUS;
		}
;
copy:		copstmnt alias LPAREN coparam RPAREN keywd SCONST =
		{
#ifdef	xPTR2
			tTfp(39, 3, "copy %12s,%12s\n", $2, $7);
#endif

			setp(PV_STR, $7, 0);
		}
;
copstmnt:	COPY =
		{
			Opflag = mdCOPY;
		}
;
coparam:	cospecs
	|	;
;
cospecs:	alias is coent
	|	cospecs COMMA alias is coent
;
coent:		alias
	|	SCONST =
		{
			setp(PV_STR, $1, 0);
		}
;
alias:		NAME =
		{
			if (!Err_current)
			{
				setp(PV_STR, $1, 0);
				if (Opflag == mdDESTROY || Opflag == mdCREATE
#ifdef	DISTRIB
					|| Opflag == mdDCREATE
#endif
								)
					rngdel($1);
			}
		}
;
specs:		alias is alias
	|	specs COMMA alias is alias
;
keywd:		INTO =
		{
			setp(PV_STR, "\0", 0);
			setp(PV_STR, "i", 0);
		}
	|	FROM =
		{
			setp(PV_STR, "\0", 0);
			setp(PV_STR, "f", 0);
		}
;
create:		crestmnt alias LPAREN specs RPAREN
;
crestmnt:	CREATE =
		{
			Opflag = mdCREATE;

			/* set up parameters for regular create */
			setp(PV_STR, "0", 0);		/* r_status = nil */
		}
/*DDD*/	|	CREATE DISTRD =
/*DDD*/		{
/*DDD*/			Opflag = mdDCREATE;
/*DDD*/
/*DDD*/			/* setup parameters for distributed create */
/*DDD*/			setp(PV_STR, "U", 0);
/*DDD*/			setp(PV_STR, "", 0);
/*DDD*/			setp(PV_STR, "01000", 0);	/* r_status = S_DISTRIBUTED */
/*DDD*/		}
;
destroy:	destmnt keys
	|	destqm destlist
	|	destmnt DELIM NAME =
		{
			Opflag = mdSTOP;
			if ((i = openr(&Reldesc, OR_WRITE, "rdelim")) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, "rdelim", 0, 0);
				YYERROR;
			}
			else
			{
				if (( i = destroy_delim(&Reldesc, $3) < 0) != 0)
				{
					closer(&Reldesc);
					par_error(DELEXIST, WARN, "rdelim",0, 0);
				}
			}
			closer(&Reldesc);
		}
;
destmnt:	DESTROY =
		{
			Opflag = mdDESTROY;
		}
;
destqm:		destmnt INTEGRITY NAME =
		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			setp(PV_STR, "6", 0);
			setp(PV_STR, $3, 0);
		}
	|	destmnt PERMIT NAME =
		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			setp(PV_STR, "5", 0);
			setp(PV_STR, $3, 0);
		}
;
destlist:	I2CONST =
		{
			setp(PV_STR, iocv(*($1)), 0);
		}
	|	destlist COMMA I2CONST =
		{
			setp(PV_STR, iocv(*($3)), 0);
		}
	|	ALL
;
help:		helstmnt hlist
	|	helstmnt =
		{
			int	i;

			i = HELP_RELLIST;
			setp(PV_INT, &i, sizeof(i));	/* all relns */
		}
	|	helqmstmnt hqmlist
	|	heldelstmnt =
		{
			int	i;

			i = HELP_ALLDELLIST;	/* all delims */
			setp(PV_INT, &i, sizeof(i));	/* all delims */
		}
	|	heldelstmnt dlist
;
helstmnt:	HELP =
		{
			Opflag = mdHELP;
		}
;
heldelstmnt:	HELP DELIM =
		{
			Opflag = mdHELP;
		}
;
helqmstmnt:	HELP VIEW =
		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("4", hqmbuf);
		}
	|	HELP PERMIT =
		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("5", hqmbuf);
		}
	|	HELP INTEGRITY =
		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("6", hqmbuf);
		}

;
hlist:		hparam
	|	hlist COMMA hparam
	|	ALL =
		{
			int	i;

			i = HELP_ALLRELINFO;
			setp(PV_INT, &i, sizeof(i));
		}
;
dlist:		dparam
	|	dlist COMMA dparam
;
dparam:	NAME =
		{
			/* relation */
			int	i;

			i = HELP_DELLIST;
			setp(PV_INT, &i, sizeof(i));
			i = $1;
			setp(PV_INT, &i, sizeof(i));
		}
;
hparam:		NAME =
		{
			int	i;

			/* relation */
			i = HELP_RELINFO;
			setp(PV_INT, &i, sizeof(i));
			setp(PV_STR, $1, 0);
		}
	|	SCONST =
		{
			int	i;

			/* manual page */
			i = HELP_MANSEC;
			setp(PV_INT, &i, sizeof(i));
			setp(PV_STR, $1, 0);
		}
;
hqmlist:	NAME =
		{
			setp(PV_STR, hqmbuf, 0);
			setp(PV_STR, $1, 0);
		}
	|	hqmlist COMMA NAME =
		{
			setp(PV_STR, hqmbuf, 0);
			setp(PV_STR, $3, 0);
		}
;
index:		instmnt LPAREN keys RPAREN =
		{
			if (Rsdmno > MAX_2ND_KEYS)
				/* too many attributes in key */
				par_error(INDEXTRA, WARN, 0, 0, 0);
		}
;
instmnt:	indexq ON NAME IS NAME =
		{
			/* init INDEX command */
			Rsdmno = 0;
			setp(PV_STR, $3, 0);
			setp(PV_STR, $5, 0);
			Indexname = $5;
		}
;
indexq:		INDEX =
		{
			Opflag = mdINDEX;
		}
;
modify:		modstmnt alias TO modstorage modkeys modqual
;
modstmnt:	MODIFY =
		{
			Opflag = mdMODIFY;
			Rsdmno = 0;
		}
;
modstorage:	NAME =
		{
			setp(PV_STR, $1, 0);
		}
modkeys:	modstkey modrptkey
	|	;
;
modstkey:	ON =
		{
			setp(PV_STR, "name", 0);
		}
;
modrptkey:	modbasekey
	|	modrptkey COMMA modbasekey
;
modbasekey:	NAME =
		{
			setp(PV_STR, $1, 0);
		}
	|	NAME COLON NAME =
		{
			concat($1, ztack(":", $3), modbuf);
			setp(PV_STR, modbuf, 0);
		}
;
modqual:	modcond modfill
	|	;
;
modcond:	WHERE =
		{
			setp(PV_STR, "\0", 0);
		}
;
modfill:	modfillnum
	|	modfill COMMA modfillnum
;
modfillnum:	NAME IS I2CONST =
		{
			setp(PV_STR, $1, 0);
			setp(PV_STR, iocv(*($3)), 0);
		}
	|	NAME IS NAME =
		{
			setp(PV_STR, $1, 0);
			setp(PV_STR, $3, 0);
		}
;
keys:		alias =
		{
			Rsdmno++;
		}
	|	keys COMMA alias =
		{
			Rsdmno++;
		}
;
print:		prinstmnt keys
;
prinstmnt:	PRINT =
		{
			Opflag = mdPRINT;
		}
;
save:		savstmnt alias UNTIL date
	|	savstmnt alias
;
savstmnt:	SAVE =
		{
			Opflag = mdSAVE;
		}
;
date:		month day_year day_year
;
month:		alias
	|	day_year
;
day_year:	I2CONST =
		{
			i = (int) iocv(*($1));

#ifdef	xPTR3
			tTfp(39, 4, "day_year: %s\n", i);
#endif

			setp(PV_STR, iocv(*($1)), 0);
		}
;
%%
#include	"scanner.h"
#include	"tables.y"
#include	"yyerror.y"

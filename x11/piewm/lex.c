# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/

/***********************************************************************
 *
 * $XConsortium: lex.l,v 1.62 89/12/10 17:46:33 jim Exp $
 *
 * .twmrc lex file
 *
 * 12-Nov-87 Thomas E. LaStrange		File created
 *
 ***********************************************************************/

/* #include <stdio.h> */		/* lex already includes stdio.h */
#include "gram.h"
#include "parse.h"
extern char *ProgramName;

extern int ParseError;

# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
			{ return (LB); }
break;
case 2:
			{ return (RB); }
break;
case 3:
			{ return (LP); }
break;
case 4:
			{ return (RP); }
break;
case 5:
			{ return (EQUALS); }
break;
case 6:
			{ return (COLON); }
break;
case 7:
			{ return PLUS; }
break;
case 8:
			{ return MINUS; }
break;
case 9:
			{ return OR; }
break;
case 10:
			{ return (AT); }
break;
case 11:
		{ int token = parse_keyword (yytext, 
							     &yylval.num);
				  if (token == ERRORTOKEN) {
				      twmrc_error_prefix();
				      fprintf (stderr,
				       "ignoring unknown keyword:  %s\n", 
					       yytext);
				      ParseError = 1;
				  } else 
				    return token;
				}
break;
case 12:
			{ yylval.num = F_EXEC; return FSKEYWORD; }
break;
case 13:
			{ yylval.num = F_CUT; return FSKEYWORD; }
break;
case 14:
		{ yylval.ptr = (char *)yytext; return STRING; }
break;
case 15:
		{ (void)sscanf(yytext, "%d", &yylval.num);
				  return (NUMBER);
				}
break;
case 16:
		{;}
break;
case 17:
			{;}
break;
case 18:
			{
				  twmrc_error_prefix();
				  fprintf (stderr, 
					   "ignoring character \"%s\"\n",
					   yytext);
				  ParseError = 1;
				}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
yywrap() { return(1);}

#undef unput
#undef input
#undef output
#undef feof
#define unput(c)	twmUnput(c)
#define input()		(*twmInputFunc)()
#define output(c)	TwmOutput(c)
#define feof()		(1)
int yyvstop[] = {
0,

18,
0,

17,
18,
0,

17,
0,

12,
18,
0,

18,
0,

18,
0,

3,
18,
0,

4,
18,
0,

7,
18,
0,

8,
18,
0,

11,
18,
0,

15,
18,
0,

6,
18,
0,

5,
18,
0,

10,
18,
0,

13,
18,
0,

1,
18,
0,

9,
18,
0,

2,
18,
0,

14,
0,

16,
0,

11,
0,

15,
0,

14,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	7,22,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	7,22,	
7,22,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	8,25,	0,0,	
0,0,	0,0,	1,6,	1,7,	
1,8,	0,0,	8,25,	8,26,	
7,23,	1,9,	1,10,	2,6,	
1,11,	2,8,	1,12,	1,13,	
24,29,	1,14,	2,9,	2,10,	
7,22,	2,11,	7,22,	2,12,	
0,0,	0,0,	0,0,	1,15,	
0,0,	0,0,	1,16,	8,25,	
0,0,	1,17,	0,0,	0,0,	
2,15,	0,0,	0,0,	2,16,	
0,0,	0,0,	2,17,	8,25,	
0,0,	8,25,	14,28,	14,28,	
14,28,	14,28,	14,28,	14,28,	
14,28,	14,28,	14,28,	14,28,	
0,0,	0,0,	0,0,	0,0,	
0,0,	22,24,	29,24,	1,18,	
0,0,	0,0,	7,24,	0,0,	
0,0,	0,0,	0,0,	0,0,	
2,18,	0,0,	24,24,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	13,27,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
1,19,	1,20,	1,21,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	2,19,	2,20,	2,21,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	13,27,	13,27,	
13,27,	13,27,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-10,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+0,	0,		yyvstop+8,
yycrank+-6,	0,		yyvstop+11,
yycrank+-29,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+24,
yycrank+71,	0,		yyvstop+27,
yycrank+30,	0,		yyvstop+30,
yycrank+0,	0,		yyvstop+33,
yycrank+0,	0,		yyvstop+36,
yycrank+0,	0,		yyvstop+39,
yycrank+0,	0,		yyvstop+42,
yycrank+0,	0,		yyvstop+45,
yycrank+0,	0,		yyvstop+48,
yycrank+0,	0,		yyvstop+51,
yycrank+-1,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+54,
yycrank+-14,	yysvec+7,	0,	
yycrank+0,	yysvec+8,	0,	
yycrank+0,	0,		yyvstop+56,
yycrank+0,	yysvec+13,	yyvstop+58,
yycrank+0,	yysvec+14,	yyvstop+60,
yycrank+-2,	yysvec+7,	yyvstop+62,
0,	0,	0};
struct yywork *yytop = yycrank+193;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,'.' ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,01  ,01  ,01  ,01  ,01  ,
01  ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,'.' ,
'.' ,'.' ,'.' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}

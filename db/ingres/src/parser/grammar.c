#ifndef lint
static char yysccsid[] = "@(#)yaccpar	1.8 (Berkeley) 01/20/91";
#endif
#define YYBYACC 1
#line 16 "grammar.y"
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
#line 78 "grammar.y"
typedef union
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
} YYSTYPE;
#line 78 "y.tab.c"
#define APPEND 257
#define COPY 258
#define CREATE 259
#define DELETE 260
#define DESTROY 261
#define HELP 262
#define INDEX 263
#define MODIFY 264
#define PRINT 265
#define RANGE 266
#define REPLACE 267
#define RETRIEVE 268
#define SAVE 269
#define DEFINE 270
#define PERMIT 271
#define VIEW 272
#define INTEGRITY 273
#define DELIM 274
#define USE 275
#define UNUSE 276
#define DISTRIBUTE 277
#define ALL 278
#define BY 279
#define FROM 280
#define IN 281
#define INTO 282
#define UNIQUE 283
#define AT 284
#define IS 285
#define OF 286
#define ON 287
#define ONTO 288
#define TO 289
#define UNTIL 290
#define WHERE 291
#define DISTRD 292
#define NAME 293
#define SCONST 294
#define I2CONST 295
#define I4CONST 296
#define F4CONST 297
#define F8CONST 298
#define COMMA 299
#define LPAREN 300
#define PERIOD 301
#define RPAREN 302
#define COLON 303
#define BGNCMNT 304
#define ENDCMNT 305
#define LBRAC 306
#define RBRAC 307
#define DOLLAR 308
#define PCT 309
#define UAOP 310
#define BAOP 311
#define BAOPH 312
#define BDOP 313
#define EOP 314
#define LBOP 315
#define LUOP 316
#define FOP 317
#define FBOP 318
#define AGOP 319
#define unaryop 320
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,   29,   29,   29,   29,   29,   29,   29,
   29,   29,   29,   29,   29,   29,   29,   29,   29,   29,
   29,   29,   29,   29,   41,   49,   30,   50,   51,   51,
   51,   51,   51,   33,   53,   54,   54,   54,   54,   42,
   55,   56,   56,   56,   56,   43,   57,   58,   58,   58,
   59,   59,   59,   48,   46,   47,   45,   60,   61,   39,
   62,   63,   63,   68,   68,   68,   68,   68,   64,   69,
   69,   69,    1,    1,    2,    2,    3,   65,   65,   66,
   66,   66,   67,   67,   67,   67,   72,   73,   70,   71,
   37,   74,   75,   75,   75,   75,   75,   76,   76,    4,
   77,    5,    5,    6,   52,    7,    8,    8,    9,    9,
    9,   79,   79,   10,   10,   78,   11,   11,   11,   11,
   12,   15,   15,   15,   13,   13,   13,   13,   13,   13,
   13,   13,   13,   14,   14,   16,   16,   17,   26,   26,
   23,   23,   23,   23,   25,   27,   27,   28,   28,   24,
   24,   18,   18,   19,   20,   20,   20,   20,   20,   20,
   20,   21,   31,   80,   81,   81,   83,   83,   84,   84,
   22,   85,   85,   82,   82,   32,   86,   86,   34,   34,
   34,   87,   89,   89,   90,   90,   90,   35,   35,   35,
   35,   35,   91,   95,   93,   93,   93,   92,   92,   92,
   96,   96,   98,   97,   97,   94,   94,   36,   99,  100,
   38,  101,  102,  103,  103,  105,  106,  106,  107,  107,
  104,  104,  108,  109,  109,  110,  110,   88,   88,   40,
  111,   44,   44,  112,  113,  114,  114,  115,
};
short yylen[] = {                                         2,
    2,    1,    0,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    5,    1,    5,    1,    1,    1,
    1,    1,    0,    4,    1,    1,    1,    1,    0,    5,
    1,    1,    1,    1,    0,    4,    1,    2,    0,    1,
    1,    1,    0,    8,    2,    2,    3,    2,    2,    8,
    2,    1,    3,    1,    1,    1,    1,    1,    2,    1,
    1,    1,    3,    0,    1,    3,    1,    2,    2,    2,
    2,    0,    2,    2,    2,    2,    0,    0,    8,    4,
    5,    2,    1,    1,    1,    1,    0,    1,    0,    4,
    1,    1,    2,    3,    1,    3,    1,    3,    3,    1,
    3,    1,    1,    2,    0,    1,    3,    2,    3,    1,
    3,    1,    1,    1,    1,    1,    3,    3,    3,    3,
    2,    4,    6,    7,    5,    1,    3,    1,    1,    1,
    1,    2,    1,    2,    3,    1,    1,    1,    1,    3,
    3,    3,    2,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    7,    1,    1,    0,    3,    5,    1,    1,
    1,    3,    5,    1,    1,    5,    1,    2,    2,    2,
    3,    1,    3,    3,    1,    3,    1,    2,    1,    2,
    1,    2,    1,    2,    2,    2,    2,    1,    3,    1,
    1,    3,    1,    1,    1,    1,    3,    4,    5,    1,
    6,    1,    1,    2,    0,    1,    1,    3,    1,    3,
    2,    0,    1,    1,    3,    3,    3,    1,    3,    2,
    1,    4,    2,    1,    3,    1,    1,    1,
};
short yydefred[] = {                                      0,
   24,   28,  164,    0,   35,  182,    0,  210,  212,  231,
   26,   41,   47,  234,    0,    0,    0,  101,    0,    9,
    2,    4,    5,    6,    7,    8,   10,   11,   12,   13,
   14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  178,  196,  195,  197,  194,   61,   59,   92,
    0,   55,   56,    1,    0,   29,   32,   30,   31,    0,
   38,   36,   37,    0,   43,   42,   44,    0,   51,   50,
   52,    0,    0,    0,    0,  105,   58,   67,   66,   68,
   65,   64,    0,   62,   95,   96,   93,   94,    0,    0,
  171,    0,    0,    0,    0,    0,  228,    0,  187,  185,
    0,  200,  204,  205,    0,  198,  206,    0,  203,    0,
  201,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   48,    0,    0,  107,    0,    0,  116,   57,
    0,   71,   70,   72,    0,    0,    0,    0,    0,    0,
    0,  184,  183,  181,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   34,    0,   46,  113,
  112,    0,    0,  106,  147,  146,  153,    0,    0,    0,
  159,  155,  156,  157,  158,    0,  162,    0,    0,    0,
    0,    0,  120,    0,  125,    0,    0,  126,    0,   63,
    0,    0,   69,   98,    0,    0,    0,  102,    0,    0,
    0,    0,    0,  229,  186,  199,  207,  202,  208,    0,
  213,    0,  238,  236,  232,    0,  237,    0,   25,   27,
   40,    0,    0,  108,  139,  140,    0,  141,    0,    0,
    0,  111,  152,    0,    0,  118,    0,    0,    0,    0,
  123,    0,    0,    0,  124,  122,    0,    0,  131,   77,
    0,   75,    0,    0,    0,    0,  103,    0,    0,    0,
    0,    0,  176,  209,  216,    0,    0,    0,    0,    0,
  144,    0,  149,  148,  150,  151,  142,  117,  130,    0,
    0,    0,  119,    0,    0,  127,    0,    0,   73,   79,
   78,    0,    0,    0,  170,  169,  167,  175,  174,    0,
    0,  172,    0,  223,  211,    0,    0,    0,  217,  235,
    0,  145,  132,    0,    0,    0,   76,   81,   80,    0,
    0,    0,    0,  163,    0,    0,    0,    0,  224,    0,
    0,   54,    0,    0,    0,  136,  135,    0,   60,    0,
   83,   85,   84,   86,  168,  173,    0,    0,  220,  218,
  133,    0,    0,    0,    0,  227,  226,  225,  137,  134,
    0,    0,    0,   90,    0,    0,   89,
};
short yydgoto[] = {                                      19,
  212,  271,  272,   20,  217,  218,   95,  145,  146,  150,
  202,  203,  204,  205,  267,  355,  356,  206,  207,  208,
  209,  117,  249,  187,  250,  251,  188,  295,   21,   22,
   23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
   33,   34,   35,   36,   37,   38,   39,   40,   41,   42,
   80,   97,   43,   84,   44,   88,   45,   92,   93,   46,
   47,   48,  103,  156,  274,  313,  341,  104,  157,  342,
  361,  343,  362,   49,  109,  215,   50,  151,  182,   51,
  220,  320,  221,  317,  223,   52,   53,  118,   54,  121,
   55,  125,   56,  128,   57,  130,  126,  131,   58,   59,
   60,  232,  286,  325,  287,  328,  329,  326,  348,  349,
   61,   62,  235,  236,  237,
};
short yysindex[] = {                                   1020,
    0,    0,    0, -262,    0,    0,  -48,    0,    0,    0,
    0,    0,    0,    0,  -37, -258, -207,    0, 1020,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
 -190,  -94,  -78,  -66,  -72, -228, -189, -229,  -90, -189,
 -159, -159, -200, -252, -210, -155, -146, -178, -124, -159,
 -159, -159,    0,    0,    0,    0,    0,    0,    0,    0,
 -118,    0,    0,    0, -101,    0,    0,    0,    0, -189,
    0,    0,    0, -189,    0,    0,    0, -189,    0,    0,
    0, -228, -189,  -93,  -73,    0,    0,    0,    0,    0,
    0,    0, -186,    0,    0,    0,    0,    0, -189,  -62,
    0,  -56,  -50,  -55,  -21,  -18,    0,  -51,    0,    0,
  -20,    0,    0,    0,  -17,    0,    0,  -16,    0,  -15,
    0, -159,  -12,  -33,  -51,  -63,  -13,    3, -228,  -73,
 -228,  -73,    0, -260, -179,    0, -163,  -11,    0,    0,
 -168,    0,    0,    0, -229,   -9, -189,    4,   -8, -159,
 -159,    0,    0,    0, -159,   -2,  -41,    2, -146, -166,
   13,    6,  -81,   11,   12,  -73,    0,  -73,    0,    0,
    0, -111,  -93,    0,    0,    0,    0, -272, -245,    0,
    0,    0,    0,    0,    0, -168,    0, -168,    7,    8,
    9,   -5,    0, -222,    0, -163,    5,    0, -111,    0,
   40,   45,    0,    0, -168,  -73,   -8,    0, -260,   33,
   42, -260, -126,    0,    0,    0,    0,    0,    0,   46,
    0,   55,    0,    0,    0,   48,    0,   49,    0,    0,
    0, -111,  -65,    0,    0,    0, -272,    0, -137, -194,
   36,    0,    0, -205, -256,    0, -111, -111, -111, -168,
    0, -111, -111, -111,    0,    0, -111,   52,    0,    0,
  -98,    0, -234,   63,   -5, -168,    0,  -39,  -52, -159,
 -159, -159,    0,    0,    0,   58,   57,   48,   59, -171,
    0, -272,    0,    0,    0,    0,    0,    0,    0, -167,
 -193, -259,    0,  -38,   39,    0,  -65,   40,    0,    0,
    0, -231,   75,   -5,    0,    0,    0,    0,    0,   62,
 -260,    0, -260,    0,    0,   64,   56,   65,    0,    0,
   60,    0,    0, -111, -111,   66,    0,    0,    0,   70,
  -73,   71,   71,    0,  -39, -159,   76,   67,    0,   74,
   57,    0, -142,  -65, -221,    0,    0,   90,    0,   77,
    0,    0,    0,    0,    0,    0,  -44,   64,    0,    0,
    0, -111,   92,  100,   80,    0,    0,    0,    0,    0,
  110,  108,  107,    0,  101,  112,    0,
};
short yyrindex[] = {                                    403,
    0,    0,    0,  115,    0,    0,  392,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  116,  117,  118,  -80,    0,    0,    0,  120,    0,
    0,    0,    0,    0,  646,    0,  671,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  704,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  727,    0,    0,
  763,    0,    0,    0,  799,    0,    0,  821,    0,  843,
    0,    0,    0,    0,  865,  888,    0,    0,    0,  704,
    0,  704,    0,  104,    0,    0,  -60,    0,    0,    0,
    0,    0,    0,    0,    0,  126,    0, -139,    0,  114,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  704,    0,  704,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    1,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  301,    0,    0,    0,   61,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  917,    0,    0,    0,
  119,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  528,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  -59,    0,    0,    0,    0,    0,    0,    0,
 -135,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  455,  942,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  975,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  122,    0,  181,  121,    0,  241,    0,    0,    0,
    0,    0,  492,  433,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  332,  572,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  704,  610,  610,    0,    0,    0,    0,  998,    0,    0,
    0,    0,    0, -197,  122,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
    0,    0,  109,    0,    0,  201,  -74,    0,  236, -136,
 -181,    0, -182,    0,    0,    0,   50,  -85,  -84,    0,
    0,  -49, -223,    0,    0,    0,    0,  175,  407,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  -43,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  272,    0,    0,
   85,    0,   86,    0,    0,    0,    0,  214, -214,    0,
    0,    0,    0,  123,    0,    0,    0,  -45,    0,    0,
    0,    0,    0,    0,    0,    0,  285,  284,    0,    0,
    0,    0,    0,    0,    0,    0,  103,    0,    0,   91,
    0,    0,    0,    0, -224,
};
#define YYTABLESIZE 1297
short yytable[] = {                                     243,
  160,  112,  113,  177,  278,  179,  110,  281,  147,  148,
  134,  288,  136,  255,  254,  135,  256,  142,  180,  335,
  245,  246,  247,  291,  181,  119,  269,   98,  261,   63,
   99,  149,  252,  275,   72,  248,  139,  100,  101,  240,
  140,  241,  120,  310,  141,  299,  338,  253,  102,  143,
  262,  263,  264,  262,  263,  264,  265,  266,  311,  290,
  161,  339,  261,  330,  176,  158,  178,  122,  332,  149,
  114,   94,  115,  116,  300,  301,  302,  372,  303,  304,
  305,  306,  123,  124,  307,   73,  170,  262,  263,  264,
  265,  266,  111,  138,  314,   75,  298,  147,  148,  152,
  153,  138,  154,   96,  138,  334,  345,  293,  346,  260,
  219,  222,  155,  213,  294,  224,  262,  263,  264,  183,
  128,  132,  184,  234,  190,  191,  192,  193,  194,  195,
  299,  196,  165,  111,  333,  229,  185,  127,  262,  263,
  264,  197,  262,  263,  264,  186,  129,  198,  199,  200,
  201,  353,  354,   99,   99,   99,   99,   99,   99,  371,
   99,  292,  133,  143,  293,  336,  143,  262,  263,  264,
   99,  294,  282,  143,  137,  283,   99,   99,   99,   99,
  129,  190,  191,  192,  193,  194,  195,   76,  242,  354,
  105,  138,   77,   78,   79,  106,  107,  108,  197,  144,
  308,   81,   82,  309,  359,  199,  200,  201,   83,   89,
   90,  111,   53,  233,   85,   86,   91,  149,  373,   49,
   87,  159,   64,   65,   66,   67,  173,  318,  316,  319,
  321,  322,  323,   68,   69,   70,   71,  162,  110,  109,
  121,  110,  109,  160,  262,  263,  264,  165,  376,  161,
  377,  123,  124,  111,  315,  172,  160,  160,  160,  160,
  160,  160,  160,  160,  160,  160,  160,  160,  160,  160,
  160,  163,  263,  264,  164,  160,  160,  160,  166,  160,
  171,  167,  168,  169,  216,  160,  174,  175,  214,  189,
  211,  160,  225,  160,  227,  316,  366,  230,  231,  160,
  114,  154,  160,  238,  239,  268,  257,  258,  259,  260,
  160,  160,  160,  160,  160,  160,  161,  161,  161,  161,
  161,  161,  161,  161,  161,  161,  161,  161,  161,  161,
  161,  219,  270,  273,  279,  161,  161,  161,  284,  161,
  280,  285,  233,  297,  253,  161,  312,  289,  324,  327,
  264,  161,  331,  161,  340,  344,  347,  360,  350,  161,
  367,  352,  161,  351,  358,  368,  369,  357,  382,  375,
  161,  161,  161,  161,  161,  161,  128,  128,  128,  128,
  128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
  128,  193,  374,  380,  381,  128,  128,  128,  383,  128,
  384,  385,    3,  386,  154,  128,  387,  177,   33,   39,
   45,  128,   97,  128,   74,  166,  337,  277,  244,  128,
  165,  379,  128,  115,  296,   74,  210,  363,  364,  276,
  128,  128,  104,  128,  128,  128,  129,  129,  129,  129,
  129,  129,  129,  129,  129,  129,  129,  129,  129,  129,
  129,  226,  228,  370,   82,  129,  129,  129,  378,  129,
    0,    0,    0,    0,    0,  129,    0,  365,    0,    0,
    0,  129,    0,  129,    0,    0,    0,    0,    0,  129,
    0,    0,  129,    0,    0,    0,    0,    0,    0,    0,
  129,   87,    0,  129,  129,  129,  121,  121,  121,  121,
  121,  121,  121,  121,  121,  121,  121,  121,  121,  121,
  121,    0,    0,    0,    0,  121,  121,  121,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  215,    0,    0,
    0,    0,    0,  121,    0,    0,    0,    0,    0,    0,
    0,    0,  121,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  121,  114,  114,  114,  114,
  114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
  114,  214,    0,    0,    0,  114,  114,  114,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  219,  219,  219,
  219,  219,  219,  219,  219,  219,  219,  219,  219,  219,
  219,  219,  114,    0,    0,    0,  219,  219,  219,   88,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  219,    0,    0,    0,    0,    0,    0,    0,
  219,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  189,    0,  193,  193,  193,
  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
  193,  193,    0,    0,    0,    0,  193,  193,  193,  193,
  191,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  193,  193,    0,    0,  104,  104,
  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,
  104,  104,  104,  115,    0,    0,    0,  104,  104,  104,
   82,   82,   82,   82,   82,   82,   82,   82,   82,   82,
   82,   82,   82,   82,   82,  104,  179,    0,    0,   82,
   82,   82,    0,    0,   82,    0,    0,    0,    0,    0,
    0,   82,    0,    0,    0,   82,    0,   87,   87,   87,
   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,
   87,   87,  180,    0,    0,    0,   87,   87,   87,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   87,    0,
    0,    0,   87,  215,  215,  215,  215,  215,  215,  215,
  215,  215,  215,  215,  215,  215,  215,  215,  188,    0,
    0,    0,  215,  215,  215,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  215,    0,
  190,    0,    0,    0,    0,    0,    0,  214,  214,  214,
  214,  214,  214,  214,  214,  214,  214,  214,  214,  214,
  214,  214,  192,    0,    0,    0,  214,  214,  214,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  214,    0,  230,   88,   88,   88,   88,   88,
   88,   88,   88,   88,   88,   88,   88,   88,   88,   88,
    0,    0,    0,    0,   88,   88,   88,  233,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   88,  189,  189,  189,  189,  189,  189,  189,  189,  189,
  189,  189,  189,  189,  189,  189,  100,    0,    0,    0,
  189,  189,  189,    0,    0,    0,  191,  191,  191,  191,
  191,  191,  191,  191,  191,  191,  191,  191,  191,  191,
  191,   91,    0,    0,    0,  191,  191,  191,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  115,
  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
  115,  115,  115,  115,  222,    0,    0,    0,  115,  115,
  115,    0,  179,  179,  179,  179,  179,  179,  179,  179,
  179,  179,  179,  179,  179,  179,  179,  221,    0,    0,
    0,  179,  179,  179,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  180,  180,
  180,  180,  180,  180,  180,  180,  180,  180,  180,  180,
  180,  180,  180,    0,    0,    0,    0,  180,  180,  180,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  188,  188,  188,  188,  188,  188,
  188,  188,  188,  188,  188,  188,  188,  188,  188,    0,
    0,    0,    0,  188,  188,  188,  190,  190,  190,  190,
  190,  190,  190,  190,  190,  190,  190,  190,  190,  190,
  190,    0,    0,    0,    0,  190,  190,  190,  192,  192,
  192,  192,  192,  192,  192,  192,  192,  192,  192,  192,
  192,  192,  192,    0,    0,    0,    0,  192,  192,  192,
  230,  230,  230,  230,  230,  230,  230,  230,  230,  230,
  230,  230,  230,  230,  230,    0,    0,    0,    0,  230,
  230,  230,    0,  233,  233,  233,  233,  233,  233,  233,
  233,  233,  233,  233,  233,  233,  233,  233,    0,    0,
    0,    0,  233,  233,  233,    0,    0,    0,    0,    0,
    0,    0,  100,  100,  100,  100,  100,  100,  100,  100,
  100,  100,  100,  100,  100,  100,  100,    0,    0,    0,
    0,  100,  100,  100,    0,    0,    0,   91,   91,   91,
   91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
   91,   91,    0,    0,    0,    0,   91,   91,   91,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,
  222,  222,  222,  222,  222,    0,    0,    0,    0,  222,
  222,  222,    0,  221,  221,  221,  221,  221,  221,  221,
  221,  221,  221,  221,  221,  221,  221,  221,    0,    0,
    0,    0,  221,  221,  221,    1,    2,    3,    4,    5,
    6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
    0,    0,    0,    0,   16,   17,   18,
};
short yycheck[] = {                                     182,
    0,   51,   52,  140,  219,  142,   50,  222,   94,   94,
   60,  236,   62,  196,  196,   61,  198,   92,  279,  279,
  293,  294,  295,  247,  285,  278,  209,  257,  285,  292,
  260,  291,  278,  215,  293,  308,   80,  267,  268,  176,
   84,  178,  295,  278,   88,  302,  278,  293,  278,   93,
  310,  311,  312,  310,  311,  312,  313,  314,  293,  242,
    0,  293,  285,  288,  139,  109,  141,  278,  292,  291,
  271,  300,  273,  274,  257,  258,  259,  299,  260,  262,
  263,  264,  293,  294,  267,  293,  132,  310,  311,  312,
  313,  314,  293,  291,  276,  286,  302,  183,  183,  286,
  287,  299,  289,  293,  302,  299,  321,  302,  323,  315,
  160,  161,  299,  157,  309,  165,  310,  311,  312,  299,
    0,  300,  302,  173,  293,  294,  295,  296,  297,  298,
  302,  300,  299,  293,  302,  302,  300,  293,  310,  311,
  312,  310,  310,  311,  312,  309,  293,  316,  317,  318,
  319,  334,  335,  293,  294,  295,  296,  297,  298,  302,
  300,  299,  287,  299,  302,  302,  302,  310,  311,  312,
  310,  309,  299,  309,  293,  302,  316,  317,  318,  319,
    0,  293,  294,  295,  296,  297,  298,  282,  300,  372,
  281,  293,  287,  288,  289,  286,  287,  288,  310,  293,
  299,  280,  281,  302,  341,  317,  318,  319,  287,  282,
  283,  293,  293,  295,  281,  282,  289,  291,  355,  300,
  287,  284,  271,  272,  273,  274,  290,  280,  278,  282,
  280,  281,  282,  271,  272,  273,  274,  293,  299,  299,
    0,  302,  302,  300,  310,  311,  312,  299,  293,  300,
  295,  293,  294,  293,  294,  289,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  293,  311,  312,  293,  275,  276,  277,  299,  279,
  293,  299,  299,  299,  293,  285,  300,  285,  285,  301,
  300,  291,  295,  293,  293,  345,  346,  285,  293,  299,
    0,  301,  302,  293,  293,  301,  300,  300,  300,  315,
  310,  311,  312,  313,  314,  315,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,    0,  293,  289,  302,  275,  276,  277,  293,  279,
  299,  287,  295,  308,  293,  285,  284,  299,  291,  293,
  312,  291,  294,  293,  280,  294,  293,  287,  303,  299,
  285,  302,  302,  299,  295,  299,  293,  302,  289,  293,
  310,  311,  312,  313,  314,  315,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,    0,  303,  302,  295,  275,  276,  277,  289,  279,
  293,  295,    0,  303,  301,  285,  295,  293,  293,  293,
  293,  291,  293,  293,  289,  302,  308,  217,  183,  299,
  302,  372,  302,  302,  250,   19,  155,  343,  343,  216,
  310,  311,    0,  313,  314,  315,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  167,  169,  351,    0,  275,  276,  277,  368,  279,
   -1,   -1,   -1,   -1,   -1,  285,   -1,  345,   -1,   -1,
   -1,  291,   -1,  293,   -1,   -1,   -1,   -1,   -1,  299,
   -1,   -1,  302,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  310,    0,   -1,  313,  314,  315,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,   -1,   -1,   -1,   -1,  275,  276,  277,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,  293,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  302,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  315,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,    0,   -1,   -1,   -1,  275,  276,  277,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,  302,   -1,   -1,   -1,  275,  276,  277,    0,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  291,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,    0,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,   -1,   -1,   -1,   -1,  275,  276,  277,  278,
    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  293,  294,   -1,   -1,  256,  257,
  258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,    0,   -1,   -1,   -1,  275,  276,  277,
  256,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  293,    0,   -1,   -1,  275,
  276,  277,   -1,   -1,  280,   -1,   -1,   -1,   -1,   -1,
   -1,  287,   -1,   -1,   -1,  291,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,    0,   -1,   -1,   -1,  275,  276,  277,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  287,   -1,
   -1,   -1,  291,  256,  257,  258,  259,  260,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,    0,   -1,
   -1,   -1,  275,  276,  277,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  291,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,    0,   -1,   -1,   -1,  275,  276,  277,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  291,   -1,    0,  256,  257,  258,  259,  260,
  261,  262,  263,  264,  265,  266,  267,  268,  269,  270,
   -1,   -1,   -1,   -1,  275,  276,  277,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  291,  256,  257,  258,  259,  260,  261,  262,  263,  264,
  265,  266,  267,  268,  269,  270,    0,   -1,   -1,   -1,
  275,  276,  277,   -1,   -1,   -1,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,    0,   -1,   -1,   -1,  275,  276,  277,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,
  257,  258,  259,  260,  261,  262,  263,  264,  265,  266,
  267,  268,  269,  270,    0,   -1,   -1,   -1,  275,  276,
  277,   -1,  256,  257,  258,  259,  260,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,    0,   -1,   -1,
   -1,  275,  276,  277,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,
  258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,   -1,   -1,   -1,   -1,  275,  276,  277,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  256,  257,  258,  259,  260,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,   -1,
   -1,   -1,   -1,  275,  276,  277,  256,  257,  258,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,   -1,   -1,   -1,   -1,  275,  276,  277,  256,  257,
  258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,   -1,   -1,   -1,   -1,  275,  276,  277,
  256,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,   -1,   -1,   -1,   -1,  275,
  276,  277,   -1,  256,  257,  258,  259,  260,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  256,  257,  258,  259,  260,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,   -1,   -1,   -1,
   -1,  275,  276,  277,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,   -1,   -1,   -1,   -1,  275,  276,  277,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  256,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,   -1,   -1,   -1,   -1,  275,
  276,  277,   -1,  256,  257,  258,  259,  260,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  256,  257,  258,  259,  260,
  261,  262,  263,  264,  265,  266,  267,  268,  269,  270,
   -1,   -1,   -1,   -1,  275,  276,  277,
};
#define YYFINAL 19
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 320
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"APPEND","COPY","CREATE","DELETE",
"DESTROY","HELP","INDEX","MODIFY","PRINT","RANGE","REPLACE","RETRIEVE","SAVE",
"DEFINE","PERMIT","VIEW","INTEGRITY","DELIM","USE","UNUSE","DISTRIBUTE","ALL",
"BY","FROM","IN","INTO","UNIQUE","AT","IS","OF","ON","ONTO","TO","UNTIL",
"WHERE","DISTRD","NAME","SCONST","I2CONST","I4CONST","F4CONST","F8CONST",
"COMMA","LPAREN","PERIOD","RPAREN","COLON","BGNCMNT","ENDCMNT","LBRAC","RBRAC",
"DOLLAR","PCT","UAOP","BAOP","BAOPH","BDOP","EOP","LBOP","LUOP","FOP","FBOP",
"AGOP","unaryop",
};
char *yyrule[] = {
"$accept : program",
"program : program stmnt",
"program : stmnt",
"program :",
"stmnt : append",
"stmnt : copy",
"stmnt : create",
"stmnt : delete",
"stmnt : destroy",
"stmnt : distribute",
"stmnt : help",
"stmnt : index",
"stmnt : integrity",
"stmnt : modify",
"stmnt : permit",
"stmnt : print",
"stmnt : range",
"stmnt : replace",
"stmnt : retrieve",
"stmnt : save",
"stmnt : view",
"stmnt : use",
"stmnt : unuse",
"stmnt : delim",
"stmnt : error",
"range : rngstmnt OF NAME IS NAME",
"rngstmnt : RANGE",
"append : apstmnt apto relation tlclause qualclause",
"apstmnt : APPEND",
"apto : INTO",
"apto : ONTO",
"apto : TO",
"apto : ON",
"apto :",
"delete : delstmnt delwd relation qualclause",
"delstmnt : DELETE",
"delwd : IN",
"delwd : ON",
"delwd : FROM",
"delwd :",
"replace : repstmnt repkwd relation tlclause qualclause",
"repstmnt : REPLACE",
"repkwd : INTO",
"repkwd : IN",
"repkwd : ON",
"repkwd :",
"retrieve : retstmnt retclause tlclause qualclause",
"retstmnt : RETRIEVE",
"retclause : retkwd relation",
"retclause :",
"retclause : UNIQUE",
"retkwd : INTO",
"retkwd : TO",
"retkwd :",
"delim : DEFINE DELIM NAME LPAREN NAME COMMA SCONST RPAREN",
"use : USE NAME",
"unuse : UNUSE NAME",
"view : viewclause tlclause qualclause",
"viewclause : viewstmnt relation",
"viewstmnt : DEFINE VIEW",
"permit : permstmnt permlist permrel permtarg permwho permplace permtd qualclause",
"permstmnt : DEFINE PERMIT",
"permlist : permxlist",
"permlist : permlist COMMA permxlist",
"permxlist : ALL",
"permxlist : RETRIEVE",
"permxlist : DELETE",
"permxlist : APPEND",
"permxlist : REPLACE",
"permrel : permword relation",
"permword : ON",
"permword : OF",
"permword : TO",
"permtarg : LPAREN permtlist RPAREN",
"permtarg :",
"permtlist : permtlelm",
"permtlist : permtlist COMMA permtlelm",
"permtlelm : NAME",
"permwho : TO NAME",
"permwho : TO ALL",
"permplace : AT NAME",
"permplace : AT ALL",
"permplace :",
"permtd : permtime permday",
"permtd : permdeftime permday",
"permtd : permtime permdefday",
"permtd : permdeftime permdefday",
"permdeftime :",
"permdefday :",
"permtime : FROM I2CONST COLON I2CONST TO I2CONST COLON I2CONST",
"permday : ON NAME TO NAME",
"integrity : integstmnt integnoise relation integis qual",
"integstmnt : DEFINE INTEGRITY",
"integnoise : ON",
"integnoise : ONTO",
"integnoise : IN",
"integnoise : OF",
"integnoise :",
"integis : IS",
"integis :",
"distribute : diststmnt relation AT distcrits",
"diststmnt : DISTRIBUTE",
"distcrits : dcriterion",
"distcrits : distcrits dcriterion",
"dcriterion : NAME where qual",
"relation : NAME",
"tlclause : LPAREN tlist RPAREN",
"tlist : tlelm",
"tlist : tlist COMMA tlelm",
"tlelm : NAME is afcn",
"tlelm : attrib",
"tlelm : var PERIOD ALL",
"is : IS",
"is : BY",
"qualclause : where qual",
"qualclause :",
"where : WHERE",
"qual : LPAREN qual RPAREN",
"qual : LUOP qual",
"qual : qual LBOP qual",
"qual : clause",
"clause : afcn relop afcn",
"relop : EOP",
"relop : IS",
"relop : BDOP",
"afcn : aggrfcn",
"afcn : attribfcn",
"afcn : afcn BAOPH afcn",
"afcn : afcn BAOP afcn",
"afcn : afcn UAOP afcn",
"afcn : LPAREN afcn RPAREN",
"afcn : uop afcn",
"afcn : FOP LPAREN afcn RPAREN",
"afcn : FBOP LPAREN afcn COMMA afcn RPAREN",
"aggrfcn : AGOP LPAREN afcn BY domseq qualclause RPAREN",
"aggrfcn : AGOP LPAREN afcn qualclause RPAREN",
"domseq : targdom",
"domseq : domseq COMMA targdom",
"targdom : afcn",
"nameprt : NAME",
"nameprt : SCONST",
"subelm : DOLLAR",
"subelm : nameprt DOLLAR",
"subelm : nameprt",
"subelm : I2CONST subelm",
"grpelm : subelm COMMA subelm",
"leftclose : PCT",
"leftclose : LPAREN",
"rightclose : PCT",
"rightclose : RPAREN",
"stringpart : leftclose subelm rightclose",
"stringpart : leftclose grpelm rightclose",
"attrib : var PERIOD NAME",
"attrib : attrib stringpart",
"var : NAME",
"attribfcn : I2CONST",
"attribfcn : I4CONST",
"attribfcn : F4CONST",
"attribfcn : F8CONST",
"attribfcn : SCONST",
"attribfcn : NAME",
"attribfcn : attrib",
"uop : UAOP",
"copy : copstmnt alias LPAREN coparam RPAREN keywd SCONST",
"copstmnt : COPY",
"coparam : cospecs",
"coparam :",
"cospecs : alias is coent",
"cospecs : cospecs COMMA alias is coent",
"coent : alias",
"coent : SCONST",
"alias : NAME",
"specs : alias is alias",
"specs : specs COMMA alias is alias",
"keywd : INTO",
"keywd : FROM",
"create : crestmnt alias LPAREN specs RPAREN",
"crestmnt : CREATE",
"crestmnt : CREATE DISTRD",
"destroy : destmnt keys",
"destroy : destqm destlist",
"destroy : destmnt DELIM NAME",
"destmnt : DESTROY",
"destqm : destmnt INTEGRITY NAME",
"destqm : destmnt PERMIT NAME",
"destlist : I2CONST",
"destlist : destlist COMMA I2CONST",
"destlist : ALL",
"help : helstmnt hlist",
"help : helstmnt",
"help : helqmstmnt hqmlist",
"help : heldelstmnt",
"help : heldelstmnt dlist",
"helstmnt : HELP",
"heldelstmnt : HELP DELIM",
"helqmstmnt : HELP VIEW",
"helqmstmnt : HELP PERMIT",
"helqmstmnt : HELP INTEGRITY",
"hlist : hparam",
"hlist : hlist COMMA hparam",
"hlist : ALL",
"dlist : dparam",
"dlist : dlist COMMA dparam",
"dparam : NAME",
"hparam : NAME",
"hparam : SCONST",
"hqmlist : NAME",
"hqmlist : hqmlist COMMA NAME",
"index : instmnt LPAREN keys RPAREN",
"instmnt : indexq ON NAME IS NAME",
"indexq : INDEX",
"modify : modstmnt alias TO modstorage modkeys modqual",
"modstmnt : MODIFY",
"modstorage : NAME",
"modkeys : modstkey modrptkey",
"modkeys :",
"modstkey : ON",
"modrptkey : modbasekey",
"modrptkey : modrptkey COMMA modbasekey",
"modbasekey : NAME",
"modbasekey : NAME COLON NAME",
"modqual : modcond modfill",
"modqual :",
"modcond : WHERE",
"modfill : modfillnum",
"modfill : modfill COMMA modfillnum",
"modfillnum : NAME IS I2CONST",
"modfillnum : NAME IS NAME",
"keys : alias",
"keys : keys COMMA alias",
"print : prinstmnt keys",
"prinstmnt : PRINT",
"save : savstmnt alias UNTIL date",
"save : savstmnt alias",
"savstmnt : SAVE",
"date : month day_year day_year",
"month : alias",
"month : day_year",
"day_year : I2CONST",
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
#line 1343 "grammar.y"
#include	"scanner.h"
#include	"tables.y"
#include	"yyerror.y"
#line 905 "y.tab.c"
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
case 1:
#line 172 "grammar.y"

		{
#ifdef	xPTR1
			tTfp(38, 0, "*** [program stmnt] parsed.\n");
#endif

			if (endquelst(Opflag) < 0)
				return (-1);
		}
break;
case 2:
#line 181 "grammar.y"

		{
#ifdef	xPTR1
			tTfp(38, 1, "*** [stmnt] parsed.\n");
#endif

			if (endquelst(Opflag) < 0)
				return (-1);
		}
break;
case 3:
#line 191 "grammar.y"
{
#ifdef	xPTR1
			tTfp(38, 2, "*** [(NULL)] parsed.\n");
#endif
		}
break;
case 24:
#line 218 "grammar.y"
{
#ifdef	xPTR1
			tTfp(38, 0, "*** [error] parsed.\n");
#endif
		}
break;
case 25:
#line 225 "grammar.y"

		{
			if ((i = openr(&Reldesc, OR_RELTID, yyvsp[0].string_type)) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, yyvsp[0].string_type, 0, 0);
				YYERROR;
			}
			else
				rngent(R_EXTERNAL, yyvsp[-2].string_type, &Reldesc);
		}
break;
case 26:
#line 239 "grammar.y"

		{
			Opflag = mdRANGE;
		}
break;
case 27:
#line 244 "grammar.y"

		{
			/* make root node */
			Lastree = par_tree(yyvsp[-1].tree_type, yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 28:
#line 250 "grammar.y"

		{
			Opflag = mdAPP;
		}
break;
case 34:
#line 261 "grammar.y"

		{
			/* make root node for delete, with a TIDNODE at leftmost */
			Lastree = par_tree(par_tree(NULL, Tidnode, RESDOM, sizeof(resdomnode_t), NULL, 0), yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 35:
#line 267 "grammar.y"

		{
			Opflag = mdDEL;
		}
break;
case 40:
#line 277 "grammar.y"

		{
			/* make root node for replace */
			Lastree = par_tree(yyvsp[-1].tree_type, yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 41:
#line 283 "grammar.y"

		{
			Opflag = mdREPL;
		}
break;
case 46:
#line 293 "grammar.y"

		{
			/* make root node for retrieve */
			Lastree = par_tree(yyvsp[-1].tree_type, yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 47:
#line 299 "grammar.y"

		{
			Opflag = mdRETR;
		}
break;
case 48:
#line 304 "grammar.y"

		{
			/* set up pipe block and save relname for create */
#ifdef	xPTR2
			tTfp(38, 4, "retclause: Rsdmno %d", Rsdmno);
#endif
			Rsdmno = 0;
			setp(PV_STR, "0", 0);	/* r_status = nil */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
break;
case 49:
#line 314 "grammar.y"

		{
			/* no result relation, output to terminal */
			Rsdmno = 0;
			Resrng = -1;
		}
break;
case 50:
#line 320 "grammar.y"

		{
			Opflag = mdRET_UNI;
			Rsdmno = 0;
			Resrng = -1;
		}
break;
case 54:
#line 332 "grammar.y"
 
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
				if (( i = make_tuples(&Reldesc, yyvsp[-5].string_type, yyvsp[-3].string_type, yyvsp[-1].string_type)) < 0)
				{
					closer(&Reldesc);
					par_error(BADBNF, WARN, "rdelim", 0, 0);
				}
			}
			closer(&Reldesc);
		}
break;
case 55:
#line 354 "grammar.y"

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
				if ((i = make_list(&Reldesc, yyvsp[0].string_type)) < 0)
				{
					closer(&Reldesc);
					par_error(DELEXIST, WARN, 0, 0, 0);
				}
			}
			closer(&Reldesc);
		}
break;
case 56:
#line 376 "grammar.y"

		{
			Opflag = mdSTOP;
			if (( i = shrink_list(yyvsp[0].string_type)) < 0)
			{
				par_error(NOGRP,WARN, 0, 0, 0);
			}
		}
break;
case 57:
#line 386 "grammar.y"

		{
			Lastree = par_tree(yyvsp[-1].tree_type, yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 58:
#line 391 "grammar.y"

		{
			Rsdmno = 0;
			setp(PV_STR, "0040", 0);	/* r_status = S_VIEW */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
break;
case 59:
#line 398 "grammar.y"

		{
			Opflag = mdVIEW;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
break;
case 60:
#line 408 "grammar.y"

		{
			Lastree = par_tree(yyvsp[-4].tree_type, yyvsp[0].tree_type, ROOT, sizeof(struct rootnode), 1, 0);
		}
break;
case 61:
#line 413 "grammar.y"

		{
			Opflag = mdPROT;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
break;
case 64:
#line 426 "grammar.y"

		{
			permcom(-1);	/* means 'all' commands */
		}
break;
case 65:
#line 430 "grammar.y"

		{
			permcom(mdRETR);
		}
break;
case 66:
#line 434 "grammar.y"

		{
			permcom(mdDEL);
		}
break;
case 67:
#line 438 "grammar.y"

		{
			permcom(mdAPP);
		}
break;
case 68:
#line 442 "grammar.y"

		{
			permcom(mdREPL);
		}
break;
case 69:
#line 447 "grammar.y"

		{
			/* put command vector into list now since this always happens */
			setp(PV_INT, &Permcomd, sizeof(Permcomd));
			Permcomd = 0;		/* reset command map */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
			bmove(Parrng[Resrng].vardesc.d_r.r_owner, permbuf, sizeof(Parrng[Resrng].vardesc.d_r.r_owner));
			permbuf[2] = 0;
			setp(PV_STR, permbuf, 0);
		}
break;
case 73:
#line 462 "grammar.y"

		{
			yyval.tree_type = yyvsp[-1].tree_type;
		}
break;
case 74:
#line 466 "grammar.y"

		{
			yyval.tree_type = NULL;
		}
break;
case 76:
#line 472 "grammar.y"

		{
			/*
			** attach bulk of permit tl to leftmost node of new elem
			*/
			if (!Err_current)
				yyval.tree_type = tlprepend(yyvsp[-2].tree_type, yyvsp[0].tree_type);
		}
break;
case 77:
#line 481 "grammar.y"

		{
			/* Resrng is set by the "relation" production */
			if (!Err_current)
			{
				Trname = yyvsp[0].string_type;
				aptr = attlookup(Resrng, Trname);
				yyval.tree_type = par_tree(NULL, NULL, VAR, sizeof(varnode_t), Resrng, aptr);
				yyval.tree_type = addresdom(NULL, yyval.tree_type);
			}
		}
break;
case 78:
#line 493 "grammar.y"

		{
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 79:
#line 497 "grammar.y"

		{
			setp(PV_STR, "all", 0);
		}
break;
case 80:
#line 502 "grammar.y"

		{
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 81:
#line 506 "grammar.y"

		{
			setp(PV_STR, "all", 0);
		}
break;
case 82:
#line 510 "grammar.y"

		{
			setp(PV_STR, "all", 0);		/* default is all */
		}
break;
case 87:
#line 520 "grammar.y"

		{
			int i;

			i = 0;
			setp(PV_INT, &i, sizeof(i));
			i = 1440;
			setp(PV_INT, &i, sizeof(i));
		}
break;
case 88:
#line 530 "grammar.y"

		{
			setp(PV_STR, "sun", 0);
			setp(PV_STR, "sat", 0);
		}
break;
case 89:
#line 536 "grammar.y"

		{
			int	i;

			i = timeofday(yyvsp[-6].I2_type, yyvsp[-4].I2_type);
			setp(PV_INT, &i, sizeof(i));
			i = timeofday(yyvsp[-2].I2_type, yyvsp[0].I2_type);
			setp(PV_INT, &i, sizeof(i));
		}
break;
case 90:
#line 546 "grammar.y"

		{
			setp(PV_STR, yyvsp[-2].string_type, 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 91:
#line 552 "grammar.y"

		{
			Lastree = par_tree(NULL, norml(yyvsp[0].tree_type), ROOT, sizeof(struct rootnode), 1, 0);
			Qlflag--;	/* turn off here */
		}
break;
case 92:
#line 558 "grammar.y"

		{
			Opflag = mdINTEG;
			Qlflag++;	/* OK to turn on here because integrity doesn't have a targ list */
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			}
		}
break;
case 100:
#line 578 "grammar.y"

/*DDD*/		{
/*DDD*/			if (!Err_current)
/*DDD*/			{
/*DDD*/				yyval.tree_type = par_tree(NULL, NULL, QLEND, 0, 0, 0);
/*DDD*/				Lastree = par_tree(yyvsp[0].tree_type, yyval.tree_type, ROOT, sizeof(struct rootnode), 1, 0);
/*DDD*/			}
/*DDD*/		}
break;
case 101:
#line 587 "grammar.y"

/*DDD*/				Opflag = mdDISTRIB;
break;
case 102:
#line 590 "grammar.y"

/*DDD*/		{
/*DDD*/			yyval.tree_type = yyvsp[0].tree_type;
/*DDD*/		}
break;
case 103:
#line 594 "grammar.y"

/*DDD*/		{
/*DDD*/			yyval.tree_type = tlprepend(yyvsp[-1].tree_type, yyvsp[0].tree_type);
/*DDD*/		}
break;
case 104:
#line 599 "grammar.y"

/*DDD*/		{
/*DDD*/			Qlflag--;
/*DDD*/			syserr("Warning this node may be the wrong size\n");
/*DDD*/			if (!Err_current)
/*DDD*/				yyval.tree_type = par_tree(NULL, norml(yyvsp[0].tree_type), SITE, 2, yyvsp[-2].string_type, 0);
/*DDD*/		}
break;
case 105:
#line 607 "grammar.y"

		{
#ifdef	xPTR2
			tTfp(38, 3, "res rel name/var: '%s'\n", yyvsp[0].string_type);
#endif
			switch (Opflag)
			{
			  case mdRETR:
			  case mdVIEW:
				/* result better not be a rel name */
				if ((i = openr(&Reldesc, OR_RELTID, yyvsp[0].string_type)) < 0)
					syserr("relation: err openr '%d'", i);
				if (i == 0)
				{
					/* reln exists */
					if (bequal(Reldesc.d_r.r_owner, Usercode, USERCODE_SIZE))
					{
						/* same owner, can't duplicate name */
						par_error(RESEXIST, WARN, yyvsp[0].string_type, 0, 0);
						YYERROR;
					}
					else if (!Err_current)
					{
						/* owned by dba -- purge range table */
						rngdel(yyvsp[0].string_type);
					}
				}
				if (!Err_current)
				{
					bmove(Usercode, Reldesc.d_r.r_owner, USERCODE_SIZE);
					pmove(yyvsp[0].string_type, Reldesc.d_r.r_id, MAX_NAME_SIZE, ' ');
					Resrng = rngent(R_INTERNAL, "", &Reldesc);
				}
				break;

			  case mdAPP:
				/* result is a rel name */
				if (!Err_current)
				{
					Resrng = rnglook(yyvsp[0].string_type, LOOKREL);
					if (Resrng < 0)
					{
						if ((i = openr(&Reldesc, OR_RELTID, yyvsp[0].string_type)) < 0)
							syserr("relation: err openr '%d'", i);
						if (i)
						{
							/* invalid relation name */
							par_error(RESAPPEX, WARN, yyvsp[0].string_type, 0, 0);
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
				Resrng = rnglook(yyvsp[0].string_type, LOOKVAR);
				if (Resrng < 0)
				{
					/* variable not declared */
					par_error(NOVBLE, WARN, yyvsp[0].string_type, 0, 0);
					YYERROR;
				}
				else
					ctlmod_decl(Resrng);
				break;

			  case mdREPL:
			  case mdDEL:
				/* the result is a tuple variable */
				Resrng = rnglook(yyvsp[0].string_type, LOOKVAR);
				if (Resrng < 0)
					/* variable not declared */
				{
					par_error(NOVBLE, WARN, yyvsp[0].string_type, 0, 0);
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
break;
case 106:
#line 703 "grammar.y"

		{
			yyval.tree_type = yyvsp[-1].tree_type;

			/*
			** replace must have tid node as left branch
			**	(so does delete but it doesn't have a targ list)
			*/
			if (Opflag == mdREPL && !Err_current)
			{
				yyval.tree_type = tlprepend(par_tree(NULL, Tidnode, RESDOM, sizeof(resdomnode_t), 0, 0), yyval.tree_type);
			}
		}
break;
case 108:
#line 718 "grammar.y"

		{
			/*
			** attach bulk of targ list to leftmost node
			** of new element
			*/
			if (!Err_current)
				yyval.tree_type = tlprepend(yyvsp[-2].tree_type, yyvsp[0].tree_type);
		}
break;
case 109:
#line 728 "grammar.y"

		{
			Trname = yyvsp[-2].string_type;
			/* make a new resdom entry for targ list */
			if (!Err_current)
				yyval.tree_type = addresdom(NULL, yyvsp[0].tree_type);
		}
break;
case 110:
#line 735 "grammar.y"

		{
		/* makes a new resdom entry for targ list */
			if (!Err_current)
				yyval.tree_type = addresdom(NULL, yyvsp[0].tree_type);
		}
break;
case 111:
#line 741 "grammar.y"

		{
			if (Opflag == mdREPL)
			{
				/* ALL not defined for REPLACE */
				par_error(REPALL, WARN,
				    trim_relname(Qt.qt_rangev[yyvsp[-2].rng_type].rngvdesc->d_rangevar), 0, 0);
				YYERROR;
			}
			/* makes set of new resdom entries for targ list */
			else if (!Err_current)
				yyval.tree_type = xdot(yyvsp[-2].rng_type);
		}
break;
case 114:
#line 758 "grammar.y"

		{
			yyval.tree_type = norml(yyvsp[0].tree_type);
			Qlflag--;
			if (Opflag == mdREPL)
			    qualindex();
		}
break;
case 115:
#line 765 "grammar.y"

		{
			/* null qualification */
			if (Opflag == mdREPL)
			    qualindex();
			yyval.tree_type = norml(NULL);
		}
break;
case 116:
#line 773 "grammar.y"

		{
			Qlflag++;
		}
break;
case 117:
#line 778 "grammar.y"

		{
			yyval.tree_type = yyvsp[-1].tree_type;
		}
break;
case 118:
#line 782 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, yyvsp[0].tree_type, UOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 119:
#line 786 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-2].tree_type, yyvsp[0].tree_type, yyvsp[-1].type_type, sizeof (struct rootnode) -2, 0, 0);
		}
break;
case 121:
#line 792 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-2].tree_type, yyvsp[0].tree_type, BOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 127:
#line 803 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-2].tree_type, yyvsp[0].tree_type, BOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 128:
#line 807 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-2].tree_type, yyvsp[0].tree_type, BOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 129:
#line 811 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-2].tree_type, yyvsp[0].tree_type, BOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 130:
#line 815 "grammar.y"

		{
			yyval.tree_type = yyvsp[-1].tree_type;
		}
break;
case 131:
#line 819 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, yyvsp[0].tree_type, UOP, 2, yyvsp[-1].type_type, 0);
		}
break;
case 132:
#line 823 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-1].tree_type, NULL, UOP, 2, yyvsp[-3].type_type, 0);
		}
break;
case 133:
#line 827 "grammar.y"

		{
			yyval.tree_type = par_tree(yyvsp[-3].tree_type, yyvsp[-1].tree_type, BOP, 2, yyvsp[-5].type_type, 0);
		}
break;
case 134:
#line 832 "grammar.y"

		{
#ifdef	xPTR2
			tTfp(39, 0, "agg func\n");
#endif
			windup(yyvsp[-2].tree_type);
			yyval.tree_type = par_tree(par_tree(yyvsp[-2].tree_type, par_tree(NULL, yyvsp[-4].tree_type, AOP, 6, yyvsp[-6].type_type, 0), BYHEAD, sizeof(resdomnode_t), 0, 0), yyvsp[-1].tree_type, AGHEAD, sizeof(struct rootnode), 0, 0);
			tlprepend(par_tree(NULL, NULL, TREE, 0, 0, 0), yyval.tree_type);
		}
break;
case 135:
#line 841 "grammar.y"

		{
			yyval.tree_type = par_tree(par_tree(NULL, yyvsp[-2].tree_type, AOP, 6, yyvsp[-4].type_type, 0), yyvsp[-1].tree_type,  AGHEAD, sizeof(struct rootnode), 0, 0);
		}
break;
case 137:
#line 847 "grammar.y"

		{
			yyval.tree_type = tlprepend(yyvsp[-2].tree_type, yyvsp[0].tree_type);
		}
break;
case 138:
#line 852 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, yyvsp[0].tree_type, RESDOM, sizeof(resdomnode_t), Rsdmno, 0);
		}
break;
case 139:
#line 857 "grammar.y"

		{
			yyval.substr_type = substring(yyvsp[0].string_type,1);
		}
break;
case 140:
#line 861 "grammar.y"

		{
			yyval.substr_type = substring(yyvsp[0].string_type,0);
		}
break;
case 141:
#line 866 "grammar.y"

		{ 
			yyval.substr_type = substring(NULL,0);
		}
break;
case 142:
#line 870 "grammar.y"

		{
			yyvsp[-1].substr_type->flag[0] |= 2;
			yyval.substr_type = yyvsp[-1].substr_type;
		}
break;
case 143:
#line 875 "grammar.y"

		{
			yyval.substr_type = yyvsp[0].substr_type;
		}
break;
case 144:
#line 879 "grammar.y"

		{
			setnumber(yyvsp[0].substr_type,yyvsp[-1].I2_type);
			yyval.substr_type = yyvsp[0].substr_type;
		}
break;
case 145:
#line 885 "grammar.y"

		{
			groupstrings(yyvsp[-2].substr_type,yyvsp[0].substr_type);
			yyval.substr_type = yyvsp[-2].substr_type;
		}
break;
case 146:
#line 891 "grammar.y"

		{
			yyval.I4_type = yyvsp[0].I4_type;
		}
break;
case 147:
#line 895 "grammar.y"

		{
			yyval.I4_type = yyvsp[0].I4_type;
		}
break;
case 148:
#line 900 "grammar.y"

		{
			yyval.I4_type = yyvsp[0].I4_type;
		}
break;
case 149:
#line 904 "grammar.y"

		{
			yyval.I4_type = yyvsp[0].I4_type;
		}
break;
case 150:
#line 909 "grammar.y"

		{
			yyval.substr_type = endvals(yyvsp[-1].substr_type,yyvsp[-2].I4_type,yyvsp[0].I4_type);
		}
break;
case 151:
#line 913 "grammar.y"

		{
			yyval.substr_type = endvals(yyvsp[-1].substr_type,yyvsp[-2].I4_type,yyvsp[0].I4_type);
		}
break;
case 152:
#line 918 "grammar.y"

		{
#ifdef	xPTR2
			tTfp(39, 1, "attrib %12s.%12s found\n",
			Qt.qt_rangev[yyvsp[-2].rng_type].rngvdesc->d_rangevar, yyvsp[0].string_type);
#endif

			/* remember attribute name */
			Trname = yyvsp[0].string_type;

			/* look up attribute */
			aptr = attlookup(yyvsp[-2].rng_type, Trname);
			yyval.tree_type = par_tree(NULL, NULL, VAR, sizeof(varnode_t), yyvsp[-2].rng_type, aptr);
		}
break;
case 153:
#line 932 "grammar.y"

		{
			yyvsp[-1].tree_type->sym.value.sym_var.varstr = yyvsp[0].substr_type;
			yyval.tree_type = yyvsp[-1].tree_type;
		}
break;
case 154:
#line 938 "grammar.y"

		{
			yyval.rng_type = rnglook(yyvsp[0].string_type, LOOKVAR);
			if (yyval.rng_type < 0)
			{
				/* variable not declared */
				par_error(NOVBLE, WARN, yyvsp[0].string_type, 0, 0);
				YYERROR;
			}
			else
				ctlmod_decl(yyval.rng_type);
		}
break;
case 155:
#line 951 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, NULL, INT_CONST, 2, yyvsp[0].I2_type, 0);
		}
break;
case 156:
#line 955 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, NULL, INT_CONST, 4, yyvsp[0].I4_type, 0);
		}
break;
case 157:
#line 959 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, NULL, FLOAT_CONST, 4, yyvsp[0].F4_type, 0);
		}
break;
case 158:
#line 963 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, NULL, FLOAT_CONST, 8, yyvsp[0].F8_type, 0);
		}
break;
case 159:
#line 967 "grammar.y"

		{
			patmat(yyvsp[0].string_type);
			yyval.tree_type = par_tree(NULL, NULL, CHAR_CONST, strlen(yyvsp[0].string_type), yyvsp[0].string_type, 0);
		}
break;
case 160:
#line 972 "grammar.y"

		{
			yyval.tree_type = par_tree(NULL, NULL, COP, 2, yyvsp[0].string_type, 0);
		}
break;
case 162:
#line 978 "grammar.y"

		{
			if (yyvsp[0].type_type == opADD)
				yyval.type_type = opPLUS;
			else
				if (yyvsp[0].type_type == opSUB)
					yyval.type_type = opMINUS;
		}
break;
case 163:
#line 987 "grammar.y"

		{
#ifdef	xPTR2
			tTfp(39, 3, "copy %12s,%12s\n", yyvsp[-5].string_type, yyvsp[0].string_type);
#endif

			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 164:
#line 996 "grammar.y"

		{
			Opflag = mdCOPY;
		}
break;
case 170:
#line 1008 "grammar.y"

		{
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 171:
#line 1013 "grammar.y"

		{
			if (!Err_current)
			{
				setp(PV_STR, yyvsp[0].string_type, 0);
				if (Opflag == mdDESTROY || Opflag == mdCREATE
#ifdef	DISTRIB
					|| Opflag == mdDCREATE
#endif
								)
					rngdel(yyvsp[0].string_type);
			}
		}
break;
case 174:
#line 1030 "grammar.y"

		{
			setp(PV_STR, "\0", 0);
			setp(PV_STR, "i", 0);
		}
break;
case 175:
#line 1035 "grammar.y"

		{
			setp(PV_STR, "\0", 0);
			setp(PV_STR, "f", 0);
		}
break;
case 177:
#line 1043 "grammar.y"

		{
			Opflag = mdCREATE;

			/* set up parameters for regular create */
			setp(PV_STR, "0", 0);		/* r_status = nil */
		}
break;
case 178:
#line 1050 "grammar.y"

/*DDD*/		{
/*DDD*/			Opflag = mdDCREATE;
/*DDD*/
/*DDD*/			/* setup parameters for distributed create */
/*DDD*/			setp(PV_STR, "U", 0);
/*DDD*/			setp(PV_STR, "", 0);
/*DDD*/			setp(PV_STR, "01000", 0);	/* r_status = S_DISTRIBUTED */
/*DDD*/		}
break;
case 181:
#line 1062 "grammar.y"

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
				if (( i = destroy_delim(&Reldesc, yyvsp[0].string_type) < 0) != 0)
				{
					closer(&Reldesc);
					par_error(DELEXIST, WARN, "rdelim",0, 0);
				}
			}
			closer(&Reldesc);
		}
break;
case 182:
#line 1084 "grammar.y"

		{
			Opflag = mdDESTROY;
		}
break;
case 183:
#line 1089 "grammar.y"

		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			setp(PV_STR, "6", 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 184:
#line 1098 "grammar.y"

		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			setp(PV_STR, "5", 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 185:
#line 1108 "grammar.y"

		{
			setp(PV_STR, iocv(*(yyvsp[0].I2_type)), 0);
		}
break;
case 186:
#line 1112 "grammar.y"

		{
			setp(PV_STR, iocv(*(yyvsp[0].I2_type)), 0);
		}
break;
case 189:
#line 1119 "grammar.y"

		{
			int	i;

			i = HELP_RELLIST;
			setp(PV_INT, &i, sizeof(i));	/* all relns */
		}
break;
case 191:
#line 1127 "grammar.y"

		{
			int	i;

			i = HELP_ALLDELLIST;	/* all delims */
			setp(PV_INT, &i, sizeof(i));	/* all delims */
		}
break;
case 193:
#line 1136 "grammar.y"

		{
			Opflag = mdHELP;
		}
break;
case 194:
#line 1141 "grammar.y"

		{
			Opflag = mdHELP;
		}
break;
case 195:
#line 1146 "grammar.y"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("4", hqmbuf);
		}
break;
case 196:
#line 1154 "grammar.y"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("5", hqmbuf);
		}
break;
case 197:
#line 1162 "grammar.y"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0, 0, 0);
			smove("6", hqmbuf);
		}
break;
case 200:
#line 1174 "grammar.y"

		{
			int	i;

			i = HELP_ALLRELINFO;
			setp(PV_INT, &i, sizeof(i));
		}
break;
case 203:
#line 1185 "grammar.y"

		{
			/* relation */
			int	i;

			i = HELP_DELLIST;
			setp(PV_INT, &i, sizeof(i));
			i = yyvsp[0].string_type;
			setp(PV_INT, &i, sizeof(i));
		}
break;
case 204:
#line 1196 "grammar.y"

		{
			int	i;

			/* relation */
			i = HELP_RELINFO;
			setp(PV_INT, &i, sizeof(i));
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 205:
#line 1205 "grammar.y"

		{
			int	i;

			/* manual page */
			i = HELP_MANSEC;
			setp(PV_INT, &i, sizeof(i));
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 206:
#line 1215 "grammar.y"

		{
			setp(PV_STR, hqmbuf, 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 207:
#line 1220 "grammar.y"

		{
			setp(PV_STR, hqmbuf, 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 208:
#line 1226 "grammar.y"

		{
			if (Rsdmno > MAX_2ND_KEYS)
				/* too many attributes in key */
				par_error(INDEXTRA, WARN, 0, 0, 0);
		}
break;
case 209:
#line 1233 "grammar.y"

		{
			/* init INDEX command */
			Rsdmno = 0;
			setp(PV_STR, yyvsp[-2].string_type, 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
			Indexname = yyvsp[0].string_type;
		}
break;
case 210:
#line 1242 "grammar.y"

		{
			Opflag = mdINDEX;
		}
break;
case 212:
#line 1249 "grammar.y"

		{
			Opflag = mdMODIFY;
			Rsdmno = 0;
		}
break;
case 213:
#line 1255 "grammar.y"

		{
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 216:
#line 1262 "grammar.y"

		{
			setp(PV_STR, "name", 0);
		}
break;
case 219:
#line 1270 "grammar.y"

		{
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 220:
#line 1274 "grammar.y"

		{
			concat(yyvsp[-2].string_type, ztack(":", yyvsp[0].string_type), modbuf);
			setp(PV_STR, modbuf, 0);
		}
break;
case 223:
#line 1283 "grammar.y"

		{
			setp(PV_STR, "\0", 0);
		}
break;
case 226:
#line 1291 "grammar.y"

		{
			setp(PV_STR, yyvsp[-2].string_type, 0);
			setp(PV_STR, iocv(*(yyvsp[0].I2_type)), 0);
		}
break;
case 227:
#line 1296 "grammar.y"

		{
			setp(PV_STR, yyvsp[-2].string_type, 0);
			setp(PV_STR, yyvsp[0].string_type, 0);
		}
break;
case 228:
#line 1302 "grammar.y"

		{
			Rsdmno++;
		}
break;
case 229:
#line 1306 "grammar.y"

		{
			Rsdmno++;
		}
break;
case 231:
#line 1313 "grammar.y"

		{
			Opflag = mdPRINT;
		}
break;
case 234:
#line 1321 "grammar.y"

		{
			Opflag = mdSAVE;
		}
break;
case 238:
#line 1331 "grammar.y"

		{
			i = (int) iocv(*(yyvsp[0].I2_type));

#ifdef	xPTR3
			tTfp(39, 4, "day_year: %s\n", i);
#endif

			setp(PV_STR, iocv(*(yyvsp[0].I2_type)), 0);
		}
break;
#line 2420 "y.tab.c"
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

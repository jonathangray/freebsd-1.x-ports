

char ups_ci_parse_y_sccsid[] = "@(#)ci_parse.y	1.12 12/9/92 (UKC)";

#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_types.h"
#include "ci_util.h"
#include "ci_decl.h"
#include "ci_func.h"
#include "ci_expr.h"
#include "ci_stm.h"
#include "ci_lex.h"

int yyparse PROTO((void));

static int Switch_level = 0;
static int Loop_level = 0;

#define YYMAXDEPTH	400


typedef union  {
	int undef;
	constant_t *constant;
	statement_t *statement;
	expr_list_t *expr_list;
	expr_t *expr;
	type_t *type;
	class_t class;
	typecode_t typecode;
	qualifiers_t qualifier;
	func_t *function;
	declaration_t *declaration;
	declarator_t *declarator;
	var_t *varlist;
	enum_member_t *enum_member;
	identifier_t *identifier;
	identifier_list_t *identifier_list;
	optype_t optype;
	initexpr_t *initialiser;
	lexinfo_t *lexinfo;
} YYSTYPE;
# define FOR 257
# define BREAK 258
# define CONTINUE 259
# define RETURN 260
# define IF 261
# define ELSE 262
# define WHILE 263
# define DO 264
# define SWITCH 265
# define CASE 266
# define DEFAULT 267
# define GOTO 268
# define SIZEOF 269
# define AUTO 270
# define REGISTER 271
# define STATIC 272
# define EXTERN 273
# define TYPEDEF 274
# define VOID 275
# define CHAR 276
# define SHORT 277
# define INT 278
# define LONG 279
# define FLOAT 280
# define DOUBLE 281
# define SIGNED 282
# define UNSIGNED 283
# define CONST 284
# define VOLATILE 285
# define STRUCT 286
# define UNION 287
# define ENUM 288
# define AND 289
# define TILDE 290
# define NOT 291
# define LESSTHAN 292
# define GREATERTHAN 293
# define XOR 294
# define OR 295
# define PLUS 296
# define MINUS 297
# define SLASH 298
# define PERCENT 299
# define STAR 300
# define DOT 301
# define COLON 302
# define QUERY 303
# define SEMI 304
# define COMMA 305
# define LPAREN 306
# define RPAREN 307
# define LBRACE 308
# define RBRACE 309
# define LBRAC 310
# define RBRAC 311
# define EQUALS 312
# define STAR_EQUALS 313
# define SLASH_EQUALS 314
# define PERCENT_EQUALS 315
# define PLUS_EQUALS 316
# define MINUS_EQUALS 317
# define LSHIFT_EQUALS 318
# define RSHIFT_EQUALS 319
# define AND_EQUALS 320
# define XOR_EQUALS 321
# define OR_EQUALS 322
# define ANDAND 323
# define OROR 324
# define EQEQ 325
# define NOTEQ 326
# define GTEQ 327
# define LESSEQ 328
# define LSHIFT 329
# define RSHIFT 330
# define PLUSPLUS 331
# define MINUSMINUS 332
# define ARROW 333
# define ELLIPSIS 334
# define INTEGER_CONSTANT 335
# define CHARACTER_CONSTANT 336
# define FLOATING_CONSTANT 337
# define STRING_CONSTANT 338
# define IDENTIFIER 339
# define TYPEDEF_NAME 340
# define BADTOK 341
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256



/*  Resolve the C typedef ambiguity - return IDENTIFIER or TYPEDEF
 *  as the type of the string name.
 */
token_t
name_type(name)
const char *name;
{
	return (ci_lookup_typedef(name) != NULL) ? TYPEDEF_NAME : IDENTIFIER;
}

parse_id_t
ci_parse_file(parse_id, filename, block, flags, report_error_func,
				resolve_name_func, getline_func, getline_arg)
parse_id_t parse_id;
const char *filename;
block_t *block;
unsigned long flags;
ci_report_error_func_t report_error_func;
ci_resolve_name_func_t resolve_name_func;
const char *(*getline_func)PROTO((char *arg));
char *getline_arg;
{
	lex_env_t lebuf;
	int res;

	lebuf.le_report_func = report_error_func;
	lebuf.le_getline = getline_func;
	lebuf.le_getline_arg = getline_arg;
	lebuf.le_filename = filename;
	lebuf.le_had_error = FALSE;
	lebuf.le_lnum = -1;
	lebuf.le_lptr = "";
	lebuf.le_abort_parse = FALSE;

	Lex_env = &lebuf;
	ci_set_diag_handler(ci_lex_error, (char *)&lebuf);
	ci_start_parse_tree((parse_res_t *)parse_id, resolve_name_func,
								block, flags);
	Loop_level = Switch_level = 0;

	if ((flags & CI_CP_DONT_PANIC) == 0)
		res = yyparse();
	else {
		extern bool ci_Catching_panics;
		extern jmp_buf ci_Catch_panic_env;

		if (ci_Catching_panics)
			panic("catch_panics botch");
		ci_Catching_panics = TRUE;

		if (setjmp(ci_Catch_panic_env) != 0)
			res = -1;
		else
			res = yyparse();
		
		ci_Catching_panics = FALSE;
	}

	ci_set_diag_handler((diag_handler_func_t)NULL, (char *)NULL);
	Lex_env = NULL;

	/*  We call yylex with Lex_env NULL, to make it put out a newline
	 *  when debugging.
	 */
	(void) yylex();

	return (parse_id_t)ci_end_parse_tree(res == 0 && !lebuf.le_had_error);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 44,
	304, 49,
	305, 49,
	-2, 10,
	};
# define YYNPROD 229
# define YYLAST 1084
int yyact[]={

   211,    86,   228,    60,   253,    58,    33,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    30,    31,    37,    38,
    36,    81,    84,    85,   355,   309,   194,   123,    82,    83,
   195,   279,    80,   254,    61,   252,    59,   165,    93,    32,
   128,   242,   237,   236,   235,   229,   207,   240,   241,   239,
   226,   227,   234,    86,   179,   180,   149,    68,   238,    32,
    34,   148,   201,    78,    79,   147,    33,    95,    96,    97,
    92,    90,    29,    81,    84,    85,    74,   175,   176,    34,
    82,    83,   173,   174,    80,   194,   151,   152,   150,   195,
    93,   347,    64,   298,   242,   237,   236,   235,   229,    32,
   240,   241,   239,   226,   227,   234,    86,   329,   259,   132,
    64,   145,   178,   177,   346,    78,    79,   124,    32,    95,
    96,    97,    92,   225,    34,    34,    81,    84,    85,    45,
    33,    33,   146,    82,    83,   259,   120,    80,   284,   353,
   263,   316,   283,    93,   195,    64,   209,   242,   237,   236,
   235,   229,   168,   240,   241,   239,   226,   227,   234,    86,
    34,    52,   308,    32,    32,    51,   263,   306,    78,    79,
   195,   244,    95,    96,    97,    92,   225,   367,   314,    81,
    84,    85,   313,   348,   206,   206,    82,    83,   297,   205,
    80,   259,   259,   362,   352,   328,    93,   259,    64,   351,
   259,   259,   350,   260,   246,   188,   248,   187,   327,   320,
   317,   299,   258,   168,   186,   112,   315,   305,   304,   259,
   318,    78,    79,   168,   259,    95,    96,    97,    92,   225,
   290,   291,   189,   307,    69,    70,   365,   363,   310,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    30,    31,    37,    38,    36,   134,   135,
   136,   137,   138,   139,   140,   141,   142,   143,   144,    34,
   354,   341,   340,   333,   312,   194,   286,   311,   303,   195,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    30,    31,    37,    38,    36,   302,
   300,   332,   292,   334,   184,   185,   183,   166,    32,    29,
    34,   339,   319,    30,    31,   171,   263,   286,   181,   182,
   195,   172,   193,    87,   338,   361,   110,   103,   115,    34,
    41,   102,    47,   232,   349,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    30,    31,    37,    38,    36,   122,
    29,   356,   357,   358,    11,   343,    72,   360,   199,   231,
   116,   121,   364,   335,   336,   337,   366,    53,   368,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    30,    31,    37,    38,    36,    98,    89,
    94,   287,   344,    99,   126,   230,   100,   101,   125,    34,
    29,     7,   167,   131,     2,    33,    39,   215,    44,    63,
    65,    40,   359,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    23,    24,    25,    26,    30,    31,    37,
    38,    36,   285,   133,   243,    54,     8,    27,    32,    29,
    28,    35,   198,    46,   106,    67,   108,     5,    62,   204,
   105,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    30,    31,    37,    38,    36,
   247,   233,    67,   127,   250,   249,   197,   107,     4,    91,
    88,    77,   109,    29,    42,    76,   208,   220,   325,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    30,    31,    37,    38,    36,   219,   274,
   275,   272,   273,   190,   218,   282,   281,   216,   213,   200,
    66,    29,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    30,    31,    37,    38,
    36,    86,    42,   217,   191,   221,   212,    53,   222,   224,
   223,   214,   289,   280,   210,   256,   295,   257,   293,    29,
   264,    81,    84,    85,   296,   197,   265,   255,    82,    83,
   266,   267,    80,   268,   269,   270,   271,    55,    93,     3,
   130,   342,     1,   301,     0,   282,     0,     0,     0,     0,
     0,     0,    29,   117,     0,     0,    54,     0,     0,     0,
     0,     0,   261,    78,    79,    66,     0,    95,    96,    97,
    92,    90,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    30,    31,    37,    38,    36,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    30,    31,    37,    38,    36,
   324,    86,     0,     0,     0,     0,   294,     0,     0,   331,
   330,     0,     0,    86,     0,     0,     0,     0,     0,     0,
   196,    81,    84,    85,     0,     0,     0,     0,    82,    83,
     0,     0,    80,    81,    84,    85,   326,    29,    93,     0,
    82,    83,     0,   323,    80,     0,     0,   169,     0,     0,
    93,    29,     0,   200,     0,   288,    12,     0,     0,     0,
     0,     0,     0,    78,    79,    86,     0,    95,    96,    97,
    92,    90,   202,   203,     0,    78,    79,     0,    86,    95,
    96,    97,    92,    90,     0,    81,    84,    85,     0,     0,
     0,    56,    82,    83,     0,     0,    80,     0,    81,    84,
    85,    75,    93,   170,   130,    82,    83,     0,     0,    80,
     0,     0,   113,     0,     0,    93,     0,     0,   169,     0,
    71,     0,     0,     0,     0,     0,     0,    78,    79,    86,
     0,    95,    96,    97,    92,    90,     0,     0,     0,     0,
    78,    79,    86,     0,    95,    96,    97,    92,    90,    81,
    84,    85,     0,     0,     0,    73,    82,    83,     0,     0,
    80,   104,    81,    84,    85,     0,    93,     0,   192,    82,
    83,     0,   129,    80,     0,     0,     9,     0,     0,   164,
   153,   155,   157,   157,   157,   157,   157,   157,   163,     0,
     0,    78,    79,    86,    10,    95,    96,    97,    92,    90,
     0,     0,     0,     0,    78,    79,     0,     0,    95,    96,
    97,    92,    90,    81,    84,    85,     0,     0,     0,     0,
    82,    83,     0,     0,    80,     0,   262,     0,     0,    57,
   154,     0,     0,     0,   129,   118,     0,   245,     0,     0,
     0,     0,   156,   158,   159,   160,   161,   162,   157,     0,
   114,   192,   251,   119,     0,    78,    79,     0,     0,    95,
    96,    97,    92,    90,     0,     0,     0,   157,   157,     0,
   118,     0,     0,   157,   157,   157,   157,   157,   157,   157,
   157,   157,   157,   157,   157,   157,   157,   157,   119,     0,
     0,     0,   118,     0,     0,   118,   118,   118,     0,    73,
     0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
   119,    73,     0,   119,   119,   119,   111,     6,     6,     0,
   262,     0,    43,     0,     0,    48,    49,    50,     0,     0,
    73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   118,     0,     0,     0,   276,   277,   278,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    43,   119,
   157,     0,     0,   322,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    43,   118,     0,     0,     0,     0,     0,    73,     0,     0,
     0,     0,     0,     0,     0,     0,    73,     0,     0,   119,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   157,   129,     0,
   321,     0,   345,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    43 };
int yypact[]={

    99,    99, -1000, -1000, -1000,   252,  -175, -1000,   252,   252,
   252,  -145,  -300, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  -176,    29,  -303,  -305, -1000, -1000, -1000,
  -198,   252, -1000,  -175,  -255, -1000,   -70, -1000, -1000, -1000,
 -1000,   449,   143,  -145,   -92,    29, -1000, -1000,    60,  -172,
  -312,  -191, -1000,   252, -1000,  -198, -1000,  -255,   436, -1000,
  -176, -1000,  -202, -1000, -1000,   -54,  -192,  -245,   564,   564,
   500,   500,   500,   500,   500,   500,   513,  -286, -1000,    12,
 -1000, -1000, -1000,  -268,    21, -1000, -1000, -1000,    32,  -243,
  -215,  -275,    22,     6, -1000, -1000,   -93,  -100,   -73, -1000,
 -1000,  -221, -1000, -1000, -1000,   351, -1000,  -240,    60,    60,
    60,  -120, -1000,  -266,  -312,  -163,   252, -1000, -1000, -1000,
   436, -1000, -1000,   500, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000,   500,   500,   500,   500,  -304,
  -306, -1000, -1000, -1000,   500, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -268,   500,   500,   -95,  -104,  -140,
 -1000,   500,   500,   500,   500,   500,   500,   500,   500,   500,
   500,   500,   500,   500,   500,   500, -1000, -1000,  -308,   219,
 -1000, -1000,  -280,  -168,   -31,   384, -1000, -1000,   -74, -1000,
     0,   500, -1000, -1000,   337, -1000,  -312,   500,  -121, -1000,
  -216, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000,    -2,   500,    -3,   -26,   -88,
   -89,  -139,  -110,  -144,  -314,   500,   -27,   -30,   -81, -1000,
 -1000, -1000, -1000,  -127, -1000, -1000,   -86,  -286,  -170,   -97,
   -85, -1000, -1000,   500, -1000,   -98,    12,    21,   500,   500,
 -1000, -1000,  -166,    10,    32,  -243,  -215,  -215,  -275,  -275,
  -275,  -275,    22,    22,     6,     6, -1000, -1000, -1000, -1000,
 -1000, -1000,  -168,   372,   181,   -99, -1000,  -112, -1000,  -204,
 -1000,  -240,   500, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -110,   -29,  -110, -1000,   500,   500,   500,    61,   500,   -32,
   -33, -1000, -1000, -1000,   272,   500, -1000, -1000,   500,  -197,
 -1000, -1000, -1000, -1000,  -220, -1000,  -124, -1000, -1000, -1000,
 -1000, -1000, -1000,  -110, -1000,  -105,  -108,  -113,  -167,   -34,
 -1000, -1000, -1000, -1000, -1000, -1000,  -315, -1000, -1000, -1000,
  -110,  -110,  -110,   500,   500, -1000,    63, -1000, -1000,  -114,
   -67,  -110,   -68,   500, -1000, -1000,  -130,  -110, -1000 };
int yypgo[]={

     0,   582,   404,   579,   834,   577,     0,   554,   407,   551,
   550,   549,   548,   546,   545,   543,   518,   517,   514,   508,
   487,    58,     2,   743,    76,   356,   485,   323,   389,   390,
   388,   393,   396,   397,   331,   327,   801,   741,   481,   480,
   479,   477,   475,   474,   471,   447,   956,   593,   360,   391,
   446,   326,   328,   332,   443,   358,   442,   401,   432,   441,
   349,   361,   354,   322,   696,   402,   816,   440,   437,   436,
    40,   434,   433,   478,   411,   330,   410,   409,   398,   395,
   359,   333 };
int yyr1[]={

     0,     1,     1,     2,     2,    74,     3,    76,     3,    45,
    45,    73,    73,    75,    75,    46,    46,    46,    46,    46,
    46,    69,    69,    69,    69,    69,    66,    66,    66,    66,
    66,    66,    66,    66,    66,    66,    66,    66,     4,     4,
    68,    68,    68,    59,    59,    52,    52,    54,    54,    53,
    53,    48,    47,    47,    47,    47,    56,    56,    55,    55,
    55,    67,    67,    67,    61,    61,    60,    60,    57,    57,
    62,    62,    62,    62,    62,    62,    62,    64,    64,    64,
    64,     5,     5,    49,    49,    50,    50,    51,    51,    51,
    41,    41,    70,    70,    70,    71,    71,    65,    65,    58,
    58,    58,    63,    63,    63,    63,    63,    63,    63,    63,
    63,     6,     6,     6,     6,     6,     6,     6,     6,     6,
     6,     6,     6,     6,    13,    16,    16,     9,     8,     8,
    78,    78,    77,     7,     7,    17,    17,    79,    15,    80,
    18,    81,    19,    44,    20,    22,    22,    14,    10,    11,
    12,    21,    21,    23,    23,    72,    72,    72,    72,    72,
    72,    72,    72,    72,    72,    72,    24,    24,    25,    26,
    26,    27,    27,    28,    28,    29,    29,    30,    30,    31,
    31,    31,    32,    32,    32,    32,    32,    33,    33,    33,
    34,    34,    34,    35,    35,    35,    35,    36,    36,    37,
    37,    37,    37,    37,    37,    37,    37,    37,    37,    37,
    38,    38,    38,    38,    38,    38,    38,    38,    39,    39,
    39,    39,    42,    42,    43,    43,    40,    40,    40 };
int yyr2[]={

     0,     3,     5,     3,     3,     1,     7,     1,     9,     3,
     5,     5,     7,     2,     4,     3,     5,     3,     5,     3,
     5,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     9,    11,     5,     3,     3,     3,     5,     3,     7,     3,
     7,     7,     3,     5,     3,     5,     3,     7,     3,     5,
     7,     9,    11,     5,     3,     7,     3,     7,     3,     5,
     3,     7,     7,     9,     7,     9,     9,     3,     5,     5,
     7,     3,     5,     3,     7,     3,     7,     5,     3,     5,
     3,     7,     3,     7,     9,     3,     7,     3,     5,     3,
     3,     5,     7,     5,     7,     7,     9,     5,     7,     7,
     9,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     7,     9,     7,     5,     7,     9,
     0,     2,     3,     3,     5,    11,    15,     3,    11,     3,
    11,     3,    15,     3,    19,     1,     3,     7,     5,     5,
     7,     3,     7,     2,     7,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     2,    11,     3,     2,
     7,     2,     7,     2,     7,     2,     7,     2,     7,     2,
     7,     7,     2,     7,     7,     7,     7,     2,     7,     7,
     2,     7,     7,     2,     7,     7,     7,     2,     9,     3,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     9,
     2,     9,     9,     7,    13,     7,     5,     5,     3,     3,
     3,     7,     1,     3,     3,     7,     3,     3,     3 };
int yychk[]={

 -1000,    -1,    -2,    -3,   -73,   -45,   -46,   -57,   -69,   -66,
    -4,   -62,   -64,   270,   271,   272,   273,   274,   275,   276,
   277,   278,   279,   280,   281,   282,   283,   -68,   -67,   340,
   284,   285,   339,   306,   300,   -59,   288,   286,   287,    -2,
   -74,   -75,   -73,   -46,   -57,   304,   -54,   -53,   -46,   -46,
   -46,   310,   306,   -62,   -57,    -5,   -64,    -4,   308,   339,
   308,   339,    -8,   -77,   308,   -76,   -73,   -57,   312,   304,
   305,   311,   -25,   -23,   -24,   -37,   -26,   -38,   331,   332,
   300,   289,   296,   297,   290,   291,   269,   -27,   -39,   -28,
   339,   -40,   338,   306,   -29,   335,   336,   337,   -30,   -31,
   -32,   -33,   -34,   -35,   -36,   307,   -49,   -41,   -50,   339,
   -51,   -46,   307,   -64,    -4,   -52,   -48,   -47,   -66,    -4,
   308,   -61,   -60,   339,   308,   -78,   -75,    -8,   -70,   -23,
   308,   -53,   311,   -72,   312,   313,   314,   315,   316,   317,
   318,   319,   320,   321,   322,   303,   324,   310,   306,   301,
   333,   331,   332,   -37,   306,   -37,   -36,   -37,   -36,   -36,
   -36,   -36,   -36,   -37,   306,   323,   295,   -65,   -21,   -47,
   -23,   294,   289,   325,   326,   292,   293,   328,   327,   329,
   330,   296,   297,   300,   298,   299,   307,   307,   305,   305,
   -57,   -58,   -64,   -63,   306,   310,   309,   -48,   -56,   -55,
   -57,   302,   -47,   -47,   -52,   309,   305,   312,   -61,   309,
    -7,    -6,   -13,   -16,    -9,    -8,   -17,   -15,   -18,   -19,
   -20,   -14,   -12,   -10,   -11,   339,   266,   267,   -22,   261,
   -79,   -80,   -81,   -44,   268,   260,   259,   258,   -21,   265,
   263,   264,   257,   -71,   -70,   -23,   -21,   -27,   -21,   -42,
   -43,   -23,   339,   310,   339,   -65,   -28,   -29,   307,   305,
   307,   -58,   -64,   306,   -30,   -31,   -32,   -32,   -33,   -33,
   -33,   -33,   -34,   -34,   -35,   -35,   -36,   -36,   -36,   339,
   334,   -51,   -63,   310,   306,   -58,   307,   -49,   311,   -25,
   304,   305,   302,   -25,   309,   -60,   -25,   309,   309,    -6,
   302,   -25,   302,   304,   306,   306,   306,    -6,   306,   339,
   -22,   304,   304,   309,   305,   302,   311,   307,   305,   -21,
   307,   -36,   -23,   311,   -25,   307,   -49,   307,   307,   311,
   -55,   -25,    -6,   302,    -6,   -21,   -21,   -21,   263,   -22,
   304,   304,   309,   -70,   -24,   -23,   311,   311,   307,    -6,
   307,   307,   307,   306,   304,   339,    -6,    -6,    -6,   -21,
   -22,   262,   307,   304,    -6,   304,   -22,   307,    -6 };
int yydef[]={

     0,    -2,     1,     3,     4,     5,     0,     9,    15,    17,
    19,    68,     0,    21,    22,    23,    24,    25,    26,    27,
    28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
    38,    39,    70,     0,    77,     0,     0,    43,    44,     2,
     0,     7,    13,     0,    -2,    11,     0,    47,    16,    18,
    20,     0,     0,    69,     0,    78,    79,    81,     0,    42,
     0,    63,     6,   130,   132,     0,    14,    49,     0,    12,
     0,    72,     0,   168,   153,   197,   166,   199,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   169,   210,   171,
   218,   219,   220,     0,   173,   226,   227,   228,   175,   177,
   179,   182,   187,   190,   193,    74,     0,     0,    83,    90,
    85,    88,    71,    80,    82,     0,    45,     0,    52,    54,
     0,     0,    64,    66,     0,   145,   131,     8,    50,    92,
     0,    48,    73,     0,   155,   156,   157,   158,   159,   160,
   161,   162,   163,   164,   165,     0,     0,     0,   222,     0,
     0,   216,   217,   200,     0,   201,   202,   197,   203,   204,
   205,   206,   207,   208,     0,     0,     0,     0,     0,    97,
   151,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    75,    76,     0,     0,
    87,    89,    99,   100,     0,     0,    40,    46,     0,    56,
    58,     0,    53,    55,     0,    61,     0,     0,     0,   128,
   145,   133,   111,   112,   113,   114,   115,   116,   117,   118,
   119,   120,   121,   122,   123,   218,     0,     0,     0,     0,
     0,     0,   145,     0,     0,   145,     0,     0,   146,   137,
   139,   141,   143,     0,    95,   154,     0,   170,     0,     0,
   223,   224,   213,     0,   215,     0,   172,   174,     0,     0,
   221,    98,    99,     0,   176,   178,   180,   181,   183,   184,
   185,   186,   188,   189,   191,   192,   194,   195,   196,    91,
    84,    86,   101,     0,     0,     0,   107,     0,   103,     0,
    51,     0,     0,    59,    41,    65,    67,    62,   129,   134,
   145,     0,   145,   127,     0,     0,     0,     0,   145,     0,
     0,   148,   149,    93,     0,     0,   211,   212,     0,     0,
   209,   198,   152,   105,     0,   109,     0,   102,   108,   104,
    57,    60,   124,   145,   126,     0,     0,     0,     0,     0,
   147,   150,    94,    96,   167,   225,     0,   106,   110,   125,
   145,   145,   145,     0,   145,   214,   135,   138,   140,     0,
     0,   145,     0,   145,   136,   142,     0,   145,   144 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"FOR",	257,
	"BREAK",	258,
	"CONTINUE",	259,
	"RETURN",	260,
	"IF",	261,
	"ELSE",	262,
	"WHILE",	263,
	"DO",	264,
	"SWITCH",	265,
	"CASE",	266,
	"DEFAULT",	267,
	"GOTO",	268,
	"SIZEOF",	269,
	"AUTO",	270,
	"REGISTER",	271,
	"STATIC",	272,
	"EXTERN",	273,
	"TYPEDEF",	274,
	"VOID",	275,
	"CHAR",	276,
	"SHORT",	277,
	"INT",	278,
	"LONG",	279,
	"FLOAT",	280,
	"DOUBLE",	281,
	"SIGNED",	282,
	"UNSIGNED",	283,
	"CONST",	284,
	"VOLATILE",	285,
	"STRUCT",	286,
	"UNION",	287,
	"ENUM",	288,
	"AND",	289,
	"TILDE",	290,
	"NOT",	291,
	"LESSTHAN",	292,
	"GREATERTHAN",	293,
	"XOR",	294,
	"OR",	295,
	"PLUS",	296,
	"MINUS",	297,
	"SLASH",	298,
	"PERCENT",	299,
	"STAR",	300,
	"DOT",	301,
	"COLON",	302,
	"QUERY",	303,
	"SEMI",	304,
	"COMMA",	305,
	"LPAREN",	306,
	"RPAREN",	307,
	"LBRACE",	308,
	"RBRACE",	309,
	"LBRAC",	310,
	"RBRAC",	311,
	"EQUALS",	312,
	"STAR_EQUALS",	313,
	"SLASH_EQUALS",	314,
	"PERCENT_EQUALS",	315,
	"PLUS_EQUALS",	316,
	"MINUS_EQUALS",	317,
	"LSHIFT_EQUALS",	318,
	"RSHIFT_EQUALS",	319,
	"AND_EQUALS",	320,
	"XOR_EQUALS",	321,
	"OR_EQUALS",	322,
	"ANDAND",	323,
	"OROR",	324,
	"EQEQ",	325,
	"NOTEQ",	326,
	"GTEQ",	327,
	"LESSEQ",	328,
	"LSHIFT",	329,
	"RSHIFT",	330,
	"PLUSPLUS",	331,
	"MINUSMINUS",	332,
	"ARROW",	333,
	"ELLIPSIS",	334,
	"INTEGER_CONSTANT",	335,
	"CHARACTER_CONSTANT",	336,
	"FLOATING_CONSTANT",	337,
	"STRING_CONSTANT",	338,
	"IDENTIFIER",	339,
	"TYPEDEF_NAME",	340,
	"BADTOK",	341,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"translation_unit : external_declaration",
	"translation_unit : translation_unit external_declaration",
	"external_declaration : function_definition",
	"external_declaration : declaration",
	"function_definition : function_declaration",
	"function_definition : function_declaration compound_statement",
	"function_definition : function_declaration declaration_list",
	"function_definition : function_declaration declaration_list compound_statement",
	"function_declaration : declarator",
	"function_declaration : declaration_specifiers declarator",
	"declaration : declaration_specifiers SEMI",
	"declaration : declaration_specifiers init_declarator_list SEMI",
	"declaration_list : declaration",
	"declaration_list : declaration_list declaration",
	"declaration_specifiers : storage_class_specifier",
	"declaration_specifiers : storage_class_specifier declaration_specifiers",
	"declaration_specifiers : type_specifier",
	"declaration_specifiers : type_specifier declaration_specifiers",
	"declaration_specifiers : type_qualifier",
	"declaration_specifiers : type_qualifier declaration_specifiers",
	"storage_class_specifier : AUTO",
	"storage_class_specifier : REGISTER",
	"storage_class_specifier : STATIC",
	"storage_class_specifier : EXTERN",
	"storage_class_specifier : TYPEDEF",
	"type_specifier : VOID",
	"type_specifier : CHAR",
	"type_specifier : SHORT",
	"type_specifier : INT",
	"type_specifier : LONG",
	"type_specifier : FLOAT",
	"type_specifier : DOUBLE",
	"type_specifier : SIGNED",
	"type_specifier : UNSIGNED",
	"type_specifier : struct_or_union_specifier",
	"type_specifier : enum_specifier",
	"type_specifier : TYPEDEF_NAME",
	"type_qualifier : CONST",
	"type_qualifier : VOLATILE",
	"struct_or_union_specifier : struct_or_union LBRACE struct_declaration_list RBRACE",
	"struct_or_union_specifier : struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE",
	"struct_or_union_specifier : struct_or_union IDENTIFIER",
	"struct_or_union : STRUCT",
	"struct_or_union : UNION",
	"struct_declaration_list : struct_declaration",
	"struct_declaration_list : struct_declaration_list struct_declaration",
	"init_declarator_list : init_declarator",
	"init_declarator_list : init_declarator_list COMMA init_declarator",
	"init_declarator : declarator",
	"init_declarator : declarator EQUALS initialiser",
	"struct_declaration : specifier_qualifier_list struct_declarator_list SEMI",
	"specifier_qualifier_list : type_specifier",
	"specifier_qualifier_list : type_specifier specifier_qualifier_list",
	"specifier_qualifier_list : type_qualifier",
	"specifier_qualifier_list : type_qualifier specifier_qualifier_list",
	"struct_declarator_list : struct_declarator",
	"struct_declarator_list : struct_declarator_list COMMA struct_declarator",
	"struct_declarator : declarator",
	"struct_declarator : COLON constant_expression",
	"struct_declarator : declarator COLON constant_expression",
	"enum_specifier : ENUM LBRACE enumerator_list RBRACE",
	"enum_specifier : ENUM IDENTIFIER LBRACE enumerator_list RBRACE",
	"enum_specifier : ENUM IDENTIFIER",
	"enumerator_list : enumerator",
	"enumerator_list : enumerator_list COMMA enumerator",
	"enumerator : IDENTIFIER",
	"enumerator : IDENTIFIER EQUALS constant_expression",
	"declarator : direct_declarator",
	"declarator : pointer direct_declarator",
	"direct_declarator : IDENTIFIER",
	"direct_declarator : LPAREN declarator RPAREN",
	"direct_declarator : direct_declarator LBRAC RBRAC",
	"direct_declarator : direct_declarator LBRAC constant_expression RBRAC",
	"direct_declarator : direct_declarator LPAREN RPAREN",
	"direct_declarator : direct_declarator LPAREN parameter_type_list RPAREN",
	"direct_declarator : direct_declarator LPAREN identifier_list RPAREN",
	"pointer : STAR",
	"pointer : STAR type_qualifier_list",
	"pointer : STAR pointer",
	"pointer : STAR type_qualifier_list pointer",
	"type_qualifier_list : type_qualifier",
	"type_qualifier_list : type_qualifier_list type_qualifier",
	"parameter_type_list : parameter_list",
	"parameter_type_list : parameter_list COMMA ELLIPSIS",
	"parameter_list : parameter_declaration",
	"parameter_list : parameter_list COMMA parameter_declaration",
	"parameter_declaration : declaration_specifiers declarator",
	"parameter_declaration : declaration_specifiers",
	"parameter_declaration : declaration_specifiers abstract_declarator",
	"identifier_list : IDENTIFIER",
	"identifier_list : identifier_list COMMA IDENTIFIER",
	"initialiser : assignment_expression",
	"initialiser : LBRACE initialiser_list RBRACE",
	"initialiser : LBRACE initialiser_list COMMA RBRACE",
	"initialiser_list : initialiser",
	"initialiser_list : initialiser_list COMMA initialiser",
	"type_name : specifier_qualifier_list",
	"type_name : specifier_qualifier_list abstract_declarator",
	"abstract_declarator : pointer",
	"abstract_declarator : direct_abstract_declarator",
	"abstract_declarator : pointer direct_abstract_declarator",
	"direct_abstract_declarator : LPAREN abstract_declarator RPAREN",
	"direct_abstract_declarator : LBRAC RBRAC",
	"direct_abstract_declarator : LBRAC constant_expression RBRAC",
	"direct_abstract_declarator : direct_abstract_declarator LBRAC RBRAC",
	"direct_abstract_declarator : direct_abstract_declarator LBRAC constant_expression RBRAC",
	"direct_abstract_declarator : LPAREN RPAREN",
	"direct_abstract_declarator : LPAREN parameter_type_list RPAREN",
	"direct_abstract_declarator : direct_abstract_declarator LPAREN RPAREN",
	"direct_abstract_declarator : direct_abstract_declarator LPAREN parameter_type_list RPAREN",
	"statement : labeled_statement",
	"statement : case_labeled_statement",
	"statement : expression_statement",
	"statement : compound_statement",
	"statement : if_statement",
	"statement : switch_statement",
	"statement : while_statement",
	"statement : do_statement",
	"statement : for_statement",
	"statement : goto_statement",
	"statement : return_statement",
	"statement : continue_statement",
	"statement : break_statement",
	"labeled_statement : IDENTIFIER COLON statement",
	"case_labeled_statement : CASE constant_expression COLON statement",
	"case_labeled_statement : DEFAULT COLON statement",
	"expression_statement : opt_expression SEMI",
	"compound_statement : start_block compound_statement_declarations RBRACE",
	"compound_statement : start_block compound_statement_declarations statement_list RBRACE",
	"compound_statement_declarations : /* empty */",
	"compound_statement_declarations : declaration_list",
	"start_block : LBRACE",
	"statement_list : statement",
	"statement_list : statement_list statement",
	"if_statement : IF LPAREN expression RPAREN statement",
	"if_statement : IF LPAREN expression RPAREN statement ELSE statement",
	"switch : SWITCH",
	"switch_statement : switch LPAREN expression RPAREN statement",
	"while : WHILE",
	"while_statement : while LPAREN expression RPAREN statement",
	"do : DO",
	"do_statement : do statement WHILE LPAREN expression RPAREN SEMI",
	"for : FOR",
	"for_statement : for LPAREN opt_expression SEMI opt_expression SEMI opt_expression RPAREN statement",
	"opt_expression : /* empty */",
	"opt_expression : expression",
	"goto_statement : GOTO IDENTIFIER SEMI",
	"continue_statement : CONTINUE SEMI",
	"break_statement : BREAK SEMI",
	"return_statement : RETURN opt_expression SEMI",
	"expression : assignment_expression",
	"expression : expression COMMA assignment_expression",
	"assignment_expression : conditional_expression",
	"assignment_expression : unary_expression assignment_operator assignment_expression",
	"assignment_operator : EQUALS",
	"assignment_operator : STAR_EQUALS",
	"assignment_operator : SLASH_EQUALS",
	"assignment_operator : PERCENT_EQUALS",
	"assignment_operator : PLUS_EQUALS",
	"assignment_operator : MINUS_EQUALS",
	"assignment_operator : LSHIFT_EQUALS",
	"assignment_operator : RSHIFT_EQUALS",
	"assignment_operator : AND_EQUALS",
	"assignment_operator : XOR_EQUALS",
	"assignment_operator : OR_EQUALS",
	"conditional_expression : logical_or_expression",
	"conditional_expression : logical_or_expression QUERY expression COLON conditional_expression",
	"constant_expression : assignment_expression",
	"logical_or_expression : logical_and_expression",
	"logical_or_expression : logical_or_expression OROR logical_and_expression",
	"logical_and_expression : inclusive_or_expression",
	"logical_and_expression : logical_and_expression ANDAND inclusive_or_expression",
	"inclusive_or_expression : exclusive_or_expression",
	"inclusive_or_expression : inclusive_or_expression OR exclusive_or_expression",
	"exclusive_or_expression : and_expression",
	"exclusive_or_expression : exclusive_or_expression XOR and_expression",
	"and_expression : equality_expression",
	"and_expression : and_expression AND equality_expression",
	"equality_expression : relational_expression",
	"equality_expression : equality_expression EQEQ relational_expression",
	"equality_expression : equality_expression NOTEQ relational_expression",
	"relational_expression : shift_expression",
	"relational_expression : relational_expression LESSTHAN shift_expression",
	"relational_expression : relational_expression GREATERTHAN shift_expression",
	"relational_expression : relational_expression LESSEQ shift_expression",
	"relational_expression : relational_expression GTEQ shift_expression",
	"shift_expression : additive_expression",
	"shift_expression : shift_expression LSHIFT additive_expression",
	"shift_expression : shift_expression RSHIFT additive_expression",
	"additive_expression : multiplicative_expression",
	"additive_expression : additive_expression PLUS multiplicative_expression",
	"additive_expression : additive_expression MINUS multiplicative_expression",
	"multiplicative_expression : cast_expression",
	"multiplicative_expression : multiplicative_expression STAR cast_expression",
	"multiplicative_expression : multiplicative_expression SLASH cast_expression",
	"multiplicative_expression : multiplicative_expression PERCENT cast_expression",
	"cast_expression : unary_expression",
	"cast_expression : LPAREN type_name RPAREN cast_expression",
	"unary_expression : postfix_expression",
	"unary_expression : PLUSPLUS unary_expression",
	"unary_expression : MINUSMINUS unary_expression",
	"unary_expression : STAR cast_expression",
	"unary_expression : AND cast_expression",
	"unary_expression : PLUS cast_expression",
	"unary_expression : MINUS cast_expression",
	"unary_expression : TILDE cast_expression",
	"unary_expression : NOT cast_expression",
	"unary_expression : SIZEOF unary_expression",
	"unary_expression : SIZEOF LPAREN type_name RPAREN",
	"postfix_expression : primary_expression",
	"postfix_expression : postfix_expression LBRAC expression RBRAC",
	"postfix_expression : postfix_expression LPAREN opt_argument_expression_list RPAREN",
	"postfix_expression : postfix_expression DOT IDENTIFIER",
	"postfix_expression : postfix_expression ARROW LBRAC expression RBRAC IDENTIFIER",
	"postfix_expression : postfix_expression ARROW IDENTIFIER",
	"postfix_expression : postfix_expression PLUSPLUS",
	"postfix_expression : postfix_expression MINUSMINUS",
	"primary_expression : IDENTIFIER",
	"primary_expression : constant",
	"primary_expression : STRING_CONSTANT",
	"primary_expression : LPAREN expression RPAREN",
	"opt_argument_expression_list : /* empty */",
	"opt_argument_expression_list : argument_expression_list",
	"argument_expression_list : assignment_expression",
	"argument_expression_list : argument_expression_list COMMA assignment_expression",
	"constant : INTEGER_CONSTANT",
	"constant : CHARACTER_CONSTANT",
	"constant : FLOATING_CONSTANT",
};
#endif /* YYDEBUG */
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc((size_t)yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc((size_t)yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				(size_t)(yymaxdepth * sizeof(YYSTYPE)));
			yys = (int*)realloc((char*)yys,
				(size_t)(yymaxdepth * sizeof(int)));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
{
		yyval.undef = 0;
	} break;
case 2:
{
		yyval.undef = 0;
	} break;
case 3:
{
		if (Switch_level != 0 || Loop_level != 0)
			ci_panic("level botch in ed");
		yyval.undef = 0;
	} break;
case 4:
{
		if (Switch_level != 0 || Loop_level != 0)
			ci_panic("level botch in ed");
		yyval.undef = 0;
	} break;
case 5:
{
		ci_check_func_decls(yypvt[-0].function);
	} break;
case 6:
{
		ci_end_func(yypvt[-0].statement);
	} break;
case 7:
{
		ci_check_func_decls(yypvt[-1].function);
	} break;
case 8:
{
		ci_end_func(yypvt[-0].statement);
	} break;
case 9:
{
		declaration_t *dn;

		dn = ci_make_declaration(CL_DECL, ci_code_to_type(TY_INT), 0, yypvt[-0].declarator);
		yyval.function = ci_start_func(dn);
	} break;
case 10:
{
		declaration_t *dn;

		ci_fix_signed_and_unsigned(yypvt[-1].declaration);
		dn = ci_make_declaration(yypvt[-1].declaration->dn_class, yypvt[-1].declaration->dn_basetype,
					 yypvt[-1].declaration->dn_qualifiers, yypvt[-0].declarator);
		yyval.function = ci_start_func(dn);
	} break;
case 11:
{
		ci_fix_signed_and_unsigned(yypvt[-1].declaration);
	} break;
case 12:
{
		ci_fix_signed_and_unsigned(yypvt[-2].declaration);
		yypvt[-2].declaration->dn_declarators = yypvt[-1].declarator;
		ci_handle_declaration(yypvt[-2].declaration);
	} break;
case 15:
{
		yyval.declaration = ci_make_declaration(yypvt[-0].class, ci_code_to_type(TY_INT), 0,
							(declarator_t *)NULL);
	} break;
case 16:
{
		ci_set_storage_class(yypvt[-0].declaration, yypvt[-1].class);
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 17:
{
		yyval.declaration = ci_make_declaration(CL_NOCLASS, yypvt[-0].type, 0, (declarator_t *)NULL);
	} break;
case 18:
{
		ci_add_type_specifier(yypvt[-0].declaration, yypvt[-1].type);
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 19:
{
		yyval.declaration = ci_make_declaration(CL_NOCLASS, (type_t *)NULL, yypvt[-0].qualifier,
							(declarator_t *)NULL);
	} break;
case 20:
{
		yypvt[-0].declaration->dn_qualifiers |= yypvt[-1].qualifier;
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 21:
{ yyval.class = CL_AUTO;		} break;
case 22:
{ yyval.class = CL_NOCLASS;	} break;
case 23:
{ yyval.class = CL_STAT;		} break;
case 24:
{ yyval.class = CL_DECL;		} break;
case 25:
{ yyval.class = CL_TYPEDEF;	} break;
case 26:
{ yyval.type = ci_code_to_type(TY_VOID);	} break;
case 27:
{ yyval.type = ci_code_to_type(TY_CHAR);	} break;
case 28:
{ yyval.type = ci_code_to_type(TY_SHORT);	} break;
case 29:
{ yyval.type = ci_code_to_type(TY_INT);		} break;
case 30:
{ yyval.type = ci_code_to_type(TY_LONG);	} break;
case 31:
{ yyval.type = ci_code_to_type(TY_FLOAT);	} break;
case 32:
{ yyval.type = ci_code_to_type(TY_DOUBLE);	} break;
case 33:
{ yyval.type = ci_code_to_type(TY_SIGNED);	} break;
case 34:
{ yyval.type = ci_code_to_type(TY_UNSIGNED);	} break;
case 35:
{ yyval.type = yypvt[-0].type;				} break;
case 36:
{ yyval.type = yypvt[-0].type;				} break;
case 37:
{ yyval.type = ci_lookup_typedef(yypvt[-0].identifier->id_name);	} break;
case 38:
{ yyval.qualifier = QU_CONST; } break;
case 39:
{ yyval.qualifier = QU_VOLATILE; } break;
case 40:
{
		yyval.type = ci_build_aggr_or_enum_def(yypvt[-3].typecode, (identifier_t *)NULL, AE_COMPLETE,
					      yypvt[-1].declaration, (enum_member_t *)NULL);
	} break;
case 41:
{
		yyval.type = ci_build_aggr_or_enum_def(yypvt[-4].typecode, yypvt[-3].identifier, AE_COMPLETE,
					      yypvt[-1].declaration, (enum_member_t *)NULL);
	} break;
case 42:
{
		typecode_t typecode;

		typecode = (yypvt[-1].typecode == TY_STRUCT) ? TY_U_STRUCT : TY_U_UNION;
		yyval.type = ci_build_aggr_or_enum_def(typecode, yypvt[-0].identifier, AE_INCOMPLETE,
					      (declaration_t *)NULL,
					      (enum_member_t *)NULL);
	} break;
case 43:
{ yyval.typecode = TY_STRUCT;	} break;
case 44:
{ yyval.typecode = TY_UNION;		} break;
case 45:
{
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 46:
{
		yypvt[-0].declaration->dn_next = yypvt[-1].declaration;
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 47:
{
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 48:
{
		yypvt[-0].declarator->dr_next = yypvt[-2].declarator;
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 49:
{
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 50:
{
		yypvt[-2].declarator->dr_initialiser = yypvt[-0].initialiser;
		yyval.declarator = yypvt[-2].declarator;
	} break;
case 51:
{
		ci_fix_signed_and_unsigned(yypvt[-2].declaration);
		yyval.declaration = ci_make_declaration(yypvt[-2].declaration->dn_class, yypvt[-2].declaration->dn_basetype,
					 yypvt[-2].declaration->dn_qualifiers, yypvt[-1].declarator);
	} break;
case 52:
{
		yyval.declaration = ci_make_declaration(CL_NOCLASS, yypvt[-0].type, 0, (declarator_t *)NULL);
	} break;
case 53:
{
		ci_add_type_specifier(yypvt[-0].declaration, yypvt[-1].type);
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 54:
{
		yyval.declaration = ci_make_declaration(CL_NOCLASS, (type_t *)NULL, yypvt[-0].qualifier,
							(declarator_t *)NULL);
	} break;
case 55:
{
		yypvt[-0].declaration->dn_qualifiers |= yypvt[-1].qualifier;
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 56:
{
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 57:
{
		yypvt[-0].declarator->dr_next = yypvt[-2].declarator;
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 58:
{
		yyval.declarator = yypvt[-0].declarator;
	} break;
case 59:
{
		yyval.declarator = ci_make_declarator(ci_make_expr_bitfield_type((type_t *)NULL,
									       yypvt[-0].expr));
	} break;
case 60:
{
		yyval.declarator = ci_make_declarator(ci_make_expr_bitfield_type(yypvt[-2].declarator->dr_type, yypvt[-0].expr));
	} break;
case 61:
{
		yyval.type = ci_build_aggr_or_enum_def(TY_ENUM, (identifier_t *)NULL,
					   AE_COMPLETE,
					   (declaration_t *)NULL, yypvt[-1].enum_member);
	} break;
case 62:
{
		yyval.type = ci_build_aggr_or_enum_def(TY_ENUM, yypvt[-3].identifier,
					   AE_COMPLETE,
					   (declaration_t *)NULL, yypvt[-1].enum_member);
	} break;
case 63:
{
		yyval.type = ci_build_aggr_or_enum_def(TY_U_ENUM, yypvt[-0].identifier,
					   AE_INCOMPLETE,
					   (declaration_t *)NULL,
					   (enum_member_t *)NULL);
	} break;
case 64:
{
		if (yypvt[-0].enum_member->em_expr_id == NULL)
			yypvt[-0].enum_member->em_val = 0;
		yypvt[-0].enum_member->em_next = NULL;
		yyval.enum_member = yypvt[-0].enum_member;
	} break;
case 65:
{
		if (yypvt[-0].enum_member->em_expr_id == NULL)
			yypvt[-0].enum_member->em_val = yypvt[-2].enum_member->em_val + 1;
		yypvt[-0].enum_member->em_next = yypvt[-2].enum_member;
		yyval.enum_member = yypvt[-0].enum_member;
	} break;
case 66:
{
		yyval.enum_member = ci_build_enum_member(yypvt[-0].identifier, (expr_t *)NULL);
	} break;
case 67:
{
		yyval.enum_member = ci_build_enum_member(yypvt[-2].identifier, yypvt[-0].expr);
	} break;
case 68:
{
		yyval.declarator = ci_make_declarator(yypvt[-0].type);
	} break;
case 69:
{
		type_t *ptype;

		ptype = ci_push_types(yypvt[-0].type, yypvt[-1].type);
		yyval.declarator = ci_make_declarator(ptype);
	} break;
case 70:
{
		type_t *type;
		
		type = ci_make_type(Parse_alloc_id, TY_IDENTIFIER);
		type->ty_identifier = yypvt[-0].identifier;
		yyval.type = type;
	} break;
case 71:
{
		yyval.type = yypvt[-1].declarator->dr_type;
	} break;
case 72:
{
		yyval.type = ci_make_array_type(yypvt[-2].type, (expr_t *)NULL);
	} break;
case 73:
{
		yyval.type = ci_make_array_type(yypvt[-3].type, yypvt[-1].expr);
	} break;
case 74:
{
		yyval.type = ci_make_funcret_type(yypvt[-2].type, FDT_IDLIST, (declaration_t *)NULL,
							(identifier_list_t *)NULL);
	} break;
case 75:
{
		yyval.type = ci_make_funcret_type(yypvt[-3].type, FDT_TYPELIST, yypvt[-1].declaration,
							(identifier_list_t *)NULL);
	} break;
case 76:
{
		yyval.type = ci_make_funcret_type(yypvt[-3].type, FDT_IDLIST, (declaration_t *)NULL, yypvt[-1].identifier_list);
	} break;
case 77:
{
		yyval.type = ci_make_pointer((type_t *)NULL, 0);
	} break;
case 78:
{
		yyval.type = ci_make_pointer((type_t *)NULL, yypvt[-0].qualifier);
	} break;
case 79:
{
		yyval.type = ci_make_pointer(yypvt[-0].type, 0);
	} break;
case 80:
{
		yyval.type = ci_make_pointer(yypvt[-0].type, yypvt[-1].qualifier);
	} break;
case 81:
{
		yyval.qualifier = yypvt[-0].qualifier;
	} break;
case 82:
{
		yyval.qualifier = yypvt[-1].qualifier | yypvt[-0].qualifier;
	} break;
case 83:
{
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 84:
{
		declaration_t *dn;

		dn = ci_make_declaration(CL_ARG, ci_code_to_type(TY_ELLIPSIS), 0,
					 ci_make_declarator((type_t *)NULL));
		dn->dn_next = yypvt[-2].declaration;
		yyval.declaration = dn;
	} break;
case 85:
{
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 86:
{
		yypvt[-0].declaration->dn_next = yypvt[-2].declaration;
		yyval.declaration = yypvt[-0].declaration;
	} break;
case 87:
{
		ci_fix_signed_and_unsigned(yypvt[-1].declaration);
		yyval.declaration = ci_make_declaration(yypvt[-1].declaration->dn_class, yypvt[-1].declaration->dn_basetype,
					 yypvt[-1].declaration->dn_qualifiers, yypvt[-0].declarator);
	} break;
case 88:
{
		ci_fix_signed_and_unsigned(yypvt[-0].declaration);
		yyval.declaration = ci_make_declaration(yypvt[-0].declaration->dn_class, yypvt[-0].declaration->dn_basetype,
					 yypvt[-0].declaration->dn_qualifiers,
					 ci_make_declarator((type_t *)NULL));
	} break;
case 89:
{
		ci_fix_signed_and_unsigned(yypvt[-1].declaration);
		yyval.declaration = ci_make_declaration(yypvt[-1].declaration->dn_class, yypvt[-1].declaration->dn_basetype,
					 yypvt[-1].declaration->dn_qualifiers, yypvt[-0].declarator);
	} break;
case 90:
{
		identifier_list_t *idl = NEW(identifier_list_t);

		idl->idl_id = yypvt[-0].identifier;
		idl->idl_next = NULL;
		yyval.identifier_list = idl;
	} break;
case 91:
{
		identifier_list_t *idl = NEW(identifier_list_t);

		idl->idl_id = yypvt[-0].identifier;
		idl->idl_next = yypvt[-2].identifier_list;
		yyval.identifier_list = idl;
	} break;
case 92:
{
		yyval.initialiser = ci_make_initexpr(FALSE, yypvt[-0].expr, (initexpr_t *)NULL);
	} break;
case 93:
{
		yyval.initialiser = ci_make_initexpr(TRUE, (expr_t *)NULL, yypvt[-1].initialiser);
	} break;
case 94:
{
		yyval.initialiser = ci_make_initexpr(TRUE, (expr_t *)NULL, yypvt[-2].initialiser);
	} break;
case 95:
{
		yyval.initialiser = yypvt[-0].initialiser;
	} break;
case 96:
{
		yypvt[-0].initialiser->ie_next = yypvt[-2].initialiser;
		yyval.initialiser = yypvt[-0].initialiser;
	} break;
case 97:
{
		ci_fix_signed_and_unsigned(yypvt[-0].declaration);
		yyval.type = yypvt[-0].declaration->dn_basetype;
	} break;
case 98:
{
		ci_fix_signed_and_unsigned(yypvt[-1].declaration);
		yyval.type = ci_push_types(yypvt[-1].declaration->dn_basetype, yypvt[-0].declarator->dr_type);
	} break;
case 99:
{
		yyval.declarator = ci_make_declarator(yypvt[-0].type);
	} break;
case 100:
{
		yyval.declarator = ci_make_declarator(yypvt[-0].type);
	} break;
case 101:
{
		yypvt[-1].type->ty_base = yypvt[-0].type;
		yyval.declarator = ci_make_declarator(yypvt[-1].type);
	} break;
case 102:
{
		yyval.type = yypvt[-1].declarator->dr_type;
	} break;
case 103:
{
		yyval.type = ci_make_array_type((type_t *)NULL, (expr_t *)NULL);
	} break;
case 104:
{
		yyval.type = ci_make_array_type((type_t *)NULL, yypvt[-1].expr);
	} break;
case 105:
{
		yyval.type = ci_make_array_type(yypvt[-2].type, (expr_t *)NULL);
	} break;
case 106:
{
		yyval.type = ci_make_array_type(yypvt[-3].type, yypvt[-1].expr);
	} break;
case 107:
{
		yyval.type = ci_make_funcret_type((type_t *)NULL, FDT_IDLIST,
						(declaration_t *)NULL,
						(identifier_list_t *)NULL);
	} break;
case 108:
{
		yyval.type = ci_make_funcret_type((type_t *)NULL, FDT_TYPELIST, yypvt[-1].declaration,
						(identifier_list_t *)NULL);
	} break;
case 109:
{
		yyval.type = ci_make_funcret_type(yypvt[-2].type, FDT_IDLIST, (declaration_t *)NULL,
						(identifier_list_t *)NULL);
	} break;
case 110:
{
		yyval.type = ci_make_funcret_type(yypvt[-3].type, FDT_TYPELIST, yypvt[-1].declaration,
						(identifier_list_t *)NULL);
	} break;
case 124:
{
		yyval.statement = ci_make_labeled_statement(yypvt[-2].identifier, yypvt[-0].statement);
	} break;
case 125:
{
		yyval.statement = ci_make_case_labeled_statement(Switch_level != 0, yypvt[-2].expr, yypvt[-0].statement);
	} break;
case 126:
{
		yyval.statement = ci_make_case_labeled_statement(Switch_level != 0,
								(expr_t *)NULL, yypvt[-0].statement);
	} break;
case 127:
{
		yyval.statement = ci_make_expression_statement(yypvt[-1].expr);
	} break;
case 128:
{
		yyval.statement = ci_end_compound_statement((statement_t *)NULL);
	} break;
case 129:
{
		yyval.statement = ci_end_compound_statement(yypvt[-1].statement);
	} break;
case 132:
{
		ci_start_block(TRUE);
	} break;
case 133:
{
		yypvt[-0].statement->st_next = NULL;
		yyval.statement = yypvt[-0].statement;
	} break;
case 134:
{
		yypvt[-0].statement->st_next = yypvt[-1].statement;
		yyval.statement = yypvt[-0].statement;
	} break;
case 135:
{
		yyval.statement = ci_make_if_statement(yypvt[-2].expr, yypvt[-0].statement, (statement_t *)NULL);
	} break;
case 136:
{
		yyval.statement = ci_make_if_statement(yypvt[-4].expr, yypvt[-2].statement, yypvt[-0].statement);
	} break;
case 137:
{
		++Switch_level;
	} break;
case 138:
{
		--Switch_level;
		yyval.statement = ci_make_switch_statement(yypvt[-2].expr, yypvt[-0].statement);
	} break;
case 139:
{
		++Loop_level;
	} break;
case 140:
{
		--Loop_level;
		yyval.statement = ci_make_while_statement(STT_WHILE, yypvt[-2].expr, yypvt[-0].statement);
	} break;
case 141:
{
		++Loop_level;
	} break;
case 142:
{
		--Loop_level;
		yyval.statement = ci_make_while_statement(STT_DO, yypvt[-2].expr, yypvt[-5].statement);
	} break;
case 143:
{
		++Loop_level;
		yyval.lexinfo = yypvt[-0].lexinfo;
	} break;
case 144:
{
		--Loop_level;
		yyval.statement = ci_make_for_statement(yypvt[-6].expr, yypvt[-4].expr, yypvt[-2].expr, yypvt[-0].statement, yypvt[-8].lexinfo);
	} break;
case 145:
{
		yyval.expr = NULL;
	} break;
case 146:
{
		yyval.expr = yypvt[-0].expr;
	} break;
case 147:
{
		yyval.statement = ci_make_goto_statement(yypvt[-1].identifier);
	} break;
case 148:
{
		yyval.statement = ci_make_continue_statement(Loop_level != 0, yypvt[-1].lexinfo);
	} break;
case 149:
{
		yyval.statement = ci_make_break_statement(Loop_level + Switch_level != 0, yypvt[-1].lexinfo);
	} break;
case 150:
{
		yyval.statement = ci_make_return_statement(yypvt[-1].expr, yypvt[-2].lexinfo);
	} break;
case 151:
{
		yyval.expr = yypvt[-0].expr;
	} break;
case 152:
{
		yyval.expr = ci_make_comma_expr(yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 154:
{
		yyval.expr = ci_make_assignment_expr(yypvt[-1].optype, yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 155:
{ yyval.optype = OP_ASSIGN;		} break;
case 156:
{ yyval.optype = OP_MUL_ASSIGN;		} break;
case 157:
{ yyval.optype = OP_DIV_ASSIGN;		} break;
case 158:
{ yyval.optype = OP_MOD_ASSIGN;		} break;
case 159:
{ yyval.optype = OP_PLUS_ASSIGN;		} break;
case 160:
{ yyval.optype = OP_MINUS_ASSIGN;		} break;
case 161:
{ yyval.optype = OP_LSHIFT_ASSIGN;	} break;
case 162:
{ yyval.optype = OP_RSHIFT_ASSIGN;	} break;
case 163:
{ yyval.optype = OP_BITWISE_AND_ASSIGN;	} break;
case 164:
{ yyval.optype = OP_BITWISE_XOR_ASSIGN;	} break;
case 165:
{ yyval.optype = OP_BITWISE_OR_ASSIGN;	} break;
case 167:
{
		yyval.expr = ci_make_conditional_expression(yypvt[-4].expr, yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 168:
{
		yyval.expr = yypvt[-0].expr;
	} break;
case 170:
{
		yyval.expr = ci_make_logical_expr(OP_LOGICAL_OR, "||", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 172:
{
		yyval.expr = ci_make_logical_expr(OP_LOGICAL_AND, "&&", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 174:
{
		yyval.expr = ci_make_bitwise_expr(OP_BITWISE_OR, "|", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 176:
{
		yyval.expr = ci_make_bitwise_expr(OP_BITWISE_XOR, "^", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 178:
{
		yyval.expr = ci_make_bitwise_expr(OP_BITWISE_AND, "&", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 180:
{
		yyval.expr = ci_make_comparison_expr(OP_IS_EQUAL, "==", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 181:
{
		yyval.expr = ci_make_comparison_expr(OP_NOT_EQUAL, "!=", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 183:
{
		yyval.expr = ci_make_comparison_expr(OP_LESS, "<", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 184:
{
		yyval.expr = ci_make_comparison_expr(OP_GREATER, ">", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 185:
{
		yyval.expr = ci_make_comparison_expr(OP_LESS_OR_EQUAL, "<=", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 186:
{
		yyval.expr = ci_make_comparison_expr(OP_GREATER_OR_EQUAL, ">=", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 188:
{
		yyval.expr = ci_make_shift_expr(OP_LSHIFT, "<<", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 189:
{
		yyval.expr = ci_make_shift_expr(OP_RSHIFT, ">>", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 191:
{
		yyval.expr = ci_make_add_or_subtract_expr(OP_PLUS, "+", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 192:
{
		yyval.expr = ci_make_add_or_subtract_expr(OP_MINUS, "-", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 194:
{
		yyval.expr = ci_make_mul_or_div_expr(OP_MUL, "*", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 195:
{
		yyval.expr = ci_make_mul_or_div_expr(OP_DIV, "/", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 196:
{
		yyval.expr = ci_make_mod_expr(OP_MOD, "%", yypvt[-2].expr, yypvt[-0].expr);
	} break;
case 198:
{
		yyval.expr = ci_make_cast_expr(yypvt[-2].type, yypvt[-0].expr);
	} break;
case 199:
{
		yyval.expr = yypvt[-0].expr;
	} break;
case 200:
{
		yyval.expr = ci_make_inc_or_dec_expr(OP_PREINC, yypvt[-0].expr, "prefix ++");
	} break;
case 201:
{
		yyval.expr = ci_make_inc_or_dec_expr(OP_PREDEC, yypvt[-0].expr, "prefix --");
	} break;
case 202:
{
		yyval.expr = ci_make_deref_expr(yypvt[-0].expr);
	} break;
case 203:
{
		yyval.expr = ci_make_address_of_expr(yypvt[-0].expr);
	} break;
case 204:
{
		yyval.expr = ci_make_unary_plus_expr(yypvt[-0].expr);
	} break;
case 205:
{
		yyval.expr = ci_make_unary_minus_expr(yypvt[-0].expr);
	} break;
case 206:
{
		yyval.expr = ci_make_bitwise_not_expr(yypvt[-0].expr);
	} break;
case 207:
{
		yyval.expr = ci_make_logical_not_expr(yypvt[-0].expr);
	} break;
case 208:
{
		yyval.expr = ci_make_sizeof_expr(yypvt[-0].expr, (type_t *)NULL);
	} break;
case 209:
{
		yyval.expr = ci_make_sizeof_expr((expr_t *)NULL, yypvt[-1].type);
	} break;
case 211:
{
		/*  Rewrite as *($1 + $3).
		 */
		yyval.expr = ci_make_deref_expr(ci_make_add_or_subtract_expr(OP_PLUS, "+",
									   yypvt[-3].expr, yypvt[-1].expr));
	} break;
case 212:
{
		yyval.expr = ci_make_func_call_expr(yypvt[-3].expr, yypvt[-1].expr_list);
	} break;
case 213:
{
		yyval.expr = ci_make_dot_expr(yypvt[-2].expr, yypvt[-0].identifier, "dot");
	} break;
case 214:
{
		yyval.expr = ci_make_multi_arrow_expr(yypvt[-5].expr, yypvt[-2].expr, yypvt[-0].identifier);
	} break;
case 215:
{
		/*  Rewrite as (*$1).$3
		 */
		yyval.expr = ci_make_dot_expr(ci_make_deref_expr(yypvt[-2].expr), yypvt[-0].identifier, "arrow");
	} break;
case 216:
{
		yyval.expr = ci_make_inc_or_dec_expr(OP_POSTINC, yypvt[-1].expr, "postfix ++");
	} break;
case 217:
{
		yyval.expr = ci_make_inc_or_dec_expr(OP_POSTDEC, yypvt[-1].expr, "postfix --");
	} break;
case 218:
{
		yyval.expr = ci_make_identifier_expr(yypvt[-0].identifier);
	} break;
case 219:
{
		yyval.expr = yypvt[-0].expr;
	} break;
case 220:
{
		yyval.expr = ci_make_string_constant_expr(yypvt[-0].constant->co_lexinfo,
						  &yypvt[-0].constant->co_string_val);
	} break;
case 221:
{
		yyval.expr = yypvt[-1].expr;
	} break;
case 222:
{
		yyval.expr_list = NULL;
	} break;
case 223:
{
		yyval.expr_list = yypvt[-0].expr_list;
	} break;
case 224:
{
		expr_list_t *el = NEW(expr_list_t);

		el->el_expr = yypvt[-0].expr;
		el->el_next = NULL;
		yyval.expr_list = el;
	} break;
case 225:
{
		expr_list_t *el = NEW(expr_list_t);

		el->el_expr = yypvt[-0].expr;
		el->el_next = yypvt[-2].expr_list;
		yyval.expr_list = el;
	} break;
case 226:
{
		yyval.expr = ci_make_integer_constant_expr(ET_INT_CONST,
						   yypvt[-0].constant->co_lexinfo,
						   yypvt[-0].constant->co_integer_val);
	} break;
case 227:
{
		yyval.expr = ci_make_integer_constant_expr(ET_CHAR_CONST,
						   yypvt[-0].constant->co_lexinfo,
						   yypvt[-0].constant->co_integer_val);
	} break;
case 228:
{
		yyval.expr = ci_make_floating_constant_expr(yypvt[-0].constant->co_lexinfo,
						    yypvt[-0].constant->co_floating_val);
	} break;
	}
	goto yystack;		/* reset registers in driver code */
}

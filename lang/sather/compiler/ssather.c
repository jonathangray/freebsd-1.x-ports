extern char *malloc(), *realloc();

# line 43 "ssather.y"

#include "all.h"

#include <stdio.h>

#if defined(rs6000)
#  undef free
#endif

extern ptr id_exprob_create();
extern ptr char_const_exprob_create();
extern ptr int_const_exprob_create();
extern ptr real_const_exprob_create();
extern ptr bool_const_exprob_create();
extern ptr str_const_exprob_create();

extern ptr op_exprob_create_unary();
extern ptr op_exprob_create_binary();

extern ptr aref_exprob_create();
extern ptr id_args_exprob_create();
extern ptr expr_args_exprob_create();
extern ptr typespec_args_exprob_create();

extern ptr exprob_put_name();

extern ptr lst_exprob_create();
extern ptr lst_exprob_push();
extern ptr lst_exprob_append();
extern ptr lst_stmtob_create();
extern ptr lst_stmtob_push();
extern ptr lst_stmtob_append();
extern ptr lst_when_stmtob_create();
extern ptr lst_when_stmtob_push();
extern ptr lst_declob_create();
extern ptr lst_declob_append();
extern ptr lst_typeob_create();
extern ptr lst_typeob_push();
extern ptr lst_featob_create();
extern ptr lst_featob_append();
extern ptr lst_featob_push();

extern ptr lst_int_create();
extern ptr lst_int_push();

extern ptr assert_stmtob_create();
extern ptr when_stmtob_create();
extern ptr switch_stmtob_create();
extern ptr loop_stmtob_create();
extern ptr elsif_stmtob_create();
extern ptr except_stmtob_create();
extern ptr cond_stmtob_create();
extern ptr assign_stmtob_create();
extern ptr call_stmtob_create();
extern ptr break_stmtob_create();
extern ptr return_stmtob_create();

extern ptr local_decl_stmtob_create();
extern ptr local_decl_stmtob_create_lst();

extern ptr simple_typeob_create();
extern ptr dispatch_typeob_create();
extern ptr param_typeob_create();

extern ptr const_decl_featob_create_lst();
extern ptr shared_decl_featob_create_lst();
extern ptr attr_decl_featob_create_lst();
extern ptr param_declob_create_lst();

extern ptr cinh_featob_create();
extern ptr rout_featob_create();
extern ptr alias_featob_create();
extern ptr rout_specob_create();

extern ptr any_declob_create();
extern int any_declob_ith_name();
extern ptr any_declob_get_type_spec();

extern ptr main_process_classdef();
extern ptr classob_table_install();
extern ptr classob_create();
extern ptr lst_featob_mark_private();
extern ptr lst_featob_mark_readonly();

extern ptr globals_classes_defs();

extern int str_table_index_of_str();
extern ptr str_table_at_index();
extern ptr str_buffer_create();
extern ptr str_buffer_strval();
extern char str_buffer_equal();
extern ptr str_buffer_terminate();
extern int str_buffer_length();
extern void str_buffer_init();
extern ptr str_buffer_push();
extern char str_buffer_pop();

extern int globals_curr_lineno;

/*
  Partial list of Sather constants used in "lexer.h".
  Refer to Sather definitions in "constants.sa".
*/

#define ABSTRACT_IND 66
#define AGAINST_IND 70
#define AND_IND 1
#define ASSERT_IND 2
#define ATTR_IND 72
#define BOOL_IND 42
#define BREAK_IND 3
#define CASE_IND 19
#define CLASS_IND 4
#define CONSTANT_IND 5
#define DEFINE_IND 62
#define ELSE_IND 7
#define ELSIF_IND 8
#define END_IND 9
#define ENSURE_IND 65
#define EXCEPTION_IND 60
#define ARG_IND 76
#define FALSE_IND 35 
#define IF_IND 10
#define INCLUDE_IND 75
#define INVARIANT_IND 67
#define IS_IND 12
#define LOOP_IND 13
#define NEW_IND 31
#define NOT_IND 14
#define OR_IND 15
#define PRIVATE_IND 16
#define PROTECT_IND 69
#define RAISE_IND 68
#define READONLY_IND 73
#define REQUIRE_IND 64
#define RES_IND 33
#define RETURN_IND 17
#define SHARED_IND 18
#define THEN_IND 20
#define TRUE_IND 36
#define TYPECASE_IND 71
#define UNDEFINE_IND 63
#define UNDEF_TYPE_IND 55
#define UNTIL_IND 21
#define WHEN_IND 22
#define WHILE_IND 74

#define NOT_OP_IND 1
#define LT_OP_IND 2
#define GT_OP_IND 3
#define LE_OP_IND 4
#define GE_OP_IND 5
#define EQ_OP_IND 6
#define NE_OP_IND 7
#define AND_OP_IND 8
#define OR_OP_IND 9
#define UMINUS_OP_IND 10
#define UPLUS_OP_IND 11
#define EXP_OP_IND 12
#define PLUS_OP_IND 13
#define MINUS_OP_IND 14
#define MULT_OP_IND 15
#define DIVIDE_OP_IND 16


# line 209 "ssather.y"
typedef union {
int lnno;                       /* the current Sather line number  */
int ind;			/* the index into the string table */
ptr val;			/* a pointer to a syntactic object */
} YYSTYPE;
# define ABSTRACT 257
# define AGAINST 258
# define ASSERT 259
# define ASSIGN 260
# define ATTR 261
# define BREAK 262
# define CASE 263
# define CLASS 264
# define CONSTANT 265
# define DEFINE 266
# define ELSE 267
# define ELSIF 268
# define END 269
# define ENSURE 270
# define INCLUDE 271
# define INVARIANT 272
# define IF 273
# define IS 274
# define LOOP 275
# define PRIVATE 276
# define PROTECT 277
# define RAISE 278
# define READONLY 279
# define REQUIRE 280
# define RETURN 281
# define SHARED 282
# define THEN 283
# define TYPECASE 284
# define UNDEFINE 285
# define UNTIL 286
# define WHEN 287
# define WHILE 288
# define CREF 289
# define CHAR_CONST 290
# define INT_CONST 291
# define REAL_CONST 292
# define STR_CONST 293
# define BOOL_CONST 294
# define IDENTIFIER 295
# define TYPID 296
# define OR 297
# define AND 298
# define NE 299
# define LE 300
# define GE 301
# define NOT 302
# define NO_OP 303
# define UNARY 304
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 973 "ssather.y"

#include "lexer.h"


int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 5,
	0, 3,
	59, 3,
	-2, 0,
-1, 10,
	274, 14,
	60, 14,
	-2, 0,
-1, 24,
	274, 14,
	60, 14,
	-2, 0,
-1, 59,
	269, 24,
	59, 24,
	-2, 0,
-1, 70,
	274, 87,
	-2, 60,
-1, 71,
	44, 21,
	-2, 87,
-1, 128,
	269, 81,
	270, 81,
	274, 81,
	59, 81,
	-2, 0,
-1, 130,
	269, 85,
	274, 85,
	287, 85,
	59, 85,
	-2, 0,
-1, 169,
	58, 64,
	-2, 22,
-1, 232,
	258, 104,
	267, 104,
	268, 104,
	269, 104,
	287, 104,
	59, 104,
	44, 21,
	-2, 151,
-1, 243,
	258, 115,
	267, 115,
	268, 115,
	269, 115,
	287, 115,
	59, 115,
	-2, 157,
-1, 257,
	269, 126,
	274, 126,
	59, 126,
	-2, 0,
-1, 263,
	300, 0,
	60, 0,
	62, 0,
	301, 0,
	-2, 162,
-1, 264,
	300, 0,
	60, 0,
	62, 0,
	301, 0,
	-2, 163,
-1, 265,
	300, 0,
	60, 0,
	62, 0,
	301, 0,
	-2, 164,
-1, 266,
	300, 0,
	60, 0,
	62, 0,
	301, 0,
	-2, 165,
-1, 294,
	258, 99,
	267, 99,
	268, 99,
	269, 99,
	287, 99,
	59, 99,
	-2, 0,
	};
# define YYNPROD 179
# define YYLAST 888
int yyact[]={

   246,   379,   133,   220,    44,    43,    44,   311,    21,    10,
   280,   281,   164,    44,    84,    64,    86,   159,    50,    24,
   162,    23,   161,    44,    16,    62,    61,    64,    83,   229,
     6,   386,    69,   157,   282,   384,    40,    36,   360,    44,
   327,    25,   325,    42,   275,    42,    84,    64,    23,    69,
    93,    94,   174,    46,    36,    70,    37,   173,    70,    92,
    44,    51,    84,    64,   358,    44,    71,    64,   165,    79,
    46,   169,    81,    37,   208,   206,    72,   207,   231,   209,
   230,   373,   120,    71,    64,    42,   110,   115,   116,   371,
   225,    44,   198,   202,   199,   288,   286,   218,   208,   206,
   381,   207,   388,   209,   105,   284,   369,   291,   308,   125,
   148,   130,    91,   223,    44,   100,   198,   202,   199,   130,
   380,   171,    90,   175,    42,   136,   106,    19,   177,   405,
    42,   100,    42,   196,   187,   100,   402,   253,   189,   188,
   141,   128,   393,   338,   381,   399,   144,   212,   146,    99,
   390,   349,   330,   117,   339,    75,   135,    59,   100,     7,
   213,   215,   216,   217,   342,   295,   221,     4,   164,    44,
   219,    85,   227,   159,     3,   303,   162,   228,   161,   258,
    18,   176,   294,   109,     9,     4,   137,   122,    59,   183,
    59,    59,     3,    59,   181,    59,   185,   252,    59,   263,
   264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
   274,   243,   221,   221,   191,   276,   277,    54,   259,    42,
   100,    42,   243,    97,   165,    45,   139,    45,   334,   287,
   292,   289,   293,   324,    45,   260,    59,   261,   297,   298,
   300,    96,   301,   290,    45,   210,   335,   166,   184,   304,
   305,   306,   307,   164,    44,   167,   132,    55,   159,   312,
    45,   162,   313,   161,    50,   294,    50,   152,   153,   154,
   156,   155,   151,    50,   356,   208,   206,    85,   207,   160,
   209,    45,    53,    50,   221,   323,    45,   322,    96,   348,
   211,   257,   118,   198,   202,   199,    87,   164,    44,    50,
   256,   190,   159,   254,   341,   162,   343,   161,   345,   165,
   317,   346,    45,   147,    85,   320,   321,   111,   104,    91,
    50,   126,   326,    78,   328,    50,   347,    15,   243,    90,
   340,   203,   200,   201,   243,    45,   344,   361,   362,   182,
   243,   363,    42,   364,   400,   195,   355,   131,   134,    91,
   163,    50,    91,   165,   204,   203,   200,   201,   350,    90,
   353,    98,   354,    52,   372,   316,   374,   370,    34,    49,
    34,   382,   383,   332,    50,   331,   368,   164,    44,    12,
   243,   221,   159,    91,   391,   162,    76,   161,   193,   214,
    45,   398,   366,    90,    17,   378,   296,   401,   315,   243,
   314,   197,   403,   194,   406,   192,   103,   385,   101,   387,
    34,   392,    15,   394,    89,   243,    26,   243,   397,   395,
    44,   396,   243,   152,   153,   154,   156,   155,   151,    50,
   404,   255,     5,   165,   243,   160,   124,   408,    97,   170,
   284,   243,   352,   172,   112,   284,    58,    49,   140,    34,
    49,   143,    44,   208,   310,    34,    49,    34,   209,   351,
    57,   124,   284,   168,   376,   208,   206,   279,   207,    22,
   209,   121,   119,   113,   333,    45,   107,   244,    11,   319,
   240,   249,   284,   198,   202,   199,   283,   208,   206,   318,
   207,   247,   209,   248,   285,   237,   241,    48,   337,   242,
   336,   302,   212,   129,   235,   198,   236,   199,   152,   153,
   154,   156,   155,   232,    47,     1,   407,   222,   224,    45,
   160,   244,     2,   102,   240,   249,    91,    88,     8,   158,
   205,   204,   203,   200,   201,   247,    90,   248,    65,   237,
   241,   150,   149,   242,    34,    14,    34,    49,   235,   299,
   236,    30,   152,   153,   154,   156,   155,   232,    47,   375,
   208,   206,   367,   207,   160,   209,   208,   206,   239,   207,
   238,   209,   226,   389,   180,   179,    80,   179,   198,   202,
   199,    68,   234,   233,   198,   202,   199,   278,   208,   206,
   178,   207,   178,   209,   138,   208,   206,   142,   207,    45,
   209,    13,    77,   145,   309,    39,   198,   202,   199,    33,
    38,    63,    20,   198,   202,   199,   208,   206,    28,   207,
     0,   209,     0,     0,     0,     0,    56,     0,     0,   245,
     0,     0,   152,   153,   154,   156,   155,   151,    50,     0,
   108,    45,   329,     0,   160,    27,   124,     0,    66,    40,
    36,    73,     0,     0,     0,    35,    31,    60,     0,    67,
    29,     0,    74,    32,     0,     0,    46,    34,     0,    37,
    82,     0,     0,    45,     0,     0,    95,    27,     0,    41,
    47,    40,    36,     0,     0,   357,   359,    35,    31,     0,
     0,     0,    29,     0,   262,    32,     0,     0,    46,   114,
     0,    37,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    41,    47,   123,   377,     0,     0,   127,     0,     0,
   205,   204,   203,   200,   201,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   200,   201,   127,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   186,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   365,   250,   251,
     0,     0,   186,     0,     0,   205,   204,   203,   200,   201,
     0,   205,   204,   203,   200,   201,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   205,   204,   203,   200,   201,     0,     0,
   205,   204,   203,   200,   201,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   186 };
int yypact[]={

   -90,   373, -1000,  -266,  -105,   -72, -1000,   442, -1000,   320,
   289,  -272, -1000,   120,  -147,  -248, -1000,   416,    24,   416,
   238,    92, -1000,   197,   204,   177, -1000,  -269, -1000,  -229,
 -1000,  -219,  -212,  -114, -1000,    24,  -226,  -223, -1000, -1000,
  -233,   256,  -158, -1000,  -278,  -278,  -281,   165,    87, -1000,
   118,   139, -1000,  -275, -1000,    24,    44,  -148, -1000,   384,
   -77,    28, -1000,   386,   380,   429, -1000, -1000, -1000,  -269,
  -158,   113,   234, -1000, -1000, -1000, -1000,   428, -1000, -1000,
   427, -1000,   -73,  -281,   219,    24,  -165,  -281,    82,    60,
 -1000,  -109,  -149, -1000, -1000,   -74,   -30,    24,   416,    24,
    24, -1000, -1000, -1000,   416,    24,   416, -1000,   254,   342,
   -13,   194,    24,  -224,   -77,  -149,  -165,  -281, -1000,  -226,
  -238,  -243,   342,   -79, -1000, -1000,   533, -1000,   -62,    52,
   -67,  -281, -1000,   342, -1000, -1000, -1000,   342,   176,    89,
 -1000,   136,   114, -1000,   134,    71,   132, -1000,   553,   199,
 -1000,   462, -1000, -1000, -1000, -1000, -1000, -1000, -1000,   133,
   342,   342,   342,  -192,  -278,   342,    78,    55, -1000, -1000,
   531,   342, -1000, -1000, -1000,   553,   342,   262,  -281,  -281,
    79,   244,  -281,   241,   232, -1000,   -81,   553,   262,   553,
 -1000, -1000, -1000,   416, -1000,   416,    24, -1000,   342,   342,
   342,   342,   342,   342,   342,   342,   342,   342,   342,   342,
  -251,   342,   342,   546,   426, -1000, -1000, -1000,  -285,   446,
   401,   553,  -193,  -278,  -194,  -278,    49,   553,   553,   206,
 -1000, -1000,   107, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,   342, -1000, -1000, -1000,   -85,   553, -1000, -1000, -1000,
 -1000, -1000,  -166,    24, -1000,   232, -1000,  -249,   342,   206,
   131,   129,    91,   574,   574,   574,   574,   445,   445,    32,
    56,   411,   411, -1000, -1000,   446,   396,   438, -1000, -1000,
   446,   446, -1000,   342,   -23, -1000,  -253,   446,  -255,   446,
  -166,    24, -1000,  -117,   218,   -32,   185,   460,   458,  -115,
   262,   553, -1000,   342,   -96,   342,   262,   342, -1000,  -158,
 -1000,   230,   553,  -118, -1000, -1000,   416, -1000, -1000, -1000,
 -1000, -1000,   418,   553,   398,   446, -1000,   446, -1000,  -158,
 -1000, -1000, -1000,   215,    29,     3,   342,   342, -1000, -1000,
   206,   553,   342,   524,   123,   553,   262,  -168, -1000, -1000,
    98, -1000, -1000, -1000, -1000,  -168, -1000,  -200,  -278,  -208,
  -278,   518,   423,    24,   553,   262, -1000,  -167,   206, -1000,
 -1000,  -260,   446,  -264,   446, -1000, -1000,  -181,   206,  -119,
   342,   262,  -127,   262,   446, -1000,   446, -1000,   262,  -123,
 -1000,    61,   206, -1000,   206, -1000, -1000,   206,  -133, -1000,
   262,  -140, -1000,   342,   206, -1000,   233,   262,   206 };
int yypgo[]={

     0,   601,   538,    41,   416,   618,   350,   612,   497,   629,
    25,   611,     5,   610,   321,   609,   605,    16,   323,   602,
    29,    80,   469,    78,   583,   582,   573,     1,   570,   568,
   562,   549,   256,    33,    34,     3,     0,   542,   541,   529,
   551,   196,   248,   527,   414,     2,   515,   522 };
int yyr1[]={

     0,    46,    46,    46,    46,    46,    47,    47,    47,    47,
    47,    47,    47,    47,     1,     1,     1,     7,     7,    22,
    22,     2,     2,     3,     3,     3,     3,     3,     4,     4,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     5,     5,     5,     5,     5,     5,     5,     5,     5,    19,
    19,    18,     6,     6,     6,    12,    12,    12,     8,     8,
     9,     9,    10,    10,    11,    13,    13,    14,    14,    14,
    14,    14,    16,    16,    16,    16,    15,    15,    15,    15,
    43,    43,    43,    44,    44,    44,    44,    17,    17,    17,
    17,    17,    17,    40,    40,    31,    31,    20,    20,    20,
    20,    20,    20,    20,    21,    21,    21,    21,    21,    21,
    21,    21,    21,    21,    21,    21,    21,    23,    23,    23,
    23,    23,    23,    41,    42,    42,    42,    42,    24,    25,
    26,    26,    27,    27,    28,    29,    30,    30,    32,    33,
    33,    33,    33,    33,    34,    34,    35,    35,    35,    36,
    36,    37,    37,    37,    37,    37,    37,    37,    37,    37,
    37,    38,    38,    38,    38,    38,    38,    38,    38,    38,
    38,    38,    38,    38,    38,    38,    39,    39,    45 };
int yyr2[]={

     0,     0,     2,     4,     6,     8,    15,    19,    23,    15,
    19,    23,    27,    19,     1,     7,     7,     3,     7,     3,
     7,     3,     7,     3,     5,     7,     9,     1,     5,     3,
     5,     5,     3,     5,    13,     5,     5,     5,     3,     5,
     9,    17,    17,    15,    15,     5,     5,     3,     3,     3,
     7,     7,     3,     5,     5,     3,     9,     9,     3,     7,
     3,     7,     7,     7,     7,     5,     9,     3,     5,     7,
     5,     7,    15,    21,    15,    25,     5,    11,     5,    15,
     5,     5,     9,     5,     5,     5,     9,     1,     3,     3,
     5,     9,     7,     9,    11,     5,    13,     3,     3,     5,
     7,     7,     9,     1,     3,     3,     3,    11,    11,     7,
     3,     3,     3,     5,     3,     3,     5,     3,     7,    15,
    15,    13,    13,     7,     3,     7,     5,     9,     9,    17,
     1,    13,     5,     1,     9,    13,    11,     1,     5,     9,
     9,     9,     9,     7,     7,     1,     3,     7,     9,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     7,
     7,     5,     7,     7,     7,     7,     7,     7,     7,     7,
     5,     5,     7,     7,     7,     7,     9,     7,     1 };
int yychk[]={

 -1000,   -46,   -47,   264,   257,    59,   296,   264,   -47,   256,
   -45,    36,    59,    -1,   256,   123,   296,   274,    60,   274,
    -7,   256,   -22,   296,   -45,    -3,    -4,   261,    -5,   276,
   -40,   272,   279,   -15,    -6,   271,   266,   285,   -13,   -16,
   265,   295,   -10,   -12,    36,   257,   282,   296,    -8,    -6,
   296,    -3,   125,    44,   125,    60,    -1,   256,   269,    59,
    -9,   295,   -10,   -11,   296,    -2,    -5,    -9,   -40,   261,
   -10,   295,   295,    -5,    -9,   269,    -6,   -19,   -18,   295,
    -2,   295,    -9,   261,   295,    58,   -17,    40,   -43,   -44,
   280,   270,   -17,   -12,   -12,    -9,   123,    58,   274,    62,
    44,   269,   -22,    -6,   274,    60,   274,    -4,   256,   260,
    58,   289,    58,    44,    -9,   -17,   -17,    40,    58,    44,
   -45,    44,   260,    -9,    -6,   274,   -14,    -9,    59,   -44,
    59,   287,   -32,   -45,   -32,   265,   274,   260,    -8,   256,
    -6,    -3,    -8,    -6,    -3,    -8,    -3,    59,   -36,   -37,
   -38,   295,   290,   291,   292,   294,   293,   -33,   -39,    40,
   302,    45,    43,    -6,    35,    91,   260,    61,    -6,   295,
   -14,   -45,   -18,   295,   295,   -36,   260,   -45,    59,    44,
    41,   256,   287,   256,   -42,   -41,    -9,   -36,   -45,   -36,
   125,   125,   269,   274,   269,   274,    62,   269,    60,    62,
   300,   301,    61,   299,   298,   297,    43,    45,    42,    47,
    46,    91,    40,   -36,   256,   -36,   -36,   -36,   289,   -12,
   -35,   -36,    -6,    35,    -6,    35,    41,   -36,   -36,   -20,
   -21,   -23,   295,   -24,   -25,   286,   288,   277,   -28,   -29,
   262,   278,   281,   -33,   259,    -9,   -36,   273,   275,   263,
    -9,    -9,   -17,    58,    59,   -42,    59,    59,   260,   -20,
    -3,    -3,    -8,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
   -36,   -36,   -36,   -36,   -36,   295,   -35,   -35,    41,    41,
   295,   296,   -34,    40,    44,    93,   289,   -12,   289,   -12,
   -17,    58,   -45,   -45,    59,    58,   289,   -45,   -45,   -31,
   -45,   -36,   -32,   260,   -45,   -45,   -45,   -45,   274,    -6,
   -41,   256,   -36,   -45,   269,   269,   274,   -34,    93,    41,
   -34,   -34,   -35,   -36,   256,   295,   -34,   295,   -34,    -6,
   269,   -21,   -23,   256,   260,    61,    40,    40,   258,   269,
   -20,   -36,   260,   -36,   -20,   -36,   -45,   -17,    59,   269,
    -3,    41,    44,   -34,   -34,   -17,    59,    -6,    35,    -6,
    35,   -36,   -36,   -45,   -36,   283,   269,   -30,   -20,   274,
   269,   289,   -12,   289,   -12,    41,    41,    -6,   -20,   -27,
   287,   267,   -45,   -45,   295,   -34,   295,   -34,   283,   -26,
   269,   -35,   -20,   269,   -20,   -34,   -34,   -20,   -27,   268,
   283,   -45,   269,   -45,   -20,   269,   -36,   283,   -20 };
int yydef[]={

     1,    -2,     2,     0,     0,    -2,   178,     0,     4,     0,
    -2,     0,     5,     0,     0,     0,   178,    27,     0,    27,
     0,     0,    17,    19,    -2,     0,    23,     0,    29,     0,
    32,     0,     0,     0,    38,     0,     0,     0,    47,    48,
     0,    87,    87,    52,     0,     0,     0,    55,     0,    58,
    55,     0,    15,     0,    16,     0,     0,     0,     6,    -2,
    28,    21,    60,     0,     0,     0,    30,    31,    33,     0,
    -2,    -2,     0,    35,    36,    37,    39,    45,    49,   178,
    46,    21,     0,     0,    21,     0,    76,     0,    88,    89,
   178,   178,    78,    53,    54,    65,     0,     0,    27,     0,
     0,     9,    18,    20,    27,     0,    27,    25,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   178,     0,
     0,     0,     0,     0,    62,   178,     0,    67,    -2,    90,
    -2,     0,    80,     0,    83,    84,   178,     0,     0,     0,
    63,     0,     0,    59,     0,     0,     0,    26,    40,   149,
   150,   151,   152,   153,   154,   155,   156,   157,   158,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    61,    -2,
     0,     0,    50,    51,    22,    93,     0,   103,    68,    70,
    87,     0,     0,     0,    92,   124,     0,   138,   103,    66,
    56,    57,     7,    27,    10,    27,     0,    13,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   161,   170,   171,     0,   145,
     0,   146,     0,     0,     0,     0,    87,   178,    94,   178,
    97,    98,    -2,   105,   106,   178,   178,   178,   110,   111,
   112,     0,   114,    -2,   178,   117,   178,   178,   178,   178,
    69,    71,    77,     0,    82,    91,    86,    -2,     0,   178,
     0,     0,     0,    -2,    -2,    -2,    -2,   166,   167,   168,
   169,   172,   173,   174,   175,   145,     0,     0,   159,   160,
   145,   145,   143,     0,     0,   177,     0,   145,     0,   145,
     0,     0,    34,     0,    -2,     0,     0,     0,     0,     0,
   103,   113,   116,     0,     0,     0,   103,     0,   178,    87,
   125,     0,   123,     0,     8,    11,    27,   140,   176,   139,
   141,   142,     0,   147,     0,   145,    43,   145,    44,    87,
    72,   100,   101,     0,     0,     0,     0,     0,   178,   109,
    95,   118,     0,     0,     0,   137,   103,    79,   127,    74,
     0,   144,   148,    41,    42,     0,   102,     0,     0,     0,
     0,     0,     0,     0,   128,   103,   134,   133,   178,   178,
    12,     0,   145,     0,   145,   107,   108,     0,   130,     0,
     0,   103,     0,   103,   145,   121,   145,   122,   103,   133,
   135,     0,   132,    73,   178,   119,   120,    96,     0,   178,
   103,     0,   129,     0,   136,    75,     0,   103,   131 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"ABSTRACT",	257,
	"AGAINST",	258,
	"ASSERT",	259,
	"ASSIGN",	260,
	"ATTR",	261,
	"BREAK",	262,
	"CASE",	263,
	"CLASS",	264,
	"CONSTANT",	265,
	"DEFINE",	266,
	"ELSE",	267,
	"ELSIF",	268,
	"END",	269,
	"ENSURE",	270,
	"INCLUDE",	271,
	"INVARIANT",	272,
	"IF",	273,
	"IS",	274,
	"LOOP",	275,
	"PRIVATE",	276,
	"PROTECT",	277,
	"RAISE",	278,
	"READONLY",	279,
	"REQUIRE",	280,
	"RETURN",	281,
	"SHARED",	282,
	"THEN",	283,
	"TYPECASE",	284,
	"UNDEFINE",	285,
	"UNTIL",	286,
	"WHEN",	287,
	"WHILE",	288,
	"CREF",	289,
	"CHAR_CONST",	290,
	"INT_CONST",	291,
	"REAL_CONST",	292,
	"STR_CONST",	293,
	"BOOL_CONST",	294,
	"IDENTIFIER",	295,
	"TYPID",	296,
	"OR",	297,
	"AND",	298,
	"=",	61,
	"NE",	299,
	"LE",	300,
	"<",	60,
	">",	62,
	"GE",	301,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"NOT",	302,
	".",	46,
	"NO_OP",	303,
	"UNARY",	304,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"class_list : /* empty */",
	"class_list : class",
	"class_list : class_list ';'",
	"class_list : class_list ';' class",
	"class_list : class_list ';' error ';'",
	"class : CLASS TYPID getlnno opt_type_vars IS feature_list END",
	"class : CLASS TYPID getlnno opt_type_vars '<' type_spec_list IS feature_list END",
	"class : CLASS TYPID getlnno opt_type_vars '<' type_spec_list '>' type_spec_list IS feature_list END",
	"class : CLASS TYPID getlnno error IS feature_list END",
	"class : ABSTRACT CLASS '$' TYPID getlnno opt_type_vars IS feature_list END",
	"class : ABSTRACT CLASS '$' TYPID getlnno opt_type_vars '<' type_spec_list IS feature_list END",
	"class : ABSTRACT CLASS '$' TYPID getlnno opt_type_vars '<' type_spec_list '>' type_spec_list IS feature_list END",
	"class : ABSTRACT CLASS '$' TYPID getlnno error IS feature_list END",
	"opt_type_vars : /* empty */",
	"opt_type_vars : '{' typid_list '}'",
	"opt_type_vars : '{' error '}'",
	"typid_list : typid",
	"typid_list : typid_list ',' typid",
	"typid : TYPID",
	"typid : TYPID '<' type_spec",
	"ident_list : IDENTIFIER",
	"ident_list : ident_list ',' IDENTIFIER",
	"feature_list : feature",
	"feature_list : feature_list ';'",
	"feature_list : feature_list ';' feature",
	"feature_list : feature_list ';' error ';'",
	"feature_list : /* empty */",
	"feature : ATTR var_dec",
	"feature : featdef",
	"feature : PRIVATE featdef",
	"feature : PRIVATE var_dec",
	"feature : const_attr_dec",
	"feature : PRIVATE const_attr_dec",
	"feature : INVARIANT IDENTIFIER ':' getlnno expr getlnno",
	"feature : READONLY featdef",
	"feature : READONLY var_dec",
	"feature : routine_dec END",
	"feature : type_spec",
	"feature : INCLUDE type_spec",
	"featdef : ATTR var_dec ASSIGN expr",
	"featdef : ATTR IDENTIFIER ':' ASSIGN type_spec CREF IDENTIFIER arg_vals",
	"featdef : ATTR IDENTIFIER CREF '=' type_spec CREF IDENTIFIER arg_vals",
	"featdef : ATTR IDENTIFIER ':' ASSIGN '#' concrete_type_spec arg_vals",
	"featdef : ATTR IDENTIFIER CREF '=' '#' concrete_type_spec arg_vals",
	"featdef : DEFINE alias_list",
	"featdef : UNDEFINE ident_list",
	"featdef : shared_attr_dec",
	"featdef : routine_def",
	"alias_list : alias_dec",
	"alias_list : alias_list ',' alias_dec",
	"alias_dec : IDENTIFIER getlnno IDENTIFIER",
	"type_spec : concrete_type_spec",
	"type_spec : '$' concrete_type_spec",
	"type_spec : ABSTRACT concrete_type_spec",
	"concrete_type_spec : TYPID",
	"concrete_type_spec : TYPID '{' type_spec_list '}'",
	"concrete_type_spec : TYPID '{' error '}'",
	"type_spec_list : type_spec",
	"type_spec_list : type_spec_list ',' type_spec",
	"var_dec : single_var_dec",
	"var_dec : mult_ident_list ':' type_spec",
	"single_var_dec : IDENTIFIER ':' type_spec",
	"single_var_dec : TYPID ':' type_spec",
	"mult_ident_list : ident_list ',' IDENTIFIER",
	"shared_attr_dec : SHARED var_dec",
	"shared_attr_dec : SHARED var_dec ASSIGN expr",
	"var_dec_list : var_dec",
	"var_dec_list : var_dec_list ';'",
	"var_dec_list : var_dec_list ';' var_dec",
	"var_dec_list : var_dec_list ','",
	"var_dec_list : var_dec_list ',' var_dec",
	"routine_def : IDENTIFIER routine_spec IS getlnno statement_list getlnno END",
	"routine_def : IDENTIFIER '(' var_dec_list ')' routine_spec IS getlnno statement_list getlnno END",
	"routine_def : single_var_dec routine_spec IS getlnno statement_list getlnno END",
	"routine_def : IDENTIFIER '(' var_dec_list ')' ':' type_spec routine_spec IS getlnno statement_list getlnno END",
	"routine_dec : IDENTIFIER routine_spec",
	"routine_dec : IDENTIFIER '(' var_dec_list ')' routine_spec",
	"routine_dec : single_var_dec routine_spec",
	"routine_dec : IDENTIFIER '(' var_dec_list ')' ':' type_spec routine_spec",
	"require : REQUIRE assert",
	"require : require ';'",
	"require : require ';' error ';'",
	"ensure : ENSURE assert",
	"ensure : ENSURE CONSTANT",
	"ensure : ensure ';'",
	"ensure : ensure ';' error ';'",
	"routine_spec : /* empty */",
	"routine_spec : require",
	"routine_spec : ensure",
	"routine_spec : require ensure",
	"routine_spec : require ensure WHEN old_dec_list",
	"routine_spec : ensure WHEN old_dec_list",
	"const_attr_dec : CONSTANT var_dec ASSIGN expr",
	"const_attr_dec : CONSTANT ATTR var_dec ASSIGN expr",
	"protected : getlnno statement_list",
	"protected : protected AGAINST getlnno type_spec THEN statement_list",
	"statement_list : statement",
	"statement_list : local_dec",
	"statement_list : statement_list ';'",
	"statement_list : statement_list ';' statement",
	"statement_list : statement_list ';' local_dec",
	"statement_list : statement_list ';' error ';'",
	"statement_list : /* empty */",
	"statement : IDENTIFIER",
	"statement : assignment",
	"statement : conditional",
	"statement : UNTIL getlnno '(' expr ')'",
	"statement : WHILE getlnno '(' expr ')'",
	"statement : PROTECT protected END",
	"statement : loop",
	"statement : case",
	"statement : BREAK",
	"statement : RAISE expr",
	"statement : RETURN",
	"statement : call",
	"statement : ASSERT assert",
	"local_dec : var_dec",
	"local_dec : var_dec ASSIGN expr",
	"local_dec : IDENTIFIER ':' ASSIGN type_spec CREF IDENTIFIER arg_vals",
	"local_dec : IDENTIFIER CREF '=' type_spec CREF IDENTIFIER arg_vals",
	"local_dec : IDENTIFIER ':' ASSIGN '#' concrete_type_spec arg_vals",
	"local_dec : IDENTIFIER CREF '=' '#' concrete_type_spec arg_vals",
	"old_dec : var_dec ASSIGN expr",
	"old_dec_list : old_dec",
	"old_dec_list : old_dec_list ';' old_dec",
	"old_dec_list : old_dec_list ';'",
	"old_dec_list : old_dec_list ';' error ';'",
	"assignment : expr getlnno ASSIGN expr",
	"conditional : IF getlnno expr THEN statement_list elsif_part else_part END",
	"elsif_part : /* empty */",
	"elsif_part : elsif_part ELSIF getlnno expr THEN statement_list",
	"else_part : ELSE statement_list",
	"else_part : /* empty */",
	"loop : LOOP getlnno statement_list END",
	"case : CASE getlnno expr when_part else_part END",
	"when_part : when_part WHEN exp_list THEN statement_list",
	"when_part : /* empty */",
	"assert : getlnno expr",
	"call : IDENTIFIER '(' exp_list ')'",
	"call : cexpr '.' IDENTIFIER arg_vals",
	"call : type_spec CREF IDENTIFIER arg_vals",
	"call : type_spec CREF TYPID arg_vals",
	"call : '#' concrete_type_spec arg_vals",
	"arg_vals : '(' exp_list ')'",
	"arg_vals : /* empty */",
	"exp_list : expr",
	"exp_list : exp_list ',' expr",
	"exp_list : exp_list ',' error ','",
	"expr : cexpr",
	"expr : nexpr",
	"cexpr : IDENTIFIER",
	"cexpr : CHAR_CONST",
	"cexpr : INT_CONST",
	"cexpr : REAL_CONST",
	"cexpr : BOOL_CONST",
	"cexpr : STR_CONST",
	"cexpr : call",
	"cexpr : aref",
	"cexpr : '(' expr ')'",
	"cexpr : '(' error ')'",
	"nexpr : NOT expr",
	"nexpr : expr '<' expr",
	"nexpr : expr '>' expr",
	"nexpr : expr LE expr",
	"nexpr : expr GE expr",
	"nexpr : expr '=' expr",
	"nexpr : expr NE expr",
	"nexpr : expr AND expr",
	"nexpr : expr OR expr",
	"nexpr : '-' expr",
	"nexpr : '+' expr",
	"nexpr : expr '+' expr",
	"nexpr : expr '-' expr",
	"nexpr : expr '*' expr",
	"nexpr : expr '/' expr",
	"aref : cexpr '[' exp_list ']'",
	"aref : '[' exp_list ']'",
	"getlnno : /* empty */",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
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
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
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
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

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
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
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
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
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
		
case 6:
# line 261 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-5].ind,yypvt[-4].lnno,yypvt[-3].val,yypvt[-1].val,NULL)); } break;
case 7:
# line 264 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-7].ind,yypvt[-6].lnno,yypvt[-5].val,yypvt[-1].val,NULL)); } break;
case 8:
# line 267 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-9].ind,yypvt[-8].lnno,yypvt[-7].val,yypvt[-1].val,NULL)); } break;
case 9:
# line 270 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-5].ind,yypvt[-4].lnno,NULL,yypvt[-1].val,NULL)); } break;
case 10:
# line 273 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-5].ind,yypvt[-4].lnno,yypvt[-3].val,yypvt[-1].val,1)); } break;
case 11:
# line 276 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-7].ind,yypvt[-6].lnno,yypvt[-5].val,yypvt[-1].val,1)); } break;
case 12:
# line 279 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-9].ind,yypvt[-8].lnno,yypvt[-7].val,yypvt[-1].val,1)); } break;
case 13:
# line 282 "ssather.y"
{ main_process_classdef(0, classob_create(NULL,yypvt[-5].ind,yypvt[-4].lnno,NULL,yypvt[-1].val,1)); } break;
case 14:
# line 287 "ssather.y"
{ yyval.val = (ptr)NULL; } break;
case 15:
# line 289 "ssather.y"
{ yyval.val = yypvt[-1].val; } break;
case 16:
# line 292 "ssather.y"
{ yyval.val = (ptr)NULL; } break;
case 17:
# line 296 "ssather.y"
{ yyval.val = (ptr)lst_int_push(lst_int_create(NULL,5),yypvt[-0].val); } break;
case 18:
# line 298 "ssather.y"
{ yyval.val = (ptr)lst_int_push(yypvt[-2].val,yypvt[-0].val); } break;
case 19:
# line 302 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].ind; } break;
case 20:
# line 304 "ssather.y"
{ yyval.val = (ptr)yypvt[-2].ind; } break;
case 21:
# line 308 "ssather.y"
{ yyval.val = (ptr)lst_int_push(lst_int_create(NULL,5),yypvt[-0].ind); } break;
case 22:
# line 310 "ssather.y"
{ yyval.val = (ptr)lst_int_push(yypvt[-2].val,yypvt[-0].ind); } break;
case 23:
# line 315 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 24:
# line 318 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 25:
# line 321 "ssather.y"
{ yyval.val = (ptr)lst_featob_append(yypvt[-2].val,yypvt[-0].val); } break;
case 26:
# line 324 "ssather.y"
{ yyval.val = (ptr)yypvt[-3].val; } break;
case 27:
# line 326 "ssather.y"
{ yyval.val = (ptr)lst_featob_create(NULL,5); } break;
case 28:
# line 332 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst(NULL,yypvt[-0].val,NULL); } break;
case 29:
# line 335 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 30:
# line 338 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val;
      lst_featob_mark_private(yyval.val); } break;
case 31:
# line 342 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst(NULL,yypvt[-0].val,NULL); 
      lst_featob_mark_private(yyval.val); } break;
case 32:
# line 346 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 33:
# line 349 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val;
      lst_featob_mark_private(yyval.val); } break;
case 34:
# line 353 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(
		   lst_featob_create(NULL,1),
                   rout_featob_create(NULL,yypvt[-4].ind,
				      rout_specob_create(NULL,NULL,NULL,NULL),
				      NULL,
				      simple_typeob_create(NULL,BOOL_IND), 
				      lst_stmtob_push(lst_stmtob_create(NULL,1),
                                       assign_stmtob_create(NULL,
                                              id_exprob_create(NULL,RES_IND),
							    yypvt[-1].val,yypvt[-2].lnno))),
                   yypvt[-2].lnno,yypvt[-0].lnno);
      lst_featob_mark_spec(yyval.val); } break;
case 35:
# line 367 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val;
      lst_featob_mark_readonly(yyval.val); } break;
case 36:
# line 371 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst(NULL,yypvt[-0].val,NULL); 
      lst_featob_mark_readonly(yyval.val); } break;
case 37:
# line 375 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				yypvt[-1].val);
      lst_featob_mark_abstract(yyval.val); 
    } break;
case 38:
# line 381 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				cinh_featob_create(NULL,yypvt[-0].val)); } break;
case 39:
# line 385 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				cinh_featob_create(NULL,yypvt[-0].val)); } break;
case 40:
# line 390 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 41:
# line 393 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-6].ind),
			       yypvt[-3].val),
	 typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val)); } break;
case 42:
# line 402 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-6].ind),
			       yypvt[-3].val),
	 typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val)); } break;
case 43:
# line 411 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-5].ind),
			       yypvt[-1].val),
	  typespec_args_exprob_create(NULL,
					 yypvt[-1].val,
					 NEW_IND,
					 yypvt[-0].val)); } break;
case 44:
# line 421 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-5].ind),
			       yypvt[-1].val),
	  typespec_args_exprob_create(NULL,
					 yypvt[-1].val,
					 NEW_IND,
					 yypvt[-0].val)); } break;
case 45:
# line 431 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 46:
# line 434 "ssather.y"
{ yyval.val = (ptr)attr_decl_featob_create_lst
	          (NULL,
		   any_declob_create(NULL,yypvt[-0].val,
		      (ptr)simple_typeob_create(NULL,UNDEF_TYPE_IND)
				  ),
		   NULL); } break;
case 47:
# line 442 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 48:
# line 445 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				yypvt[-0].val); } break;
case 49:
# line 450 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(lst_featob_create(NULL,1),yypvt[-0].val); } break;
case 50:
# line 452 "ssather.y"
{ yyval.val = (ptr)lst_featob_push(yypvt[-2].val,yypvt[-0].val); } break;
case 51:
# line 456 "ssather.y"
{ yyval.val = (ptr)alias_featob_create(NULL,yypvt[-2].ind,yypvt[-0].ind,yypvt[-1].lnno); } break;
case 52:
# line 460 "ssather.y"
{ yyval.val = (ptr) yypvt[-0].val; } break;
case 53:
# line 462 "ssather.y"
{ yyval.val = (ptr)dispatch_typeob_create(NULL,yypvt[-0].val); } break;
case 54:
# line 464 "ssather.y"
{ yyval.val = (ptr)dispatch_typeob_create(NULL,yypvt[-0].val); } break;
case 55:
# line 468 "ssather.y"
{ yyval.val = (ptr)simple_typeob_create(NULL,yypvt[-0].ind); } break;
case 56:
# line 470 "ssather.y"
{ yyval.val = (ptr)param_typeob_create(NULL,yypvt[-3].ind,yypvt[-1].val); } break;
case 57:
# line 473 "ssather.y"
{ yyval.val = (ptr)simple_typeob_create(NULL,yypvt[-3].ind); } break;
case 58:
# line 477 "ssather.y"
{ yyval.val = (ptr)lst_typeob_push(lst_typeob_create(NULL,5),yypvt[-0].val); } break;
case 59:
# line 479 "ssather.y"
{ yyval.val = (ptr)lst_typeob_push(yypvt[-2].val,yypvt[-0].val); } break;
case 60:
# line 484 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 61:
# line 487 "ssather.y"
{ yyval.val = (ptr)any_declob_create(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 62:
# line 491 "ssather.y"
{ yyval.val = (ptr)any_declob_create(NULL,
			       lst_int_push(lst_int_create(NULL,1),yypvt[-2].ind),
			       yypvt[-0].val); } break;
case 63:
# line 495 "ssather.y"
{ yyval.val = (ptr)any_declob_create(NULL,
			       lst_int_push(lst_int_create(NULL,1),yypvt[-2].ind),
			       yypvt[-0].val); } break;
case 64:
# line 508 "ssather.y"
{ yyval.val = (ptr)lst_int_push(yypvt[-2].val,yypvt[-0].ind); } break;
case 65:
# line 512 "ssather.y"
{ yyval.val = (ptr)shared_decl_featob_create_lst(NULL,yypvt[-0].val,NULL); } break;
case 66:
# line 514 "ssather.y"
{ yyval.val = (ptr)shared_decl_featob_create_lst(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 67:
# line 518 "ssather.y"
{ yyval.val = (ptr)param_declob_create_lst(NULL,yypvt[-0].val); } break;
case 68:
# line 520 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 69:
# line 522 "ssather.y"
{ yyval.val = (ptr)lst_declob_append(yypvt[-2].val,param_declob_create_lst(NULL,yypvt[-0].val)); } break;
case 70:
# line 525 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 71:
# line 527 "ssather.y"
{ yyval.val = (ptr)lst_declob_append(yypvt[-2].val,param_declob_create_lst(NULL,yypvt[-0].val)); } break;
case 72:
# line 531 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-6].ind,yypvt[-5].val,NULL,NULL,yypvt[-2].val,yypvt[-3].lnno,yypvt[-1].lnno); } break;
case 73:
# line 534 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-9].ind,yypvt[-5].val,yypvt[-7].val,NULL,yypvt[-2].val,yypvt[-3].lnno,yypvt[-1].lnno); } break;
case 74:
# line 537 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,
				any_declob_ith_name(yypvt[-6].val,0),
				yypvt[-5].val,
				NULL,
				any_declob_get_type_spec(yypvt[-6].val),
				yypvt[-2].val,
				yypvt[-3].lnno,yypvt[-1].lnno); } break;
case 75:
# line 547 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-11].ind,yypvt[-5].val,yypvt[-9].val,yypvt[-6].val,yypvt[-2].val,yypvt[-3].lnno,yypvt[-1].lnno); } break;
case 76:
# line 552 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-1].ind,yypvt[-0].val,NULL,NULL,
				   lst_stmtob_create(NULL,1),NULL,NULL); } break;
case 77:
# line 556 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-4].ind,yypvt[-0].val,yypvt[-2].val,NULL,
				   lst_stmtob_create(NULL,1),NULL,NULL); } break;
case 78:
# line 560 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,
				any_declob_ith_name(yypvt[-1].val,0),
				yypvt[-0].val,
				NULL,
				any_declob_get_type_spec(yypvt[-1].val),
				lst_stmtob_create(NULL,1),NULL,NULL); } break;
case 79:
# line 568 "ssather.y"
{ yyval.val = (ptr)rout_featob_create(NULL,yypvt[-6].ind,yypvt[-0].val,yypvt[-4].val,yypvt[-1].val,
				   lst_stmtob_create(NULL,1),NULL,NULL); } break;
case 80:
# line 574 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 81:
# line 577 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 82:
# line 580 "ssather.y"
{ yyval.val = (ptr)yypvt[-3].val; } break;
case 83:
# line 585 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 84:
# line 588 "ssather.y"
{ yyval.val = (ptr) (ptr)id_exprob_create(NULL, CONSTANT_IND); } break;
case 85:
# line 590 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 86:
# line 593 "ssather.y"
{ yyval.val = (ptr)yypvt[-3].val; } break;
case 87:
# line 598 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,NULL,NULL,NULL); } break;
case 88:
# line 601 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,yypvt[-0].val,NULL,NULL); } break;
case 89:
# line 604 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,NULL,NULL,yypvt[-0].val); } break;
case 90:
# line 607 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,yypvt[-1].val,NULL,yypvt[-0].val); } break;
case 91:
# line 610 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,yypvt[-3].val,yypvt[-0].val,yypvt[-2].val); } break;
case 92:
# line 613 "ssather.y"
{ yyval.val = (ptr)rout_specob_create(NULL,NULL,yypvt[-0].val,yypvt[-2].val); } break;
case 93:
# line 618 "ssather.y"
{ yyval.val = (ptr)const_decl_featob_create_lst(NULL,yypvt[-2].val,yypvt[-0].val);
      lst_featob_mark_shared(yyval.val); } break;
case 94:
# line 622 "ssather.y"
{ yyval.val = (ptr)const_decl_featob_create_lst(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 95:
# line 627 "ssather.y"
{ yyval.val = (ptr)except_stmtob_create(NULL,yypvt[-0].val,NULL,NULL,yypvt[-1].lnno)	; } break;
case 96:
# line 629 "ssather.y"
{ yyval.val = (ptr)except_stmtob_create(NULL,
				     lst_stmtob_push(lst_stmtob_create(NULL,1),yypvt[-5].val),
				     yypvt[-0].val,
		     (ptr)any_declob_create(NULL,
                           lst_int_push(lst_int_create(NULL,1),EXCEPTION_IND), 
                           yypvt[-2].val),
		     yypvt[-3].lnno); } break;
case 97:
# line 640 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_push(lst_stmtob_create(NULL,5),yypvt[-0].val); } break;
case 98:
# line 642 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create_lst(NULL,yypvt[-0].val); } break;
case 99:
# line 644 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 100:
# line 646 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_push(yypvt[-2].val,yypvt[-0].val); } break;
case 101:
# line 648 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_append(yypvt[-2].val,local_decl_stmtob_create_lst(NULL,yypvt[-0].val)); } break;
case 102:
# line 651 "ssather.y"
{ yyval.val = yypvt[-3].val; } break;
case 103:
# line 653 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_create(NULL,5); } break;
case 104:
# line 662 "ssather.y"
{ yyval.val = (ptr)call_stmtob_create(NULL,
				id_exprob_create(NULL,
					      yypvt[-0].ind)); } break;
case 105:
# line 666 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 106:
# line 668 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 107:
# line 670 "ssather.y"
{ yyval.val = (ptr) cond_stmtob_create(NULL,yypvt[-1].val, /* then */
				   lst_stmtob_push(lst_stmtob_create(NULL,1),
				   		   break_stmtob_create(NULL,NULL) ), 
				   lst_stmtob_create(NULL,1), /* empty elsif */
				   NULL,           /* empty else */
				   yypvt[-3].lnno              /* line */
				   );			   
   } break;
case 108:
# line 680 "ssather.y"
{ yyval.val = (ptr) cond_stmtob_create(NULL, 
				   op_exprob_create_unary(NULL,NOT_OP_IND,yypvt[-1].val),
				   /* then */
				   lst_stmtob_push(lst_stmtob_create(NULL,1),
				   		   break_stmtob_create(NULL,NULL) ), 
				   lst_stmtob_create(NULL,1), /* empty elsif */
				   NULL,           /* empty else */
				   yypvt[-3].lnno              /* line */
				   );			   
   } break;
case 109:
# line 692 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 110:
# line 694 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 111:
# line 696 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 112:
# line 700 "ssather.y"
{ yyval.val = (ptr)break_stmtob_create(NULL,NULL); } break;
case 113:
# line 706 "ssather.y"
{ yyval.val = (ptr)break_stmtob_create(NULL,yypvt[-0].val); } break;
case 114:
# line 712 "ssather.y"
{ yyval.val = (ptr)return_stmtob_create(NULL); } break;
case 115:
# line 714 "ssather.y"
{ yyval.val = (ptr)call_stmtob_create(NULL,yypvt[-0].val); } break;
case 116:
# line 716 "ssather.y"
{ yyval.val = yypvt[-0].val; } break;
case 117:
# line 724 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create(NULL,yypvt[-0].val,NULL); } break;
case 118:
# line 726 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 119:
# line 728 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-6].ind),
			       yypvt[-3].val),
	 typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val)); } break;
case 120:
# line 737 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-6].ind),
			       yypvt[-3].val),
	 typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val)); } break;
case 121:
# line 746 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-5].ind),
			       yypvt[-1].val),
	  typespec_args_exprob_create(NULL,
					 yypvt[-1].val,
					 NEW_IND,
					 yypvt[-0].val)); } break;
case 122:
# line 755 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),yypvt[-5].ind),
			       yypvt[-1].val),
	  typespec_args_exprob_create(NULL,
					 yypvt[-1].val,
					 NEW_IND,
					 yypvt[-0].val)); } break;
case 123:
# line 767 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create(NULL,yypvt[-2].val,yypvt[-0].val); } break;
case 124:
# line 772 "ssather.y"
{ yyval.val = (ptr)local_decl_stmtob_create_lst(NULL,yypvt[-0].val); } break;
case 125:
# line 774 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_append(yypvt[-2].val,local_decl_stmtob_create_lst(NULL,yypvt[-0].val)); } break;
case 126:
# line 776 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 127:
# line 778 "ssather.y"
{ yyval.val = yypvt[-3].val; } break;
case 128:
# line 782 "ssather.y"
{ yyval.val = (ptr)assign_stmtob_create(NULL,yypvt[-3].val,yypvt[-0].val,yypvt[-2].lnno); } break;
case 129:
# line 788 "ssather.y"
{ yyval.val = (ptr)cond_stmtob_create(NULL,yypvt[-5].val,yypvt[-3].val,yypvt[-2].val,yypvt[-1].val,yypvt[-6].lnno); } break;
case 130:
# line 792 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_create(NULL,5); } break;
case 131:
# line 794 "ssather.y"
{ yyval.val = (ptr)lst_stmtob_push(yypvt[-5].val,elsif_stmtob_create(NULL,yypvt[-2].val,yypvt[-0].val,yypvt[-3].lnno)); } break;
case 132:
# line 799 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 133:
# line 801 "ssather.y"
{ yyval.val = (ptr)NULL; } break;
case 134:
# line 804 "ssather.y"
{ yyval.val = (ptr)loop_stmtob_create(NULL,NULL,yypvt[-1].val,yypvt[-2].lnno); } break;
case 135:
# line 808 "ssather.y"
{ yyval.val = (ptr)switch_stmtob_create(NULL,yypvt[-3].val,yypvt[-2].val,yypvt[-1].val,yypvt[-4].lnno); } break;
case 136:
# line 813 "ssather.y"
{ yyval.val = (ptr)lst_when_stmtob_push(yypvt[-4].val,when_stmtob_create(NULL,yypvt[-2].val,yypvt[-0].val)); } break;
case 137:
# line 815 "ssather.y"
{ yyval.val = (ptr)lst_when_stmtob_create(NULL, 5);} break;
case 138:
# line 819 "ssather.y"
{ yyval.val = (ptr)assert_stmtob_create(NULL,yypvt[-0].val,yypvt[-1].lnno); } break;
case 139:
# line 824 "ssather.y"
{ yyval.val = (ptr)id_args_exprob_create(NULL,
				   yypvt[-3].ind,
				   yypvt[-1].val); } break;
case 140:
# line 835 "ssather.y"
{ yyval.val = (ptr)expr_args_exprob_create(NULL,
				     yypvt[-3].val,
				     yypvt[-1].ind,
				     yypvt[-0].val); } break;
case 141:
# line 841 "ssather.y"
{ yyval.val = (ptr)typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val); } break;
case 142:
# line 847 "ssather.y"
{ yyval.val = (ptr)typespec_args_exprob_create(NULL,
					 yypvt[-3].val,
					 yypvt[-1].ind,
					 yypvt[-0].val); } break;
case 143:
# line 853 "ssather.y"
{ yyval.val = (ptr)typespec_args_exprob_create(NULL,
					 yypvt[-1].val,
					 NEW_IND,
					 yypvt[-0].val); } break;
case 144:
# line 861 "ssather.y"
{ yyval.val = (ptr)yypvt[-1].val; } break;
case 145:
# line 863 "ssather.y"
{ yyval.val = NULL; } break;
case 146:
# line 866 "ssather.y"
{ yyval.val = (ptr)lst_exprob_push(lst_exprob_create(NULL, -1),yypvt[-0].val); } break;
case 147:
# line 868 "ssather.y"
{ yyval.val = (ptr)lst_exprob_push(yypvt[-2].val,yypvt[-0].val); } break;
case 148:
# line 871 "ssather.y"
{ yyval.val = yypvt[-3].val; } break;
case 149:
# line 890 "ssather.y"
{ yyval.val=yypvt[-0].val; } break;
case 150:
# line 892 "ssather.y"
{ yyval.val=yypvt[-0].val; } break;
case 151:
# line 897 "ssather.y"
{ yyval.val = (ptr)id_exprob_create(NULL, yypvt[-0].ind); } break;
case 152:
# line 899 "ssather.y"
{ yyval.val = (ptr)char_const_exprob_create(NULL, yypvt[-0].val); } break;
case 153:
# line 901 "ssather.y"
{ yyval.val = (ptr)int_const_exprob_create(NULL, yypvt[-0].val); } break;
case 154:
# line 903 "ssather.y"
{ yyval.val = (ptr)real_const_exprob_create(NULL, yypvt[-0].val); } break;
case 155:
# line 905 "ssather.y"
{ yyval.val = (ptr)bool_const_exprob_create(NULL, yypvt[-0].ind); } break;
case 156:
# line 907 "ssather.y"
{ yyval.val = (ptr)str_const_exprob_create(NULL, yypvt[-0].ind); } break;
case 157:
# line 909 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 158:
# line 911 "ssather.y"
{ yyval.val = (ptr)yypvt[-0].val; } break;
case 159:
# line 913 "ssather.y"
{ yyval.val = yypvt[-1].val; } break;
case 160:
# line 916 "ssather.y"
{ yyval.val = (ptr)int_const_exprob_create(NULL, "0"); } break;
case 161:
# line 921 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_unary(NULL,NOT_OP_IND,yypvt[-0].val); } break;
case 162:
# line 923 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,LT_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 163:
# line 925 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,GT_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 164:
# line 927 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,LE_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 165:
# line 929 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,GE_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 166:
# line 931 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,EQ_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 167:
# line 933 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,NE_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 168:
# line 935 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,AND_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 169:
# line 937 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,OR_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 170:
# line 939 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_unary(NULL,UMINUS_OP_IND,yypvt[-0].val); } break;
case 171:
# line 941 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_unary(NULL,UPLUS_OP_IND,yypvt[-0].val); } break;
case 172:
# line 947 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,PLUS_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 173:
# line 949 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,MINUS_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 174:
# line 951 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,MULT_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 175:
# line 953 "ssather.y"
{ yyval.val = (ptr)op_exprob_create_binary(NULL,DIVIDE_OP_IND,yypvt[-2].val,yypvt[-0].val); } break;
case 176:
# line 957 "ssather.y"
{ yyval.val = (ptr)aref_exprob_create(NULL,yypvt[-3].val,yypvt[-1].val); } break;
case 177:
# line 959 "ssather.y"
{ yyval.val = (ptr)aref_exprob_create(NULL,NULL,yypvt[-1].val); } break;
case 178:
# line 969 "ssather.y"
{ yyval.lnno = (int)globals_curr_lineno; } break;
	}
	goto yystack;		/* reset registers in driver code */
}

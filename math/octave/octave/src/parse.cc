
/*  A Bison parser, made from parse.y with Bison version GNU Bison version 1.21
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	FOR	258
#define	WHILE	259
#define	IF	260
#define	ELSEIF	261
#define	ELSE	262
#define	FCN	263
#define	BREAK	264
#define	CONTINUE	265
#define	FUNC_RET	266
#define	SCREW_TWO	267
#define	END_OF_INPUT	268
#define	GLOBAL	269
#define	CLEAR	270
#define	USING	271
#define	TITLE	272
#define	WITH	273
#define	COLON	274
#define	OPEN_BRACE	275
#define	CLOSE_BRACE	276
#define	NUM	277
#define	IMAG_NUM	278
#define	NAME	279
#define	SCREW	280
#define	TEXT	281
#define	STYLE	282
#define	END	283
#define	PLOT	284
#define	EXPR_AND	285
#define	EXPR_OR	286
#define	EXPR_LT	287
#define	EXPR_LE	288
#define	EXPR_EQ	289
#define	EXPR_NE	290
#define	EXPR_GE	291
#define	EXPR_GT	292
#define	LEFTDIV	293
#define	EMUL	294
#define	EDIV	295
#define	ELEFTDIV	296
#define	QUOTE	297
#define	TRANSPOSE	298
#define	UNARY	299
#define	PLUS_PLUS	300
#define	MINUS_MINUS	301
#define	EXPR_NOT	302
#define	POW	303
#define	EPOW	304

#line 28 "parse.y"

#define YYDEBUG 1

#include "SLStack.h"

#include "Matrix.h"

#include "error.h"
#include "variables.h"
#include "user-prefs.h"
#include "input.h"
#include "utils.h"
#include "tree.h"
#include "symtab.h"
#include "builtins.h"
#include "octave.h"
#include "parse.h"
#include "lex.h"

// Identifier to define if we are reading an M-fie.
tree_identifier *id_to_define;

// Nonzero means we're in the middle of defining a function.
int defining_func = 0;

// Nonzero means we're in the middle of defining a loop.
int looping = 0;

// Nonzero means we're in the middle of defining a conditional expression.
int iffing = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed_again = 0;

// Temporary symbol table pointer used to cope with bogus function syntax.
symbol_table *tmp_local_sym_tab = (symbol_table *) NULL;

// Stack to hold list of literal matrices.
SLStack <tree_matrix *> ml;

// A nonzero element corresponding to an element of ml means we just
// started reading a new matrix.  This should probably be part of a
// new struct for matrix lists... 
SLStack <int> mlnm;

// The current input line number.
int input_line_number = 0;

// The column of the current token.
int current_input_column = 0;

// Buffer for help text snagged from M-files.
// Probably shouldn't be a fixed size...
char help_buf [HELP_BUF_LENGTH];

// Nonzero means we're working on a plot command.
int plotting = 0;

// Nonzero means we're looking at the range part of a plot command.
int in_plot_range = 0;

// Nonzero means we're looking at the using part of a plot command.
int in_plot_using = 0;

// Nonzero means we're looking at the style part of a plot command.
int in_plot_style = 0;

// The type of an END token.  This declaration is repeated in lex.l.
enum end_tok_type
  {
    simple_end,
    for_end,
    function_end,
    if_end,
    while_end,
  };

// The type of a PLOT token.  This declaration is repeated in lex.l.
enum plot_tok_type
  {
    two_dee = 2,
    three_dee = 3,
  };

// Error mesages for mismatched end statements.
static void end_error (char *type, end_tok_type ettype);

// Generic error messages.
static void yyerror (char *s);

static tree *maybe_convert_to_ans_assign (tree *expr);
static void maybe_warn_assign_as_truth_value (tree *expr);

#define ABORT_PARSE \
  do \
    { \
      global_command = NULL_TREE; \
      reset_parser (); \
      yyerrok; \
      if (interactive) \
	YYACCEPT; \
      else \
	YYABORT; \
    } \
  while (0)


#line 144 "parse.y"
typedef union
{
  tree *tree_type;
  tree_constant *tree_constant_type;
  tree_matrix *tree_matrix_type;
  tree_identifier *tree_identifier_type;
  tree_function *tree_function_type;
  tree_index_expression *tree_index_expression_type;
  tree_colon_expression *tree_colon_expression_type;
  tree_argument_list *tree_argument_list_type;
  tree_parameter_list *tree_parameter_list_type;
  tree_word_list *tree_word_list_type;
  tree_command *tree_command_type;
  tree_if_command *tree_if_command_type;
  tree_command_list *tree_command_list_type;
  tree_word_list_command *tree_word_list_command_type;
  tree_plot_command *tree_plot_command_type;
  tree_subplot_list *tree_subplot_list_type;
  tree_plot_limits *tree_plot_limits_type;
  tree_plot_range *tree_plot_range_type;
  tree_subplot_using *tree_subplot_using_type;
  tree_subplot_style *tree_subplot_style_type;
  symbol_record *sym_rec;
  double number;
  char *string;
  end_tok_type ettype;
  plot_tok_type pttype;
} YYSTYPE;

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

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		311
#define	YYFLAG		-32768
#define	YYNTBASE	63

#define YYTRANSLATE(x) ((unsigned)(x) <= 304 ? yytranslate[x] : 120)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    32,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    60,
    61,    45,    44,    31,    43,     2,    46,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    42,    30,     2,
    33,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    59,     2,    62,     2,     2,     2,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    34,    35,    36,    37,    38,    39,
    40,    41,    47,    48,    49,    50,    51,    52,    53,    54,
    55,    56,    57,    58
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,    10,    12,    15,    18,    21,    25,
    27,    29,    31,    34,    37,    39,    42,    45,    49,    53,
    55,    58,    61,    63,    66,    69,    71,    73,    76,    78,
    81,    82,    84,    86,    89,    92,    94,    98,   102,   104,
   106,   108,   110,   112,   115,   119,   121,   124,   128,   134,
   139,   144,   148,   151,   153,   157,   159,   162,   164,   166,
   168,   171,   174,   177,   180,   183,   186,   190,   194,   198,
   202,   206,   210,   212,   215,   218,   222,   225,   228,   232,
   238,   239,   241,   244,   246,   250,   254,   260,   261,   263,
   269,   277,   283,   292,   299,   309,   311,   313,   315,   321,
   328,   329,   331,   333,   335,   337,   340,   343,   346,   347,
   351,   358,   362,   364,   366,   369,   372,   375,   378,   382,
   386,   390,   394,   398,   402,   406,   410,   414,   418,   422,
   426,   430,   434,   438,   442,   446,   450,   452,   454,   456,
   458,   462,   464,   466,   468,   471,   474,   477,   480,   483,
   487,   491,   494,   496,   499,   501,   503,   506,   507,   508,
   509,   510,   515,   520,   526,   532,   537,   541,   546,   551,
   557,   561,   563,   565,   567,   572,   576,   579,   582,   585,
   589,   592,   596,   598,   600,   602,   606,   608,   612,   615,
   619,   624,   626,   629,   633,   635,   638
};

static const short yyrhs[] = {    32,
     0,    13,     0,    64,    32,     0,    64,    13,     0,     1,
     0,     1,    32,     0,     1,    13,     0,    64,     1,     0,
    64,     1,    13,     0,    66,     0,    67,     0,    65,     0,
    65,    66,     0,    65,    67,     0,    73,     0,    66,    73,
     0,    67,    73,     0,    65,    66,    73,     0,    65,    67,
    73,     0,    30,     0,    66,    31,     0,    66,    30,     0,
    31,     0,    67,    30,     0,    67,    31,     0,    31,     0,
    32,     0,    68,    92,     0,    30,     0,    69,    92,     0,
     0,    71,     0,    72,     0,    72,    68,     0,    72,    69,
     0,    73,     0,    72,    68,    73,     0,    72,    69,    73,
     0,    74,     0,    89,     0,    85,     0,   105,     0,    86,
     0,    29,    77,     0,    29,    75,    77,     0,    76,     0,
    76,    76,     0,    76,    76,    76,     0,    20,    94,    19,
    94,    21,     0,    20,    19,    94,    21,     0,    20,    94,
    19,    21,     0,    20,    19,    21,     0,    20,    21,     0,
    78,     0,    77,    31,    78,     0,    94,     0,    94,    79,
     0,    80,     0,    82,     0,    83,     0,    80,    82,     0,
    82,    80,     0,    80,    83,     0,    83,    80,     0,    82,
    83,     0,    83,    82,     0,    80,    82,    83,     0,    80,
    83,    82,     0,    82,    80,    83,     0,    82,    83,    80,
     0,    83,    80,    82,     0,    83,    82,    80,     0,    81,
     0,    81,    94,     0,    16,    94,     0,    81,    19,    94,
     0,    17,    94,     0,    18,    27,     0,    18,    27,    94,
     0,    18,    27,    94,    84,    94,     0,     0,    94,     0,
    14,    87,     0,    24,     0,    24,    33,    94,     0,    87,
    88,    24,     0,    87,    88,    24,    33,    94,     0,     0,
    31,     0,     4,    94,    91,    70,    28,     0,     3,   114,
    33,    94,    91,    70,    28,     0,     5,    94,    91,    70,
    28,     0,     5,    94,    91,    70,     7,    91,    70,    28,
     0,     5,    94,    91,    70,    90,    28,     0,     5,    94,
    91,    70,    90,     7,    91,    70,    28,     0,     9,     0,
    10,     0,    11,     0,     6,    91,    94,    91,    70,     0,
    90,     6,    91,    94,    91,    70,     0,     0,    92,     0,
    31,     0,    30,     0,    32,     0,    92,    31,     0,    92,
    30,     0,    92,    32,     0,     0,   111,    33,    94,     0,
    59,    93,   119,    12,    33,    94,     0,    22,    33,    94,
     0,    95,     0,    96,     0,   114,    54,     0,   114,    55,
     0,    95,    51,     0,    95,    52,     0,    95,    57,    95,
     0,    95,    58,    95,     0,    95,    44,    95,     0,    95,
    43,    95,     0,    95,    45,    95,     0,    95,    46,    95,
     0,    95,    48,    95,     0,    95,    49,    95,     0,    95,
    47,    95,     0,    95,    50,    95,     0,    95,    36,    95,
     0,    95,    37,    95,     0,    95,    38,    95,     0,    95,
    40,    95,     0,    95,    41,    95,     0,    95,    39,    95,
     0,    95,    34,    95,     0,    95,    35,    95,     0,    22,
     0,    23,     0,    26,     0,    98,     0,    60,    94,    61,
     0,   111,     0,   117,     0,    97,     0,    54,   114,     0,
    55,   114,     0,    56,    95,     0,    44,    95,     0,    43,
    95,     0,    95,    42,    95,     0,    97,    42,    95,     0,
   114,    99,     0,    15,     0,    15,    99,     0,   100,     0,
    26,     0,   100,    26,     0,     0,     0,     0,     0,     8,
   101,   104,   106,     0,     8,   101,   104,   108,     0,    25,
   103,   101,    33,   108,     0,   107,    62,   101,    33,   108,
     0,    59,   103,   102,   114,     0,   107,    31,   114,     0,
   114,   103,   102,   109,     0,   112,    91,    70,   110,     0,
    60,    61,    91,    70,   110,     0,    91,    70,   110,     0,
    28,     0,    13,     0,   114,     0,   114,    60,   115,    61,
     0,   114,    60,    61,     0,   114,    59,     0,   113,    61,
     0,    60,   114,     0,   113,    31,   114,     0,    60,     1,
     0,   113,    31,     1,     0,    24,     0,   116,     0,    42,
     0,   116,    31,    42,     0,    94,     0,   116,    31,    94,
     0,    59,    62,     0,    59,    30,    62,     0,    59,    93,
   118,    62,     0,   119,     0,   118,    30,     0,   118,    30,
   119,     0,    94,     0,   119,    31,     0,   119,    31,    94,
     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   269,   275,   281,   287,   293,   295,   297,   299,   301,   305,
   307,   309,   311,   316,   320,   322,   324,   326,   331,   335,
   336,   337,   340,   341,   342,   345,   346,   347,   350,   351,
   354,   356,   359,   361,   363,   370,   372,   374,   381,   383,
   385,   387,   389,   393,   402,   413,   415,   417,   421,   423,
   425,   427,   429,   433,   435,   439,   441,   445,   447,   449,
   451,   453,   455,   457,   459,   461,   463,   465,   467,   469,
   471,   473,   477,   482,   489,   494,   498,   502,   504,   506,
   510,   513,   517,   521,   523,   531,   533,   543,   544,   552,
   564,   575,   587,   600,   615,   633,   644,   655,   667,   672,
   679,   680,   683,   684,   685,   686,   687,   688,   691,   695,
   697,   720,   727,   731,   733,   735,   737,   739,   741,   743,
   745,   747,   749,   751,   753,   755,   757,   759,   762,   764,
   766,   768,   770,   772,   774,   776,   780,   782,   784,   786,
   788,   794,   796,   798,   800,   802,   804,   806,   808,   812,
   814,   825,   827,   841,   856,   860,   862,   868,   872,   876,
   880,   884,   890,   898,   906,   914,   916,   920,   954,   959,
   961,   965,   977,   984,   986,   988,   993,  1003,  1010,  1012,
  1014,  1019,  1026,  1029,  1033,  1039,  1050,  1052,  1063,  1068,
  1073,  1082,  1083,  1084,  1087,  1103,  1104
};

static const char * const yytname[] = {   "$","error","$illegal.","FOR","WHILE",
"IF","ELSEIF","ELSE","FCN","BREAK","CONTINUE","FUNC_RET","SCREW_TWO","END_OF_INPUT",
"GLOBAL","CLEAR","USING","TITLE","WITH","COLON","OPEN_BRACE","CLOSE_BRACE","NUM",
"IMAG_NUM","NAME","SCREW","TEXT","STYLE","END","PLOT","';'","','","'\\n'","'='",
"EXPR_AND","EXPR_OR","EXPR_LT","EXPR_LE","EXPR_EQ","EXPR_NE","EXPR_GE","EXPR_GT",
"':'","'-'","'+'","'*'","'/'","LEFTDIV","EMUL","EDIV","ELEFTDIV","QUOTE","TRANSPOSE",
"UNARY","PLUS_PLUS","MINUS_MINUS","EXPR_NOT","POW","EPOW","'['","'('","')'",
"']'","input","simple_list","simple_list1","semi_comma","comma_semi","comma_nl_sep",
"semi_sep","opt_list","list","list1","command","plot_command","ranges","ranges1",
"plot_command1","plot_command2","plot_options","using","using1","title","style",
"bogus_syntax","ans_expression","global_decl","global_decl1","optcomma","statement",
"elseif","optsep","sep","screwed_again","expression","simple_expr","simple_expr1",
"colon_expr","word_list_cmd","word_list","word_list1","g_symtab","local_symtab",
"safe","are_we_screwed","func_def","func_def1","func_def1a","func_def2","func_def3",
"fcn_end_or_eof","variable","param_list","param_list1","identifier","arg_list",
"arg_list1","matrix","rows","matrix_row",""
};
#endif

static const short yyr1[] = {     0,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    64,
    64,    64,    64,    64,    65,    65,    65,    65,    65,    66,
    66,    66,    67,    67,    67,    68,    68,    68,    69,    69,
    70,    70,    71,    71,    71,    72,    72,    72,    73,    73,
    73,    73,    73,    74,    74,    75,    75,    75,    76,    76,
    76,    76,    76,    77,    77,    78,    78,    79,    79,    79,
    79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
    79,    79,    80,    80,    81,    81,    82,    83,    83,    83,
    84,    85,    86,    87,    87,    87,    87,    88,    88,    89,
    89,    89,    89,    89,    89,    89,    89,    89,    90,    90,
    91,    91,    92,    92,    92,    92,    92,    92,    93,    94,
    94,    94,    94,    95,    95,    95,    95,    95,    95,    95,
    95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
    95,    95,    95,    95,    95,    95,    96,    96,    96,    96,
    96,    96,    96,    96,    96,    96,    96,    96,    96,    97,
    97,    98,    98,    98,    99,   100,   100,   101,   102,   103,
   104,   105,   105,   106,   106,   107,   107,   108,   109,   109,
   109,   110,   110,   111,   111,   111,   111,   112,   113,   113,
   113,   113,   114,   115,   116,   116,   116,   116,   117,   117,
   117,   118,   118,   118,   119,   119,   119
};

static const short yyr2[] = {     0,
     1,     1,     2,     2,     1,     2,     2,     2,     3,     1,
     1,     1,     2,     2,     1,     2,     2,     3,     3,     1,
     2,     2,     1,     2,     2,     1,     1,     2,     1,     2,
     0,     1,     1,     2,     2,     1,     3,     3,     1,     1,
     1,     1,     1,     2,     3,     1,     2,     3,     5,     4,
     4,     3,     2,     1,     3,     1,     2,     1,     1,     1,
     2,     2,     2,     2,     2,     2,     3,     3,     3,     3,
     3,     3,     1,     2,     2,     3,     2,     2,     3,     5,
     0,     1,     2,     1,     3,     3,     5,     0,     1,     5,
     7,     5,     8,     6,     9,     1,     1,     1,     5,     6,
     0,     1,     1,     1,     1,     2,     2,     2,     0,     3,
     6,     3,     1,     1,     2,     2,     2,     2,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     1,     1,     1,     1,
     3,     1,     1,     1,     2,     2,     2,     2,     2,     3,
     3,     2,     1,     2,     1,     1,     2,     0,     0,     0,
     0,     4,     4,     5,     5,     4,     3,     4,     4,     5,
     3,     1,     1,     1,     4,     3,     2,     2,     2,     3,
     2,     3,     1,     1,     1,     3,     1,     3,     2,     3,
     4,     1,     2,     3,     1,     2,     3
};

static const short yydefact[] = {     0,
     5,     0,     0,     0,   158,    96,    97,    98,     2,     0,
   153,   137,   138,   183,   139,     0,    20,    23,     1,     0,
     0,     0,     0,     0,   109,     0,     0,    12,    10,    11,
    15,    39,    41,    43,    40,    82,   113,   114,   144,   140,
    42,   142,   174,   143,     7,     6,     0,   101,   101,   161,
    84,    83,   156,   154,   155,     0,     0,     0,    46,    44,
    54,    56,   137,   109,   149,   142,   148,   145,   146,   147,
     0,   189,     0,     0,     8,     4,     3,    13,    14,    22,
    21,    16,    24,    25,    17,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   117,   118,     0,     0,     0,     0,   115,   116,
   177,     0,   152,     0,   104,   103,   105,    31,   102,    31,
     0,     0,    89,     0,   157,   112,     0,    53,     0,    45,
    47,     0,     0,     0,     0,    57,    58,    73,    59,    60,
     0,   190,   195,     0,   192,   141,     9,    18,    19,   135,
   136,   129,   130,   131,   134,   132,   133,   150,   122,   121,
   123,   124,   127,   125,   126,   128,   119,   120,   151,   110,
   185,   176,   187,     0,   184,   101,     0,    32,    33,    36,
   107,   106,   108,     0,   160,   160,   162,     0,   163,   160,
    85,    86,    52,     0,     0,    48,    55,    75,    77,    78,
    61,    63,     0,    74,    62,    65,    64,    66,   192,   193,
   191,     0,   196,   175,     0,    31,    90,    29,    26,    27,
    34,    35,   101,   101,    92,     0,   158,   159,     0,   158,
   159,     0,    50,    51,     0,    81,    67,    68,    76,    69,
    70,    71,    72,   194,     0,   197,   186,   188,     0,    37,
    28,    38,    30,     0,    31,   101,   101,    94,     0,     0,
   167,     0,   101,    87,    49,     0,   111,    91,   101,     0,
     0,    31,     0,   166,     0,     0,    31,   168,   101,     0,
    80,    31,    93,   101,     0,   164,   165,   181,   101,   179,
     0,    31,     0,   178,    99,    31,    95,    31,   173,   172,
   171,     0,   182,   180,   100,     0,   169,   170,     0,     0,
     0
};

static const short yydefgoto[] = {   309,
    27,    28,    29,    30,   221,   222,   177,   178,   179,   180,
    32,    58,    59,    60,    61,   136,   137,   138,   139,   140,
   266,    33,    34,    52,   124,    35,   226,   118,   119,    73,
    36,    37,    38,    39,    40,    54,    55,    50,   260,   227,
   121,    41,   187,   188,   189,   278,   301,    42,   279,   280,
    43,   174,   175,    44,   144,   145
};

static const short yypact[] = {   338,
    13,    -5,   738,   738,-32768,-32768,-32768,-32768,-32768,     8,
    11,    51,-32768,-32768,-32768,   621,-32768,-32768,-32768,   761,
   761,    -5,    -5,   761,   -13,   738,    22,    65,   454,   512,
-32768,-32768,-32768,-32768,-32768,-32768,   788,-32768,    49,-32768,
-32768,    79,   -16,-32768,-32768,-32768,    87,    68,    68,-32768,
    89,     5,-32768,-32768,   104,   738,   596,   738,   114,   110,
-32768,    85,-32768,   -13,    60,-32768,    60,-32768,-32768,    60,
    80,-32768,   738,    86,   133,-32768,-32768,   454,   512,-32768,
-32768,-32768,-32768,-32768,-32768,   761,   761,   761,   761,   761,
   761,   761,   761,   761,   761,   761,   761,   761,   761,   761,
   761,   761,-32768,-32768,   761,   761,   761,   738,-32768,-32768,
-32768,   101,-32768,   738,-32768,-32768,-32768,   570,    74,   570,
   -11,   738,-32768,   124,-32768,-32768,   645,-32768,   130,   110,
   114,   738,   738,   738,   123,-32768,    97,   668,    57,   112,
   738,-32768,-32768,   -12,    48,-32768,-32768,-32768,-32768,   811,
   811,   235,   235,   235,   235,   235,   235,   328,   384,   384,
    36,    36,    36,    36,    36,    36,    60,    60,   328,-32768,
-32768,-32768,-32768,    90,   121,    68,   126,-32768,    78,-32768,
-32768,-32768,-32768,    40,-32768,-32768,-32768,    -4,-32768,-32768,
-32768,   134,-32768,   145,   692,-32768,-32768,-32768,-32768,   738,
   150,   152,   738,-32768,   150,   154,   152,   154,   141,   738,
-32768,   143,   738,-32768,   715,   570,-32768,-32768,-32768,-32768,
   396,   396,    68,    68,-32768,    46,-32768,-32768,    -5,-32768,
-32768,   738,-32768,-32768,   157,   158,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,   141,   738,-32768,-32768,-32768,   153,-32768,
    74,-32768,    74,   738,   570,    68,    68,-32768,   147,    -5,
-32768,   151,    10,-32768,-32768,   738,-32768,-32768,    68,   159,
   738,   570,    -5,-32768,    -5,     6,   570,-32768,    68,     0,
-32768,   570,-32768,    68,   163,-32768,-32768,-32768,    68,-32768,
    53,   570,    27,-32768,-32768,   570,-32768,   570,-32768,-32768,
-32768,    53,-32768,-32768,-32768,    53,-32768,-32768,   185,   192,
-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,   165,   167,-32768,-32768,  -119,-32768,-32768,     4,
-32768,-32768,   -54,   138,    66,-32768,  -128,-32768,  -131,  -115,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -41,   -83,   136,
    -1,   209,-32768,-32768,-32768,   160,-32768,  -138,   -34,  -127,
-32768,-32768,-32768,-32768,  -211,-32768,  -237,   231,-32768,-32768,
    -2,-32768,-32768,-32768,-32768,  -125
};


#define	YYLAST		869


static const short yytable[] = {    47,
   184,    48,    49,    31,   131,   201,   288,   120,   208,    53,
   205,   207,    14,   185,    62,   209,    71,   210,    14,    68,
    69,   202,    75,   206,    74,    45,   229,   303,   -88,    14,
   293,    51,    82,    85,    76,   123,    53,   109,   110,   115,
   116,   117,   111,   112,    46,   223,   224,   186,    72,   211,
    14,   256,   257,    77,   126,   129,    62,   230,   228,   212,
   294,   286,   231,   287,   307,   299,   289,   225,   308,   276,
   238,   143,   133,   258,   135,   242,   196,   241,   213,   243,
   300,   148,   149,    56,   244,   237,   103,   104,   259,   240,
   107,   262,   105,   106,    17,    18,   249,   115,   116,   117,
   133,   134,   135,   181,   182,   183,   170,   218,   219,   220,
   173,   108,   176,   134,   135,    11,   105,   106,   190,   114,
   191,   122,    12,    13,    14,   194,    15,   133,   134,   125,
    62,   198,   199,    57,   216,   270,   204,   251,   253,   143,
   132,   142,   171,    20,    21,   147,   146,   192,   195,   200,
   214,   215,   285,   217,    22,    23,    24,   291,   -79,    25,
    26,   172,   295,   -79,   -79,   233,   232,   135,   134,   133,
   -79,   213,   302,   -79,   -79,   245,   305,   265,   306,   273,
   268,   254,   255,   275,   310,   -79,   283,   -79,   -79,   -79,
   297,   311,    78,   235,    79,   130,   263,   197,   236,   141,
     0,   239,   113,     0,     0,     0,     0,     0,   143,     0,
     0,   246,     0,   248,   271,   272,     0,     0,     0,     0,
     0,   277,     0,     0,   250,   252,   261,   282,    65,    67,
   264,     0,    70,     0,     0,     0,     0,   292,     0,     0,
     0,     0,   296,   267,     0,     0,     0,   298,     0,     0,
    66,    66,   269,     0,    66,     0,     0,   274,     0,     0,
     0,     0,     0,     0,   281,     0,     0,     0,     0,   284,
   190,     0,   190,   290,     0,     0,    94,    95,    96,    97,
    98,    99,   100,   101,   102,   103,   104,     0,     0,     0,
   304,   105,   106,     0,   150,   151,   152,   153,   154,   155,
   156,   157,   158,   159,   160,   161,   162,   163,   164,   165,
   166,     0,     0,   167,   168,   169,    66,    66,    66,    66,
    66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
    66,    66,    66,     0,     0,    66,    66,    66,     1,     0,
     2,     3,     4,     0,     0,     5,     6,     7,     8,     0,
     9,    10,    11,     0,     0,     0,     0,     0,     0,    12,
    13,    14,     0,    15,     0,     0,    16,    17,    18,    19,
    95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
    20,    21,     0,     0,   105,   106,     0,     0,     0,     0,
     0,    22,    23,    24,     0,     0,    25,    26,     2,     3,
     4,     0,     0,     5,     6,     7,     8,     0,     0,    10,
    11,     0,     0,     0,     0,     0,     0,    12,    13,    14,
     0,    15,     0,     0,    16,   115,   116,   117,    97,    98,
    99,   100,   101,   102,   103,   104,     0,     0,    20,    21,
   105,   106,     0,     0,     0,     0,     0,     0,     0,    22,
    23,    24,     0,     0,    25,    26,     2,     3,     4,     0,
     0,     5,     6,     7,     8,     0,     0,    10,    11,     0,
     0,     0,     0,     0,     0,    12,    13,    14,     0,    15,
     0,     0,    16,    80,    81,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    20,    21,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    22,    23,    24,
     0,     0,    25,    26,     2,     3,     4,     0,     0,     5,
     6,     7,     8,     0,     0,    10,    11,     0,     0,     0,
     0,     0,     0,    12,    13,    14,     0,    15,     0,     0,
    16,    83,    84,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    20,    21,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    22,    23,    24,     0,     0,
    25,    26,     2,     3,     4,     0,     0,     5,     6,     7,
     8,     0,     0,    10,    11,     0,     0,     0,     0,     0,
     0,    12,    13,    14,     0,    15,     0,     0,    16,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    11,     0,    20,    21,   127,     0,   128,    12,    13,    14,
     0,    15,     0,    22,    23,    24,     0,     0,    25,    26,
     0,     0,     0,     0,     0,    11,     0,     0,    20,    21,
    57,     0,    12,    13,    14,     0,    15,     0,     0,    22,
    23,    24,     0,     0,    25,    26,     0,     0,     0,    11,
     0,     0,     0,    20,    21,   193,    12,    13,    14,     0,
    15,     0,     0,     0,    22,    23,    24,     0,     0,    25,
    26,     0,    11,     0,     0,     0,   203,    20,    21,    12,
    13,    14,     0,    15,     0,     0,     0,     0,    22,    23,
    24,     0,     0,    25,    26,     0,    11,     0,     0,     0,
    20,    21,   234,    12,    13,    14,     0,    15,     0,     0,
     0,    22,    23,    24,     0,     0,    25,    26,     0,    11,
     0,     0,     0,     0,    20,    21,    12,    13,    14,     0,
    15,     0,     0,     0,     0,    22,    23,    24,     0,     0,
    25,    26,    11,     0,     0,     0,   247,    20,    21,    12,
    13,    14,     0,    15,     0,     0,     0,     0,    22,    23,
    24,     0,     0,    25,    26,    11,     0,     0,     0,     0,
    20,    21,    63,    13,    14,     0,    15,     0,     0,     0,
     0,    22,    23,    24,     0,     0,    25,    26,     0,     0,
     0,     0,     0,    20,    21,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    22,    23,    24,     0,     0,    64,
    26,    86,    87,    88,    89,    90,    91,    92,    93,    94,
    95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     0,     0,     0,     0,   105,   106,    88,    89,    90,    91,
    92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
   102,   103,   104,     0,     0,     0,     0,   105,   106
};

static const short yycheck[] = {     2,
   120,     3,     4,     0,    59,   137,     1,    49,   140,    26,
   139,   140,    24,    25,    16,   141,    30,    30,    24,    22,
    23,   137,     1,   139,    26,    13,    31,     1,    24,    24,
    31,    24,    29,    30,    13,    31,    26,    54,    55,    30,
    31,    32,    59,    60,    32,     6,     7,    59,    62,    62,
    24,     6,     7,    32,    56,    57,    58,    62,   186,    12,
    61,   273,   190,   275,   302,    13,    61,    28,   306,    60,
   202,    73,    16,    28,    18,   207,   131,   206,    31,   208,
    28,    78,    79,    33,   210,   201,    51,    52,   227,   205,
    42,   230,    57,    58,    30,    31,   216,    30,    31,    32,
    16,    17,    18,    30,    31,    32,   108,    30,    31,    32,
   112,    33,   114,    17,    18,    15,    57,    58,   121,    33,
   122,    33,    22,    23,    24,   127,    26,    16,    17,    26,
   132,   133,   134,    20,   176,   255,   138,   221,   222,   141,
    31,    62,    42,    43,    44,    13,    61,    24,    19,    27,
    61,    31,   272,    28,    54,    55,    56,   277,     1,    59,
    60,    61,   282,     6,     7,    21,    33,    18,    17,    16,
    13,    31,   292,    16,    17,    33,   296,    21,   298,    33,
    28,   223,   224,    33,     0,    28,    28,    30,    31,    32,
    28,     0,    28,   195,    28,    58,   231,   132,   200,    64,
    -1,   203,    43,    -1,    -1,    -1,    -1,    -1,   210,    -1,
    -1,   213,    -1,   215,   256,   257,    -1,    -1,    -1,    -1,
    -1,   263,    -1,    -1,   221,   222,   229,   269,    20,    21,
   232,    -1,    24,    -1,    -1,    -1,    -1,   279,    -1,    -1,
    -1,    -1,   284,   245,    -1,    -1,    -1,   289,    -1,    -1,
    20,    21,   254,    -1,    24,    -1,    -1,   260,    -1,    -1,
    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,    -1,   271,
   273,    -1,   275,   276,    -1,    -1,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
   293,    57,    58,    -1,    86,    87,    88,    89,    90,    91,
    92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
   102,    -1,    -1,   105,   106,   107,    86,    87,    88,    89,
    90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
   100,   101,   102,    -1,    -1,   105,   106,   107,     1,    -1,
     3,     4,     5,    -1,    -1,     8,     9,    10,    11,    -1,
    13,    14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    22,
    23,    24,    -1,    26,    -1,    -1,    29,    30,    31,    32,
    43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
    43,    44,    -1,    -1,    57,    58,    -1,    -1,    -1,    -1,
    -1,    54,    55,    56,    -1,    -1,    59,    60,     3,     4,
     5,    -1,    -1,     8,     9,    10,    11,    -1,    -1,    14,
    15,    -1,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,
    -1,    26,    -1,    -1,    29,    30,    31,    32,    45,    46,
    47,    48,    49,    50,    51,    52,    -1,    -1,    43,    44,
    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,
    55,    56,    -1,    -1,    59,    60,     3,     4,     5,    -1,
    -1,     8,     9,    10,    11,    -1,    -1,    14,    15,    -1,
    -1,    -1,    -1,    -1,    -1,    22,    23,    24,    -1,    26,
    -1,    -1,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    56,
    -1,    -1,    59,    60,     3,     4,     5,    -1,    -1,     8,
     9,    10,    11,    -1,    -1,    14,    15,    -1,    -1,    -1,
    -1,    -1,    -1,    22,    23,    24,    -1,    26,    -1,    -1,
    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    43,    44,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    54,    55,    56,    -1,    -1,
    59,    60,     3,     4,     5,    -1,    -1,     8,     9,    10,
    11,    -1,    -1,    14,    15,    -1,    -1,    -1,    -1,    -1,
    -1,    22,    23,    24,    -1,    26,    -1,    -1,    29,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    15,    -1,    43,    44,    19,    -1,    21,    22,    23,    24,
    -1,    26,    -1,    54,    55,    56,    -1,    -1,    59,    60,
    -1,    -1,    -1,    -1,    -1,    15,    -1,    -1,    43,    44,
    20,    -1,    22,    23,    24,    -1,    26,    -1,    -1,    54,
    55,    56,    -1,    -1,    59,    60,    -1,    -1,    -1,    15,
    -1,    -1,    -1,    43,    44,    21,    22,    23,    24,    -1,
    26,    -1,    -1,    -1,    54,    55,    56,    -1,    -1,    59,
    60,    -1,    15,    -1,    -1,    -1,    19,    43,    44,    22,
    23,    24,    -1,    26,    -1,    -1,    -1,    -1,    54,    55,
    56,    -1,    -1,    59,    60,    -1,    15,    -1,    -1,    -1,
    43,    44,    21,    22,    23,    24,    -1,    26,    -1,    -1,
    -1,    54,    55,    56,    -1,    -1,    59,    60,    -1,    15,
    -1,    -1,    -1,    -1,    43,    44,    22,    23,    24,    -1,
    26,    -1,    -1,    -1,    -1,    54,    55,    56,    -1,    -1,
    59,    60,    15,    -1,    -1,    -1,    42,    43,    44,    22,
    23,    24,    -1,    26,    -1,    -1,    -1,    -1,    54,    55,
    56,    -1,    -1,    59,    60,    15,    -1,    -1,    -1,    -1,
    43,    44,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
    -1,    54,    55,    56,    -1,    -1,    59,    60,    -1,    -1,
    -1,    -1,    -1,    43,    44,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    54,    55,    56,    -1,    -1,    59,
    60,    34,    35,    36,    37,    38,    39,    40,    41,    42,
    43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
    -1,    -1,    -1,    -1,    57,    58,    36,    37,    38,    39,
    40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
    50,    51,    52,    -1,    -1,    -1,    -1,    57,    58
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/gnu/lib/bison.simple"

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
void *alloca (unsigned int);
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

#line 184 "/usr/local/gnu/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

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
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

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

case 1:
#line 270 "parse.y"
{
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 2:
#line 276 "parse.y"
{
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYABORT;
		  ;
    break;}
case 3:
#line 282 "parse.y"
{
		    global_command = yyvsp[-1].tree_command_list_type;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 4:
#line 288 "parse.y"
{
		    global_command = yyvsp[-1].tree_command_list_type;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 5:
#line 294 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 6:
#line 296 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 7:
#line 298 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 8:
#line 300 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 9:
#line 302 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 10:
#line 306 "parse.y"
{ yyval.tree_command_list_type = (tree_command_list *) NULL; ;
    break;}
case 11:
#line 308 "parse.y"
{ yyval.tree_command_list_type = (tree_command_list *) NULL; ;
    break;}
case 12:
#line 310 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type->reverse (); ;
    break;}
case 13:
#line 312 "parse.y"
{
		    yyvsp[-1].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse ();
		  ;
    break;}
case 14:
#line 317 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse (); ;
    break;}
case 15:
#line 321 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 16:
#line 323 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 17:
#line 325 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 18:
#line 327 "parse.y"
{
		    yyvsp[-2].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type);
		  ;
    break;}
case 19:
#line 332 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type); ;
    break;}
case 31:
#line 355 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (); ;
    break;}
case 32:
#line 357 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type; ;
    break;}
case 33:
#line 360 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type->reverse (); ;
    break;}
case 34:
#line 362 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse (); ;
    break;}
case 35:
#line 364 "parse.y"
{
		    yyvsp[-1].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse ();
		  ;
    break;}
case 36:
#line 371 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 37:
#line 373 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type); ;
    break;}
case 38:
#line 375 "parse.y"
{
		    yyvsp[-2].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type);
		  ;
    break;}
case 39:
#line 382 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_plot_command_type; ;
    break;}
case 40:
#line 384 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_command_type; ;
    break;}
case 41:
#line 386 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 42:
#line 388 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_function_type; ;
    break;}
case 43:
#line 390 "parse.y"
{ yyval.tree_type = NULL_TREE; ;
    break;}
case 44:
#line 394 "parse.y"
{
		    tree_subplot_list *tmp = yyvsp[0].tree_subplot_list_type->reverse ();
		    yyval.tree_plot_command_type = new tree_plot_command (tmp, yyvsp[-1].pttype);
		    plotting = 0;
		    in_plot_range = 0;
		    in_plot_using = 0;
		    in_plot_style = 0;
		  ;
    break;}
case 45:
#line 403 "parse.y"
{
		    tree_subplot_list *tmp = yyvsp[0].tree_subplot_list_type->reverse ();
		    yyval.tree_plot_command_type = new tree_plot_command (tmp, yyvsp[-1].tree_plot_limits_type, yyvsp[-2].pttype);
		    plotting = 0;
		    in_plot_range = 0;
		    in_plot_using = 0;
		    in_plot_style = 0;
		  ;
    break;}
case 46:
#line 414 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[0].tree_plot_range_type); ;
    break;}
case 47:
#line 416 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[-1].tree_plot_range_type, yyvsp[0].tree_plot_range_type); ;
    break;}
case 48:
#line 418 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[-2].tree_plot_range_type, yyvsp[-1].tree_plot_range_type, yyvsp[0].tree_plot_range_type); ;
    break;}
case 49:
#line 422 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (yyvsp[-3].tree_type, yyvsp[-1].tree_type); ;
    break;}
case 50:
#line 424 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (NULL, yyvsp[-1].tree_type); ;
    break;}
case 51:
#line 426 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (yyvsp[-2].tree_type, NULL); ;
    break;}
case 52:
#line 428 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (); ;
    break;}
case 53:
#line 430 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (); ;
    break;}
case 54:
#line 434 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[0].tree_subplot_list_type; ;
    break;}
case 55:
#line 436 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[-2].tree_subplot_list_type->chain (yyvsp[0].tree_subplot_list_type); ;
    break;}
case 56:
#line 440 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_type); ;
    break;}
case 57:
#line 442 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[0].tree_subplot_list_type->set_data (yyvsp[-1].tree_type); ;
    break;}
case 58:
#line 446 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, NULL, NULL); ;
    break;}
case 59:
#line 448 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[0].tree_type, NULL); ;
    break;}
case 60:
#line 450 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, NULL, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 61:
#line 452 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[0].tree_type, NULL); ;
    break;}
case 62:
#line 454 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-1].tree_type, NULL); ;
    break;}
case 63:
#line 456 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, NULL, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 64:
#line 458 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, NULL, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 65:
#line 460 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[-1].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 66:
#line 462 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[0].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 67:
#line 464 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-2].tree_subplot_using_type, yyvsp[-1].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 68:
#line 466 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-2].tree_subplot_using_type, yyvsp[0].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 69:
#line 468 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[-2].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 70:
#line 470 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-2].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 71:
#line 472 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[0].tree_type, yyvsp[-2].tree_subplot_style_type); ;
    break;}
case 72:
#line 474 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-1].tree_type, yyvsp[-2].tree_subplot_style_type); ;
    break;}
case 73:
#line 478 "parse.y"
{
		    yyval.tree_subplot_using_type = yyvsp[0].tree_subplot_using_type;
		    in_plot_using = 0;
		  ;
    break;}
case 74:
#line 483 "parse.y"
{
		    yyval.tree_subplot_using_type = yyvsp[-1].tree_subplot_using_type->set_format (yyvsp[0].tree_type);
		    in_plot_using = 0;
		  ;
    break;}
case 75:
#line 490 "parse.y"
{
		    tree_subplot_using *tmp = new tree_subplot_using ();
		    yyval.tree_subplot_using_type = tmp->add_qualifier (yyvsp[0].tree_type);
		  ;
    break;}
case 76:
#line 495 "parse.y"
{ yyval.tree_subplot_using_type = yyvsp[-2].tree_subplot_using_type->add_qualifier (yyvsp[0].tree_type); ;
    break;}
case 77:
#line 499 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 78:
#line 503 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[0].string); ;
    break;}
case 79:
#line 505 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[-1].string, yyvsp[0].tree_type); ;
    break;}
case 80:
#line 507 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[-3].string, yyvsp[-2].tree_type, yyvsp[0].tree_type); ;
    break;}
case 82:
#line 514 "parse.y"
{ yyval.tree_type = maybe_convert_to_ans_assign (yyvsp[0].tree_type); ;
    break;}
case 83:
#line 518 "parse.y"
{ ;
    break;}
case 84:
#line 522 "parse.y"
{ force_global (yyvsp[0].sym_rec->name ()); ;
    break;}
case 85:
#line 524 "parse.y"
{
		    symbol_record *sr = force_global (yyvsp[-2].sym_rec->name ());
		    tree_identifier *id = new tree_identifier (sr);
		    tree_simple_assignment_expression *expr =
		      new tree_simple_assignment_expression (id, yyvsp[0].tree_type);
		    expr->eval (0);
		  ;
    break;}
case 86:
#line 532 "parse.y"
{ force_global (yyvsp[0].sym_rec->name ()); ;
    break;}
case 87:
#line 534 "parse.y"
{
		    symbol_record *sr = force_global (yyvsp[-2].sym_rec->name ());
		    tree_identifier *id = new tree_identifier (sr);
		    tree_simple_assignment_expression *expr =
		      new tree_simple_assignment_expression (id, yyvsp[0].tree_type);
		    expr->eval (0);
		  ;
    break;}
case 89:
#line 545 "parse.y"
{
		    if (user_pref.warn_comma_in_global_decl)
		      warning ("comma in global declaration not\
 interpreted as a command separator");
		  ;
    break;}
case 90:
#line 553 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-3].tree_type);
		    if (yyvsp[0].ettype != while_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("while", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    looping--;
		    yyval.tree_command_type = new tree_while_command (yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type);
		  ;
    break;}
case 91:
#line 565 "parse.y"
{
		    if (yyvsp[0].ettype != for_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("for", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    looping--;
		    yyval.tree_command_type = new tree_for_command (yyvsp[-5].tree_identifier_type, yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type);
		  ;
    break;}
case 92:
#line 576 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-3].tree_type);
		    if (yyvsp[0].ettype != if_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("if", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    iffing--;
		    yyval.tree_command_type = new tree_if_command (yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type);
		  ;
    break;}
case 93:
#line 588 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-6].tree_type);
		    if (yyvsp[0].ettype != if_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("if", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    iffing--;
		    tree_if_command *t1 = new tree_if_command (yyvsp[-1].tree_command_list_type);
		    yyval.tree_command_type = t1->chain (yyvsp[-6].tree_type, yyvsp[-4].tree_command_list_type);
		  ;
    break;}
case 94:
#line 601 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-4].tree_type);
		    if (yyvsp[0].ettype != if_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("if", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    iffing--;
		    tree_if_command *t1 = yyvsp[-1].tree_if_command_type->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    yyval.tree_command_type = t1->chain (yyvsp[-4].tree_type, yyvsp[-2].tree_command_list_type);
		  ;
    break;}
case 95:
#line 616 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-7].tree_type);
		    if (yyvsp[0].ettype != if_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("if", yyvsp[0].ettype);
			ABORT_PARSE;
		      }
		    iffing--;
		    // Add the else list to the head of the elseif list,
		    // then reverse the list.
		    tree_if_command *t1 = yyvsp[-4].tree_if_command_type->chain (yyvsp[-1].tree_command_list_type);
		    t1 = t1->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    yyval.tree_command_type = t1->chain (yyvsp[-7].tree_type, yyvsp[-5].tree_command_list_type);
		  ;
    break;}
case 96:
#line 634 "parse.y"
{
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("break: only meaningful within a `for'\
 or `while' loop");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_break_command ();
		  ;
    break;}
case 97:
#line 645 "parse.y"
{
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("continue: only meaningful within a\
 `for' or `while' loop");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_break_command ();
		  ;
    break;}
case 98:
#line 656 "parse.y"
{
		    if (!defining_func)
		      {
			yyerror ("parse error");
			error ("return: only meaningful within a function");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_return_command ();
		  ;
    break;}
case 99:
#line 668 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-2].tree_type);
		    yyval.tree_if_command_type = new tree_if_command (yyvsp[-2].tree_type, yyvsp[0].tree_command_list_type);
		  ;
    break;}
case 100:
#line 673 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-2].tree_type);
		    yyval.tree_if_command_type = yyvsp[-5].tree_if_command_type->chain (yyvsp[-2].tree_type, yyvsp[0].tree_command_list_type);
		  ;
    break;}
case 109:
#line 692 "parse.y"
{ maybe_screwed_again++; ;
    break;}
case 110:
#line 696 "parse.y"
{ yyval.tree_type = new tree_simple_assignment_expression (yyvsp[-2].tree_index_expression_type, yyvsp[0].tree_type); ;
    break;}
case 111:
#line 698 "parse.y"
{

// Will need a way to convert the matrix list to a list of
// identifiers.  If that fails, we can abort here, without losing
// anything -- no other possible syntax is valid if we've seen the
// equals sign as the next token after the `]'.

		    yyval.tree_type = (tree_multi_assignment_expression *) NULL;
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->reverse ();
		    tree_return_list *id_list = tmp->to_return_list ();
		    if (id_list == NULL_TREE)
		      {
			yyerror ("parse error");
		        error ("invalid identifier list for assignment");
			yyval.tree_type = (tree_multi_assignment_expression *) NULL;
			ABORT_PARSE;
		      }
		    else
		      yyval.tree_type = new tree_multi_assignment_expression (id_list, yyvsp[0].tree_type);
		  ;
    break;}
case 112:
#line 721 "parse.y"
{
		    yyerror ("parse error");
		    error ("invalid assignment to a number");
		    yyval.tree_type = (tree_simple_assignment_expression *) NULL;
		    ABORT_PARSE;
		  ;
    break;}
case 113:
#line 728 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 114:
#line 732 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 115:
#line 734 "parse.y"
{ yyval.tree_type = new tree_postfix_expression (yyvsp[-1].tree_identifier_type, tree::increment); ;
    break;}
case 116:
#line 736 "parse.y"
{ yyval.tree_type = new tree_postfix_expression (yyvsp[-1].tree_identifier_type, tree::decrement); ;
    break;}
case 117:
#line 738 "parse.y"
{ yyval.tree_type = new tree_unary_expression (yyvsp[-1].tree_type, tree::hermitian); ;
    break;}
case 118:
#line 740 "parse.y"
{ yyval.tree_type = new tree_unary_expression (yyvsp[-1].tree_type, tree::transpose); ;
    break;}
case 119:
#line 742 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::power); ;
    break;}
case 120:
#line 744 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::elem_pow); ;
    break;}
case 121:
#line 746 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::add); ;
    break;}
case 122:
#line 748 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::subtract); ;
    break;}
case 123:
#line 750 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::multiply); ;
    break;}
case 124:
#line 752 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::divide); ;
    break;}
case 125:
#line 754 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::el_mul); ;
    break;}
case 126:
#line 756 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::el_div); ;
    break;}
case 127:
#line 758 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::leftdiv); ;
    break;}
case 128:
#line 760 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type,
						     tree::el_leftdiv); ;
    break;}
case 129:
#line 763 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_lt); ;
    break;}
case 130:
#line 765 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_le); ;
    break;}
case 131:
#line 767 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_eq); ;
    break;}
case 132:
#line 769 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_ge); ;
    break;}
case 133:
#line 771 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_gt); ;
    break;}
case 134:
#line 773 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_ne); ;
    break;}
case 135:
#line 775 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::and); ;
    break;}
case 136:
#line 777 "parse.y"
{ yyval.tree_type = new tree_binary_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::or); ;
    break;}
case 137:
#line 781 "parse.y"
{ yyval.tree_type = new tree_constant (yyvsp[0].number); ;
    break;}
case 138:
#line 783 "parse.y"
{ yyval.tree_type = new tree_constant (Complex (0.0, yyvsp[0].number)); ;
    break;}
case 139:
#line 785 "parse.y"
{ yyval.tree_type = new tree_constant (yyvsp[0].string); ;
    break;}
case 140:
#line 787 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_word_list_command_type; ;
    break;}
case 141:
#line 789 "parse.y"
{
		    if (yyvsp[-1].tree_type->is_assignment_expression ())
		      ((tree_assignment_expression *) yyvsp[-1].tree_type) -> in_parens++;
		    yyval.tree_type = yyvsp[-1].tree_type;
		  ;
    break;}
case 142:
#line 795 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_index_expression_type; ;
    break;}
case 143:
#line 797 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_matrix_type; ;
    break;}
case 144:
#line 799 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_colon_expression_type; ;
    break;}
case 145:
#line 801 "parse.y"
{ yyval.tree_type = new tree_prefix_expression (yyvsp[0].tree_identifier_type, tree::increment); ;
    break;}
case 146:
#line 803 "parse.y"
{ yyval.tree_type = new tree_prefix_expression (yyvsp[0].tree_identifier_type, tree::decrement); ;
    break;}
case 147:
#line 805 "parse.y"
{ yyval.tree_type = new tree_unary_expression (yyvsp[0].tree_type, tree::not); ;
    break;}
case 148:
#line 807 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 149:
#line 809 "parse.y"
{ yyval.tree_type = new tree_unary_expression (yyvsp[0].tree_type, tree::uminus); ;
    break;}
case 150:
#line 813 "parse.y"
{ yyval.tree_colon_expression_type = new tree_colon_expression (yyvsp[-2].tree_type, yyvsp[0].tree_type); ;
    break;}
case 151:
#line 815 "parse.y"
{
		    yyval.tree_colon_expression_type = yyvsp[-2].tree_colon_expression_type->chain (yyvsp[0].tree_type);
		    if (yyval.tree_colon_expression_type == (tree_colon_expression *) NULL)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  ;
    break;}
case 152:
#line 826 "parse.y"
{ yyval.tree_word_list_command_type = new tree_word_list_command (yyvsp[-1].tree_identifier_type, yyvsp[0].tree_word_list_type); ;
    break;}
case 153:
#line 828 "parse.y"
{
		    if (defining_func)
		      {
			yyerror ("parse error");
			error ("clear: invalid within function body");
			ABORT_PARSE;
		      }
		    symbol_record *sr = global_sym_tab->lookup ("clear", 1, 0);
		    assert (sr != (symbol_record *) NULL);
		    tree_identifier *tmp = new tree_identifier (sr);
		    yyval.tree_word_list_command_type = new tree_word_list_command (tmp,
						     (tree_word_list *) NULL);
		  ;
    break;}
case 154:
#line 842 "parse.y"
{
		    if (defining_func)
		      {
			yyerror ("parse error");
			error ("clear: invalid within function body");
			ABORT_PARSE;
		      }
		    symbol_record *sr = global_sym_tab->lookup ("clear", 1, 0);
		    assert (sr != (symbol_record *) NULL);
		    tree_identifier *tmp = new tree_identifier (sr);
		    yyval.tree_word_list_command_type = new tree_word_list_command (tmp, yyvsp[0].tree_word_list_type);
		  ;
    break;}
case 155:
#line 857 "parse.y"
{ yyval.tree_word_list_type = yyvsp[0].tree_word_list_type->reverse (); ;
    break;}
case 156:
#line 861 "parse.y"
{ yyval.tree_word_list_type = new tree_word_list (yyvsp[0].string); ;
    break;}
case 157:
#line 863 "parse.y"
{ yyval.tree_word_list_type = yyvsp[-1].tree_word_list_type->chain (yyvsp[0].string); ;
    break;}
case 158:
#line 869 "parse.y"
{ curr_sym_tab = global_sym_tab; ;
    break;}
case 159:
#line 873 "parse.y"
{ curr_sym_tab = tmp_local_sym_tab; ;
    break;}
case 160:
#line 877 "parse.y"
{ maybe_screwed = 0; ;
    break;}
case 161:
#line 881 "parse.y"
{ maybe_screwed = 1; ;
    break;}
case 162:
#line 885 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    yyval.tree_function_type = (tree_function *) NULL;
		  ;
    break;}
case 163:
#line 891 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    yyval.tree_function_type = (tree_function *) NULL;
		  ;
    break;}
case 164:
#line 899 "parse.y"
{
		    tree_identifier *tmp = new tree_identifier (yyvsp[-4].sym_rec);
		    tree_parameter_list *tpl = new tree_parameter_list (tmp);
		    tpl = tpl->reverse ();
		    tpl->mark_as_formal_parameters ();
		    yyval.tree_function_type = yyvsp[0].tree_function_type->define_ret_list (tpl);
		  ;
    break;}
case 165:
#line 907 "parse.y"
{
		    tree_parameter_list *tpl = yyvsp[-4].tree_parameter_list_type->reverse ();
		    tpl->mark_as_formal_parameters ();
		    yyval.tree_function_type = yyvsp[0].tree_function_type->define_ret_list (tpl);
		  ;
    break;}
case 166:
#line 915 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 167:
#line 917 "parse.y"
{ yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type->chain (yyvsp[0].tree_identifier_type); ;
    break;}
case 168:
#line 921 "parse.y"
{
		    char *id_name = yyvsp[-3].tree_identifier_type->name ();
//		    if (is_text_function_name (id_name))
//		      {
//			yyerror ("parse error");
//			error ("invalid use of reserved word %s", id_name);
//			ABORT_PARSE;
//		      }

// If input is coming from a file, issue a warning if the name of the
// file does not match the name of the function stated in the file.
// Matlab doesn't provide a diagnostic (it ignores the stated name).

		    if (reading_m_file)
		      {
			if (strcmp (curr_m_file_name, id_name) != 0)
			  warning ("function name `%s' does not agree\
 with M-file name `%s.m'", id_name, curr_m_file_name);

			id_to_define->define (yyvsp[0].tree_function_type);
			id_to_define->document (help_buf);
		      }
		    else
		      {
			yyvsp[-3].tree_identifier_type->define (yyvsp[0].tree_function_type);
			yyvsp[-3].tree_identifier_type->document (help_buf);
			top_level_sym_tab->clear (id_name);
		      }

		    yyval.tree_function_type = yyvsp[0].tree_function_type;
		  ;
    break;}
case 169:
#line 955 "parse.y"
{
		    tree_function *fcn = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab);
		    yyval.tree_function_type = fcn->define_param_list (yyvsp[-3].tree_parameter_list_type);
		  ;
    break;}
case 170:
#line 960 "parse.y"
{ yyval.tree_function_type = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab); ;
    break;}
case 171:
#line 962 "parse.y"
{ yyval.tree_function_type = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab); ;
    break;}
case 172:
#line 966 "parse.y"
{
		    if (yyvsp[0].ettype != function_end && yyvsp[0].ettype != simple_end)
		      {
			yyerror ("parse error");
			end_error ("function", yyvsp[0].ettype);
			ABORT_PARSE;
		      }

		    if (reading_m_file)
		      check_for_garbage_after_fcn_def ();
		  ;
    break;}
case 173:
#line 978 "parse.y"
{
		    if (! (reading_m_file || reading_script_file))
		      YYABORT;
		  ;
    break;}
case 174:
#line 985 "parse.y"
{ yyval.tree_index_expression_type = new tree_index_expression (yyvsp[0].tree_identifier_type); ;
    break;}
case 175:
#line 987 "parse.y"
{ yyval.tree_index_expression_type = new tree_index_expression (yyvsp[-3].tree_identifier_type, yyvsp[-1].tree_argument_list_type); ;
    break;}
case 176:
#line 989 "parse.y"
{
		    yyval.tree_index_expression_type = new tree_index_expression
			       (yyvsp[-2].tree_identifier_type, (tree_argument_list *) NULL);
		  ;
    break;}
case 177:
#line 994 "parse.y"
{
		    yyerror ("parse error");
		    error ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    yyval.tree_index_expression_type = (tree_index_expression *) NULL;
		    ABORT_PARSE;
		  ;
    break;}
case 178:
#line 1004 "parse.y"
{
		    tree_parameter_list *tmp = yyvsp[-1].tree_parameter_list_type->reverse ();
		    tmp->mark_as_formal_parameters ();
		    yyval.tree_parameter_list_type = tmp;
		  ;
    break;}
case 179:
#line 1011 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 180:
#line 1013 "parse.y"
{ yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type->chain (yyvsp[0].tree_identifier_type); ;
    break;}
case 181:
#line 1015 "parse.y"
{
		    error ("parameter lists may only contain identifiers");
		    yyval.tree_parameter_list_type = (tree_parameter_list *) NULL;
		  ;
    break;}
case 182:
#line 1020 "parse.y"
{
		    error ("parameter lists may only contain identifiers");
		    yyval.tree_parameter_list_type = (tree_parameter_list *) NULL;
		  ;
    break;}
case 183:
#line 1027 "parse.y"
{ yyval.tree_identifier_type = new tree_identifier (yyvsp[0].sym_rec); ;
    break;}
case 184:
#line 1030 "parse.y"
{ yyval.tree_argument_list_type = yyvsp[0].tree_argument_list_type->reverse (); ;
    break;}
case 185:
#line 1034 "parse.y"
{
		    tree_constant *colon;
		    colon = new tree_constant (tree_constant_rep::magic_colon);
		    yyval.tree_argument_list_type = new tree_argument_list (colon);
		  ;
    break;}
case 186:
#line 1040 "parse.y"
{
		    tree_constant *colon;
		    colon = new tree_constant (tree_constant_rep::magic_colon);
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type->chain (colon);
		    if (yyval.tree_argument_list_type == NULL_TREE)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  ;
    break;}
case 187:
#line 1051 "parse.y"
{ yyval.tree_argument_list_type = new tree_argument_list (yyvsp[0].tree_type); ;
    break;}
case 188:
#line 1053 "parse.y"
{
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type->chain (yyvsp[0].tree_type);
		    if (yyval.tree_argument_list_type == NULL_TREE)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  ;
    break;}
case 189:
#line 1064 "parse.y"
{
		    mlnm.pop ();
		    yyval.tree_matrix_type = new tree_matrix ();
		  ;
    break;}
case 190:
#line 1069 "parse.y"
{
		    mlnm.pop ();
		    yyval.tree_matrix_type = new tree_matrix ();
		  ;
    break;}
case 191:
#line 1074 "parse.y"
{
		    mlnm.pop ();
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    yyval.tree_matrix_type = tmp->reverse ();
		  ;
    break;}
case 195:
#line 1088 "parse.y"
{
		    if (mlnm.top ())
		      {
			mlnm.pop ();
			mlnm.push (0);
		        tree_matrix *tmp = new tree_matrix (yyvsp[0].tree_type, tree::md_none);
			ml.push (tmp);
		      }
		    else
		      {
			tree_matrix *tmp = ml.pop ();
			tmp = tmp->chain (yyvsp[0].tree_type, tree::md_down);
			ml.push (tmp);
		      }
		  ;
    break;}
case 197:
#line 1105 "parse.y"
{
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->chain (yyvsp[0].tree_type, tree::md_right);
		    ml.push (tmp);
		  ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 457 "/usr/local/gnu/lib/bison.simple"

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
#line 1112 "parse.y"


static void
yyerror (char *s)
{
  char *line = current_input_line;
  int err_col = current_input_column;
  if (err_col == 0)
    err_col = strlen (current_input_line) + 1;

// Print a message like `parse error'.
  fprintf (stderr, "\n%s", s);

// Maybe print the line number and file name.
  if (reading_m_file || reading_script_file)
    fprintf (stderr, " near line %d of file %s.m", input_line_number,
	     curr_m_file_name);

  int len = strlen (line);
  if (line[len-1] == '\n')
    {
      len--;
      line[len] = '\0';
    }

// Print the line, maybe with a pointer near the error token.
  if (err_col > len)
    fprintf (stderr, ":\n\n  %s\n\n", line);
  else
    fprintf (stderr, ":\n\n  %s\n  %*s\n\n", line, err_col, "^");
}

static void
end_error (char *type, end_tok_type ettype)
{
  switch (ettype)
    {
    case simple_end:
      error ("%s command matched by `end'", type);
      break;
    case for_end:
      error ("%s command matched by `endfor'", type);
      break;
    case function_end:
      error ("%s command matched by `endfunction'", type);
      break;
    case if_end:
      error ("%s command matched by `endif'", type);
      break;
    case while_end:
      error ("%s command matched by `endwhile'", type);
      break;
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Need to make sure that the expression isn't already an identifier
 * that has a name, or an assignment expression.
 *
 * Note that an expression can't be just an identifier anymore -- it
 * must at least be an index expression (see the definition of the
 * non-terminal `variable' above).
 */
tree *
maybe_convert_to_ans_assign (tree *expr)
{
  tree *retval = expr;

  symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

  assert (sr != (symbol_record *) NULL);

  if (expr->is_index_expression ())
    {
      tree_index_expression *idx_expr = (tree_index_expression *) expr;
      tree_argument_list *args = idx_expr->arg_list ();

      if (args == (tree_argument_list *) NULL)
	{
	  tree_identifier *tmp = idx_expr->ident ();
	  tree *defn = tmp->def ();
	  if (defn != NULL_TREE && ! defn->is_builtin ())
	    {
	      return retval;
	    }
	}
    }
  else if (expr->is_assignment_expression ())
    {
      return retval;
    }

  tree_identifier *ans = new tree_identifier (sr);
  retval = new tree_simple_assignment_expression (ans, expr);

  return retval;
}

void
maybe_warn_assign_as_truth_value (tree *expr)
{
  if (user_pref.warn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && ((tree_assignment_expression *) expr) -> in_parens < 2)
    {
      warning ("suggest parenthesis around assignment used as truth value");
    }
}

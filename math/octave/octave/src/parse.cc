
/*  A Bison parser, made from parse.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	EXPR_AND	258
#define	EXPR_OR	259
#define	EXPR_NOT	260
#define	EXPR_LT	261
#define	EXPR_LE	262
#define	EXPR_EQ	263
#define	EXPR_NE	264
#define	EXPR_GE	265
#define	EXPR_GT	266
#define	LEFTDIV	267
#define	EMUL	268
#define	EDIV	269
#define	ELEFTDIV	270
#define	QUOTE	271
#define	TRANSPOSE	272
#define	PLUS_PLUS	273
#define	MINUS_MINUS	274
#define	POW	275
#define	EPOW	276
#define	NUM	277
#define	IMAG_NUM	278
#define	NAME	279
#define	SCREW	280
#define	END	281
#define	PLOT	282
#define	TEXT	283
#define	STYLE	284
#define	FOR	285
#define	WHILE	286
#define	IF	287
#define	ELSEIF	288
#define	ELSE	289
#define	BREAK	290
#define	CONTINUE	291
#define	FUNC_RET	292
#define	FCN	293
#define	SCREW_TWO	294
#define	END_OF_INPUT	295
#define	GLOBAL	296
#define	USING	297
#define	TITLE	298
#define	WITH	299
#define	COLON	300
#define	OPEN_BRACE	301
#define	CLOSE_BRACE	302
#define	UNARY	303

#line 28 "parse.y"

#define YYDEBUG 1

#include "SLStack.h"

#include "Matrix.h"

#include "error.h"
#include "variables.h"
#include "octave-hist.h"
#include "user-prefs.h"
#include "input.h"
#include "utils.h"
#include "tree.h"
#include "symtab.h"
#include "builtins.h"
#include "octave.h"
#include "parse.h"
#include "lex.h"
#include "token.h"

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
int current_input_column = 1;

// Buffer for help text snagged from M-files.
// Probably shouldn't be a fixed size...
char help_buf [HELP_BUF_LENGTH];

// Nonzero means we're working on a plot command.
int plotting = 0;

// Nonzero means we've seen something that means we must be past the
// range part of a plot command.
int past_plot_range = 0;

// Nonzero means we're looking at the range part of a plot command.
int in_plot_range = 0;

// Nonzero means we're looking at the using part of a plot command.
int in_plot_using = 0;

// Nonzero means we're looking at the style part of a plot command.
int in_plot_style = 0;

// Check to see that end statements are properly matched.
static int check_end (token *tok, token::end_tok_type expected);

// Error mesages for mismatched end statements.
static void end_error (char *type, token::end_tok_type ettype, int l, int c);

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


#line 133 "parse.y"
typedef union
{
// The type of the basic tokens returned by the lexer.
  token *tok_val;

// Types for the nonterminals we generate.
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
  tree_global_command *tree_global_command_type;
  tree_command_list *tree_command_list_type;
  tree_word_list_command *tree_word_list_command_type;
  tree_plot_command *tree_plot_command_type;
  tree_subplot_list *tree_subplot_list_type;
  tree_plot_limits *tree_plot_limits_type;
  tree_plot_range *tree_plot_range_type;
  tree_subplot_using *tree_subplot_using_type;
  tree_subplot_style *tree_subplot_style_type;
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



#define	YYFINAL		310
#define	YYFLAG		-32768
#define	YYNTBASE	62

#define YYTRANSLATE(x) ((unsigned)(x) <= 303 ? yytranslate[x] : 119)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    56,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    59,
    60,     7,     6,    55,     5,     2,     8,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     4,    54,     2,
     3,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    58,     2,    61,     2,     2,     2,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     2,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    52,    53,    57
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
   238,   239,   241,   244,   248,   250,   254,   258,   264,   265,
   267,   273,   281,   287,   296,   303,   313,   315,   317,   319,
   325,   332,   333,   335,   337,   339,   341,   344,   347,   350,
   351,   355,   362,   366,   368,   370,   373,   376,   379,   382,
   386,   390,   394,   398,   402,   406,   410,   414,   418,   422,
   426,   430,   434,   438,   442,   446,   450,   454,   456,   458,
   460,   462,   466,   468,   470,   472,   475,   478,   481,   484,
   487,   491,   495,   498,   500,   502,   505,   506,   507,   508,
   509,   514,   519,   525,   531,   536,   540,   545,   550,   556,
   560,   562,   564,   566,   571,   575,   578,   581,   584,   588,
   591,   595,   597,   599,   601,   605,   607,   611,   614,   618,
   623,   625,   628,   632,   634,   637
};

static const short yyrhs[] = {    56,
     0,    46,     0,    63,    56,     0,    63,    46,     0,     1,
     0,     1,    56,     0,     1,    46,     0,    63,     1,     0,
    63,     1,    46,     0,    65,     0,    66,     0,    64,     0,
    64,    65,     0,    64,    66,     0,    72,     0,    65,    72,
     0,    66,    72,     0,    64,    65,    72,     0,    64,    66,
    72,     0,    54,     0,    65,    55,     0,    65,    54,     0,
    55,     0,    66,    54,     0,    66,    55,     0,    55,     0,
    56,     0,    67,    91,     0,    54,     0,    68,    91,     0,
     0,    70,     0,    71,     0,    71,    67,     0,    71,    68,
     0,    72,     0,    71,    67,    72,     0,    71,    68,    72,
     0,    73,     0,    88,     0,    84,     0,   104,     0,    85,
     0,    33,    76,     0,    33,    74,    76,     0,    75,     0,
    75,    75,     0,    75,    75,    75,     0,    52,    93,    51,
    93,    53,     0,    52,    51,    93,    53,     0,    52,    93,
    51,    53,     0,    52,    51,    53,     0,    52,    53,     0,
    77,     0,    76,    55,    77,     0,    93,     0,    93,    78,
     0,    79,     0,    81,     0,    82,     0,    79,    81,     0,
    81,    79,     0,    79,    82,     0,    82,    79,     0,    81,
    82,     0,    82,    81,     0,    79,    81,    82,     0,    79,
    82,    81,     0,    81,    79,    82,     0,    81,    82,    79,
     0,    82,    79,    81,     0,    82,    81,    79,     0,    80,
     0,    80,    93,     0,    48,    93,     0,    80,    51,    93,
     0,    49,    93,     0,    50,    35,     0,    50,    35,    93,
     0,    50,    35,    93,    83,    93,     0,     0,    93,     0,
    47,    86,     0,    47,    86,    55,     0,    30,     0,    30,
     3,    93,     0,    86,    87,    30,     0,    86,    87,    30,
     3,    93,     0,     0,    55,     0,    37,    93,    90,    69,
    32,     0,    36,   110,     3,    93,    90,    69,    32,     0,
    38,    93,    90,    69,    32,     0,    38,    93,    90,    69,
    40,    90,    69,    32,     0,    38,    93,    90,    69,    89,
    32,     0,    38,    93,    90,    69,    89,    40,    90,    69,
    32,     0,    41,     0,    42,     0,    43,     0,    39,    90,
    93,    90,    69,     0,    89,    39,    90,    93,    90,    69,
     0,     0,    91,     0,    55,     0,    54,     0,    56,     0,
    91,    55,     0,    91,    54,     0,    91,    56,     0,     0,
   110,     3,    93,     0,    58,    92,   118,    45,     3,    93,
     0,    28,     3,    93,     0,    94,     0,    95,     0,   113,
    24,     0,   113,    25,     0,    94,    22,     0,    94,    23,
     0,    94,    26,    94,     0,    94,    27,    94,     0,    94,
     6,    94,     0,    94,     5,    94,     0,    94,     7,    94,
     0,    94,     8,    94,     0,    94,    19,    94,     0,    94,
    20,    94,     0,    94,    18,    94,     0,    94,    21,    94,
     0,    94,    12,    94,     0,    94,    13,    94,     0,    94,
    14,    94,     0,    94,    16,    94,     0,    94,    17,    94,
     0,    94,    15,    94,     0,    94,     9,    94,     0,    94,
    10,    94,     0,    28,     0,    29,     0,    34,     0,    97,
     0,    59,    93,    60,     0,   110,     0,   116,     0,    96,
     0,    24,   113,     0,    25,   113,     0,    11,    94,     0,
     6,    94,     0,     5,    94,     0,    94,     4,    94,     0,
    96,     4,    94,     0,   113,    98,     0,    99,     0,    34,
     0,    99,    34,     0,     0,     0,     0,     0,    44,   100,
   103,   105,     0,    44,   100,   103,   107,     0,    31,   102,
   100,     3,   107,     0,   106,    61,   100,     3,   107,     0,
    58,   102,   101,   113,     0,   106,    55,   113,     0,   113,
   102,   101,   108,     0,   111,    90,    69,   109,     0,    59,
    60,    90,    69,   109,     0,    90,    69,   109,     0,    32,
     0,    46,     0,   113,     0,   113,    59,   114,    60,     0,
   113,    59,    60,     0,   113,    58,     0,   112,    60,     0,
    59,   113,     0,   112,    55,   113,     0,    59,     1,     0,
   112,    55,     1,     0,    30,     0,   115,     0,     4,     0,
   115,    55,     4,     0,    93,     0,   115,    55,    93,     0,
    58,    61,     0,    58,    54,    61,     0,    58,    92,   117,
    61,     0,   118,     0,   117,    54,     0,   117,    54,   118,
     0,    93,     0,   118,    55,     0,   118,    55,    93,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   227,   233,   239,   245,   251,   253,   255,   257,   259,   263,
   265,   267,   269,   274,   278,   280,   282,   284,   289,   293,
   294,   295,   298,   299,   300,   303,   304,   305,   308,   309,
   312,   314,   317,   319,   321,   328,   330,   332,   339,   341,
   343,   345,   347,   351,   361,   373,   375,   377,   381,   383,
   385,   387,   389,   393,   395,   399,   401,   405,   407,   409,
   411,   413,   415,   417,   419,   421,   423,   425,   427,   429,
   431,   433,   437,   442,   449,   454,   458,   462,   464,   466,
   470,   473,   477,   479,   483,   488,   493,   498,   505,   506,
   514,   523,   531,   540,   550,   561,   576,   587,   599,   611,
   617,   624,   625,   628,   629,   630,   631,   632,   633,   636,
   640,   643,   667,   674,   678,   680,   683,   686,   689,   692,
   695,   698,   701,   704,   707,   710,   713,   716,   719,   722,
   725,   728,   731,   734,   737,   740,   743,   748,   750,   752,
   754,   756,   762,   764,   766,   768,   771,   774,   777,   779,
   784,   787,   798,   802,   806,   808,   814,   818,   822,   826,
   830,   836,   844,   853,   861,   863,   867,   915,   920,   922,
   926,   934,   941,   946,   951,   957,   967,   974,   976,   978,
   983,   990,   994,   998,  1004,  1015,  1017,  1028,  1033,  1038,
  1047,  1048,  1049,  1052,  1068,  1069
};

static const char * const yytname[] = {   "$","error","$illegal.","'='","':'",
"'-'","'+'","'*'","'/'","EXPR_AND","EXPR_OR","EXPR_NOT","EXPR_LT","EXPR_LE",
"EXPR_EQ","EXPR_NE","EXPR_GE","EXPR_GT","LEFTDIV","EMUL","EDIV","ELEFTDIV","QUOTE",
"TRANSPOSE","PLUS_PLUS","MINUS_MINUS","POW","EPOW","NUM","IMAG_NUM","NAME","SCREW",
"END","PLOT","TEXT","STYLE","FOR","WHILE","IF","ELSEIF","ELSE","BREAK","CONTINUE",
"FUNC_RET","FCN","SCREW_TWO","END_OF_INPUT","GLOBAL","USING","TITLE","WITH",
"COLON","OPEN_BRACE","CLOSE_BRACE","';'","','","'\\n'","UNARY","'['","'('","')'",
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
    62,    62,    62,    62,    62,    62,    62,    62,    62,    63,
    63,    63,    63,    63,    64,    64,    64,    64,    64,    65,
    65,    65,    66,    66,    66,    67,    67,    67,    68,    68,
    69,    69,    70,    70,    70,    71,    71,    71,    72,    72,
    72,    72,    72,    73,    73,    74,    74,    74,    75,    75,
    75,    75,    75,    76,    76,    77,    77,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    79,    79,    80,    80,    81,    82,    82,    82,
    83,    84,    85,    85,    86,    86,    86,    86,    87,    87,
    88,    88,    88,    88,    88,    88,    88,    88,    88,    89,
    89,    90,    90,    91,    91,    91,    91,    91,    91,    92,
    93,    93,    93,    93,    94,    94,    94,    94,    94,    94,
    94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
    94,    94,    94,    94,    94,    94,    94,    95,    95,    95,
    95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
    96,    96,    97,    98,    99,    99,   100,   101,   102,   103,
   104,   104,   105,   105,   106,   106,   107,   108,   108,   108,
   109,   109,   110,   110,   110,   110,   111,   112,   112,   112,
   112,   113,   114,   115,   115,   115,   115,   116,   116,   116,
   117,   117,   117,   118,   118,   118
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
     0,     1,     2,     3,     1,     3,     3,     5,     0,     1,
     5,     7,     5,     8,     6,     9,     1,     1,     1,     5,
     6,     0,     1,     1,     1,     1,     2,     2,     2,     0,
     3,     6,     3,     1,     1,     2,     2,     2,     2,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     1,     1,     1,
     1,     3,     1,     1,     1,     2,     2,     2,     2,     2,
     3,     3,     2,     1,     1,     2,     0,     0,     0,     0,
     4,     4,     5,     5,     4,     3,     4,     4,     5,     3,
     1,     1,     1,     4,     3,     2,     2,     2,     3,     2,
     3,     1,     1,     1,     3,     1,     3,     2,     3,     4,
     1,     2,     3,     1,     2,     3
};

static const short yydefact[] = {     0,
     5,     0,     0,     0,     0,     0,   138,   139,   182,     0,
   140,     0,     0,     0,    97,    98,    99,   157,     2,     0,
    20,    23,     1,   110,     0,     0,    12,    10,    11,    15,
    39,    41,    43,    40,    82,   114,   115,   145,   141,    42,
   143,   173,   144,     7,     6,   138,   110,   150,   143,   149,
   148,   146,   147,     0,     0,     0,    46,    44,    54,    56,
     0,   173,   102,   102,   160,    85,    83,     0,   188,     0,
     0,     8,     4,     3,    13,    14,    22,    21,    16,    24,
    25,    17,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
   119,     0,     0,     0,     0,   116,   117,   155,   176,     0,
   153,   154,     0,   113,     0,    53,     0,    45,    47,     0,
     0,     0,     0,    57,    58,    73,    59,    60,     0,   105,
   104,   106,    31,   103,    31,     0,     0,    84,     0,   189,
   194,     0,   191,   142,     9,    18,    19,   151,   123,   122,
   124,   125,   136,   137,   130,   131,   132,   135,   133,   134,
   128,   126,   127,   129,   120,   121,   152,   111,   184,   175,
   186,     0,   183,   156,   191,    52,     0,     0,    48,    55,
    75,    77,    78,    61,    63,     0,    74,    62,    65,    64,
    66,   102,     0,    32,    33,    36,   108,   107,   109,     0,
   159,   159,   161,     0,   162,   159,    86,    87,   192,   190,
     0,   195,   174,     0,    50,    51,     0,    81,    67,    68,
    76,    69,    70,    71,    72,    31,    91,    29,    26,    27,
    34,    35,    93,   102,   102,     0,   157,   158,     0,   157,
   158,     0,   193,     0,   196,   185,   187,    49,     0,     0,
    37,    28,    38,    30,     0,    31,    95,   102,   102,     0,
     0,   166,     0,   102,    88,   112,    80,    92,   102,     0,
     0,    31,     0,   165,     0,     0,    31,   167,   102,     0,
    31,    94,   102,     0,   163,   164,   180,   102,   178,     0,
    31,     0,   177,   100,    31,    96,    31,   171,   172,   170,
     0,   181,   179,   101,     0,   168,   169,     0,     0,     0
};

static const short yydefgoto[] = {   308,
    26,    27,    28,    29,   231,   232,   193,   194,   195,   196,
    31,    56,    57,    58,    59,   124,   125,   126,   127,   128,
   249,    32,    33,    67,   139,    34,   236,   133,   134,    70,
    35,    36,    37,    38,    39,   111,   112,    65,   261,   237,
   136,    40,   203,   204,   205,   278,   300,    41,   279,   280,
    42,   172,   173,    43,   142,   143
};

static const short yypact[] = {   339,
    -7,   252,   252,   252,    26,    26,    37,-32768,-32768,   526,
-32768,    26,   620,   620,-32768,-32768,-32768,-32768,-32768,    39,
-32768,-32768,-32768,   -30,   620,    17,    80,   425,   465,-32768,
-32768,-32768,-32768,-32768,-32768,   654,-32768,    83,-32768,-32768,
    97,     3,-32768,-32768,-32768,-32768,   -30,   115,-32768,   115,
   115,-32768,-32768,   620,   195,   620,    43,    53,-32768,    64,
   107,    95,    65,    65,-32768,   112,    -8,    62,-32768,   620,
    66,    98,-32768,-32768,   425,   465,-32768,-32768,-32768,-32768,
-32768,-32768,   252,   252,   252,   252,   252,   252,   252,   252,
   252,   252,   252,   252,   252,   252,   252,   252,   252,-32768,
-32768,   252,   252,   252,   620,-32768,-32768,-32768,-32768,    30,
-32768,   121,   620,-32768,   562,-32768,   113,    53,    43,   620,
   620,   620,   123,-32768,   118,   577,   -25,   127,   620,-32768,
-32768,-32768,   505,    92,   505,    -1,   620,   139,   141,-32768,
-32768,    14,    -2,-32768,-32768,-32768,-32768,   725,   381,   381,
    70,    70,   678,   678,   702,   702,   702,   702,   702,   702,
    70,    70,    70,    70,   115,   115,   725,-32768,-32768,-32768,
-32768,   125,   128,-32768,   133,-32768,   136,   588,-32768,-32768,
-32768,-32768,   620,   145,   149,   620,-32768,   145,   151,   149,
   151,    65,   171,-32768,    96,-32768,-32768,-32768,-32768,    45,
-32768,-32768,-32768,   -17,-32768,-32768,-32768,   201,   620,-32768,
   206,   620,-32768,   132,-32768,-32768,   158,   138,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   505,-32768,-32768,-32768,-32768,
   385,   385,-32768,    65,    65,    59,-32768,-32768,    26,-32768,
-32768,   620,   133,   620,-32768,-32768,-32768,-32768,   620,   183,
-32768,    92,-32768,    92,   620,   505,-32768,    65,    65,   213,
    26,-32768,   215,    73,-32768,-32768,-32768,-32768,    65,   196,
   620,   505,    26,-32768,    26,    12,   505,-32768,    65,    10,
   505,-32768,    65,   198,-32768,-32768,-32768,    65,-32768,   -20,
   505,    15,-32768,-32768,   505,-32768,   505,-32768,-32768,-32768,
   -20,-32768,-32768,-32768,   -20,-32768,-32768,   227,   231,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,   205,   212,-32768,-32768,  -132,-32768,-32768,     4,
-32768,-32768,   -52,   177,   117,-32768,  -108,-32768,  -114,  -110,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -62,   -51,   197,
    -4,   239,-32768,-32768,-32768,-32768,-32768,  -135,     6,  -120,
-32768,-32768,-32768,-32768,  -227,-32768,  -198,   210,-32768,-32768,
    -5,-32768,-32768,-32768,-32768,  -105
};


#define	YYLAST		752


static const short yytable[] = {    52,
    53,   135,   200,    30,   119,    60,    62,   175,    63,    64,
   184,   298,   287,   191,   185,   302,   189,    72,   188,   190,
    71,   -89,   121,    68,   123,   299,   106,   107,     9,   201,
    69,    79,    82,   169,     2,     3,   108,   239,    44,    54,
     4,     9,   211,   240,     9,   285,   138,   286,    45,   114,
   117,    60,   212,     5,     6,     9,   202,     7,     8,     9,
   109,   110,    73,    11,   292,   141,   179,   209,    66,   293,
   220,   288,    74,   219,   210,   224,   233,   222,   146,   147,
   223,   238,   225,   234,   235,   241,   104,    24,    25,   170,
   257,   100,   101,   250,    55,   102,   103,   258,   259,   105,
   168,   260,   306,   243,   263,   171,   307,   120,   141,   129,
   177,   121,   122,   123,   137,    60,   181,   182,   130,   131,
   132,   187,   140,   270,   192,   144,   130,   131,   132,   226,
   206,   276,   207,    21,    22,   246,     2,     3,   -79,   284,
   102,   103,     4,   145,   290,   197,   198,   199,   294,   228,
   229,   230,   109,   110,   174,     5,     6,   183,   301,     7,
     8,     9,   304,   178,   305,    11,   122,   123,   -90,   -79,
   208,   255,   256,   217,   121,   122,   -79,   -79,   218,   252,
   254,   221,   214,   -79,   213,   -79,   -79,   212,   215,    24,
    25,   -79,   -79,   -79,   123,   271,   272,   122,   121,     2,
     3,   277,   227,   242,   141,     4,   281,   245,   244,   247,
   248,    49,    49,    49,   268,   273,   291,   275,     5,     6,
   295,    61,     7,     8,     9,   297,   309,   282,    11,   296,
   310,    75,   118,   262,   251,   253,   180,   265,    76,   266,
    48,    50,    51,   113,   267,   115,   264,   116,     0,     0,
   269,     0,    24,    25,     0,   274,     2,     3,     0,     0,
     0,     0,     4,     0,     0,     0,   283,   206,     0,   206,
   289,     0,     0,     0,     0,     5,     6,     0,     0,    46,
     8,     9,     0,     0,     0,    11,   303,     0,     0,     0,
     0,     0,    49,    49,    49,    49,    49,    49,    49,    49,
    49,    49,    49,    49,    49,    49,    49,    49,    49,    47,
    25,    49,    49,    49,     0,     0,     0,     0,     0,     0,
     0,   148,   149,   150,   151,   152,   153,   154,   155,   156,
   157,   158,   159,   160,   161,   162,   163,   164,     0,     1,
   165,   166,   167,     2,     3,     0,     0,     0,     0,     4,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     5,     6,     0,     0,     7,     8,     9,     0,
     0,    10,    11,     0,    12,    13,    14,     0,     0,    15,
    16,    17,    18,     0,    19,    20,     0,    86,    87,     2,
     3,     0,    21,    22,    23,     4,    24,    25,    96,    97,
    98,    99,   100,   101,     0,     0,   102,   103,     5,     6,
     0,     0,     7,     8,     9,     0,     0,    10,    11,     0,
    12,    13,    14,     0,     0,    15,    16,    17,    18,     2,
     3,    20,     0,     0,     0,     4,     0,     0,   130,   131,
   132,     0,    24,    25,     0,     0,     0,     0,     5,     6,
     0,     0,     7,     8,     9,     0,     0,    10,    11,     0,
    12,    13,    14,     0,     0,    15,    16,    17,    18,     2,
     3,    20,     0,     0,     0,     4,     0,     0,    77,    78,
     0,     0,    24,    25,     0,     0,     0,     0,     5,     6,
     0,     0,     7,     8,     9,     0,     0,    10,    11,     0,
    12,    13,    14,     0,     0,    15,    16,    17,    18,     2,
     3,    20,     0,     0,     0,     4,     0,     0,    80,    81,
     0,     0,    24,    25,     0,     0,     0,     0,     5,     6,
     2,     3,     7,     8,     9,     0,     4,    10,    11,     0,
    12,    13,    14,     0,     0,    15,    16,    17,    18,     5,
     6,    20,     0,     7,     8,     9,     0,     0,     0,    11,
     0,     0,    24,    25,     0,     0,     2,     3,     0,     0,
     0,     0,     4,     0,     0,     0,     0,    55,     0,     0,
     0,     2,     3,    24,    25,     5,     6,     4,     0,     7,
     8,     9,     2,     3,     0,    11,     0,     0,     4,     0,
     5,     6,     0,     0,     7,     8,     9,     0,     0,     0,
    11,     5,     6,     0,   176,     7,     8,     9,     0,    24,
    25,    11,     0,     0,     2,     3,     0,   186,     0,     0,
     4,     0,     0,     0,    24,    25,     0,     0,     0,     0,
   216,     0,     0,     5,     6,    24,    25,     7,     8,     9,
     0,     0,     0,    11,     0,     0,     0,    83,    84,    85,
    86,    87,    88,    89,     0,    90,    91,    92,    93,    94,
    95,    96,    97,    98,    99,   100,   101,    24,    25,   102,
   103,    83,    84,    85,    86,    87,     0,     0,     0,    90,
    91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
   101,     0,     0,   102,   103,    83,    84,    85,    86,    87,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    96,
    97,    98,    99,   100,   101,     0,     0,   102,   103,    84,
    85,    86,    87,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    96,    97,    98,    99,   100,   101,     0,     0,
   102,   103
};

static const short yycheck[] = {     5,
     6,    64,   135,     0,    57,    10,    12,   113,    13,    14,
   125,    32,     1,   128,   125,     1,   127,     1,   127,   128,
    25,    30,    48,    54,    50,    46,    24,    25,    30,    31,
    61,    28,    29,     4,     5,     6,    34,    55,    46,     3,
    11,    30,    45,    61,    30,   273,    55,   275,    56,    54,
    55,    56,    55,    24,    25,    30,    58,    28,    29,    30,
    58,    59,    46,    34,    55,    70,   119,    54,    30,    60,
   185,    60,    56,   184,    61,   190,    32,   188,    75,    76,
   189,   202,   191,    39,    40,   206,     4,    58,    59,    60,
    32,    22,    23,   226,    52,    26,    27,    39,    40,     3,
   105,   237,   301,   209,   240,   110,   305,    55,   113,     3,
   115,    48,    49,    50,     3,   120,   121,   122,    54,    55,
    56,   126,    61,   256,   129,    60,    54,    55,    56,   192,
   136,    59,   137,    54,    55,     4,     5,     6,     1,   272,
    26,    27,    11,    46,   277,    54,    55,    56,   281,    54,
    55,    56,    58,    59,    34,    24,    25,    35,   291,    28,
    29,    30,   295,    51,   297,    34,    49,    50,    30,    32,
    30,   234,   235,   178,    48,    49,    39,    40,   183,   231,
   232,   186,    55,    46,    60,    48,    49,    55,    53,    58,
    59,    54,    55,    56,    50,   258,   259,    49,    48,     5,
     6,   264,    32,     3,   209,    11,   269,   212,     3,   214,
    53,     2,     3,     4,    32,     3,   279,     3,    24,    25,
   283,    12,    28,    29,    30,   288,     0,    32,    34,    32,
     0,    27,    56,   239,   231,   232,   120,   242,    27,   244,
     2,     3,     4,    47,   249,    51,   241,    53,    -1,    -1,
   255,    -1,    58,    59,    -1,   261,     5,     6,    -1,    -1,
    -1,    -1,    11,    -1,    -1,    -1,   271,   273,    -1,   275,
   276,    -1,    -1,    -1,    -1,    24,    25,    -1,    -1,    28,
    29,    30,    -1,    -1,    -1,    34,   292,    -1,    -1,    -1,
    -1,    -1,    83,    84,    85,    86,    87,    88,    89,    90,
    91,    92,    93,    94,    95,    96,    97,    98,    99,    58,
    59,   102,   103,   104,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    83,    84,    85,    86,    87,    88,    89,    90,    91,
    92,    93,    94,    95,    96,    97,    98,    99,    -1,     1,
   102,   103,   104,     5,     6,    -1,    -1,    -1,    -1,    11,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    24,    25,    -1,    -1,    28,    29,    30,    -1,
    -1,    33,    34,    -1,    36,    37,    38,    -1,    -1,    41,
    42,    43,    44,    -1,    46,    47,    -1,     7,     8,     5,
     6,    -1,    54,    55,    56,    11,    58,    59,    18,    19,
    20,    21,    22,    23,    -1,    -1,    26,    27,    24,    25,
    -1,    -1,    28,    29,    30,    -1,    -1,    33,    34,    -1,
    36,    37,    38,    -1,    -1,    41,    42,    43,    44,     5,
     6,    47,    -1,    -1,    -1,    11,    -1,    -1,    54,    55,
    56,    -1,    58,    59,    -1,    -1,    -1,    -1,    24,    25,
    -1,    -1,    28,    29,    30,    -1,    -1,    33,    34,    -1,
    36,    37,    38,    -1,    -1,    41,    42,    43,    44,     5,
     6,    47,    -1,    -1,    -1,    11,    -1,    -1,    54,    55,
    -1,    -1,    58,    59,    -1,    -1,    -1,    -1,    24,    25,
    -1,    -1,    28,    29,    30,    -1,    -1,    33,    34,    -1,
    36,    37,    38,    -1,    -1,    41,    42,    43,    44,     5,
     6,    47,    -1,    -1,    -1,    11,    -1,    -1,    54,    55,
    -1,    -1,    58,    59,    -1,    -1,    -1,    -1,    24,    25,
     5,     6,    28,    29,    30,    -1,    11,    33,    34,    -1,
    36,    37,    38,    -1,    -1,    41,    42,    43,    44,    24,
    25,    47,    -1,    28,    29,    30,    -1,    -1,    -1,    34,
    -1,    -1,    58,    59,    -1,    -1,     5,     6,    -1,    -1,
    -1,    -1,    11,    -1,    -1,    -1,    -1,    52,    -1,    -1,
    -1,     5,     6,    58,    59,    24,    25,    11,    -1,    28,
    29,    30,     5,     6,    -1,    34,    -1,    -1,    11,    -1,
    24,    25,    -1,    -1,    28,    29,    30,    -1,    -1,    -1,
    34,    24,    25,    -1,    53,    28,    29,    30,    -1,    58,
    59,    34,    -1,    -1,     5,     6,    -1,    51,    -1,    -1,
    11,    -1,    -1,    -1,    58,    59,    -1,    -1,    -1,    -1,
    53,    -1,    -1,    24,    25,    58,    59,    28,    29,    30,
    -1,    -1,    -1,    34,    -1,    -1,    -1,     4,     5,     6,
     7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    58,    59,    26,
    27,     4,     5,     6,     7,     8,    -1,    -1,    -1,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    -1,    -1,    26,    27,     4,     5,     6,     7,     8,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
    19,    20,    21,    22,    23,    -1,    -1,    26,    27,     5,
     6,     7,     8,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    18,    19,    20,    21,    22,    23,    -1,    -1,
    26,    27
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

#line 184 "/usr/local/gnu/lib/bison.simple"
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

case 1:
#line 228 "parse.y"
{
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 2:
#line 234 "parse.y"
{
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYABORT;
		  ;
    break;}
case 3:
#line 240 "parse.y"
{
		    global_command = yyvsp[-1].tree_command_list_type;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 4:
#line 246 "parse.y"
{
		    global_command = yyvsp[-1].tree_command_list_type;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 5:
#line 252 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 6:
#line 254 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 7:
#line 256 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 8:
#line 258 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 9:
#line 260 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 10:
#line 264 "parse.y"
{ yyval.tree_command_list_type = (tree_command_list *) NULL; ;
    break;}
case 11:
#line 266 "parse.y"
{ yyval.tree_command_list_type = (tree_command_list *) NULL; ;
    break;}
case 12:
#line 268 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type->reverse (); ;
    break;}
case 13:
#line 270 "parse.y"
{
		    yyvsp[-1].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse ();
		  ;
    break;}
case 14:
#line 275 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse (); ;
    break;}
case 15:
#line 279 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 16:
#line 281 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 17:
#line 283 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 18:
#line 285 "parse.y"
{
		    yyvsp[-2].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type);
		  ;
    break;}
case 19:
#line 290 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type); ;
    break;}
case 31:
#line 313 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (); ;
    break;}
case 32:
#line 315 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type; ;
    break;}
case 33:
#line 318 "parse.y"
{ yyval.tree_command_list_type = yyvsp[0].tree_command_list_type->reverse (); ;
    break;}
case 34:
#line 320 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse (); ;
    break;}
case 35:
#line 322 "parse.y"
{
		    yyvsp[-1].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-1].tree_command_list_type->reverse ();
		  ;
    break;}
case 36:
#line 329 "parse.y"
{ yyval.tree_command_list_type = new tree_command_list (yyvsp[0].tree_type); ;
    break;}
case 37:
#line 331 "parse.y"
{ yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type); ;
    break;}
case 38:
#line 333 "parse.y"
{
		    yyvsp[-2].tree_command_list_type->set_print_flag (0);
		    yyval.tree_command_list_type = yyvsp[-2].tree_command_list_type->chain (yyvsp[0].tree_type);
		  ;
    break;}
case 39:
#line 340 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_plot_command_type; ;
    break;}
case 40:
#line 342 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_command_type; ;
    break;}
case 41:
#line 344 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 42:
#line 346 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_function_type; ;
    break;}
case 43:
#line 348 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_global_command_type; ;
    break;}
case 44:
#line 352 "parse.y"
{
		    tree_subplot_list *tmp = yyvsp[0].tree_subplot_list_type->reverse ();
		    yyval.tree_plot_command_type = new tree_plot_command (tmp, yyvsp[-1].tok_val->pttype ());
		    plotting = 0;
		    past_plot_range = 0;
		    in_plot_range = 0;
		    in_plot_using = 0;
		    in_plot_style = 0;
		  ;
    break;}
case 45:
#line 362 "parse.y"
{
		    tree_subplot_list *tmp = yyvsp[0].tree_subplot_list_type->reverse ();
		    yyval.tree_plot_command_type = new tree_plot_command (tmp, yyvsp[-1].tree_plot_limits_type, yyvsp[-2].tok_val->pttype ());
		    plotting = 0;
		    past_plot_range = 0;
		    in_plot_range = 0;
		    in_plot_using = 0;
		    in_plot_style = 0;
		  ;
    break;}
case 46:
#line 374 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[0].tree_plot_range_type); ;
    break;}
case 47:
#line 376 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[-1].tree_plot_range_type, yyvsp[0].tree_plot_range_type); ;
    break;}
case 48:
#line 378 "parse.y"
{ yyval.tree_plot_limits_type = new tree_plot_limits (yyvsp[-2].tree_plot_range_type, yyvsp[-1].tree_plot_range_type, yyvsp[0].tree_plot_range_type); ;
    break;}
case 49:
#line 382 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (yyvsp[-3].tree_type, yyvsp[-1].tree_type); ;
    break;}
case 50:
#line 384 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (NULL, yyvsp[-1].tree_type); ;
    break;}
case 51:
#line 386 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (yyvsp[-2].tree_type, NULL); ;
    break;}
case 52:
#line 388 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (); ;
    break;}
case 53:
#line 390 "parse.y"
{ yyval.tree_plot_range_type = new tree_plot_range (); ;
    break;}
case 54:
#line 394 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[0].tree_subplot_list_type; ;
    break;}
case 55:
#line 396 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[-2].tree_subplot_list_type->chain (yyvsp[0].tree_subplot_list_type); ;
    break;}
case 56:
#line 400 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_type); ;
    break;}
case 57:
#line 402 "parse.y"
{ yyval.tree_subplot_list_type = yyvsp[0].tree_subplot_list_type->set_data (yyvsp[-1].tree_type); ;
    break;}
case 58:
#line 406 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, NULL, NULL); ;
    break;}
case 59:
#line 408 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[0].tree_type, NULL); ;
    break;}
case 60:
#line 410 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, NULL, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 61:
#line 412 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[0].tree_type, NULL); ;
    break;}
case 62:
#line 414 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-1].tree_type, NULL); ;
    break;}
case 63:
#line 416 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, NULL, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 64:
#line 418 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, NULL, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 65:
#line 420 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[-1].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 66:
#line 422 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (NULL, yyvsp[0].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 67:
#line 424 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-2].tree_subplot_using_type, yyvsp[-1].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 68:
#line 426 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-2].tree_subplot_using_type, yyvsp[0].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 69:
#line 428 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[-2].tree_type, yyvsp[0].tree_subplot_style_type); ;
    break;}
case 70:
#line 430 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-2].tree_type, yyvsp[-1].tree_subplot_style_type); ;
    break;}
case 71:
#line 432 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[-1].tree_subplot_using_type, yyvsp[0].tree_type, yyvsp[-2].tree_subplot_style_type); ;
    break;}
case 72:
#line 434 "parse.y"
{ yyval.tree_subplot_list_type = new tree_subplot_list (yyvsp[0].tree_subplot_using_type, yyvsp[-1].tree_type, yyvsp[-2].tree_subplot_style_type); ;
    break;}
case 73:
#line 438 "parse.y"
{
		    yyval.tree_subplot_using_type = yyvsp[0].tree_subplot_using_type;
		    in_plot_using = 0;
		  ;
    break;}
case 74:
#line 443 "parse.y"
{
		    yyval.tree_subplot_using_type = yyvsp[-1].tree_subplot_using_type->set_format (yyvsp[0].tree_type);
		    in_plot_using = 0;
		  ;
    break;}
case 75:
#line 450 "parse.y"
{
		    tree_subplot_using *tmp = new tree_subplot_using ();
		    yyval.tree_subplot_using_type = tmp->add_qualifier (yyvsp[0].tree_type);
		  ;
    break;}
case 76:
#line 455 "parse.y"
{ yyval.tree_subplot_using_type = yyvsp[-2].tree_subplot_using_type->add_qualifier (yyvsp[0].tree_type); ;
    break;}
case 77:
#line 459 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 78:
#line 463 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[0].tok_val->string ()); ;
    break;}
case 79:
#line 465 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[-1].tok_val->string (), yyvsp[0].tree_type); ;
    break;}
case 80:
#line 467 "parse.y"
{ yyval.tree_subplot_style_type = new tree_subplot_style (yyvsp[-3].tok_val->string (), yyvsp[-2].tree_type, yyvsp[0].tree_type); ;
    break;}
case 82:
#line 474 "parse.y"
{ yyval.tree_type = maybe_convert_to_ans_assign (yyvsp[0].tree_type); ;
    break;}
case 83:
#line 478 "parse.y"
{ yyval.tree_global_command_type = yyvsp[0].tree_global_command_type->reverse (); ;
    break;}
case 84:
#line 480 "parse.y"
{ yyval.tree_global_command_type = yyvsp[-1].tree_global_command_type->reverse (); ;
    break;}
case 85:
#line 484 "parse.y"
{
		    yyval.tree_global_command_type = new tree_global_command
			   (yyvsp[0].tok_val->sym_rec (), yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ());
		  ;
    break;}
case 86:
#line 489 "parse.y"
{
		    yyval.tree_global_command_type = new tree_global_command
			   (yyvsp[-2].tok_val->sym_rec (), yyvsp[0].tree_type, yyvsp[-2].tok_val->line (), yyvsp[-2].tok_val->column ());
		  ;
    break;}
case 87:
#line 494 "parse.y"
{
		    yyval.tree_global_command_type = yyvsp[-2].tree_global_command_type->chain (yyvsp[0].tok_val->sym_rec (), yyvsp[0].tok_val->line (),
				    yyvsp[0].tok_val->column ());
		  ;
    break;}
case 88:
#line 499 "parse.y"
{
		    yyval.tree_global_command_type = yyvsp[-4].tree_global_command_type->chain (yyvsp[-2].tok_val->sym_rec (), yyvsp[0].tree_type, yyvsp[-2].tok_val->line (),
				    yyvsp[-2].tok_val->column ());
		  ;
    break;}
case 90:
#line 507 "parse.y"
{
		    if (user_pref.warn_comma_in_global_decl)
		      warning ("comma in global declaration not\
 interpreted as a command separator");
		  ;
    break;}
case 91:
#line 515 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-3].tree_type);
		    if (check_end (yyvsp[0].tok_val, token::while_end))
		      ABORT_PARSE;
		    looping--;
		    yyval.tree_command_type = new tree_while_command (yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type, yyvsp[-4].tok_val->line (),
						 yyvsp[-4].tok_val->column ());
		  ;
    break;}
case 92:
#line 524 "parse.y"
{
		    if (check_end (yyvsp[0].tok_val, token::for_end))
		      ABORT_PARSE;
		    looping--;
		    yyval.tree_command_type = new tree_for_command (yyvsp[-5].tree_index_expression_type, yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type,
					       yyvsp[-6].tok_val->line (), yyvsp[-6].tok_val->column ());
		  ;
    break;}
case 93:
#line 532 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-3].tree_type);
		    if (check_end (yyvsp[0].tok_val, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    yyval.tree_command_type = new tree_if_command (yyvsp[-3].tree_type, yyvsp[-1].tree_command_list_type,
					      yyvsp[-4].tok_val->line (), yyvsp[-4].tok_val->column ());
		  ;
    break;}
case 94:
#line 541 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-6].tree_type);
		    if (check_end (yyvsp[0].tok_val, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    tree_if_command *t1 = new tree_if_command
					    (yyvsp[-1].tree_command_list_type, yyvsp[-3].tok_val->line (), yyvsp[-3].tok_val->column ());
		    yyval.tree_command_type = t1->chain (yyvsp[-6].tree_type, yyvsp[-4].tree_command_list_type, yyvsp[-7].tok_val->line (), yyvsp[-7].tok_val->column ());
		  ;
    break;}
case 95:
#line 551 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-4].tree_type);
		    if (check_end (yyvsp[0].tok_val, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    tree_if_command *t1 = yyvsp[-1].tree_if_command_type->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    yyval.tree_command_type = t1->chain (yyvsp[-4].tree_type, yyvsp[-2].tree_command_list_type, yyvsp[-5].tok_val->line (), yyvsp[-5].tok_val->column ());
		  ;
    break;}
case 96:
#line 562 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-7].tree_type);
		    if (check_end (yyvsp[0].tok_val, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    // Add the else list to the head of the elseif list,
		    // then reverse the list.
		    tree_if_command *t1 = yyvsp[-4].tree_if_command_type->chain (yyvsp[-1].tree_command_list_type, yyvsp[-3].tok_val->line (),
						     yyvsp[-3].tok_val->column ());
		    t1 = t1->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    yyval.tree_command_type = t1->chain (yyvsp[-7].tree_type, yyvsp[-5].tree_command_list_type, yyvsp[-8].tok_val->line (), yyvsp[-8].tok_val->column ());
		  ;
    break;}
case 97:
#line 577 "parse.y"
{
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("break: only meaningful within a `for'\
 or `while' loop");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_break_command (yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ());
		  ;
    break;}
case 98:
#line 588 "parse.y"
{
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("continue: only meaningful within a\
 `for' or `while' loop");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_continue_command (yyvsp[0].tok_val->line (),
						    yyvsp[0].tok_val->column ());
		  ;
    break;}
case 99:
#line 600 "parse.y"
{
		    if (!defining_func)
		      {
			yyerror ("parse error");
			error ("return: only meaningful within a function");
			ABORT_PARSE;
		      }
		    yyval.tree_command_type = new tree_return_command (yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ());
		  ;
    break;}
case 100:
#line 612 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-2].tree_type);
		    yyval.tree_if_command_type = new tree_if_command (yyvsp[-2].tree_type, yyvsp[0].tree_command_list_type, yyvsp[-4].tok_val->line (),
					      yyvsp[-4].tok_val->column ());
		  ;
    break;}
case 101:
#line 618 "parse.y"
{
		    maybe_warn_assign_as_truth_value (yyvsp[-2].tree_type);
		    yyval.tree_if_command_type = yyvsp[-5].tree_if_command_type->chain (yyvsp[-2].tree_type, yyvsp[0].tree_command_list_type, yyvsp[-4].tok_val->line (), yyvsp[-4].tok_val->column ());
		  ;
    break;}
case 110:
#line 637 "parse.y"
{ maybe_screwed_again++; ;
    break;}
case 111:
#line 641 "parse.y"
{ yyval.tree_type = new tree_simple_assignment_expression
		      (yyvsp[-2].tree_index_expression_type, yyvsp[0].tree_type, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 112:
#line 644 "parse.y"
{

// Will need a way to convert the matrix list to a list of
// identifiers.	 If that fails, we can abort here, without losing
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
		      yyval.tree_type = new tree_multi_assignment_expression
			(id_list, yyvsp[0].tree_type, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ());
		  ;
    break;}
case 113:
#line 668 "parse.y"
{
		    yyerror ("parse error");
		    error ("invalid assignment to a number");
		    yyval.tree_type = (tree_simple_assignment_expression *) NULL;
		    ABORT_PARSE;
		  ;
    break;}
case 114:
#line 675 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 115:
#line 679 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 116:
#line 681 "parse.y"
{ yyval.tree_type = new tree_postfix_expression
		      (yyvsp[-1].tree_identifier_type, tree::increment, yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ()); ;
    break;}
case 117:
#line 684 "parse.y"
{ yyval.tree_type = new tree_postfix_expression
		      (yyvsp[-1].tree_identifier_type, tree::decrement, yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ()); ;
    break;}
case 118:
#line 687 "parse.y"
{ yyval.tree_type = new tree_unary_expression
		      (yyvsp[-1].tree_type, tree::hermitian, yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ()); ;
    break;}
case 119:
#line 690 "parse.y"
{ yyval.tree_type = new tree_unary_expression
		      (yyvsp[-1].tree_type, tree::transpose, yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ()); ;
    break;}
case 120:
#line 693 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::power, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 121:
#line 696 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::elem_pow, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 122:
#line 699 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::add, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 123:
#line 702 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::subtract, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 124:
#line 705 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::multiply, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 125:
#line 708 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::divide, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 126:
#line 711 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::el_mul, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 127:
#line 714 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::el_div, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 128:
#line 717 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::leftdiv, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 129:
#line 720 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::el_leftdiv, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 130:
#line 723 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_lt, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 131:
#line 726 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_le, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 132:
#line 729 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_eq, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 133:
#line 732 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_ge, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 134:
#line 735 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_gt, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 135:
#line 738 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::cmp_ne, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 136:
#line 741 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::and, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 137:
#line 744 "parse.y"
{ yyval.tree_type = new tree_binary_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, tree::or, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 138:
#line 749 "parse.y"
{ yyval.tree_type = new tree_constant (yyvsp[0].tok_val->number ()); ;
    break;}
case 139:
#line 751 "parse.y"
{ yyval.tree_type = new tree_constant (Complex (0.0, yyvsp[0].tok_val->number ())); ;
    break;}
case 140:
#line 753 "parse.y"
{ yyval.tree_type = new tree_constant (yyvsp[0].tok_val->string ()); ;
    break;}
case 141:
#line 755 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_word_list_command_type; ;
    break;}
case 142:
#line 757 "parse.y"
{
		    if (yyvsp[-1].tree_type->is_assignment_expression ())
		      ((tree_assignment_expression *) yyvsp[-1].tree_type) -> in_parens++;
		    yyval.tree_type = yyvsp[-1].tree_type;
		  ;
    break;}
case 143:
#line 763 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_index_expression_type; ;
    break;}
case 144:
#line 765 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_matrix_type; ;
    break;}
case 145:
#line 767 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_colon_expression_type; ;
    break;}
case 146:
#line 769 "parse.y"
{ yyval.tree_type = new tree_prefix_expression
		      (yyvsp[0].tree_identifier_type, tree::increment, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 147:
#line 772 "parse.y"
{ yyval.tree_type = new tree_prefix_expression
		      (yyvsp[0].tree_identifier_type, tree::decrement, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 148:
#line 775 "parse.y"
{ yyval.tree_type = new tree_unary_expression
		      (yyvsp[0].tree_type, tree::not, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 149:
#line 778 "parse.y"
{ yyval.tree_type = yyvsp[0].tree_type; ;
    break;}
case 150:
#line 780 "parse.y"
{ yyval.tree_type = new tree_unary_expression
		      (yyvsp[0].tree_type, tree::uminus, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 151:
#line 785 "parse.y"
{ yyval.tree_colon_expression_type = new tree_colon_expression
		      (yyvsp[-2].tree_type, yyvsp[0].tree_type, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ()); ;
    break;}
case 152:
#line 788 "parse.y"
{
		    yyval.tree_colon_expression_type = yyvsp[-2].tree_colon_expression_type->chain (yyvsp[0].tree_type);
		    if (yyval.tree_colon_expression_type == (tree_colon_expression *) NULL)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  ;
    break;}
case 153:
#line 799 "parse.y"
{ yyval.tree_word_list_command_type = new tree_word_list_command (yyvsp[-1].tree_identifier_type, yyvsp[0].tree_word_list_type); ;
    break;}
case 154:
#line 803 "parse.y"
{ yyval.tree_word_list_type = yyvsp[0].tree_word_list_type->reverse (); ;
    break;}
case 155:
#line 807 "parse.y"
{ yyval.tree_word_list_type = new tree_word_list (yyvsp[0].tok_val->string ()); ;
    break;}
case 156:
#line 809 "parse.y"
{ yyval.tree_word_list_type = yyvsp[-1].tree_word_list_type->chain (yyvsp[0].tok_val->string ()); ;
    break;}
case 157:
#line 815 "parse.y"
{ curr_sym_tab = global_sym_tab; ;
    break;}
case 158:
#line 819 "parse.y"
{ curr_sym_tab = tmp_local_sym_tab; ;
    break;}
case 159:
#line 823 "parse.y"
{ maybe_screwed = 0; ;
    break;}
case 160:
#line 827 "parse.y"
{ maybe_screwed = 1; ;
    break;}
case 161:
#line 831 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    yyval.tree_function_type = (tree_function *) NULL;
		  ;
    break;}
case 162:
#line 837 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    yyval.tree_function_type = (tree_function *) NULL;
		  ;
    break;}
case 163:
#line 845 "parse.y"
{
		    tree_identifier *tmp = new tree_identifier
		      (yyvsp[-4].tok_val->sym_rec (), yyvsp[-4].tok_val->line (), yyvsp[-4].tok_val->column ());
		    tree_parameter_list *tpl = new tree_parameter_list (tmp);
		    tpl = tpl->reverse ();
		    tpl->mark_as_formal_parameters ();
		    yyval.tree_function_type = yyvsp[0].tree_function_type->define_ret_list (tpl);
		  ;
    break;}
case 164:
#line 854 "parse.y"
{
		    tree_parameter_list *tpl = yyvsp[-4].tree_parameter_list_type->reverse ();
		    tpl->mark_as_formal_parameters ();
		    yyval.tree_function_type = yyvsp[0].tree_function_type->define_ret_list (tpl);
		  ;
    break;}
case 165:
#line 862 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 166:
#line 864 "parse.y"
{ yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type->chain (yyvsp[0].tree_identifier_type); ;
    break;}
case 167:
#line 868 "parse.y"
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
			  {
			    warning ("function name `%s' does not agree\
 with M-file name `%s.m'", id_name, curr_m_file_name);

			    yyvsp[-3].tree_identifier_type->rename (curr_m_file_name);
			    id_name = yyvsp[-3].tree_identifier_type->name ();
			  }

			yyvsp[0].tree_function_type->stash_m_file_name (curr_m_file_name);
			yyvsp[0].tree_function_type->stash_m_file_time (time ((time_t *) NULL));
			yyvsp[0].tree_function_type->mark_as_system_m_file ();
		      }
		    else if (! input_from_tmp_history_file
			     && reading_script_file
			     && strcmp (curr_m_file_name, id_name) == 0)
		      {
			warning ("function `%s' defined within\
 script file `%s.m'", id_name, curr_m_file_name);
		      }

		    top_level_sym_tab->clear (id_name);

		    yyvsp[0].tree_function_type->stash_function_name (id_name);

		    yyvsp[-3].tree_identifier_type->define (yyvsp[0].tree_function_type);
		    yyvsp[-3].tree_identifier_type->document (help_buf);

		    yyval.tree_function_type = yyvsp[0].tree_function_type;
		  ;
    break;}
case 168:
#line 916 "parse.y"
{
		    tree_function *fcn = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab);
		    yyval.tree_function_type = fcn->define_param_list (yyvsp[-3].tree_parameter_list_type);
		  ;
    break;}
case 169:
#line 921 "parse.y"
{ yyval.tree_function_type = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab); ;
    break;}
case 170:
#line 923 "parse.y"
{ yyval.tree_function_type = new tree_function (yyvsp[-1].tree_command_list_type, curr_sym_tab); ;
    break;}
case 171:
#line 927 "parse.y"
{
		    if (check_end (yyvsp[0].tok_val, token::function_end))
			ABORT_PARSE;

		    if (reading_m_file)
		      check_for_garbage_after_fcn_def ();
		  ;
    break;}
case 172:
#line 935 "parse.y"
{
		    if (! (reading_m_file || reading_script_file))
		      YYABORT;
		  ;
    break;}
case 173:
#line 942 "parse.y"
{
		    yyval.tree_index_expression_type = new tree_index_expression
			   (yyvsp[0].tree_identifier_type, yyvsp[0].tree_identifier_type->line (), yyvsp[0].tree_identifier_type->column ());
		  ;
    break;}
case 174:
#line 947 "parse.y"
{
		    yyval.tree_index_expression_type = new tree_index_expression
			   (yyvsp[-3].tree_identifier_type, yyvsp[-1].tree_argument_list_type, yyvsp[-3].tree_identifier_type->line (), yyvsp[-3].tree_identifier_type->column ());
		  ;
    break;}
case 175:
#line 952 "parse.y"
{
		    yyval.tree_index_expression_type = new tree_index_expression
		           (yyvsp[-2].tree_identifier_type, (tree_argument_list *) NULL,
			    yyvsp[-2].tree_identifier_type->line (), yyvsp[-2].tree_identifier_type->column ());
		  ;
    break;}
case 176:
#line 958 "parse.y"
{
		    yyerror ("parse error");
		    error ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    yyval.tree_index_expression_type = (tree_index_expression *) NULL;
		    ABORT_PARSE;
		  ;
    break;}
case 177:
#line 968 "parse.y"
{
		    tree_parameter_list *tmp = yyvsp[-1].tree_parameter_list_type->reverse ();
		    tmp->mark_as_formal_parameters ();
		    yyval.tree_parameter_list_type = tmp;
		  ;
    break;}
case 178:
#line 975 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 179:
#line 977 "parse.y"
{ yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type->chain (yyvsp[0].tree_identifier_type); ;
    break;}
case 180:
#line 979 "parse.y"
{
		    error ("parameter lists may only contain identifiers");
		    yyval.tree_parameter_list_type = (tree_parameter_list *) NULL;
		  ;
    break;}
case 181:
#line 984 "parse.y"
{
		    error ("parameter lists may only contain identifiers");
		    yyval.tree_parameter_list_type = (tree_parameter_list *) NULL;
		  ;
    break;}
case 182:
#line 991 "parse.y"
{ yyval.tree_identifier_type = new tree_identifier
		      (yyvsp[0].tok_val->sym_rec (), yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ()); ;
    break;}
case 183:
#line 995 "parse.y"
{ yyval.tree_argument_list_type = yyvsp[0].tree_argument_list_type->reverse (); ;
    break;}
case 184:
#line 999 "parse.y"
{
		    tree_constant *colon;
		    colon = new tree_constant (tree_constant_rep::magic_colon);
		    yyval.tree_argument_list_type = new tree_argument_list (colon);
		  ;
    break;}
case 185:
#line 1005 "parse.y"
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
case 186:
#line 1016 "parse.y"
{ yyval.tree_argument_list_type = new tree_argument_list (yyvsp[0].tree_type); ;
    break;}
case 187:
#line 1018 "parse.y"
{
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type->chain (yyvsp[0].tree_type);
		    if (yyval.tree_argument_list_type == NULL_TREE)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  ;
    break;}
case 188:
#line 1029 "parse.y"
{
		    mlnm.pop ();
		    yyval.tree_matrix_type = new tree_matrix ();
		  ;
    break;}
case 189:
#line 1034 "parse.y"
{
		    mlnm.pop ();
		    yyval.tree_matrix_type = new tree_matrix ();
		  ;
    break;}
case 190:
#line 1039 "parse.y"
{
		    mlnm.pop ();
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    yyval.tree_matrix_type = tmp->reverse ();
		  ;
    break;}
case 194:
#line 1053 "parse.y"
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
case 196:
#line 1070 "parse.y"
{
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->chain (yyvsp[0].tree_type, tree::md_right);
		    ml.push (tmp);
		  ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/local/gnu/lib/bison.simple"

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
#line 1077 "parse.y"


static void
yyerror (char *s)
{
  char *line = current_input_line;
  int err_col = current_input_column - 1;
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

static int
check_end (token *tok, token::end_tok_type expected)
{
  token::end_tok_type ettype = tok->ettype ();
  if (ettype != expected && ettype != token::simple_end)
    {
      yyerror ("parse error");

      int l = tok->line ();
      int c = tok->column ();

      switch (expected)
	{
	case token::for_end:
	  end_error ("for", ettype, l, c);
	  break;
	case token::function_end:
	  end_error ("function", ettype, l, c);
	  break;
	case token::if_end:
	  end_error ("if", ettype, l, c);
	  break;
	case token::while_end:
	  end_error ("while", ettype, l, c);
	  break;
	default:
	  panic_impossible ();
	  break;
	}
      return 1;
    }
  else
    return 0;
}

static void
end_error (char *type, token::end_tok_type ettype, int l, int c)
{
  static char *fmt = "%s command matched by `%s' near line %d column %d";

  switch (ettype)
    {
    case token::simple_end:
      error (fmt, type, "end", l, c);
      break;
    case token::for_end:
      error (fmt, type, "endfor", l, c);
      break;
    case token::function_end:
      error (fmt, type, "endfunction", l, c);
      break;
    case token::if_end:
      error (fmt, type, "endif", l, c);
      break;
    case token::while_end:
      error (fmt, type, "endwhile", l, c); 
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
 *
 * XXX FIXME XXX.  This isn't quite sufficient.  For example, try the
 * command `x = 4, x' for `x' previously undefined.
 *
 * XXX FIXME XXX -- we should probably delay doing this until eval-time.
 */
tree *
maybe_convert_to_ans_assign (tree *expr)
{
  if (expr->is_index_expression ())
    {
      expr->mark_for_possible_ans_assign ();
      return expr;
    }
  else if (expr->is_assignment_expression ()
	   || expr->is_prefix_expression ())
    {
      return expr;
    }
  else
    {
      symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

      assert (sr != (symbol_record *) NULL);
      
      tree_identifier *ans = new tree_identifier (sr);

      return new tree_simple_assignment_expression (ans, expr);
    }
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

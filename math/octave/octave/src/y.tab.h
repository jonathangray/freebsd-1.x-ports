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


extern YYSTYPE yylval;

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


extern YYSTYPE yylval;

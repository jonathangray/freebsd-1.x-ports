#define INT_CONST 257
#define STR 258
typedef union
{
	int	yyint;		/* integer */
	char	*yystr;		/* string */
	char	yypip;		/* pipe id */
	char	yychar;		/* single character */
} YYSTYPE;
extern YYSTYPE yylval;

/* ci_tokens.h - yacc generation token #defines */

/* Copyright 1989 Kent Software Tools Centre */

/* %W% %G% (UKC) */

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
extern YYSTYPE yylval;
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

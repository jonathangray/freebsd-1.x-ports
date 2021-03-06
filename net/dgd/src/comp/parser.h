#define FOR 257
#define VOID 258
#define DO 259
#define CONTINUE 260
#define INHERIT 261
#define MAPPING 262
#define INT 263
#define LOCK 264
#define NOMASK 265
#define CATCH 266
#define CASE 267
#define FLOAT 268
#define BREAK 269
#define MIXED 270
#define STATIC 271
#define VARARGS 272
#define STRING 273
#define ELSE 274
#define IF 275
#define WHILE 276
#define SWITCH 277
#define DEFAULT 278
#define RETURN 279
#define PRIVATE 280
#define OBJECT 281
#define ARROW 282
#define PLUS_PLUS 283
#define MIN_MIN 284
#define LSHIFT 285
#define RSHIFT 286
#define LE 287
#define GE 288
#define EQ 289
#define NE 290
#define LAND 291
#define LOR 292
#define PLUS_EQ 293
#define MIN_EQ 294
#define MULT_EQ 295
#define DIV_EQ 296
#define MOD_EQ 297
#define LSHIFT_EQ 298
#define RSHIFT_EQ 299
#define AND_EQ 300
#define XOR_EQ 301
#define OR_EQ 302
#define COLON_COLON 303
#define DOT_DOT 304
#define ELLIPSIS 305
#define STRING_CONST 306
#define IDENTIFIER 307
#define INT_CONST 308
#define FLOAT_CONST 309
#define MARK 310
#define HASH 311
#define HASH_HASH 312
#define INCL_CONST 313
#define NR_TOKENS 314
typedef union {
    Int number;			/* lex input */
    xfloat real;		/* lex input */
    unsigned short type;	/* internal */
    struct _node_ *node;	/* internal */
} YYSTYPE;
extern YYSTYPE yylval;

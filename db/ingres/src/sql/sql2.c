#ifndef lint
/*static char yysccsid[] = "from: @(#)yaccpar	1.9 (Berkeley) 02/21/93";*/
static char yyrcsid[] = "$Id: sql2.c,v 1.2 1994/06/19 05:38:34 alm Exp $";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
#define YYPREFIX "yy"
#line 3 "sql2.y"
typedef union {
	int intval;
	double floatval;
	char *strval;
	int subtok;
} YYSTYPE;
#line 20 "y.tab.c"
#define NAME 257
#define STRING 258
#define INTNUM 259
#define APPROXNUM 260
#define OR 261
#define AND 262
#define NOT 263
#define COMPARISON 264
#define UMINUS 265
#define ALL 266
#define AMMSC 267
#define ANY 268
#define AS 269
#define ASC 270
#define AUTHORIZATION 271
#define BETWEEN 272
#define BY 273
#define CHARACTER 274
#define CHECK 275
#define CLOSE 276
#define COMMIT 277
#define CONTINUE 278
#define CREATE 279
#define CURRENT 280
#define CURSOR 281
#define DECIMAL 282
#define DECLARE 283
#define DEFAULT 284
#define DELETE 285
#define DESC 286
#define DISTINCT 287
#define DOUBLE 288
#define ESCAPE 289
#define EXISTS 290
#define FETCH 291
#define FLOAT 292
#define FOR 293
#define FOREIGN 294
#define FOUND 295
#define FROM 296
#define GOTO 297
#define GRANT 298
#define GROUP 299
#define HAVING 300
#define IN 301
#define INDICATOR 302
#define INSERT 303
#define INTEGER 304
#define INTO 305
#define IS 306
#define KEY 307
#define LANGUAGE 308
#define LIKE 309
#define NULLX 310
#define NUMERIC 311
#define OF 312
#define ON 313
#define OPEN 314
#define OPTION 315
#define ORDER 316
#define PARAMETER 317
#define PRECISION 318
#define PRIMARY 319
#define PRIVILEGES 320
#define PROCEDURE 321
#define PUBLIC 322
#define REAL 323
#define REFERENCES 324
#define ROLLBACK 325
#define SCHEMA 326
#define SELECT 327
#define SET 328
#define SMALLINT 329
#define SOME 330
#define SQLCODE 331
#define SQLERROR 332
#define TABLE 333
#define TO 334
#define UNION 335
#define UNIQUE 336
#define UPDATE 337
#define USER 338
#define VALUES 339
#define VIEW 340
#define WHENEVER 341
#define WHERE 342
#define WITH 343
#define WORK 344
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    1,    2,    4,    4,    5,    5,    6,    6,
    6,    7,   11,   11,   12,   12,   13,   17,   17,   18,
   18,   18,   18,   18,   18,   18,   18,   18,   14,   14,
   14,   14,   14,   21,   21,    8,   24,   24,   22,   22,
    9,   27,   27,   25,   25,   25,   28,   28,   29,   29,
   29,   29,   29,   26,   26,   30,   30,    1,   31,   34,
   34,   35,   35,   36,   36,   37,   37,   37,    1,   39,
   39,   39,   39,   39,   39,   39,   39,   39,   39,   39,
   40,   41,   42,   43,   44,   45,   53,   53,   54,   54,
   55,   55,   46,   47,   48,   57,   57,   57,   49,   60,
   60,   60,   61,   61,   50,   52,   52,   63,   51,   51,
   33,   33,   33,   66,   66,   23,   58,   58,   59,   68,
   71,   71,   72,   72,   65,   69,   69,   74,   74,   70,
   70,   20,   20,   20,   20,   20,   20,   75,   75,   75,
   75,   75,   75,   75,   76,   76,   77,   77,   78,   78,
   84,   84,   79,   79,   80,   80,   80,   80,   85,   85,
   81,   86,   86,   86,   82,   83,   62,   62,   62,   62,
   62,   62,   62,   62,   62,   62,   67,   67,   56,   56,
   56,   64,   64,   64,   87,   87,   87,   87,   19,   19,
   19,   10,   10,   38,   38,   38,   16,   16,   16,   16,
   16,   16,   16,   16,   16,   16,   16,   16,   16,   16,
   15,   32,   88,   73,    3,    1,    1,   89,   89,
};
short yylen[] = {                                         2,
    2,    3,    1,    5,    0,    1,    1,    2,    1,    1,
    1,    6,    1,    3,    1,    1,    3,    0,    2,    2,
    3,    4,    2,    2,    2,    4,    2,    5,    4,    5,
    7,   10,    4,    1,    3,    7,    0,    3,    0,    3,
    7,    0,    3,    2,    1,    1,    1,    3,    1,    1,
    1,    2,    2,    1,    3,    1,    1,    1,    6,    0,
    3,    1,    3,    2,    2,    0,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    2,    2,    7,    4,    4,    5,    4,    1,    1,    3,
    1,    1,    2,    2,    6,    0,    1,    1,    8,    0,
    1,    3,    3,    3,    5,    1,    3,    1,    0,    1,
    1,    3,    4,    1,    3,    4,    1,    1,    4,    2,
    1,    3,    1,    2,    2,    0,    3,    1,    3,    0,
    2,    0,    3,    3,    2,    3,    1,    1,    1,    1,
    1,    1,    1,    1,    3,    3,    6,    5,    5,    4,
    0,    2,    4,    3,    6,    5,    6,    5,    1,    3,
    4,    1,    1,    1,    2,    6,    3,    3,    3,    3,
    2,    2,    1,    1,    1,    3,    1,    3,    1,    1,
    1,    1,    2,    3,    4,    5,    5,    4,    1,    1,
    1,    1,    3,    1,    3,    5,    1,    4,    1,    4,
    6,    1,    4,    6,    1,    1,    1,    4,    1,    2,
    1,    1,    1,    1,    1,    4,    3,    2,    1,
};
short yydefred[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    3,   58,   69,   70,   71,   72,
   73,   74,   75,   76,   77,   78,   79,   80,  212,   81,
   82,    0,    0,    0,    0,    0,   93,   94,   97,   98,
    0,    0,    0,    0,    0,    0,    1,    0,    0,    0,
    0,    0,    0,  189,  190,  191,    0,    0,  118,    0,
  213,  181,    0,  180,  174,  173,    0,    0,  179,    0,
  175,    0,    0,    0,    0,  219,    0,  217,    2,  215,
    0,    0,    0,   84,  110,    0,  106,  108,    0,    0,
    0,  171,  172,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  183,  193,  211,    0,    0,  101,  216,  218,
    0,    0,    4,    0,    7,    9,   10,   11,    0,    0,
  114,    0,  111,    0,    0,    0,    0,    0,    0,    0,
  137,  138,  139,  140,  141,  142,  143,  144,    0,   34,
    0,    0,   88,   86,    0,    0,    0,    0,    0,  176,
    0,    0,    0,  169,  170,    0,  184,    0,    0,    0,
  105,    0,    0,    0,   51,   50,    0,   49,    0,    0,
    0,   47,    8,    0,    0,    0,    0,   59,  135,    0,
    0,  165,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  107,   40,    0,    0,    0,  185,    0,    0,
  188,    0,   95,    0,  104,    0,    0,  102,    0,    0,
   44,   53,   52,    0,    0,    0,  115,    0,    0,  112,
   83,    0,  136,    0,  134,    0,  154,    0,    0,    0,
  163,  162,  164,    0,    0,  146,    0,    0,    0,    0,
   35,   92,    0,   89,   91,  196,  187,  186,    0,    0,
  121,    0,    0,    0,    0,    0,    0,   48,  116,    0,
    0,   62,    0,  113,    0,  153,    0,    0,    0,  161,
    0,  159,    0,    0,    0,  150,   87,    0,  214,  124,
    0,    0,    0,   99,    0,    0,    0,    0,    0,   13,
   15,   16,    0,    0,    0,   67,   68,   64,    0,   65,
    0,    0,    0,    0,  149,    0,  156,  158,    0,  152,
   90,  122,    0,    0,  119,    0,    0,    0,    0,   12,
    0,    0,    0,    0,    0,  205,    0,  209,  206,   18,
    0,   56,   57,    0,   54,   63,    0,    0,  155,  157,
  160,  128,    0,    0,    0,    0,    0,    0,   14,    0,
    0,  210,    0,    0,    0,    0,   36,    0,    0,   41,
  166,    0,   33,    0,    0,   29,    0,    0,    0,    0,
    0,    0,    0,    0,   19,    0,    0,   55,  129,    0,
   30,  198,  203,    0,  208,  200,    0,    0,    0,   24,
   25,   23,    0,   38,   43,    0,    0,    0,    0,   21,
    0,    0,    0,  204,  201,   22,   26,    0,    0,   28,
    0,   32,
};
short yydgoto[] = {                                      13,
   14,   15,  333,  113,  114,  115,  116,  117,  118,  249,
  289,  290,  291,  292,  140,  330,  355,  375,   64,  128,
  141,   90,  121,  357,  170,  334,  360,  171,  172,  335,
   16,   30,  122,  178,  261,  262,  298,   65,   17,   18,
   19,   20,   21,   22,   23,   24,   25,   26,   27,   28,
   84,   86,  144,  243,  244,   66,   41,   67,  203,  107,
  108,  130,   87,   69,   85,  123,   70,  204,  283,  315,
  250,  251,  280,  343,  131,  132,  133,  134,  135,  136,
  137,  138,  182,  276,  274,  237,   71,   72,   78,
};
short yysindex[] = {                                    658,
 -193, -269, -261, -193, -215, -193, -185, -193, -202, -225,
 -109, -235,  658,  110,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  -90,  -89, -109, -107, -109,    0,    0,    0,    0,
  608,  175, -122,  -80, -125,  170,    0,  -22,  -65, -101,
  -70,  220,  203,    0,    0,    0,  653,  653,    0,  246,
    0,    0,  653,    0,    0,    0,  -12,  398,    0,  255,
    0, -207,   60,   72, -125,    0,   97,    0,    0,    0,
 -111,   -7,  432,    0,    0,  290,    0,    0,   72, -284,
  109,    0,    0,  474,  169,  -70,  653,  653,  653,  653,
  653,  -70,    0,    0,    0,  284,  -34,    0,    0,    0,
 -158, -213,    0, -111,    0,    0,    0,    0, -225,   -7,
    0, -162,    0,  580,   58,  332,  580, -214,   84,  733,
    0,    0,    0,    0,    0,    0,    0,    0,  -70,    0,
  155,  356,    0,    0,  355,  369,  653,  159,  437,    0,
  -32,  137,  137,    0,    0,  398,    0,  614,  487,   72,
    0, -109, -109,  107,    0,    0,  220,    0,  220,  129,
  388,    0,    0,  608,  -38,  180,   -4,    0,    0, -193,
  125,    0,   77,  710,  580,  580, -234, -164,  506,  653,
  417, -167,    0,    0,   72, -171,  210,    0,  775,  427,
    0, -109,    0,  127,    0,  398,  171,    0,  450,  220,
    0,    0,    0, -109, -147,  208,    0,  -71,   -7,    0,
    0, -225,    0,  236,    0,  196,    0,  653,  473, -167,
    0,    0,    0,  627,  398,    0,  332,    7,   -1,  226,
    0,    0,  156,    0,    0,    0,    0,    0,  264,  478,
    0,  580,  234, -193,  -92,  269,  216,    0,    0, -226,
  498,    0, -226,    0,  608,    0,   16,   -1,  226,    0,
  653,    0,  513,  164, -167,    0,    0, -171,    0,    0,
 -109,  285,  263,    0,  529,  268,  276,  541,  179,    0,
    0,    0,  523,  261, -223,    0,    0,    0,  -71,    0,
  208,  653,  548,  299,    0,  398,    0,    0, -167,    0,
    0,    0,  159,  580,    0,  580,  554,  555,   72,    0,
  -92,  556,  562,  286,  566,    0,  567,    0,    0,    0,
  265,    0,    0,  -39,    0,    0,  572,  398,    0,    0,
    0,    0,  570, -214,   86,   72,   72,  361,    0,  359,
  367,    0,  368,  370, -135,  346,    0,  326, -223,    0,
    0,  159,    0,  365,  392,    0,  590,  453,  591,  485,
  323,  597, -153, -109,    0,  324,  325,    0,    0,  320,
    0,    0,    0,  390,    0,    0,  399, -279,  580,    0,
    0,    0,  623,    0,    0, -109,  624,  625,  373,    0,
  103,   72,  629,    0,    0,    0,    0,  499,   72,    0,
  515,    0,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  641,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  -33,    0,    0,    0,    0,    0,    0,    0,  615,
    0, -218,   26,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  -42,    0, -183,
    0,  119,    0,  -29,    0,    0,    0,    0,    0,    0,
  616,    0,  117,    0,    0,  617,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  615,    0,    0,    0,
    0,    0,    0,  626,    0,    0,    0,    0,  641,    0,
    0,  628,    0,  202,    0,    0,  114,  312,  740,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   88,    0,    0,    0,    0,    0,
    0,  -24,  150,    0,    0,  -26,    0,    0,  117,    0,
    0,    0,    0,  360,    0,    0,  -40,    0,  -40,    0,
  375,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  202,  202,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  362,    0,  -28,    0,    0,    0,  425,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  641,    0,  287,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  209,    0,    0,    0,    0,  212,
    0,    0,    0,    0,    0,    0,    0,    0,   57,  300,
    0,  202,  160,    0,    0,    0,    0,    0,    0,  115,
  643,    0,  115,    0,    0,    0,    0,    0,  212,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  241,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  262,    0,    0,    0,    0,
    0,    0,    0,  303,    0,  114,    0,    0,    0,    0,
    0,  163,  181,    0,  393,    0,  445,    0,    0,    0,
  -53,    0,    0,  -46,    0,    0,    0,  274,    0,    0,
    0,    0,  336,  366,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  527,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  455,  114,    0,
    0,    0,  526,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  536,    0,    0,    0,    0,    0,    0,    0,
    0,    0,
};
short yygindex[] = {                                      0,
  687,    0,  655,    0,    0,  592,    0,    0,    0,  -11,
    0,  383,    0,    0,  -37,    0,    0,    0,  334,  407,
 -268,  -67,  -81,    0,    0,    0,    0,    0,  495,  352,
    0,   76,  593,    0,    0,  415,  452,  -82,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  -85,  620,    0,    0,  441,  639, -105, -150, -189,    0,
  564,  923,  586,  -13,    0, -145,    0,    0,    0,    0,
    0,  446,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -143,  457,  460,    0,    0,  -16,  660,
};
#define YYTABLESIZE 1225
short yytable[] = {                                      43,
  129,  177,  217,   39,  359,   37,  192,  192,  143,  160,
  192,  139,   42,  174,  100,  103,  167,  178,  167,  167,
  167,  161,   50,  216,   52,  192,  259,   44,  226,  100,
  103,  220,  120,   80,  167,  120,  106,   88,  181,  399,
   39,  129,  119,  296,  129,  236,  185,  186,   99,   97,
  348,   98,  164,  100,  142,  103,  400,   99,   97,  297,
   98,   40,  100,   29,   32,  200,  194,  194,  194,  194,
  194,  165,  194,  264,   31,  227,  129,  364,  365,   33,
   34,   35,   88,   37,  194,  157,   54,   55,   56,  166,
   54,   55,   56,  270,  102,  273,   45,  123,  332,  212,
  123,  213,  129,  129,   54,   55,   56,  228,   39,   61,
  167,  337,  117,  168,  301,  123,  265,  223,  253,   36,
   39,  117,  106,  169,  303,   88,  363,  371,  195,  195,
  195,  195,  195,  408,  195,  263,  229,  165,  242,  372,
  411,   38,  256,  407,  230,   61,  195,   42,  373,   61,
  209,  210,   76,  176,  132,  166,  390,  241,   66,  182,
  182,  182,  182,  182,  105,  182,   62,  111,   47,  129,
   62,   77,  177,   66,  162,  132,  167,  182,   99,  168,
   48,  163,  285,  100,  391,   53,  112,  260,  374,  169,
  168,   49,  168,  168,  168,  194,  277,   51,  195,  278,
  126,  286,  257,  197,  308,   74,  197,  309,  168,  150,
   99,   97,  331,   98,   75,  100,  263,  293,  126,  320,
   73,  202,  321,  192,  202,   37,  287,   82,   79,  192,
  342,  129,   42,  129,   80,  192,  167,  167,  167,  167,
   83,  192,  132,  288,   37,  192,   61,  167,   91,  145,
  192,   42,  151,  177,  167,  221,   54,   55,   56,   89,
  132,  219,  177,  202,  192,  192,  192,  145,  271,  178,
  151,  167,   39,  167,  167,  167,  167,  302,  178,  379,
  167,  130,  192,  293,  167,   94,  194,  194,  194,  194,
  192,  167,   96,  192,  192,  194,  177,  194,  101,  130,
  192,  192,  148,  358,  194,  192,  129,  159,  192,  192,
  167,  194,  100,  103,  147,   61,  104,  167,  167,  119,
  148,  194,  119,  194,  194,  194,  194,  133,  105,  284,
  194,  194,  147,  139,  194,  123,   62,  185,  186,  340,
  120,  194,  309,  132,  158,  133,  185,  186,  195,  195,
  195,  195,  125,  110,  123,  123,  123,  195,  120,  195,
  194,  132,  393,  185,  186,  145,  195,  194,  194,  180,
  125,  181,  123,  195,  132,  132,  127,  132,  132,  182,
  182,  182,  182,  195,  403,  195,  195,  195,  195,  187,
  182,  123,  195,  195,  127,  196,  195,  182,  123,  123,
  197,  366,  109,  195,  195,  380,  131,  182,  195,  198,
  168,  168,  168,  168,  182,   53,  182,  182,  182,  182,
  109,  168,  195,  182,  131,  197,  211,  182,  168,  195,
  195,  215,  381,  207,  182,  195,  207,  197,  126,   99,
   97,  214,   98,  202,  100,  168,  197,  168,  168,  168,
  168,  222,  218,  182,  168,  202,  239,  126,  168,  126,
  182,  182,  132,  132,  202,  168,  246,  248,  252,  145,
  145,  127,  151,  151,   57,  126,   58,  201,   99,   97,
  132,   98,  254,  100,  168,  199,  197,  145,  199,  255,
  151,  168,  168,  383,  126,   20,  384,  186,   20,  132,
  132,  132,  126,  202,  202,  266,  145,  145,  145,  151,
  151,  151,  268,   63,  275,  146,   57,  132,   58,  130,
  279,  281,  148,  148,  145,  386,  127,  151,  387,   57,
  179,   58,  282,  183,  147,  147,  132,  294,  130,  410,
  148,  299,  195,  145,  132,  234,  151,  133,   57,  295,
   58,  145,  147,  307,  151,  412,  130,  313,  195,  148,
  148,  148,  314,  132,  132,  133,   27,   17,  316,   27,
   17,  147,  147,  147,  317,  130,   31,  148,  120,   31,
  319,  132,  318,  130,  133,  133,  133,  119,  339,  147,
  125,  224,  225,  346,  347,  350,  148,  120,  120,  120,
  132,  351,  133,  352,  148,  353,  354,  356,  147,  125,
  125,  125,  361,  362,  127,  120,  147,  367,  132,  127,
  376,  133,   57,  377,   58,  368,  369,  125,  370,  133,
  382,  385,  388,  127,  120,  127,  389,  132,  394,  395,
  109,  120,  120,  396,  131,  132,  125,   63,  397,   59,
   57,  127,   58,   63,  125,  207,   57,  398,   58,  109,
  109,  109,  402,  131,  404,  405,   63,  207,  409,   57,
  127,   58,   45,  109,    5,   85,  207,  109,  127,  406,
   96,  131,   96,   96,    6,   96,   60,   46,   53,   54,
   55,   56,   63,   39,  124,   57,  109,   58,   60,   46,
  131,   61,   81,  349,  109,  173,  392,  199,  131,  258,
  378,  125,  175,  336,  300,  151,  207,   20,  311,  199,
  344,  126,  345,  208,  193,  305,  312,  304,  199,   20,
   53,   54,   55,   56,  109,    0,    0,    0,   20,  147,
   60,    0,    0,   53,   54,   55,   56,    0,   61,  124,
  150,   99,   97,   60,   98,    0,  100,    0,    0,    0,
  148,    0,   53,   54,   55,   56,  207,    0,  199,   62,
    0,  231,   60,  232,   99,   97,  126,   98,   20,  100,
  174,  174,  174,    0,  174,    0,  174,    0,   27,    0,
   61,    0,    0,    0,    0,  401,  322,    0,    0,    0,
   27,    0,    0,   61,  323,    0,    0,    0,    0,   27,
  324,   62,    0,    0,  325,  247,   99,   97,    0,   98,
    0,  100,   61,    0,   62,    0,  326,    0,    0,    0,
  240,    0,    0,  327,  245,  233,   53,   54,   55,   56,
    0,    0,  124,   62,    0,  328,   60,    0,    0,   27,
    0,  329,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   53,   54,   55,   56,  269,  126,
   53,   54,   55,   56,   60,    0,    0,  272,    0,    0,
   60,    0,    0,   53,   54,   55,   56,    0,    0,    0,
    0,    0,    0,   60,    0,    0,   61,   96,   96,   96,
   96,    0,    0,    0,    0,    0,  272,   96,    0,   53,
   54,   55,   56,  310,    0,    0,  245,   62,    0,   60,
    0,    0,    0,  205,   61,    0,    0,    0,    0,    0,
   61,    0,    0,    1,    2,    0,    3,    0,    0,    0,
    4,    0,    5,   61,    0,   62,    0,  341,    6,    0,
    0,   62,    0,  222,    0,    0,    0,   96,    0,    0,
    7,    0,    0,   68,   62,    0,    0,    0,    0,   61,
    0,    8,  188,  189,    0,    0,    0,    0,   96,   92,
   93,  190,    9,    0,   10,   95,    0,    0,    0,    0,
   62,    0,    0,    0,   11,  188,  189,    0,   12,    0,
    0,    0,  174,  174,  190,    0,    0,    0,    0,    0,
  191,  174,    0,    0,    0,    0,  149,    0,  192,  152,
  153,  154,  155,  156,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  191,    0,    0,    0,    0,    0,    0,
  174,  192,    0,    0,    0,    0,    0,    0,  174,  184,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  199,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  206,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   68,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  235,  238,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  267,    0,    0,    0,    0,    0,   95,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   68,    0,    0,
    0,    0,    0,  306,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  338,
};
short yycheck[] = {                                      11,
   83,   44,   41,   44,   44,   59,   40,   41,   90,   44,
   44,   44,   59,  119,   44,   44,   41,   44,   43,   44,
   45,  107,   34,  174,   36,   59,  216,  263,  263,   59,
   59,  177,   40,  257,   59,   40,   74,   51,   40,  319,
  266,  124,  327,  270,  127,  189,  261,  262,   42,   43,
  319,   45,  266,   47,  339,   72,  336,   42,   43,  286,
   45,  287,   47,  257,  326,  148,   41,   42,   43,   44,
   45,  285,   47,  219,  344,  310,  159,  346,  347,    4,
  296,    6,   96,    8,   59,  102,  258,  259,  260,  303,
  258,  259,  260,  237,  302,  239,  332,   41,  322,  167,
   44,  169,  185,  186,  258,  259,  260,  272,  327,  317,
  324,  301,  296,  327,  265,   59,  222,   41,  204,  305,
  339,  305,  160,  337,  268,  139,   41,  263,   41,   42,
   43,   44,   45,  402,   47,  218,  301,  285,  310,  275,
  409,  344,  210,   41,  309,  317,   59,  257,  284,  317,
  162,  163,  278,  316,   41,  303,  310,  195,   44,   41,
   42,   43,   44,   45,  257,   47,  338,  279,   59,  252,
  338,  297,  335,   59,  333,   59,  324,   59,   42,  327,
  271,  340,  275,   47,  338,  257,  298,  259,  324,  337,
   41,  281,   43,   44,   45,   41,   41,  305,   44,   44,
   41,  294,  214,   41,   41,  328,   44,   44,   59,   41,
   42,   43,  294,   45,  295,   47,  299,  255,   59,   41,
   46,   41,   44,  257,   44,  279,  319,  293,   59,  263,
  313,  314,  279,  316,  257,  269,  261,  262,  263,  264,
  342,  275,   41,  336,  298,  279,  317,  272,   46,   41,
  284,  298,   41,  296,  279,  180,  258,  259,  260,   40,
   59,  266,  305,  296,  298,  299,  300,   59,  262,  296,
   59,  296,  313,  298,  299,  300,  301,  262,  305,  362,
  305,   41,  316,  321,  309,   40,  261,  262,  263,  264,
  324,  316,  305,  327,  328,  270,  335,  272,   44,   59,
  334,  335,   41,  343,  279,  339,  389,  342,  342,  343,
  335,  286,  342,  342,   41,  317,  257,  342,  343,  327,
   59,  296,  327,  298,  299,  300,  301,   41,  257,  254,
  305,  306,   59,   44,  309,  279,  338,  261,  262,   41,
   41,  316,   44,   41,   61,   59,  261,  262,  261,  262,
  263,  264,   41,  257,  298,  299,  300,  270,   59,  272,
  335,   59,  374,  261,  262,  257,  279,  342,  343,  312,
   59,   40,  316,  286,  261,  262,   41,  261,  262,  261,
  262,  263,  264,  296,  396,  298,  299,  300,  301,  306,
  272,  335,  305,  306,   59,   40,  309,  279,  342,  343,
   46,   41,   41,  316,   44,   41,   41,  289,   44,   41,
  261,  262,  263,  264,  296,  257,  298,  299,  300,  301,
   59,  272,  335,  305,   59,  263,  320,  309,  279,  342,
  343,   44,   41,   41,  316,   44,   44,  275,  279,   42,
   43,  313,   45,  263,   47,  296,  284,  298,  299,  300,
  301,  327,  273,  335,  305,  275,   40,  298,  309,  300,
  342,  343,  261,  262,  284,  316,  257,   41,  342,  261,
  262,   40,  261,  262,   43,  316,   45,   41,   42,   43,
  279,   45,  312,   47,  335,   41,  324,  279,   44,   40,
  279,  342,  343,   41,  335,   41,   44,  262,   44,  298,
  299,  300,  343,  296,  324,  310,  298,  299,  300,  298,
  299,  300,   40,   40,  289,   42,   43,  316,   45,  279,
  257,   44,  261,  262,  316,   41,   40,  316,   44,   43,
  124,   45,  299,  127,  261,  262,  335,  269,  298,   41,
  279,   44,   44,  335,  343,   40,  335,  261,   43,  334,
   45,  343,  279,   41,  343,   41,  316,  273,   44,  298,
  299,  300,  300,  261,  262,  279,   41,   41,   40,   44,
   44,  298,  299,  300,  307,  335,   41,  316,  279,   44,
   40,  279,  307,  343,  298,  299,  300,  327,   41,  316,
  279,  185,  186,   40,   40,   40,  335,  298,  299,  300,
  298,   40,  316,  318,  343,   40,   40,  343,  335,  298,
  299,  300,   41,   44,  279,  316,  343,  259,  316,   40,
  275,  335,   43,  298,   45,  259,  259,  316,  259,  343,
   41,   41,  310,  298,  335,  300,   40,  335,  315,  315,
  279,  342,  343,  324,  279,  343,  335,   40,  259,   42,
   43,  316,   45,   40,  343,  263,   43,  259,   45,  298,
  299,  300,   40,  298,   41,   41,   40,  275,   40,   43,
  335,   45,  313,   59,   59,   59,  284,  316,  343,  307,
   40,  316,   42,   43,   59,   45,   59,  313,  257,  258,
  259,  260,   40,  269,  263,   43,  335,   45,  267,   13,
  335,   59,   48,  321,  343,  114,  373,  263,  343,  215,
  359,  280,  120,  299,  263,   96,  324,  263,  278,  275,
  314,  290,  316,  160,  139,  269,  281,  268,  284,  275,
  257,  258,  259,  260,   75,   -1,   -1,   -1,  284,  266,
  267,   -1,   -1,  257,  258,  259,  260,   -1,  317,  263,
   41,   42,   43,  267,   45,   -1,   47,   -1,   -1,   -1,
  287,   -1,  257,  258,  259,  260,  280,   -1,  324,  338,
   -1,  266,  267,  268,   42,   43,  290,   45,  324,   47,
   41,   42,   43,   -1,   45,   -1,   47,   -1,  263,   -1,
  317,   -1,   -1,   -1,   -1,  389,  274,   -1,   -1,   -1,
  275,   -1,   -1,  317,  282,   -1,   -1,   -1,   -1,  284,
  288,  338,   -1,   -1,  292,   41,   42,   43,   -1,   45,
   -1,   47,  317,   -1,  338,   -1,  304,   -1,   -1,   -1,
  192,   -1,   -1,  311,  196,  330,  257,  258,  259,  260,
   -1,   -1,  263,  338,   -1,  323,  267,   -1,   -1,  324,
   -1,  329,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,  260,  230,  290,
  257,  258,  259,  260,  267,   -1,   -1,  239,   -1,   -1,
  267,   -1,   -1,  257,  258,  259,  260,   -1,   -1,   -1,
   -1,   -1,   -1,  267,   -1,   -1,  317,  257,  258,  259,
  260,   -1,   -1,   -1,   -1,   -1,  268,  267,   -1,  257,
  258,  259,  260,  275,   -1,   -1,  278,  338,   -1,  267,
   -1,   -1,   -1,  310,  317,   -1,   -1,   -1,   -1,   -1,
  317,   -1,   -1,  276,  277,   -1,  279,   -1,   -1,   -1,
  283,   -1,  285,  317,   -1,  338,   -1,  309,  291,   -1,
   -1,  338,   -1,  327,   -1,   -1,   -1,  317,   -1,   -1,
  303,   -1,   -1,   41,  338,   -1,   -1,   -1,   -1,  317,
   -1,  314,  263,  264,   -1,   -1,   -1,   -1,  338,   57,
   58,  272,  325,   -1,  327,   63,   -1,   -1,   -1,   -1,
  338,   -1,   -1,   -1,  337,  263,  264,   -1,  341,   -1,
   -1,   -1,  263,  264,  272,   -1,   -1,   -1,   -1,   -1,
  301,  272,   -1,   -1,   -1,   -1,   94,   -1,  309,   97,
   98,   99,  100,  101,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  301,   -1,   -1,   -1,   -1,   -1,   -1,
  301,  309,   -1,   -1,   -1,   -1,   -1,   -1,  309,  127,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  147,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  158,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  174,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  189,  190,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  228,   -1,   -1,   -1,   -1,   -1,  234,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  265,   -1,   -1,
   -1,   -1,   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  302,
};
#define YYFINAL 13
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 344
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'","'*'","'+'","','","'-'","'.'","'/'",0,0,0,0,0,0,0,0,0,0,
0,"';'",0,"'='",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,"NAME","STRING","INTNUM","APPROXNUM","OR","AND","NOT","COMPARISON",
"UMINUS","ALL","AMMSC","ANY","AS","ASC","AUTHORIZATION","BETWEEN","BY",
"CHARACTER","CHECK","CLOSE","COMMIT","CONTINUE","CREATE","CURRENT","CURSOR",
"DECIMAL","DECLARE","DEFAULT","DELETE","DESC","DISTINCT","DOUBLE","ESCAPE",
"EXISTS","FETCH","FLOAT","FOR","FOREIGN","FOUND","FROM","GOTO","GRANT","GROUP",
"HAVING","IN","INDICATOR","INSERT","INTEGER","INTO","IS","KEY","LANGUAGE",
"LIKE","NULLX","NUMERIC","OF","ON","OPEN","OPTION","ORDER","PARAMETER",
"PRECISION","PRIMARY","PRIVILEGES","PROCEDURE","PUBLIC","REAL","REFERENCES",
"ROLLBACK","SCHEMA","SELECT","SET","SMALLINT","SOME","SQLCODE","SQLERROR",
"TABLE","TO","UNION","UNIQUE","UPDATE","USER","VALUES","VIEW","WHENEVER",
"WHERE","WITH","WORK",
};
char *yyrule[] = {
"$accept : sql_list",
"sql_list : sql ';'",
"sql_list : sql_list sql ';'",
"sql : schema",
"schema : CREATE SCHEMA AUTHORIZATION user opt_schema_element_list",
"opt_schema_element_list :",
"opt_schema_element_list : schema_element_list",
"schema_element_list : schema_element",
"schema_element_list : schema_element_list schema_element",
"schema_element : base_table_def",
"schema_element : view_def",
"schema_element : privilege_def",
"base_table_def : CREATE TABLE table '(' base_table_element_commalist ')'",
"base_table_element_commalist : base_table_element",
"base_table_element_commalist : base_table_element_commalist ',' base_table_element",
"base_table_element : column_def",
"base_table_element : table_constraint_def",
"column_def : column data_type column_def_opt_list",
"column_def_opt_list :",
"column_def_opt_list : column_def_opt_list column_def_opt",
"column_def_opt : NOT NULLX",
"column_def_opt : NOT NULLX UNIQUE",
"column_def_opt : NOT NULLX PRIMARY KEY",
"column_def_opt : DEFAULT literal",
"column_def_opt : DEFAULT NULLX",
"column_def_opt : DEFAULT USER",
"column_def_opt : CHECK '(' search_condition ')'",
"column_def_opt : REFERENCES table",
"column_def_opt : REFERENCES table '(' column_commalist ')'",
"table_constraint_def : UNIQUE '(' column_commalist ')'",
"table_constraint_def : PRIMARY KEY '(' column_commalist ')'",
"table_constraint_def : FOREIGN KEY '(' column_commalist ')' REFERENCES table",
"table_constraint_def : FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')'",
"table_constraint_def : CHECK '(' search_condition ')'",
"column_commalist : column",
"column_commalist : column_commalist ',' column",
"view_def : CREATE VIEW table opt_column_commalist AS query_spec opt_with_check_option",
"opt_with_check_option :",
"opt_with_check_option : WITH CHECK OPTION",
"opt_column_commalist :",
"opt_column_commalist : '(' column_commalist ')'",
"privilege_def : GRANT privileges ON table TO grantee_commalist opt_with_grant_option",
"opt_with_grant_option :",
"opt_with_grant_option : WITH GRANT OPTION",
"privileges : ALL PRIVILEGES",
"privileges : ALL",
"privileges : operation_commalist",
"operation_commalist : operation",
"operation_commalist : operation_commalist ',' operation",
"operation : SELECT",
"operation : INSERT",
"operation : DELETE",
"operation : UPDATE opt_column_commalist",
"operation : REFERENCES opt_column_commalist",
"grantee_commalist : grantee",
"grantee_commalist : grantee_commalist ',' grantee",
"grantee : PUBLIC",
"grantee : user",
"sql : cursor_def",
"cursor_def : DECLARE cursor CURSOR FOR query_exp opt_order_by_clause",
"opt_order_by_clause :",
"opt_order_by_clause : ORDER BY ordering_spec_commalist",
"ordering_spec_commalist : ordering_spec",
"ordering_spec_commalist : ordering_spec_commalist ',' ordering_spec",
"ordering_spec : INTNUM opt_asc_desc",
"ordering_spec : column_ref opt_asc_desc",
"opt_asc_desc :",
"opt_asc_desc : ASC",
"opt_asc_desc : DESC",
"sql : manipulative_statement",
"manipulative_statement : close_statement",
"manipulative_statement : commit_statement",
"manipulative_statement : delete_statement_positioned",
"manipulative_statement : delete_statement_searched",
"manipulative_statement : fetch_statement",
"manipulative_statement : insert_statement",
"manipulative_statement : open_statement",
"manipulative_statement : rollback_statement",
"manipulative_statement : select_statement",
"manipulative_statement : update_statement_positioned",
"manipulative_statement : update_statement_searched",
"close_statement : CLOSE cursor",
"commit_statement : COMMIT WORK",
"delete_statement_positioned : DELETE FROM table WHERE CURRENT OF cursor",
"delete_statement_searched : DELETE FROM table opt_where_clause",
"fetch_statement : FETCH cursor INTO target_commalist",
"insert_statement : INSERT INTO table opt_column_commalist values_or_query_spec",
"values_or_query_spec : VALUES '(' insert_atom_commalist ')'",
"values_or_query_spec : query_spec",
"insert_atom_commalist : insert_atom",
"insert_atom_commalist : insert_atom_commalist ',' insert_atom",
"insert_atom : atom",
"insert_atom : NULLX",
"open_statement : OPEN cursor",
"rollback_statement : ROLLBACK WORK",
"select_statement : SELECT opt_all_distinct selection INTO target_commalist table_exp",
"opt_all_distinct :",
"opt_all_distinct : ALL",
"opt_all_distinct : DISTINCT",
"update_statement_positioned : UPDATE table SET assignment_commalist WHERE CURRENT OF cursor",
"assignment_commalist :",
"assignment_commalist : assignment",
"assignment_commalist : assignment_commalist ',' assignment",
"assignment : column '=' scalar_exp",
"assignment : column '=' NULLX",
"update_statement_searched : UPDATE table SET assignment_commalist opt_where_clause",
"target_commalist : target",
"target_commalist : target_commalist ',' target",
"target : parameter_ref",
"opt_where_clause :",
"opt_where_clause : where_clause",
"query_exp : query_term",
"query_exp : query_exp UNION query_term",
"query_exp : query_exp UNION ALL query_term",
"query_term : query_spec",
"query_term : '(' query_exp ')'",
"query_spec : SELECT opt_all_distinct selection table_exp",
"selection : scalar_exp_commalist",
"selection : '*'",
"table_exp : from_clause opt_where_clause opt_group_by_clause opt_having_clause",
"from_clause : FROM table_ref_commalist",
"table_ref_commalist : table_ref",
"table_ref_commalist : table_ref_commalist ',' table_ref",
"table_ref : table",
"table_ref : table range_variable",
"where_clause : WHERE search_condition",
"opt_group_by_clause :",
"opt_group_by_clause : GROUP BY column_ref_commalist",
"column_ref_commalist : column_ref",
"column_ref_commalist : column_ref_commalist ',' column_ref",
"opt_having_clause :",
"opt_having_clause : HAVING search_condition",
"search_condition :",
"search_condition : search_condition OR search_condition",
"search_condition : search_condition AND search_condition",
"search_condition : NOT search_condition",
"search_condition : '(' search_condition ')'",
"search_condition : predicate",
"predicate : comparison_predicate",
"predicate : between_predicate",
"predicate : like_predicate",
"predicate : test_for_null",
"predicate : in_predicate",
"predicate : all_or_any_predicate",
"predicate : existence_test",
"comparison_predicate : scalar_exp COMPARISON scalar_exp",
"comparison_predicate : scalar_exp COMPARISON subquery",
"between_predicate : scalar_exp NOT BETWEEN scalar_exp AND scalar_exp",
"between_predicate : scalar_exp BETWEEN scalar_exp AND scalar_exp",
"like_predicate : scalar_exp NOT LIKE atom opt_escape",
"like_predicate : scalar_exp LIKE atom opt_escape",
"opt_escape :",
"opt_escape : ESCAPE atom",
"test_for_null : column_ref IS NOT NULLX",
"test_for_null : column_ref IS NULLX",
"in_predicate : scalar_exp NOT IN '(' subquery ')'",
"in_predicate : scalar_exp IN '(' subquery ')'",
"in_predicate : scalar_exp NOT IN '(' atom_commalist ')'",
"in_predicate : scalar_exp IN '(' atom_commalist ')'",
"atom_commalist : atom",
"atom_commalist : atom_commalist ',' atom",
"all_or_any_predicate : scalar_exp COMPARISON any_all_some subquery",
"any_all_some : ANY",
"any_all_some : ALL",
"any_all_some : SOME",
"existence_test : EXISTS subquery",
"subquery : '(' SELECT opt_all_distinct selection table_exp ')'",
"scalar_exp : scalar_exp '+' scalar_exp",
"scalar_exp : scalar_exp '-' scalar_exp",
"scalar_exp : scalar_exp '*' scalar_exp",
"scalar_exp : scalar_exp '/' scalar_exp",
"scalar_exp : '+' scalar_exp",
"scalar_exp : '-' scalar_exp",
"scalar_exp : atom",
"scalar_exp : column_ref",
"scalar_exp : function_ref",
"scalar_exp : '(' scalar_exp ')'",
"scalar_exp_commalist : scalar_exp",
"scalar_exp_commalist : scalar_exp_commalist ',' scalar_exp",
"atom : parameter_ref",
"atom : literal",
"atom : USER",
"parameter_ref : parameter",
"parameter_ref : parameter parameter",
"parameter_ref : parameter INDICATOR parameter",
"function_ref : AMMSC '(' '*' ')'",
"function_ref : AMMSC '(' DISTINCT column_ref ')'",
"function_ref : AMMSC '(' ALL scalar_exp ')'",
"function_ref : AMMSC '(' scalar_exp ')'",
"literal : STRING",
"literal : INTNUM",
"literal : APPROXNUM",
"table : NAME",
"table : NAME '.' NAME",
"column_ref : NAME",
"column_ref : NAME '.' NAME",
"column_ref : NAME '.' NAME '.' NAME",
"data_type : CHARACTER",
"data_type : CHARACTER '(' INTNUM ')'",
"data_type : NUMERIC",
"data_type : NUMERIC '(' INTNUM ')'",
"data_type : NUMERIC '(' INTNUM ',' INTNUM ')'",
"data_type : DECIMAL",
"data_type : DECIMAL '(' INTNUM ')'",
"data_type : DECIMAL '(' INTNUM ',' INTNUM ')'",
"data_type : INTEGER",
"data_type : SMALLINT",
"data_type : FLOAT",
"data_type : FLOAT '(' INTNUM ')'",
"data_type : REAL",
"data_type : DOUBLE PRECISION",
"column : NAME",
"cursor : NAME",
"parameter : PARAMETER",
"range_variable : NAME",
"user : NAME",
"sql : WHENEVER NOT FOUND when_action",
"sql : WHENEVER SQLERROR when_action",
"when_action : GOTO NAME",
"when_action : CONTINUE",
};
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
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
#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
#if defined(__STDC__)
yyparse(void)
#else
yyparse()
#endif
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
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
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
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
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
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
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
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 40 "sql2.y"
{ end_sql(); }
break;
case 2:
#line 41 "sql2.y"
{ end_sql(); }
break;
#line 982 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
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
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
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
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
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

extern char *malloc(), *realloc();

# line 40 "gram.y"
#include <stdio.h>
#include <ctype.h>
#include "twm.h"
#include "menus.h"
#include "list.h"
#include "util.h"
#include "screen.h"
#include "parse.h"
#include <X11/Xos.h>
#include <X11/Xmu/CharSet.h>

#define PI 3.1415926535897932
#define TWO_PI 6.2831853071795865
#define DEG_TO_RAD(d) (((float)(d) * TWO_PI) / 360.0)

static char *Action = "";
static char *Name = "";
static MenuRoot	*root, *pull = NULL;

static MenuRoot *GetRoot();

static Bool CheckWarpScreenArg(), CheckWarpRingArg();
static Bool CheckColormapArg();
static void GotButton(), GotKey(), GotTitleButton();
static char *ptr;
static name_list **list;
static int cont = 0;
static int color;
int mods = 0;
unsigned int mods_used = (ShiftMask | ControlMask | Mod1Mask);

extern int do_single_keyword(), do_string_keyword(), do_number_keyword();
extern name_list **do_colorlist_keyword();
extern int do_color_keyword();
extern int yylineno;

# line 77 "gram.y"
typedef union 
{
    int num;
    char *ptr;
} YYSTYPE;
# define LB 257
# define RB 258
# define LP 259
# define RP 260
# define AT 261
# define MENUS 262
# define MENU 263
# define PIEMENU 264
# define BUTTON 265
# define DEFAULT_FUNCTION 266
# define PLUS 267
# define MINUS 268
# define ALL 269
# define OR 270
# define CURSORS 271
# define PIXMAPS 272
# define ICONS 273
# define COLOR 274
# define MONOCHROME 275
# define FUNCTION 276
# define ICONMGR_SHOW 277
# define ICONMGR 278
# define WINDOW_FUNCTION 279
# define ZOOM 280
# define ICONMGRS 281
# define ICONMGR_GEOMETRY 282
# define ICONMGR_NOSHOW 283
# define MAKE_TITLE 284
# define ICONIFY_BY_UNMAPPING 285
# define DONT_ICONIFY_BY_UNMAPPING 286
# define STICKY 287
# define NO_TITLE 288
# define AUTO_RAISE 289
# define NO_HILITE 290
# define ICON_REGION 291
# define META 292
# define SHIFT 293
# define LOCK 294
# define CONTROL 295
# define WINDOW 296
# define TITLE 297
# define ICON 298
# define ROOT 299
# define FRAME 300
# define COLON 301
# define EQUALS 302
# define SQUEEZE_TITLE 303
# define DONT_SQUEEZE_TITLE 304
# define START_ICONIFIED 305
# define NO_TITLE_HILITE 306
# define TITLE_HILITE 307
# define MOVE 308
# define RESIZE 309
# define WAIT 310
# define SELECT 311
# define KILL 312
# define LEFT_TITLEBUTTON 313
# define RIGHT_TITLEBUTTON 314
# define NUMBER 315
# define KEYWORD 316
# define NKEYWORD 317
# define CKEYWORD 318
# define CLKEYWORD 319
# define FKEYWORD 320
# define FSKEYWORD 321
# define SKEYWORD 322
# define DKEYWORD 323
# define JKEYWORD 324
# define PKEYWORD 325
# define WINDOW_RING 326
# define WARP_CURSOR 327
# define ERRORTOKEN 328
# define NO_STACKMODE 329
# define ICON_TITLE 330
# define NO_ICON_TITLE 331
# define STRING 332
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 646 "gram.y"

yyerror(s) char *s;
{
    twmrc_error_prefix();
    fprintf (stderr, "error in input file:  %s\n", s ? s : "");
    ParseError = 1;
}
RemoveDQuote(str)
char *str;
{
    register char *i, *o;
    register n;
    register count;

    for (i=str+1, o=str; *i && *i != '\"'; o++)
    {
	if (*i == '\\')
	{
	    switch (*++i)
	    {
	    case 'n':
		*o = '\n';
		i++;
		break;
	    case 'b':
		*o = '\b';
		i++;
		break;
	    case 'r':
		*o = '\r';
		i++;
		break;
	    case 't':
		*o = '\t';
		i++;
		break;
	    case 'f':
		*o = '\f';
		i++;
		break;
	    case '0':
		if (*++i == 'x')
		    goto hex;
		else
		    --i;
	    case '1': case '2': case '3':
	    case '4': case '5': case '6': case '7':
		n = 0;
		count = 0;
		while (*i >= '0' && *i <= '7' && count < 3)
		{
		    n = (n<<3) + (*i++ - '0');
		    count++;
		}
		*o = n;
		break;
	    hex:
	    case 'x':
		n = 0;
		count = 0;
		while (i++, count++ < 2)
		{
		    if (*i >= '0' && *i <= '9')
			n = (n<<4) + (*i - '0');
		    else if (*i >= 'a' && *i <= 'f')
			n = (n<<4) + (*i - 'a') + 10;
		    else if (*i >= 'A' && *i <= 'F')
			n = (n<<4) + (*i - 'A') + 10;
		    else
			break;
		}
		*o = n;
		break;
	    case '\n':
		i++;	/* punt */
		o--;	/* to account for o++ at end of loop */
		break;
	    case '\"':
	    case '\'':
	    case '\\':
	    default:
		*o = *i++;
		break;
	    }
	}
	else
	    *o = *i++;
    }
    *o = '\0';
}

static MenuRoot *GetRoot(name, fore, back)
char *name;
char *fore, *back;
{
    MenuRoot *tmp;

    tmp = FindMenuRoot(name);
    if (tmp == NULL)
	tmp = NewMenuRoot(name);

    if (fore)
    {
	int save;

	save = Scr->FirstTime;
	Scr->FirstTime = TRUE;
	GetColor(COLOR, &tmp->hi_fore, fore);
	GetColor(COLOR, &tmp->hi_back, back);
	Scr->FirstTime = save;
    }

    return tmp;
}

static void GotButton(butt, func)
int butt, func;
{
    int i;

    for (i = 0; i < NUM_CONTEXTS; i++)
    {
	if ((cont & (1 << i)) == 0)
	    continue;

	Scr->Mouse[butt][i][mods].func = func;
	if (func == F_MENU || func == F_PIEMENU)
	{
	    pull->prev = NULL;
	    Scr->Mouse[butt][i][mods].menu = pull;
	}
	else
	{
	    root = GetRoot(TWM_ROOT, NULLSTR, NULLSTR);
	    Scr->Mouse[butt][i][mods].item = AddToMenu(root,"x",Action,
		    NULLSTR, func, NULLSTR, NULLSTR);
	}
    }
    Action = "";
    pull = NULL;
    cont = 0;
    mods_used |= mods;
    mods = 0;
}

static void GotKey(key, func)
char *key;
int func;
{
    int i;

    for (i = 0; i < NUM_CONTEXTS; i++)
    {
	if ((cont & (1 << i)) == 0) 
	  continue;
	if (!AddFuncKey(key, i, mods, func, Name, Action)) 
	  break;
    }

    Action = "";
    pull = NULL;
    cont = 0;
    mods_used |= mods;
    mods = 0;
}


static void GotTitleButton (bitmapname, func, rightside)
    char *bitmapname;
    int func;
    Bool rightside;
{
    if (!CreateTitleButton (bitmapname, func, Action, pull, rightside, True)) {
	twmrc_error_prefix();
	fprintf (stderr, 
		 "unable to create %s titlebutton \"%s\"\n",
		 rightside ? "right" : "left", bitmapname);
    }
    Action = "";
    pull = NULL;
}

static Bool CheckWarpScreenArg (s)
    register char *s;
{
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s,  WARPSCREEN_NEXT) == 0 ||
	strcmp (s,  WARPSCREEN_PREV) == 0 ||
	strcmp (s,  WARPSCREEN_BACK) == 0)
      return True;

    for (; *s && isascii(*s) && isdigit(*s); s++) ;
    return (*s ? False : True);
}


static Bool CheckWarpRingArg (s)
    register char *s;
{
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s,  WARPSCREEN_NEXT) == 0 ||
	strcmp (s,  WARPSCREEN_PREV) == 0)
      return True;

    return False;
}


static Bool CheckColormapArg (s)
    register char *s;
{
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s, COLORMAP_NEXT) == 0 ||
	strcmp (s, COLORMAP_PREV) == 0 ||
	strcmp (s, COLORMAP_DEFAULT) == 0)
      return True;

    return False;
}


twmrc_error_prefix ()
{
    fprintf (stderr, "%s:  line %d:  ", ProgramName, yylineno);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	0, 1,
	-2, 0,
-1, 14,
	257, 16,
	-2, 18,
-1, 20,
	257, 27,
	-2, 29,
-1, 23,
	257, 34,
	-2, 36,
-1, 24,
	257, 37,
	-2, 39,
-1, 25,
	257, 40,
	-2, 42,
-1, 26,
	257, 43,
	-2, 45,
-1, 27,
	257, 46,
	-2, 48,
-1, 41,
	257, 81,
	-2, 83,
-1, 46,
	257, 163,
	-2, 162,
-1, 47,
	257, 166,
	-2, 165,
-1, 261,
	257, 155,
	-2, 154,
	};
# define YYNPROD 200
# define YYLAST 426
int yyact[]={

     4,    49,   263,   139,    98,   294,   252,    33,    34,    48,
    39,    64,    65,   213,   206,    13,    12,    36,    37,    38,
    35,    22,    66,    40,    11,    21,    10,    20,    29,    14,
    19,    32,    26,    31,    24,     9,    53,   198,   140,   192,
    64,    65,    53,   105,   104,   270,   257,    46,    47,    30,
    23,    68,    49,    49,   288,   203,   169,    15,    16,   209,
    43,    45,   201,   247,   248,   281,    44,    18,   297,   271,
    42,    41,   245,    25,    28,    27,    49,    50,    51,    49,
    49,   275,   276,    59,    60,    61,   246,    49,    49,   268,
   240,   241,   242,   243,   244,   238,   126,   124,   128,   170,
   165,    83,    84,    85,   283,   142,    64,    65,    64,    65,
   143,    49,    93,    49,   165,   153,   175,   149,   103,   212,
   211,   137,   161,   162,   163,   164,    49,    62,   133,    53,
   148,   166,   131,   106,    52,   112,   161,   162,   163,   164,
    57,    55,   236,   237,   253,   159,   202,   132,   258,   146,
   147,   234,   145,   205,   207,   176,   107,   193,   156,   199,
   150,   151,   152,   154,   155,   235,   167,    89,    90,   229,
   230,   231,   232,   233,   227,    96,   178,    95,   295,   291,
    94,   282,   272,    97,   172,   210,    99,   177,   144,   101,
   141,   100,   168,   239,   171,   228,   160,   197,   195,    92,
    91,    88,    87,   130,    86,   174,   129,   204,   293,   127,
   180,   102,   280,   181,   182,   183,   184,   185,   186,   187,
   188,   189,   190,   191,   194,   108,   125,   278,    82,    81,
    80,    79,   157,   158,    78,   200,   134,    77,    76,    75,
    74,    73,    72,   111,   208,    71,   214,    70,    69,   216,
   217,   218,   219,   220,   221,   222,   223,   224,   225,   226,
    58,   256,    56,   173,    54,   249,     8,     7,   250,   251,
   254,   255,     6,     5,   179,     3,   260,     2,     1,   261,
   262,   109,   110,    67,   113,   114,   115,   116,   117,   118,
   119,   120,   121,   122,   123,    63,   196,   273,    17,     0,
     0,     0,     0,   135,   136,     0,     0,     0,   138,     0,
     0,     0,     0,     0,   215,     0,     0,     0,   267,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   259,     0,   287,     0,   289,   279,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   298,     0,     0,     0,     0,   264,   292,     0,     0,   296,
     0,     0,     0,     0,   299,     0,   265,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   269,     0,     0,   266,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   274,     0,
     0,     0,   277,     0,     0,     0,     0,     0,   284,     0,
   285,   286,     0,     0,     0,     0,   290,     0,     0,     0,
     0,     0,     0,     0,     0,   300 };
int yypact[]={

 -1000, -1000,  -256, -1000, -1000, -1000, -1000, -1000, -1000,  -331,
  -331,  -273,  -116,  -117, -1000,  -331,  -331,  -280,  -251, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  -331,  -331,  -331, -1000, -1000, -1000,  -309,
  -309, -1000, -1000, -1000,  -331,  -273, -1000, -1000,  -273, -1000,
  -319,  -273, -1000, -1000, -1000, -1000, -1000, -1000,  -139,  -258,
  -259, -1000, -1000, -1000, -1000,  -331, -1000, -1000, -1000,  -139,
  -139,  -122,  -139,  -139,  -139,  -139,  -139,  -139,  -139,  -139,
  -139,  -139,  -139,  -162,  -163, -1000,  -125,  -129,  -129, -1000,
 -1000,  -139,  -139, -1000, -1000,  -136,  -139, -1000,  -320, -1000,
  -220,  -148, -1000, -1000,  -309,  -309, -1000,  -156,  -170, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -331,  -158,  -331,  -158,  -273,  -141,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -273,
 -1000, -1000,  -331, -1000, -1000,  -331,  -331,  -331,  -331,  -331,
  -331,  -331,  -331,  -331,  -331,  -331,  -219, -1000, -1000, -1000,
 -1000,  -273, -1000, -1000, -1000, -1000, -1000,  -221,  -239, -1000,
 -1000,  -246, -1000, -1000, -1000, -1000,  -244,  -199,  -245,  -273,
 -1000,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,
  -331,  -331, -1000, -1000, -1000,  -127, -1000,  -206, -1000, -1000,
  -331,  -331,  -252,  -331,  -158,  -212, -1000, -1000,  -331, -1000,
 -1000,  -331,  -331, -1000,  -322, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -309, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -309, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -279,  -171, -1000, -1000,  -214,  -191, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  -186, -1000, -1000, -1000,  -273, -1000, -1000,
  -331,  -196,  -153,  -273, -1000,  -273,  -273, -1000,  -158,  -247,
  -158,  -273, -1000, -1000, -1000, -1000, -1000, -1000,  -331, -1000,
 -1000,  -253,  -192,  -158, -1000, -1000,  -331,  -309, -1000, -1000,
 -1000 };
int yypgo[]={

     0,    67,   127,   298,   134,   297,   295,   283,   278,   277,
   275,   273,   272,   267,   266,   264,   262,   260,   211,   248,
   247,   245,   243,   242,   241,   240,   239,   238,   237,   234,
   231,   230,   229,   228,   227,    56,   226,   212,   209,   208,
   207,   206,   205,   204,   203,   202,   147,   201,   200,   199,
   156,   198,   197,   196,   195,   193,   191,   190,   189,   188,
   187,   185,   182,   181,   179,   178,   177,   176,   175,   166,
   159,   158,   157,   155,   154,   153,   148,   146,   144 };
int yyr1[]={

     0,     8,     9,     9,    10,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    17,    10,    10,    10,
    10,    10,    10,    10,    10,    19,    10,    20,    10,    10,
    21,    10,    23,    10,    24,    10,    10,    25,    10,    10,
    26,    10,    10,    27,    10,    10,    28,    10,    10,    29,
    10,    30,    10,    31,    10,    32,    10,    33,    10,    34,
    10,    36,    10,    37,    10,    38,    10,    39,    10,    40,
    10,    41,    10,    43,    10,    45,    10,    47,    10,    10,
    10,    48,    10,    10,    49,    10,    11,    12,    13,     6,
     7,    50,    50,    53,    53,    53,    53,    53,    53,    51,
    51,    54,    54,    54,    54,    54,    54,    54,    54,    54,
    52,    52,    55,    55,    55,    55,    55,    55,    55,    55,
    55,    55,    15,    56,    56,    57,    16,    58,    58,    59,
    59,    59,    59,    59,    59,    59,    59,    59,    59,    59,
    59,    59,    59,    59,    59,    59,    59,    59,    59,    59,
    59,    46,    60,    60,    61,    62,    61,    61,    63,    64,
    64,    65,    14,    66,    14,    14,    68,    14,    67,    67,
    22,    69,    69,    70,    70,    18,    71,    71,    72,    44,
    73,    73,    74,    42,    75,    75,    76,    35,    77,    77,
    78,    78,     2,     2,     5,     5,     5,     3,     1,     4 };
int yyr2[]={

     0,     2,     0,     4,     2,     2,     2,     2,     2,    13,
     7,     5,     5,     3,     5,     5,     1,     6,     3,     9,
     9,     5,     5,     5,     5,     1,     6,     1,     6,     3,
     1,     6,     1,     6,     1,     6,     3,     1,     6,     3,
     1,     6,     3,     1,     6,     3,     1,     6,     3,     1,
     6,     1,     6,     1,     6,     1,     6,     1,     6,     1,
    19,     1,     9,     1,    19,     1,     9,     1,    23,     1,
    13,     1,     8,     1,     6,     1,     6,     1,     6,     5,
     5,     1,     6,     3,     1,     6,     3,     5,     5,    13,
    13,     0,     4,     3,     3,     3,     3,     5,     3,     0,
     4,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     0,     4,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     6,     0,     4,     5,     6,     0,     4,     7,
     5,     7,     5,     7,     5,     7,     5,     7,     5,     7,
     5,     7,     5,     7,     5,     7,     5,     7,     5,     7,
     5,     6,     0,     4,     5,     1,     8,     5,     6,     0,
     4,     5,     3,     1,    10,     3,     1,     6,     0,    11,
     6,     0,     4,     7,     9,     6,     0,     4,     3,     6,
     0,     4,     5,     6,     0,     4,     3,     6,     0,     4,
     5,    15,     3,     5,     3,     5,     5,     5,     3,     3 };
int yychk[]={

 -1000,    -8,    -9,   -10,   256,   -11,   -12,   -13,   -14,   291,
   282,   280,   272,   271,   285,   313,   314,    -3,    -1,   286,
   283,   281,   277,   306,   290,   329,   288,   331,   330,   284,
   305,   289,   287,   263,   264,   276,   273,   274,   275,   266,
   279,   327,   326,   316,   322,   317,   303,   304,   265,   332,
    -1,    -1,    -4,   315,   -15,   257,   -16,   257,   -17,    -1,
    -1,    -1,    -2,    -6,   320,   321,   302,    -7,   302,   -19,
   -20,   -21,   -23,   -24,   -25,   -26,   -27,   -28,   -29,   -30,
   -31,   -32,   -33,    -1,    -1,    -1,   -43,   -45,   -47,    -2,
    -2,   -48,   -49,    -1,    -4,   -66,   -68,    -4,   323,    -4,
   -56,   -58,   -18,   257,   302,   302,    -1,   -50,   -50,   -18,
   -18,   -22,   257,   -18,   -18,   -18,   -18,   -18,   -18,   -18,
   -18,   -18,   -18,   -18,   259,   -36,   259,   -38,   261,   -41,
   -44,   257,   -46,   257,   -46,   -18,   -18,   257,   -18,   323,
   258,   -57,   325,   258,   -59,   300,   297,   298,   278,   265,
   308,   309,   310,   263,   311,   312,   -71,    -2,    -2,   301,
   -53,   292,   293,   294,   295,   270,   301,   -69,    -1,   -35,
   257,    -1,   -35,    -4,   -42,   257,   -73,   -60,   -67,    -4,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   258,   -72,    -1,   -51,    -4,   -52,   258,   -70,
    -1,   301,   -77,   301,   -40,   -75,   258,   -74,    -1,   258,
   -61,   319,   318,   258,    -1,    -4,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,   301,   -54,   296,
   297,   298,   299,   300,   278,   292,   269,   270,   301,   -55,
   296,   297,   298,   299,   300,   278,   292,   269,   270,    -1,
    -1,    -1,   258,   -78,    -1,    -1,   -35,   258,   -76,    -2,
    -1,    -1,    -1,   324,    -2,    -2,    -4,    -1,   260,    -2,
   259,   260,   -62,    -5,    -4,   267,   268,    -4,   -34,    -1,
   -37,   261,   -63,   257,    -4,    -4,    -4,   -35,   301,   -35,
    -4,   -64,    -1,   -39,   258,   -65,    -1,   260,   -35,    -1,
    -2 };
int yydef[]={

     2,    -2,    -2,     3,     4,     5,     6,     7,     8,     0,
     0,    13,     0,     0,    -2,     0,     0,     0,     0,    25,
    -2,    30,    32,    -2,    -2,    -2,    -2,    -2,    49,    51,
    53,    55,    57,     0,     0,     0,    73,    75,    77,     0,
     0,    -2,    84,    86,     0,     0,    -2,    -2,     0,   198,
     0,    11,    12,   199,    14,   123,    15,   127,     0,     0,
     0,    21,    22,    24,   192,     0,    91,    23,    91,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    61,    65,    71,     0,     0,     0,    79,
    80,     0,     0,    87,    88,     0,     0,   197,     0,    10,
     0,     0,    17,   176,     0,     0,   193,     0,     0,    26,
    28,    31,   171,    33,    35,    38,    41,    44,    47,    50,
    52,    54,    56,    58,     0,     0,     0,     0,     0,     0,
    74,   180,    76,   152,    78,    82,    85,   168,   167,     0,
   122,   124,     0,   126,   128,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    19,    20,    99,
    92,    93,    94,    95,    96,    98,   110,     0,     0,    62,
   188,     0,    66,    69,    72,   184,     0,     0,     0,     0,
   125,   130,   132,   134,   136,   138,   140,   142,   144,   146,
   148,   150,   175,   177,   178,     0,    97,     0,   170,   172,
     0,     0,     0,     0,     0,     0,   179,   181,     0,   151,
   153,     0,     0,   164,     0,     9,   129,   131,   133,   135,
   137,   139,   141,   143,   145,   147,   149,     0,   100,   101,
   102,   103,   104,   105,   106,   107,   108,   109,     0,   111,
   112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     0,     0,   187,   189,     0,     0,    70,   183,   185,   186,
   182,    -2,   157,     0,    89,    90,   173,     0,    59,   190,
     0,    63,     0,     0,   194,     0,     0,   174,     0,     0,
     0,     0,   156,   159,   169,   195,   196,    60,     0,    64,
    67,     0,     0,     0,   158,   160,     0,     0,    68,   161,
   191 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"LB",	257,
	"RB",	258,
	"LP",	259,
	"RP",	260,
	"AT",	261,
	"MENUS",	262,
	"MENU",	263,
	"PIEMENU",	264,
	"BUTTON",	265,
	"DEFAULT_FUNCTION",	266,
	"PLUS",	267,
	"MINUS",	268,
	"ALL",	269,
	"OR",	270,
	"CURSORS",	271,
	"PIXMAPS",	272,
	"ICONS",	273,
	"COLOR",	274,
	"MONOCHROME",	275,
	"FUNCTION",	276,
	"ICONMGR_SHOW",	277,
	"ICONMGR",	278,
	"WINDOW_FUNCTION",	279,
	"ZOOM",	280,
	"ICONMGRS",	281,
	"ICONMGR_GEOMETRY",	282,
	"ICONMGR_NOSHOW",	283,
	"MAKE_TITLE",	284,
	"ICONIFY_BY_UNMAPPING",	285,
	"DONT_ICONIFY_BY_UNMAPPING",	286,
	"STICKY",	287,
	"NO_TITLE",	288,
	"AUTO_RAISE",	289,
	"NO_HILITE",	290,
	"ICON_REGION",	291,
	"META",	292,
	"SHIFT",	293,
	"LOCK",	294,
	"CONTROL",	295,
	"WINDOW",	296,
	"TITLE",	297,
	"ICON",	298,
	"ROOT",	299,
	"FRAME",	300,
	"COLON",	301,
	"EQUALS",	302,
	"SQUEEZE_TITLE",	303,
	"DONT_SQUEEZE_TITLE",	304,
	"START_ICONIFIED",	305,
	"NO_TITLE_HILITE",	306,
	"TITLE_HILITE",	307,
	"MOVE",	308,
	"RESIZE",	309,
	"WAIT",	310,
	"SELECT",	311,
	"KILL",	312,
	"LEFT_TITLEBUTTON",	313,
	"RIGHT_TITLEBUTTON",	314,
	"NUMBER",	315,
	"KEYWORD",	316,
	"NKEYWORD",	317,
	"CKEYWORD",	318,
	"CLKEYWORD",	319,
	"FKEYWORD",	320,
	"FSKEYWORD",	321,
	"SKEYWORD",	322,
	"DKEYWORD",	323,
	"JKEYWORD",	324,
	"PKEYWORD",	325,
	"WINDOW_RING",	326,
	"WARP_CURSOR",	327,
	"ERRORTOKEN",	328,
	"NO_STACKMODE",	329,
	"ICON_TITLE",	330,
	"NO_ICON_TITLE",	331,
	"STRING",	332,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"twmrc : stmts",
	"stmts : /* empty */",
	"stmts : stmts stmt",
	"stmt : error",
	"stmt : noarg",
	"stmt : sarg",
	"stmt : narg",
	"stmt : squeeze",
	"stmt : ICON_REGION string DKEYWORD DKEYWORD number number",
	"stmt : ICONMGR_GEOMETRY string number",
	"stmt : ICONMGR_GEOMETRY string",
	"stmt : ZOOM number",
	"stmt : ZOOM",
	"stmt : PIXMAPS pixmap_list",
	"stmt : CURSORS cursor_list",
	"stmt : ICONIFY_BY_UNMAPPING",
	"stmt : ICONIFY_BY_UNMAPPING win_list",
	"stmt : ICONIFY_BY_UNMAPPING",
	"stmt : LEFT_TITLEBUTTON string EQUALS action",
	"stmt : RIGHT_TITLEBUTTON string EQUALS action",
	"stmt : button string",
	"stmt : button action",
	"stmt : string fullkey",
	"stmt : button full",
	"stmt : DONT_ICONIFY_BY_UNMAPPING",
	"stmt : DONT_ICONIFY_BY_UNMAPPING win_list",
	"stmt : ICONMGR_NOSHOW",
	"stmt : ICONMGR_NOSHOW win_list",
	"stmt : ICONMGR_NOSHOW",
	"stmt : ICONMGRS",
	"stmt : ICONMGRS iconm_list",
	"stmt : ICONMGR_SHOW",
	"stmt : ICONMGR_SHOW win_list",
	"stmt : NO_TITLE_HILITE",
	"stmt : NO_TITLE_HILITE win_list",
	"stmt : NO_TITLE_HILITE",
	"stmt : NO_HILITE",
	"stmt : NO_HILITE win_list",
	"stmt : NO_HILITE",
	"stmt : NO_STACKMODE",
	"stmt : NO_STACKMODE win_list",
	"stmt : NO_STACKMODE",
	"stmt : NO_TITLE",
	"stmt : NO_TITLE win_list",
	"stmt : NO_TITLE",
	"stmt : NO_ICON_TITLE",
	"stmt : NO_ICON_TITLE win_list",
	"stmt : NO_ICON_TITLE",
	"stmt : ICON_TITLE",
	"stmt : ICON_TITLE win_list",
	"stmt : MAKE_TITLE",
	"stmt : MAKE_TITLE win_list",
	"stmt : START_ICONIFIED",
	"stmt : START_ICONIFIED win_list",
	"stmt : AUTO_RAISE",
	"stmt : AUTO_RAISE win_list",
	"stmt : STICKY",
	"stmt : STICKY win_list",
	"stmt : MENU string LP string COLON string RP",
	"stmt : MENU string LP string COLON string RP menu",
	"stmt : MENU string",
	"stmt : MENU string menu",
	"stmt : PIEMENU string LP string COLON string RP",
	"stmt : PIEMENU string LP string COLON string RP menu",
	"stmt : PIEMENU string",
	"stmt : PIEMENU string menu",
	"stmt : PIEMENU string LP string COLON string RP AT number",
	"stmt : PIEMENU string LP string COLON string RP AT number menu",
	"stmt : PIEMENU string AT number",
	"stmt : PIEMENU string AT number menu",
	"stmt : FUNCTION string",
	"stmt : FUNCTION string function",
	"stmt : ICONS",
	"stmt : ICONS icon_list",
	"stmt : COLOR",
	"stmt : COLOR color_list",
	"stmt : MONOCHROME",
	"stmt : MONOCHROME color_list",
	"stmt : DEFAULT_FUNCTION action",
	"stmt : WINDOW_FUNCTION action",
	"stmt : WARP_CURSOR",
	"stmt : WARP_CURSOR win_list",
	"stmt : WARP_CURSOR",
	"stmt : WINDOW_RING",
	"stmt : WINDOW_RING win_list",
	"noarg : KEYWORD",
	"sarg : SKEYWORD string",
	"narg : NKEYWORD number",
	"full : EQUALS keys COLON contexts COLON action",
	"fullkey : EQUALS keys COLON contextkeys COLON action",
	"keys : /* empty */",
	"keys : keys key",
	"key : META",
	"key : SHIFT",
	"key : LOCK",
	"key : CONTROL",
	"key : META number",
	"key : OR",
	"contexts : /* empty */",
	"contexts : contexts context",
	"context : WINDOW",
	"context : TITLE",
	"context : ICON",
	"context : ROOT",
	"context : FRAME",
	"context : ICONMGR",
	"context : META",
	"context : ALL",
	"context : OR",
	"contextkeys : /* empty */",
	"contextkeys : contextkeys contextkey",
	"contextkey : WINDOW",
	"contextkey : TITLE",
	"contextkey : ICON",
	"contextkey : ROOT",
	"contextkey : FRAME",
	"contextkey : ICONMGR",
	"contextkey : META",
	"contextkey : ALL",
	"contextkey : OR",
	"contextkey : string",
	"pixmap_list : LB pixmap_entries RB",
	"pixmap_entries : /* empty */",
	"pixmap_entries : pixmap_entries pixmap_entry",
	"pixmap_entry : PKEYWORD string",
	"cursor_list : LB cursor_entries RB",
	"cursor_entries : /* empty */",
	"cursor_entries : cursor_entries cursor_entry",
	"cursor_entry : FRAME string string",
	"cursor_entry : FRAME string",
	"cursor_entry : TITLE string string",
	"cursor_entry : TITLE string",
	"cursor_entry : ICON string string",
	"cursor_entry : ICON string",
	"cursor_entry : ICONMGR string string",
	"cursor_entry : ICONMGR string",
	"cursor_entry : BUTTON string string",
	"cursor_entry : BUTTON string",
	"cursor_entry : MOVE string string",
	"cursor_entry : MOVE string",
	"cursor_entry : RESIZE string string",
	"cursor_entry : RESIZE string",
	"cursor_entry : WAIT string string",
	"cursor_entry : WAIT string",
	"cursor_entry : MENU string string",
	"cursor_entry : MENU string",
	"cursor_entry : SELECT string string",
	"cursor_entry : SELECT string",
	"cursor_entry : KILL string string",
	"cursor_entry : KILL string",
	"color_list : LB color_entries RB",
	"color_entries : /* empty */",
	"color_entries : color_entries color_entry",
	"color_entry : CLKEYWORD string",
	"color_entry : CLKEYWORD string",
	"color_entry : CLKEYWORD string win_color_list",
	"color_entry : CKEYWORD string",
	"win_color_list : LB win_color_entries RB",
	"win_color_entries : /* empty */",
	"win_color_entries : win_color_entries win_color_entry",
	"win_color_entry : string string",
	"squeeze : SQUEEZE_TITLE",
	"squeeze : SQUEEZE_TITLE",
	"squeeze : SQUEEZE_TITLE LB win_sqz_entries RB",
	"squeeze : DONT_SQUEEZE_TITLE",
	"squeeze : DONT_SQUEEZE_TITLE",
	"squeeze : DONT_SQUEEZE_TITLE win_list",
	"win_sqz_entries : /* empty */",
	"win_sqz_entries : win_sqz_entries string JKEYWORD signed_number number",
	"iconm_list : LB iconm_entries RB",
	"iconm_entries : /* empty */",
	"iconm_entries : iconm_entries iconm_entry",
	"iconm_entry : string string number",
	"iconm_entry : string string string number",
	"win_list : LB win_entries RB",
	"win_entries : /* empty */",
	"win_entries : win_entries win_entry",
	"win_entry : string",
	"icon_list : LB icon_entries RB",
	"icon_entries : /* empty */",
	"icon_entries : icon_entries icon_entry",
	"icon_entry : string string",
	"function : LB function_entries RB",
	"function_entries : /* empty */",
	"function_entries : function_entries function_entry",
	"function_entry : action",
	"menu : LB menu_entries RB",
	"menu_entries : /* empty */",
	"menu_entries : menu_entries menu_entry",
	"menu_entry : string action",
	"menu_entry : string LP string COLON string RP action",
	"action : FKEYWORD",
	"action : FSKEYWORD string",
	"signed_number : number",
	"signed_number : PLUS number",
	"signed_number : MINUS number",
	"button : BUTTON number",
	"string : STRING",
	"number : NUMBER",
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
		
case 9:
# line 117 "gram.y"
{ AddIconRegion(yypvt[-4].ptr, yypvt[-3].num, yypvt[-2].num, yypvt[-1].num, yypvt[-0].num); } break;
case 10:
# line 118 "gram.y"
{ if (Scr->FirstTime)
						  {
						    Scr->iconmgr.geometry=yypvt[-1].ptr;
						    Scr->iconmgr.columns=yypvt[-0].num;
						  }
						} break;
case 11:
# line 124 "gram.y"
{ if (Scr->FirstTime)
						    Scr->iconmgr.geometry = yypvt[-0].ptr;
						} break;
case 12:
# line 127 "gram.y"
{ if (Scr->FirstTime)
					  {
						Scr->DoZoom = TRUE;
						Scr->ZoomCount = yypvt[-0].num;
					  }
					} break;
case 13:
# line 133 "gram.y"
{ if (Scr->FirstTime) 
						Scr->DoZoom = TRUE; } break;
case 14:
# line 135 "gram.y"
{} break;
case 15:
# line 136 "gram.y"
{} break;
case 16:
# line 137 "gram.y"
{ list = &Scr->IconifyByUn; } break;
case 18:
# line 139 "gram.y"
{ if (Scr->FirstTime) 
		    Scr->IconifyByUnmapping = TRUE; } break;
case 19:
# line 141 "gram.y"
{ 
					  GotTitleButton (yypvt[-2].ptr, yypvt[-0].num, False);
					} break;
case 20:
# line 144 "gram.y"
{ 
					  GotTitleButton (yypvt[-2].ptr, yypvt[-0].num, True);
					} break;
case 21:
# line 147 "gram.y"
{ root = GetRoot(yypvt[-0].ptr, NULLSTR, NULLSTR);
					  Scr->Mouse[yypvt[-1].num][C_ROOT][0].func = F_MENU;
					  Scr->Mouse[yypvt[-1].num][C_ROOT][0].menu = root;
					} break;
case 22:
# line 151 "gram.y"
{ Scr->Mouse[yypvt[-1].num][C_ROOT][0].func = yypvt[-0].num;
					  if (yypvt[-0].num == F_MENU || yypvt[-0].num == F_PIEMENU)
					  {
					    pull->prev = NULL;
					    Scr->Mouse[yypvt[-1].num][C_ROOT][0].menu = pull;
					  }
					  else
					  {
					    root = GetRoot(TWM_ROOT,NULLSTR,NULLSTR);
					    Scr->Mouse[yypvt[-1].num][C_ROOT][0].item = 
						AddToMenu(root,"x",Action,
							  NULLSTR,yypvt[-0].num,NULLSTR,NULLSTR);
					  }
					  Action = "";
					  pull = NULL;
					} break;
case 23:
# line 167 "gram.y"
{ GotKey(yypvt[-1].ptr, yypvt[-0].num); } break;
case 24:
# line 168 "gram.y"
{ GotButton(yypvt[-1].num, yypvt[-0].num); } break;
case 25:
# line 169 "gram.y"
{ list = &Scr->DontIconify; } break;
case 27:
# line 171 "gram.y"
{ list = &Scr->IconMgrNoShow; } break;
case 29:
# line 173 "gram.y"
{ Scr->IconManagerDontShow = TRUE; } break;
case 30:
# line 174 "gram.y"
{ list = &Scr->IconMgrs; } break;
case 32:
# line 176 "gram.y"
{ list = &Scr->IconMgrShow; } break;
case 34:
# line 178 "gram.y"
{ list = &Scr->NoTitleHighlight; } break;
case 36:
# line 180 "gram.y"
{ if (Scr->FirstTime)
						Scr->TitleHighlight = FALSE; } break;
case 37:
# line 182 "gram.y"
{ list = &Scr->NoHighlight; } break;
case 39:
# line 184 "gram.y"
{ if (Scr->FirstTime)
						Scr->Highlight = FALSE; } break;
case 40:
# line 186 "gram.y"
{ list = &Scr->NoStackModeL; } break;
case 42:
# line 188 "gram.y"
{ if (Scr->FirstTime)
						Scr->StackMode = FALSE; } break;
case 43:
# line 190 "gram.y"
{ list = &Scr->NoTitle; } break;
case 45:
# line 192 "gram.y"
{ if (Scr->FirstTime)
						Scr->NoTitlebar = TRUE; } break;
case 46:
# line 194 "gram.y"
{ list = &Scr->NoIconTitleL; } break;
case 48:
# line 196 "gram.y"
{ if (Scr->FirstTime)
						Scr->NoIconTitle = TRUE; } break;
case 49:
# line 198 "gram.y"
{ list = &Scr->IconTitleL; } break;
case 51:
# line 200 "gram.y"
{ list = &Scr->MakeTitle; } break;
case 53:
# line 202 "gram.y"
{ list = &Scr->StartIconified; } break;
case 55:
# line 204 "gram.y"
{ list = &Scr->AutoRaise; } break;
case 57:
# line 206 "gram.y"
{ list = &Scr->StickyL; } break;
case 59:
# line 209 "gram.y"
{ root = GetRoot(yypvt[-5].ptr, yypvt[-3].ptr, yypvt[-1].ptr); } break;
case 60:
# line 210 "gram.y"
{ root->real_menu = TRUE;} break;
case 61:
# line 211 "gram.y"
{ root = GetRoot(yypvt[-0].ptr, NULLSTR, NULLSTR); } break;
case 62:
# line 212 "gram.y"
{ root->real_menu = TRUE; } break;
case 63:
# line 214 "gram.y"
{ root = GetRoot(yypvt[-5].ptr, yypvt[-3].ptr, yypvt[-1].ptr);
					  root->pie_menu = 1; } break;
case 64:
# line 216 "gram.y"
{ root->real_menu = TRUE;} break;
case 65:
# line 217 "gram.y"
{ root = GetRoot(yypvt[-0].ptr, NULLSTR, NULLSTR);
					  root->pie_menu = 1; } break;
case 66:
# line 219 "gram.y"
{ root->real_menu = TRUE; } break;
case 67:
# line 221 "gram.y"
{ root = GetRoot(yypvt[-7].ptr, yypvt[-5].ptr, yypvt[-3].ptr);
					  root->pie_menu = 1;
					  root->initial_angle = DEG_TO_RAD(yypvt[-0].num); } break;
case 68:
# line 224 "gram.y"
{ root->real_menu = TRUE;} break;
case 69:
# line 226 "gram.y"
{ root = GetRoot(yypvt[-2].ptr, NULLSTR, NULLSTR);
					  root->pie_menu = 1;
					  root->initial_angle = DEG_TO_RAD(yypvt[-0].num); } break;
case 70:
# line 229 "gram.y"
{ root->real_menu = TRUE; } break;
case 71:
# line 230 "gram.y"
{ root = GetRoot(yypvt[-0].ptr, NULLSTR, NULLSTR); } break;
case 73:
# line 232 "gram.y"
{ list = &Scr->IconNames; } break;
case 75:
# line 234 "gram.y"
{ color = COLOR; } break;
case 77:
# line 236 "gram.y"
{ color = MONOCHROME; } break;
case 79:
# line 238 "gram.y"
{ Scr->DefaultFunction.func = yypvt[-0].num;
					  if (yypvt[-0].num == F_MENU || yypvt[-0].num == F_PIEMENU)
					  {
					    pull->prev = NULL;
					    Scr->DefaultFunction.menu = pull;
					  }
					  else
					  {
					    root = GetRoot(TWM_ROOT,NULLSTR,NULLSTR);
					    Scr->DefaultFunction.item = 
						AddToMenu(root,"x",Action,
							  NULLSTR,yypvt[-0].num, NULLSTR, NULLSTR);
					  }
					  Action = "";
					  pull = NULL;
					} break;
case 80:
# line 254 "gram.y"
{ Scr->WindowFunction.func = yypvt[-0].num;
					   root = GetRoot(TWM_ROOT,NULLSTR,NULLSTR);
					   Scr->WindowFunction.item = 
						AddToMenu(root,"x",Action,
							  NULLSTR,yypvt[-0].num, NULLSTR, NULLSTR);
					   Action = "";
					   pull = NULL;
					} break;
case 81:
# line 262 "gram.y"
{ list = &Scr->WarpCursorL; } break;
case 83:
# line 264 "gram.y"
{ if (Scr->FirstTime) 
					    Scr->WarpCursor = TRUE; } break;
case 84:
# line 266 "gram.y"
{ list = &Scr->WindowRingL; } break;
case 86:
# line 271 "gram.y"
{ if (!do_single_keyword (yypvt[-0].num)) {
					    twmrc_error_prefix();
					    fprintf (stderr,
					"unknown singleton keyword %d\n",
						     yypvt[-0].num);
					    ParseError = 1;
					  }
					} break;
case 87:
# line 281 "gram.y"
{ if (!do_string_keyword (yypvt[-1].num, yypvt[-0].ptr)) {
					    twmrc_error_prefix();
					    fprintf (stderr,
				"unknown string keyword %d (value \"%s\")\n",
						     yypvt[-1].num, yypvt[-0].ptr);
					    ParseError = 1;
					  }
					} break;
case 88:
# line 291 "gram.y"
{ if (!do_number_keyword (yypvt[-1].num, yypvt[-0].num)) {
					    twmrc_error_prefix();
					    fprintf (stderr,
				"unknown numeric keyword %d (value %d)\n",
						     yypvt[-1].num, yypvt[-0].num);
					    ParseError = 1;
					  }
					} break;
case 89:
# line 303 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
case 90:
# line 306 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
case 93:
# line 313 "gram.y"
{ mods |= Mod1Mask; } break;
case 94:
# line 314 "gram.y"
{ mods |= ShiftMask; } break;
case 95:
# line 315 "gram.y"
{ mods |= LockMask; } break;
case 96:
# line 316 "gram.y"
{ mods |= ControlMask; } break;
case 97:
# line 317 "gram.y"
{ if (yypvt[-0].num < 1 || yypvt[-0].num > 5) {
					     twmrc_error_prefix();
					     fprintf (stderr, 
				"bad modifier number (%d), must be 1-5\n",
						      yypvt[-0].num);
					     ParseError = 1;
					  } else {
					     mods |= (Mod1Mask << (yypvt[-0].num - 1));
					  }
					} break;
case 98:
# line 327 "gram.y"
{ } break;
case 101:
# line 334 "gram.y"
{ cont |= C_WINDOW_BIT; } break;
case 102:
# line 335 "gram.y"
{ cont |= C_TITLE_BIT; } break;
case 103:
# line 336 "gram.y"
{ cont |= C_ICON_BIT; } break;
case 104:
# line 337 "gram.y"
{ cont |= C_ROOT_BIT; } break;
case 105:
# line 338 "gram.y"
{ cont |= C_FRAME_BIT; } break;
case 106:
# line 339 "gram.y"
{ cont |= C_ICONMGR_BIT; } break;
case 107:
# line 340 "gram.y"
{ cont |= C_ICONMGR_BIT; } break;
case 108:
# line 341 "gram.y"
{ cont |= C_ALL_BITS; } break;
case 109:
# line 342 "gram.y"
{  } break;
case 112:
# line 349 "gram.y"
{ cont |= C_WINDOW_BIT; } break;
case 113:
# line 350 "gram.y"
{ cont |= C_TITLE_BIT; } break;
case 114:
# line 351 "gram.y"
{ cont |= C_ICON_BIT; } break;
case 115:
# line 352 "gram.y"
{ cont |= C_ROOT_BIT; } break;
case 116:
# line 353 "gram.y"
{ cont |= C_FRAME_BIT; } break;
case 117:
# line 354 "gram.y"
{ cont |= C_ICONMGR_BIT; } break;
case 118:
# line 355 "gram.y"
{ cont |= C_ICONMGR_BIT; } break;
case 119:
# line 356 "gram.y"
{ cont |= C_ALL_BITS; } break;
case 120:
# line 357 "gram.y"
{ } break;
case 121:
# line 358 "gram.y"
{ Name = yypvt[-0].ptr; cont |= C_NAME_BIT; } break;
case 125:
# line 369 "gram.y"
{ do_pixmap_keyword(yypvt[-1].num,yypvt[-0].ptr); } break;
case 129:
# line 380 "gram.y"
{
			NewBitmapCursor(&Scr->FrameCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 130:
# line 382 "gram.y"
{
			NewFontCursor(&Scr->FrameCursor, yypvt[-0].ptr); } break;
case 131:
# line 384 "gram.y"
{
			NewBitmapCursor(&Scr->TitleCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 132:
# line 386 "gram.y"
{
			NewFontCursor(&Scr->TitleCursor, yypvt[-0].ptr); } break;
case 133:
# line 388 "gram.y"
{
			NewBitmapCursor(&Scr->IconCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 134:
# line 390 "gram.y"
{
			NewFontCursor(&Scr->IconCursor, yypvt[-0].ptr); } break;
case 135:
# line 392 "gram.y"
{
			NewBitmapCursor(&Scr->IconMgrCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 136:
# line 394 "gram.y"
{
			NewFontCursor(&Scr->IconMgrCursor, yypvt[-0].ptr); } break;
case 137:
# line 396 "gram.y"
{
			NewBitmapCursor(&Scr->ButtonCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 138:
# line 398 "gram.y"
{
			NewFontCursor(&Scr->ButtonCursor, yypvt[-0].ptr); } break;
case 139:
# line 400 "gram.y"
{
			NewBitmapCursor(&Scr->MoveCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 140:
# line 402 "gram.y"
{
			NewFontCursor(&Scr->MoveCursor, yypvt[-0].ptr); } break;
case 141:
# line 404 "gram.y"
{
			NewBitmapCursor(&Scr->ResizeCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 142:
# line 406 "gram.y"
{
			NewFontCursor(&Scr->ResizeCursor, yypvt[-0].ptr); } break;
case 143:
# line 408 "gram.y"
{
			NewBitmapCursor(&Scr->WaitCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 144:
# line 410 "gram.y"
{
			NewFontCursor(&Scr->WaitCursor, yypvt[-0].ptr); } break;
case 145:
# line 412 "gram.y"
{
			NewBitmapCursor(&Scr->MenuCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 146:
# line 414 "gram.y"
{
			NewFontCursor(&Scr->MenuCursor, yypvt[-0].ptr); } break;
case 147:
# line 416 "gram.y"
{
			NewBitmapCursor(&Scr->SelectCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 148:
# line 418 "gram.y"
{
			NewFontCursor(&Scr->SelectCursor, yypvt[-0].ptr); } break;
case 149:
# line 420 "gram.y"
{
			NewBitmapCursor(&Scr->DestroyCursor, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 150:
# line 422 "gram.y"
{
			NewFontCursor(&Scr->DestroyCursor, yypvt[-0].ptr); } break;
case 154:
# line 433 "gram.y"
{ if (!do_colorlist_keyword (yypvt[-1].num, color,
								     yypvt[-0].ptr)) {
					    twmrc_error_prefix();
					    fprintf (stderr,
			"unhandled list color keyword %d (string \"%s\")\n",
						     yypvt[-1].num, yypvt[-0].ptr);
					    ParseError = 1;
					  }
					} break;
case 155:
# line 442 "gram.y"
{ list = do_colorlist_keyword(yypvt[-1].num,color,
								      yypvt[-0].ptr);
					  if (!list) {
					    twmrc_error_prefix();
					    fprintf (stderr,
			"unhandled color list keyword %d (string \"%s\")\n",
						     yypvt[-1].num, yypvt[-0].ptr);
					    ParseError = 1;
					  }
					} break;
case 157:
# line 453 "gram.y"
{ if (!do_color_keyword (yypvt[-1].num, color,
								 yypvt[-0].ptr)) {
					    twmrc_error_prefix();
					    fprintf (stderr,
			"unhandled color keyword %d (string \"%s\")\n",
						     yypvt[-1].num, yypvt[-0].ptr);
					    ParseError = 1;
					  }
					} break;
case 161:
# line 472 "gram.y"
{ if (Scr->FirstTime &&
					      color == Scr->Monochrome)
					    AddToList(list, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 162:
# line 477 "gram.y"
{ 
#ifdef SHAPE
				    if (HasShape) Scr->SqueezeTitle = TRUE;
#endif
				} break;
case 163:
# line 482 "gram.y"
{ list = &Scr->SqueezeTitleL; 
#ifdef SHAPE
				  if (HasShape && Scr->SqueezeTitle == -1)
				    Scr->SqueezeTitle = TRUE;
#endif
				} break;
case 165:
# line 489 "gram.y"
{ Scr->SqueezeTitle = FALSE; } break;
case 166:
# line 490 "gram.y"
{ list = &Scr->DontSqueezeTitleL; } break;
case 169:
# line 495 "gram.y"
{
				if (Scr->FirstTime) {
				   do_squeeze_entry (list, yypvt[-3].ptr, yypvt[-2].num, yypvt[-1].num, yypvt[-0].num);
				}
			} break;
case 173:
# line 510 "gram.y"
{ if (Scr->FirstTime)
					    AddToList(list, yypvt[-2].ptr, (char *)
						AllocateIconManager(yypvt[-2].ptr, NULLSTR,
							yypvt[-1].ptr,yypvt[-0].num));
					} break;
case 174:
# line 516 "gram.y"
{ if (Scr->FirstTime)
					    AddToList(list, yypvt[-3].ptr, (char *)
						AllocateIconManager(yypvt[-3].ptr,yypvt[-2].ptr,
						yypvt[-1].ptr, yypvt[-0].num));
					} break;
case 178:
# line 530 "gram.y"
{ if (Scr->FirstTime)
					    AddToList(list, yypvt[-0].ptr, 0);
					} break;
case 182:
# line 542 "gram.y"
{ if (Scr->FirstTime) AddToList(list, yypvt[-1].ptr, yypvt[-0].ptr); } break;
case 186:
# line 552 "gram.y"
{ AddToMenu(root, "", Action, NULLSTR, yypvt[-0].num,
						NULLSTR, NULLSTR);
					  Action = "";
					} break;
case 190:
# line 565 "gram.y"
{ AddToMenu(root, yypvt[-1].ptr, Action, pull, yypvt[-0].num,
						NULLSTR, NULLSTR);
					  Action = "";
					  pull = NULL;
					} break;
case 191:
# line 570 "gram.y"
{
					  AddToMenu(root, yypvt[-6].ptr, Action, pull, yypvt[-0].num,
						yypvt[-4].ptr, yypvt[-2].ptr);
					  Action = "";
					  pull = NULL;
					} break;
case 192:
# line 578 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
case 193:
# line 579 "gram.y"
{
				yyval.num = yypvt[-1].num;
				Action = yypvt[-0].ptr;
				switch (yypvt[-1].num) {
				  case F_MENU:
				  case F_PIEMENU:
				    pull = GetRoot (yypvt[-0].ptr, NULLSTR,NULLSTR);
				    pull->prev = root;
				    break;
				  case F_WARPRING:
				    if (!CheckWarpRingArg (Action)) {
					twmrc_error_prefix();
					fprintf (stderr,
			"ignoring invalid f.warptoring argument \"%s\"\n",
						 Action);
					yyval.num = F_NOP;
				    }
				  case F_WARPTOSCREEN:
				    if (!CheckWarpScreenArg (Action)) {
					twmrc_error_prefix();
					fprintf (stderr, 
			"ignoring invalid f.warptoscreen argument \"%s\"\n", 
					         Action);
					yyval.num = F_NOP;
				    }
				    break;
				  case F_COLORMAP:
				    if (CheckColormapArg (Action)) {
					yyval.num = F_COLORMAP;
				    } else {
					twmrc_error_prefix();
					fprintf (stderr,
			"ignoring invalid f.colormap argument \"%s\"\n", 
						 Action);
					yyval.num = F_NOP;
				    }
				    break;
				} /* end switch */
				   } break;
case 194:
# line 621 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
case 195:
# line 622 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
case 196:
# line 623 "gram.y"
{ yyval.num = -(yypvt[-0].num); } break;
case 197:
# line 626 "gram.y"
{ yyval.num = yypvt[-0].num;
					  if (yypvt[-0].num == 0)
						yyerror("bad button 0");

					  if (yypvt[-0].num > MAX_BUTTONS)
					  {
						yyval.num = 0;
						yyerror("button number too large");
					  }
					} break;
case 198:
# line 638 "gram.y"
{ ptr = (char *)malloc(strlen(yypvt[-0].ptr)+1);
					  strcpy(ptr, yypvt[-0].ptr);
					  RemoveDQuote(ptr);
					  yyval.ptr = ptr;
					} break;
case 199:
# line 643 "gram.y"
{ yyval.num = yypvt[-0].num; } break;
	}
	goto yystack;		/* reset registers in driver code */
}

/* -*-C-*- dvibit.c */
/*-->dvibit*/
/**********************************************************************/
/******************************* dvibit *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef BBNBITGRAPH
#define  BBNBITGRAPH       1		/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Version 3.10 BBN BitGraph terminal"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"bit"

#undef ALLOW_INTERRUPT
#define ALLOW_INTERRUPT	1		/* to allow ^C for debugging */
					/* undefine for normal catch-^C mode */

#define  BELLS		"\007\007\007\007\007"	/* bells at end-of-page */


/* Number of 16-bit words permitted in a big character  before we switch
to sending raster bitmaps.  The BitGraph does not  impose any real limit
(since 16-bit font metrics are  provided for), but  we could conceivably
exhaust font storage if we allow  too  many  big characters.  The screen
has a resolution of about 72 dpi, so  we allow roughly characters  up to
72pt in  size to  be downloaded as fonts.  The   old value here  was 32,
which made display rather slow for large characters; this new version is
MUCH peppier. */

#define  BIGBGCHAR	 ((72 * 72)/16)

#define  BYTE_SIZE        8		/* output file byte size */

#define  FIRSTBGCHAR	 32
#define  LASTBGCHAR	127
#define  NBGFONTS	  3	/* number of PXL fonts that can be */
				/* downloaded into the BitGraph */

#undef STDRES
#define STDRES  0		/* 0 for low-resolution devices */

#define  XDIFFMAX	20
#define  XDIFFORG	(FIRSTBGCHAR+XDIFFMAX)


/* NB: XSIZE is really 768, but BitGraph Version 3.10 seems to have a bug.
Anything past 724 is overwritten on previous character.  We reduce it to
720 in order to preserve a multiple of 16 (needed for correct positioning
in eopact() */
#define  XPSIZE		8		/* horizontal paper size in inches */
#define  XSIZE		720		/* number of horizontal dots */
#define  XDPI		(XSIZE/XPSIZE)	/* horizontal dots/inch */
#define  YDIFFMAX	20
#define  YDIFFORG	(FIRSTBGCHAR+XDIFFMAX+1+XDIFFMAX+YDIFFMAX)

#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(1024-5*16)	/* number of vertical dots, less */
					/* menu bars */
#define  YDPI		(YSIZE/YPSIZE)	/* vertical dots/inch */

#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"

/*-->bopact*/
/**********************************************************************/
/******************************* bopact *******************************/
/**********************************************************************/

void
bopact()			/* beginning-of-page action */
{
    int y;
/* Define a shorthand for inverse-video strings */
#define OUTIV(str) OUTF("\033[7m%s\033[m",str)

    pbghpos = pbgvpos = -99;
    cpagep = (long)FTELL(dvifp) - 1L;
    ppagep = (long)nosignex(dvifp,(BYTE)4);
    if (!g_interactive)
	return;			/* no action if not a terminal */
    OUTS("\033:[");		/* BBNPSH push context */
    OUTS("\033:2e");		/* BBNSEM Select Emulation Mode VT100 */
    OUTS("\033(B");		/* revert to ASCII character set */

    OUTS("\033[H\033[K");	/* cursor to line 1 and clear */
    OUTIV("SPACE");		/* Output help message in top line */
    OUTS(":next-page ");
    OUTIV("P");
    OUTS(":previous-page ");
    OUTIV("^L");
    OUTS(":current-page ");
    OUTIV("L");
    OUTS(":Left ");
    OUTIV("R");
    OUTS(":right ");
    OUTIV("D");
    OUTS(":Down ");
    OUTIV("U");
    OUTS(":up");

    OUTS("\033[2H\033[K");	/* cursor to line 2 and clear */
    OUTIV("@");
    OUTS(":initial position ");
    OUTIV("EMACS keys, arrow keys, l, r, d, u");
    OUTS(":move in small steps ");

    OUTS("\033[3H\033[K");	/* cursor to line 3 and clear */
    OUTIV("Z");
    OUTS(": zoom up ");
    OUTIV("z");
    OUTS(": zoom down ");
    OUTIV("###<RET>");
    OUTS(":goto page ### ");

    OUTS("\033[4H\033[K");	/* cursor to line 4 and clear */
    OUTF3("Document page: %d/%d  TeX pages: %s",
	(int)(cur_index+1),(int)page_count,tctos());
    OUTF3(" Xscreen = %d, Yscreen = %d, Mag = %ld",
	(int)xscreen, (int)yscreen, (long)runmag);

    OUTS("\033[5H\033[J");	/* clear from line 5 to end of screen */

    OUTS("\033[H");		/* home cursor */
    OUTS("\033:]");		/* BBNPOP pop context */

    /* Draw menu bars across screen */
    OUTS("\033:15v");	/* BBNSDO - set drawing operation to foreground color */
    for (y = YSIZE; y < YSIZE + 8; y += 2)
	OUTF3("\033:0;%dm\033:%d;%dd",y,XSIZE-1,y);
}

#include "chargf.h"
#include "charpk.h"
#include "charpxl.h"
#include "clrrow.h"
#include "dbgopen.h"

/*-->devinit*/
/**********************************************************************/
/****************************** devinit *******************************/
/**********************************************************************/

void
devinit(argc,argv)		/* initialize device */
int argc;
char *argv[];
{
    register UNSIGN16 the_font;	/* loop index */
    register UNSIGN16 the_char;	/* loop index */

    char *tcp;

    g_interactive = (kbopen(KB_CBREAK) == 0);

    if (g_interactive)
    {
	if ((tcp=GETENV("TERM")) == (char*)NULL)
	{
	    (void)sprintf(message,
		"devinit():  [%s] cannot find environment variable [TERM]",
		g_progname);
	    (void)fatal(message);
	}
	else
	{

#if    OS_VAXVMS
	/* VMS C overrides user's value of TERM, so skip check */
#else
#if    OS_TOPS20
	    if (strcmp(tcp,"BITGRAPH") && strncmp(tcp,"BG",2))
#else
	    if (strcmp(tcp,"bitgraph") && strncmp(tcp,"bg",2))
#endif

	    {
		(void)sprintf(message,"devinit():  [%s] cannot run \
interactively on non-BitGraph terminal",
		    g_progname);
		(void)fatal(message);
	    }
#endif
	}
    }

#if    ALLOW_INTERRUPT
#else /* NOT ALLOW_INTERRUPT */
    signal(SIGINT, SIG_IGN);  /* ignore interrupts */
#endif /* ALLOW_INTERRUPT */

    initterm();

#if    ALLOW_INTERRUPT
#else /* NOT ALLOW_INTERRUPT */
    signal(SIGINT, gotint);		/* catch interrupts */
#endif /* ALLOW_INTERRUPT */

    /* Clear all BitGraph font char_entry pointers */
    for (the_font = 0; the_font < (NBGFONTS+(NBGFONTS+2)/3); ++the_font)
	for (the_char = FIRSTPXLCHAR; the_char <= LASTPXLCHAR; ++the_char)
	    bgcp[the_font][the_char] = (struct char_entry *)NULL;
}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
    rsetterm();
}

#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"

/* -*-C-*- eopact.h */
/*-->eopact*/
/**********************************************************************/
/******************************* eopact *******************************/
/**********************************************************************/

#define MAXFRAC	 8		/* maximum fractional step divisor */
#define MINFRAC 64		/* minimum fractional step divisor */
#define DIGVAL(c) (INT16)(c - '0')

void
eopact()			/* end-of-page action */
{
    int loop_count;		/* loop count */
    INT16 np;			/* input page number */
    BOOLEAN show_page;		/* page redisplay flag */
    char tc;			/* temporary character */

    /*
    Screen positioning control is done by obvious mnemonics given in the
    menu bar displayed	by   bopact(), but cursor keys	 and  EMACS-like
    control keys are also recognized.  Keyboard input causes  an instant
    end-of-page action, to  facilitate	 rapid page  repositioning,  and
    skimming through a document.  All  available commands are acted upon
    before   screen display   is   reattempted.	  This	 makes	repeated
    positioning commands cumulative.
    */

    if (!quiet)
	OUTS(BELLS);		/* wake up the user */
    (void)fflush(plotfp);	/* make sure display is up-to-date */

    loop_count = 1;
    show_page = FALSE;
    while ((loop_count-- > 0) || kbinput())
    {
	tc = (char)kbget();
	if (tc == '\033')	/* ESC sequence--function key */
	{
	    /* Expect ESC x, ESC O x or ESC [ x (x = A,B,C,D) */
	    tc = (char)kbget();
	    if ((tc == '[') || (tc == 'O'))
		tc = (char)kbget();
	    switch(tc)
	    {
	    case 'A':		/* cursor up */
		tc = 'u';
		break;
	    case 'B':		/* cursor down */
		tc = 'd';
		break;
	    case 'C':		/* cursor right */
		tc = 'r';
		break;
	    case 'D':		/* cursor left */
		tc = 'l';
		break;
	    case 'V':		/* META-V -- previous page */
	    case 'v':
		tc = 'P';
		break;
	    default:		/* none of these--accept it anyway */
		break;
	    }
	}
	switch (tc)
	{
	case '0':		/* goto n-th page (recursively) */
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    np = DIGVAL(tc);
	    for (tc = (char)kbget(); ;tc = (char)kbget())
	    {
	       if (isdigit(tc))
		   np = 10*np + DIGVAL(tc);
	       else if ((tc == '\177') || (tc == '\b'))
		   np /= 10;	/* rubout last digit */
	       else
		   break;
	    }
	    np = MIN(np,page_count); /* big numbers go to last page */
	    if (np > 0)		/* got a page number */
	    {
		cur_index = np - 1;
		show_page = TRUE; /* out-of-sequence page */
	    }
	    else
		show_page = FALSE;
	    continue;

	case ' ':		/* normal case - start next page */
	case '\026':		/* CTL-V */
	case '\r':
	case '\n':
	case '\0':
	    show_page = FALSE;
	    continue;

	case '\f':		/* redisplay current page  */
	case '.':
	    show_page = TRUE;
	    continue;

	case '@':		/* reset to initial state */
	    xscreen = 0;	/* 'i' is mnemonic, but too close to 'u' */
	    yscreen = 0;	/* on keyboard */
	    show_page = TRUE;
	    continue;

	case 'd':		/* move down and redisplay  */
	case '\016':	/* CTL-N */
	    yscreen -= (YSIZE/MINFRAC);
	    show_page = TRUE;
	    continue;

	case 'D':
	    yscreen -= (YSIZE/MAXFRAC);
	    show_page = TRUE;
	    continue;

	case 'l':		/* move left and redisplay */
	case '\002':		/* CTL-B */
	    xscreen -= (XSIZE/MINFRAC);
	    show_page = TRUE;
	    continue;

	case 'L':
	    xscreen -= (XSIZE/MAXFRAC);
	    show_page = TRUE;
	    continue;

	case 'p':		/* redisplay from previous page */
	case 'P':
	case '\b':
	case '^':
	    if (cur_index > 0)
	    {
		cur_index--;
		show_page = TRUE;
	    }
	    else		/* no previous page--ignore */
	    {
		OUTS(BELLS);
		loop_count = 1;
		show_page = FALSE;
	    }
	    continue;

	case 'q':		/* quit */
	case 'Q':
	case 'x':		/* exit */
	case 'X':
	case '\003':		/* CTL-C */
	case '\031':		/* CTL-Y */
	    (void)devterm();	/* terminate device output */
	    (void)dviterm();	/* terminate DVI file processing */
	    (void)alldone();	/* this does not return */
	    continue;

	case 'r':		/* move right and redisplay */
	case '\006':		/* CTL-F */
	    xscreen += (XSIZE/MINFRAC);
	    show_page = TRUE;
	    continue;

	case 'R':
	    xscreen += (XSIZE/MAXFRAC);
	    show_page = TRUE;
	    continue;

	case 'u':		/* move up and redisplay */
	case '\020':		/* CTL-P */
	    yscreen += (YSIZE/MINFRAC);
	    show_page = TRUE;
	    continue;

	case 'U':
	    yscreen += (YSIZE/MAXFRAC);
	    show_page = TRUE;
	    continue;

	case 'z':		/* zoom down one magstep (1.2) */
	    runmag = MAGSIZE(actfact((UNSIGN32)((5*runmag)/6)));
	    conv = ((float)num/(float)den) *
		((float)runmag/(float)STDMAG) *

#if    USEGLOBALMAG
		actfact(mag) *
#endif

		((float)RESOLUTION/254000.0);
	    (void)unloadfonts();
	    show_page = TRUE;
	    continue;

	case 'Z':		/* zoom up one magstep (1.2) */
	    runmag = MAGSIZE(actfact((UNSIGN32)((runmag*6)/5)));
	    conv = ((float)num/(float)den) *
		((float)runmag/(float)STDMAG) *

#if    USEGLOBALMAG
		actfact(mag) *
#endif

		((float)RESOLUTION/254000.0);
	    (void)unloadfonts();
	    show_page = TRUE;
	    continue;

	default:		/* unimplemented command */
	    OUTS(BELLS);	/* beep to flag ignored input */
	    (void)kbflush();	/* clear any typeahead */
	    show_page = FALSE;
	    loop_count = 1;
	    continue;		/* stay in read-loop */}
    }
    if (show_page)
	(void)prtpage(page_ptr[cur_index]);
}

#include "f20open.h"
#include "fatal.h"

/*-->fillrect*/
/**********************************************************************/
/****************************** fillrect ******************************/
/**********************************************************************/

void
fillrect(x,y,width,height)
COORDINATE x,y,width,height;		/* lower left corner, size */

/***********************************************************************
With the page origin (0,0) at the lower-left corner of the bitmap,  draw
a filled rectangle at (x,y).
***********************************************************************/

{
    if ( ((x+width) <= 0) || (XSIZE <= x) || ((y+height) <= 0) ||
	(YSIZE <= y) )
	/*NO-OP*/;	/* Trivial reject -- rectangle completely offpage */
    else
    {
	OUTS("\033:15;");		/* BBNRAST  Rastop */
	OUTF2("%d;%d;",x,y);
	OUTF2("%d;%d;",x,y);
	OUTF2("%d;%dr",width,height);
    }
}


#include "findpost.h"
#include "fixpos.h"
#include "fontfile.h"
#include "fontsub.h"
#include "getbytes.h"
#include "getfntdf.h"
#include "getpgtab.h"
#include "inch.h"
#include "initglob.h"

/*-->initterm*/
/**********************************************************************/
/****************************** initterm ******************************/
/**********************************************************************/

void
initterm()	/* initialize terminal */

{
    register INT16 i;  /* loop index */

#if    OS_TOPS20

    /* TOPS-20 regrettably does not have a "raw" terminal mode (except for */
    /* binary I/O (bytesize other than 7)), so several JSYS's are needed to */
    /* suppress the fiddling it does with the output. */

    /* First, save the current terminal state */

    ac1 = jfn_plotfp;
    if (jsys(JSrfmod,acs) != JSok)	/* get jfn mode word */
	;				/* ignore errors */
    bg_modeword = ac2;

    ac1 = jfn_plotfp;
    if (jsys(JSrfcoc,acs) != JSok)	/* get terminal control codes */
	;				/* ignore errors */
    bg_1ccoc = ac2;
    bg_2ccoc = ac3;

    ac1 = jfn_plotfp;
    ac2 = MOrlw;
    if (jsys(JSmtopr,acs) != JSok)	/* get terminal width */
	;				/* ignore errors */
    bg_width = ac3;

    ac1 = jfn_plotfp;
    ac2 = MOrll;
    if (jsys(JSmtopr,acs) != JSok)	/* get terminal page length */
	;				/* ignore errors */
    bg_length = ac3;

    ac1 = jfn_plotfp;
    ac2 = MOrnt;
    if (jsys(JSmtopr,acs) != JSok)	/* get terminal refuse/accept */
					/* system message status */
	;				/* ignore errors */
    bg_sysmsg = ac3;

    /* Now reset the terminal state so no one else can bother us and */
    /* the terminal can operate in "raw" mode */

    ac1 = jfn_plotfp;
    ac2 = MOsnt;
    ac3 = MOsmn;
    if (jsys(JSmtopr,acs) != JSok)	/* suppress system messages */
	;				/* ignore errors */

    ac1 = Value(TL_cro) | Value(TL_cor) | Value(TL_sab) | Value(TL_sta) |
	Value(TL_obj);
    ac2 = -1;
    if (jsys(JStlink,acs) != JSok)	/* refuse links and advice */
	;				/* ignore errors */


    ac1 = jfn_plotfp;
    ac2 = Value(TT_mff) | Value(TT_tab) | Value(TT_lca) | Value(TT_pgm);
    if (jsys(JSstpar,acs) != JSok)	/* formfeed, tab, lowercase */
					/* capability, plus X-on/X-off */
					/* control */
	;				/* ignore errors */

    ac1 = jfn_plotfp;
    ac2 = 0525252525252;
    ac3 = 0525252525252;
    if (jsys(JSsfcoc,acs) != JSok)	/* untranslated output of all 32 */
	;				/* control characters */

    ac1 = jfn_plotfp;
    ac2 = MOslw;
    if (jsys(JSmtopr,acs) != JSok)	/* line width to 0 to suppress */
	;				/* CR LF insertion at EOL */

    ac1 = jfn_plotfp;
    ac2 = MOsll;
    if (jsys(JSmtopr,acs) != JSok)	/* page length to 0 to suppress */
	;				/* pause at end-of-page */
#endif /* OS_TOPS20 */

    OUTS("\033:2e");			/* BBNSEM Select Emulation Mode VT100 */
    OUTS("\033(B");			/* revert to ASCII character set */
    OUTS("\033[H\033[J");		/* home cursor and clear screen */
    OUTS("\033:[");			/* BBNPSH  Push Context */
    OUTS("\033:0e");			/* BBNSEM  Select Emulation Mode */
    OUTS("\033[?7l");			/* RM DECAWM  no right margin */
					/* wrap wanted. */
    OUTS("\033:2L");			/* BBNSFP  Set Font Parameters */
					/* to reset all fonts */
    OUTS("\033:7o");			/* BBNSTO  Set Text Operation */

    for (i=(-XDIFFMAX); i<=XDIFFMAX; i++)  /* load horizontal skips */
    {
	OUTS("\033P:0;");		/* BBNLFC  Load Font Character */
	OUTF("%d;",XDIFFORG+i);
	OUTC('0'+NBGFONTS+(NBGFONTS+2)/3);
	OUTS(";0;0;0;0;");
	OUTF("%dL",i);
    }

    for (i=(-YDIFFMAX); i<=YDIFFMAX; i++)/* load vertical skips */
    {
	OUTS("\033P:0;");		/* BBNLFC  Load Font Character */
	OUTF("%d;",YDIFFORG+i);
	OUTC('0'+NBGFONTS+(NBGFONTS+2)/3);
	OUTS(";0;0;0;0;");
	OUTF("%dL",i);
    }
    OUTF("\033)%c",(char)('0'+NBGFONTS+(NBGFONTS+2)/3));
					/* SCS  Select G1 to be a */
					/* loadable font */
}


#include "movedown.h"
#include "moveover.h"
#include "moveto.h"

/*-->newfont*/
/**********************************************************************/
/****************************** newfont *******************************/
/**********************************************************************/

void
newfont()
{
    register UNSIGN16 the_char;	/* loop index */
    register struct char_entry *tcharptr;

    fullfont = (fullfont+1) % NBGFONTS;
    if (fullfont == 0)
    {
	partfont = NBGFONTS;
	partchar = FIRSTBGCHAR;
    }
    else
    {
	partchar += FIRSTBGCHAR;
	if (partchar > LASTBGCHAR)
	{
	    partfont++;
	    partchar = FIRSTBGCHAR;
	}
    }
    for (the_char = FIRSTPXLCHAR; the_char <= LASTPXLCHAR; the_char++)
    {
	tcharptr = &(fontptr->ch[the_char]);
	tcharptr->isloaded = FALSE;
	tcharptr->istoobig = ((int)(tcharptr->hp*((tcharptr->wp+15) >> 4))
	    >= BIGBGCHAR);
	if (the_char < FIRSTBGCHAR)
	{
	    tcharptr->bgfont = partfont;
	    tcharptr->bgchar = partchar+the_char;
	}
	else
	{
	    tcharptr->bgfont = fullfont;
	    tcharptr->bgchar  = the_char;
	}
    }
}

#include "nosignex.h"
#include "openfont.h"
#include "option.h"

/*-->outrow*/
/**********************************************************************/
/******************************* outrow *******************************/
/**********************************************************************/

void
outrow(c,yoff)	/* send img_row[] to output device */
BYTE c;		/* current character value */
UNSIGN16 yoff;	/* offset from top row (0,1,...,hp-1) (UNUSED here) */
{
    register UNSIGN16 k;	/* loop index */
    register UNSIGN32 *p;	/* pointer into img_row[] */
    struct char_entry *tcharptr;/* temporary char_entry pointer */
    UNSIGN16 half_words_per_row;/* number of raster half-words to copy */

    tcharptr = &(fontptr->ch[c]);
    half_words_per_row = (UNSIGN16)((tcharptr->wp + 15) >> 4);
    p = img_row;		/* we step along img_row[] */

    for (k = half_words_per_row; k > 0; (++p,--k))
    {
	putout((INT16)((*p) >> 16));	/* first half-word */
	if (--k == 0)
	    break;
	putout((INT16)((*p) & 0xffff));	/* second half-word */
    }
}

#include "prtpage.h"

/*-->putout*/
/**********************************************************************/
/******************************* putout *******************************/
/**********************************************************************/

void
putout(x)				/* put a 16-bit raster pattern */
					/* to the BitGraph */
register INT16 x;			/* number to put out */

{
    BOOLEAN negative;			/* was x negative? */
    register INT16 part1, part2, part3;	/* parts of number */

    negative = FALSE;
    if (x < 0)
    {
	negative = TRUE;
	x = -x;
    }
    part1 = (x & 0xfc00) >> 10;
    part2 = (x & 0x03f0) >>  4;
    part3 = (x & 0x000f);
    if (part1)
    {
	OUTC(0100+part1);
	OUTC(0100+part2);
    }
    else
	if (part2)
	    OUTC(0100+part2);
    if (negative)
	OUTC(040+part3);
    else
	OUTC(060+part3);
}

#include "readfont.h"
#include "readgf.h"
#include "readpk.h"
#include "readpost.h"
#include "readpxl.h"
#include "reldfont.h"

/*-->rsetterm*/
/**********************************************************************/
/****************************** rsetterm ******************************/
/**********************************************************************/

void
rsetterm()   /* Reset Terminal */
{
    if (plotfp == (FILE *)NULL)
	return;
    OUTS("\033\\");			/* ST  String Terminator (in case of */
					/* earlier errors resulting in */
					/* incomplete output */
    OUTS("\033:2L");			/* BBNSFP - Set Font Parameters */
					/* to reset all fonts */
    OUTS("\033:]");			/* BBNPOP - Pop Context  */
    OUTS("\033:2e");			/* BBNSEM Select Emulation Mode VT100 */

    /* The Pop Context does not restore the state of the world; the terminal */
    /* comes back overwriting text and believing it has only 24 lines instead */
    /* of 64.  The following commands fix that.  The order is important, and */
    /* DECSTBM must come last, or it will revert to 24 line scrolling. */

    OUTS("\033");
    OUTS("7");				/* DECSC - save cursor */
    OUTS("\033:3o");			/* BBNSTO - set text operation */
					/* to replace */
    OUTS("\033");
    OUTS("8");				/* DECRC - restore cursor */
    OUTS("\033[2J");			/* clear screen */
    OUTS("\033(B");			/* SCS - select character set */
    OUTS("\033[1;64;1;80r");		/* DECSTBM - set top and bottom */
					/* margins to full screen  */
    (void)fflush(plotfp);

#if    OS_TOPS20

    /* restore the terminal settings saved at entry */

    ac1 = jfn_plotfp;
    ac2 = bg_modeword;
    if (jsys(JSsfmod,acs) != JSok)	/* set jfn mode word */
	;				/* ignore errors */

    ac1 = jfn_plotfp;
    ac2 = bg_modeword;
    if (jsys(JSstpar,acs) != JSok)	/* set more of jfn mode word */
	;				/* ignore errors */

    ac1 = Value(TL_sab) | Value(TL_sta) | Value(TL_obj);
    if ((bg_modeword & Value(TT_alk)) != 0)
	ac1 |= Value(TL_abs);		/* terminal was receiving links */
    if ((bg_modeword & Value(TT_aad)) != 0)
	ac1 |= Value(TL_aad);		/* terminal was receiving advice */
    ac2 = -1;
    if (jsys(JStlink,acs) != JSok)	/* set links and advice action */
	;				/* ignore errors */

    ac1 = jfn_plotfp;
    ac2 = bg_1ccoc;
    ac3 = bg_2ccoc;
    if (jsys(JSsfcoc,acs) != JSok)	/* set terminal control codes */
	;				/* ignore errors */

    ac1 = jfn_plotfp;
    ac2 = MOslw;
    ac3 = bg_width;
    if (jsys(JSmtopr,acs) != JSok)	/* set terminal line width */
	;				/* ignore errors */


    ac1 = jfn_plotfp;
    ac2 = MOsll;
    ac3 = bg_length;
    if (jsys(JSmtopr,acs) != JSok)	/* set terminal page length */
	;				/* ignore errors */


    ac1 = jfn_plotfp;
    ac2 = MOsnt;
    ac3 = bg_sysmsg;
    if (jsys(JSmtopr,acs) != JSok)	/* set terminal refuse/accept */
					/* system message status */
	;				/* ignore errors */

#endif /* OS_TOPS20 */

    if (g_interactive)
	(void)kbclose();
}

#include "rulepxl.h"

/*-->setchar*/
/**********************************************************************/
/****************************** setchar *******************************/
/**********************************************************************/

void
setchar(c, update_h)
register BYTE c;
register BOOLEAN update_h;
{
    register struct char_entry *tcharptr;  /* temporary char_entry pointer */
    void (*charyy)();		/* subterfuge to get around PCC-20 bug */

    if (DBGOPT(DBG_SET_TEXT))
    {
	(void)fprintf(stderr,"setchar('");
	if (isprint(c))
	    (void)putc(c,stderr);
	else
	    (void)fprintf(stderr,"\\%03o",(int)c);
	(void)fprintf(stderr,"'<%d>) (hh,vv) = (%ld,%ld) font name <%s>",
	    (int)c, (long)hh, (long)vv, fontptr->n);
	NEWLINE(stderr);
    }

    tcharptr = &(fontptr->ch[c]);

    /*
    Any character which is off screen is completed ignored; there is  no
    point  sending  bitmaps  or   chardefs  for  invisible   characters.
    Partially visible  characters could  perhaps be  supported, but  for
    now, we ignore them too.
    */

    if (((hh - tcharptr->xoffp + tcharptr->pxlw) < (XSIZE - xscreen))
        && (hh >= -xscreen)
	&& (vv < (YSIZE + yscreen))
	&& ((vv - tcharptr->yoffp)  >= yscreen))
    {				/* character fits entirely on screen */
	if (tcharptr->istoobig)
	{	/* big character must be displayed in raster graphics mode */
	    (void)fflush(plotfp);
	    if (fontptr != pfontptr)
	        (void)openfont(fontptr->n);
	    if (fontfp == (FILE *)NULL)	/* do nothing if no font file */
	        return;
	    OUTF2("\033:%d;%dm",		/* BBNMOV  Move Graphically */
		(pbghpos=(INT16)((hh-tcharptr->xoffp))+xscreen),
		(pbgvpos=(INT16)((YSIZE-vv+tcharptr->yoffp))+yscreen));
	    OUTF2("\033P:3;%d;%ds",tcharptr->wp, tcharptr->hp);
					/* BBNDPD  Display Pixel Data */
	    /* Bug fix: PCC-20 otherwise jumps to charxx instead of *charxx */
	    charyy = fontptr->charxx;
	    (void)(*charyy)(c,outrow);	/* load rasters into device */
	    OUTS("\033\\");		/* ST  String Terminator */
	}
	else	/* small character to be downloaded */
	{
	    if (!tcharptr->isloaded)
	    {
	        (void)fflush(plotfp);
		if (fontptr != pfontptr)
		    (void)openfont(fontptr->n);
		if (fontfp == (FILE *)NULL)	/* do nothing if no font file */
		    return;
		if ((bgcp[tcharptr->bgfont][tcharptr->bgchar] != NULL) &&
		    (bgcp[tcharptr->bgfont][tcharptr->bgchar]->isloaded))
		{
		    bgcp[tcharptr->bgfont][tcharptr->bgchar]->isloaded = FALSE;
		    OUTF2("\033:2;%d;%cL",tcharptr->bgchar,
			  '0'+tcharptr->bgfont);
					/* BBNSFP Set Font Parameters */
					/* to reset character */
		}

		OUTS("\033P:0;");	/* BBNLFC  Load Font Character */
		OUTF("%d;",tcharptr->bgchar);
		OUTC('0'+tcharptr->bgfont);
		OUTC(';');
		OUTF("%d;",tcharptr->wp);
		OUTF("%d;",tcharptr->hp);
		OUTF("%d;",tcharptr->xoffp);
		OUTF("%d;",tcharptr->hp-tcharptr->yoffp-1);
		OUTF("%dL",tcharptr->pxlw-tcharptr->xoffp);

		/* Bug fix: PCC-20 jumps to charxx instead of *charxx */
		charyy = fontptr->charxx;
		(void)(*charyy)(c,outrow);	/* load rasters into device */
		OUTS("\033\\");		/* ST  String Terminator */

		tcharptr->isloaded = TRUE;
		bgcp[tcharptr->bgfont][tcharptr->bgchar] = tcharptr;
	    }
	    xdiff = hh-pbghpos;
	    ydiff = YSIZE-vv-pbgvpos;
	    if ((xdiff) || (ydiff))
	        if ((ABS(xdiff) <= XDIFFMAX) && (ABS(ydiff) <= YDIFFMAX))
		{
		    OUTC('\016');/* SO  Switches to the G1 character set */
		    if (xdiff)
			OUTC(XDIFFORG+xdiff);
		    if (ydiff)
			OUTC(YDIFFORG+ydiff);
		    OUTC('\017');/* SI  Switches to the G0 character set */
		}
		else
		    OUTF2("\033:%d;%dm",hh+xscreen, YSIZE-vv+yscreen);
					/* BBNMOV  Move Graphically */

	    if (tcharptr->bgfont != pbgf)
	    {			/* SCS Select G0 to be a loadable font */
		OUTF("\033(%c",(char)('0'+tcharptr->bgfont));
		pbgf = tcharptr->bgfont;
	    }
	    OUTC(tcharptr->bgchar);
	    pbghpos = (INT16)(hh + tcharptr->pxlw - tcharptr->xoffp);
	    pbgvpos = (INT16)(YSIZE - vv);
	}
    }

    if (update_h)
    {
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh = (COORDINATE)(fixpos(hh-lmargin,h,conv) + lmargin);
    }
}

#include "setfntnm.h"
#include "setrule.h"
#include "signex.h"
#include "skgfspec.h"
#include "skipfont.h"
#include "skpkspec.h"
#include "special.h"
#include "strchr.h"
#include "strcm2.h"
#include "strid2.h"
#include "strrchr.h"
#include "tctos.h"

/*-->unloadfonts*/
/**********************************************************************/
/***************************** unloadfonts ****************************/
/**********************************************************************/

void
unloadfonts()			/* mark all fonts as not loaded */
{				/* and set no current fonts */
    INT16 k;

    for (fontptr = hfontptr; fontptr != (struct font_entry *)NULL;
	 fontptr = fontptr->next)
    {
	for (k = 0; k < NPXLCHARS; ++k)
	    fontptr->ch[k].isloaded = FALSE;

	if (fontptr->font_file_id != (FILE*)NULL)
	{
	    (void)fclose(fontptr->font_file_id);
	    fontptr->font_file_id = (FILE*)NULL;
	}
    }

    fontfp = (FILE*)NULL;		/* no current font file */
    for ( ; nopen > 0; --nopen)		/* clear font file cache */
    {
	font_files[nopen].font_id = (FILE*)NULL;
	font_files[nopen].use_count = (INT16)0;
    }

    /* NB: It is important here that the loop index be global; the relation
    of fontptr to pfontptr is used by openfont() to decide whether the font
    file is already open. */
    for (fontptr = hfontptr; fontptr != (struct font_entry *)NULL;
	 fontptr = fontptr->next)
    {
	pfontptr = (struct font_entry *)(NULL); /* so reldfont() calls openfont() */
	(void)reldfont(fontptr);	/* get new font metrics */
    }
}

#include "usage.h"
#include "warning.h"

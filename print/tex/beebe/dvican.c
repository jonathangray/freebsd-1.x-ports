/* -*-C-*- dvican.c */
/* Developed for Canon LBP-8 A2 from DVIIMP.C and DVIJEP.C [03-Jan-87]
   Page references in comments refer to
   Canon LBP-8 A1/A2 Laser Beam Printer User's Manual
   edition PUB.R-IE-034-V1C.0186B2.5 (1985)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    NB: This is a preliminary release.  Serious problems with the  Canon
    A2 downloaded font mechanism force loading of characters as  bitmaps
    at every reference, making it very slow....
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/*-->DVICAN*/
/**********************************************************************/
/******************************* dvican *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef CANON_A2
#define  CANON_A2  1			/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Canon LBP-8 A2 laser printer"
					/* this string is printed at runtime */

#define OUTFILE_EXT	"can"

#define  BYTE_SIZE	  8		/* output file byte size */

#undef STDRES
#define STDRES  1			/* to get standard font resolution */

#define  MAXLINE	4096		/* maximum input file line size */
					/* it is needed only in special() */

#define  XDPI		300		/* horizontal dots/inch */
#define  XPSIZE		9		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */

#define  YDPI		300		/* vertical dots/inch */
#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

/* The following values may be printer model dependent, or even  printer
serial number  dependent.  These  values were  obtained with  the  LaTeX
TESTPAGE.TEX file for a Canon LBP-8  A2 printer; they express that  fact
that the real (0,0) is 30 dots to  the left of, and 111 dots above,  the
top left corner of an 8.5in x  11in page.  This disagrees with p. 53  of
the User's Manual which gives values of 48 and 60. */

#define XORIGIN		 30		/* measured pixel coordinates of */
#define YORIGIN		111		/* page origin (should be at (0,0)) */

/* All Canon A2 command sequences are defined symbolically here, so that no
magic byte strings appear in the text. */

#define CSI "\033["		/* control sequence introducer */

#define DOT_MODE "\033[11h"	/* coordinates in dots */

#define FONTNUMBER(n) (int)(200 + (n))	/* user font number in 200..255 */

#define GS_NUMBER(n) (int)(3840 + (n))	/* graphics set number in 3840..4095 */

#define HARD_RESET "\033c"	/* hard reset to power-on defaults */

#define ISO_MODE "\033;"	/* ISO text mode */

#define RESET_MODE "\033[?1;4;5;6;7l"	/* set no auto CR/LF, no LF with CR, */
				/* no CR with LF, no CR with FF, no CR */
				/* with VT */

#define SEL_PAINT "\033[2&z"	/* select paint memory mode - full */

#define SET_MODE "\033[?2;3h"	/* set no auto FF, no auto position feed */

#define SIZE_UNIT "\033[7\040I"	/* units of 1/300 inch (i.e. dots) */

#define SPACING_INCREMENT "\033[0;0\040G"	/* set HMI and VMI to zero; */
				/* all character spacing is done explicitly */

/* Horizontal rule with lower-left corner at (x,y).  NB: h in 1..9 */
#define HRULE(x,y,w,h) {MOVETO(x,y); OUTCSI; OUTNUM(h); OUTC('{');\
    RMOVEX(w); OUTCSI; OUTC('}');}

/* Map characters (0..32,33..127) to (160..192,33..127) */
#define MAPCHAR(c) (((c) > 32) ? (c) : ((c) + 160))

/* Move absolute to (x,y), page origin in lower left corner */
#define MOVETO(x,y) {MOVEX(x); MOVEY(y);}

/* Move absolute to (x,current y), page origin in lower left corner */
#define MOVEX(x) {OUTCSI; OUTNUM(x); OUTC('\140');}

/* Move absolute to (current x,y), page origin in lower left corner */
#define MOVEY(y) {OUTCSI; OUTNUM(YSIZE-y); OUTC('d');}

/* Output bitmap prefix to be followed by <bytecount> bytes of the bitmap */
#define OUTBITMAP(bytecount,bytewidth) {\
    OUTCSI;\
    OUTNUM(bytecount);\
    OUTC(';');\
    OUTNUM(bytewidth);\
    OUTS(".r");}

/* Output TeX character number with mapping to Canon number */
#define OUTCHAR(c) OUTC(MAPCHAR(c))

/* Output control sequence prefix */
#define OUTCSI {OUTC('\033'); OUTC('[');}

/* Output a number as a minimal decimal digit string */
#define OUTNUM(n) OUTF("%d",n)

/* Eject a page from the printer--this is the last command on each page */
#define PAGEEJECT OUTC('\f')

/* Move relative by (dx,0) from current point */
#define RMOVEX(dx) if ((dx) > 0) \
	    {OUTCSI; OUTNUM(dx); OUTC('a');} \
	else if ((dx) < 0) \
	    {OUTCSI; OUTNUM(-(dx)); OUTC('j');}

/* Move relative by (0,dy) from current point */
#define RMOVEY(dy) if ((dy) > 0) \
	    {OUTCSI; OUTNUM(dy); OUTC('k');} \
	else if ((dy) < 0) \
	    {OUTCSI; OUTNUM(-(dy)); OUTC('e');}

/* Set  the number  of copies  of the  current page  (issue just  before
PAGEEJECT) */
#define SETCOPIES(n) {OUTCSI; OUTNUM(n); OUTC('v');}

/* Set the current font number.  Canon A2 seems to need both a font
number and an encoded graphics set number, sigh... */
#if    OS_ATARI
#define SETCURRENTFONT(n) setcurrentfont(n)
void
setcurrentfont(n)
UNSIGN16 n;
{
    OUTS("\033\050");
    OUTC(0x20 | (0x0f & (GS_NUMBER(n)>>8)));
    OUTC(0x20 | (0x0f & (GS_NUMBER(n)>>4)));
    OUTC(0x30 | (0x0f & (GS_NUMBER(n))));
    OUTF2("%s%dy",CSI,FONTNUMBER(n));
}
#else
#define SETCURRENTFONT(n) {OUTS("\033\050");\
    OUTC(0x20 | (0x0f & (GS_NUMBER(n)>>8)));\
    OUTC(0x20 | (0x0f & (GS_NUMBER(n)>>4)));\
    OUTC(0x30 | (0x0f & (GS_NUMBER(n))));\
    OUTF2("%s%dy",CSI,FONTNUMBER(n));}
#endif

/* Vertical rule with lower-left corner at (x,y). NB: w in 1..9 */
#define VRULE(x,y,w,h) {MOVETO(x,y); OUTCSI; OUTNUM(w); OUTC('{');\
    RMOVEY(h); OUTCSI; OUTC('}');}

#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"

/*-->bopact*/
/**********************************************************************/
/******************************* bopact *******************************/
/**********************************************************************/

void
bopact()			/* beginning of page action */
{
    str_ycp = -1;		/* last string ycp */
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
    OUTS(HARD_RESET);		/* reset to power-up status */
    OUTS(ISO_MODE);		/* we do everything in ISO mode */
    OUTS(SEL_PAINT);		/* paint memory full needed for downloaded */
				/* character support */
    OUTS(DOT_MODE);		/* spacing in units of dots */
    OUTS(SIZE_UNIT);		/* ditto, sigh... */
    OUTS(SPACING_INCREMENT);	/* HMI and VMI to 0 to prevent auto spacing */
    OUTS(SET_MODE);		/* these two prevent page ejects when objects */
    OUTS(RESET_MODE);		/* extend into margins */

    font_count = 0;		/* no font numbers are assigned yet */
    font_switched = TRUE;
}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
    OUTS(HARD_RESET);		/* reset to power-up status */
}

#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"

/*-->eopact*/
/**********************************************************************/
/******************************* eopact *******************************/
/**********************************************************************/

void
eopact()			/* end of page action */
{
    if (copies > 1)
        SETCOPIES(copies);
    PAGEEJECT;
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
With the  page origin  (0,0) at  the lower-left  corner, draw  a  filled
rectangle at (x,y).

For most  TeX  uses, rules  are  uncommon, and  little  optimization  is
possible.  However, for the LaTeX Bezier option, curves are simulated by
many small rules (typically  2 x 2)  separated by positioning  commands.

It is not possible to use relative, instead of absolute, moves in  these
sequences, without stacking rules for the whole page, because each  rule
is separated in  the DVI file  by push, pop,  and positioning  commands,
making  for  an  uncertain  correspondence  between  internal  (xcp,ycp)
pixel page coordinates and external device coordinates.

The last string y coordinate, str_ycp,  must be reset here to force  any
later setstr() to reissue new absolute positioning commands.
***********************************************************************/

{
    str_ycp = -1;		/* invalidate string y coordinate */

    /*
    Output a new  rule at  TeX position (x,y).   The device  coordinates
    will  be  changed  on  completion.   The  rule  origin  is  the  TeX
    convention of the lower-left corner.   The Canon A2 limits  vertical
    rule widths, and horizontal rule heights, to 9 dots, so we may  need
    to generate more than one rule request.
    */


    if (width > height)			/* horizontal rule */
    {
        for (; height > 0; (y += 9, height -= 9))
	    HRULE(x,y,width,height);
    }
    else				/* vertical rule */
    {
        for (; width > 0; (x += 9, width -= 9))
	    VRULE(x,y,width,height);

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

/*-->loadbmap*/
/**********************************************************************/
/****************************** loadbmap ******************************/
/**********************************************************************/

void
loadbmap(c)			/* load big character as raster bitmap */
register BYTE c;
{
    register struct char_entry *tcharptr;  /* temporary char_entry pointer */
    register INT16 bytewidth;
    void (*charyy)();		/* subterfuge to get around PCC-20 bug */

    if (fontptr != pfontptr)
        (void)openfont(fontptr->n);
    if (fontfp == (FILE *)NULL)	/* do nothing if no font file */
	return;

    tcharptr = &(fontptr->ch[c]);

    moveto(hh - tcharptr->xoffp,YSIZE-vv+tcharptr->yoffp);

    MOVETO(xcp,ycp);
    bytewidth = ((tcharptr->wp)+7)>>3;
    OUTBITMAP(bytewidth*(tcharptr->hp), bytewidth);

    /* Bug fix: PCC-20 otherwise jumps to charxx instead of *charxx */
    charyy = fontptr->charxx;
    (void)(*charyy)(c,outrow);	/* load rasters into device */
}

/*-->loadchar*/
/**********************************************************************/
/****************************** loadchar ******************************/
/**********************************************************************/

    /*******************************************************************

	<------wp------>
    ^	................ ^
    |	................ |
    |	................ |
    |	................ |
    |	................ |
    |	................ |--- yoffp (negative if (xcp,ycp) above bitmap)
    |	................ |
    hp  ................ |
    |	................ |
    |	................ |
    |	................ |
    |	.....o.......... v     <-- character reference point (xcp,ycp) at "o"
    |	................
    |	................
    |	................
    |	................
    v	+...............       <-- (xcorner,ycorner) at "+"
	<--->
	  |
	  |
	 xoffp (negative if (xcp,ycp) left of bitmap)
	 when char set: (xcp',ycp') := (xcp+tfmw,ycp)

    -----			--------
    TeX				Canon A2
    -----			--------
    xoffp			left offset
    yoffp			top offset
    wp				width
    hp				height
    tfmw			advance width

    *******************************************************************/

void
loadchar(c)
register BYTE c;
{
    int baseline;

    void (*charyy)();		/* subterfuge to get around PCC-20 bug */
    register struct char_entry *tcharptr; /* temporary char_entry pointer */

    if ((c < FIRSTPXLCHAR) || (LASTPXLCHAR < c)) /* check character range */
	return;

    tcharptr = &(fontptr->ch[c]);

    if (!VISIBLE(tcharptr))
	return;				/* do nothing for invisible fonts */

    if (fontptr != pfontptr)
	openfont(fontptr->n);

    if (fontfp == (FILE *)NULL)		/* do nothing if no font file */
	return;

    (void)clearerr(plotfp);

    tcharptr->isloaded = TRUE;

    baseline = tcharptr->hp - tcharptr->yoffp;
    if (baseline >= 0)
        baseline = MAX(1,MIN(baseline,tcharptr->hp));
    else
        baseline = -MAX(1,MIN(-baseline,tcharptr->hp));

    OUTCSI;
    OUTF("%d;",
	 2+(int)(((tcharptr->wp)+7)>>3)*(tcharptr->hp)); /* byte count */
    OUTS("1;0;0;");
    OUTF("%d;",GS_NUMBER(fontptr->font_number)); /* graphics set number */
    OUTS("1;");
    OUTF("%d;",(int)(tcharptr->hp)*24);	/* hp*7200/300 == ht in centipoints */
    OUTF("%d;",
	 (int)(30000/MAX(1,tcharptr->pxlw)));/* pitch as 100*chars/inch */
    OUTS("0;0;");
    OUTF("%d;",FONTNUMBER(fontptr->font_number));/* font number */
    OUTF("%d;",(int)(tcharptr->wp));	/* cell width in 1..64 */
    OUTF("%d;",(int)(tcharptr->hp));	/* cell height in 1..64 */
    OUTF("%d;",(int)ABS(baseline));	/* cell baseline */
    OUTS("1;1;0;");
    OUTF("%d.p",(baseline >= 0 ? 0 : 1)); /* cell baseline sign flag */
    OUTC(c);				/* character code */
    OUTC(tcharptr->pxlw);		/* character width */
    /* Now comes binary raster data */

    /* Bug fix: PCC-20 otherwise jumps to charxx instead of *charxx */
    charyy = fontptr->charxx;
    (void)(*charyy)(c,outrow);		/* output rasters */

    SETCURRENTFONT(fontptr->font_number);	/* Canon seems to need this */
    font_switched = FALSE;		/* again--most other devices do not */

    if (DISKFULL(plotfp))
	(void)fatal("loadchar():  Output error -- disk storage probably full");
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
    INT16 j;			/* loop index */

    for (the_char = FIRSTPXLCHAR; the_char <= LASTPXLCHAR; the_char++)
	fontptr->ch[the_char].isloaded = FALSE;

    for (j = 0; j < (INT16)font_count; ++j)
	if (font_table[j] == fontptr)	/* then font already known */
	    break;

    if (j >= (INT16)font_count)		/* new font */
    {
	fontptr->font_number = font_count;
	font_table[font_count++] = fontptr;
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
outrow(c,yoff)	/* copy img_row[] into rasters[] if allocated, else no-op */
BYTE c;		/* current character value */
UNSIGN16 yoff;	/* offset from top row (0,1,...,hp-1)  */
{
    UNSIGN16 bytes_per_row;	/* number of raster bytes to copy */
    register UNSIGN16 k;	/* loop index */
    register UNSIGN32 *p;	/* pointer into img_row[] */
    struct char_entry *tcharptr;/* temporary char_entry pointer */
    register BYTE the_byte;	/* unpacked raster byte */

    tcharptr = &(fontptr->ch[c]);/* assume check for valid c has been done */
    bytes_per_row = ((tcharptr->wp) + 7) >> 3;	/* wp div 8 */
    p = img_row;		/* we step pointer p along img_row[] */

    for (k = bytes_per_row; ; ++p)
    {
	the_byte = (BYTE)((*p) >> 24);
	OUTC(the_byte);
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)((*p) >> 16);
	OUTC(the_byte);
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)((*p) >> 8);
	OUTC(the_byte);
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)(*p);
	OUTC(the_byte);
	if ((--k) <= 0)
	    break;
    }
}

#include "prtpage.h"
#include "readfont.h"
#include "readgf.h"
#include "readpk.h"
#include "readpost.h"
#include "readpxl.h"
#include "reldfont.h"
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

    /* BIGCHAR() and ONPAGE() are used here and in setstr() */

#define BIGCHAR(t) ((t->wp > (COORDINATE)size_limit) ||\
    (t->hp > (COORDINATE)size_limit))

#define ONPAGE(t) (((hh - t->xoffp + t->pxlw) <= XSIZE) \
    && (hh >= 0)\
    && (vv <= YSIZE)\
    && (vv >= 0))

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

    moveto(hh,YSIZE-vv);
    if (ONPAGE(tcharptr))
    {				/* character fits entirely on page */
	if (font_switched)
	{
	    SETCURRENTFONT(fontptr->font_number);
	    font_switched = FALSE;
	}

	if (VISIBLE(tcharptr))
	{
	    if (BIGCHAR(tcharptr))
	        loadbmap(c);
	    else
	    {
		if (!tcharptr->isloaded)
 		    loadchar(c);

		if (ycp != str_ycp)
		{
		    MOVETO(xcp,ycp);
		    str_ycp = ycp;
		}
		else
	            MOVEX(xcp);

		OUTCHAR(c);
	    }
	}
    }
    else if (DBGOPT(DBG_OFF_PAGE) && !quiet)
    {				/* character is off page -- discard it */
	(void)fprintf(stderr,
	    "setchar(): Char %c [10#%3d 8#%03o 16#%02x] off page.",
	    isprint(c) ? c : '?',c,c,c);
	NEWLINE(stderr);
    }

    if (update_h)
    {
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh = fixpos(hh-lmargin,h,conv) + lmargin;
    }
}

#include "setfntnm.h"
#include "setrule.h"

/*-->setstr*/
/**********************************************************************/
/******************************* setstr *******************************/
/**********************************************************************/

void
setstr(c)
register BYTE c;
{
    register struct char_entry *tcharptr;  /* temporary char_entry pointer */
    register BOOLEAN inside;
    INT32 h0,v0;		/* (h,v) at entry */
    COORDINATE hh0,vv0;		/* (hh,vv) at entry */
    COORDINATE hh_last;		/* hh before call to fixpos() */
    register UNSIGN16 k;	/* loop index */
    UNSIGN16 nstr;		/* number of characters in str[] */
    BYTE str[MAXSTR+1];		/* string accumulator */
    BOOLEAN truncated;		/* off-page string truncation flag */

    /*******************************************************************
    Set a sequence of characters in SETC_000 .. SETC_127 with a  minimal
    number of commands.  These sequences tend to occur in long clumps in
    a  DVI   file,  and   setting   them  together   whenever   possible
    substantially decreases  the overhead  and the  size of  the  output
    file.  A sequence can be set as a single string if

	* TeX and device coordinates of  each character agree, AND

	* each character is in the same font (this will always be true
	  in a sequence from a DVI file), AND

	* each character fits within the page boundaries, AND

	* each character definition is already loaded, AND

	* each character is from a visible font

    Whenever any of these conditions  does not hold, any string  already
    output is terminated, and a new one begun.

    Two output optimizations are implemented here.  First, up to  MAXSTR
    (in practice more  than enough) characters  are collected in  str[],
    and any  that  require downloading  are  handled.  Then  the  entire
    string is set at once, subject to the above limitations.  Second, by
    recording the vertical page coordinate, ycp, in the global  variable
    str_ycp (reset  in  prtpage()  at begin-page  processing),  it  is
    possible to avoid  outputting y coordinates  unnecessarily, since  a
    single line of  text will  generally result  in many  calls to  this
    function.
    *******************************************************************/

#define BEGINSTRING {inside = TRUE;\
    if (ycp != str_ycp)\
    {\
	MOVETO(xcp,ycp);\
	str_ycp = ycp;\
    }\
    else\
	MOVEX(xcp);}

#define ENDSTRING {inside = FALSE;}

#define OFF_PAGE (-1)	/* off-page coordinate value */

    if (font_switched)	/* output new font selection */
    {
	SETCURRENTFONT(fontptr->font_number);
	font_switched = FALSE;
    }

    inside = FALSE;
    truncated = FALSE;

    hh0 = hh;
    vv0 = vv;
    h0 = h;
    v0 = v;
    nstr = 0;
    while ((SETC_000 <= c) && (c <= SETC_127) && (nstr < MAXSTR))
    {			/* loop over character sequence */
	tcharptr = &(fontptr->ch[c]);

	moveto(hh,YSIZE-vv);

	if (ONPAGE(tcharptr) && VISIBLE(tcharptr))
	{		/* character fits entirely on page and is visible */
	    if ((!tcharptr->isloaded) && (!BIGCHAR(tcharptr)))
		    loadchar(c);
	}
	/* update horizontal positions in TFM and pixel units */
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;

	str[nstr++] = c;		/* save string character */

	c = (BYTE)nosignex(dvifp,(BYTE)1);
    }

    /* put back character which terminated the loop */
    (void)UNGETC((int)(c),dvifp);

    hh = hh0;			/* restore coordinates at entry */
    vv = vv0;
    h = h0;
    v = v0;

    if (DBGOPT(DBG_SET_TEXT))
    {
	(void)fprintf(stderr,"setstr(\"");
	for (k = 0; k < nstr; ++k)
	{
	    c = str[k];
	    if (isprint(c))
	        (void)putc(c,stderr);
	    else
	        (void)fprintf(stderr,"\\%03o",(int)c);
	}
	(void)fprintf(stderr,"\") (hh,vv) = (%ld,%ld) font name <%s>",
	    (long)hh, (long)vv, fontptr->n);
	NEWLINE(stderr);
    }

    for (k = 0; k < nstr; ++k)
    {			/* loop over character sequence */
	c = str[k];
	tcharptr = &(fontptr->ch[c]);
	moveto(hh,YSIZE-vv);

	if (ONPAGE(tcharptr) && VISIBLE(tcharptr))
	{		/* character fits entirely on page and is visible */
	    if (tcharptr->isloaded) /* character already downloaded */
	    {
		if (!inside)
		    BEGINSTRING;
		OUTCHAR(c);
	    }
	    else	/* character must be downloaded first */
	    {
		if (inside)
		    ENDSTRING;	    /* finish any open string */
		if (BIGCHAR(tcharptr))
		    loadbmap(c);
	    }
	}
	else		/* character does not fit on page -- output */
	{		/* current string and discard the character */
	    truncated = TRUE;
	    if (inside)
		ENDSTRING;
	}
	/* update horizontal positions in TFM and pixel units */
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh_last = hh;
	hh = fixpos(hh-lmargin,h,conv) + lmargin;
	if (DBGOPT(DBG_POS_CHAR))
	{
	    (void)fprintf(stderr,
		"[%03o] xcp = %d\tycp = %d\thh = %d\thh_last = %d\n",
		c,xcp,ycp,hh,hh_last);
	}

	/* If fixpos() changed position, we need new string next time */
	if ((hh != hh_last) && inside)
	    ENDSTRING;
    }
    if (truncated && DBGOPT(DBG_OFF_PAGE) && !quiet)
    {
	(void)fprintf(stderr,"setstr(): Text [");
	for (k = 0; k < nstr; ++k)
	    (void)fprintf(stderr,isprint(str[k]) ? "%c" : "\\%03o",str[k]);
	(void)fprintf(stderr,"] truncated at page boundaries.");
	NEWLINE(stderr);
    }
    if (inside)		/* finish last string */
	ENDSTRING;
}

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
#include "usage.h"
#include "warning.h"

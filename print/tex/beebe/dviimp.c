/* -*-C-*- dviimp.c */
/*-->dviimp*/
/**********************************************************************/
/******************************* dviimp *******************************/
/**********************************************************************/

#include "dvihead.h"

/***********************************************************************
***********************************************************************/

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef IMPRESS
#define  IMPRESS  1			/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"imPRESS [Imagen laser printer family]"
					/* this string is printed at runtime */

#define OUTFILE_EXT	"imp"

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
TESTPAGE.TEX file for  an Imagen  3320 printer; they  express that  fact
that the real (0,0) is 15 dots above  and 3 dots to the left of the  top
left corner of an 8.5in x 11in page. */

#define XORIGIN		-3		/* measured pixel coordinates of */
#define YORIGIN		-15		/* page origin (should be at (0,0)) */

/***********************************************************************
Define macros for imPRESS commands.

With the  exception of  the function  "loadchar", which  requires  extra
data, and the function fillrect, for which the macro definition was  too
complicated for the compiler, NO  escape sequences appear anywhere  else
in the text.
***********************************************************************/

#define IM_SABSH	(BYTE)135	/* IM_SET_ABS_H */
#define IM_SRELH	(BYTE)136	/* IM_SET_REL_H */
#define IM_SABSV	(BYTE)137	/* IM_SET_ABS_V */
#define IM_SRELV	(BYTE)138	/* IM_SET_REL_V */
#define IM_BRULE	(BYTE)193
#define IM_BGLY		(BYTE)199
#define IM_SET_FAMILY	(BYTE)207
#define IM_PAGE		(BYTE)213
#define IM_ENDPAGE	(BYTE)219
#define IM_EOF		(BYTE)255

#define ROUND(x) ((int)(x + 0.5))

/* Output a 16-bit binary number in two bytes */
#define OUT16(n) {OUTC((n)>>8); OUTC(n);}

    /*******************************************************************
    The imPRESS XY coordinate  system has X increasing  to the right,  Y
    increasing down, and the origin at the upper left corner.

    The DVI  XY coordinate  system  has X  increasing  to the  right,  Y
    increasing up, and the origin at the lower left corner.

    Rather  than  redefine  the  imPRESS  HV  coordinates  (and  advance
    directions) to correspond to  DVI XY coordinates,  I have chosen  to
    let MOVETO/MOVEY/RMOVEY convert from DVI-Y to imPRESS-Y coordinates.
    *******************************************************************/
/* Absolute movement */
#define MOVETO(x,y) if (x||(y-YSIZE)) {MOVEX(x); MOVEY(y);} else OUTC(IM_PAGE)
#define MOVEX(x) {OUTC(IM_SABSH); OUT16(x);}
#define MOVEY(y) {OUTC(IM_SABSV); OUT16(YSIZE-y);}

/* Relative movement */
#define RMOVETO(delx,dely) {RMOVEX(delx); RMOVEY(dely);}
#define RMOVEX(delx) if (delx) {OUTC(IM_SRELH); OUT16(delx);}
#define RMOVEY(dely) if (dely) {OUTC(IM_SRELV); OUT16(-dely);}

/* Eject a page from the printer--this is the last command on each page */
#define PAGEEJECT OUTC(IM_ENDPAGE)

    /*******************************************************************
    State variable defaults are:
         coordinate system:
	     Origin is at top left corner of page (top and left defined
	     by HV)
	     Positive V axis is 90 degrees clockwise from positive H axis
	     Positive H axis is physically "down" the page
	    OUTC(IM_SET_HV_SYSTEM), OUTC( (2<<5) | (2<<3) | 4)
         current position:
	    MOVETO(0,0)
         text print direction:
	     Main advance direction is along H-axis
	     Secondary advance direction is clockwise from Main adv. dir.
	    OUTC(IM_SET_ADV_DIRS), OUTC( (0<<1) | 0 )
         spacing (not used: done by TeX):
	    OUTC(IM_SET_SP), OUT16(0?)
	    OUTC(IM_SET_BOL), OUT16(0)   * default left margin is 0 *
	    OUTC(IM_SET_IL), OUT16(0)    * no default interline spacing *
         selected font:
             OUTC(IM_SET_FAMILY), OUTC(0?)
         graphics (TEXTURE is used for rules, others not used by TeX):
	    OUTC(IM_SET_TEXTURE), OUT16(0)  * Texture is solid black *
	    OUTC(IM_SET_PEN), OUTC(1?)
	    OUTC(IM_SET_MAGNIFICATION), OUTC(0)
	    OUTC(IM_SET_PUM), OUTC(0)
         push mask:
            OUTC(IM_SET_PUSH_MASK), OUTC(0x01FF)
    *******************************************************************/

#define ENDJOB OUTC(IM_EOF)

    /*******************************************************************
    Note: if predefined fonts are  used, the definitions to access  them
    can't be redefined (i.e.  the member maps  and family tables).   And
    once a  glyph  has been  explicitly  defined,  there is  no  way  to
    redefine it as  a reference to  a predefined glyph.   This would  be
    troublesome if TeX used the predefined fonts.
    *******************************************************************/

/* Set the current font number */
#define SETCURRENTFONT(fontnumber) {OUTC(IM_SET_FAMILY);OUTC(fontnumber);}


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
    /* Provide a minimal document header which prevents the printer from
    rearranging the page order. */
    OUTS("@document(language impress, pagecollation off, pagereversal off, \
prerasterization off, jamresistance on");
    if ((int)copies > 1)	/* avoid unnecessary output to allow */
	OUTF(", copies %d",(int)copies); /* later override */
    OUTC(')');
    OUTC(IM_SET_FAMILY);
    OUTC(0);
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
    ENDJOB;
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
later setstring() to reissue new absolute positioning commands.
***********************************************************************/

{
    str_ycp = -1;		/* invalidate string y coordinate */

    /*
    Output a new  rule at  TeX position (x,y).   The device  coordinates
    will  be  changed  on  completion.   The  rule  origin  is  the  TeX
    convention  of  the  lower-left  corner,  while  imPRESS  uses   the
    upper-left corner, but allows an  offset to be provided.  Note  that
    the offset must  be negated because  imPRESS and TeX  differ on  the
    direction of the Y-axis.
    */
    MOVETO(x,y);
    OUTC(IM_BRULE);
    OUT16(width);
    OUT16(height);
    OUT16(-height);
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

    -----			-------
    TeX				imPRESS
    -----			-------
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

    OUTC(IM_BGLY);
    OUT16( (0<<14) + ((fontptr->font_number)<<7) + c);
    OUT16(tcharptr->pxlw);		/* advance width */
    OUT16(tcharptr->wp);		/* width */
    OUT16(tcharptr->xoffp);		/* left offset */
    OUT16(tcharptr->hp);		/* height */
    OUT16(tcharptr->yoffp+1);		/* top offset */

    /* Bug fix: PCC-20 otherwise jumps to charxx instead of *charxx */
    charyy = fontptr->charxx;
    (void)(*charyy)(c,outrow);		/* output rasters */

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

    /* ONPAGE() is used here and in setstr() */

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
	    if (!tcharptr->isloaded)
 		loadchar(c);

	    if (ycp != str_ycp)
	    {
		MOVETO(xcp,ycp);
		str_ycp = ycp;
	    }
	    else
	        MOVEX(xcp);

	    OUTC(c);
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
    number of imPRESS commands.  These  sequences tend to occur in  long
    clumps in a DVI  file, and setting  them together whenever  possible
    substantially decreases  the overhead  and the  size of  the  output
    file.  A sequence can be set as a single string if

	* TeX and imPRESS coordinates of  each character agree, AND

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
	    if (!tcharptr->isloaded) loadchar(c);
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
		OUTC(c);
	    }
	    else	/* character must be downloaded first */
	    {
		if (inside)
		    ENDSTRING;	    /* finish any open string */
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

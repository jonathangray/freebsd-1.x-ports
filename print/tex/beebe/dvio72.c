/* -*-C-*- dvio72.c */
/*-->dvio72*/
/**********************************************************************/
/******************************* dvio72 *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef OKIDATA2410
#define  OKIDATA2410       1		/* conditional compilation flag */

#undef HIRES
#define  HIRES		  0		/* this is 72 dpi version  */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#if    HIRES
#define  DEVICE_ID	"OKIDATA Pacemark 2410 144 dpi dot matrix printer"
					/* this string is printed at runtime */
#define  XDPI		144		/* horizontal dots/inch */
#undef MAXOPEN
#define MAXOPEN		 06		/* limit on number of open PXL files */
#else
#define  DEVICE_ID	"OKIDATA Pacemark 2410 72 dpi dot matrix printer"
					/* this string is printed at runtime */
#define  XDPI		72		/* horizontal dots/inch */
#endif

#define OUTFILE_EXT	"o72"

#define  BYTE_SIZE        7		/* output file byte size */

#undef STDRES
#define STDRES  0			/* 0 for low-resolution devices */

#define  XPSIZE		14		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		XDPI		/* vertical dots/inch */
#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

/* The printer bit map. */

#define XBIT XWORDS
#define YBIT YSIZE

#if    (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD)
#undef SEGMEM
#define SEGMEM 1 /* ( ((long)XBIT * (long)YBIT) > 65536L ) */
#endif

#include "bitmap.h"


#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"
#include "chargf.h"
#include "charpk.h"
#include "charpxl.h"
#include "clrbmap.h"
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
    (void)getbmap();
}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
}

#include "dispchar.h"
#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"
#include "f20open.h"
#include "fatal.h"
#include "fillrect.h"
#include "findpost.h"
#include "fixpos.h"
#include "fontfile.h"
#include "fontsub.h"
#include "getbmap.h"
#include "getbytes.h"
#include "getfntdf.h"
#include "getpgtab.h"
#include "inch.h"
#include "initglob.h"
#include "loadchar.h"

/*-->makechar*/
/**********************************************************************/
/****************************** makechar ******************************/
/**********************************************************************/

char
makechar(p,mask)
UNSIGN32* p[];
register UNSIGN32 mask;
{
    register char c;


#if    SEGMEM
#define Q(n) p[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p[0] - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    c = '\000';
    if (*Q(0) & mask)
	c |= '\001';
    if (*Q(1) & mask)
	c |= '\002';
    if (*Q(2) & mask)
	c |= '\004';
    if (*Q(3) & mask)
	c |= '\010';
    if (*Q(4) & mask)
	c |= '\020';
    if (*Q(5) & mask)
	c |= '\040';
    if (*Q(6) & mask)
	c |= '\100';
    return(c);
}

#include "movedown.h"
#include "moveover.h"
#include "moveto.h"
#include "nosignex.h"
#include "openfont.h"
#include "option.h"

/*-->outline*/
/**********************************************************************/
/****************************** outline *******************************/
/**********************************************************************/

/*
************************************************************************
The OKIDATA Pacemark 2410 has a serious  design flaw in that it lacks  a
command to reset to  power-on state.  This is  serious, because it  uses
ETX to enter graphics mode, ETX STX  to exit graphics mode, and ETX  ETX
to mean ETX as graphics  data.  This means that  a single ETX sent  from
the host may cause it to enter graphics mode, or generate an error if it
is already in graphics mode.  Similarly, an ETX STX will enter  graphics
mode if it is not already in  it, instead of exiting graphics mode.   We
therefore turn on graphics mode for  each line, then turn it off  again.
That way, there may be some remote hope of resynchronizing if we lose an
ETX due to line errors along the way.

From an efficiency standout, it is regrettable that it has no run-length
encoding of graphics data.  The  output quality and speed are  otherwise
quite reasonable as low-cost dot matrix printers go.
************************************************************************
*/

void
outline(pline)
char *pline;
{

/* A few ASCII control character definitions */

#define	STX '\002'
#define	ETX '\003'
#define	SO  '\016'
#define	ESC '\033'

    register INT16 left;
    register char *c;

    for ((left = XSIZE, c = pline + XSIZE - 1);
	(*c == '\000') && (left > 1);
	(--left,--c))			/* trim white space but leave */
	;				/* at least 1 character in line */

    OUTC(ESC);				/* ESC A - 12 cpi for square */
    OUTC('A');				/* aspect ratio*/

#if    HIRES
    OUTC(ESC);
    OUTC('R');				/* ESC R - 144 dpi plot mode */
#else
    OUTC(ESC);
    OUTC('P');				/* ESC P -  72 dpi plot mode */
#endif

    OUTC(ETX);				/* ETX - begin graphics mode */

    for (c = pline; left; --left)
    {
	if (*c == ETX)
	    OUTC(ETX);			/* ETX's must be doubled */
	OUTC(*c);
	++c;
    }
    OUTC(ETX);
    OUTC(SO);				/* ETX SO - graphics carriage return */
    OUTC(ETX);
    OUTC(STX);				/* ETX STX - end graphics mode */
}

#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()
{

    register char *c;			/* pointer into v7[] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */
    register UNSIGN32 mask;		/* mask for single bit selection */
    INT16 i,j,k,ybottom,ytop;
    char v7[XSIZE+2];			/* vertical 7-bit raster encodings */

#undef Q
#if    SEGMEM
    UNSIGN32* q[14];			/* pointers to 14 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    (void)clearerr(plotfp);

    ytop = YBIT-1;

    k = -1;	    /* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j) /* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j,k) == 0)); --k)
	    ;				/* trim white space */
#else
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;			/* trim white space */
#endif

    }
    ybottom = MAX(0,7*((j-1+6)/7));	/* need 7n raster lines */

    for (j = ytop; (j >= ybottom) ; j -= 7)    /* loop over raster lines */
					       /* in groups of 7 */
    {

#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v7[0];		/* vertical 7-bit encodings */
	for (i = 0; i < XBIT; (++p,++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE-1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 16; ++k)	/* compute pointers to 16 rasters */
	        q[k] = BITMAP(j-k,i);
#endif

	    for (k = 0; k < HOST_WORD_SIZE; (++c,++k))/* loop over word bits */
	    {
#if    SEGMEM
		*c = makechar(&q[0],mask);	/* examine bits in 7 adjacent */
					/* rows and build 7-bit */
					/* character value */
#else
		*c = makechar(&p,mask);	/* examine bits in 7 adjacent */
					/* rows and build 7-bit */
					/* character value */
#endif
		mask >>= 1;		/* move masking bit right 1 position */
	    }
	}
	--c;				/* last character encoded */

	outline(&v7[0]);
    }
    OUTS("\r\r\r\r\r\n");		/* force a carriage return since it
    					does not always correctly return the
					head to the left margin */
    OUTC('\f');				/* FF to eject page */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}

#include "prtpage.h"
#include "readfont.h"
#include "readgf.h"
#include "readpk.h"
#include "readpost.h"
#include "readpxl.h"
#include "reldfont.h"
#include "rulepxl.h"
#include "setchar.h"
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
#include "usage.h"
#include "warning.h"

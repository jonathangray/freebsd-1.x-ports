/* -*-C-*- dvim72.c */
/*-->dvim72*/
/**********************************************************************/
/******************************* dvim72 *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef APPLEIMAGEWRITER
#define  APPLEIMAGEWRITER  1		/* conditional compilation flag */

#undef HIRES
#define  HIRES		  0		/* this is 72 dpi version */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#if    HIRES
#define  DEVICE_ID	"Apple ImageWriter 144 dpi dot matrix printer"
					/* this string is printed at runtime */
#define  XDPI		144		/* horizontal dots/inch */
#undef MAXOPEN				/* need to redefine this */
#define MAXOPEN		 06		/* limit on number of open PXL files */
#else
#define  DEVICE_ID	"Apple ImageWriter 72 dpi dot matrix printer"
					/* this string is printed at runtime */
#define  XDPI		72		/* horizontal dots/inch */
#endif

#define OUTFILE_EXT	"m72"

#define  BYTE_SIZE	  8		/* output file byte size */

#undef STDRES
#define STDRES  0		/* 0 for low-resolution devices */

#define  XPSIZE		8		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		XDPI		/* vertical dots/inch */
#define  YPSIZE		22		/* vertical paper size in inches */
#define  YSIZE (((YDPI*YPSIZE+15)/16)*16) /* number of vertical dots (16n)*/

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
    if (runlengthcode && !quiet)
    {
	(void)fprintf(stderr,"[Run-length encoding of output file]");
	NEWLINE(stderr);
    }
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
register UNSIGN32* p[];
register UNSIGN32 mask;
{
    register char c;

#if    SEGMEM
#define Q(n) p[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p[0] - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    c = '\000';

#if    HIRES
    if (*Q(0) & mask)
	c |= '\001';
    if (*Q(2) & mask)
	c |= '\002';
    if (*Q(4) & mask)
	c |= '\004';
    if (*Q(6) & mask)
	c |= '\010';
    if (*Q(8) & mask)
	c |= '\020';
    if (*Q(10) & mask)
	c |= '\040';
    if (*Q(12) & mask)
	c |= '\100';
    if (*Q(14) & mask)
	c |= '\200';
#else
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
    if (*Q(7) & mask)
	c |= '\200';
#endif

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

void
outline(pline)
char *pline;

{
    register INT16 left;
    register char *a,*b,*c;
    INT16 len;

    for ((left = XSIZE, c = pline + XSIZE - 1);
	(*c == '\000') && (left > 1);
	(--left,--c))			/* trim white space but leave */
	;				/* at least 1 character in line */

    /*******************************************************************
    We search for runs as follows.  "b" marks the beginning of unwritten
    data, and "a" anchors the beginning of a run which continues to just
    before "c".   The  run length  is  "c"-"a".  Since  the  run  length
    encoding carries a penalty of additional output equal to the  length
    of the  run  length  prefix  (<ESC>Vnnnn), short  runs  are  not  so
    encoded.

    If a long  enough run  is found,  then the  string "b"  .. "a"-1  is
    output since its length is now  known, followed by the run  encoding
    for the character at "a".  Then "a" and "b" are advanced to "c", and
    the scan continues.

    If the  run  beginning at  "a"  is too  short,  then "a"  is  simply
    advanced.

    Since the  character strings  take on  all values  on 0  .. 255,  we
    cannot store a termination marker, but must instead keep a  counter,
    "left", which is  decremented to  0 when the  end of  the string  is
    reached.

    ?????????????xxxxxxxxxxxxxxxx????????????????????
    ^            ^               ^
    |            |               |
    b            a               c

                 <-----run------>
    *******************************************************************/

    if (runlengthcode && (left > 7))
    {
	for (a = b = pline; (left > 0); --left)
	{
	    for (c = a; (*a == *++c) && left; --left)
		;			    /* advance over run */
	    len = (INT16)(c-a);		    /* "c" points past run */
	    if (len > 7)		    /* output long run */
	    {
	        if (a > b)		    /* output previous string */
		{
		    if ((int)(a-b) & 07)
		        OUTF("\033G%04d",(int)(a - b));
		    else
		        OUTF("\033g%03d",((int)(a - b)) >> 3);
		    for ( ; b < a; b++)
		        OUTC(*b);
		}
		OUTF2("\033V%04d%c",len,*a);
		a = b = c;
	    }
	    else			    /* ignore short run */
	    {
		++a;
		left += len - 1;
	    }
	}
	if (a > b)	    /* output remaining string */
	{
	    if ((int)(a-b) & 07)
	        OUTF("\033G%04d",(int)(a - b));
	    else
	        OUTF("\033g%03d",((int)(a - b)) >> 3);
	    for ( ; b < a; b++)
		OUTC(*b);
	}
    }
    else				/* no runlength coding */
    {
        if (left & 07)
	    OUTF("\033G%04d",left);
	else
	    OUTF("\033g%03d",left >> 3);

    /* cannot use fprintf %s because 0 bytes terminate string */
	for (c = pline; left; --left, c++)
	    OUTC(*c);
    }
    NEWLINE(plotfp);
}

#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()
{
    register BYTE *c;			/* pointer into v8[][] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */

#if    SEGMEM
#else
    UNSIGN32 *ptmp;			/* pointer into bitmap[][] */
#endif

    register UNSIGN32 mask;		/* mask for single bit selection */
    register INT16 k;
    INT16 i,j,ybottom,ytop;

#if    HIRES
    BYTE v8[2][XSIZE];			/* vertical 8-bit raster encodings */

#undef Q
#if    SEGMEM
    UNSIGN32* q[16];			/* pointers to 16 raster lines */
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
	    ;			/* trim white space */

#else
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
#endif

    }
    ybottom = (j/16)*16 + 15;

    OUTS("\033c");			/* reset printer */

    for (j = ytop; (j >= ybottom) ; j -= 16)   /* loop over raster lines */
					       /* in groups of 16 */
    {

#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v8[0][0];		/* vertical 8-bit encodings */
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
		/* examine bits in 16 adjacent rows and build two 8-bit */
		/* character values, the first with rows n,n+2,...,n+14, */
		/* the second with rows n+1,n+3,...,n+15.  The two */
		/* characters are then printed with 1 dot vertical spacing */

#if    SEGMEM
		*c = (BYTE)makechar(&q[0],mask);
		*(c+XSIZE) = (BYTE)makechar(&q[1],mask);
#else
		*c = (BYTE)makechar(&p,mask);
		ptmp = p - XBIT;
		*(c+XSIZE) = (BYTE)makechar(&ptmp,mask);
#endif

		mask >>= 1;		/* move masking bit right 1 position */
	    }				/* end loop over k */
	}				/* end loop over i */

	/* ESC p for 144 dots/inch horizontally */
	/* ESC T01 for 1/144 inch spacing */
	/* ESC g nnn for length/8 count */
	OUTS("\033p\033T01");
	(void)outline((char *)&v8[0][0]);

	/* ESC p for 144 dots/inch horizontally */
	/* ESC T15 for 15/144 inch spacing */
	/* ESC g nnn for length/8 count */
	OUTS("\033p\033T15");
	(void)outline((char *)&v8[1][0]);
    }					/* end loop over j */

    OUTC('\f');				/* eject page with FF */
    OUTS("\033c");			/* reset printer */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}
#else /* NOT HIRES */
    BYTE v8[XSIZE+2];			/* vertical 8-bit raster encodings */

#undef Q
#if    SEGMEM
    UNSIGN32* q[8];			/* pointers to 8 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

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

    }					/* end loop over j */
    ybottom = (j/8)*8 + 7;

    OUTS("\033c");			/* reset printer */

    for (j = ytop; (j >= ybottom) ; j -= 8)    /* loop over raster lines */
					       /* in groups of 8 */
    {

#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v8[0];		/* vertical 8-bit encodings */
	for (i = 0; i < XBIT; (++p,++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE-1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 8; ++k)	/* compute pointers to 8 rasters */
	        q[k] = BITMAP(j-k,i);
#endif

	    for (k = 0; k < HOST_WORD_SIZE; (++c,++k))/* loop over word bits */
	    {
		/* examine bits in 8 adjacent rows and build 8-bit */
		/* character value */

#if    SEGMEM
		*c = (BYTE)makechar(q,mask);
#else
		*c = (BYTE)makechar(&p,mask);
#endif

		mask >>= 1;		/* move masking bit right 1 position */
	    }				/* end loop over k */
	}				/* end loop over i */
	--c;				/* last character encoded */

	/* ESC n for 72 dots/inch horizontally */
	/* ESC T16 for 16/144 inch spacing */
	/* ESC G nnnn for length count */
	OUTS("\033n\033T16");
	(void)outline((char *)&v8[0]);
    }					/* end loop over j */
    OUTC('\f');				/* eject page with FF */
    OUTS("\033c");			/* reset printer */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}
#endif

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

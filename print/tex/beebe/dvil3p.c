/* -*-C-*- dvil3p.c */
/*-->dvil3p*/
/**********************************************************************/
/******************************* dvil3p *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef DECLN03PLUS
#define  DECLN03PLUS 1			/* conditional compilation flag */

#undef HIRES				/* this is 150 dpi version */
#define  HIRES		  1		/* define for 300 dpi version */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#if    HIRES
#define  DEVICE_ID	"Digital LN03-PLUS 300 dpi laser printer"
					/* this string is printed at runtime */
#define  XDPI		300		/* horizontal dots/inch */
#define  YDPI		300		/* vertical dots/inch */
#else
#define  DEVICE_ID	"Digital LN03-PLUS 150 dpi laser printer"
					/* this string is printed at runtime */
#define  XDPI		150		/* horizontal dots/inch */
#define  YDPI		150		/* vertical dots/inch */
#endif

#define OUTFILE_EXT	"l3p"

#define  BYTE_SIZE        7		/* output file byte size */

#undef STDRES
#define STDRES  1		/* 0 for low-resolution devices */

#define  V6SIZE		XSIZE
#define  XPSIZE		8		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */
#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

/* The printer bit map (must have a multiple of 6 rows) */

#define XBIT ((1+2*XWORDS)/2)
#define YBIT (((YSIZE+5)/6)*6)

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
char *pline;				/* pointer to raster line */
{
   /* On return, the caller must output the character that selects the
      spacing to the next line */

    register INT16 left;
    register char *a,*b,*c;
    INT16 len;

    for ((left = V6SIZE, c = pline + V6SIZE - 1);
	(*c == '\077') && (left > 1);
	(--left,--c))			/* trim white space but leave */
	;				/* at least 1 character in line */

    /*******************************************************************
    We search for runs as follows.  "b" marks the beginning of unwritten
    data, and "a" anchors the beginning of a run which continues to just
    before "c".   The  run length  is  "c"-"a".  Since  the  run  length
    encoding carries a penalty of additional output equal to the  length
    of the  run  length  prefix (!<number>), short runs are not encoded.

    If a long  enough run  is found,  then the  string "b"  .. "a"-1  is
    output since its length is now  known, followed by the run  encoding
    for the character at "a".  Then "a" and "b" are advanced to "c", and
    the scan continues.

    If the  run  beginning at  "a"  is too  short,  then "a"  is  simply
    advanced.

    We do not store a termination marker, but instead  keep  a  counter,
    "left", which is  decremented to  0 when the  end of  the string  is
    reached.

    ?????????????xxxxxxxxxxxxxxxx????????????????????
    ^            ^               ^
    |            |               |
    b            a               c

                 <-----run------>
    *******************************************************************/

    if (runlengthcode && (left > 3))
    {
	for (a = b = pline; (left > 0); --left)
	{
	    for (c = a; (*a == *++c) && left; --left)
		;			    /* advance over run */
	    len = (INT16)(c-a);		    /* "c" points past run */
	    if (len > 3)		    /* output long run */
	    {
	        if (a > b)		    /* output previous string */
		{
		    for ( ; b < a; b++)
		        OUTC(*b);
		}
		OUTF2("!%d%c",len,*a);
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
	    for ( ; b < a; b++)
		OUTC(*b);
	}
    }
    else				/* no runlength coding */
    {
       for (c = pline; left; --left, c++)
	   OUTC(*c);
    }
}

#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()
{
    register char *c;			/* pointer into v6[] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */
    register UNSIGN32 mask;		/* mask for single bit selection */
    INT16 i,j,k,ybottom,ytop;
    char v6[V6SIZE];			/* vertical 6-bit raster encodings */

#if    SEGMEM
    UNSIGN32* q[6];			/* pointers to 6 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    ytop = YSIZE-1;

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
    ybottom = MAX(0,6*((j-1)/6));	/* need 6n raster lines */

#if    HIRES
    /* <CR><ESC>[7 I<ESC>P0;0;1q"1;1 for 300 dpi spacing */
    OUTS("\015\033[7 I\033P0;0;1q\0421;1");
#else
    /* <CR><ESC>[7 I<ESC>P0;0;2q"1;1 for 150 dpi spacing */
    OUTS("\015\033[7 I\033P0;0;2q\0421;1");
#endif

    for (j = ytop; (j >= ybottom) ; j -= 6)    /* loop over raster lines */
					       /* in groups of 6 */
    {
#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v6[0];		/* vertical 6-bit encodings */
	for (i = 0; i < XBIT; (++p,++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE-1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 6; ++k)	/* compute pointers to 6 rasters */
	        q[k] = BITMAP(j-k,i);
#endif

	    for (k = 0; k < HOST_WORD_SIZE; (++c,++k))/* loop over word bits */
	    {
		/* examine bits in 6 adjacent rows and build 6-bit */
		/* character value */
		*c = '\0';		/* clear character template */
		if (*Q(0) & mask)
 		    *c |= '\001';
		if (*Q(1) & mask)
		    *c |= '\002';
		if (*Q(2) & mask)
		    *c |= '\004';
		if (*Q(3) & mask)
		    *c |= '\010';
		if (*Q(4) & mask)
		    *c |= '\020';
		if (*Q(5) & mask)
		    *c |= '\040';
		mask >>= 1;		/* move masking bit right 1 position */
		*c += '\077';		/* bias 6-bit value by 63 */
	    }
	};
	outline(&v6[0]);
        OUTS("-\n");
    }
    /* Terminate the graphics image */
    /* <ESC>\<FF>
    */
    OUTS("\033\\\f");

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

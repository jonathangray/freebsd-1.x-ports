/* -*-C-*- dvimpi.c */
/*-->dvimpi*/
/**********************************************************************/
/******************************* dvimpi *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef MPISPRINTER
#define  MPISPRINTER  1			/* conditional compilation flag */

#undef HIRES				/* this is 72 dpi version */
#define  HIRES		  1		/* define for 144 dpi version */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#if    HIRES
#define  DEVICE_ID	"MPI Sprinter 144h x 144v dot matrix printer"
					/* this string is printed at runtime */
#define V6SIZE		(XSIZE+1)/2
#define  XDPI		144		/* horizontal dots/inch */
#define  YDPI		144		/* vertical dots/inch */
#else
#define  DEVICE_ID	"MPI Sprinter 72h x 72v dot matrix printer"
					/* this string is printed at runtime */
#define V6SIZE		XSIZE
#define  XDPI		72		/* horizontal dots/inch */
#define  YDPI		72		/* vertical dots/inch */
#endif

#define OUTFILE_EXT	"mpi"

#define  BYTE_SIZE        7		/* output file byte size */

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
#define  YPSIZE		16		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

/* The printer bit map (must have an even number of columns). */

#define XBIT ((1+2*XWORDS)/2)
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

#if    HIRES
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

    c = '\000';				/* clear character template */
    if (*Q(0) & mask)
	c |= '\040';
    if (*Q(2) & mask)
	c |= '\020';
    if (*Q(4) & mask)
	c |= '\010';
    if (*Q(6) & mask)
	c |= '\004';
    if (*Q(8) & mask)
	c |= '\002';
    if (*Q(10) & mask)
	c |= '\001';
    return('\100' | c);			/* bias into range 64 .. 127 */
}
#endif

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
    register char *c;

    for ((left = V6SIZE, c = pline + V6SIZE - 1);
	(*c == '\100') && (left > 1);
	(--left,--c))			/* trim white space but leave */
	;				/* at least 1 character in line */
    OUTC('\033');			/* <ESC> */
    OUTC('\027');			/* <CTL-W> */

    for (c = pline; left; --left, c++)
	OUTC(*c);
}

#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()

#if    HIRES
{
    register char *c;			/* pointer into v6[][] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */
    UNSIGN32 *ptmp;			/* pointer into bitmap[][] */
    register UNSIGN32 mask;		/* mask for single bit selection */
    INT16 i,j,k,ybottom,ytop;
    char v6[4][V6SIZE];			/* vertical 6-bit raster encodings */

#undef Q
#if    SEGMEM
    UNSIGN32* q[12];			/* pointers to 12 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    (void)clearerr(plotfp);

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

    /* <ESC><CTL-Z> for 72 dots/inch spacing
       <ESC><CTL-V> to disable paper perforation skip
    */
    OUTS("\033\032\033\026");

    for (j = ytop; (j >= ybottom) ; j -= 12)	/* loop over raster lines */
						/* in groups of 12 */
    {

#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v6[0][0];		/* vertical 6-bit encodings */
	for (i = 0; i < XBIT; (++p,++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE-1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 12; ++k)	/* compute pointers to 12 rasters */
	        q[k] = BITMAP(j-k,i);
#endif

	    for (k = 0; k < HOST_WORD_SIZE; (++c,k+=2))
	    {				/* loop over word bits 2 at a time */
#if    SEGMEM
		*c	      = makechar(&q[0],mask);
		*(c+  V6SIZE) = makechar(&q[0],mask >> 1);
		*(c+2*V6SIZE) = makechar(&q[1],mask);
		*(c+3*V6SIZE) = makechar(&q[1],mask >> 1);
#else /* NOT SEGMEM */
		*c	      = makechar(&p,mask);
		*(c+  V6SIZE) = makechar(&p,mask >> 1);
		ptmp = p - XBIT;
		*(c+2*V6SIZE) = makechar(&ptmp,mask);
		*(c+3*V6SIZE) = makechar(&ptmp,mask >> 1);
#endif /* SEGMEM */

		mask >>= 2;		/* move mask bit right 2 positions */
	    }
	};

	outline(&v6[0][0]);
	OUTS("0\033P1.");		/* carriage return and move */
					/* right 1/2 dot.  */
	outline(&v6[1][0]);
	OUTS("0\033Q1.");		/* carriage return and move */
					/* down 1/2 dot.  */
	outline(&v6[2][0]);
	OUTS("0\033P1.");		/* carriage return and move */
					/* right 1/2 dot.  */
	outline(&v6[3][0]);
	OUTS("6");			/* new line down 6/72 inch */
    }
    OUTC('\f');				/* FF to eject page */
    /* The MPI has no reset command, so put parameters back to something
       reasonable.
       <ESC><CTL-T> for 6 lpi spacing
       <ESC><CTL-U> to enable paper perforation skip
    */
    OUTS("\033\024\033\025");

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}
#else /* NOT HIRES */
{
    register char *c;			/* pointer into v6[] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */
    register UNSIGN32 mask;		/* mask for single bit selection */
    INT16 i,j,k,ybottom,ytop;
    char v6[V6SIZE];			/* vertical 6-bit raster encodings */

#undef Q
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
    ybottom = MAX(0,6*((j-1+5)/6));	/* need 6n raster lines */

    /* <ESC><CTL-Z> for 72 dots/inch spacing
       <ESC><CTL-V> to disable paper perforation skip
    */
    OUTS("\033\032\033\026");

    for (j = ytop; (j >= ybottom) ; j -= 6)    /* loop over raster lines */
					       /* in groups of 6 */
    {

#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif

	c = &v6[0];		/* vertical 6-bit encodings */
	*c++ = '\0';		/* first slot is NUL for bit adjacency test */
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
 		    *c |= '\040';
		if (*Q(1) & mask)
		    *c |= '\020';
		if (*Q(2) & mask)
		    *c |= '\010';
		if (*Q(3) & mask)
		    *c |= '\004';
		if (*Q(4) & mask)
		    *c |= '\002';
		if (*Q(5) & mask)
		    *c |= '\001';
		mask >>= 1;		/* move masking bit right 1 position */
		*c |= '\100';		/* bias 6-bit value by 64 */
	    }
	};
	outline(&v6[0]);
	OUTS("6");		/* new line 6/72 inch down */
    }
    OUTC('\f');			/* FF to eject page */
    /* The MPI has no reset command, so put parameters back to something
       reasonable.
       <ESC><CTL-T> for 6 lpi spacing
       <ESC><CTL-U> to enable paper perforation skip
    */
    OUTS("\033\024\033\025");

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}
#endif /* HIRES */

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

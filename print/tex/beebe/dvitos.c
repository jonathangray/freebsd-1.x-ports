/* -*-C-*- dvitos.c */
/*-->dvitos*/
/**********************************************************************/
/******************************* dvitos *******************************/
/**********************************************************************/
/* <BEEBE.TEX.DVI>DVITOS.C.37,  5-Jun-85 00:34:50, Edit by BEEBE */
/* (1) output FF at end-of-page before printer reset, and then output */
/* tab stop settings (because the printer has no default ones!) */
/* (2) make run-length encoding a run-time option '-r' */
/* (3) Preloading of fonts always enabled when selected page lists specified */
/* (4) Add code to remove adjacent bits on graphics lines */
/* <BEEBE.TEX.DVI>DVITOS.C.30,  4-Jun-85 17:17:21, Edit by BEEBE */
/* Adapted from DVIJET */

/***********************************************************************

                Performance remarks on the Toshiba P1351
                              [04-Jun-85]

Statistics were collected on  tests run from a  dedicated IBM PC with  a
9600-baud serial interface to the  printer, using a stand-alone  printer
driver program supporting XON/XOFF flow control.

------------------------------------------------------------------------
Test File		File Size	Time		Char/sec
			 (bytes)	(sec)		(limit is 960)
------------------------------------------------------------------------
TeX page (-m1000)	122983		485		253
run-length coded

TeX page (-m1000)	187362		212		883
full bitmap

TeX page (-m579)	 50476		152		327
run-length coded

TeX page (-m579)	 64933		 78		832
full bitmap

80 50-char lines of	  4174		 35		119
X's

C program		  3949		100		 39
(12 cpi draft mode)

C program		  3949		117		 33
(12 cpi Elite mode)

------------------------------------------------------------------------

The Toshiba P-1351 manual claims  a 12 cpi draft  mode speed of 192  cps
and a  12  cpi  Elite  speed  of 93  cps.   The  9600-baud  serial  port
limitation of 960 cps is far above these, but the figures above indicate
that the performance  on actual  manuscripts is  highly overrated.   The
printer driver program was  designed to echo a  character to the  screen
every time an XOFF was received from the printer, and from several dozen
to several hundred (for the run-length coded data) were received.   This
indicates that (1) the input buffer is probably a bottleneck, except for
full bitmap graphics,  and (2) horizontal  spacing and carriage  returns
(the C  programs)  drastically  reduce  the  throughput.   It  would  be
interesting to see how the printer performs with a parallel interface.

The TeX output quality  was marginal with full  bitmap graphics, due  to
bad alignment of  successive 24-bit-column  strips of  the bitmap.   The
output with run-length encoding was  terrible due to bad positioning  of
horizontal skips.   The  correctness  of  the  run-length  encoding  was
verified by an independent program which unpacks the output bitmap  into
another file which  compared identical  to the full  bitmap output.   In
addition, the bitmap itself was  verified by calling PRXBitMap()  inside
prtbmap() to format it for the Printronix dot matrix printer.

***********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef TOSHIBAP1351
#define  TOSHIBAP1351       1		/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Toshiba P-1351 180h x 180v dpi dot matrix printer"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"tos"

#define  BYTE_SIZE        7		/* output file byte size */

#undef STDRES
#define STDRES  0			/* for low-resolution devices */

#define  XDPI		180		/* horizontal dots/inch */
#define  XPSIZE		11		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		180		/* vertical dots/inch */
#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

#define  V24SIZE	(4*XWORDS*HOST_WORD_SIZE)
					/* 4 chars encode 24 bits in each  */
					/* bit position of each word */

/* The printer bit map (must have an even number of columns). */

#define XBIT ((1+2*XWORDS)/2)
#define YBIT YSIZE

#if    (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD)
#undef SEGMEM
#define SEGMEM 1 /* ( ((long)XBIT * (long)YBIT) > 65536L ) */
#endif

#include "bitmap.h"

#undef MAXOPEN				/* default too big with large bitmap */
#define  MAXOPEN	 3		/* reduce number of open PXL files */


#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"

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

#include "chargf.h"
#include "charpk.h"
#include "charpxl.h"
#include "clrbmap.h"
#include "clrrow.h"
#include "dbgopen.h"
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
    register INT16 len;
    register char *a,*b,*c;
    INT16 k,rem;

    for ((len = V24SIZE, c = pline + V24SIZE - 1);
	(*c == '\100') && (len > 1);
	(--len,--c))			/* trim white space to 1 or more */
	;
    len = (len + 3) & ~03;		/* but round up to multiple of 4 */
    pline[len] = '\0';			/* store string terminator */

    /*******************************************************************
    The Toshiba P-1351 has the requirement in raster graphics mode  that
    no two horizontally adjacent dots can be on.  We must therefore scan
    the character quadruples in the line and turn off matching bits.
    *******************************************************************/
    for ((a = pline, k = 4); k < len; k += 4)
    {
        *(a+4) = (*(a+4) & ~(*a)) | '\100';
	++a;
        *(a+4) = (*(a+4) & ~(*a)) | '\100';
	++a;
        *(a+4) = (*(a+4) & ~(*a)) | '\100';
	++a;
        *(a+4) = (*(a+4) & ~(*a)) | '\100';
	++a;
    }

    /*******************************************************************
    We search for runs as follows.  "b" marks the beginning of unwritten
    data, and "a" anchors the beginning of a run which continues to just
    before "c".   The  run length  is  "c"-"a".  Since  the  run  length
    encoding carries a penalty of additional output equal to the  length
    of the  run length  prefix (<ESC>Hnnn)  plus a  new graphics  prefix
    (<ESC>;nnnn), short runs are not so encoded.

    If a long  enough run  is found,  then the  string "b"  .. "a"-1  is
    output since its length is now  known, followed by the run  encoding
    for the character at "a".  Then "a" and "b" are advanced to "c", and
    the scan continues.

    If the  run  beginning at  "a"  is too  short,  then "a"  is  simply
    advanced.  The scan terminates at the NUL end-of-string marker.

    ?????????????xxxxxxxxxxxxxxxx????????????????????'\0'
        ^	     ^		     ^			  ^
        |	     |		     |			  |
        b	     a		     c			 EOS

    	     <-----run------>

    The Toshiba P-1351 makes no provision for runs of black space,  only
    horizontal relative moves (in units if 1/180 inch) over white space,
    so we search only for quadruples (24-bit columns in 6 low-order bits
    of 4 characters) of the latter, which appear as strings "@@@@".
    *******************************************************************/

    if (runlengthcode)
    {
	if (len <= 12)			/* too short to run-length scan */
	    OUTF2("\033;%04d%s",len >> 2,pline); /* quadruple count ESC;nnnn */
	else
	{
	    for (a = b = c = pline; *a; )
	    {
		if (*a == '\100')		/* allowable run character */
		    for (c = a; (*a == *++c); )
			;			/* advance over run */
		if (((INT16)(c-a)) > 11)	/* long enough run */
		{
		    if (rem = ((INT16)(a - b)) & 03)
			    a += 4 - rem;	/* must keep (a-b) = 4n */
		    c -= ((INT16)(c - a)) % 12; /* and (c-a) = 12n */
		}
		if (((INT16)(c-a)) > 11)	/* output long run */
		{
		    if (a > b)			/* output previous string */
		    {
			OUTF("\033;%04d",((INT16)(a - b)) >> 2);
			for ( ; b < a; b++)
			    OUTC(*b);
		    }
		    len = (INT16)(c-a) / 4;

		    /* NB: manual says in one place the count is of
		    1/120 inch, and in another, 1/180 inch.  Experiment
		    shows it to be 1/180 inch. */

		    OUTC('\033');
		    OUTC('H');
		    OUTC(0100|(len >> 8));
		    OUTC(0100|((len >> 4) & 017));
		    OUTC(0100|(len & 017));
		    a = b = c;
		}
		else				/* ignore short run */
		    ++a;
	    }

	    if (rem = ((INT16)(a - b)) % 4)	/* should never be necessary */
		a += 4 - rem;			/* but must keep (a-b) = 4n */

	    if (a > b)				/* output remaining string */
	    {
		OUTF("\033;%04d",((INT16)(a - b)) >> 2);
		for ( ; b < a; b++)
		     OUTC(*b);
	    }
	}
    }
    else				/* no runlength coding */
    {
	OUTF("\033;%04d",len >> 2);	/* quadruple count ESC;nnnn */
	OUTS(pline);
    }
    OUTC('\n');			/* end-of-graphics line */
}

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()
{
    register char *c;			/* pointer into v24[] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */
    register UNSIGN32 mask;		/* mask for single bit selection */
    register INT16 k;			/* innermost loop index */
    INT16 i,j,ybottom,ytop;
    char v24[V24SIZE+1];		/* vertical 24-bit raster encodings */
					/* with space for string terminator */

#if    SEGMEM
    UNSIGN32* q[24];			/* pointers to 24 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */


    (void)clearerr(plotfp);

    v24[V24SIZE] = '\0';		/* NUL for EOS marker */

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
    ybottom = MAX(0,24*((j-1)/24));	/* need 24n raster lines */

    OUTS("\033\032I\033G\033L07");
	/* ESC CTL-Z I to reinitialize printer */
	/* ESC L 0 7 to select correct interline spacing */
	/* ESC G for 180 dots/inch spacing and graphics mode */

    for (j = ytop; (j >= ybottom) ; j -= 24)	/* loop over raster lines */
						/* in groups of 24 */
    {
#if    SEGMEM
#else
	p = BITMAP(j,0);	/* the j-th raster line */
#endif
	c = &v24[0];		/* vertical 24-bit encodings */
	for (i = 0; i < XBIT; ++i)	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE-1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 24; ++k)	/* compute pointers to 24 rasters */
	        q[k] = BITMAP(j-k,i);
#endif

	    for (k = 0; k < HOST_WORD_SIZE; (++c,++k))/* loop over word bits */
	    {
		/* examine bits in 24 adjacent rows and build 4 6-bit */
		/* character values */

		*c = '\100';		/* initial character template */
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


		*(++c) = '\100';	/* initial character template */
		if (*Q( 6) & mask)
 		    *c |= '\040';
		if (*Q( 7) & mask)
		    *c |= '\020';
		if (*Q( 8) & mask)
		    *c |= '\010';
		if (*Q( 9) & mask)
		    *c |= '\004';
		if (*Q(10) & mask)
		    *c |= '\002';
		if (*Q(11) & mask)
		    *c |= '\001';


		*(++c) = '\100';	/* initial character template */
		if (*Q(12) & mask)
 		    *c |= '\040';
		if (*Q(13) & mask)
		    *c |= '\020';
		if (*Q(14) & mask)
		    *c |= '\010';
		if (*Q(15) & mask)
		    *c |= '\004';
		if (*Q(16) & mask)
		    *c |= '\002';
		if (*Q(17) & mask)
		    *c |= '\001';


		*(++c) = '\100';	/* initial character template */
		if (*Q(18) & mask)
 		    *c |= '\040';
		if (*Q(19) & mask)
		    *c |= '\020';
		if (*Q(20) & mask)
		    *c |= '\010';
		if (*Q(21) & mask)
		    *c |= '\004';
		if (*Q(22) & mask)
		    *c |= '\002';
		if (*Q(23) & mask)
		    *c |= '\001';

		mask >>= 1;		/* move masking bit right 1 position */
	    }				/* end loop over k */
#if    SEGMEM
#else
	    ++p;			/* point to next word on raster */
#endif
	}				/* end loop over i */
	outline(&v24[0]);
    }					/* end loop over j */
    OUTS("\033\064");	/* ESC 4 to end graphics mode */
    OUTC('\f');		/* page eject */
    OUTS("\033\032I\n");/* reset printer to initial state */
    OUTS(		/* reset tab stops -- dumb printer has no defaults */
    "\033\062\033(09,17,25,33,41,49,57,65,73,81,89,97,A5,B3,C1,C9,D7,E5,F3.");

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}

#include "outrow.h"
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

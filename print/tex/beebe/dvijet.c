/* -*-C-*- dvijet.c */
/*-->dvijet*/
/**********************************************************************/
/******************************* dvijet *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef HPLASERJET
#define  HPLASERJET       1		/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Hewlett-Packard 2686A Laser Jet laser printer"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"jet"

#define  DEFAULT_RESOLUTION 100		/* default dots/inch on HP Laser Jet */

#define  BYTE_SIZE        8		/* output file byte size */

#undef STDRES
#define STDRES  0			/* 0 for low-resolution devices */

#define  XDPI		100		/* HP Laser Jet horizontal dots/inch */
#define  XPSIZE		8		/* horizontal paper size in inches */

#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		100		/* HP Laser Jet vertical dots/inch */

#define  YPSIZE		11		/* vertical paper size in inches */
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
    OUTF("\033E\033*t%dR",(int)hpres);	/* printer reset, resolution */
}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
}

#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"
#include "dispchar.h"
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
#include "initglob.h"
#include "inch.h"
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
outline(pbit)
UNSIGN32 *pbit;				/* pointer to raster line */

/*************************************************************************
Use machine-specific coding here for efficiency.  For TOPS-20, we encode
9 bytes from every pair  of 36-bit words.

For each raster line on the paper, the Laser Jet expects a binary  8-bit
byte stream of the form

    <ESC>*bnnnWxxxxxxx ... xxxxxxx
               <--- nnn bytes --->

where each byte contains, in order from high to low bit, a left-to-right
bit pattern.  No  end-of-line marker  is required;  the escape  sequence
automatically causes a new raster line to be started.
*************************************************************************/

{
    register UNSIGN32 w_even,w_odd;
    register UNSIGN32 *p;
    register BYTE *pbuf;
    BYTE buf[1+(XSIZE+7)/8];		/* space for EOS + n 8-bit bytes */
    register INT16 i,last_word;


#if    IBM_PC_MICROSOFT
    for (last_word = XBIT - 1;
	(last_word >= 1) && (*(UNSIGN32*)normaddr(pbit,last_word) == 0);
	--last_word)
        ;				/* trim white space a word at a time */
#else
    p = pbit + XBIT - 1;		/* point to last word on line */
    for (last_word = XBIT - 1; (last_word >= 1) && (*p == 0); --last_word)
        --p;				/* trim white space a word at a time */
#endif

    p = pbit;
    pbuf = &buf[0];
    for (i = 0; i <= last_word; i += 2)	/* loop over trimmed raster */
    {
        w_even = (*p++);
        w_odd = (*p++);

#if    (HOST_WORD_SIZE == 36)
	*pbuf++ = (BYTE)( (w_even >> 28) & 0xff);
	*pbuf++ = (BYTE)( (w_even >> 20) & 0xff);
	*pbuf++ = (BYTE)( (w_even >> 12) & 0xff);
	*pbuf++ = (BYTE)( (w_even >>  4) & 0xff);
	*pbuf++ = (BYTE)( ((w_even <<  4) | (w_odd >> 32)) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >> 24) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >> 16) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >>  8) & 0xff);
	*pbuf++ = (BYTE)( (w_odd       ) & 0xff);
#else /* HOST_WORD_SIZE == 32 */
	/* encode 8 bytes at a time on 32-bit machines */
	*pbuf++ = (BYTE)( (w_even >> 24) & 0xff);
	*pbuf++ = (BYTE)( (w_even >> 16) & 0xff);
	*pbuf++ = (BYTE)( (w_even >>  8) & 0xff);
	*pbuf++ = (BYTE)( (w_even      ) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >> 24) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >> 16) & 0xff);
	*pbuf++ = (BYTE)( (w_odd  >>  8) & 0xff);
	*pbuf++ = (BYTE)( (w_odd       ) & 0xff);
#endif

    }

    *pbuf = '\0';			/* trailing EOS marker */

    last_word |= 1;			/* make last_word ODD */
    for (i = ((last_word+1)*HOST_WORD_SIZE)/8;
        (*(--pbuf) == '\0') && (i > 1); --i)
	;	/* trim trailing zero bytes, leaving at least one */
    last_word = i;

    OUTF("\033*b%dW",(int)last_word);
    pbuf = &buf[0];	/* cannot use fprintf with %s format because of
    			   NUL's in string, and it is slow anyway */
    for (i = 0; i < last_word; ++pbuf,++i)
        OUTC(*pbuf);
}

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()

{
    register UNSIGN32 *p;
    register INT16 j,k,ybottom,ytop;

    if (DBGOPT(DBG_PAGE_DUMP))
    {
	INT16 k1,k2,k3;

	for (k3 = 0; k3 < XBIT; (k3 += 7, ++p))
	{	/*  print bitmap 7 words at a pass */
	    k1 = k3;
	    k2 = MIN(XBIT,k1+7);
	    (void)printf("prtbmap()...bitmap words %d..%d",k1,k2-1);
	    NEWLINE(stdout);
	    (void)printf("     ");
	    for (k = k1; k < k2; ++k)
	        (void)printf("%10d",k*HOST_WORD_SIZE);
	    NEWLINE(stdout);
	    for (j = YBIT-1; j >= 0; --j)
	    {
	        p = BITMAP(j,0);
		for (k = 0; k < XBIT; (++k,++p))
		{
	            if (*p)	/* print non-blank raster line */
		    {
		        p = BITMAP(j,k1);
			(void)printf("%5d:",j);
			for (k = k1; k < k2; (++k,++p))
			    (void)printf(" %09lx",*p);
			NEWLINE(stdout);
			break;	/* exit loop over k */
		    }
		}
	    }
	}
    }

    (void)clearerr(plotfp);

#if    ZAPTHISOUT
    k = -1;	    /* find top non-zero raster */
    for (j = YBIT-1; (j > 0) && (k < 0); --j)  /* loop over raster lines */
    {
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
    }
    ytop = j;
#else
    ytop = YBIT-1;
#endif

    k = -1;	    /* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j) /* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1;((k >= 0) && (*BITMAP(j,k) == 0));--k)
	    ;		/* trim white space */
#else
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
#endif

    }
    ybottom = MAX(0,j-1);

#if    ZAPTHISOUT
    for (j = ytop; (j >= ybottom); --j)
        {
	OUTF("%5d:",(int)j);
        for (k = 0; k < XBIT; ++k)
            OUTF(" %9x",*BITMAP(j,k));
	NEWLINE(plotfp);
        }
#endif

    OUTF("\033&l%dX",(int)copies);	/* number of copies */

    OUTS("\033*r1A");			/* start plot at current position */

    for (j = ytop; (j >= ybottom) ; --j)	/* loop over raster lines */
	outline(BITMAP(j,0));

    OUTS("\033*rB\f");			/* end raster graphics, eject page */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("Output error -- disk storage probably full");
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

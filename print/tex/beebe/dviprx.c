/* -*-C-*- dviprx.c */
/*-->dviprx*/
/**********************************************************************/
/******************************* dviprx *******************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef PRINTRONIX
#define  PRINTRONIX       1		/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Printronix 300/600 60h x 72v dpi dot matrix printer"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"prx"

#define  BYTE_SIZE        7		/* output file byte size */

#undef STDRES
#define STDRES  0			/* 0 for low-resolution devices */

#define  XDPI		60		/* horizontal dots/inch */
#define  XPSIZE		13		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		72		/* vertical dots/inch */
#define  YPSIZE		22		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

/* The printer bit map */

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
#include "movedown.h"
#include "moveover.h"
#include "moveto.h"
#include "nosignex.h"
#include "openfont.h"
#include "option.h"
#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()

{
    register UNSIGN32 *p;
    register INT16 i,j,k,ybottom,ytop;

#if    (HOST_WORD_SIZE == 32)
    UNSIGN32 word;
#endif

    /* The Printronix encodes 6 horizontal bits in a single ASCII */
    /* character biased by 64 to make it printable, but requires that */
    /* the bits be reversed.  The following table is indexed by a */
    /* 6-bit pattern (0..63) and was produced automatically by an */
    /* EMACS TECO macro */
    /*  raster	   code as */
    /*  123456 -->  654321 */
    static char remap[] =
	{   /* 1000000 --> 1000000 */
	64,
	/* 1000001 --> 1100000 */  96,
	/* 1000010 --> 1010000 */  80,
	/* 1000011 --> 1110000 */  112,
	/* 1000100 --> 1001000 */  72,
	/* 1000101 --> 1101000 */  104,
	/* 1000110 --> 1011000 */  88,
	/* 1000111 --> 1111000 */  120,
	/* 1001000 --> 1000100 */  68,
	/* 1001001 --> 1100100 */  100,
	/* 1001010 --> 1010100 */  84,
	/* 1001011 --> 1110100 */  116,
	/* 1001100 --> 1001100 */  76,
	/* 1001101 --> 1101100 */  108,
	/* 1001110 --> 1011100 */  92,
	/* 1001111 --> 1111100 */  124,
	/* 1010000 --> 1000010 */  66,
	/* 1010001 --> 1100010 */  98,
	/* 1010010 --> 1010010 */  82,
	/* 1010011 --> 1110010 */  114,
	/* 1010100 --> 1001010 */  74,
	/* 1010101 --> 1101010 */  106,
	/* 1010110 --> 1011010 */  90,
	/* 1010111 --> 1111010 */  122,
	/* 1011000 --> 1000110 */  70,
	/* 1011001 --> 1100110 */  102,
	/* 1011010 --> 1010110 */  86,
	/* 1011011 --> 1110110 */  118,
	/* 1011100 --> 1001110 */  78,
	/* 1011101 --> 1101110 */  110,
	/* 1011110 --> 1011110 */  94,
	/* 1011111 --> 1111110 */  126,
	/* 1100000 --> 1000001 */  65,
	/* 1100001 --> 1100001 */  97,
	/* 1100010 --> 1010001 */  81,
	/* 1100011 --> 1110001 */  113,
	/* 1100100 --> 1001001 */  73,
	/* 1100101 --> 1101001 */  105,
	/* 1100110 --> 1011001 */  89,
	/* 1100111 --> 1111001 */  121,
	/* 1101000 --> 1000101 */  69,
	/* 1101001 --> 1100101 */  101,
	/* 1101010 --> 1010101 */  85,
	/* 1101011 --> 1110101 */  117,
	/* 1101100 --> 1001101 */  77,
	/* 1101101 --> 1101101 */  109,
	/* 1101110 --> 1011101 */  93,
	/* 1101111 --> 1111101 */  125,
	/* 1110000 --> 1000011 */  67,
	/* 1110001 --> 1100011 */  99,
	/* 1110010 --> 1010011 */  83,
	/* 1110011 --> 1110011 */  115,
	/* 1110100 --> 1001011 */  75,
	/* 1110101 --> 1101011 */  107,
	/* 1110110 --> 1011011 */  91,
	/* 1110111 --> 1111011 */  123,
	/* 1111000 --> 1000111 */  71,
	/* 1111001 --> 1100111 */  103,
	/* 1111010 --> 1010111 */  87,
	/* 1111011 --> 1110111 */  119,
	/* 1111100 --> 1001111 */  79,
	/* 1111101 --> 1101111 */  111,
	/* 1111110 --> 1011111 */  95,
	/* 1111111 --> 1111111 */  127
    };

    (void)clearerr(plotfp);

    ytop = YBIT-1;

    k = -1;	    /* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j) /* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j,k) == 0)); --k)
	    ;		/* trim white space */
#else
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
#endif

    }
    ybottom = MAX(0,j-1);

    for (j = ytop; (j >= ybottom) ; --j)    /* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j,k) == 0)); --k)
	    ;		/* trim white space */
#else
	p = BITMAP(j,XBIT-1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
#endif
	OUTC('\005');		/* Printronix graphics prefix */

	p = BITMAP(j,0);	/* the j-th raster line */

#if    (HOST_WORD_SIZE == 36)
        if (DBGOPT(DBG_PAGE_DUMP))
	{
	    (void)fprintf(stderr,"%6d:\t",j);
	    for (i = 0; i <= k; ++i)
		(void)fprintf(stderr,"%09x",*p++);
	    NEWLINE(stderr);
	    p = BITMAP(j,0);	/* the j-th raster line */
	}
	for (i = 0; i <= k; ++i)/* loop over trimmed raster */
	{ /* use machine-specific coding here for efficiency */
	    OUTC(remap[((*p) >> 30) & 077]);
	    OUTC(remap[((*p) >> 24) & 077]);
	    OUTC(remap[((*p) >> 18) & 077]);
	    OUTC(remap[((*p) >> 12) & 077]);
	    OUTC(remap[((*p) >>  6) & 077]);
	    OUTC(remap[((*p)      ) & 077]);
	    ++p;
	}
#else /* HOST_WORD_SIZE == 32 */
        if (DBGOPT(DBG_PAGE_DUMP))
	{
	    (void)fprintf(stderr,"%6d:\t",j);
	    for (i = 0; i <= k; ++i)
		(void)fprintf(stderr,"%08x",*p++);
	    NEWLINE(stderr);
	    p = BITMAP(j,0);	/* the j-th raster line */
	}
        for (i = 0; i <= k; i += 3)/* loop over trimmed raster */
	{ /* use machine-specific coding here for efficiency */
	    OUTC(remap[((*p) >> 26) & 077]);
	    OUTC(remap[((*p) >> 20) & 077]);
	    OUTC(remap[((*p) >> 14) & 077]);
	    OUTC(remap[((*p) >>  8) & 077]);
	    OUTC(remap[((*p) >>  2) & 077]);
	    word = (*p) << 4;
	    ++p;
	    word |= (*p) >> 28;
	    OUTC(remap[word & 077]);
	    OUTC(remap[((*p) >> 22) & 077]);
	    OUTC(remap[((*p) >> 16) & 077]);
	    OUTC(remap[((*p) >> 10) & 077]);
	    OUTC(remap[((*p) >>  4) & 077]);
	    word = (*p) << 2;
	    ++p;
	    word |= (*p) >> 30;
	    OUTC(remap[word & 077]);
	    OUTC(remap[((*p) >> 24) & 077]);
	    OUTC(remap[((*p) >> 18) & 077]);
	    OUTC(remap[((*p) >> 12) & 077]);
	    OUTC(remap[((*p) >>  6) & 077]);
	    OUTC(remap[((*p)      ) & 077]);
	    ++p;
	}
#endif
	NEWLINE(plotfp);
    }

    if (DBGOPT(DBG_PAGE_DUMP))
        putc('\f',stderr);

    OUTC('\f');			/* FF to eject page */

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

/* -*-C-*- tosprx.c */
/*-->tosprx*/
/**********************************************************************/
/******************************* tosprx *******************************/
/**********************************************************************/

#include "machdefs.h"
#include "typedefs.h"

/* Toshiba P1351 to Printronix 300/600 plot file translator */

#ifndef ANSI_PROTOTYPES
#define ANSI_PROTOTYPES 0
#endif

#ifndef OS_TOPS20
#define OS_TOPS20 0
#endif

#define NL  '\n'
#define FF  '\f'
#define ESC '\033'
#if    OS_TOPS20
#undef FASTZERO
#define FASTZERO 1
#endif

#define  XDPI		60		/* horizontal dots/inch */
#define  XPSIZE		13		/* horizontal paper size in inches */
#define  XSIZE		(XDPI*XPSIZE)	/* number of horizontal dots */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		72		/* vertical dots/inch */
#define  YPSIZE		22		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

#define  XBIT XWORDS
#define  YBIT YSIZE

int nplot,xrast,yrast;
float topmargin = 0.0;
float leftmargin = 0.0;

/* The printer bit map */

UNSIGN32 bitmap[YBIT] [XBIT];

#include <stdio.h>
#include <time.h>
#include "gendefs.h"
#include "gblprocs.h"
#define BITMAP(y,x) (bitmap + ((UNSIGN32)XBIT*(UNSIGN32)(y)) + (UNSIGN32)(x))

#if    ANSI_PROTOTYPES
int getdigit(void);
void plotline(void);
void relhormv(void);
#else /* NOT ANSI_PROTOTYPES */
int getdigit();
void plotline();
void relhormv();
#endif /* ANSI_PROTOTYPES */

int
main()
{
    register int c;

    nplot = 0;				/* plot page number */
    xrast = 0;
    yrast = YBIT-1;			/* current raster line */
    clrbmap();
    while ((c = getchar()) != EOF)
    {
        switch (c)
	{
	case ESC:
	    if ((c = getchar()) == ';')
	        plotline();
	    else if (c == 'H')
	    {
	        ungetc(c,stdin);
		relhormv();
	    }
	    break;

	case NL:
	    xrast = 0;
	    break;

	case FF:
	    nplot++;
	    prtbmap();
	    (void)fprintf(stderr," [%d",nplot);
	    clrbmap();
	    (void)fprintf(stderr,"]");
	    break;

	default:			/* discard other characters */
	    break;
	}
    }
    prtbmap();
}

int
getdigit()
{
    register int c;

    c = getchar();
    if ((c < '0') | ('9' < c))
    {
        (void)fprintf(stderr,
	    "?Expected digit, but found char %03o\n on page %d in line %d\n",
	    c,nplot,YBIT-yrast);
	exit(1);
    }
    else
        return (c - '0');
}

void
plotline()
{
    register int c,mask,nword,y;
    int nchar,count,org_count;

    if (yrast < 24)			/* bit map is full, print it */
    {
        prtbmap();
	clrbmap();
	yrast = YBIT-1;
    }
    count = getdigit();
    count = 10*count + getdigit();
    count = 10*count + getdigit();
    count = 10*count + getdigit();	/* count = number of columns */
    count *= 4;				/* count = number of char quadruples */
    org_count = count;

    nchar = 0;				/* cyclic counter 0 1 2 3  */
    while (((c = getchar()) != EOF) && (c != NL) && count)
    {
        count--;
        if (c < '\100')
	{
	    (void)fprintf(stderr,
	        "?Illegal character %03o on page %d line %d -- flushed line\n",
	        c,nplot,YBIT-yrast);
	    while (((c = getchar()) != EOF) && (c != NL))
	        count--;
	}
	else if (xrast < XSIZE)		/*still inside bitmap */
	{
	    if (c > '\100')		/* have dots to set */
	    {
		nword = xrast/HOST_WORD_SIZE;
		mask = 1;
		mask <<= (HOST_WORD_SIZE-1);	/* bit at left end of word */
		mask >>= (xrast % HOST_WORD_SIZE);
		y = yrast - 6*nchar;
		if (c & '\040')
	            bitmap[y  ][nword] |= mask;
		if (c & '\020')
		    bitmap[y-1][nword] |= mask;
		if (c & '\010')
		    bitmap[y-2][nword] |= mask;
		if (c & '\004')
	            bitmap[y-3][nword] |= mask;
		if (c & '\002')
	            bitmap[y-4][nword] |= mask;
		if (c & '\001')
	            bitmap[y-5][nword] |= mask;
	    }
	    nchar = (++nchar) & 03;
	    if (!nchar)
	        ++xrast;		/* new column every four characters */
	}
    }
    if (count)
        (void)fprintf(stderr,"?Input count = %d, but %d left at EOL\n",
	    org_count,count);
    if (c == NL)
    {
        xrast = 0;
        yrast -= 24;			/* one printline has 24 rasters */
    }
    else if (c == ESC)			/* expect <ESC>Hnnnc hor. motion */
        relhormv();
    else
        (void)fprintf(stderr,
	    "?Expected NL, but found %03o -- flushing line\n",c);
}

#include "clrbmap.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()

{
    register UNSIGN32 *p;
    register INT16 i,j,k,ybottom,ytop;

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

    ytop = YBIT-1;

    k = -1;	    /* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j) /* loop over raster lines */
    {
	p = &bitmap[j][XBIT-1];
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
    }
    ybottom = MAX(0,j-1);

    putchar('\f');	/* start new page with FF */
    for (j = (INT16)(topmargin*((float)YDPI)); j > 0; --j)
    {				/* supply top margin spacing */
	putchar('\005');	/* Printronix graphics prefix */
	putchar('\n');
    }
    for (j = ytop; (j >= ybottom) ; --j)    /* loop over raster lines */
    {
	p = &bitmap[j][XBIT-1];
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;		/* trim white space */
	putchar('\005');	/* Printronix graphics prefix */

	for (i = (5 + (INT16)(leftmargin*((float)XDPI)))/6;
	    (i > 0) && (k >= 0); --i)
	    putchar('\100');/* left margin spacing if line not empty */

	p = &bitmap[j][0];	/* the j-th raster line */

	for (i = 0; i <= k; ++i)/* loop over trimmed raster */
	{ /* use machine-specific coding here for efficiency */
	    putchar(remap[((*p) >> 30) & 077]);
	    putchar(remap[((*p) >> 24) & 077]);
	    putchar(remap[((*p) >> 18) & 077]);
	    putchar(remap[((*p) >> 12) & 077]);
	    putchar(remap[((*p) >>  6) & 077]);
	    putchar(remap[((*p)      ) & 077]);
	    ++p;
	}
	putchar('\n');
    }
    (void)fflush(stdout);
}

/*-->relhormv*/
/**********************************************************************/
/****************************** relhormv ******************************/
/**********************************************************************/

void
relhormv()
{
    register int c,n;

    if ((c = getchar()) != 'H')
    {
        (void)fprintf(stderr,
	    "?Expected <ESC>Hnnn Relative Horizontal Motion command \
- flushing line\n");
	while (((c = getchar()) != EOF) && (c != NL))
	    ;
	return;
    }
    c = getchar();
    if (IN('@',c,'F'))
        n = (c - '@')*256;
    else
    {
        (void)fprintf(stderr,
	    "?Illegal <ESC>Hnnn Relative Horizontal Motion command \
- flushing line\n");
	while (((c = getchar()) != EOF) && (c != NL))
	    ;
	return;
    }
    c = getchar();
    if (IN('@',c,'O'))
        n += (c - '@')*16;
    else
    {
        (void)fprintf(stderr,
	    "?Illegal <ESC>Hnnn Relative Horizontal Motion command \
- flushing line\n");
	while (((c = getchar()) != EOF) && (c != NL))
	    ;
	return;
    }
    c = getchar();
    if (IN('@',c,'O'))
        n += (c - '@');
    else
    {
        (void)fprintf(stderr,
	    "?Illegal <ESC>Hnnn Relative Horizontal Motion command \
- flushing line\n");
	while (((c = getchar()) != EOF) && (c != NL))
	    ;
	return;
    }
    xrast += n;
}

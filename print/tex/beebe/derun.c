/* -*-C-*- derun.c */
/*-->derun*/
/**********************************************************************/
/******************************* derun ********************************/
/**********************************************************************/

/***********************************************************************
Expand run-length encoded Toshiba P1351 plot file to unexpanded form for
checking of run-length coding algorithm.

Usage:
	derun <infile >outfile
***********************************************************************/

#include <stdio.h>

#define ESC '\033'
#define FF '\f'
#define CTLZ '\032'
#define MAXBUF 14*180*4
#define NL '\n'
#define TRUE 1
#define FALSE 0
int buf[MAXBUF+5],kbuf;			/* leave extra space at end of buf[] */
#if    ANSI
void OutBuf(int);
#else
void OutBuf();
#endif

void
main(argc,argv)
int argc;
char* argv[];
{
    int c,n,isgraph;


    kbuf = 0;
    isgraph = FALSE;
    while (((c = getchar()) != EOF) && (kbuf < MAXBUF))
    {
	if (c == FF)			/* <CTL-L> */
	{
	    buf[kbuf++] = c;
	    OutBuf(FALSE);
	}
        else if (c == ESC)		/* <ESC> ... */
	{
	    c = getchar();
	    if (c == 'G')		/* <ESC> G */
	    {
	        buf[kbuf++] = ESC;
	        buf[kbuf++] = c;
		OutBuf(FALSE);
	    }
	    else if (c == CTLZ)		/* <ESC> <CTL-Z> I */
	    {
	        buf[kbuf++] = ESC;
	        buf[kbuf++] = c;
		buf[kbuf++] = getchar();
		OutBuf(FALSE);
	    }
	    else if (c == 'L')		/* <ESC> L n n */
	    {
	        buf[kbuf++] = ESC;
	        buf[kbuf++] = c;
		buf[kbuf++] = getchar();
		buf[kbuf++] = getchar();
		OutBuf(FALSE);
	    }
	    else if (c == 'H')
	    {
	        isgraph = TRUE;
	        n = (getchar() - '@')*256;
		n += (getchar() - '@')*16;
		n += (getchar() - '@');
		n *= 6;
		for ( ; (n > 0) && (kbuf < MAXBUF); --n)
		    buf[kbuf++] = '@';
	    }
	    else if (c == ';')
	    {
	        isgraph = TRUE;
	        n = (getchar() - '0')*1000;
		n += (getchar() - '0')*100;
		n += (getchar() - '0')*10;
		n += (getchar() - '0');
		n *= 4;
		for ( ; (n > 0) && (kbuf < MAXBUF); --n)
		    buf[kbuf++] = getchar();
	    }
	    else
	    {
	        buf[kbuf++] = ESC;
	        buf[kbuf++] = c;
	    }
	}
	else if (c == NL)
	{
	    if ((kbuf & 03) && isgraph)
	    {
	        (void)fprintf(stderr,"%Byte count %d not multiple of 4!\n",
		    kbuf);
	        (void)printf("%Byte count %d not multiple of 4!\n",kbuf);
	    }
	    if (isgraph)
	        (void)printf("\033;%04d",kbuf>>2);
	    OutBuf(TRUE);
	    isgraph = FALSE;
	}
	else
	    buf[kbuf++] = c;
    }
    if (kbuf >= MAXBUF)
        (void)fprintf(stderr,
	    "?Buffer overflow -- buf[%d] needs %d\n",MAXBUF,kbuf);
    if (kbuf > 0)
    {
        (void)fprintf(stderr,"%Last line not terminated by NL\n");
	OutBuf(FALSE);
    }
}

void
OutBuf(putnl)
int putnl;
{
    register int n;

    for (n = 0 ; n < kbuf; ++n)
        putchar((char)buf[n]);

    if (putnl)
        putchar(NL);
    kbuf = 0;
}

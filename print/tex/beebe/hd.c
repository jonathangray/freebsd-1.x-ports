/***********************************************************************
Dump one or more files to stdout, translating unprintable characters  to
\ooo octal sequences, and  starting newlines at each  ESC, or when  line
length would exceed 72 characters.

Usage:
	hd filelist

Both 7-bit and 8-bit input files are supported, and the output  contains
the file name and its last-write date at the top of the page.

Outside of TOPS-20,  it will  be necessary to  provide (possibly  dummy)
definitions of  fileof()  and timeof(),  which  are intended  to  return
character strings containing the file name and last read or write  time,
given the file pointer.

[15-Aug-87]

***********************************************************************/

#define MAXLINE 72
#define COMMENT '!'		/* marks start of comment to end-of-line */

#include <stdio.h>

char* fileof();
char* timeof();
void  hd();

FILE* tty;


main(argc,argv)
int argc;
char* argv[];
{
    int k;

    tty = stdout;
    for (k = 1; k < argc; ++k)
        hd(argv[k]);
}

void
hd(filename)
char* filename;
{
    register int c;
    register FILE* fp;
    register int hpos;

    if ((fp = fopen(filename,"rb")) == (FILE*)NULL)
        exit(2);

    (void)fprintf(tty,"%c%s [%s]\n",
        COMMENT,fileof(fileno(fp)),timeof(fileno(fp),"w"));

    hpos = 0;
    while ((c = getc(fp)) != EOF)
    {
	if ((c < 040) || (c > 0176) || (c == '\\') || (c == COMMENT))
	{				/* use octal encoding */
	    if (c == 033)
	    {
	        putc('\n',tty);
		hpos = 0;
	    }
	    else if (hpos > (MAXLINE-4))
	    {
	        putc('\n',tty);
		putc('\t',tty);
		hpos = 8;
	    }

	    (void)fprintf(tty,"\\%03o",c);
	    hpos += 4;
	    if (c == '\n')
	    {
	        putc('\n',tty);
		hpos = 0;
	    }
	}
	else				/* printable character */
	{
	    if (hpos > (MAXLINE-1))
	    {
	        putc('\n',tty);
		putc('\t',tty);
		hpos = 8;
	    }
	    putc(c,tty);
	    hpos++;
	}
    }
    putc('\n',tty);
    putc('\f',tty);

    (void)fflush(tty);
    (void)fclose(fp);
}

/***********************************************************************
Process a file output by hd back into an 8-bit binary file.  Characters
out of the range 040..0176 are ignored.

Usage:
	unhd infile outfile

The open mode flags passed to fopen() are likely to require modification
outside of TOPS-20.

[15-Aug-87]
***********************************************************************/

#include <stdio.h>
#define COMMENT '!'		/* marks start of comment to end-of-line */

void
main(argc,argv)
int argc;
char* argv[];
{
    FILE* in;
    FILE* out;
    register int c;

    if (argc < 2)
    {
	(void)fprintf(stderr,"Usage: unhd infile outfile");
	exit(0);
    }
    if ((in = fopen(argv[1],"rb")) == (FILE*)NULL)
        exit(1);
    if ((out = fopen(argv[2],"wb8")) == (FILE*)NULL)
        exit(2);

    while ((c = getc(in)) != EOF)
    {
        if (c == COMMENT)
	{
	    while (((c = getc(in)) != EOF) && (c != '\n'))
	        /* flush comment */;
	}
	else if (c > 037)
	{
	    if (c == '\\')
	    {
		c = getc(in) - '0';
		c = (c << 3) + (getc(in) - '0');
		c = (c << 3) + (getc(in) - '0');
	    }
	    putc(c,out);
	}
    }
    exit(0);
}

#include <stdio.h>
#include "rst.h"

/*
 * write the value n, which is size bytes long, to the
 * FILE descriptor *fp.  Most signifigant byte first.
 */

out1byt(c, fp)
	ONEB c;
	FILE *fp;
{
	putc(c&0377, fp);
}

out2byt(c, fp)
	TWOB c;
	FILE *fp;
{
	putc((c>>8) & 0377, fp);
	putc(c&0377, fp);
}

out3byt(c, fp)
	THREEB c;
	FILE *fp;
{
	putc((c>>16) & 0377, fp);
	putc((c>>8) & 0377, fp);
	putc(c&0377, fp);
}

out4byt(c, fp)
	FOURB c;
	FILE *fp;
{
	putc((c>>24) & 0377, fp);
	putc((c>>16) & 0377, fp);
	putc((c>>8) & 0377, fp);
	putc(c&0377, fp);
}

outstr(s, fp)
	char *s;
	FILE *fp;
{
	while (*s)
		putc(*s++, fp);
}

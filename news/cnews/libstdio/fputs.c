#include <stdio.h>

fputs(s, fp)
register char *s;
register FILE *fp;
{
	unsigned len = strlen(s);

	if (fwrite(s, 1, len, fp) < len)
		return EOF;
	return 0;
}

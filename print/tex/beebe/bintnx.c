/***********************************************************************
Convert a TOPS-20 file transferred in FTP "binary" mode to "tenex" mode.
In "binary" mode, we have 2 36-bit words in 9 8-bit bytes.  In "tenex"
mode, we want the top 32 bits of each 36-bit group, giving 8 8-bit bytes.

Who knows what FTP did if the file had an odd number of 36-bit words.

[08-Oct-87]

***********************************************************************/

#include <stdio.h>

main()
{
    int c,d;

    for (;;)
    {
        c = getchar();
	if (c == EOF)
	    break;
        putchar(c);			/* 0..7 */
	c = getchar();	putchar(c);	/* 8..15 */
	c = getchar();	putchar(c);	/* 16..23 */
	c = getchar();	putchar(c);	/* 24..31 */

	d = getchar();

	c = (d << 4);
	d = getchar();
	c |= 0xFF & (d >> 4);
	putchar(c);			/* 4..11 */

	c = (d << 4);
	d = getchar();
	c |= 0xFF & (d >> 4);
	putchar(c);			/* 12..19 */

	c = (d << 4);
	d = getchar();
	c |= 0xFF & (d >> 4);
	putchar(c);			/* 20..27 */

	c = (d << 4);
	d = getchar();
	c |= 0xFF & (d >> 4);
	putchar(c);			/* 28..36 */

    }
}

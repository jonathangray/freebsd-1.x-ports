/* endian.c
 *
 * COPYRIGHT (c) 1990 AT&T Bell Laboratories.
 *
 * Determine the endianess of this machine.
 */

char		s[4] = {0x01, 0x02, 0x04, 0x08};

main ()
{
    int		*p = (int *)s;

    if (*p == 0x01020408)
	printf("Big\n");
    else if (*p == 0x08040201)
	printf("Little\n");
    else
	exit(1);
}

/*
 * bdecode [file]
 */
#include <stdio.h>
#include <string.h>
#include "coder.h"
char *myname, *inputfile = "(stdin)";

main(argc, argv) 
	char **argv;
{
	register long word;
	register int c, bcount;
	register FILE *fin = stdin, *fout = stdout;	/* in regs for speed */
	register char *map, *p;
	register long nbytes;
	register unsigned crc;
	long nbytes2;
	unsigned w, crc2;
	char buf[512];

	myname = argv[0];
	if (sizeof(word) < 4)
		fprintf(stderr, "%s: word size too small\n", myname), exit(1);
	if (argc > 2)
		fprintf(stderr, "Usage: %s [file]\n", myname), exit(1);
	if (argc == 2) {
		if ((fin = fopen(argv[1], "r")) == NULL) {
			fprintf(stderr, "%s: ", myname);
			perror(argv[1]);
			exit(1);
		}
		inputfile = argv[1];
	}
	/* skip to beginning of encoded data */
	do {
		if (fgets(buf, sizeof buf, fin) == NULL)
			fatal("Missing header");
		/* trim trailing blanks (sigh) */
		p = strchr(buf, '\n');
		if (p == 0)
			continue;
		while (*--p == ' ')
			;
		p[1] = '\n';
		p[2] = '\0';
	} while (strcmp(buf, header) != 0);
	
	/* define input mapping table */
	map = buf+1;
	for (c = 0; c < 256; c++)
		map[c] = 64;		/* illegal */
	for (c = 0; c < 64; c++)
		map[ENCODE(c)] = c;
	map[EOF] = 65;		/* special cases */
	map['/'] = 66;

	word = 0;
	bcount = 4;
	nbytes = 0;
	crc = 0;
#define PUTC(x)  { c = (x) & 0xff; CRC(crc, c); putc(c, fout); nbytes++; }
	for (;;) {
		c = map[getc(fin)];
		if ((unsigned)c < 64) {
			word <<= 6;
			word |= c;
			if (--bcount == 0) {
				PUTC(word >> 16);
				PUTC(word >>  8);
				PUTC(word);
				word = 0;
				bcount = 4;
			}
			continue;
		}
		switch (c) {

		default:
			/*
			 * Ignore stuff not in the code set.
			 */
			continue;

		case 65:	/* EOF */
			fatal("Unexpected EOF");

		case 66:	/* '/' */
			/* trailer follows: %d%x */
			c = getc(fin);
			if (fscanf(fin, "%x", &w) != 1)
				fatal("Corrupted input (trailer)");
			switch (c) {
			case '2': PUTC(w >> 8);
			case '1': PUTC(w);
			case '0': break;
			default: fatal("Corrupted input (trailer)");
			}
			/*
			 * Byte count and CRC follow.
			 */
			if (fscanf(fin, "%ld%x", &nbytes2, &crc2) != 2)
				fatal("Corrupted input (missing byte count/CRC)");
			if (nbytes2 != nbytes)
				fatal("Corrupted input (byte count is wrong)");
			if (crc2 != (crc & 0xffff))
				fatal("Corrupted input (CRC mismatch)");
			exit(0);
		}
	}
}

fatal(s)
	char *s;
{
	fprintf(stderr, "%s: %s: %s\n", myname, inputfile, s);
	exit(2);
}

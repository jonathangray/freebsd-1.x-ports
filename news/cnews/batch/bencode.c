/*
 * bencode [file]
 */
#include <stdio.h>
#include "coder.h"
#define MAXPERLINE 78		/* max chars/line */
char *myname;

main(argc,argv) 
	char **argv;
{
	register FILE *fin = stdin, *fout = stdout; /* faster in a register */
	register int c, bcount, ccount = MAXPERLINE-1;
	register long word, nbytes;
	register unsigned crc;

	myname = argv[0];
	if (sizeof(word) < 4)
		fprintf(stderr, "%s: word size too small\n", myname), exit(1);
	if (argc == 2 && (fin = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr, "%s: ", myname);
		perror(argv[1]);
		exit(1);
	}
	else if (argc > 2) {
		fprintf(stderr, "Usage: %s [file]\n", myname);
		exit(1);
	}

#define PUTC(c) \
	putc(c, fout); \
	if (--ccount == 0) { \
		putc('\n', fout); \
		ccount = MAXPERLINE-1; \
	}

	fputs(header, fout);
	word = 0;
	bcount = 3;
	crc = 0;
	for (nbytes = 0; (c = getc(fin)) != EOF; nbytes++) {
		CRC(crc, c);
		word <<= 8;
		word |= c;
		if (--bcount == 0) {
			PUTC(ENCODE((word >> 18) & 077));
			PUTC(ENCODE((word >> 12) & 077));
			PUTC(ENCODE((word >>  6) & 077));
			PUTC(ENCODE((word      ) & 077));
			word = 0;
			bcount = 3;
		}
	}
	/*
	 * A trailing / marks end of data.
	 * The last partial encoded word follows in hex,
	 * preceded by a byte count.
	 */
	if (ccount != MAXPERLINE-1)	/* avoid empty lines */
		putc('\n', fout);
	fprintf(fout, "/%d%x\n", 3-bcount, word);
	/*
	 * And finally the byte count and CRC.
	 */
	fprintf(fout, "%ld %x\n", nbytes, crc & 0xffff);
	exit(0);
}

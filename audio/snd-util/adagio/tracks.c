
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

main(argc, argv)
int argc;
char *argv[];
{	int k;

	f_header(argc-1);
	for (k = 1; --argc; k++) dumptrack(argv[k]);
}

FILE *fopen();

dumptrack(name)
char *name;
{	FILE *f;
	struct stat statbuf;
	int c;

	if (stat(name, &statbuf)) {
		perror(name);
		return;
    	}

	t_header(statbuf.st_size + 4);

	if ( (f = fopen(name, "r")) == NULL ) {
		fprintf(stderr, "no file %s\n", name);
		exit(1);
	}

	while ( (c = getc(f)) != EOF ) {
		putchar(c);
	}
	t_trailer();

	fclose(f);
}

WriteInt(value)
int value;
{
	putchar((value >> 24)&0xff);
	putchar((value >> 16)&0xff);
	putchar((value >> 8)&0xff);
	putchar((value)&0xff);
}

WriteShort(value)
int value;
{
	putchar((value >> 8)&0xff);
	putchar((value)&0xff);
}

f_header(numtracks)
int numtracks;
{
	printf("MThd");
	WriteInt(6);
	WriteShort((numtracks > 1)? 1 : 0);
	WriteShort(numtracks);
	WriteShort(96);
	/*putchar(-25);*/	/* 25 frames per sec */
	/*putchar(4);*/	/* 4 ticks per frame */
}

t_header(tracksize)
int tracksize;
{
	printf("MTrk");
	WriteInt(tracksize);
}

t_trailer()
{
	putchar(0);
	putchar(0xff);
	putchar(0x2f);
	putchar(0);
}

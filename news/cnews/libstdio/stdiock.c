/*
 * If this compiles and runs successfully, your stdio is very probably
 * compatible with stdio.fast.
 * Buffers by default; -u stops buffering.
 * Writes on stdout by default; -i reads from stdin.
 */
#include <stdio.h>

FILE *fp = stdout;
int status = 0;

main(argc, argv)
char **argv;
{
	int wantbuf = 1, usestdin = 0, nosetbuf = 0;
	char line[1024];
	static char buf[BUFSIZ];
	static char buggered[] =
"Your stdio appears to be buggered: fwrite of 4 bytes doesn't yield 4.\n";

	for (; argc > 1; argv++, argc--) {
		if (strcmp(argv[1], "-u") == 0)
			wantbuf = 0;
		if (strcmp(argv[1], "-i") == 0) {
			usestdin = 1;
			fp = stdin;
		}
		if (strcmp(argv[1], "-f") == 0)
			fp = fopen("/tmp/stdiock", "w");
		if (strcmp(argv[1], "-n") == 0)
			nosetbuf = 1;
	}

	evalfile("start");

	if (nosetbuf)
		;
	else if (wantbuf) {
		setbuf(fp, buf);
		evalfile("setbuf(buf)");
	} else {
		setbuf(fp, (char *)NULL);
		evalfile("setbuf(0)");
	}

	if (usestdin) {
		(void) fgets(line, sizeof line, fp);
		evalfile("fgets");
	} else {
		if (fwrite("your", 1, 4, fp) != 4) {
			status = 1;
			(void) write(1, buggered, strlen(buggered));
		}
		evalfile("fwrite");

		(void) fputs(" stdio seems to be ", stdout);
		evalfile("fputs");
	}

	if (wantbuf && fp->_ptr == NULL) {
		status = 1;
		(void) fputs("incompatible (_ptr) ", stdout);
	} else
		(void) fputs("compatible (_ptr) ", stdout);
	if (fp->_cnt >= 0 && (nosetbuf || fp->_cnt <= BUFSIZ))
		(void) fputs("compatible (_cnt)", stdout);
	else {
		status = 1;
		(void) fputs("incompatible (_cnt)", stdout);
	}
	evalfile("test");

	(void) fputs(" with stdio.fast\n", stdout);
	evalfile("fputs");

	(void) fflush(stdout);
	(void) putc('\n', stdout);
	evalfile("putc 1");
	(void) putc(' ', stdout);
	evalfile("putc 2");
	(void) putc('\n', stdout);
	evalfile("putc 3");

	(void) fflush(fp);
	(void) fflush(stdout);
	evalfile("fflush");

	exit(status);

	/* next line is not executed, but drags in the appropriate fread */
	(void) fread(buf, 1, sizeof buf, stdin);
}

/* write on stdout without using stdio. ugh */
evalfile(s)
char *s;
{
	static char ptr[] = "ptr ";
	static char cnt[] = " cnt ";
	static char *bufend = NULL;
	static char buggered[] =
"Your stdio appears to be buggered: _ptr+_cnt does not yield a constant.\n";
	static char negcnt[] = "_cnt can go negative.\n";

	if (fp->_ptr != NULL && fp->_cnt != 0) {
		if (fp->_cnt < 0) {
			status = 1;
			(void) write(1, negcnt, strlen(negcnt));
		}
		if (bufend == NULL)
			bufend = (char *)fp->_ptr + fp->_cnt;
		if (bufend != (char *)fp->_ptr + fp->_cnt) {
			status = 1;
			(void) write(1, buggered, strlen(buggered));
		}
	}

	(void) write(1, s, strlen(s));
	(void) write(1, "\t", 1);
	(void) write(1, ptr, strlen(ptr));
	prn((int)fp->_ptr, 10);
	(void) write(1, cnt, strlen(cnt));
	prn((int)fp->_cnt, 10);
	(void) write(1, "\n", 1);
}

prn(i, rad)
int i, rad;
{
	int quot, rem;
	char c;

	if (i < 0) {
		(void) write(1, "-", 1);
		i = -i;
	}
	quot = i / rad;
	rem = i % rad;
	if (quot > 0)
		prn(quot, rad);
	c = rem + '0';
	(void) write(1, &c, 1);
}

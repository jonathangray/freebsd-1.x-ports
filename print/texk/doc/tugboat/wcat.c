#include <stdio.h>
#include <signal.h>
#include <termios.h>

#define STKSIZ BUFSIZ

void onintr(), setty(), resetty();

int echo = 1;		/* echo flag */
int ts = 8;		/* tab size */
int flin = 0;		/* floating indentation */

/* wcat: line buffered cat with wrap-around */
main(argc, argv)
int argc;
char **argv;
{
    extern char *optarg;
    extern int optind;

    char *getline();

    int nl = 0;
    int nflag = 1;	    /* nl > 1 forces break */
    int c;
    int linelen = BUFSIZ;
    char buf[BUFSIZ];

    while ((c = getopt(argc, argv, "i:nqw:")) != EOF)
	switch (c) {
	case 'i':
	    flin = atoi(optarg);
	    break;
	case 'q':
	    echo = 0;
	    break;
	case 'w':
	    linelen = atoi(optarg);
	    break;
	case 'n':
	    nflag = 0;
	    break;
	default:
	    fprintf(stderr, "Usage: %s [-nq] [-w len]\n", argv[0]);
	    exit(1);
	}


    signal(SIGINT, SIG_IGN);

    /* disable line discipline */
    setty();

    for (; getline(buf, BUFSIZ, linelen) != 0; puts(buf))

	if (nflag && (nl =  (buf[0] == '\0') ? nl + 1 : 0) > 1 
	 || buf[0] == '.' && buf[1] == '\0') 
	    break;
	
    resetty();
}

int kill_ch = 0x15; 	/* ^U */
int erase_ch = 0x08;	/* ^H */

#define perr(x)	putc(x, stderr)
#define backsp() perr('\b'), perr(' '), perr('\b')

char *
getline(buf, bufsiz, linesiz)
char *buf;
int bufsiz;
int linesiz;
{
    int n, c;
    int ct = 0;
    int nsp = 0;

    while ((c = getkey()) != '\n')  {
	if (c == EOF)
	    break;
	else if (c == kill_ch) {	    /* erase line */
	    while (ct) {
		ct--;
		if (echo) backsp();
	    }
	} else if (c == erase_ch && ct) {    /* erase char */
	    ct--;
	    if (echo) backsp();
	} else if (c == '\f' || c >= 32 && c <= 127) {    /* ok char */
	    buf[ct++] = c;
	    if (echo) perr(c);
	} else if (c == '\t') {		    /* expand tab to next tab stop */
	    for (n = ts - ct%ts; --n;)
		pushkey(' ');
	    buf[ct++] = ' ';
	    if (echo) perr(' ');
	} else {				/* ignore char */
	    ;
	}

	if (ct >= bufsiz-1)
	    break;

	nsp += isspace(c);
	if (ct >= linesiz && nsp) {
	    while (!isspace(buf[ct-1])) {    /* push word */
		pushkey(buf[--ct]);
		if (echo) backsp();
	    }
	    for (n = 0; n < flin; n++)
		pushkey(' ');		    /* indent */
	    break;
	}
    }
    while (ct > 0 && isspace(buf[ct-1]))    /* strip trailing white space */
	ct--;
    buf[ct] = '\0';
    if (echo)
	perr('\n');
    return (c == EOF && ct == 0) ? (char *) 0 : buf;
}

getkey()
{
    char c;
    
    if ((c = popkey()) < 0 && read(0, &c, 1) <= 0 || c == 0x04)
	c =  EOF;

    return (int) c;
}


char cstack[STKSIZ];
int si = 0;

pushkey(c)
{
    cstack[si] = c;
    if (si < STKSIZ-1) si++;
    return c;
}


popkey()
{
    return (si > 0) ? cstack[--si] : -1;
}

void
onintr()
{
    signal(SIGINT, SIG_IGN);
    resetty();
    exit(1);
}


static struct termios ttysave;

void
setty()
{
    struct termios tbuf;

    tcgetattr(0, &tbuf);
    ttysave = tbuf;
    erase_ch = tbuf.c_cc[VERASE];
    kill_ch = tbuf.c_cc[VKILL];
    cfmakeraw(&tbuf);
/*
    tbuf.c_iflag &= ~(INLCR | IUCLC | ISTRIP | IXON | BRKINT);
    tbuf.c_iflag |= ICRNL;
    tbuf.c_oflag |= (ONLCR | OPOST);
    tbuf.c_lflag &= ~(ICANON | ISIG | ECHO);
    tbuf.c_cc[VMIN] = 5;
    tbuf.c_cc[VTIME] = 2;
    tcsetattr(0, TCSAFLUSH, &tbuf);
*/
}

void
resetty()
{
    tcsetattr(0, TCSAFLUSH, &ttysave);
}

#include <sys/types.h>
#include <sys/ioctl.h>      /*  K.Okamoto  */

#include <ctype.h>
#include <signal.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include <ctlmod.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)monitor.c	8.1	12/31/84)



/*
**  MONITOR
**
**	This routine maintains the logical query buffer in
**	/tmp/INGQxxxx.  It in general just does a copy from input
**	to query buffer, unless it gets a backslash escape character
**	or dollarsign escape character.
**	It recognizes the following escapes:
**
**	\a -- force append mode (no autoclear)
**	\b -- branch (within an include file only)
**	\c -- reserved for screen clear in geoquel
**	\d -- change working directory
**	\e -- enter editor
**	\g -- "GO": submit query to INGRES
**	\i -- include (switch input to external file)
**	\k -- mark (for \b)
**	\l -- list: print query buffer after macro evaluation
**	\p -- print query buffer (before macro evaluation)
**	\q -- quit ingres
**	\r -- force reset (clear) of query buffer
**	\s -- call shell
**	\t -- print current time
**	\v -- evaluate macros, but throw away result (for side effects)
**	\w -- write query buffer to external file
**	\$t -- change setting of trace flags
**	\$r -- reset system
**	\\ -- produce a single backslash in query buffer
**
**	Uses trace flag 2
*/

/*
**  COMMAND TABLE
**	To add synonyms for commands, add entries to this table
*/

struct cntrlwd {
	char	*name;
	int	code;
};

struct cntrlwd	Controlwords[] = {
	{ "a",		C_APPEND },
	{ "append",	C_APPEND },
	{ "b",		C_BRANCH },
	{ "branch",	C_BRANCH },
	{ "cd",		C_CHDIR },
	{ "chdir",	C_CHDIR },
	{ "e",		C_EDIT },
	{ "ed",		C_EDIT },
	{ "edit",	C_EDIT },
	{ "editor",	C_EDIT },
	{ "g",		C_GO },
	{ "go",		C_GO },
	{ "i",		C_INCLUDE },
	{ "include",	C_INCLUDE },
	{ "read",	C_INCLUDE },
	{ "k",		C_MARK },
	{ "mark",	C_MARK },
	{ "l",		C_LIST },
	{ "list",	C_LIST },
	{ "p",		C_PRINT },
	{ "print",	C_PRINT },
	{ "q",		C_QUIT },
	{ "quit",	C_QUIT },
	{ "r",		C_RESET },
	{ "reset",	C_RESET },
	{ "s",		C_SHELL },
	{ "sh",		C_SHELL },
	{ "shell",	C_SHELL },
	{ "t",		C_TIME },
	{ "time",	C_TIME },
	{ "date",	C_TIME },
	{ "v",		C_EVAL },
	{ "eval",	C_EVAL },
	{ "w",		C_WRITE },
	{ "write",	C_WRITE },
	{ "$t",		C_SYSTRACE },
	{ "$trace",	C_SYSTRACE },
	{ "$r",		C_SYSRESET },
	{ "$reset",	C_SYSRESET },
        { 0 }
};

extern jmp_buf	CmReset;
extern int	RubLevel;

void
putch(char ch)
{
	register char	c;

	c = ch;

	Prompt = Newline = (c == '\n');
	if (iscntrl(c) && (c != '\n' && c != '\t')) {
		printf("Funny character 0%o converted to blank\n", c);
		c = ' ';
	}
	prompt(0);     /* if Prompt = !GiveEof == TRUE, show prompt and 
			  then fflush(stdout).    K.Okamoto      */
	if (Autoclear)
		clear(0);   /* Qbname -->truncated Qryiop, and show 
			     prompt 'go'  Autoclear = Notnull = 0,
			     K.Okamoto  */
	fputc(c, Qryiop);
	Notnull++;
}

void
monitor(bool recurs)
{
	register char	chr;
        static time_t	*timevec;
	register int	controlno;

	if ( recurs == FALSE )
	  setjmp(CmReset);

	initbuf(Qbuf, QbufSize, ERR_QBUF, error);
	clrmem(&Ctx, sizeof(Ctx));
	Ctx.ctx_cmark = Ctx.ctx_pmark = markbuf(Qbuf);
	setprocname(Ctx.ctx_name = Cm.cm_myname);
	Ctx.ctx_tvect = tT = FuncVect[0]->fn_tvect;
	xwait();
	if (RubLevel >= 0)
		signal(SIGINT, rubcatch);

	while ((chr = getch()) != 0) {

		if (chr == '\\') {
			/*
			** do not process "\\" as an escape char
			*/
			if ((chr = getch()) == '\\') {
				putch(chr);
				putch(chr);
				continue;
			} else
				ungetc(chr, Input);

			/* process control sequence */
			if ((controlno = getescape(1)) == 0)
				continue;

			switch (controlno) {

			  case C_EDIT:
				edit();
				continue;

			  case C_PRINT:
				print();
				continue;

			  case C_LIST:
				eval(1);
				continue;

			  case C_EVAL:
				eval(0);
				Autoclear = TRUE;
				continue;

			  case C_INCLUDE:
				include(0);
				cgprompt();
				continue;

			  case C_WRITE:
				writeout();
				cgprompt();
				continue;

			  case C_CHDIR:
				newdirec();
				cgprompt();
				continue;

			  case C_RESET:
				clear(1);
				continue;

			  case C_GO:
				go();
				continue;

			  case C_QUIT:
				clrline(1);
				quit(0);

			  case C_SHELL:
				shell();
				continue;

			  case C_TIME:
				time(timevec);
				printf("%s", ctime(timevec));
				clrline(0);
				continue;

			  case C_APPEND:
				Autoclear = 0;
				clrline(0);
				continue;

			  case C_MARK:
				getfilenm();
				prompt("");
				continue;

			  case C_BRANCH:
				branch();
				prompt("");
				continue;

			  case C_SYSTRACE:
				trace();
				prompt("");
				continue;

			  case C_SYSRESET:
				reset();
				clrline(0);
				continue;

			  default:
				syserr("monitor: bad code %d", controlno);
			}
		}
		putch(chr);
	}
	if (Input == stdin) {
		if (Nodayfile >= 0)
			printf("\n");
	}
	else
		fclose(Input);
}

char *
getname(void)
{
	register char	*p;
	static char	buf[41];
	register int	len;
	register char	c;

	p = buf;
	for (len = 0; len < 40; len++) {
		c = getch();
		if (isalpha(c)) {
			*p++ = c;
		} else if ((len == 0) && (c == '$')) {
			/* system control command */
			*p++ = c;
		} else {
			ungetc(c, Input);
			break;
		}
	}
	*p = '\0';
	return(buf);
}

int
getescape(int copy)
{
	register struct cntrlwd	*cw;
	register char		*word;

	word = getname();
	for (cw = Controlwords; cw->name; cw++) {
		if (strcmp(cw->name, word) == 0) {
			return (cw->code);
		}
	}

	/* not found -- pass symbol through and return failure */
	if (copy == 0)
		return (0);
	putch('\\');
	while (*word != 0) {
		putch(*word++);
	}
	return (0);
}

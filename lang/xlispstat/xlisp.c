/* xlisp.c - a small implementation of lisp with object-oriented programming */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* define the banner line string */
#define BANNER	"XLISP version 2.1, Copyright (c) 1989, by David Betz"

/* global variables */
jmp_buf top_level;
char *progname;  /* used for reading the symbol table - L. Tierney */

/* external variables */
extern LVAL s_stdin,s_evalhook,s_applyhook;
extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
extern int xltrcindent;
extern int xldebug;
extern LVAL true;
extern char buf[];
extern FILE *tfp;

/* external routines */
extern FILE *osaopen();

/* main - the main routine */
main(argc,argv)
  int argc; char *argv[];
{
    char *transcript;
    CONTEXT cntxt;
    int verbose,i;
    LVAL expr;
#ifdef AMIGA
    char project[30],defdir[50];
#endif AMIGA

    /* setup default argument values */
    transcript = NULL;
    verbose = FALSE;

    /* parse the argument list switches */
#ifndef MACINTOSH
#ifdef AMIGA
    FindStart(&argc,argv,deftool,project,defdir);
#endif AMIGA
    progname = argv[0];  /* L. Tierney */
    for (i = 1; i < argc; ++i)
	if (argv[i][0] == '-')
	    switch(argv[i][1]) {
	    case 't':
	    case 'T':
		transcript = &argv[i][2];
		break;
	    case 'v':
	    case 'V':
		verbose = TRUE;
		break;
	    }
#endif /* MACINTOSH */

    /* initialize and print the banner line */
    osinit(BANNER);

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    if (setjmp(cntxt.c_jmpbuf))
	xlfatal("fatal initialization error");
    if (setjmp(top_level))
	xlfatal("RESTORE not allowed during initialization");

    /* initialize xlisp */
    xlinit();
    xlend(&cntxt);

    /* reset the error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,true);

    /* open the transcript file */
    if (transcript && (tfp = osaopen(transcript,"w")) == NULL) {
	sprintf(buf,"error: can't open transcript file: %s",transcript);
	stdputstr(buf);
    }

    /* load "init.lsp" */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	xlload("init.lsp",TRUE,FALSE);

    /* load any files mentioned on the command line */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	for (i = 1; i < argc; i++)
	    if (argv[i][0] != '-' && !xlload(argv[i],TRUE,verbose))
		xlerror("can't load file",cvstring(argv[i]));

    /* target for restore */
    if (setjmp(top_level))
	xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,true);

    /* protect some pointers */
    xlsave1(expr);

    /* main command processing loop */
    for (;;) {

	/* setup the error return */
	if (setjmp(cntxt.c_jmpbuf)) {
	    setvalue(s_evalhook,NIL);
	    setvalue(s_applyhook,NIL);
	    xltrcindent = 0;
	    xldebug = 0;
	    osreset();   /* L. Tierney */
	    xlflush();
	}

	/* print a prompt */
	stdputstr("> ");

	/* read an expression */
	if (!xlread(getvalue(s_stdin),&expr,FALSE))
	    break;

	/* process carriage return (to clean up dribble) - L. Tierney */
	if (xlpeek(getvalue(s_stdin)) == '\n')
	  xlgetc(getvalue(s_stdin));
  
	/* save the input expression */
	xlrdsave(expr);

	/* evaluate the expression */
	expr = xleval(expr);

	/* save the result */
	xlevsave(expr);

	/* print it */
	stdprint(expr);
    }
    xlend(&cntxt);

    /* clean up */
    wrapup();
}

/* xlrdsave - save the last expression returned by the reader */
xlrdsave(expr)
  LVAL expr;
{
    setvalue(s_3plus,getvalue(s_2plus));
    setvalue(s_2plus,getvalue(s_1plus));
    setvalue(s_1plus,getvalue(s_minus));
    setvalue(s_minus,expr);
}

/* xlevsave - save the last expression returned by the evaluator */
xlevsave(expr)
  LVAL expr;
{
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}

/* xlfatal - print a fatal error message and exit */
xlfatal(msg)
  char *msg;
{
    oserror(msg);
    wrapup();
}

/* wrapup - clean up and exit to the operating system */
wrapup()
{
    if (tfp)
	osclose(tfp);
    osfinish();
    exit(0);
}

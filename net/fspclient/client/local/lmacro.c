/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND macro none "define a named macro"
 *  ---INFOEND---  */

#include "client.h"
#include "macro.h"
#include <ctype.h>

typedef enum { MACRO_DEFINE, MACRO_LIST, MACRO_REMOVE } MacroOp;

static int
#ifndef ANSI_PROTOTYPES
define_macro(name, help)
    char *name;
    char *help;
#else /* ANSI_PROTOTYPES */
define_macro(char *name, char *help)
#endif /* ANSI_PROTOTYPES */
{
    char buf[1024];
    int linecnt = 0;
    char **thismacro = 0;

    ffprintf(STDPROMPT,
	"enter macro definition terminated by `.' at start of blank line\n");

    while (!feof(STDIN))
    {	/* tidy the string up a little bit (remove leading and trailing WS) */
	char *sobuf, *eobuf, *pnt;

	ffprintf(STDPROMPT,"[%03d] ", linecnt+1);
	
	if (my_fgets(buf, 1024, STDIN) == 0)
	    break;

	if (buf[0] == '.' && (buf[1] == '\0' || buf[1] == '\n'))
	    break;

	for (pnt = buf, sobuf = eobuf = (char*)0; *pnt; pnt++)
	    if (!isspace(*pnt))
	    {
		if (!sobuf) sobuf = pnt;
		eobuf = pnt;
	    }

	if (!sobuf)
	    continue;

	*(eobuf + 1) = '\0';
	linecnt++;

	if (thismacro)
	    thismacro = (char**)realloc((char*)thismacro,
					sizeof(char*) * linecnt);
	else
	    thismacro = (char**)malloc(sizeof(char*) * linecnt);

	thismacro[linecnt-1] = strdup(sobuf);
    }

    if (feof(STDIN))
    {
	ffprintf(STDERR, "?macro: read EOF before end of macro `%s'\n", name);
	return 1;
    }

    return install_macro(name, linecnt, thismacro, help);
}

int
#ifndef ANSI_PROTOTYPES
main(argc, argv, envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
main(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    int retval, errcnt, ch;
    extern int optind, opterr;
    MacroOp macro_op;

    optind = 1;
    opterr = 1;
    errcnt = 0;

    macro_op = MACRO_DEFINE;

    while ((ch = getopt(argc, argv, "lr")) != EOF)
	switch (ch)
	{
	  case 'l':
	    macro_op = MACRO_LIST;
	    break;
	  case 'r':
	    macro_op = MACRO_REMOVE;
	    break;
	  default:
	    errcnt++;
	    break;
	}

    if (errcnt > 0)
	return 1;

    if (optind == argc)
    {
	ffprintf(STDERR, "?macro: macro needs to be named\n");
	return 1;
    }

    switch (macro_op)
    {
      case MACRO_DEFINE:
	retval = define_macro(argv[optind], argv[optind+1]);
	break;
      case MACRO_LIST:
	retval = list_macro(argv[optind]);
	break;
      case MACRO_REMOVE:
	retval = remove_macro(argv[optind]);
	break;
    }

    return retval;
}

/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#include "client.h"
#include "main.h"
#include "parse.h"
#include <ctype.h>

extern char **environ;

#define ISMAGIC 0x80
#define ISPROT  0x40

void
#ifndef ANSI_PROTOTYPES
freemyargs(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
freemyargs(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    if (argv == (char**)0)
	return;

#ifdef MALLOCDEBUG
  {
    int oldlvl = malloc_debug(0);
#endif

    for (i = 0; i < argc && argv[i]; i++)
	(void)free(argv[i]);

#ifdef MALLOCDEBUG
    (void)malloc_debug(oldlvl);
    if (!malloc_verify())
    {
        ffprintf(STDERR, "??freemyargs() has screwed up malloc()\n");
	abort();
    }
  }
#endif
}

/**********************************************************************
* create an argument list from a command line; the original line is
* undamaged by this function, and the values returned in the pargv
* are all malloc'd and should be free'd with freemyargs()
**********************************************************************/
int
#ifndef ANSI_PROTOTYPES
parsemyargs(comm, pargv, pmaxargc, gargc, gargv)
    char *comm;
    char **pargv[];
    int *pmaxargc;
    int gargc;
    char *gargv[];
#else /* ANSI_PROTOTYPES */
parsemyargs(char *comm, char ***pargv, int *pmaxargc, int gargc, char **gargv)
#endif /* ANSI_PROTOTYPES */
{
    int argc, i, j, rdsp, inquotes;
    char *buff, *magic;

    if ((buff = strdup(comm)) == (char*)0)
    {
	ffprintf(STDERR, "??out of memory in parsemyargs()\n");
	return 0;
    }

    /* count words -- ok, I know this can be done better, but I'm lazy */
    for (i = 0, argc = 0, rdsp = 1, inquotes = 0; buff[i]; i++)
    {
	/* buff[i] != 0 => next test can only succeed if inquotes != 0 */
	if (inquotes == '\\' || buff[i] == inquotes)
	{
	    inquotes = 0;
	    rdsp = 0;
	}
	else if (inquotes)
	{
	    ;
	}
	else if (isspace(buff[i]))
	{
	    if (!rdsp)
		buff[i] = '\0';
	    while (isspace(buff[i+1]))
		i++;
	    rdsp = 1;
	}
	else
	{
	    if (rdsp)
	    {
		/* check if we are starting a comment */
		if (buff[i] == '#')
		    break;

		/***********************************************************
		* if we have a single character command, pretend we have
		* spaces after it, even if we don't (true for first word only)
		***********************************************************/
		rdsp = (argc == 0)
		       && (buff[i] == '!' || buff[i] == '@');

		argc++;

		/***********************************************************
		* if the first character in the word is an unquoted `|', then
		* consider that as the beginning of the final word (which
		* will take the rest of the line)
		***********************************************************/
		if (buff[i] == '|')
		    break;
	    }

	    if (buff[i] == '"' || buff[i] == '\'' || buff[i] == '\\')
		inquotes = buff[i];
	}
    }

    if (inquotes)
    {
	ffprintf(STDERR, "?unclosed quotes\n");
	(void)free(buff);
	return 0;
    }

    if (argc > *pmaxargc - 1)
    {
	*pmaxargc = argc + 1;

	if (*pargv == (char**)0)
	    *pargv = (char**)malloc(*pmaxargc * sizeof(char*));
	else
	    *pargv = (char**)realloc((char*)*pargv, *pmaxargc * sizeof(char*));

	if (*pargv == (char**)0)
	{
	    ffprintf(STDERR, "??out of memory in parsemyargs()\n");
	    *pmaxargc = 0;
	    return 0;
	}
    }

    if ((magic = strdup(comm)) == (char*)0)
    {
	ffprintf(STDERR, "??out of memory in parsemyargs() for magic\n");
	(void)free(buff);
	*pmaxargc = 0;
	return 0;
    }

    /* set up *pargv[] */
    for (i = 0, j = 0; i < argc; i++, j++)
    {
	int k, d;
	
	while (isspace(buff[j]))
	    j++;

	if (i == 0 && (buff[j] == '!' || buff[j] == '@'))
	    switch (buff[j])
	    {
	      case '!':
		(*pargv)[i] = "shell";
		break;
	      case '@':
		(*pargv)[i] = BUILTIN;
		break;
	      default:
		(*pargv)[i] = "error";
		break;
	    }
	else if (i == argc - 1 && buff[j] == '|')
	    (*pargv)[i] = buff + j;
	else
	{
	    (*pargv)[i] = buff + j;
	    k = j;
	    while (buff[j])
		j++;

	    /***********************************************************
	    * at this point, (*pargv)[i] to buff+j is the word, still with
	    * quote characters in it; at this point we remove the quotes
	    * we mark magic characters (non-8bit clean) with a 0x80 top
	    * bit; outside of quotes, ?*{[$\ are all magic; inside `"'s
	    * just $\ are magic; inside "'"s nothing is magic; a magic \
	    * turns off the magic ability of the next character
	    ***********************************************************/
	    for (inquotes = 0, d = k; k <= j; k++)
		if ((inquotes & 0x7f) == '\\')
		{
		    inquotes = (inquotes & ISMAGIC)? '"': 0;
		    buff[d]    = buff[k];
		    magic[d++] = ISPROT;
		}
		else if (inquotes)
		{
		    if (inquotes == '"' && buff[k] == '\\')
			inquotes |= ISMAGIC;
		    else
		    {
			if (buff[k] != inquotes)
			{
			    buff[d]    = buff[k];
			    magic[d++] = (inquotes != '\''
					  && buff[k] == '$')? ISMAGIC: 0;
			}
			else
			    inquotes = 0;
		    }
		}
		else if (buff[k] == '"' || buff[k] == '\'' || buff[k] == '\\')
		    inquotes = buff[k];
		else
		{
		    buff[d]    = buff[k];
		    magic[d++] = (inquotes != '\'' &&
				    (buff[k] == '?' || buff[k] == '*' ||
				     buff[k] == '{' || buff[k] == '[' ||
				     buff[k] == '$'))? ISMAGIC: 0;
		}
	}
    }

    for (i = 0; i < argc; i++)
	(*pargv)[i] = strdup((*pargv)[i]);

    (*pargv)[argc] = (char*)0;
    (void)free(buff);
    (void)free(magic);

    return argc;
}

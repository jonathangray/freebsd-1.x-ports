/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#include "client.h"
#include "main.h"
#include "macro.h"

typedef struct MACRO {
	char	*name;
	int	length;
	char	**text;
	char	*help;
} MACRO;

static int maxmacro = 0, nummacro = 0;
static MACRO *macrotable = (MACRO*)0;

static int *macrostack = (int*)0;
static int stacktop    = 0;
static int stacklimit  = 0;
static int *cntstack   = (int*)0;

static void
#ifndef ANSI_PROTOTYPES
clean_macro(macro)
    MACRO *macro;
#else /* ANSI_PROTOTYPES */
clean_macro(MACRO *macro)
#endif /* ANSI_PROTOTYPES */
{
    if (macro == (MACRO*)0)
	return;

    macro->name   = (char*)0;
    macro->length = 0;
    macro->text   = (char**)0;
    macro->help   = (char*)0;
}

static void
#ifndef ANSI_PROTOTYPES
destroy_macro(macro)
    MACRO *macro;
#else /* ANSI_PROTOTYPES */
destroy_macro(MACRO *macro)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    if (macro == (MACRO*)0)
	return;

    for (i = 0; i < macro->length; i++)
        (void)free((char*)(macro->text[i]));

    (void)free((char*)(macro->name));
    (void)free((char*)(macro->help));

    nummacro--;

    clean_macro(macro);
}

static MACRO *
#ifndef ANSI_PROTOTYPES
lookup_macro(name)
    char *name;
#else /* ANSI_PROTOTYPES */
lookup_macro(char *name)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    if (macrotable == (MACRO*)0)
	return (MACRO*)0;

    for (i = 0; i < nummacro; i++)
        if (macrotable[i].name && strcmp(macrotable[i].name, name) == 0)
	    break;

    return (i < nummacro)? (&macrotable[i]): (MACRO*)0;
}

int
#ifndef ANSI_PROTOTYPES
remove_macro(name)
    char *name;
#else /* ANSI_PROTOTYPES */
remove_macro(char *name)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *macro;

    macro = lookup_macro(name);

    if (macro)
	destroy_macro(macro);
    else
	ffprintf(STDERR, "?no macro `%s'\n", name);

    return (macro == 0);
}

int
#ifndef ANSI_PROTOTYPES
list_macro(name)
    char *name;
#else /* ANSI_PROTOTYPES */
list_macro(char *name)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *macro;

    macro = lookup_macro(name);

    if (macro)
    {
	int i;

	ffprintf(STDOUT, "--- Macro `%s':  %d lines\n",
		 macro->name, macro->length);

	if (macro->help)
	    ffprintf(STDOUT, "    Help text: `%s'\n", macro->help);
	else
	    ffprintf(STDOUT, "	  No help text defined\n");

	for (i = 0; i < macro->length; i++)
	    if (macro->text[i])
	        ffprintf(STDOUT, "\t%s\n", macro->text[i]);
	    else
	        ffprintf(STDOUT, "\n");

	ffprintf(STDERR, ".\n");
    }
    else
	ffprintf(STDERR, "?no macro `%s'\n", name);

    return (macro == 0);
}

/***************************************************************************
* it is assumed that name will be free'd by the caller (if necessary)
* but that macrotext is being handed over fully to this function;
* i.e., do what you like with name back in the calling environment,
* but under no circumstances free macrotext!
***************************************************************************/
int
#ifndef ANSI_PROTOTYPES
install_macro(name, macrolen, macrotext, macrohelp)
    char *name;
    int macrolen;
    char **macrotext;
    char *macrohelp;
#else /* ANSI_PROTOTYPES */
install_macro(char *name, int macrolen, char **macrotext, char *macrohelp)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *oldmacro;
    MACRO *newmacro = (MACRO*)0;

    if (name == (char*)0 || *name == '\0')
    {
	ffprintf(STDERR, "?attempt to install null named macro failed\n");
	return 1;
    }

    oldmacro = lookup_macro(name);

    if (oldmacro != (MACRO*)0)
    {	/* free the old macro; the new macro can take its place */
	ffprintf(STDINFO, "?overwriting old macro definition for `%s'\n",
		      name);
	destroy_macro(oldmacro);
	newmacro = oldmacro;
    }
    else if (nummacro < maxmacro)
    {	/* there is a free space in the table */
	int i;
	for (i = 0; i < maxmacro; i++)
	    if (!macrotable[i].name)
		break;

	/* if this fails then we have *big* problems, oh well, check anyway */
	if (i < maxmacro)
	    newmacro = &macrotable[i];
	else
	{
	    ffprintf(STDERR,"??internal error: nummacro(%d) < maxmacro(%d) and no gap found\n", nummacro, maxmacro);
	    return 1;
	}
    }
    else
    {	/* no free space in table; extend by a bit */
	maxmacro += 4;

	if (macrotable)
	    macrotable = (MACRO*)realloc((char*)macrotable,
					  sizeof(MACRO) * maxmacro);
	else
	    macrotable = (MACRO*)malloc(sizeof(MACRO) * maxmacro);

	if (macrotable == (MACRO*)0)
	{
	    ffprintf(STDERR, "??run out of memory in install_macro()\n");
	    abort();
	}
	else
	{
	    int i;
	    for (i = nummacro + 1; i < maxmacro; i++)
		clean_macro(&macrotable[i]);
	}

	newmacro = &macrotable[nummacro];
    }

    nummacro++;

    newmacro->name   = strdup(name);
    newmacro->length = macrolen;
    newmacro->text   = macrotext;
    newmacro->help   = macrohelp? strdup(macrohelp): 0;

    return 0;
}

int
#ifndef ANSI_PROTOTYPES
deinstall_macro(name)
    char *name;
#else /* ANSI_PROTOTYPES */
deinstall_macro(char *name)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *macro;

    macro = lookup_macro(name);

    if (macro == (MACRO*)0)
    {
	ffprintf(STDERR, "?can not delete macro `%s': doesn't exist\n", name);
	return 1;
    }

    destroy_macro(macro);

    return 0;
}

int
#ifndef ANSI_PROTOTYPES
initialise_macro(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
initialise_macro(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *macro;
    int i;

    macro = lookup_macro(argv[0]);
    if (macro == (MACRO*)0)
	return 1;

    if (dbug_flag > 0)
    {
	int i;
	ffprintf(STDDBG, "\nmacro initialise called with valid macro name:\n");
	for (i = 0; i <= argc; i++)
	    ffprintf(STDDBG, "    $%-2d = \"%s\"\n", i, argv[i]);

	ffprintf(STDDBG, "\ndefinition:\n");
	for (i = 0; i < macro->length; i++)
	    ffprintf(STDDBG, "%02d: %s\n", i, macro->text[i]);

	ffprintf(STDDBG, "\n");
    }

    /* we don't allow recursion (yet) so check this isn't trying to */
    for (i = 0; i < stacktop; i++)
	if (macro == &macrotable[macrostack[i]])
	{
	    ffprintf(STDERR,
		"?attempted recursion in macro `%s' -- ignoring call to `%s'\n",
		macrotable[macrostack[stacktop-1]].name, macro->name);
	    (void)fflush(STDERR);
	    return 0;
	}

    /* push this macro onto the macro execution stack */
    if (stacktop >= stacklimit)
    {
	stacklimit += 8;

	if (macrostack)
	    macrostack = (int*)realloc((char*)macrostack,
					  stacklimit * sizeof(int));
	else
	    macrostack = (int*)malloc(stacklimit * sizeof(int));

	if (cntstack)
	    cntstack = (int*)realloc((char*)cntstack,
					  stacklimit * sizeof(int));
	else
	    cntstack = (int*)malloc(stacklimit * sizeof(int));
    }

    macrostack[stacktop] = (int)(macro - macrotable);
    cntstack[stacktop++] = 0;

    return 0;
}

char *
#ifndef ANSI_PROTOTYPES
get_macroline()
#else /* ANSI_PROTOTYPES */
get_macroline(void)
#endif /* ANSI_PROTOTYPES */
{
    if (stacktop > 0)
    {
	int st = stacktop - 1;
	if (cntstack[st] < macrotable[macrostack[st]].length)
	    return (macrotable[macrostack[st]].text[cntstack[st]++]);
	else
	    stacktop--;
    }

    return (char*)0;
}

#define NCO 6
void
#ifndef ANSI_PROTOTYPES
fsp_macro_long_help_all()
#else /* ANSI_PROTOTYPES */
fsp_macro_long_help_all(void)
#endif /* ANSI_PROTOTYPES */
{
    int i;
    int notext = 0;

    if (macrotable == (MACRO*)0 || nummacro == 0)
    {
	ffprintf(STDOUT,"No macros defined\n");
	return;
    }

    ffprintf(STDOUT,"Macros are:\n");

    for (i = 0; i < maxmacro; i++)
	if (macrotable[i].name)
	{
	    if (macrotable[i].help)
		ffprintf(STDOUT,"      %-10s    %s\n",
			 macrotable[i].name, macrotable[i].help);
	    else
		notext++;
	}

    if (notext)
    {
	int j;

	ffprintf(STDOUT,"\nUndocumented macros are:\n");

	for (i = 0, j = 0; i < maxmacro; i++)
	    if (macrotable[i].name)
		if (!macrotable[i].help)
		{
		    ffprintf(STDOUT,"  %-10s%s",
			     macrotable[i].name, j == NCO-1? "\n" : "");
		    j = (j+1) % NCO;
		}

	if (j)
	    ffprintf(STDOUT,"\n");
    }
}

int
#ifndef ANSI_PROTOTYPES
fsp_macro_long_help(name)
    char *name;
#else /* ANSI_PROTOTYPES */
fsp_macro_long_help(char *name)
#endif /* ANSI_PROTOTYPES */
{
    MACRO *macro;

    macro = lookup_macro(name);

    if (macro)
    {
	if (macro->help)
	    ffprintf(STDOUT,"%-10s    %s\n", macro->name, macro->help);
	else
	    ffprintf(STDOUT, "no help available for macro `%s'\n", name);
    }
    else
	return 1;

    return 0;
}

void
#ifndef ANSI_PROTOTYPES
fsp_macro_short_help()
#else /* ANSI_PROTOTYPES */
fsp_macro_short_help(void)
#endif /* ANSI_PROTOTYPES */
{
    int i, j;

    if (macrotable == (MACRO*)0 || nummacro == 0)
    {
	ffprintf(STDOUT,"No macros defined\n");
	return;
    }

    ffprintf(STDOUT,"Macros are:\n");

    for (i = 0, j = 0; i < maxmacro; i++)
	if (macrotable[i].name)
	{
	    ffprintf(STDOUT,"  %-10s%s",
		     macrotable[i].name, j == NCO-1? "\n" : "");
	    j = (j+1) % NCO;
	}

    if (j)
	ffprintf(STDOUT,"\n");
}

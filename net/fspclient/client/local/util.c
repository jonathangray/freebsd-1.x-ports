/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

#include "client.h"
#include "main.h"
#include "util.h"
#include <ctype.h>

#define RDLNSIZE 256

char *
#ifndef ANSI_PROTOTYPES
readword()
#else /* ANSI_PROTOTYPES */
readword(void)
#endif /* ANSI_PROTOTYPES */
{
    static char word[RDLNSIZE];
    char *soword, *eoword;

    if (my_fgets(word, RDLNSIZE, STDIN) == (char*)0)
	return (char*)0;

    soword = word;
    while (isspace(*soword))
	soword++;

    eoword = soword;
    while (*eoword && !isspace(*eoword))
	eoword++;

    if (*eoword)
	*eoword = '\0';

    return soword;
}

void
#ifndef ANSI_PROTOTYPES
disconnect()
#else /* ANSI_PROTOTYPES */
disconnect(void)
#endif /* ANSI_PROTOTYPES */
{
    if (!notconnected)
    {
	util_dirty_version();
	util_flushdir();
	util_flushpro();
	finish_client();
	notconnected = 1;
    }
}

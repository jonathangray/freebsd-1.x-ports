/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND buffer none "set the size of the data packets (max 1024)"
 *  ---INFOEND---  */

#include "client.h"
#include "util.h"

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
    char *newbuffer;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT, "(size) ");
	    newbuffer = readword();
	}
	else
	    newbuffer = (char*) 0;
    }
    else
	newbuffer = argv[1];

    if (newbuffer == (char*) 0)
	return 1;

    client_buf_len = atoi(newbuffer);
    if (client_buf_len > UBUF_SPACE)
	client_buf_len = UBUF_SPACE;
    client_net_len = htons(client_buf_len);

    ffprintf(STDINFO, "buffer size set to %d bytes\n", client_buf_len);

    return 0;
}

/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

#ifndef ANSI_PROTOTYPES
extern char *readword();
extern void disconnect();
#else /* ANSI_PROTOTYPES */
extern char *readword(void);
extern void disconnect(void);
#endif /* ANSI_PROTOTYPES */

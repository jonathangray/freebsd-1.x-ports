/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#define BUILTIN "builtin"

#ifndef ANSI_PROTOTYPES
extern int parsemyargs();
extern void freemyargs();

#else /* ANSI_PROTOTYPES */

extern int parsemyargs(char *comm, char ***pargv, int *pmaxargc,
		       int gargc, char **gargv);
extern void freemyargs(int argc, char **argv);
#endif /* ANSI_PROTOTYPES */

/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

typedef enum { NOGLOB, LOCALGLOB, REMOTEGLOB } GLOBS;

typedef struct DT {
	char	*com;
	int	needconn;
	GLOBS	glob;
#if !defined(ANSI_PROTOTYPES) || defined(BROKEN_STRUCT_PROTOTYPES)
	int	(*fun)();
#else /* ANSI_PROTOTYPES */
	int	(*fun)(int argc, char *argv[], char *envp[]);
#endif /* ANSI_PROTOTYPES */
	char	*help;
} DT;

extern int notconnected;
extern int notquit;
extern int last_retcode;

#ifndef ANSI_PROTOTYPES
extern void execute_command();
extern int execute_stdin();

#else /* ANSI_PROTOTYPES */

extern void execute_command(int argc, char **argv);
extern int execute_stdin(int argc, char **argv);
#endif /* ANSI_PROTOTYPES */

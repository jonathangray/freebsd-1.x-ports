/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#ifndef ANSI_PROTOTYPES
extern int list_macro();
extern int remove_macro();
extern int install_macro();
extern int deinstall_macro();
extern int initialise_macro();
extern char *get_macroline();

extern void fsp_macro_long_help_all();
extern int fsp_macro_long_help();
extern void fsp_macro_short_help();

#else /* ANSI_PROTOTYPES */

extern int list_macro(char *name);
extern int remove_macro(char *name);
extern int install_macro(char *name, int macrolen,
			 char **macrotext, char *macrohelp);
extern int deinstall_macro(char *name);
extern int initialise_macro(int argc, char **argv);
extern char *get_macroline(void);

extern void fsp_macro_long_help_all(void);
extern int fsp_macro_long_help(char *name);
extern void fsp_macro_short_help(void);
#endif /* ANSI_PROTOTYPES */

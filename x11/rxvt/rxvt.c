/*  Copyright 1992 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */
/*
 * This module has been heavily modified by R. Nation
 * (nation@rocket.sanders.lockheed.com).
 * No additional restrictions are applied
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 */
#include <stdio.h>

#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "command.h"
#include "xsetup.h"
#include "screen.h"
#include "sbar.h"
#include "rxvt.h"
#include "debug.h"

/*  The default command argv if we just run a shell.
 */
static char *shell_argv[2] = 
{
  "/bin/sh",
  NULL,
};


char *command = (char *)0;

/*  Run the command in a subprocess and return a file descriptor for the
 *  master end of the pseudo-teletype pair with the command talking to
 *  the slave.
 */
void main(int argc,char **argv)
{
  int i;
  char *shell;
  char **com_argv;
  
  for (i = 0; i < argc; i++)
    if (strcmp(argv[i],"-e") == 0)
      break;
  if (i < argc - 1) 
    {
      argv[i] = NULL;
      com_argv = argv + i + 1;
      argc = i;
      command = com_argv[0];
    } 
  else 
    {
      com_argv = shell_argv;
      if ((shell = getenv("SHELL")) != NULL)
	shell_argv[0] = shell;
      command = (char *)0;
    }
  
  /*  Add a TERM entry to the environment.
   */
  putenv(TERM_ENV);
  
  init_display(argc,argv);
  if(command == (char *)0)
    command = com_argv[0];

  init_command(command,(unsigned char **)com_argv);
  get_token();
}

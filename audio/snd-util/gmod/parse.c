
/*
* This file is part of the GMOD package.
*
* parse_args by Andrew J. Robinson
*
* Parse command-line arguments.
*/


#include <unistd.h>
#ifndef __386BSD__
#include <getopt.h>
#endif
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

int
parse_args (int argc, char *argv[], struct options_info *options)
{
  extern char *optarg;
  extern int optind;
  int option, num_err = 0;

  while ((option = getopt (argc, argv, "ehlv:")) != -1)
    switch (option)
      {
      case 'h':
	printf ("Usage: %s [options] modfile . . .\n\n", argv[0]);
	printf ("Options:\n");
	printf ("     -e     Show empty samples (for messages)\n");
	printf ("     -h     Help\n");
	printf ("     -l     Break infinite loops\n");
	printf ("     -v x   Set volume to x (0 - 255)\n\n");
	printf ("One or more MultiTracker, UltraTracker, 4 or 8 channel MOD files,\n");
	printf ("or 669 files should be listed on the command line.  The files will be\n");
	printf ("played in the order specified.\n\n");
	exit (0);
      case 'e':
	options->show_empty_samples = 1;
	break;
      case 'l':
	options->loop_breaker = 1;
	break;
      case 'v':
	option = atoi (optarg);
	if ((option > 255) || (option < 0))
	  {
	    printf ("%s: volume must be between 0 and 255.\n", argv[0]);
	    num_err = 1;
	  }
	else
	  options->main_volume = option;
	break;
      case '?':
	num_err = 1;
	break;
      case ':':
	num_err = 1;
	break;
      }

  if (num_err)
    {
      printf ("\nUsage: %s [options] modfile . . .\n", argv[0]);
      printf ("Use %s -h for help.\n\n", argv[0]);
      exit (50);
    }

  return optind;
}

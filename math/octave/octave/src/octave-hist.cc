// octave-hist.cc                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

The function listed below was adapted from a similar function from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  builtin_history

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fstream.h>

#include "statdefs.h"
#include "utils.h"
#include "error.h"
#include "input.h"
#include "octave.h"
#include "octave-hist.h"

extern "C"
{
#include <readline/history.h>
}

// Nonzero means we are saving history lines.
int saving_history = 1;

// The number of lines to save in the history file.
static int octave_hist_size = 1024;

// The name of the history file.
static char *octave_hist_file;

// The number of hisory lines we read from the history file.
static int history_lines_in_file = 0;

// The number of history lines we've saved so far.
static int history_lines_this_session = 0;

/*
 * Get some default values, possibly reading them from the
 * environment.
 */
static int
default_history_size (void)
{
  int size = 1024;
  char *env_size = getenv ("OCTAVE_HISTSIZE");
  if (env_size != (char *) NULL)
    {
      int val;
      if (sscanf (env_size, "%d", &val) == 1)
	size = val > 0 ? val : 0;
    }
  return size;
}

static char *
default_history_file (void)
{
  char *file = (char *) NULL;;

  char *env_file = getenv ("OCTAVE_HISTFILE");
  if (env_file != (char *) NULL)
    {
      fstream f (env_file, (ios::in | ios::out));
      if (f != 0)
	{
	  file = strsave (env_file);
	  f.close ();
	}
    }

  if (file == (char *) NULL)
    {
      if (home_directory != NULL)
	file = strconcat (home_directory, "/.octave_hist");
    }

  return file;
}

/*
 * Prime the history list.
 */
void
initialize_history (void)
{
  octave_hist_file = default_history_file ();
  octave_hist_size = default_history_size ();

  read_history (octave_hist_file);
  using_history ();
  history_lines_in_file = where_history ();
}

void
clean_up_history (void)
{
  stifle_history (octave_hist_size);
  write_history (octave_hist_file);
}

void
maybe_save_history (char *s)
{
  if (saving_history)
    {
      add_history (s);
      history_lines_this_session++;
    }
}

/*
 * Display, save, or load history.  Stolen and modified from bash.
 *
 * Arg of -w FILENAME means write file, arg of -r FILENAME
 * means read file.  Arg of N means only display that many items.
 */
void
do_history (int argc, char **argv)
{
  HIST_ENTRY **hlist;

  while (--argc > 0)
    {
      argv++;

      if (*argv[0] == '-' && strlen (*argv) == 2
	  && ((*argv)[1] == 'r' || (*argv)[1] == 'w'
	      || (*argv)[1] == 'a' || (*argv)[1] == 'n'))
	{
	  char *file;
	  int result = 0;

	  if (argc > 1)
	    file = *(argv+1);
	  else
	    file = octave_hist_file;

	  switch ((*argv)[1])
	    {
	    case 'a':		// Append `new' lines to file.
	      {
		if (history_lines_this_session)
		  {
		    if (history_lines_this_session < where_history ())
		      {
// If the filename was supplied, then create it if it doesn't already
// exist.
			if (file)
			  {
			    struct stat buf;

			    if (stat (file, &buf) == -1)
			      {
				int tem;

				tem = open (file, O_CREAT, 0666);
				close (tem);
			      }
			  }

			result =
			  append_history (history_lines_this_session, file);
			history_lines_in_file += history_lines_this_session;
			history_lines_this_session = 0;
		      }
		  }
	      }
	      break;
	    case 'w':		// Write entire history.
	      result = write_history (file);
	      break;
	    case 'r':		// Read entire file.
	      result = read_history (file);
	      break;
	    case 'n':		// Read `new' history from file.
// Read all of the lines in the file that we haven't already read.
	      using_history ();
	      result = read_history_range (file, history_lines_in_file, -1);
	      using_history ();
	      history_lines_in_file = where_history ();
	      break;
	    }
	  return;
	}
      else if (strcmp (*argv, "--") == 0)
	{
	  argc--;
	  argv++;
	  break;
	}
      else
	break;
    }

  int limited = 0;
  int limit = 0;

  if (argc > 0)
    {
      limited = 1;
      if (sscanf (*argv, "%d", &limit) != 1)
        {
	  if (*argv[0] == '-')
	    message ("history", "unrecognized option `%s'", *argv);
	  else
	    message ("history", "bad non-numeric arg `%s'", *argv);
	  return;
        }
    }

  hlist = history_list ();

  if (hlist)
    {
      for (int i = 0; hlist[i] != (HIST_ENTRY *) NULL; i++)
	; // Do nothing.

      if (limit < 0)
	limit = -limit;

      if (!limited)
	i = 0;
      else
	if ((i -= limit) < 0)
	  i = 0;

      while (hlist[i])
	{
//	  QUIT;  // in bash: (interrupt_state) throw_to_top_level ();

	  char tmp[7];
	  sprintf (tmp, "%5d%c ", i + history_base,
		   hlist[i]->data ? '*' : ' ');
	  cerr << hlist[i]->line << "\n";
	  i++;
	}
    }
}

int
current_history_number (void)
{
  using_history ();

  if (octave_hist_size > 0)
    return history_base + where_history ();
  else
    return -1;

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

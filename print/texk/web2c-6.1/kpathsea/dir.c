/* dir.c: directory operations.

Copyright (C) 1992, 93 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/c-stat.h>
#include <kpathsea/dir.h>


/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

boolean
dir_p P1C(const_string, fn)
{
  struct stat stats;
  return stat (fn, &stats) == 0 && S_ISDIR (stats.st_mode);
}


/* Return -1 if FN isn't a directory, else its number of links.
   Duplicate the call to stat; no need to incur overhead of a function
   call for that much cleanliness. */

int
dir_links P1C(const_string, fn)
{
  int ret;
  struct stat stats;
  
  ret = stat (fn, &stats) == 0 && S_ISDIR (stats.st_mode)
        ? stats.st_nlink : -1;

#ifdef DEBUG
  if (DEBUG_P (DEBUG_STAT))
    printf ("dir_links (%s) => %d\n", fn, ret);
#endif

  return ret;
}

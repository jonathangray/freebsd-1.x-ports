/* tex-file.c: stuff for all TeX formats.

Copyright (C) 1993, 94 Karl Berry.

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

#include <kpathsea/default.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-file.h>


/* See tex-font.h.  */
const_string kpse_fallback_font = NULL;
unsigned *kpse_fallback_resolutions = NULL;
string kpse_font_override_path = NULL;

/* The compiled-in default list, DEFAULT_FONT_SIZES, is intended to be
   set from the command line (presumably via the Makefile).  */

#ifndef DEFAULT_FONT_SIZES
#define DEFAULT_FONT_SIZES ""
#endif

void
kpse_init_fallback_resolutions P1C(string, envvar)
{
  const_string size_var = ENVVAR (envvar, "TEXSIZES");
  string size_str = getenv (size_var);
  unsigned *last_resort_sizes = NULL;
  unsigned size_count = 0;
  string size_list = kpse_expand_default (size_str, DEFAULT_FONT_SIZES);

  /* Initialize the list of last-resort sizes.  */
  for (size_str = kpse_path_element (size_list); size_str != NULL;
       size_str = kpse_path_element (NULL))
    {
      if (! *size_str)
        continue;

      size_count++;
      XRETALLOC (last_resort_sizes, size_count, unsigned);
      last_resort_sizes[size_count - 1] = atoi (size_str);
    }

  /* Add a zero to mark the end of the list.  */
  size_count++;
  XRETALLOC (last_resort_sizes, size_count, unsigned);
  last_resort_sizes[size_count - 1] = 0;

  kpse_fallback_resolutions = last_resort_sizes;
}

/* See the .h file for the description.  */

string
kpse_find_file P4C(const_string, name,  const_string, ext,
                   const_string, path,  boolean, must_exist)
{
  string full_name = concat3 (name, ".", ext);
  string ret = kpse_path_search (path, full_name, must_exist);
  
  free (full_name);

  return ret;
}

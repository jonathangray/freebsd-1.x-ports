/* init-path.c: Find how to initialize a path.

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
#include <kpathsea/init-path.h>


/* See init-path.h.  */

string
kpse_init_path PVAR2C(const_string, current,
                      const_string, default_path,  ap)
{
  string env_name;
  string ret;
  string env_path = NULL;

  /* First envvar that's set will exit the loop.  */
  while (!env_path && (env_name = va_arg (ap, string)) != NULL)
    {
      env_path = getenv (env_name);
    }
  va_end (ap);
  
  /* Expand extra colons.  */
  ret = kpse_expand_default (env_path ? env_path : current, default_path);
  
  return ret;
}}

#ifdef TEST
int
main ()
{
  printf ("null = \"%s\"\n", kpse_init_path (false, "def", NULL));
  printf ("TEXFONTS" = \"%s\"\n",
          kpse_init_path (false, "def", "TEXFONTS", NULL));
  printf ("GLYPHFONTS" = \"%s\"\n",
          kpse_init_path ("false, def", "GLYPHFONTS", "TEXFONTS", NULL));
  return 0;
}

#endif /* TEST */


/*
Local variables:
compile-command: "gcc -posix -g -I. -I.. -DTEST init-path.c kpathsea.a"
End:
*/

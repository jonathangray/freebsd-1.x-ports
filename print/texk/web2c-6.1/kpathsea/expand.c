/* expand.c: general expansion.

Copyright (C) 1993 Karl Berry.

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

#include <kpathsea/expand.h>
#include <kpathsea/tilde.h>
#include <kpathsea/variable.h>


/* Do variable expansion so ~${USER} will work.  (Besides, it's what the
   shells do.)  */

string
kpse_expand P1C(const_string, s)
{
  string var_expansion = kpse_var_expand (s);
  string tilde_expansion = kpse_tilde_expand (var_expansion);
  
  /* `kpse_var_expand' always gives us new memory; `kpse_tilde_expand'
     doesn't, necessarily.  So be careful that we don't free what we are
     about to return.  */
  if (tilde_expansion != var_expansion)
    free (var_expansion);
  
  return tilde_expansion;
}

// procstream.cc                                          -*- C++ -*-
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

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include "procstream.h"

procstreambase::procstreambase (void)
{
  init (new procbuf ());
}

procstreambase::procstreambase (const char *command, int mode = ios::out)
{
  init (new procbuf ());
  if (! rdbuf()->open (command, mode))
    set (ios::badbit);
}

void
procstreambase::open (const char *command, int mode = ios::out)
{
  clear ();
  if (! rdbuf()->open (command, mode))
    set (ios::badbit);
}

void
procstreambase::close (void)
{
  if (! rdbuf()->close ())
    set (ios::failbit);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

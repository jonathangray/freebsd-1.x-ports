// NLConst.cc                                            -*- C++ -*-
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

#include "NLConst.h"

NLConst::NLConst (void) : Bounds (), NLFunc ()
{
}

NLConst::NLConst (int n) : Bounds (n), NLFunc ()
{
}

NLConst::NLConst (const Vector l, const NLFunc f, const Vector u)
  : Bounds (l, u), NLFunc (f)
{
}

NLConst::NLConst (const NLConst& a)
  : Bounds (a.lb, a.ub), NLFunc (a.fun, a.jac)
{
}

NLConst&
NLConst::operator = (const NLConst& a)
{
  nb = a.nb;
  lb = a.lb;
  fun = a.fun;
  jac = a.jac;
  ub = a.ub;

  return *this;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

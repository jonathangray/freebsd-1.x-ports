/* Replacements for missing math functions.               -*- C -*- */
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

#if !defined (_missing_math_h)
#define _missing_math_h 1

extern "C"
{

#if defined (ACOSH_MISSING)
extern double acosh (double);
#endif

#if defined (ASINH_MISSING)
extern double asinh (double);
#endif

#if defined (ATANH_MISSING)
extern double atanh (double);
#endif

#if defined (GAMMA_MISSING)
extern double gamma (double);
#endif

}

#endif

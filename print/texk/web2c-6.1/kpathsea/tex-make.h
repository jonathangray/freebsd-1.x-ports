/* tex-make.h: declarations for executing external scripts.

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

#ifndef KPATHSEA_TEX_MAKE_H
#define KPATHSEA_TEX_MAKE_H

#include <kpathsea/c-proto.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/types.h>


typedef struct
{
  boolean enabled; /* whether to run the program */
  const_string program;  /* executable name */
  const_string args;     /* additional args to pass */
} kpse_make_spec_type;

#define KPSE_MAKE_SPEC_ENABLED(ms) ((ms).enabled)
#define KPSE_MAKE_SPEC_PROGRAM(ms) ((ms).program)
#define KPSE_MAKE_SPEC_ARGS(ms) ((ms).args)

/* Indexed by `kpse_file_format_type', gives info for each file format.  */
extern kpse_make_spec_type kpse_make_specs[];


/* Says whether to throw away stderr output from the MakeTeX... scripts,
   or let it be seen.  */
extern boolean kpse_make_tex_discard_errors;


/* Look for a file named by BASE_FILE in format FORMAT.  If other
   information about the file is needed, it can be passed through
   environment variables.  See the MakeTeXPK stuff in `tex-make.c' for
   an example.  */
extern string kpse_make_tex P2H(kpse_file_format_type format,
                                const_string base_file);

/* Set the environment variable MAKETEX_MAG to the magstep string
   (suitable for assigning to Metafont's `mag' parameter) corresponding
   to a font loaded at DPI on a base resolution of BASE_DPI.  */
extern void kpse_set_maketex_mag P2H(int dpi, int base_dpi);


/* Defines how far away a pixel file can be found from its stated size.
   The DVI standard says any resolution within 0.2% of the stated size
   is ok, but we are more forgiving.  */
#define KPSE_BITMAP_TOLERANCE(r) ((r) / 500.0 + 1)

/* Check whether DPI2 is within KPSE_BITMAP_TOLERANCE of DPI1. */
extern boolean kpse_check_bitmap_tolerance P2H(double dpi1, double dpi2);

#endif /* not KPATHSEA_TEX_MAKE_H */

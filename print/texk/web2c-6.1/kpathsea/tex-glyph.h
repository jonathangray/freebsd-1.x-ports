/* tex-glyph.h: look for a TeX glyph font (GF or PK).

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

#ifndef KPATHSEA_TEX_GLYPH_H
#define KPATHSEA_TEX_GLYPH_H

#include <kpathsea/tex-file.h>


/* Search first for the font named FONT_NAME at resolution DPI in the
   glyph format FORMAT (see `try_size' for details of format searching).
   Then try resolutions within KPSE_BITMAP_TOLERANCE of DPI.  Then try
   the resolutions in `kpse_fallback_sizes', then within the tolerance
   of each of those.  Then if FONT_NAME is an alias defined in a
   texfonts.map do all the above for its real name.  Then try the above
   for kpse_fallback_name.  Then fail.  Return either the filename
   found, or NULL.  Also return information about the file found in
   *FONT_FILE.  */
extern string kpse_find_glyph_format
  P4H(const_string font_name, unsigned dpi, 
      kpse_file_format_type format, kpse_font_file_type *font_file);

/* Look for PK files only.  */
#define kpse_find_pk(font_name, dpi, font_file) \
  kpse_find_glyph_format (font_name, dpi, kpse_pk_format, font_file)

#endif /* not KPATHSEA_TEX_GLYPH_H */

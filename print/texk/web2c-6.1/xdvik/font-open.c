/* font-open.c: find font filenames.  This bears no relation (but the
   interface) to the original font_open.c, so I renamed it.  */

#include "config.h"

#include <kpathsea/tex-glyph.h>


/* We try for a VF first because that's what dvips does.  Also, it's
   easier to avoid running MakeTeXPK if we have a VF this way.  */

FILE *
font_open (font, font_ret, dpi, mag_ret, filename_ret)
    _Xconst char *font;
    char **font_ret;
    float dpi;
    int *mag_ret;
    char **filename_ret;
{
  FILE *ret;
  char *name = kpse_find_vf (font);
  
  if (name)
    {
      /* VF fonts don't have a resolution, but loadfont will complain if
         we don't return what it asked for.  */
      *mag_ret = dpi;
      *font_ret = NULL;
    }
  else
    {
      kpse_font_file_type file_ret;
      name = kpse_find_glyph_format (font, (unsigned) (dpi + .5),
                                     kpse_any_glyph_format, &file_ret);
      if (name)
        {
          /* If we got it normally, from an alias, or from MakeTeXPK,
             don't fill in FONT_RET.  That tells load_font to complain.  */
          *font_ret
             = KPSE_FONT_FILE_SOURCE (file_ret) == kpse_source_fallback
               ? KPSE_FONT_FILE_NAME (file_ret)
               : NULL; /* tell load_font we found something good */
          
          *mag_ret = KPSE_FONT_FILE_DPI (file_ret);
        }
      /* If no VF and no PK, FONT_RET is irrelevant? */
    }
  
  /* load_font wants the magnification, not the resolution.  */
  *mag_ret *= 5;

  /* If we found a name, return the stream.  */
  ret = name ? xfopen (name) : NULL;
  *filename_ret = name;

  return ret;
}

/* tex-glyph.c: Search for GF/PK files.

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

#include <kpathsea/expand.h>
#include <kpathsea/fontmap.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/tex-glyph.h>
#include <kpathsea/tex-make.h>
#include <kpathsea/variable.h>

/* Routines are in bottom-up order.  */

/* Return malloced filename for bitmap font NAME at resolution DPI with
   extension SUFFIX.  We depend on callers setting the environment
   variables; we only find the right variable spec to use.  */

#ifndef KPATHSEA_BITMAP_NAME
#ifdef DOS
/* dpi300\cmr10.pk */
#define KPATHSEA_BITMAP_NAME \
  "dpi$KPATHSEA_DPI\\$KPATHSEA_NAME.$KPATHSEA_FORMAT"
#else
/* cmr10.300pk */
#define KPATHSEA_BITMAP_NAME "$KPATHSEA_NAME.$KPATHSEA_DPI$KPATHSEA_FORMAT"
#endif /* not DOS */
#endif /* not KPATHSEA_BITMAP_NAME */

static string
bitmap_name P3C(const_string, name,  unsigned, dpi,  const_string, suffix)
{
  string ret;
  const_string spec = getenv ("KPATHSEA_BITMAP_NAME");
  
  /* We could save a speck of time by setting the environment variable
     to our compile-time default at the beginning of the program, but it
     it doesn't seem worth the code separation.  */
  if (!spec)
    spec = KPATHSEA_BITMAP_NAME;
  
  ret = kpse_var_expand (spec);

  return ret;
}


/* Look up FONT_NAME at resolution DPI in PATH, with filename suffix
   EXTENSION.  Return file found or NULL.  */

static string
try_format P4C(const_string, font_name,  unsigned, dpi,
               const_string, path,  const_string, extension)
{
  string name, ret;
  
  /* Set the suffix on the name we'll be searching for.  */
  xputenv ("KPATHSEA_FORMAT", extension);

  name = bitmap_name (font_name, dpi, extension);
  ret = kpse_path_search (path, name, true);
  
  if (name != ret)
    free (name);
    
  return ret;
}

/* Look for FONT_NAME at resolution DPI, using GLYPH_PATHS.  If
   GLYPH_PATHS[format] is NULL, don't search for that format; otherwise,
   it is the raw search path to use for that format.  Search the
   (entire) PK path first, then the GF path, if we're looking for both.
   Return the filename found, and (if it's non-NULL) fill in FONT_FILE.  */

static string
try_size P4C(const_string, font_name,
             unsigned, dpi,
             string *, glyph_paths,
             kpse_font_file_type *, font_file)
{
  kpse_file_format_type format_found;
  string ret;
  string gf_path = glyph_paths[kpse_gf_format];
  string pk_path = glyph_paths[kpse_pk_format];

  xputenv_int ("KPATHSEA_DPI", dpi);
  
  /* Look for PK first (since it's more likely to be found), then GF.  */
  ret = pk_path ? try_format (font_name, dpi, pk_path, "pk") : NULL;

  if (ret != NULL)
    format_found = kpse_pk_format;
  else
    {
      if (gf_path)
        {
          ret = try_format (font_name, dpi, gf_path, "gf");
          format_found = kpse_gf_format;
        }
    }
  
  if (ret != NULL && font_file)
    { /* Success.  Fill in the return info.  Discard const.  */
      font_file->name = (string) font_name;
      font_file->dpi = dpi;
      font_file->format = format_found;
    }
    
  return ret;
}

/* Look for FONT_NAME at resolution DPI, then at the resolutions within
   KPSE_BITMAP_TOLERANCE of DPI.  See `try_name' for information on
   other args and return value.  */

static string
try_resolution P4C(const_string, font_name,
                   unsigned, dpi,
                   string *, glyph_paths,
                   kpse_font_file_type *, font_file)
{
  string ret = try_size (font_name, dpi, glyph_paths, font_file);
  
  if (!ret)
    {
      unsigned r;
      unsigned tolerance = KPSE_BITMAP_TOLERANCE (dpi);
      unsigned lower_bound = (int) (dpi - tolerance) < 0 ? 0 : dpi - tolerance;
      unsigned upper_bound = dpi + tolerance;
      
      /* Prefer scaling up to scaling down, since scaling down can omit
         character features (Tom did this in dvips).  */
      for (r = lower_bound; !ret && r <= upper_bound; r++)
        if (r != dpi)
          ret = try_size (font_name, r, glyph_paths, font_file);
    }
  
  return ret;
}

/* Look up FONT_NAME in format FORMAT at DPI in the texfonts.map files
   that we can find, returning the filename found and FONT_FILE.  */

static string
try_fontmap P4C(const_string, font_name,
                unsigned, dpi,
		string *, glyph_paths,
                kpse_font_file_type *, font_file)
{
  static hash_table_type fontmap;
  string *mapped_names;
  string ret = NULL;

  if (fontmap.size == 0)
    { /* If we wanted to complicate our lives, we could handle separate
         maps for GF and PK ones.  I don't see that this has any
         practical utility, though, because if someone wants an alias,
         most likely the alias should apply to non-glyphs as well as
         glyphs (let alone to only GF format or PK format).  */
      const_string map_path = KPSE_GLYPH_PATH ();
      fontmap = map_create (map_path);
    }

  mapped_names = map_lookup (fontmap, font_name);
  if (mapped_names)
    {
      string mapped_name;
      while ((mapped_name = *mapped_names++) && !ret)
        {
          xputenv ("KPATHSEA_NAME", mapped_name);
          ret = try_resolution (mapped_name, dpi, glyph_paths, font_file);
        }
    }

  return ret;
}

/* Look for FONT_NAME in `kpse_fallback_resolutions', omitting DPI if we
   happen across it.  Pass GLYPH_PATHS and FONT_FILE along as usual.
   Assume `kpse_fallback_resolutions' is sorted.  */

static string
try_fallback_resolutions P4C(const_string, font_name,
                             unsigned, dpi,
                             string *, glyph_paths,
                             kpse_font_file_type *, font_file)
{
  unsigned s;
  int loc, max_loc;
  int lower_loc, upper_loc;
  unsigned lower_diff, upper_diff;
  unsigned closest_diff = UINT_MAX;
  string ret = NULL; /* Initialize in case the first fallback resolution
                        is DPI.  */

  /* First find the fallback size closest to DPI.  */
  for (s = 0; kpse_fallback_resolutions[s] != 0; s++)
    {
      unsigned this_diff = abs (kpse_fallback_resolutions[s] - dpi);
      if (this_diff < closest_diff)
        {
          closest_diff = this_diff;
          loc = s;
        }
    }
  max_loc = s;
  lower_loc = loc - 1;
  upper_loc = loc + 1;
  
  for (;;)
    {
      /* Don't bother to try DPI itself again.  */
      if (kpse_fallback_resolutions[loc] != dpi)
        ret = try_resolution (font_name, kpse_fallback_resolutions[loc],
                              glyph_paths, font_file);
      if (ret)
        break;
      
      /* That didn't work. How far away are the locs above or below?  */
      lower_diff = lower_loc > -1
                   ? dpi - kpse_fallback_resolutions[lower_loc] : INT_MAX;
      upper_diff = upper_loc < max_loc
                   ? kpse_fallback_resolutions[upper_loc] - dpi : INT_MAX;
      
      /* But if we're at the end in both directions, quit.  */
      if (lower_diff == INT_MAX && upper_diff == INT_MAX)
        break;
      
      /* Go in whichever direction is closest.  */
      if (lower_diff < upper_diff)
        {
          loc = lower_loc;
          lower_loc--;
        }
      else
        {
          loc = upper_loc;
          upper_loc++;
        }
    }

  return ret;
}

/* See the .h file for description.  This is the entry point.  */

string
kpse_find_glyph_format P4C(const_string, font_name,
                           unsigned, dpi,
                           kpse_file_format_type, format,
                           kpse_font_file_type *, font_file)
{
  string glyph_paths[kpse_any_glyph_format];
  string ret;
  kpse_source_type source;
  
  /* Initialize the path strings for the glyph formats we will try.  */
  glyph_paths[kpse_gf_format]
    = format == kpse_any_glyph_format || format == kpse_gf_format
      ? KPSE_GF_PATH () : NULL;
  
  glyph_paths[kpse_pk_format]
   =  format == kpse_any_glyph_format || format == kpse_pk_format
      ? KPSE_PK_PATH () : NULL;


  /* Start the search: try the name we're given.  */
  source = kpse_source_normal;
  xputenv ("KPATHSEA_NAME", font_name);
  ret = try_resolution (font_name, dpi, glyph_paths, font_file);
  
  /* Sorry for the spaghetti logic.  How to improve?  */
  if (!ret)
    {
      /* Maybe FONT_NAME was an alias.  */
      source = kpse_source_alias;
      ret = try_fontmap (font_name, dpi, glyph_paths, font_file);

      /* OK, maybe we can create it on the fly with MakeTeXPK.  */
      if (!ret)
        {
          source = kpse_source_maketex;
          /* `try_resolution' leaves the envvar set randomly.  */
          xputenv_int ("KPATHSEA_DPI", dpi);
          ret = kpse_make_tex (format, font_name);
        }
       
      /* If MakeTeX... succeeded, set return struct.  (Doesn't make sense for
         `kpse_make_tex' to set it, since it can only succeed or fail,
         unlike the other routines.)  */
      if (ret)
        {
          KPSE_FONT_FILE_DPI (*font_file) = dpi;
          /* Discarding const here.  */
          KPSE_FONT_FILE_NAME (*font_file) = (string) font_name;
        }

      /* If MakeTeX... failed, try any fallback resolutions.  */
      else
        {
          if (kpse_fallback_resolutions)
            ret = try_fallback_resolutions (font_name, dpi, glyph_paths,
                                            font_file);

          /* We're down to the font of last resort.  */
          if (!ret && kpse_fallback_font)
            {
              /* As before, first try it at the given size.  */
              source = kpse_source_fallback;
              xputenv ("KPATHSEA_NAME", kpse_fallback_font);
              ret = try_resolution (kpse_fallback_font, dpi,
                                    glyph_paths, font_file);
              
              /* The fallback font at the fallback resolutions.  */
              if (!ret && kpse_fallback_resolutions)
                ret = try_fallback_resolutions (kpse_fallback_font, dpi,
                                               glyph_paths, font_file);
            }
        }
    }
  
  /* If RET is null, then the caller must not look at FONT_FILE, so it
     doesn't matter if we assign something incorrect to it.  */
  KPSE_FONT_FILE_SOURCE (*font_file) = source;
  
  return ret;
}

#ifdef TEST

void
test_find_glyph (const_string font_name, unsigned dpi)
{
  string answer;
  kpse_font_file_type ret;
  
  printf ("\nSearch for %s@%u:\n\t", font_name, dpi);

  answer = kpse_find_glyph_format (font_name, dpi,
                                   kpse_any_glyph_format, &ret);
  if (answer)
    {
      string format = ret.format == kpse_pk_format ? "pk" : "gf";
      if (!ret.name)
        ret.name = "(null)";
      printf ("%s\n\t(%s@%u, %s)\n", answer, ret.name, ret.dpi, format);
    }
  else
    puts ("(null)");
}


int
main ()
{
  test_find_glyph ("/usr/local/lib/tex/fonts/cm/cmr10", 300); /* absolute */
  test_find_glyph ("cmr10", 300);     /* normal */
  test_find_glyph ("logo10", 300);    /* find gf */
  test_find_glyph ("cmr10", 299);     /* find 300 */
  test_find_glyph ("circle10", 300);  /* in fontmap */
  test_find_glyph ("none", 300);      /* do not find */
  kpse_fallback_font = "cmr10";
  test_find_glyph ("fallback", 300);  /* find fallback font cmr10 */
  kpse_init_fallback_resolutions ("KPATHSEA_TEST_SIZES");
  test_find_glyph ("fallbackdpi", 759); /* find fallback font cmr10@300 */
  
  xputenv ("GFFONTS", ".");
  test_find_glyph ("cmr10", 300);     /* different GFFONTS/TEXFONTS */
  
  return 0;
}

#endif /* TEST */


/*
Local variables:
test-compile-command: "gcc -posix -g -I. -I.. -DTEST tex-glyph.c kpathsea.a"
End:
*/

/* tex-file.h: declarations for all TeX font formats.

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

#ifndef KPATHSEA_TEX_FILE_H
#define KPATHSEA_TEX_FILE_H

#include <kpathsea/c-proto.h>
#include <kpathsea/default.h>
#include <kpathsea/init-path.h>
#include <kpathsea/paths.h>
#include <kpathsea/types.h>


/* If non-NULL, try looking for this if can't find the real font.  */
extern const_string kpse_fallback_font;


/* If non-NULL, check these if can't find (within a few percent of) the
   given resolution.  List must end with a zero element.  */
extern unsigned *kpse_fallback_resolutions;

/* This initializes the fallback resolution list.  If ENVVAR
   is set, it is used; otherwise, the envvar `TEXSIZES' is looked at; if
   that's not set either, a compile-time default is used.  */
extern void kpse_init_fallback_resolutions P1H(string envvar);


/* If non-null, used instead of the usual envvar/path defaults, e.g.,
   set to `getenv ("XDVIFONTS")'.  */
extern string kpse_font_override_path;

/* We put the glyphs first so we don't waste space in an array.  A new
   format here should be accompanied by a new initialization
   abbreviation below and a new entry in `tex-make.c'.  */
typedef enum
{
  kpse_gf_format,
  kpse_pk_format,
  kpse_any_glyph_format, /* ``any'' meaning any of the above */
  kpse_bib_format, 
  kpse_bst_format, 
  kpse_mf_format, 
  kpse_tex_format, 
  kpse_tfm_format, 
  kpse_vf_format
} kpse_file_format_type;


/* Abbreviate what we're going to do a million times.  */
#define KPSE_FONT_PATH(def, envs) (kpse_font_override_path \
  ? kpse_expand_default (kpse_font_override_path, def) \
  : kpse_init_path (NULL, def, envs, NULL))
#define KPSE_NONFONT_PATH(def, envs) kpse_init_path (NULL, def, envs, NULL)

/* Do initialization for the various file formats.  We separate the
   envvar lists so they can be used in other contexts with a different
   default, as in dvipsk (until we have config file support).  */
#define KPSE_BASE_ENVS "MFBASES"
#define KPSE_BASE_PATH() KPSE_NONFONT_PATH (DEFAULT_BASE_PATH, KPSE_BASE_ENVS)

#define KPSE_BIB_ENVS "BIBINPUTS"
#define KPSE_BIB_PATH() KPSE_NONFONT_PATH (DEFAULT_BIB_PATH, KPSE_BIB_ENVS)

#define KPSE_BST_ENVS "BSTINPUTS", "TEXINPUTS"
#define KPSE_BST_PATH() KPSE_NONFONT_PATH (DEFAULT_BST_PATH, KPSE_BST_ENVS)

#define KPSE_FMT_ENVS "TEXFORMATS"
#define KPSE_FMT_PATH() KPSE_NONFONT_PATH (DEFAULT_FMT_PATH, KPSE_FMT_ENVS)

#define KPSE_GF_ENVS "GFFONTS", KPSE_GLYPH_ENVS
#define KPSE_GF_PATH() KPSE_FONT_PATH (DEFAULT_GF_PATH, KPSE_GF_ENVS)

#define KPSE_GLYPH_ENVS "GLYPHFONTS", "TEXFONTS"
#define KPSE_GLYPH_PATH() KPSE_FONT_PATH (DEFAULT_GLYPH_PATH, KPSE_GLYPH_ENVS)

#define KPSE_MF_ENVS "MFINPUTS"
#define KPSE_MF_PATH() KPSE_NONFONT_PATH (DEFAULT_MF_PATH, KPSE_MF_ENVS)

#define KPSE_MFPOOL_ENVS "MFPOOL"
#define KPSE_MFPOOL_PATH() \
  KPSE_NONFONT_PATH (DEFAULT_MFPOOL_PATH, KPSE_MFPOOL_ENVS)

#define KPSE_PK_ENVS "PKFONTS", "TEXPKS", KPSE_GLYPH_ENVS
#define KPSE_PK_PATH() KPSE_FONT_PATH (DEFAULT_PK_PATH, KPSE_PK_ENVS)

#define KPSE_TEX_ENVS "TEXINPUTS"
#define KPSE_TEX_PATH() KPSE_NONFONT_PATH (DEFAULT_TEX_PATH, KPSE_TEX_ENVS)

#define KPSE_TEXPOOL_ENVS "TEXPOOL"
#define KPSE_TEXPOOL_PATH() \
  KPSE_NONFONT_PATH (DEFAULT_TEXPOOL_PATH, KPSE_TEXPOOL_ENVS)

#define KPSE_TFM_ENVS "TFMFONTS", "TEXFONTS"
#define KPSE_TFM_PATH() KPSE_FONT_PATH (DEFAULT_TFM_PATH, KPSE_TFM_ENVS)

#define KPSE_VF_ENVS "VFFONTS", "TEXFONTS"
#define KPSE_VF_PATH() KPSE_FONT_PATH (DEFAULT_VF_PATH, KPSE_VF_ENVS)

/* This type describes a glyph font that we have found.  Maybe it would work
   for all kinds of fonts, and other files.  */

typedef enum
{
  kpse_source_normal,  /* the searched-for font: already existed */
  kpse_source_alias,   /* : was an alias for an existing file */
  kpse_source_maketex, /* : was created on the fly */
  kpse_source_fallback /* : wasn't found, but the fallback font was */
} kpse_source_type;

typedef struct
{
  string name;			/* font name found */
  unsigned dpi;			/* size found, for glyphs */
  kpse_file_format_type format;	/* glyph format found */
  kpse_source_type source;	/* where we found it */
} kpse_font_file_type;		

#define KPSE_FONT_FILE_NAME(f) ((f).name)
#define KPSE_FONT_FILE_DPI(f) ((f).dpi)
#define KPSE_FONT_FILE_FORMAT(f) ((f).format)
#define KPSE_FONT_FILE_SOURCE(f) ((f).source)


/* Concatenate NAME "." EXT and call `kpse_path_search' with the result
   and the other arguments.  */
extern string kpse_find_file P4H(const_string name, const_string ext,
                                 const_string path, boolean must_exist);

/* Here are the file formats we actually look up like this, so far.  We
   do call init_path every time, which is bad.  The saving grace is that
   the actual filesystem lookups are only done once (if at all), because
   we cache the results (per path element). Still not very clean, though.
   
   xdvik uses kpse_find_vf, and dviljk uses kpse_find_tfm. */
#define kpse_find_tfm(name) kpse_find_file (name, "tfm", KPSE_TFM_PATH (),true)
#define kpse_find_vf(name) kpse_find_file (name, "vf", KPSE_VF_PATH (), false)

#endif /* not KPATHSEA_TEX_FILE_H */

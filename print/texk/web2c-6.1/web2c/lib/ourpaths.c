/* ourpaths.c: path searching.  */

#include "config.h"

#include <kpathsea/tex-file.h>
#include <kpathsea/fontmap.h>
#include <kpathsea/pathsearch.h>


/* `path_dirs' is initialized in `setpaths', to a null-terminated array
   of directories to search for.  */
static string path_dirs[LAST_PATH];


/* This sets up the paths, by either copying from an environment variable
   or using the default path, which is defined as a preprocessor symbol
   (with the same name as the environment variable) in `site.h'.  The
   parameter PATH_BITS is a logical or of the paths we need to set.  */

extern void
setpaths (path_bits)
    int path_bits;
{
  if (path_bits & BIBINPUTPATHBIT)
    path_dirs[BIBINPUTPATH] = KPSE_BIB_PATH ();

  if (path_bits & BSTINPUTPATHBIT)
    path_dirs[BSTINPUTPATH] = KPSE_BST_PATH ();

  if (path_bits & GFFILEPATHBIT)
    path_dirs[GFFILEPATH] = KPSE_GF_PATH ();

  if (path_bits & MFBASEPATHBIT)
    path_dirs[MFBASEPATH] = KPSE_BASE_PATH ();

  if (path_bits & MFINPUTPATHBIT)
    path_dirs[MFINPUTPATH] = KPSE_MF_PATH ();

  if (path_bits & MFPOOLPATHBIT)
    path_dirs[MFPOOLPATH] = KPSE_MFPOOL_PATH ();

  if (path_bits & PKFILEPATHBIT)
    path_dirs[PKFILEPATH] = KPSE_PK_PATH ();

  if (path_bits & TEXFORMATPATHBIT)
    path_dirs[TEXFORMATPATH] = KPSE_FMT_PATH ();

  if (path_bits & TEXINPUTPATHBIT)
    path_dirs[TEXINPUTPATH] = KPSE_TEX_PATH ();

  if (path_bits & TEXPOOLPATHBIT)
    path_dirs[TEXPOOLPATH] = KPSE_TEXPOOL_PATH ();

  if (path_bits & TFMFILEPATHBIT)
    path_dirs[TFMFILEPATH] = KPSE_TFM_PATH ();

  if (path_bits & VFFILEPATHBIT)
    path_dirs[VFFILEPATH] = KPSE_VF_PATH ();
}

/* Look for NAME, a Pascal string, in the colon-separated list of
   directories given by `path_dirs[PATH_INDEX]'.  If the search is
   successful, leave the full pathname in NAME (which therefore must
   have enough room for such a pathname), padded with blanks.
   Otherwise, or if NAME is an absolute or relative pathname, just leave
   it alone.  */

boolean
testreadaccess (name, path_index)
    string name;
    int path_index;
{
  string found;  
  string path = path_dirs[path_index];
  
  make_c_string (&name);

  /* Look for it.  Don't use the kpse_find_glyph stuff, since we don't
     have the dpi available separately, and anyway we don't care about
     having pktogf run MakeTeXPK, etc.  */
  found = kpse_path_search (path, name, 1);

  /* If we didn't find it, and we're looking for a font, maybe it's
     an alias defined in a mapping file.  This duplicates most of
     `try_fontmap' in `kpathsea/tex-glyph.c', but the differences are
     substantial enough that it doesn't seem worth combining.  */
  if (!found && (path_index == TFMFILEPATH || path_index == GFFILEPATH
                 || path_index == PKFILEPATH))
    {
      string *mapped_names;
      static hash_table_type fontmap;
      
      /* Fault in the mapping if necessary.  */
      if (fontmap.size == 0)
        fontmap = map_create (path);
      
      /* Now look for our filename in the mapping.  */
      mapped_names = map_lookup (fontmap, name);
      if (mapped_names)
        {
          string mapped_name;
          while ((mapped_name = *mapped_names++) && !found)
            {
              found = kpse_path_search (path, mapped_name, 1);
            }
        }
    }

  /* If we found it somewhere, save it.  */
  if (found)
    strcpy (name, found);
  
  make_pascal_string (&name);
  
  return found != NULL;
}

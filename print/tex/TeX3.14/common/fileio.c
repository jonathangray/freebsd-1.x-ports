/* fileio.c: routines used by TeX, Metafont, and BibTeX.  */

#include "../common/fileio.h"

#ifndef BibTeX
#define EXTERN /* Don't instantiate data here.  */
#ifdef TeX
#include "texd.h"
#else
#include "mfd.h"
#endif
/* This is defined in ./texmf.c.  */
extern void funny_core_dump ();
#endif /* not BibTeX */


#ifdef TeX
/* See comments in ctex.ch for why we need this.  */
extern integer tfmtemp;
#endif


#ifdef BibTeX
/* See comments in bibtex.ch for why we need these.  */
FILE *standardinput = stdin;
FILE *standardoutput = stdout;
#undef FUNNY_CORE_DUMP
/* Because we don't generate a .h file with all the global definitions
   for BibTeX, as we do with TeX and Metafont, we must declare all these
   variables.  */
extern char nameoffile[];
extern integer namelength;
#endif



/* This constant is used in BibTeX, when opening the top-level .aux file.  */
#define NO_FILE_PATH -1

/* Open an input file F, using the path PATHSPEC (the values are defined
   in ./extra.h).  The filename is in `nameoffile', as a Pascal string.
   We return whether or not the open succeeded.  If it did, we also set
   `namelength' to the length of the full pathname that we opened.  */

boolean
open_input (f, path_index)
  FILE **f;
  int path_index;
{
#ifdef FUNNY_CORE_DUMP
  /* This only applies if a preloaded TeX (or Metafont) is being made;
     it allows for automatic creation of the core dump (typing ^\
     requires manual intervention).  */
  if (path_index == TEXINPUTPATH
      && strncmp (nameoffile+1, "HackyInputFileNameForCoreDump.tex", 33) == 0)
    funny_core_dump ();
#endif /* FUNNY_CORE_DUMP */

#ifdef BibTeX
  if (path_index == NO_FILE_PATH)
    {
      unsigned temp_length;

      /* We can't use `make_c_string' or `make_pascal_string', since
         `nameoffile' is an array, not a pointer.  */
      null_terminate (nameoffile + 1);
      *f = fopen (nameoffile + 1, "r");
      temp_length = strlen (nameoffile + 1);
      space_terminate (nameoffile + 1);
      if (*f != NULL)
        {
          namelength = temp_length;
          return true;
        }
      else
        return false;
    }
#endif /* BibTeX */

  if (testreadaccess (nameoffile, path_index))
    {
      *f = checked_fopen (nameoffile, "r");
      null_terminate (nameoffile + 1);
      
      /* If we found the file in the current directory, don't leave the
         `./' at the beginning of `nameoffile', since it looks dumb when
         TeX says `(./foo.tex ... )', and analogously for Metafont.  */
      if (nameoffile[1] == '.' && nameoffile[2] == '/')
        {
          unsigned i = 1;
          while (nameoffile[i] != 0)
            {
              nameoffile[i] = nameoffile[i+2];
              i++;
            }
        }

      /* We've got the real name in `nameoffile' now, so it's time to
         get the real length in `namelength'.  */
      namelength = strlen (nameoffile + 1);
      
      /* At the moment, `nameoffile' is in an anomalous state: it still
         begins with a space, but now it is terminated with a null.  So
         we need to terminate it with a space.  */
      space_terminate (nameoffile);
      
#ifdef TeX
      /* If we just opened a TFM file, we have to read the first byte,
         since TeX wants to look at it.  */
      if (path_index == TFMFILEPATH)
        tfmtemp = getc (*f);
#endif /* TeX */

      return true;
    }
  else
    return false;
}


/* Open an output file F either in the current directory or in
   $TEXMFOUTPUT/F, if the environment variable `TEXMFOUTPUT' exists.
   (Actually, this applies to the BibTeX output files, also, but
   `TEXMFBIBOUTPUT' was just too long.)  The filename is in the global
   `nameoffile', as a Pascal string.  We return whether or not the open
   succeeded.  If it did, the global `namelength' is set to the length
   of the actual filename.  */

boolean
open_output (f)
  FILE **f;
{
  unsigned temp_length;

  /* We can't use `checked_fopen' here, since that routine aborts if the
     file can't be opened.  We also can't use `make_c_string' or
     `make_pascal_string', since `nameoffile' is an array, not a
     pointer.  */
  null_terminate (nameoffile + 1);
  *f = fopen (nameoffile + 1, "w");
  
  if (*f == NULL)
    {
      /* Can't open in the current directory.  Try the directory
         specified by the environment variable.  */
      char *temp_dir = getenv ("TEXMFOUTPUT");

      if (temp_dir != NULL && chdir (temp_dir) == 0)
        *f = fopen (nameoffile + 1, "w");
    }

  temp_length = strlen (nameoffile + 1);
  space_terminate (nameoffile + 1);

  if (*f != NULL)
    {
      namelength = temp_length;
      return true;
    }
  
  else
    return false;
}

/*
 *   The search routine takes a directory list, separated by PATHSEP, and
 *   tries to open a file.  Null directory components indicate current
 *   directory. if the file SUBDIR exists and the file is a font file,
 *   it checks for the file in a subdirectory named the same as the font name.
 *   Returns the open file descriptor if ok, else NULL.
 */
#include "dvips.h" /* The copyright notice in that file is included too! */
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-glyph.h>
/*
 *
 *   We hope MAXPATHLEN is enough -- only rudimentary checking is done!
 */

#ifdef DEBUG
extern integer debug_flag;
#endif  /* DEBUG */
extern char *mfmode ;
extern int actualdpi ;
string realnameoffile ;

FILE *
search(path, file, mode)
    char *path, *file, *mode ;
{
  FILE *ret;
  string name;
  extern char *vfpath;

#ifdef SECURE
  /* This change suggested by maj@cl.cam.ac.uk to disallow reading of
     arbitrary files.  */
  if (secure && kpse_absolute_p (file)) return NULL;
#endif
   /* Most file looked for through here must exist -- the exception is
      VF's. Bitmap fonts go through pksearch. */
   name = kpse_path_search (path, file, path != vfpath);

  if (name)
    {
      ret = fopen (name, mode);
      if (!ret)
        FATAL_PERROR (name);
      realnameoffile = name;
    }
  else
    ret = NULL;

  return ret;
}               /* end search */

FILE *
pksearch(path, file, mode, dpi, name_ret, dpi_ret)
        char *path, *file, *mode ;
	char **name_ret ;
	halfword dpi;
	int *dpi_ret ;
{
  FILE *ret;
  kpse_font_file_type font_file;
  string name = kpse_find_pk (file, dpi, &font_file);
  
  if (name)
    {
      ret = fopen (name, mode);
      if (!ret)
        FATAL_PERROR (name);
      realnameoffile = name;
      *name_ret = KPSE_FONT_FILE_NAME (font_file);
      *dpi_ret = KPSE_FONT_FILE_DPI (font_file);
    }
  else
    ret = NULL;

  return ret;
}               /* end search */

/* do we report file openings? */

#ifdef DEBUG
#  ifdef fopen
#    undef fopen
#  endif
#  ifdef VMCMS  /* IBM: VM/CMS */
#    define fopen cmsfopen
#  endif /* IBM: VM/CMS */
FILE *my_real_fopen(n, t)
register char *n, *t ;
{
   FILE *tf ;
   if (dd(D_FILES)) {
      fprintf(stderr, "<%s(%s)> ", n, t) ;
      tf = fopen(n, t) ;
      if (tf == 0)
         fprintf(stderr, "failed\n") ;
      else
         fprintf(stderr, "succeeded\n") ;
   } else
      tf = fopen(n, t) ;
#ifdef OS2
   if (tf == (FILE *)NULL)
     tf = fat_fopen(n, t); /* try again with filename truncated to 8.3 */
#endif
   return tf ;
}
#endif

#ifdef OS2
/* truncate filename at end of fname to FAT filesystem 8.3 limit */
/* if truncated, return fopen() with new name */
FILE *fat_fopen(fname, t)
char *fname, *t;
{
   char *np;	/* pointer to name within path */
   char nbuf[13], *ns, *nd;
   char n[MAXPATHLEN];
   int ni, ne;
   FILE *tf;
   strcpy(n, fname);
   for (ns=n; *ns; ns++) {
      if (*ns=='/')
         *ns=DIRSEP;
   }
   np = strrchr(n,DIRSEP);
   if (np==(char *)NULL)
      np = n;
   else
      np++;
   /* fail if it contains more than one '.' */
   ni = 0;
   for (ns=np; *ns; ns++) {
      if (*ns=='.')
         ni++;
   }
   if (ni>1)
      return (FILE *)NULL;
   /* copy it to nbuf, truncating to 8.3 */
   ns = np;
   nd = nbuf;
   ni = 0;
   while ((*ns!='.') && (*ns) && (ni<8)) {
      *nd++ = *ns++;
      ni++;
   }
   while ((*ns!='.') && (*ns)) {
      ns++;
      ni++;
   }
   ne = 0;
   if (*ns=='.') {
      *nd++ = *ns++;
      while ((*ns!='.') && (*ns) && (ne<3)) {
         *nd++ = *ns++;
         ne++;
      }
      while (*ns) {
         ns++;
         ne++;
      }
   }
   *nd++='\0';
   if ((ni>8) || (ne>3)) {
      strcpy(np,nbuf);
      /* now code copied from my_real_fopen() */
      if (dd(D_FILES)) {
         fprintf(stderr, "<%s(%s)> ", n, t) ;
         tf = fopen(n, t) ;
         if (tf == 0)
            fprintf(stderr, "failed\n") ;
         else
            fprintf(stderr, "succeeded\n") ;
      }
      else
         tf = fopen(n, t) ;
      return tf;
   }
   return (FILE *)NULL;
}
#endif

/* open.c 
	vi:se ts=3 sw=3:
 */

/* Magic open file: path lookup and transparent decompression */

/* $Id: open.c,v 1.1 1994/02/19 16:03:08 ache Exp $ 
 * $Log: open.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.1  1994/01/12  16:11:07  espie
 * Last minute changes.
 *
 * Revision 4.0  1994/01/11  17:50:46  espie
 * A little more abstract, should work better
 *
 * Revision 1.6  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.5  1994/01/08  19:43:57  Espie
 * Better amiga patterns.
 *
 * Revision 1.4  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1994/01/05  13:50:43  Espie
 * Cosmetic change.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.12  1993/12/04  16:12:50  espie
 * New semantics.
 *
 * Revision 3.11  1993/12/02  15:45:33  espie
 * Simpler.
 *
 * Revision 3.10  1993/11/27  17:29:53  espie
 * Suppressed stupid abstraction.
 *
 * Revision 3.9  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.8  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.7  1993/08/17  16:53:09  espie
 * New gzip suffix.
 *
 * Revision 3.3  1993/07/14  16:33:41  espie
 * Added gzip/shorten.
 *
 * Revision 3.2  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 1.5  1992/11/01  13:10:06  espie
 * Cleaned up path handler, and some more bugs.
 * Check for size now.
 * Added path support. Transparent interface. We look up through the file
 * list, which is small anyway.
 */

#include "defs.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#else
#include <stdlib.h>
#endif

#include "extern.h"

ID("$Id: open.c,v 1.1 1994/02/19 16:03:08 ache Exp $")

extern int error;

struct exfile
   {
   FILE *handle;
   void (*close)P((struct exfile *f));
   void (*rewind)P((struct exfile *f));
   int (*getcar)P((struct exfile *f));
   int (*tell)P((struct exfile *f));
   int pos;
   };

LOCAL int do_getchar(f)
struct exfile *f;
   {
   int c;

   if ((c = fgetc(f->handle)) == EOF)
      error = FILE_TOO_SHORT;
   else
      f->pos++;
   return c;
   }

LOCAL int do_tell(f)
struct exfile *f;
   {
   return f->pos;
   }

LOCAL void do_pclose(f)
struct exfile *f;
   {
   pclose(f->handle);
   }

LOCAL void do_fclose(f)
struct exfile *f;
   {
   fclose(f->handle);
   }

   
/* compression methods we do know about.
 * Important restriction: for the time being, the output
 * must be a single module.
 */

LOCAL struct compression_method
   {
   char *extension;
   char *command;
   } comp_table[] =
   {
   ".czip", "unczip %s",
   ".gz",   "gzip -dc %s",
#ifdef GZIP
   ".z", "gzip -dc %s",
#else
    ".Z",   "zcat %s",
#endif
   ".s", "shorten -x %s -",
   ".shn",  "shorten -x %s -",
   ".zoo", "zoo xpq %s",
#ifdef AMIGA
   ".lzh", "lha -q p \"%s\"",
   ".lha", "lha -q p \"%s\"",
#else
   ".lzh", "lha pq %s",
   ".lha", "lha pq %s",
#endif
   ".zip", "unzip -pq %s",
   ".arc", "arc pn %s",
   NULL,   NULL
   };

/***
 *
 *  Handling extensions.
 *
 ***/

LOCAL boolean check_ext(s, ext)
char *s, *ext;
   {
   int ext_len, s_len;
   char *c;

   ext_len = strlen(ext);
   s_len = strlen(s);
   if (s_len < ext_len)
      return FALSE;
   for (c = s + s_len - ext_len; *c; c++, ext++)
      if (tolower(*c) != tolower(*ext))
         return FALSE;
   return TRUE;
   }

LOCAL boolean exist_file(fname)
char *fname;
   {
   FILE *temp;

   temp = fopen(fname, "r");
   if (temp)
      {
      fclose(temp);
      return TRUE;
      }
   else
      return FALSE;
   }

#ifndef MAXPATHLEN
#define MAXPATHLEN 350
#endif

LOCAL char *find_file(fname, path)
char *fname;
char *path;
   {
   char *sep;
   static char buffer[MAXPATHLEN];
   int len;

      /* first, check the current directory */
   if (exist_file(fname))
      return fname;
   while(path)
      {
      sep = strchr(path, ':');
      if (sep)
         len = sep - path;
      else
         len = strlen(path);
      if (len < MAXPATHLEN)
         {
         strncpy(buffer, path, len);
         buffer[len] = '/';
         if (len + strlen(fname) < MAXPATHLEN - 5)
            {
            strcpy(buffer + len + 1, fname);
            puts(buffer);
            if (exist_file(buffer))
               return buffer;
            }
         }
      if (sep)
         path = sep + 1;
      else
         return NULL;
      }
   return NULL;
   }

FILE *file_handle(f)
struct exfile *f;
   {
   return f->handle;
   }

int getc_file(f)
struct exfile *f;
   {
   return (*f->getcar)(f);
   }

int tell_file(f)
struct exfile *f;
   {
   return (*f->tell)(f);
   }

struct exfile *open_file(fname, mode, path)
char *fname;
char *mode; /* right now, only mode "r" is supported */
char *path; 
   {
   struct exfile *new;
   struct compression_method *comp;

   if (mode[0] != 'r' || mode[1] != 0)
      return NULL;
    
   new = (struct exfile *)malloc(sizeof(struct exfile));
   if (!new)
      return NULL;

   new->getcar = do_getchar;
   new->tell = do_tell;
   new->pos = 0;
   fname = find_file(fname, path);
   if (!fname)
      return NULL;
   for (comp = comp_table; comp->extension; comp++)
      if (check_ext(fname, comp->extension))
         {
         char pipe[MAXPATHLEN + 25];

         sprintf(pipe, comp->command, fname);
         new->close = do_pclose;
         if (new->handle = popen(pipe, "r"))
            return new;
         else
            {
            free(new);
            return NULL;
            }
         }
   new->close = do_fclose;
   if (new->handle = fopen(fname, "r"))
      return new;
   else
      {
      free(new);
      return NULL;
      }
   }


void close_file(file)
struct exfile *file;
   {
   (*file->close)(file);
   free(file);
   }


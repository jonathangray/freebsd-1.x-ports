/* lookup.c 
	vi:se ts=3 sw=3:
 */

/* A simple database lookup.
 * every entry in the database files is of the form: initial string value
 * We just have to match the initial string, no separators required.
 * Not that the specification just asks for a matching entry, not
 * necessarily the first or the last.
 */
/* $Id: lookup.c,v 1.1 1994/02/19 16:03:08 ache Exp $ */

/* $Log: lookup.c,v $
/* Revision 1.1  1994/02/19 16:03:08  ache
/* Initial revision
/*
 * Revision 4.0  1994/01/11  17:49:04  espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.2  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 1.2  1992/07/22  14:50:25  espie
 * open_file changed, so lookup had to change to incorporate paths as well.
 *
 * Revision 1.1  1992/07/16  17:02:00  espie
 * Initial revision
 *
 *
 */

#include <stdio.h>
#include <string.h>
#include <malloc.h>

#include "defs.h"
#include "extern.h"

ID("$Id: lookup.c,v 1.1 1994/02/19 16:03:08 ache Exp $")

/* the lookup structure is PRIVATE */

#define HANDLE 0    /* a null type, for memory handling */
#define FNAME 1     /* a filename, which has not yet been opened */
#define FOPENED 2   /* a file, once opened */
#define STRING 3    /* an already accounted for string */

struct lookup
   {
   int type;
   union 
      {
      char *filename;
      FILE *file;
      char *string;
      char *path;
      } value;
   struct lookup *next;
   };

/***
 *
 * lookup memory handling 
 *
 ***/

/* link_lookup(base, new): link a new element inside the existing chain */
LOCAL void link_lookup(handle, new)
struct lookup *handle, *new;
   {
   new->next = handle->next;
   handle->next = new;
   }

LOCAL void set_lookup(handle, path)
struct lookup *handle;
char *path;
   {
   handle->type = HANDLE;
   handle->value.path = path;
   }

/* new = create_lookup(): create an empty element, 
 * chain anchor or to be filled 
 */
struct lookup *create_lookup(path)
char *path;
   {
   struct lookup *new;

   new = (struct lookup *)malloc(sizeof(struct lookup));
   if (!new)
      exit(10);
   set_lookup(path);
   new->next = NULL;
   return new;
   }

/* free_lookup(handle): free the whole chain. Note that filenames
 * do not belong to us
 */
void free_lookup(handle)
struct lookup *handle;
   {
   struct lookup *to_free;

   while(handle)
      {
      to_free = handle;
      handle = handle->next;
      switch(to_free->type)
         {
      case HANDLE:
      case FNAME: /* filenames don't belong to you */
         break;
      case FOPENED:
         close_file(to_free->value.file.fhandle, 
            to_free->value.file.filetype);
         break;
      case STRING:
         free(to_free->value.string);
         break;
         }
      free(to_free);
      }
   }

/* add_lookup(handle, filename): add a new potential filename to the lookup
 * handle
 */
void add_lookup(handle, filename)
struct lookup *handle;
char *filename;
   {
   struct lookup *new;

   new = create_lookup();
   new->type = FNAME;
   new->value.filename = filename;
   link_lookup(handle, new);
   }


/* postfix = check_prefix(template, s):
 * if s is a prefix of template, return the remaining part of template,
 * else return NULL.
 * Note difference between NULL result and empty postfix (pointer to NULL)
 */
static char *check_prefix(template, s)
char *template;
char *s;
   {
   for (; *s; template++, s++)
      /* *s != 0 at that point */
      if (*template != *s)
         return NULL;
   while(*template == ' ' || *template == '\t')
      template++;
   return template;
   }

/* copy = create_copy(s): allocate memory and create a copy of string s.
 * get rid of spurious \n at the end.
 */
static char *create_copy(s)
char *s;
   {
   char *new;
   int len;

   len = strlen(s);
   while(s[len - 1] == '\n')
      s[--len] = 0;
   new = malloc(len + 1);
   if (!new)
      exit(10);
   return strcpy(new, s);
   }

#define BUFSIZE 1500

LOCAL char *internal_lookup();

/* postfix = lookup(handle, s): lookup string s in the database indexed by
 * handle.
 */
char *lookup(handle, s)
struct lookup *handle;
char *s;
   {
      /* the internal handler also maintains a current path */
   return internal_lookup(handle, s, NULL);
   }


LOCAL char *internal_lookup(handle, s, path)
struct lookup *handle;
char *s;
char *path;
   {
   char *r;
   static char buffer[BUFSIZE];
   struct lookup *new;

   if (handle)
      {
      switch(handle->type)
         {
      case STRING:
         r = check_string(handle->value.string, s);
         if (r)
            return r;
         else
            return internal_lookup(handle->next, s, path);
      case HANDLE:
         return internal_lookup(handle->next, s,
            handle->value.path ? handle->value.path : path);
      case FNAME:
         r = internal_lookup(handle->next, s, path);
         if (r)
            return r;
         else
            {
            if (handle->value.file = 
               open_file(handle->value.filename, "r", path))
               {
               handle->type = FOPENED;
               return internal_lookup(handle, s, path);
               }
            else
               {
               set_lookup(handle, NULL);
               return NULL;
               }
            }
      case FOPENED:
         r = internal_lookup(handle->next, s, path);
         if (r)
            return r;
         else
            {
            if (fgets(buffer, BUFSIZE, handle->value.file))
               {
               new = create_lookup(NULL);
               new->type = STRING;
               new->value.string = create_copy(buffer);
               link_lookup(handle, new);
               r = check_prefix(new->value.string, s);
               if (r)
                  return r;
               else
                  return internal_lookup(handle, s, path);
               }
            else
               {
               close_file(handle->value.file);
               set_lookup(new, NULL);
               }
            }
         }
      }
   else
      return NULL;
   }
            

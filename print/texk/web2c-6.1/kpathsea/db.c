/* db.c: an external database to avoid filesystem lookups.

Copyright (C) 1994 Karl Berry.

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
#include <kpathsea/c-fopen.h>
#include <kpathsea/c-pathch.h>
#include <kpathsea/debug.h>
#include <kpathsea/db.h>
#include <kpathsea/hash.h>
#include <kpathsea/line.h>
#include <kpathsea/readable.h>
#include <kpathsea/str-list.h>


/* If no DB_FILE, return false (maybe they aren't using this feature).
   Otherwise, build the db and return true.  */

static boolean
db_build P1C(hash_table_type *, table)
{
  string line;
  string cur_dir = NULL; /* First thing in ls-R might be a filename.  */
  string db_filename = concat3 (DB_DIR, DIR_SEP_STRING, DB_NAME);
  FILE *db_file = fopen (db_filename, FOPEN_R_MODE);
  
  if (db_file)
    {
      while ((line = read_line (db_file)) != NULL)
        {
          unsigned len = strlen (line);
          if (line[0] == DIR_SEP && line[len - 1] == ':') /* /...: = new dir */
            {
              cur_dir = xstrdup (line);
              cur_dir[len - 1] = DIR_SEP;
            }
          else if (line[0] != 0 && cur_dir) /* other nonblank line */
            { /* New hash table entry with a key of `line' and a data of
                 `cur_dir'.  Already-existing identical keys are ok, since
                 a file named `foo' can be in more than one directory.
                 Since it doesn't hurt, share the directory name string
                 among all the files in the directory. */
              hash_insert (table, xstrdup (line), cur_dir);
            }
          /* else ignore blank lines */

          free (line);
        }
      xfclose (db_file, db_filename);

#ifdef DEBUG
      if (DEBUG_P (DEBUG_DB_BUILD))
        hash_print (*table);
#endif
    }

  free (db_filename);
  return db_file != NULL;
}

/* Return true if FILENAME could be in PATH_ELT, i.e., if the directory
   part of FILENAME matches PATH_ELT.  Have to consider // wildcards, but
   $ and ~ expansion have already been done.  */
     
static boolean
match P2C(const_string, filename,  const_string, path_elt)
{
  return true; /* Punt for now.  */
}

/* Don't bother implementing a search path for the database itself, or
   multiple databases, until people ask.  */

str_list_type *
kpse_db_search P3C(const_string, name,  const_string, path_elt,  boolean, all)
{
  static hash_table_type db;
  string *db_dirs, *orig_dirs;
  boolean done;
  str_list_type *ret;
  
  /* Hash up the database if this is the first call.  */
  if (db.size == 0)
    {
      db = hash_create (7603); /* What the heck, sparse is ok.  */
      if (!db_build (&db))
        { /* If db can't be built, leave `size' alone (so we don't
             rebuild it), but clear `buckets' (so we don't look in it).  */
          free (db.buckets);
          db.buckets = NULL;
        }
    }
  
  /* If we failed to build the database, quit.  */
  if (db.buckets == NULL)
    return NULL;
  
  /* We have a db.  Look up NAME.  */
  orig_dirs = db_dirs = hash_lookup (db, name);

  done = false;
  ret = XTALLOC1 (str_list_type);
  *ret = str_list_init ();
  
  /* For each filename found, see if it matches the path element.  For
     example, if we have ../cx/cmr10.300pk and .../ricoh/cmr10.300pk,
     and the path looks like .../cx, we don't want the ricoh file.  */
  while (!done && db_dirs && *db_dirs)
    {
      string db_file = concat (*db_dirs, name);
      
      if (match (db_file, path_elt) && kpse_readable_file (db_file))
        {
          str_list_add (ret, db_file);
          if (!all) done = true;
        }
      else
        free (db_file);
      
      /* On to the next directory, if any.  */
      db_dirs++;
    }
  
  /* This is just the space for the pointers, not the strings.  */
  if (orig_dirs && *orig_dirs) free (orig_dirs);
  
  return ret;
}

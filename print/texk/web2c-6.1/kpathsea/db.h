/* db.h: lookups in an externally built db file.

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

#ifndef KPATHSEA_DB_H
#define KPATHSEA_DB_H

#include <kpathsea/c-proto.h>
#include <kpathsea/types.h>
#include <kpathsea/str-list.h>

/* It's not an error if this doesn't exist; we just go ahead and search
   the actual directories.  */
#ifndef DB_DIR
#define DB_DIR "/usr/local/lib/texmf"
#endif
#ifndef DB_NAME
#define DB_NAME "ls-R"
#endif

/* Return list of matches for NAME in the external db file that
   match PATH.  If ALL is set, return all matches, else the first.
   
   If there are no matches, returns a pointer to an empty list.
   If the database can't be located, returns NULL.  */
extern str_list_type *kpse_db_search P3H(const_string name, 
                                        const_string path, boolean all);

#endif /* not KPATHSEA_DB_H */

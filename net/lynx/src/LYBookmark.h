
#ifndef LYBookmark_H
#define LYBookmark_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern int get_bookmark_filename PARAMS((char **name));
extern void save_bookmark_link PARAMS((document *doc));

#endif /* LYBookmark_H */



#ifndef LYHistory_H
#define LYHistory_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern void push PARAMS((document *doc));
extern void pop PARAMS((document *doc));
extern void pop_num PARAMS((int number, document *doc));
extern void showhistory PARAMS((char **newfile));
extern void historytarget PARAMS((document *newdoc));

#define HISTORY_PAGE_TITLE  "Lynx History Page"

#endif /* LYHistory_H */

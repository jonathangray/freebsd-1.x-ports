
#ifndef LYSearch_H
#define LYSearch_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern void textsearch PARAMS((document *cur_doc, char *prev_target));

#define IN_FILE 1
#define IN_LINKS 2

#ifndef NOT_FOUND
#define NOT_FOUND 0
#endif


#endif /* LYSearch_H */

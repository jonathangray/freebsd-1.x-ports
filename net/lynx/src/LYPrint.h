
#ifndef LYPrint_H
#define LYPrint_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern int printfile PARAMS((document *newdoc));
extern int print_options PARAMS((char **newfile, int lines_in_file));

#define PRINT_OPTIONS_TITLE "Lynx Printing Options"

#endif /* LYPrint_H */


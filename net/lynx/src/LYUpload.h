
#ifndef LYUpload_H
#define LYUpload_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern void LYUpload PARAMS((char *line));
extern int LYUpload_options PARAMS((char **newfile, char *directory));

#define UPLOAD_OPTIONS_TITLE "Lynx Upload Options"

#endif /* LYUpload_H */



#ifndef LYDownload_H
#define LYDownload_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern void LYDownload PARAMS((char *line));
extern int LYdownload_options PARAMS((char **newfile, char *data_file));

#define DOWNLOAD_OPTIONS_TITLE "Lynx Download Options"

#endif /* LYDownload_H */


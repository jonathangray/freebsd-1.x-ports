/* config.h: master configuration file, included first by all compilable
   source files (not headers).  */

#ifndef CONFIG_H
#define CONFIG_H

/* The stuff from the path searching library.  */
#include <kpathsea/config.h>

/* How to open files with fopen.  */
#include <kpathsea/c-fopen.h>

#if defined (DOS) || defined (MSDOS)
#undef DOS
#undef MSDOS
#define DOS
#define MSDOS
#endif

#define READ FOPEN_R_MODE
#define READBIN FOPEN_RBIN_MODE
#define WRITEBIN FOPEN_WBIN_MODE

/* Include debugging by default.  */
#ifndef NO_DEBUG
#undef DEBUG
#define DEBUG
#endif

#endif /* not CONFIG_H */

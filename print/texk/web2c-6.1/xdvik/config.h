/* config.h: master configuration file, included first by all compilable
   source files (not headers).  */

#ifndef CONFIG_H
#define CONFIG_H

/* The stuff from the path searching library.  */
#include <kpathsea/config.h>

#include <setjmp.h>

#ifndef HAVE_VPRINTF
#ifdef HAVE_DOPRNT
#define	vfprintf(stream, message, args)	_doprnt(message, args, stream)
/* If we have neither, should fall back to fprintf with fixed args.  */
#endif
#endif

/* Some definitions of our own.  */
#include "xdvi.h"

#endif /* not CONFIG_H */

#ifndef ALLOC_H
#define ALLOC_H

#ifndef notdef
/* stdlib.h declares malloc, realloc */
extern char *emalloc(), *nemalloc(), *strsave(), *str3save();
#else				/* notdef */
/* setup for UT debugging malloc */
#define MALLOC_TRACE
#define TRACE
#include "malloc.h"		/* defines CSRIMALLOC */
extern char *_nemalloc(), *_strsave(), *_str3save();
#define nemalloc(x)		_nemalloc((x), __FILE__, __LINE__)
#define strsave(x)		_strsave((x), __FILE__, __LINE__)	/* TODO: conflict with malloc.h; fix */
#define str3save(x, y, z)	_str3save((x), y, z, __FILE__, __LINE__)
#endif				/* notdef */

#endif				/* ALLOC_H */

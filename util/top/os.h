#include <sys/types.h>
#include <sys/param.h>	/* This defines BSD */
#if defined(BSD) && !defined(BSD4_4)
# include <stdio.h>
# include <strings.h>
# define strchr(a, b)		index((a), (b))
# define strrchr(a, b)		rindex((a), (b))
# define memcpy(a, b, c)	bcopy((b), (a), (c))
# define memzero(a, b)		bzero((a), (b))
# define memcmp(a, b, c)	bcmp((a), (b), (c))
  typedef int sigret_t;
#else 
# include <stdio.h>
# define setbuffer(f, b, s)	setvbuf((f), (b), (b) ? _IOFBF : _IONBF, (s))
# include <string.h>
# include <memory.h>
# define memzero(a, b)		memset((a), 0, (b))
  typedef void sigret_t;
#endif

/*
 * Empirical evidence suggests that Unix systems with TMP_MAX defined (in
 * stdio.h) also have a strerror function in libc.  This next section depends
 * on this always being true.  If you discover that this is not true, please
 * let the author know!
 */
#ifndef TMP_MAX
#define strerror(err) (sys_errlist[err])
#endif

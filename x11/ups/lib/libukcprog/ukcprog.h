/* ukcprog.h -- Declarations for UKC programmers' library routines. */

/* @(#)ukcprog.h	1.18 17/9/92 UKC */

#ifndef UKCPROG_H_DEFINED
#define UKCPROG_H_DEFINED

#ifdef __STDC__
#ifndef PROTO
#define PROTO(a)	a
#endif	/* !PROTO */

typedef void *voidptr;

#include <stddef.h>	/* needed for size_t */

#else /* !__STDC__ */

#include <sys/types.h>	/* size_t for old C */

#ifndef PROTO
#define PROTO(a)	()
#endif	/* !PROTO */

/*  Patch up for things that are missing without ANSI C. */
#ifndef const
#define const
#endif
#ifndef volatile
#define volatile
#endif
#ifndef signed
#define signed
#endif

/*  Pragmatically easiest solution to the problem of missing
 *  function declarations.
 */
#if !(defined(ultrix) && defined(_SIZE_T_))
char *memcpy(), *memmove(), *memchr(), *memset();
#endif

typedef char *voidptr;

#endif /* !__STDC__ */

/*  Defining boolean type.  This might cause problems for some ...  */
#ifndef FALSE
#define FALSE	0
#endif
#ifndef TRUE
#define TRUE	1
#endif
#ifndef bool
#define bool int
#endif

/*  Macro to get control characters (works for ASCII only).  */
#define CONTROL(c)	((c) & 0x1f)

/*  Define NULL - this avoids having the whole of stdio.h */
#ifndef NULL
#define NULL 0
#endif

/*  Macro to concatenate two or three names.  */
#undef CAT
#ifdef __STDC__
#define CAT(a,b)	a ## b
#define CAT3(a,b,c)	a ## b ## c
#else
#define _IDENT(a) a
#define CAT(a,b) _IDENT(a)b
#define CAT3(a,b,c) CAT(a,b)c
#endif /* !__STDC__ */

/* from panic.c */
typedef void (*panic_handler_t) PROTO((const char *message));

panic_handler_t install_panic_handler PROTO((panic_handler_t handler));
void panic PROTO((const char *message));


/* from e_malloc.c */
voidptr e_malloc PROTO((size_t size));


/* from e_realloc.c */
voidptr e_realloc PROTO((voidptr old, size_t size));


/* from strsave.c */
char *strsave PROTO((const char *s));


/* from config.c */
char *config_trim_line PROTO((char *line));


#ifdef UKC_GCC_HAS_SYSLOG_ATTR
#define FORMF_ARGS(fpos, argpos)	__attribute__((format(syslog, fpos, argpos)))
#else
#define FORMF_ARGS(fpos, argpos)	/* nothing */
#endif

/* from formf.c */
#ifdef __STDC__
#include <stdarg.h>	/* nasty, but needed for prototype */
#endif

char *formf PROTO((char *buffer_space, int buffer_size,
			const char *format, va_list args)) FORMF_ARGS(3, 4);


/* from errf.c */
typedef void (*errf_ofunc_t) PROTO((const char *string));

errf_ofunc_t errf_set_ofunc PROTO((errf_ofunc_t func));
const char *errf_set_prefix PROTO((const char *prefix));
const char *errf_get_prefix PROTO((void));
void errf_set_progname PROTO((const char *progname));
const char *errf_get_progname PROTO((void));
void errf_usage PROTO((const char *usage));

void errf PROTO((const char *fmt, ...)) FORMF_ARGS(1, 2);
char *strf PROTO((const char *fmt, ...)) FORMF_ARGS(1, 2);
void strnf PROTO((char *buf, int bufsize, const char *fmt, ...))
							FORMF_ARGS(3, 4);


/* from fpgetline.c
 *
 * Only include this prototype if stdio.h has been #included already.
 * This is to mandating the inclusion of stdio.h unless fpgetline()
 * is required.
 */
#ifdef EOF
char *fpgetline PROTO((FILE *fp));
#endif


/* from alloc.c */

/*  Opaque type for alloc pool ids and marks.
 */
typedef struct { int dummy; } *alloc_id_t;
typedef struct { int dummy; } *alloc_mark_id_t;

alloc_id_t alloc_create_pool PROTO((void));
void alloc_free_pool PROTO((alloc_id_t alloc_id));
void alloc_reset_pool PROTO((alloc_id_t alloc_id));

/*  Various forms of allocation.  alloc() aligns like malloc,
 *  allocstr() doesn't.  alloc_strdup() is like strdup() but
 *  used allocstr() rather than malloc().  All of the preceding
 *  calls panic if they run out memory.  alloc_ck() and allocstr_ck()
 *  are like alloc() and allocstr() except that they return NULL
 *  rather than panicking if memory runs out.
 */
voidptr alloc PROTO((alloc_id_t alloc_id, int nbytes));
char *allocstr PROTO((alloc_id_t alloc_id, int nbytes));
voidptr alloc_ck PROTO((alloc_id_t alloc_id, int nbytes));
char *allocstr_ck PROTO((alloc_id_t alloc_id, int nbytes));
char *alloc_strdup PROTO((alloc_id_t alloc_id, const char *s));

/*  Control - set and clear debug flags both globally and per-pool.
 *  If the debug flag is set new memory is initialised to garbage
 *  and set to (different) garbage when a pool is freed.
 */
bool alloc_set_default_debug_flag PROTO((bool val));
bool alloc_set_debug_flag PROTO((alloc_id_t alloc_id, bool val));

/*  alloc_mark() returns an alloc_mark_id that represents the current
 *  state of the alloc pool.  alloc_release() releases any memory
 *  allocated since the alloc_mark() call.
 */
alloc_mark_id_t alloc_mark PROTO((alloc_id_t alloc_id));
void alloc_release PROTO((alloc_id_t alloc_id, alloc_mark_id_t alloc_mark_id));


/* from ssplit.c */

char **ssplit PROTO((const char *line, const char *delimiters));

/* from ip.c */

#ifdef IPPROTO_TCP
int get_host_addr PROTO((const char *hostname, struct in_addr *p_addr));
int get_service_port PROTO((const char *servname, int *p_port));
#endif

/* from sccsdata.c */
const char *ukcprog_version PROTO((void));

#endif	/* !UKCPROG_H_DEFINED */


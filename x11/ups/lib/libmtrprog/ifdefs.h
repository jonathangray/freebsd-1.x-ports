/* ifdefs.h - #defines of symbols based on predefined macros */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ifdefs.h	1.12 18/9/92 (UKC) */

#ifndef IFDEFS_H_INCLUDED
#define IFDEFS_H_INCLUDED

/*  Turn ANSI style __* #defines into old style ones.  We have to treat
 *  the old style ones as reserved anyway.
 */
#ifdef __sun__
#undef sun
#define sun		1
#endif

#ifdef __vax__
#undef vax
#define vax		1
#endif

#ifdef __ultrix__
#undef ultrix
#define ultrix		1
#endif

#ifdef __unix__
#undef unix
#define unix		1
#endif

#ifdef __i386__
#undef i386
#define i386		1
#endif

#ifdef __sparc__
#undef sparc
#define sparc		1
#endif

#ifdef __mc68000__
#undef mc68000
#define mc68000
#undef mc68020
#define mc68020
#endif

#if defined(__clipper__) || defined(__clipper)
#undef clipper
#define clipper		1
#endif

/*  Which architecture?
 */
#ifdef i386
#undef ARCH_386
#define ARCH_386	1
#endif

#if defined(sun) && defined(mc68020)
#undef ARCH_SUN3
#define ARCH_SUN3	1
#define THIS_ARCH	"sun3"
#endif

#if defined(sun) && defined(i386)
#undef ARCH_SUN386
#define ARCH_SUN386	1
#define THIS_ARCH	"sun386"
#endif

#if defined(bsdi) && defined(i386)
#undef ARCH_BSDI386
#define ARCH_BSDI386	1
#define THIS_ARCH	"bsdi386"
#endif

#if defined(sun) && defined(sparc)
#undef ARCH_SUN4
#define ARCH_SUN4	1
#define THIS_ARCH	"sun4"
#endif

#ifdef vax
#undef ARCH_VAX
#define ARCH_VAX	1
#define THIS_ARCH	"vax"
#endif

#ifdef clipper
#undef ARCH_CLIPPER
#define ARCH_CLIPPER
#define THIS_ARCH	"clipper"
#endif

#ifdef mips
#	define ARCH_MIPS
#	define THIS_ARCH	"mips"

#	ifdef ultrix
#		undef OS_ULTRIX
#		define OS_ULTRIX
#		define THIS_OS		"ultrix"
#	else
#	ifdef riscos		/* from top level Makefile */
#		undef OS_RISCOS
#		define OS_RISCOS
#		define THIS_OS		"riscos"
#	else
		/*  mips but not ultrix or riscos - must be NEWS.
		 */
#		undef OS_NEWSOS
#		define OS_NEWSOS
#		define THIS_OS		"newsos"
#	endif /* !riscos */
#	endif /* !ultrix */

#	undef OS_BSD
#	define OS_BSD
#endif /* mips */

/*  Which OS?
 */
#ifdef sun
#	define OS_BSD
#	define OS_SUNOS
#	ifdef SUNOS3
#		define OS_SUNOS_3
#		define THIS_OS		"sunos3"
#	else
#		define OS_SUNOS_4
#		define THIS_OS		"sunos4"
#	endif
#endif /* !sun */

#ifdef vax
#ifdef ultrix
#define OS_ULTRIX
#endif
#define OS_BSD
#endif

#ifdef bsdi
#define OS_BSDI
#define THIS_OS		"bsdi"
#define OS_BSD
#endif

#ifdef clipper
#define OS_BSD
#endif

#if defined(ARCH_SUN3) || defined(ARCH_SUN4) || defined(OS_RISCOS)
#define IS_BIG_ENDIAN		1
#undef IS_LITTLE_ENDIAN
#else
#undef IS_BIG_ENDIAN
#define IS_LITTLE_ENDIAN	1
#endif

/*  define SYMLINKS for UNIX versions with them (currently only bsd)
 */
#ifdef OS_BSD
#undef SYMLINKS
#define SYMLINKS
#endif

/*  similarly for SOCKETS
 */
#ifdef OS_BSD
#undef SOCKETS
#define SOCKETS
#endif

#endif /* !IFDEFS_H_INCLUDED */

/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* std.h */
/* Standard definitions for Aladdin Enterprises code */

#ifndef std_INCLUDED
#  define std_INCLUDED

/* Include the architecture definitions. */
#include "arch.h"
#define arch_ints_are_short (arch_sizeof_int == arch_sizeof_short)

/*
 * Here we deal with the vagaries of various C compilers.  We assume that:
 *	ANSI-standard Unix compilers define __STDC__.
 *	Borland Turbo C and Turbo C++ define __MSDOS__ and __TURBOC__.
 *	Borland C++ defines __BORLANDC__, __MSDOS__, and __TURBOC__.
 *	Microsoft C/C++ defines _MSC_VER and _MSDOS.
 *	Watcom C defines __WATCOMC__ and MSDOS.
 *
 * We arrange to define __MSDOS__ on all the MS-DOS platforms.
 */
#if (defined(MSDOS) || defined(_MSDOS)) && !defined(__MSDOS__)
#  define __MSDOS__
#endif
/*
 * Also, not used much here, but used in other header files, we assume:
 *	Unix System V environments define USG or SYSV.
 *	  (GNU software uses the former, non-GNU tends to use the latter.)
 *	The SCO ODT compiler defines M_SYSV and M_SYS3.
 *	VMS systems define VMS.
 *	  (The VMS C compiler handles prototypes and const, but does not
 *	  define __STDC__.)
 *	bsd 4.2 or 4.3 systems define BSD4_2.
 *	POSIX-compliant environments define _POSIX_SOURCE.
 *	Motorola 88K BCS/OCS systems defined m88k.
 *	OSF/1 compilers define __osf__ or __OSF__.
 *
 * We make fairly heroic efforts to confine all uses of these flags to
 * header files, and never to use them in code.
 */
#if defined(__osf__) && !defined(__OSF__)
#  define __OSF__ /* */
#endif
#if defined(M_SYSV) && !defined(SYSV)
#  define SYSV /* */
#endif
#if defined(M_SYS3) && !defined(__SVR3)
#  define __SVR3 /* */
#endif

#if defined(__STDC__) || defined(__MSDOS__) || defined(__convex__) || defined(VMS)
# if defined(M_UNIX) || defined(__GNUC__) || !defined(M_XENIX) /* SCO Xenix cc is broken */
#  define __PROTOTYPES__ /* */
# endif
#endif

/* Recognize USG and SYSV as synonyms.  GNU software uses the former, */
/* non-GNU tends to use the latter.  We use the latter. */
#ifdef USG
# ifndef SYSV
#  define SYSV /* */
# endif
#endif

/* Define dummy values for __FILE__ and __LINE__ if the compiler */
/* doesn't provide these.  Note that places that use __FILE__ */
/* must check explicitly for a null pointer. */
#ifndef __FILE__
#  define __FILE__ NULL
#endif
#ifndef __LINE__
#  define __LINE__ 0
#endif

/* Disable 'const' and 'volatile' if the compiler can't handle them. */
#ifndef __PROTOTYPES__
#  undef const
#  define const /* */
#  undef volatile
#  define volatile /* */
#endif

/*
 * The SVR4.2 C compiler incorrectly considers the result of << and >>
 * to be unsigned if the left operand is signed and the right operand is
 * unsigned.  We believe this only causes trouble in Ghostscript code when
 * the right operand is a sizeof(...), which is unsigned for this compiler.
 * Therefore, we replace the relevant uses of sizeof with size_of:
 */
#define size_of(x) ((int)(sizeof(x)))

/* Disable MS-DOS specialized pointer types on non-MS-DOS systems. */
/* Watcom C defines near, far, and huge as macros, so we must undef them. */
/* far_data is used for static data that must get its own segment. */
/* This is supported in Borland C++, but none of the others. */
#undef far_data
#ifdef __TURBOC__
#  ifdef __BORLANDC__
#    define far_data far
#  else
#    define far_data /* */
#  endif
#else
#  undef near
#  define near /* */
#  undef far
#  define far /* */
#  define far_data /* */
#  undef huge
#  define huge /* */
#  define _cs /* */
#  define _ds /* */
/* _es is never safe to use */
#  define _ss /* */
#endif

/* Define a couple of useful language extensions. */
/* Get the size of a statically declared array. */
#define countof(a) (sizeof(a) / sizeof((a)[0]))
#define count_of(a) (size_of(a) / size_of((a)[0]))
/* Get the offset of a structure member. */
/* Amazingly enough, this appears to work on all compilers */
/* (except for one broken MIPS compiler). */
#define offset_of(type, memb) ((int) &((type *) 0)->memb)

/* Define short names for the unsigned types. */
typedef unsigned char byte;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;

/* Since sys/types.h often defines one or more of these (depending on */
/* the platform), we have to take steps to prevent name clashes. */
/*** NOTE: This requires that you include std.h *before* any other ***/
/*** header file that includes sys/types.h. ***/
#define uchar uchar_
#define uint uint_
#define ushort ushort_
#define ulong ulong_
#include <sys/types.h>
#undef uchar
#undef uint
#undef ushort
#undef ulong

/* Minimum and maximum values for the signed types. */
#define min_signed(type) (-(type)1 << (size_of(type) * 8 - 1))
#define min_short min_signed(short)
#define max_short (~min_short)
#define min_int min_signed(int)
#define max_int (~min_int)
#define min_long min_signed(long)
#define max_long (~min_long)

/*
 * The maximum values for the unsigned types are defined in arch.h,
 * because so many compilers handle unsigned constants wrong.
 * In particular, most of the DEC VMS compilers incorrectly sign-extend
 * short unsigned constants (but not short unsigned variables) when
 * widening them to longs.  We program around this on a case-by-case basis.
 * Some compilers don't truncate constants when they are cast down.
 * The UTek compiler does special weird things of its own.
 * All the rest (including gcc on all platforms) do the right thing.
 */
#define max_uchar arch_max_uchar
#define max_ushort arch_max_ushort
#define max_uint arch_max_uint
#define max_ulong arch_max_ulong

/* Define a reliable arithmetic right shift. */
/* Must use arith_rshift_1 for a shift by a literal 1. */
#define arith_rshift_slow(x,n) ((x) < 0 ? ~(~(x) >> (n)) : (x) >> (n))
#if arch_arith_rshift == 2
#  define arith_rshift(x,n) ((x) >> (n))
#  define arith_rshift_1(x) ((x) >> 1)
#else
#if arch_arith_rshift == 1		/* OK except for n=1 */
#  define arith_rshift(x,n) ((x) >> (n))
#  define arith_rshift_1(x) arith_rshift_slow(x,1)
#else
#  define arith_rshift(x,n) arith_rshift_slow(x,n)
#  define arith_rshift_1(x) arith_rshift_slow(x,1)
#endif
#endif

/* The type to be used for comparing pointers for order (<, >=, etc.). */
/* Turbo C large model doesn't compare pointers correctly */
/* if they have different segment parts. */
#ifdef __TURBOC__
typedef unsigned long ptr_ord_t;
#else
typedef char *ptr_ord_t;
#endif
/* Define all the pointer comparison operations. */
#define _ptr_cmp(p1, rel, p2)  ((ptr_ord_t)(p1) rel (ptr_ord_t)(p2))
#define ptr_le(p1, p2) _ptr_cmp(p1, <=, p2)
#define ptr_lt(p1, p2) _ptr_cmp(p1, <, p2)
#define ptr_ge(p1, p2) _ptr_cmp(p1, >=, p2)
#define ptr_gt(p1, p2) _ptr_cmp(p1, >, p2)
#define ptr_between(ptr, lo, hi)\
  (ptr_ge(ptr, lo) && ptr_lt(ptr, hi))

/* Define max and min, but make sure to use the identical definition */
/* to the one that all the compilers seem to have.... */
#ifndef min
#  define min(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef max
#  define max(a, b) (((a) > (b)) ? (a) : (b))
#endif

/* Define a standard way to round values to a (constant) modulus. */
#define round_down(value, modulus)\
  ( (modulus) & ((modulus) - 1) ?	/* not a power of 2 */\
    (value) - (value) % (modulus) :\
    (value) & -(modulus) )
#define round_up(value, modulus)\
  ( (modulus) & ((modulus) - 1) ?	/* not a power of 2 */\
    ((value) + ((modulus) - 1)) / (modulus) * (modulus) :\
    ((value) + ((modulus) - 1)) & -(modulus) )

/* VMS doesn't have the unlink system call.  Use delete instead. */
#ifdef VMS
#  define unlink(fname) delete(fname)
#endif

/*
 * In pre-ANSI C, float parameters get converted to double.
 * However, if we pass a float to a function that has been declared
 * with a prototype, and the parameter has been declared as float,
 * the ANSI standard specifies that the parameter is left as float.
 * To avoid problems caused by missing prototypes,
 * we declare almost all float parameters as double.
 */
typedef double floatp;

/* If we are debugging, make all static variables and procedures public */
/* so they get passed through the linker. */
#ifdef NOPRIVATE
# define private /* */
#else
# define private static
#endif

/*
 * Macros for argument templates.  ANSI C has these, as does Turbo C,
 * but older pcc-derived (K&R) Unix compilers don't.  The syntax is
 *	resulttype func(Pn(arg1, ..., argn));
 */

#ifdef __PROTOTYPES__
# define P0() void
# define P1(t1) t1
# define P2(t1,t2) t1,t2
# define P3(t1,t2,t3) t1,t2,t3
# define P4(t1,t2,t3,t4) t1,t2,t3,t4
# define P5(t1,t2,t3,t4,t5) t1,t2,t3,t4,t5
# define P6(t1,t2,t3,t4,t5,t6) t1,t2,t3,t4,t5,t6
# define P7(t1,t2,t3,t4,t5,t6,t7) t1,t2,t3,t4,t5,t6,t7
# define P8(t1,t2,t3,t4,t5,t6,t7,t8) t1,t2,t3,t4,t5,t6,t7,t8
# define P9(t1,t2,t3,t4,t5,t6,t7,t8,t9) t1,t2,t3,t4,t5,t6,t7,t8,t9
# define P10(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10
# define P11(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11
# define P12(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12
#else
# define P0() /* */
# define P1(t1) /* */
# define P2(t1,t2) /* */
# define P3(t1,t2,t3) /* */
# define P4(t1,t2,t3,t4) /* */
# define P5(t1,t2,t3,t4,t5) /* */
# define P6(t1,t2,t3,t4,t5,t6) /* */
# define P7(t1,t2,t3,t4,t5,t6,t7) /* */
# define P8(t1,t2,t3,t4,t5,t6,t7,t8) /* */
# define P9(t1,t2,t3,t4,t5,t6,t7,t8,t9) /* */
# define P10(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) /* */
# define P11(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) /* */
# define P12(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) /* */
#endif

/* Standard error printing macros. */
/* Use dprintf for messages that just go to dstderr, */
/* eprintf for error messages to estderr that include the program name, */
/* lprintf for debugging messages that should include line number info. */
/* Since we intercept fprintf to redirect output under MS Windows, */
/* we have to define dputc and dputs in terms of fprintf also. */

/* dstderr and estderr may be redefined. */
#define dstderr stderr
#define estderr stderr

#define dputc(chr) dprintf1("%c", chr)
#define dputs(str) dprintf1("%s", str)
#define dprintf(str)\
  fprintf(dstderr, str)
#define dprintf1(str,arg1)\
  fprintf(dstderr, str, arg1)
#define dprintf2(str,arg1,arg2)\
  fprintf(dstderr, str, arg1, arg2)
#define dprintf3(str,arg1,arg2,arg3)\
  fprintf(dstderr, str, arg1, arg2, arg3)
#define dprintf4(str,arg1,arg2,arg3,arg4)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4)
#define dprintf5(str,arg1,arg2,arg3,arg4,arg5)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5)
#define dprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6)
#define dprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
#define dprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
#define dprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
#define dprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
#define dprintf11(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
#define dprintf12(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)\
  fprintf(dstderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)

#ifdef PROGRAM_NAME
extern const char *PROGRAM_NAME;
#  define _epn fprintf(estderr, "%s: ", PROGRAM_NAME),
#else
#  define _epn /* */
#endif

#define eprintf(str)\
  (_epn fprintf(estderr, str))
#define eprintf1(str,arg1)\
  (_epn fprintf(estderr, str, arg1))
#define eprintf2(str,arg1,arg2)\
  (_epn fprintf(estderr, str, arg1, arg2))
#define eprintf3(str,arg1,arg2,arg3)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3))
#define eprintf4(str,arg1,arg2,arg3,arg4)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4))
#define eprintf5(str,arg1,arg2,arg3,arg4,arg5)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5))
#define eprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6))
#define eprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
#define eprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
#define eprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
#define eprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
  (_epn fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))

#if __LINE__				/* compiler provides it */
#  define _epl _epn fprintf(estderr, "%s(%d): ", __FILE__, __LINE__),
#else
#  define _epl _epn
#endif

#define lprintf(str)\
  (_epl fprintf(estderr, str))
#define lprintf1(str,arg1)\
  (_epl fprintf(estderr, str, arg1))
#define lprintf2(str,arg1,arg2)\
  (_epl fprintf(estderr, str, arg1, arg2))
#define lprintf3(str,arg1,arg2,arg3)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3))
#define lprintf4(str,arg1,arg2,arg3,arg4)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4))
#define lprintf5(str,arg1,arg2,arg3,arg4,arg5)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5))
#define lprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6))
#define lprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
#define lprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
#define lprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
#define lprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
  (_epl fprintf(estderr, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))

#endif					/* std_INCLUDED */

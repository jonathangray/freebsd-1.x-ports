/* config.h: Master configuration file.  This is included by common.h,
   which everyone includes.  */

#ifndef CONFIG_H
#define CONFIG_H

/* The stuff from the path searching library.  */
#include <kpathsea/config.h>

/* Maximum length of an entire pathname. */
#include <kpathsea/c-pathmx.h>

/* How to open a binary file.  */
#include <kpathsea/c-fopen.h>

/* Argument parsing.  */
#include "getopt.h"

/* Path searching.  */
#include "ourpaths.h"

/* We never need the `link' system call, which is sometimes declared in
   <unistd.h>, but we do have lots of variables called `link' in the web
   sources.  */
#ifdef link
#undef link
#endif
#define link link_var


/* Throw away VMS' library routine `getname', as WEB uses that name.  */
#ifdef VMS
#ifdef getname
#undef getname
#endif
#define getname vms_getname
#endif

/* The smallest signed type: use `signed char' if ANSI C, `short' if
   char is unsigned, otherwise `char'.  */
#ifndef SCHAR_TYPE
#ifdef __STDC__
#define SCHAR_TYPE signed char
#else /* not __STDC */
#ifdef __CHAR_UNSIGNED__
#define SCHAR_TYPE short
#else
#define SCHAR_TYPE char
#endif
#endif /* not __STDC__ */
#endif /* not SCHAR_TYPE */
typedef SCHAR_TYPE schar;


/* The type `integer' must be a signed integer capable of holding at
   least the range of numbers (-2^31)..(2^31-1).  If your compiler goes
   to great lengths to make programs fail, you might have to change this
   definition.  If this changes, you may have to modify
   web2c/fixwrites.c, since it generates code to do integer output using
   "%ld", and casts all integral values to be printed to `long'.
   
   If you define your own INTEGER_TYPE, you have to define your own
   INTEGER_MAX and INTEGER_MIN, too. */
#ifndef INTEGER_TYPE

#if defined (LONG_64_BITS) && !defined (NO_FMTBASE_SWAP)
/* If we have 64-bit longs and want to share format files (with 32-bit
   machines), use `int'.  */
#define INTEGER_IS_INT
#endif

#ifdef INTEGER_IS_INT
#define INTEGER_TYPE int
#define INTEGER_MAX INT_MAX
#define INTEGER_MIN INT_MIN
#else
#define INTEGER_TYPE long
#define INTEGER_MAX LONG_MAX
#define INTEGER_MIN LONG_MIN
#endif /* not INTEGER_IS_INT */

#endif /* not INTEGER_TYPE */

typedef INTEGER_TYPE integer;


#ifdef TeX
/* The type `glueratio' should be a floating point type which won't
   unnecessarily increase the size of the memoryword structure.  This is
   the basic requirement.  On most machines, if you're building a
   normal-sized TeX, then glueratio must probably meet the following
   restriction: sizeof(glueratio) <= sizeof(integer).  Usually, then,
   glueratio must be `float'.  But if you build a big TeX, you can (on
   most machines) and should make it `double' to avoid loss of precision
   and conversions to and from double during calculations.  (All this
   also goes for Metafont.)  Furthermore, if you have enough memory, it
   won't hurt to have this defined to be `double' for running the
   trip/trap tests.
   
   This type is set automatically to `float' by configure if a small TeX
   is built.  */
#ifndef GLUERATIO_TYPE
#define GLUERATIO_TYPE double
#endif
typedef GLUERATIO_TYPE glueratio;
#endif /* TeX */

/* Declarations for the routines we provide ourselves.  */

extern integer zround ();


/* File routines.  */
extern boolean eof ();
extern boolean eoln ();
extern void errprintpascalstring ();
extern void extendfilename ();
extern integer inputint ();
extern void main_body ();
extern boolean open_input ();
extern boolean open_output ();
extern void fprintreal ();
extern void make_c_string ();
extern void make_pascal_string ();
extern void makesuffixpas ();
extern void null_terminate ();
extern void printpascalstring ();
extern void setpaths ();
extern void space_terminate ();
extern boolean test_eof ();
extern boolean testreadaccess ();
extern void uexit ();
extern FILE *xfopen_pas ();
extern void zinput2ints ();
extern void zinput3ints ();


/* Argument handling, etc.  */
extern int argc;
extern char **gargv;
extern void argv ();
extern char *versionstring;

#endif /* not CONFIG_H */

/* More configuration definitions for web2c.  Unlike those in site.h,
   you need not and should not change these.  If you do, then (a) tell
   the people listed in README why you need to change them, and (b) be
   prepared for everything to fail.  */

/* Define if you're running under MS-DOS with Microsoft C.  (This port
   is not complete.)  */
#ifndef MS_DOS
#undef	MS_DOS
#endif

/* These types can be basically anything, so they don't need to be put in
   site.h.  Despite the dire warning above, probably nothing bad will
   happen if you change them -- but you shouldn't need to.  */
typedef char boolean;
typedef double real;

/* The maximum length of a filename including a directory specifier.  */
#ifdef TANDY
#define FILENAMESIZE 128
#else
#define	FILENAMESIZE 512
#endif

/* Hack to get around High C on an IBM RT treating `char' differently
   than normal compilers, etc.   */

#if defined(__HIGHC__) && defined(ibm032)
pragma	Off(Char_default_unsigned);
pragma	On(Char_is_rep);
pragma	On(Parm_warnings);
pragma	On(Pointers_compatible);
pragma	On(Pointers_compatible_with_ints);
#endif	/* __HIGHC__ && ibm032 */

/* config.h 
	vi:se ts=3 sw=3:
 */

/* Configuration for the current platform */

#include <stdio.h>


/* #define SOLARIS */

/* Don't use protos, because gcc doesn't like structures in parameters
 * and mixing protos with non-protos style */
/* #define P(args) args */
#define P(x) ()

/* #define ID(x) */
#define ID(x)  LOCAL char *id = x ;
#define USE_AT_EXIT

/* #define KLUDGE_TAG */
#define GZIP 		
/* #define FORKING	*/
/*	#define void		*/

typedef void *GENERIC;
/* typedef char *GENERIC; */

#undef MALLOC_NOT_IN_STDLIB

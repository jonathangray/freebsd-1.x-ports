/* defs.h 
	vi:se ts=3 sw=3:
 */

/* $Id: defs.h,v 1.1 1994/02/19 16:03:07 ache Exp $ 
 * $Log: defs.h,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.1  1994/02/04  14:54:08  espie
 * Fixed up ansi C stupid bug.
 *
 * Revision 4.0  1994/01/11  17:44:52  espie
 * Added prototypes, and `generic' values.
 *
 * Revision 1.3  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1994/01/05  13:53:25  Espie
 * Better portability
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.6  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 3.5  1993/12/02  15:45:33  espie
 * Stupid fix + type casts.
 *
 * Revision 3.4  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.3  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */

#include "config.h"
#define LOCAL static
/* X is too short */
#define XT extern

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define boolean int
#define MIN(A,B) ((A)<(B) ? (A) : (B))
#define MAX(A,B) ((A)>(B) ? (A) : (B))
     
#define D fprintf(stderr, "%d\n", __LINE__);

typedef union
   {
   unsigned long scalar;
   float real;
   GENERIC pointer;
   } VALUE;

/* predefinitions for relevant structures */
struct tag;
struct channel;
struct song;
struct automaton;
struct sample_info;
struct event;




/* /usr/home/jtc/CVSROOT/mh/miscellany/patch-2.0.12u8/version.c,v 1.1.1.1 1993/01/30 04:40:54 jtc Exp
 *
 * version.c,v
 * Revision 1.1.1.1  1993/01/30  04:40:54  jtc
 * mh-6.8
 *
 * Revision 2.0  86/09/17  15:40:11  lwall
 * Baseline for netwide release.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "INTERN.h"
#include "patchlevel.h"
#include "version.h"

void my_exit();

/* Print out the version number and die. */

void
version()
{
    fprintf(stderr, "Patch version 2.0, patch level %s\n", PATCHLEVEL);
    my_exit(0);
}

/* s/ file for freebsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "freebsd"

#undef KERNEL_FILE
#define KERNEL_FILE "/386bsd"

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "_averunnable"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define HAVE_SETSID

#define LD_SWITCH_SYSTEM -static
#define YMF_PASS_LDFLAGS(flags) flags
#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil

#define HAVE_GETLOADAVG

/* For mem-limits.h.  */
#define BSD4_2

/* Reread the time zone on startup.  */
#define LOCALTIME_CACHE

/* needed for job control in subshells.  */
#define HAVE_SETSID

#include "hosts/i386v.h"

/* Core file stuff.  At least some, perhaps all, of the following
   defines work on many more systems than just SCO.  */

#define NBPG NBPC
#define UPAGES USIZE
#define HOST_DATA_START_ADDR u.u_exdata.ux_datorg
#define HOST_STACK_START_ADDR u.u_sub
#define TRAD_UNIX_CORE_FILE_FAILING_SIGNAL(abfd) \
  ((core_upage(abfd)->u_sysabort != 0) \
   ? core_upage(abfd)->u_sysabort \
   : -1)

/* In my tests on SCO I got 108 extra bytes (on two different core
   dumps of different sizes).  I'm not sure what's in them.  Allow a
   few more than that, in case someone increases it a bit in a future
   release.  */

#define TRAD_CORE_EXTRA_SIZE_ALLOWED 256

#ifdef __GNUC__
#include_next <sys/types.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
#ifndef __GNUC__
#include "/usr/include/sys/types.h"
#endif
extern int seteuid(uid_t);
extern int setruid(uid_t);
extern int setegid(gid_t);
extern int setrgid(gid_t);
#ifdef __cplusplus
}
#endif


#ifdef __GNUC__
#include_next <unistd.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __GNUC__
#include "/usr/include/unistd.h"
#endif

	extern int seteuid(uid_t);
	extern int setegid(gid_t);

#ifdef __cplusplus
}
#endif


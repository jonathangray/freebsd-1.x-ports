/* work around multiple typedefs in stddef.h and sys/types.h */
/* types.h,v 1.1.1.1 1993/05/21 05:37:53 cgd Exp */

#include <stddef.h>		/* defines size_t and ptrdiff_t */
#include <time.h>		/* defines time_t and clock_t */

/* "inhibit" the typedefs in sys/types.h */
#define size_t _size_t
#define	time_t _time_t
#define	clock_t _clock_t
#include "/./usr/include/sys/types.h"
#undef	size_t
#undef	time_t
#undef	clock_t


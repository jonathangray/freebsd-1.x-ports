/* quickly coded bmove for any machine */

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "protos.h"

void
bmove(void *s, void *d, int l)
{
	(void) memcpy(d, s, l);
}

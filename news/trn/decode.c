/* $Id: decode.c,v 1.1 1993/07/19 20:07:00 nate Exp $
 */

#include "EXTERN.h"
#include "common.h"
#include "INTERN.h"
#include "decode.h"

void
decode_init()
{
    unship_init();
}

void
decode_end()
{
    if (decode_fp != Nullfp) {
	fclose(decode_fp);
	decode_fp = Nullfp;
	printf("\n%s INCOMPLETE -- removed.\n", decode_dest) FLUSH;
	unlink(decode_dest);
    }
}

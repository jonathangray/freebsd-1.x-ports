/* $Id: decode.c,v 1.3 1993/11/17 23:02:48 nate Exp $
 */
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
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

/* $Id: decode.h,v 1.1 1993/07/19 20:07:02 nate Exp $
 */

EXT FILE *decode_fp INIT(NULL);
EXT char decode_fname[MAXFILENAME];
EXT char decode_dest[MAXFILENAME];
EXT int decode_type;

void decode_init _((void));
void decode_end _((void));

void uud_start _((void));
int uudecode _((FILE*));

void unship_init _((void));
int unship _((FILE*));

#define UUDECODE 0
#define UNSHIP   1

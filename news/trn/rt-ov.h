/* $Id: rt-ov.h,v 1.4 1994/02/22 01:50:47 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

bool ov_init _((void));
bool ov_data _((ART_NUM,ART_NUM,bool_int));
void ov_close _((void));

/* Stuff internal to rt-ov.c */

#ifdef DOINIT

static ARTICLE *ov_parse _((char*, ART_NUM));
#ifndef USE_XOVER
static char *ov_name _((char*));
#endif

#endif

/* $Id: rt-util.h,v 1.3 1993/11/17 23:03:56 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

char *extract_name _((char*));
char *compress_from _((ARTICLE*,int));
char *compress_name _((char*,int));
char *compress_subj _((ARTICLE*,int));
char *get_subject_start _((char*));
#ifndef HAS_STRCASECMP
int strcasecmp _((char*,char*));
int strncasecmp _((char*,char*,int));
#endif

EXT char spin_char INIT(' ');	/* char to put back when we're done spinning */

#define SPIN_OFF	0
#define SPIN_POP	1
#define SPIN_FOREGROUND	2
#define SPIN_BACKGROUND 3

void setspin _((int));
void spin _((int));

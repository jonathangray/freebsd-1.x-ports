/* $Id: rt-process.h,v 1.2 1993/07/26 19:13:24 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

int msgid_cmp _((char*,int,HASHDATUM));
ARTICLE *allocate_article _((ART_NUM));
bool valid_article _((ARTICLE*));
ARTICLE *get_article _((char*));
void thread_article _((ARTICLE*));

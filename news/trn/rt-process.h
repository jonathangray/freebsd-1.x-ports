/* $Id: rt-process.h,v 1.1 1993/07/19 20:07:06 nate Exp $
*/

int msgid_cmp _((char*,int,HASHDATUM));
ARTICLE *allocate_article _((ART_NUM));
bool valid_article _((ARTICLE*));
ARTICLE *get_article _((char*));
void thread_article _((ARTICLE*));

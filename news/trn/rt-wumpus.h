/* $Id: rt-wumpus.h,v 1.3 1993/11/17 23:03:58 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

void init_tree _((void));
void entire_tree _((ARTICLE*));
int tree_puts _((char*,ART_LINE,int));
int finish_tree _((ART_LINE));

#ifdef DOINIT

static void find_depth _((ARTICLE*,int));
static void cache_tree _((ARTICLE*,int,char*));
static void display_tree _((ARTICLE*,char*));
static char letter _((ARTICLE*));
static int check_page_line _((void));

#endif

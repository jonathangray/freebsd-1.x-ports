/* $Id: rt-page.h,v 1.2 1993/07/26 19:13:22 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

bool set_sel_mode _((char_int));
bool set_sel_sort _((char_int));
void set_selector _((int,int));
void init_pages _((void));
bool first_page _((void));
bool last_page _((void));
bool next_page _((void));
bool prev_page _((void));
void display_page _((void));
void update_page _((void));
void output_sel _((int));

/* Stuff internal to rt-select.c */

#ifdef DOINIT

static int count_subject_lines _((SUBJECT*, int*));
static int count_thread_lines _((SUBJECT*, int*));
static void display_article _((ARTICLE*, char_int, int));
static void display_subject _((SUBJECT*, char_int, int));

#endif

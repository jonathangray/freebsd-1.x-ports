/* edit.c */
void edit __PARMS((long count));
int get_literal __PARMS((int *nextc));
void insertchar __PARMS((unsigned int c));
int oneright __PARMS((void));
int oneleft __PARMS((void));
void beginline __PARMS((int flag));
int oneup __PARMS((long n));
int onedown __PARMS((long n));
int onepage __PARMS((int dir, long count));
void stuff_inserted __PARMS((int c, long count, int no_esc));

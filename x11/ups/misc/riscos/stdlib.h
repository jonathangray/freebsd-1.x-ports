/* stdlib.h - minimal stdlib.h for RISC/os */

/* @(#)stdlib.h	1.1 18/9/92 (UKC) */

/*  stdlib.h seems to be missing on RISC/os.  This version declares
 *  enough to make ups compile cleanly.
 */

double atof();
char *getenv();

char *malloc();
char *realloc();
void free();

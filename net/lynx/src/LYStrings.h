
#ifndef LYSTRINGS_H
#define LYSTRINGS_H

#include <string.h>

extern char * LYstrncpy PARAMS((char *dst, char *src, int n));
extern int LYgetch NOPARAMS;
extern int LYgetstr PARAMS((char *inputline, int hidden));
extern char * LYstrstr PARAMS((char *chptr, char *tarptr));
extern char * LYno_underline_strstr PARAMS((char *chptr, char *tarptr));
extern char * LYno_underline_case_strstr PARAMS((char *chptr, char *tarptr));

extern char * SNACopy PARAMS((char **dest, CONST char *src, int n));
extern char * SNACat PARAMS((char **dest, CONST char *src, int n));

#define StrnAllocCopy(dest, src, n)  SNACopy (&(dest), src, n)
#define StrnAllocCat(dest, src, n)   SNACat  (&(dest), src, n)

#define printable(c) (((c)>31 && (c)<=255) || (c)==9 || (c)==10 || (c)<0 )

/* values for LYgetch */
#define UPARROW 128
#define DNARROW 129
#define RTARROW 130
#define LTARROW 131
#define PGDOWN  132
#define PGUP    133
#define HOME    134
#define END     135
#define F1      136
#define DO_KEY     137
#define FIND_KEY   138
#define SELECT_KEY 139
#define INSERT_KEY 140
#define REMOVE_KEY 141
#define DO_NOTHING 142

#define VISIBLE 0
#define HIDDEN  1

#endif /* LYSTRINGS_H */

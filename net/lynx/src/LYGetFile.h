
#ifndef LYGETFILE_H
#define LYGETFILE_H

#define NOT_FOUND 0
#define NORMAL 1
#define NULLFILE 3

extern BOOLEAN getfile PARAMS((document *doc));
extern int follow_link_number PARAMS((int c, int *cur));

/* values for follow_link_number.c */
#define DO_FORMS_STUFF 1
#define DO_LINK_STUFF  2
#define PRINT_ERROR    3


#endif /* LYGETFILE_H */

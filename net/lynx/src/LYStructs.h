
#ifndef LYSTRUCTS_H
#define LYSTRUCTS_H

#ifndef USERDEFS_H
#include "userdefs.h"
#endif

#ifndef HTAnchor_H
#include "HTAnchor.h"
#endif

typedef struct link {
    char *lname;
    char *target;
    char *hightext;
    int lx;
    int ly;
    int type;      /* type of link, Forms, WWW, etc */
    struct _FormInfo *form;  /* pointer to form info */
} linkstruct; 
extern linkstruct links[MAXLINKS];
extern int nlinks;

typedef struct _document {
   char *address;
   char *title;
   int  link;
   int  line;
} document;

#ifndef HTForms_H
#include "HTForms.h" 
#endif

typedef struct _histstruct {
    char *hfname;
    char *hightext;
    int hlinkno;
    int hpageno;
} histstruct;

extern histstruct history[MAXHIST];
extern int nhist;

typedef struct _printer_type {
    struct _printer_type *next;         /*the next printer in the linked list*/
    char *name;                         /* a description of the printer */
    char *command;                      /* the command to print */
    int  always_enabled;                /* a constant to tell whether or
                                        * not to disable the printer
                                        * when the no_print option is on
                                        */
} printer_type;
extern printer_type *printers;

/* for download commands */
typedef struct _printer_type download_type;
extern download_type *downloaders;

#endif /* LYSTRUCTS_H */


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
    char *hightext2;
    int hightext2_offset;
    int lx;
    int ly;
    int type;      /* type of link, Forms, WWW, etc */
    int anchor_number;   /* the anchor number within the Gridtext structure */
    struct _FormInfo *form;  /* pointer to form info */
} linkstruct; 
extern linkstruct links[MAXLINKS];
extern int nlinks;

typedef struct _document {
   char * title;
   char * address;
   char * post_data;
   char * post_content_type;
   int    link;
   int    line;
} document;

#ifdef DIRED_SUPPORT
typedef struct _taglink {
   char *name;
   struct _taglink *next;
} taglink;
#endif

#ifndef HTForms_H
#include "HTForms.h" 
#endif

typedef struct _histstruct {
    char * title;
    char * address;
    char * post_data;
    char * post_content_type;
    int    link;
    int    page;
} histstruct;

extern histstruct history[MAXHIST];
extern int nhist;

typedef struct _lynx_html_item_type {
    struct _lynx_html_item_type *next;  /* the next item in the linked list */
    char *name;                         /* a description of the item */
    char *command;                      /* the command to execute */
    int  always_enabled;                /* a constant to tell whether or
                                        * not to disable the printer
                                        * when the no_print option is on
                                        */
} lynx_html_item_type;
extern lynx_html_item_type *printers;

/* for download commands */

extern lynx_html_item_type *downloaders;

/* for upload commands */

extern lynx_html_item_type *uploaders;

#endif /* LYSTRUCTS_H */

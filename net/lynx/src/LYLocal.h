#ifndef LYLOCAL_H
#define LYLOCAL_H

#ifdef DIRED_SUPPORT

#ifdef VMS
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif /* VMS */

extern BOOLEAN local_create PARAMS((document *doc));
extern BOOLEAN local_modify PARAMS((document *doc));
extern BOOLEAN local_remove PARAMS((document *doc));

/* Define the PRIVATE routines in case they ever go PUBLIC

extern BOOLEAN modify_name PARAMS((char *testpath));
extern BOOLEAN modify_location PARAMS((char *testpath));
extern BOOLEAN create_file PARAMS((char *testpath));
extern BOOLEAN create_directory PARAMS((char *testpath));
extern BOOLEAN modify_tagged PARAMS((char *testpath));
extern BOOLEAN remove_tagged NOPARAMS;
extern BOOLEAN remove_single PARAMS ((char *testpath));
*/
extern BOOLEAN is_a_file PARAMS((char *testname));
extern void tagflag PARAMS((int flag, int cur)); 
extern void showtags PARAMS((taglink *tag));
extern char * strip_trailing_slash PARAMS((char * dirname));
extern int local_dired PARAMS((document *doc));
extern int dired_options PARAMS ((document *doc, char ** newfile));

#define DIRED_MENU_TITLE "File Management Options"

#endif /* DIRED_SUPPORT */

#endif /* LYLOCAL_H */

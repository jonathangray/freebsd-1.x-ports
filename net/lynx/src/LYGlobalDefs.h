/* global variable definitions */

#ifndef USERDEFS_H
#include "userdefs.h"
#endif

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

#ifndef LYCopyRight_H
#include "LYCopyRight.h"
#endif

#ifndef GLOBAL_DEFS_H
#define GLOBAL_DEFS_H

#define NUMBERS_AS_ARROWS 0
#define LINKS_ARE_NUMBERED 1

#ifdef VMS
#include<stdlib.h>
#if defined(VAXC) && !defined(__DECC)
#define malloc	VAXC$MALLOC_OPT
#define calloc	VAXC$CALLOC_OPT
#define free	VAXC$FREE_OPT
#define cfree	VAXC$CFREE_OPT
#define realloc	VAXC$REALLOC_OPT
#endif /* VAXC && !__DECC */
#endif /* VMS */

#define NOVICE_MODE 	  0
#define INTERMEDIATE_MODE 1
#define ADVANCED_MODE 	  2

#define MAX_LINE        300     /* Hope that no widow is larger than this */
extern char * star_string; /* from GridText.c */
#define STARS(n) (&star_string[(MAX_LINE-1) - (n)])
#define DIRNAMESIZE 256
extern BOOLEAN LYShowCursor;   /* show the cursor or hide it */
extern BOOLEAN LYCursesON;  /* start_curses()->TRUE, stop_curses()->FALSE */
extern BOOLEAN LYUserSpecifiedURL;  /* URL from a goto or document? */
extern int more;  /* is there more document to display? */
extern int HTCacheSize;  /* the number of documents cached in memory */
extern int display_lines; /* number of lines in the display */
extern int www_search_result;
extern char *checked_box;  /* form boxes */
extern char *unchecked_box;  /* form boxes */
extern char *empty_string;
extern char *startfile;
extern char *helpfile;
extern char *display;
extern BOOLEAN LYforce_HTML_mode;
extern BOOLEAN LYforce_no_cache;
extern BOOLEAN user_mode; /* novice or advanced */
extern BOOLEAN is_www_index;
extern BOOLEAN dump_output_immediately;
extern BOOLEAN lynx_mode;

#ifdef DIRED_SUPPORT
extern BOOLEAN lynx_edit_mode;
extern BOOLEAN dir_list_style;
extern taglink *tagged;
#define FILES_FIRST 1
#define MIXED_STYLE 2
#endif

extern BOOLEAN recent_sizechange;
extern BOOLEAN telnet_ok;
extern BOOLEAN news_ok;
extern BOOLEAN no_print;    /* TRUE to disable printing */
#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
extern BOOLEAN local_exec;  /* TRUE to enable local program execution */
        /* TRUE to enable local program execution in local files only */
extern BOOLEAN local_exec_on_local_files; 
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */
extern BOOLEAN child_lynx;	  /* TRUE to exit with an arrow */
extern BOOLEAN error_logging;     /* TRUE to mail error messages */
extern BOOLEAN vi_keys;           /* TRUE to turn on vi-like key movement */
extern BOOLEAN emacs_keys;        /* TRUE to turn on emacs-like key movement */
extern BOOLEAN keypad_mode;       /* is set to either NUMBERS_AS_ARROWS or
				   * LINKS_ARE_NUMBERED 
				   */
extern BOOLEAN case_sensitive;    /* TRUE to turn on case sensitive search */

extern BOOLEAN no_inside_telnet;  /* this and following are restrictions */
extern BOOLEAN no_outside_telnet;
extern BOOLEAN no_inside_news;  
extern BOOLEAN no_outside_news;
extern BOOLEAN no_suspend;
extern BOOLEAN no_editor;
extern BOOLEAN no_shell;
extern BOOLEAN no_bookmark;
extern BOOLEAN no_option_save;
extern BOOLEAN no_print;
extern BOOLEAN no_download;
extern BOOLEAN no_disk_save;
extern BOOLEAN no_exec;
extern BOOLEAN exec_frozen;
extern BOOLEAN no_goto;
extern BOOLEAN no_file_url;
extern BOOLEAN no_newspost;

#ifdef DIRED_SUPPORT
extern BOOLEAN no_dired_support;
#endif

extern char *indexfile;
extern char *personal_mail_address;
extern char *editor;          /* if non empty it enables edit mode with
				   * the editor that is named */
extern char *bookmark_page;
extern char *personal_type_map;
extern char *global_type_map;
extern char *global_extension_map;
extern char *personal_extension_map;

#endif /* GLOBAL_DEFS_H */

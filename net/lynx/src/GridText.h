/*	Specialities of GridText as subclass of HText
*/
#include "HText.h"		/* Superclass */

#ifndef HTForms_H
#include "HTForms.h"
#endif

#define LY_UNDERLINE_START_CHAR '\003'
#define LY_UNDERLINE_END_CHAR   '\004'


#ifdef SHORT_NAMES
#define HText_childNumber		HTGTChNu
#define HText_canScrollUp		HTGTCaUp
#define HText_canScrollDown		HTGTCaDo
#define HText_scrollUp			HTGTScUp
#define HText_scrollDown		HTGTScDo
#define HText_scrollTop			HTGTScTo
#define HText_scrollBottom		HTGTScBo
#define HText_sourceAnchors		HTGTSoAn
#define HText_setStale			HTGTStal
#define HText_refresh			HTGTRefr
#endif

extern int WWW_TraceFlag;
extern int HTCacheSize;

extern HTChildAnchor * HText_childNumber PARAMS((int n));

/*	Is there any file left?
*/
extern BOOL HText_canScrollUp PARAMS((HText * text));
extern BOOL HText_canScrollDown ();

/*	Move display within window
*/
extern void HText_scrollUp PARAMS((HText * text));	/* One page */
extern void HText_scrollDown PARAMS((HText * text));	/* One page */
extern void HText_scrollTop PARAMS((HText * text));
extern void HText_scrollBottom PARAMS((HText * text));
extern void HText_pageDisplay PARAMS((int line_num, char *target));

extern int HText_sourceAnchors PARAMS((HText * text));
extern void HText_setStale PARAMS((HText * text));
extern void HText_refresh PARAMS((HText * text));
extern char * HText_getTitle PARAMS(());
extern char * HText_getOwner PARAMS(());
extern void print_wwwfile_to_fd();
extern BOOLEAN HTFindPoundSelector PARAMS((char *selector));
extern int HTGetLinkInfo PARAMS((int number, char **hightext, char **lname));
extern int HTisDocumentSource();
extern void HTuncache_current_document();
extern int HText_getTopOfScreen ();
extern int HText_getNumOfLines();
extern int do_www_search PARAMS((document *doc));

/* forms stuff */
extern void HText_beginForm PARAMS((char *action, char *method));
extern void HText_endForm();
extern void HText_beginSelect PARAMS((char *name, BOOLEAN multiple));
extern void HText_setLastOptionValue PARAMS((HText *text, char *value));
extern int HText_beginInput PARAMS((HText *text, InputFieldData *I));
extern char * HText_SubmitForm PARAMS((FormInfo *form));
extern void HText_ResetForm PARAMS((FormInfo *form));
extern void HText_activateRadioButton PARAMS((FormInfo *form));


#ifdef CURSES
extern int HText_getTopOfScreen ();
extern int HText_getLines PARAMS((HText * text));
#endif

extern void user_message PARAMS((char * message, char * argument));

/* fed.h - header file for the field editing package fed.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)fed.h	1.9 25/4/92 (UKC) */

#define FED_H_INCLUDED

/*  Various special control keys
 */
#define CONTROL_C	('C' & 037)	/* ^C cancels the edit		*/
#define OOPS		('U' & 037)	/* ^U deletes to start of line	*/
#define CONTROL_L	('L' & 037)	/* forward space edit cursor	*/
#define DEL		'\177'		/* delete a single character	*/
#define BACKSPACE	'\b'		/* backspace edit cursor	*/
#define RETURN		'\r'		/* confirm edit			*/
#define ESC		('[' & 037)	/* ditto 			*/

/*  Typedefs for the callback routines.
 *
 *  BUG: the "void *edesc" parameters should be "struct edesc *edesc",
 *       but gcc won't allow members of a structure to be pointers
 *	 to functions which have a pointer to that structure as an
 *	 argument.
 */
typedef void (*ed_keyfunc_t)PROTO((void *edesc));
typedef void (*ed_puckfunc_t)PROTO((void *edesc));
typedef int (*ed_quitfunc_t)PROTO((void *edesc, int n_quit_attempts));
typedef void (*ed_expose_handler_t)PROTO((unsigned long resize_type,
							bool clear_first));

/*  The structure describing the current edit. The a pointer to this
 *  type is passed around in fed.c
 */
typedef struct edescst {
	window_t ed_wn;		/* viewport we are editing in		*/
	font_t *ed_font;	/* font (possibly var width)		*/
	short ed_fg;		/* foreground pixel color for text	*/
	short ed_bg;		/* background pixel color for text	*/
	char ed_act;		/* type of action			*/
	char ed_meaning;	/* what the action means		*/
	char ed_char;		/* key entered if ed_act is ED_KEY	*/
	char ed_flags;		/* various flags - see below		*/
	short ed_puck_x;	/* x,y coords of puck press relative ...   */
	short ed_puck_y;      /*   ... to ed_wn for EDA_PUCK action	*/
	short ed_buttons;	/* button pressed for EDA_PUCK		*/
	short ed_curpos;	/* current cursor position		*/
	short ed_newpos;	/* new value of pos - set by get_act	*/
	short ed_scrlen;	/* length of string to shift on screen	*/
	short ed_curlen;	/* current ed_copy string length	*/
	short ed_maxlen;	/* maximum ed_copy string length	*/
	const char *ed_orig;	/* pointer to original string		*/
	char *ed_copy;		/* pointer to working copy		*/
	int ed_user;		/* for the user				*/
	ed_keyfunc_t ed_keyfunc;   /* called to check key input		*/
	ed_puckfunc_t ed_puckfunc; /* called to check puck input	*/
	ed_quitfunc_t ed_quitfunc; /* called when user tries to confirm edit */
	ed_expose_handler_t ed_expose_handler; /* called on window expose event */
} edesc_t;

/*  meanings of bits in ed_flags above
 */
#define EDF_CURSOR_ON	 01	/* editing cursor on/off		*/
#define EDF_BLOCK_CURSOR 02	/* block/vertical bar editing cursor	*/
#define EDF_OVERWRITE	 04	/* overwrite/insert mode editing	*/
#define EDF_UNGETPUCK	010	/* user confirmed edit with puck	*/
#define EDF_ONETRIP	020	/* caller has done input, handle this only */
#define EDF_CONT_ON_CANCEL 040	/* continue editing after a cancel req	*/

/*  flags for the use of the user
 */
#define EDF_USER1	0100

/*  The three types of action returned by get_act()
 */
#define EDA_PUCK	11	/* puck button pressed	*/
#define EDA_NONE	12	/* nothing happened	*/
#define EDA_KEY		13	/* key pressed		*/

/*  The meanings, as set by preprocess_{puck,key}, and given to
 *  do_edit() and process_key()
 */
#define EDM_SETCURSOR		21	/* set edit cursor pos		*/
#define EDM_CONT		22	/* carry on editing - no action */
#define EDM_CANCEL		23	/* cancel - restore orig string */
#define EDM_DELETE_LINE		25	/* delete to start of line	*/
#define EDM_FORWARD_SPACE	26	/* forward one character	*/
#define EDM_BACK_SPACE		27	/* back one character		*/
#define EDM_ILLEGAL_CHAR	28	/* illegal character		*/
#define EDM_CONFIRM		29	/* user confirmed edit		*/
#define EDM_INSERT_CHAR		30	/* delete single character	*/
#define EDM_DELETE_CHAR		31	/* delete single character	*/
#define EDM_PASTE_SELECTION	32	/* paste window system cut buffer */

/*  Values returned by (*quit_handler)() in edit_field()
 */
#define EDR_CANT_QUIT		41	/* user tried to quit but can't	 */
#define EDR_CONT		42	/* keep editing			 */
#define EDR_CANCEL		43	/* cancel edit			 */
#define EDR_CANCEL_AND_QUIT	44	/* cancel edit and quit		 */
#define EDR_CONFIRM_CHANGE	45	/* confirm edit and quit	 */
#define EDR_CONFIRM_NO_CHANGE	46	/* quit - edit has had no effect */

/*  Arguments to ed_cursor
 */
#define EDC_CURSOR_OFF	0
#define EDC_CURSOR_ON	1

void make_edesc PROTO((edesc_t *edesc, int wn, const char *orig,
						int maxlen, int fg, int bg));
int process_key PROTO((edesc_t *edesc, int meaning));
void ed_newstring PROTO((edesc_t *edesc, const char *s));
void ed_new_orig PROTO((edesc_t *edesc, const char *new_orig));
void ed_cursor PROTO((edesc_t *edesc, int cursor_on));
int edit_field PROTO((edesc_t *edesc));
void do_edit PROTO((edesc_t *edesc, int meaning));

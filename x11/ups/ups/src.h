/* src.h - public header file for src.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)src.h	1.7 4/7/91 (UKC) */

#define SRC_H_INCLUDED

/*  Opaque handle on a source window.
 */
typedef struct { int dummy_member; } *srcwin_id_t;

srcwin_id_t src_make_srcwin PROTO((int wn));
void src_redraw PROTO((srcwin_id_t srcwin_id));
void src_close_srcwin PROTO((srcwin_id_t srcwin_id));
void src_goto_ypos PROTO((srcwin_id_t srcwin_id, int ypos));
void src_clear PROTO((srcwin_id_t srcwin_id));
void src_clear_history PROTO((srcwin_id_t srcwin_id));
void src_push_current_pos PROTO((srcwin_id_t srcwin_id));
void src_pop PROTO((srcwin_id_t srcwin_id));
int src_scroll PROTO((srcwin_id_t srcwin_id, int npixels));
void src_show_lnum PROTO((srcwin_id_t srcwin_id, int src_lnum, bool centre));
void src_search PROTO((srcwin_id_t srcwin_id, const char *pattern, bool forwards));

void src_set_data PROTO((srcwin_id_t srcwin_id, char *data));
char *src_get_data PROTO((srcwin_id_t srcwin_id));

int src_get_window_height PROTO((srcwin_id_t srcwin_id));

long src_draw_arrow PROTO((srcwin_id_t srcwin_id, int lnum));
void src_draw_box PROTO((srcwin_id_t srcwin_id, int lnum, int col, int nchars));

#ifdef TEXTWIN_H_INCLUDED
void src_set_textwin_callback_and_data PROTO((srcwin_id_t srcwin_id,
					     tw_callback_t textwin_callback,
					     char *arg));
#endif

#ifdef SYMTAB_H_INCLUDED

/*  Opaque handle on an edit block.
 */
typedef struct editblock_idst {
	int dummy_member;
} *editblock_id_t;

#ifdef WN_H_INCLUDED
/*  The information returned by src_info.
 */
typedef struct srcinfost {
	fil_t *si_fil;			/* displayed file, or NULL */
	editblock_id_t si_editblock_id;	/* edit block (x,y) is in, or NULL */
	int si_editblock_offset;	/* offset into edit block, or -1 */
	int si_wn;			/* window number of source window */
	font_t *si_font;		/* font used to display source */
	const char *si_text;		/* text of line (x,y) is over, or NULL */
	int si_fil_lnum;		/* fil lnum of line (x,y) is over, or -1 */
	int si_src_lnum;		/* src lnum of line (x,y) is over, or -1 */
	int si_cnum;			/* cnum of line (x,y) is over, or -1 */
} srcinfo_t;

void src_get_info PROTO((srcwin_id_t srcwin_id, int x, int y, srcinfo_t *si));
void src_get_fonts PROTO((srcwin_id_t srcwin_id,
					font_t **p_srcfont, font_t **p_editfont));
#endif

int src_lnum_to_ypos PROTO((srcwin_id_t srcwin_id, fil_t *fil, int lnum));
int src_show PROTO((srcwin_id_t srcwin_id, fil_t *fil, int fil_lnum, bool centre));

/*  The information returned by get_editblock_info().
 */
typedef struct editblockinfost {
	fil_t *ei_fil;			/* file this edit block belongs to */
	int ei_fil_lnum;		/* line number in original file */
	int ei_src_lnum;		/* line number in modified source */
	char **ei_lines;		/* the editblock lines */
	int ei_nlines;			/* the number of lines in the block */
	int ei_fg;			/* foreground pixel of edit block */
	int ei_bg;			/* background pixel of edit block */
} editblockinfo_t;

typedef enum { EDL_SELECT, EDL_DESELECT, EDL_REMOVE } editblock_action_t;
typedef enum { DONT_CALL_CALLBACK, DO_CALL_CALLBACK } call_callback_t;

typedef char *callback_arg_t;

typedef void (*editblock_callback_t)PROTO((editblock_id_t editblock,
					   editblock_action_t action));
typedef bool (*editblock_edit_callback_t)PROTO((editblock_id_t editblock,
					        const char **lines, int nlines,
						callback_arg_t callback_arg));

editblock_id_t add_editblock PROTO((fil_t *fil, int lnum,
				    const char **lines, int nlines));
void get_editblock_info PROTO((editblock_id_t editblock_id, editblockinfo_t *ei));

int change_editblock PROTO((editblock_id_t editblock_id,
			    const char **lines, int nlines,
			    call_callback_t call_callback,
			    callback_arg_t callback_arg));

void remove_editblock PROTO((editblock_id_t editblock_id,
			     call_callback_t call_callback));
void remove_all_editblocks PROTO((fil_t *fil, call_callback_t call_callback));
void register_editblock_callback PROTO((editblock_id_t editblock_id,
					editblock_callback_t callback));
void register_editblock_edit_callback PROTO((editblock_id_t editblock_id,
					editblock_edit_callback_t edit_callback));				
void select_editblock PROTO((editblock_id_t editblock_id,
			     editblock_action_t action,
			     call_callback_t call_callback));
void deselect_all_editblocks PROTO((call_callback_t call_callback));
void set_editblock_data PROTO((editblock_id_t editblock_id, char *data));
char *get_editblock_data PROTO((editblock_id_t editblock_id));
#endif /* SYMTAB_H_INCLUDED */

/* menudata.h - definitions of menu return values for menudata.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)menudata.h	1.2 4/7/91 (UKC) */

/*  The ups menus were created using the UKC med3 menu editor.
 *  This uses character values like 'a' as return values.
 *  Unfortunately there is no way to automatically tie these to
 *  a C #define or enum, so we do this by hand in this file.
 */

/*  Return values from menus/varmen.c
 */
#define MR_VAR_STRING			's'
#define MR_VAR_SIGNED_DECIMAL		't'
#define MR_VAR_UNSIGNED_DECIMAL		'T'
#define MR_VAR_SIGNED_OCTAL		'o'
#define MR_VAR_UNSIGNED_OCTAL		'O'
#define MR_VAR_SIGNED_HEX		'h'
#define MR_VAR_UNSIGNED_HEX		'H'
#define MR_VAR_UNSIGNED_BINARY		'B'
#define MR_VAR_ASCII_BYTE		'a'
#define MR_VAR_DEREF	 		'u'
#define MR_VAR_ADDRESS			'd'
#define MR_VAR_EXPAND			'e'
#define MR_VAR_DUP			'D'
#define MR_VAR_DELETE			'k'
#define MR_VAR_COLLAPSE			'c'
#define MR_VAR_COLLAPSE_COMPLETELY	'C'
#define MR_VAR_WANT_TYPEDEFS		'U'
#define MR_VAR_NO_TYPEDEFS		'I'

/*  Return values from menus/srcmen.c
 */
#define MR_SRCWIN_BACK			'r'
#define MR_SRCWIN_SEARCH_FORWARDS	'f'
#define MR_SRCWIN_SEARCH_BACKWARDS	'b'
#define MR_SRCWIN_PAGE_UP		'u'
#define MR_SRCWIN_PAGE_DOWN		'd'

/*  Return values from menus/outmen.c
 */
#define MR_OUTWIN_PAGE_UP		'u'
#define MR_OUTWIN_PAGE_DOWN		'd'
#define MR_OUTWIN_CLEAR			'c'
#define MR_OUTWIN_SEARCH_FORWARDS	'f'
#define MR_OUTWIN_SEARCH_BACKWARDS	'b'

/*  Return values from menus/bphmen.c
 */
#define MR_ADD_BREAKPOINT		'a'
#define MR_REMOVE_ALL_BREAKPOINTS	'r'

/*  Return values from menus/bptmen.c
 */
#define MR_BPTMEN_REMOVE		'd'
#define MR_BPTMEN_SOURCE		'e'

/*  Return values from menus/gblmen.c
 */
#define MR_SHOW_UNTYPED_VARS		'a'
#define MR_HIDE_UNTYPED_VARS		'c'

/*  Return values from menus/cbhmen.c
 */
#define MR_SHOW_COMMON_BLOCKS		'e'
#define MR_HIDE_ALL_COMMON_BLOCKS	'c'

/*  Return values from menus/cblockmen.c
 */
#define MR_EXPAND_COMMON_BLOCK		'e'
#define MR_COLLAPSE_COMMON_BLOCK	'c'
#define MR_HIDE_COMMON_BLOCK		'h'

/*  Return values from menus/shmen.c
 */
#define MR_SHOW_SOURCE_FILES		'e'
#define MR_HIDE_SOURCE_FILES		'c'

/*  Return values from menus/varmen.c
 */
#define MR_ADD_VARS			'a'
#define MR_HIDE_VARS			'c'
#define MR_DISPLAY_SOURCE		'e'
#define MR_ADD_EXPRESSION		'x'

/*  Return values from menus/sghmen.c
 */
#define MR_SHOW_ALL_SIGNALS		'a'
#define MR_HIDE_ALL_SIGNALS		'c'

/*  Return values from menus/sigmen.c
 */
#define MR_SIG_TOGGLE_STOP_CONT		's'
#define MR_SIG_TOGGLE_ACCEPT_IGNORE	'a'
#define MR_SIG_TOGGLE_REDRAW		'r'
#define MR_HIDE_SIGNAL			'h'

/*  Return values from menus/cmdmen.c
 */
#define MR_QUIT_UPS			'q'
#define MR_DONT_QUIT			'?'

/*  Return values from menus/coremen.c
 */
#define MR_WRITE_CORE			'd'

/*  Return values from the target menus.  The target captions
 *  are done as seperate menus to support the stop button.
 */
#define MR_TGT_RUN			'r'
#define MR_TGT_CONT			'c'
#define MR_TGT_STEP			's'
#define MR_TGT_NEXT			'n'
#define MR_TGT_STOP			'S'
#define MR_TGT_KILL			'e'

/*  Return values from menus/envheadmen.c
 */
#define MR_RESET_ENVTAB			'r'
#define MR_SHOW_ALL_ENV_ENTRIES		's'
#define MR_HIDE_ALL_ENV_ENTRIES		'h'
#define MR_ADD_ENV_ENTRY		'a'

/*  Return values from menus/envmen.c
 */
#define MR_DELETE_ENV_ENTRY		'd'
#define MR_HIDE_ENV_ENTRY		'h'
#define MR_APPEND_ENV_ENTRY		'a'

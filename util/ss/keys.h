/*
 * %W% %G%
 *
 *	ss	Art's Spread sheet program
 *
 *	Key Definition header file
 *		- This is a prime candidate for an 'rc' file
 */

#define ctl(c) ((c)&037)
#define ESC 033
#define DEL 0177

/*
 * I set up this header file, to make it easy for customization
 * of key bindings.  An "rc" file is the next step.
 */

/*
 * CONTROL KEYS
 * 	I don't know how to disable flow control, so I don't use
 * 	control `s' or `q'.
 *
 * Checklist:
 * A: kSTART	G: kGOTO	M: kABORT	S: n/a		Y: kCOPY
 * B: kLEFT	H: kBS 		N: kDOWN	T: kTOP		Z: kSTOP
 * C: kBREAK	I: kTAB		O		U
 * D: kDEL	J: kJUMP	P: kUP		V: kPGDN
 * E: kFINISH	K: kEXP		Q: n/a		W: kMARK
 * F: kRIGHT	L: kREDRAW	R: kVAL		X: Ctrl-X Prefix
 */

#define kLEFT	ctl('b')	/* Move Cursor Left */
#define kDOWN	ctl('n')	/* ... Down */
#define kUP	ctl('p')	/* ... Up */
#define kRIGHT	ctl('f')	/* ... Right */
	/* the main input routine [nmgetch()] converts the keypad keys to
	 * control chars (defined here).  Hence, *at this time*
 	 * we cannot just use the curses definitions of the keys.
 	 */

#define kTAB	ctl('i')	/* Tab key */

#define kSTART	ctl('a')	/* Move to column A of current row */
#define kFINISH ctl('e')	/* Move to last valid column in curr. row */

#define kSTOP	ctl('z')	/* Stop/Halt/Suspend program */
#define kBREAK	ctl('c')	/* Break */
#define kABORT	ctl('m') 	/* Abort menus */

#define kBS	ctl('h')	/* Backspace */

#define kTOP	ctl('t')	/* Move to row 0 of current column */
#define kPGDN	ctl('v')	/* Move cursor down one page */

#define kREDRAW	ctl('l')	/* Redraw screen */
#define kEXP	ctl('k')	/* Redraw, highlight Expression cells */
#define kVAL	ctl('r')	/* Redraw, highlight value cells */

#define kJUMP	ctl('j')	/* Jump to the end of a specified range */
#define kGOTO	ctl('g')	/* Goto a Cell */

#define kDEL	ctl('d')	/* Delete/Erase the current cell */
#define kMARK	ctl('w')	/* Mark a cell for copying */
#define kCOPY	ctl('y')	/* Copy a previously Marked Cell
				   to the current cell */
/*
 * META KEYS
 * 	- in combination with the meta (ESC) keys
 */
#define kMETA	ESC		/* Meta Prefix Key. */

#define kHOME	'<'		/* Move cursor to cell A0 */
#define kEND	'>'		/* Move to last row of current column */

#define kPGUP	'v'		/* Move cursor up one 'page' */

#define kBACK	'b'		/* backward to prev. valid cell */
#define kFORW	'f'		/* forward to next valid cell */

/*
 * Ctrl-X KEYS
 * 	- in combination with the Ctrl-X key.
 */
#define kCTRLX	ctl('x')	/* ^X- Prefix Key */
    /* Sort of silly, but constants are better than
     * "Magic" values/numbers scattered througout
     * your code.
     */
       
#define kPGRIGHT '>'		/* Move Right one page */
#define kPGLEFT  '<'		/* Move Left one page */

#define kEDVAL   'v'		/* Edit the Cell Value */
#define kEDLABL  'l'		/* Edit the Cell Label */
/*******************
*	END
********************/

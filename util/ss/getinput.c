/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Get User input.
*
* This is intended (as the original was) to be a re-usable package.
* Unlike the original it *does* depend on Curses.
* The Internal functions are independent, and should need NO
* customization at all to be used with another software package.
* the Externally accessable functions will probably need tweeking &
* modifying for whatever package they are used with.
*
* The Macro functins gi_getchar(), gi_ungetc(), and gi_beep() also
* probably need adjusting to fit whatever package this is
* incorporated into.
*
***********************************************************************
* This is based upon the public domain utility package ``input-edit''
* by Chris Thewalt.  His Copyright, appears here:
*
* * Copyright (C) 1991, 1992 by Chris Thewalt (thewalt@ce.berkeley.edu)
* *
* * Permission to use, copy, modify, and distribute this software
* * for any purpose and without fee is hereby granted, provided
* * that the above copyright notices appear in all copies and that both
* * copyright notice and this permission notice appear in supporting
* * documentation.  This software is provided "as is" without express or
* * implied warranty.
*
**********************************************************************/
#ifndef lint
  static char Sccsid[] = "%W% %G%";
#endif

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include "curses_stuff.h"
#include <ctype.h>

/**
  #include <sys/types.h>
  #include <signal.h>
**/

#include "ss.h"
    /* the function prototype for ``nmgetch()'' is really all
     * that is gotten from here
     */
#include "keys.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 
#define gi_getchar()	nmgetch()  /* Get a character of input */
#define gi_ungetc(c)	nmungetch(c) /* push `c' back onto input stream */
#define gi_beep()	beep()	   /* error bell */

#define BUF_SIZE 512
static char gi_buff[BUF_SIZE];		/* input buffer */
static char gi_killbuff[BUF_SIZE] = "";	/* hold killed text for yanking */

static int	gi_buffsize;		/* Size of input in buffer */
static int	gi_buffpos;		/* Position of Cursor in buffer */
static int	gi_extent = 0;
/* How much (the extent) of the input buffer must be redrawn each
 * time through the gi_display_buff() function.  0 = all of it.
 * IE: in overwrite mode, you only need to redraw the single character
 *     that has been entered into the buffer.
 */
static int 	gi_overwrite = 0; 	/* 1=overwrite mode, 0=insert mode */

#define GI_NOCHANGE	-1

#define GI_NOT_DONE	1	/* Flag: Not Done Processing input */
#define GI_DONE		2	/* Flag: Done Processing Input */
#define GI_ABORT	3	/* Flag: User ABORTED input */

/*	Function Prototypes
 ***********************************************************************
 */

static int 	gi_edit_input();
static void 	gi_addchar();		/* add character to buffer */
static void 	gi_display_buff();	/* display any changes to buffer */
static void 	gi_newline();
static void     gi_del();               /* delete char, left or right */
static void     gi_kill();              /* delete to EOL */
static void     gi_yank();              /* yank killed text */
static void     gi_transpose();         /* transpose two chars */

/*	Externally Accessible Functions
 ***********************************************************************
 */

char * gi_line ()
/*----------------------------------------------------------------------
** Get a line of input.
** RETURNS: NULL -> signals that the user aborted input.
**	    A pointer to a *static* *internal* buffer, which holds
**		the line of input.  Do Not Free() it!  
**
** This function should serve as a template for writing other
** function front-ends to the internal functions in this
** getinput.c package.
**
** BUG: all the curses stuff (X,Y) location, curses window, window width
** is all "hard-wired" in.  Should be passed as a function parameter.
*/
{
    int c;
    int flag;			/* returned from gi_edit_input() */

    (void)move(0,0);		/* Position Cursor, Clear  */
    (void)clrtoeol();

    gi_buff[0]  = 0;		/* Initialize buffer & var's */
    gi_buffpos  = 0;
    gi_buffsize = 0;
  
    do {			/* Input processing loop */
	c = gi_getchar();

	/* PRE-PROCESS INPUT HERE: */

	/* PROCESS INPUT: */
	flag = gi_edit_input(c); /* General Input/Edit routine */
    } while (flag == GI_NOT_DONE);

    if (flag == GI_ABORT)
	return NULL;
    else
	return gi_buff;		/* Return pointer to the input buffer */

} /* gi_line() */

char * gi_line_cursor()
/*----------------------------------------------------------------------
** Get a line of input.  
** RETURNS: NULL -> signals that the user aborted input.
**	    A pointer to a *static* *internal* buffer, which holds
**		the line of input.  Do Not Free() it!  
**
** Based on, and almost identical to ``gi_line()''.  Input is
** preprocessed, such that a cursor key (Arrow Key) terminates input.
** Furthermore, the character that caused the termination of input
** is pushed back onto the input stream, so that it is available to
** the calling routine to act upon.
*/
{
    int c;
    int flag;			/* Returned from gi_edit_input() */

    (void)move(0,0);		/* Position Cursor, Clear  */
    (void)clrtoeol();

    gi_buff[0]  = 0;		/* Initialize buffer & var's */
    gi_buffpos  = 0;
    gi_buffsize = 0;

    do {				/* Input processing loop */
	c = gi_getchar();

	/* PRE-PROCESS INPUT HERE:
	 *
	 * Check if `c' is an arrow/cursor key
	 */
	if ( (c == kLEFT) || (c == kRIGHT) || (c == kDOWN) || (c == kUP) ) {
	    gi_newline();
	    flag = GI_DONE;
	} else

	    /* PROCESS INPUT: */
	    flag = gi_edit_input(c); /* General Input/Edit routine */
    
    } while (flag == GI_NOT_DONE);

    if (flag == GI_ABORT)
	return NULL;
    else {
	gi_ungetc(c);		/* push `c' back onto the input stream */
	return gi_buff;		/* Return pointer to the input buffer */
    }

} /* gi_line_cursor() */

char * gi_editline (initstr)
/*----------------------------------------------------------------------
** Get a line of input.  The input is returned in a *static* *internal*
** buffer.  Do Not Free() it!  
**
** Based on, and almost identical to ``gi_line()''.
** The function starts out with a non-empty edit buffer -> ``initstr''.
*/
    char *initstr;		/* Initial string to be edited. */
{
    int c;
    int flag;

    (void)move(0,0);		/* Position Cursor, Clear  */
    (void)clrtoeol();

    /*
     * Initialize edit buffer to hold the user-provided initial string
     * (initstr).  If the user string exceeds the internal edit buffer
     * size, it is truncated to fit.  Calculate the size of the buffer
     * and the cursor position (end of the string).  Finally, display
     * the buffer, and everything is ready for user editing.
     */	
    strncpy(gi_buff, initstr, (BUF_SIZE - 1)); /* Initialize edit buffer */
    gi_buffpos = gi_buffsize = strlen(gi_buff);
    gi_display_buff(0, gi_buffpos); /* Display initial Edit buffer */
  
    do {			/* Input processing loop */
	c = gi_getchar();

	/* PRE-PROCESS INPUT HERE: */

	/* PROCESS INPUT: */
	flag = gi_edit_input(c); /* General Input/Edit routined */
    } while (flag == GI_NOT_DONE);

    if (flag == GI_ABORT)
	return NULL;
    else
	return gi_buff;		/* Return pointer to the input buffer */

} /* gi_editline() */



/*	Internal Functions
 ***********************************************************************
 */

static int gi_edit_input(c)
/*----------------------------------------------------------------------
** General input/Edit routine.
** RETURNS: GI_DONE -> done processing input line, a <CR> was entered.
**	    GI_NOT_DONE -> not yet done processing input.
**	    GI_ABORT -> user aborted (via ^g, ^c) input
*/
     int c;
{
  if (isprint(c)) { 		/* Just a regular character */
      gi_addchar(c);
  } else {
      switch(c) {
	case ctl('a'):			/* ^A -> Beginning of Line */
	    gi_display_buff(GI_NOCHANGE, 0);
	    break;
	  
	case ctl('b'): 			/* ^B, Left Arrow Key */
	    if (gi_buffpos == 0 )
		gi_beep();	/* At the begining of the line */
	    else
		gi_display_buff(GI_NOCHANGE, gi_buffpos-1);
	    break;

	case ctl('c'): case ctl('g'):	/* ^C,^G -> ABORT input */
	    return GI_ABORT;

	case ctl('d'):			/* ^D -> Delete char 'under' cursor */
	    gi_del(0);
	    break;
	    
	case ctl('e'):			/* ^E -> End of Line */
	    gi_display_buff(GI_NOCHANGE, gi_buffsize); 
	    break;
	    
	case ctl('f'): 			/* ^F, Right Arrow Key */
	    if (gi_buffpos == gi_buffsize)
		gi_beep();	/* At the end of the line */
	    else
		gi_display_buff(GI_NOCHANGE, gi_buffpos+1);
	    break;

	/* See above for ^G */
	    
	case ctl('h'): case '\177':	/* ^H, <del> -> Delete char */
	    gi_del(-1);			/* to left of cursor. */
	    break;

	case ctl('k'):			/* ^K -> Kill to EOL */
	    gi_kill();
	    break;
	      
	case ctl('l'):			/* ^L -> Redraw line */
	    /* Redraw by re-displaying the entire buffer from position 0.
	     * The cursor does not change position.
	     */
	    gi_display_buff(0, gi_buffpos);
	    break;

	case ctl('m'):			/* ^M, <CR> -> done input */
	    gi_newline();
	    return GI_DONE;
	    break;
	
	case ctl('o'):			/* ^O -> toggle overwrite mode */
	    gi_overwrite = ! gi_overwrite; 
	    break;

        case ctl('t'): 			/* ^T -> transpose char's */
	    gi_transpose();
	    break;
	    
	case ctl('y'):			/* ^Y -> Yank back killed text */
	    gi_yank();
	    break;
	    
	default:
	  gi_beep();
	  break;
      }
  }
  return GI_NOT_DONE;		/* not done yet! */
  
} /* gi_input_edit() */
	    
static void gi_addchar(c)
     int c;
{
    int i;
    
    /*
     * Make sure the buffer is not over flowed.  (BUF_SIZE - 1) because
     * C arrays count from 0 to n-1.  (BUF_SIZE - 2) so that we *always*
     * have room for the string-terminating NULL character
     */
    if (gi_buffsize >= (BUF_SIZE - 2)) {
	gi_beep();
	return;
    }

    if (gi_overwrite == 0 || gi_buffpos == gi_buffsize) {
	/* If we're NOT in over-write mode, or at the end of the input:
	 * 1) move everything from cursor position to the end of the buffer
	 *    to the right one.
	 * 2) insert the new character.
	 */
        for (i=gi_buffsize; i >= gi_buffpos; i--)
            gi_buff[i+1] = gi_buff[i];
        gi_buff[gi_buffpos] = c;

	gi_buffsize++;		/* Buffer is now one char bigger... */
        gi_display_buff(gi_buffpos, gi_buffpos+1);

	
    } else {
	/* Else we are in overwrite mode. Overwrite the character at
	 * the current buffer position with the new character.
	 */
	gi_buff[gi_buffpos] = c;
	gi_extent = 1; 		/* Only need to display 1 character */
        gi_display_buff(gi_buffpos, gi_buffpos+1);
    }
} /* gi_addchar() */

static void gi_display_buff( change, cursor )
/*----------------------------------------------------------------------
** Main output routine.  Displays the contents of the buffer.
** -> would get considerably more complicated if you add code to overflow
**    the edge of the display.
** -> if ``change'' is negative, special activitiy occurs.
*/
     int change;	/* index of start of changes in gi_buff */
     int cursor;	/* where to put cursor after changes */
{
    int x;
    int how_far;	/* index of the end of changes in gi_buff. */
			/* (How Far we have to redisplay the buffer) */
    
    if (change >= 0) {

	/* Usually we redisplay the buffer from the current cursor position
	 * right through to the end of the buffer (gi_buffsize).  However,
	 * sometimes that is not necessary.  IE: in overwrite mode, you only
	 * need to display the newly entered character, the rest of the
	 * buffer is fine.  When gi_extent is non-zero, it indicates how
	 * many characters (starting at the current cursor position) must
	 * be displayed.
	 */
	if (gi_extent != 0)
	    how_far = change + gi_extent;
	else
	    how_far = gi_buffsize;
	
	for (x=change; x < how_far; x++) 
	    mvaddch( 0, x, gi_buff[x]);
            /* Display the buffer, starting at the `change' location,
	     * through to ``how far'' we're supposed to go.
	     */
	
	if (gi_extent == 0) {
	    move (0, how_far);
	    /* mvadch() doesn't seem to leave us in the right spot for
	     * this, misses the last character in the buffer.  Odd
	     */
	    clrtoeol();		/* In case we've deleted something */
	}
	
	gi_extent = 0;	
    }

    gi_buffpos = cursor;	/* Position the cursor */
    move (0, cursor);
    Refresh(); 			/* Now's the acid test... */
      /* Yes there is a refresh in nmgetch(), but there
      ** seems to be some lag time, should investigate...
      */
	
} /* gi_display_buff() */

static void gi_newline()
/*----------------------------------------------------------------------
** The user has entered a <CR>.  Finish up & clean up the buffer.
*/
{
    gi_buff[++gi_buffsize] = '\0';
    
} /* gi_newline() */


static void gi_del(loc)
/*----------------------------------------------------------------------
** Delete a character.  The loc variable can be:
** -1 : delete character to left of cursor
**  0 : delete character under cursor
*/
    int loc;

{
    int i;

    /*
     * -Deleting to the left: Make sure that we aren't already at
     *  the left margin.
     * -Deleting to the right: Check that we aren't at the right margin.
     * -Delete by moving everything in the buffer left one position,
     *  starting at the position (gi_buffpos + loc).
     */
    if ((loc == -1 && gi_buffpos > 0) ||
	(loc == 0 && gi_buffpos < gi_buffsize)) {

	for (i=gi_buffpos+loc; i < gi_buffsize; i++)
	    gi_buff[i] = gi_buff[i+1];

	gi_buffsize--;		/* reset buffer size */
	gi_display_buff(gi_buffpos+loc, gi_buffpos+loc);
	
    } else			/* Nothing to delete! */
	gi_beep();

} /* gi_del() */


static void gi_kill()
/*----------------------------------------------------------------------
** Delete from current position to the end of line.  The deleted test
** is stored in a kill buffer, from which it can be later retrieved.
*/
{
    if (gi_buffpos < gi_buffsize) { /* Not at end of line, go ahead... */
	
	strcpy(gi_killbuff, gi_buff + gi_buffpos);
	gi_buff[gi_buffpos] = '\0';

	gi_buffsize = gi_buffpos;	/* reset buffer size */
	gi_display_buff(gi_buffpos, gi_buffpos);
	
    } else			/* At the end of the line, */
	gi_beep();		/* Nothing to Kill */

} /* gi_kill() */


static void gi_yank()
/*----------------------------------------------------------------------
** Add the kill buffer to the input buffer at current location.
** Overwrite the text following the cursor in the buffer, if we
** are in overwrite mode.  Otherwise shift it right (Insert).
*/
{
    int  i, len;

    len = strlen(gi_killbuff);	/* Length of text in kill buffer */
    if (len <= 0)
	gi_beep();		/* nothing to yank! */

    else {
	if (gi_overwrite == 0) {	/* INSERT mode */
	    if (gi_buffsize + len >= BUF_SIZE - 2)
		gi_beep();	/* No Room in buffer for yanked text ! */

	    else {
		/* Move the current text over */
		for (i=gi_buffsize; i >= gi_buffpos; i--)
		    gi_buff[i + len] = gi_buff[i];

		/* Insert the yanked text */
		for (i=0; i<len; i++)
		    gi_buff[gi_buffpos+i] = gi_killbuff[i];

		gi_buffsize += len; 	/* reset buffer contents size */
	    }
	} else {			/* OVERWRITE mode */
            if (gi_buffpos + len >= BUF_SIZE - 2)
		gi_beep();      /* No Room in buffer for yanked text ! */
	    
	    else {
		/* Write the yanked text over the current buffer text */
		for (i=0; i < len; i++)
		    gi_buff[gi_buffpos + i] = gi_killbuff[i];

		/* Reset size of buffer contents, if necessary. */
		if ((gi_buffpos + len) > gi_buffsize)
		    gi_buffsize = gi_buffpos + len;

		/* In overwrite mode, only the characters entered need
		 * to be displayed, so set gi_extent accordingly.
		 */
		gi_extent = len;
	    }
	}
	gi_display_buff(gi_buffpos, gi_buffpos + len);
    }

} /* gi_yank() */
				    
static void gi_transpose()
/*----------------------------------------------------------------------
** Switch character under cursor and to left of cursor
*/
{
    int    c;

    /* - if the cursor is at the left margin, you cannot transpose,
     * since there is no character to the left of it.
     * - if the cursor is at the right margin (past the end of
     * the text) you cannot transpose either.
     */
    if ((gi_buffpos > 0) && (gi_buffsize > gi_buffpos)) {
	c = gi_buff[gi_buffpos-1];
	gi_buff[gi_buffpos-1] = gi_buff[gi_buffpos];
	gi_buff[gi_buffpos] = c;

	gi_extent = 2; 	/* Only need to redisplay the 2 affected char's. */
	gi_display_buff(gi_buffpos-1, gi_buffpos);
    } else
	gi_beep();

} /* gi_transpose() */	

/**********************************************************************
*       End
**********************************************************************/

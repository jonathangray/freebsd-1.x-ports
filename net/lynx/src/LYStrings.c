#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYGlobalDefs.h"
#include "GridText.h"

#include <ctype.h>

#ifndef FANCY_CURSES
#define NO_KEYPAD
#endif

/*
 * LYstrncpy() terminates strings with a null byte.
 * Writes a null byte into the n+1 byte of dst.
 */
PUBLIC char * LYstrncpy ARGS3(char *,dst, char *,src, int,n)
{
    char *val;
    int len=strlen(src);

    val = strncpy(dst, src, n);
    if(len<n)
        *(dst+len) = '\0';
    else
        *(dst+n) = '\0';
    return val;
}

#ifdef VMS
#define GetChar() ttgetc()
#else
#ifdef SNAKE
#define GetChar() wgetch(stdscr)
#else /* everything but VMS and SNAKE */
#define GetChar() getchar()  /* used to be "getc(stdin)" and "getch()" */
#endif /* SNAKE */
#endif /* VMS */


/*
 * LYgetch() translates some escape sequences and may fake noecho
 */
PUBLIC int LYgetch ()
{
    int a, b, c, d;

    c = GetChar();

    if (c == 27) {      /* handle escape sequence */
        b = GetChar();

        if (b == '[' || b == 'O') {
            a = GetChar();
        } else {
            a = b;
	}

        switch (a) {
        case 'A': c = UPARROW; break;
        case 'x': c = UPARROW; break;  /* keypad up on pc ncsa telnet */
        case 'B': c = DNARROW; break;
        case 'r': c = DNARROW; break; /* keypad down on pc ncsa telnet */
        case 'C': c = RTARROW; break;
        case 'v': c = RTARROW; break; /* keypad right on pc ncsa telnet */
        case 'D': c = LTARROW; break;
        case 't': c = LTARROW; break; /* keypad left on pc ncsa telnet */
        case 'y': c = PGUP; break;  /* keypad on pc ncsa telnet */
        case 's': c = PGDOWN; break;  /* keypad on pc ncsa telnet */
        case 'w': c = HOME; break;  /* keypad on pc ncsa telnet */
        case 'q': c = END; break;  /* keypad on pc ncsa telnet */
        case 'M': c = '\n'; break; /* kepad enter on pc ncsa telnet */

        case 'm':
#ifdef VMS
            if (b != 'O')
#endif /* VMS */
                c = '-';  /* keypad on pc ncsa telnet */
            break;
	case 'k':
	    if(b == 'O');
		c = '+';  /* keypad + on my xterminal :) */
	    break;
        case 'l':
#ifdef VMS
            if (b != 'O')
#endif /* VMS */
                c = '+';  /* keypad on pc ncsa telnet */
            break;
        case 'P':
#ifdef VMS
            if (b != 'O')
#endif /* VMS */
                c = F1;
            break;
        case 'u':
#ifdef VMS
            if (b != 'O')
#endif /* VMS */
                c = F1;  /* macintosh help button */
            break;
        case '1':                           /** VT300  Find  **/
            if (b == '[' && GetChar() == '~')
                c = FIND_KEY;
            break;
	case '2':
	    if (b == '[') {
	        if ((d=GetChar())=='~')     /** VT300 Insert **/
	            c = INSERT_KEY;
	        else if ((d == '8' ||
			  d == '9') &&
			 GetChar() == '~')
	         {
		    if (d == '8')            /** VT300  Help **/
	                c = F1;
	            else if (d == '9')       /** VT300   Do  **/
	                c = DO_KEY;
		 }
	    }
	    break;
	case '3':			     /** VT300 Delete **/
	    if (b == '[' && GetChar() == '~')
	        c = REMOVE_KEY;
	    break;
        case '4':                            /** VT300 Select **/
            if (b == '[' && GetChar() == '~')
                c = SELECT_KEY;
            break;
        case '5':                            /** VT300 PrevScreen **/
            if (b == '[' && GetChar() == '~')
                c = '-';
            break;
        case '6':                            /** VT300 NextScreen **/
            if (b == '[' && GetChar() == '~')
                c = '+';
            break;
	default:
	   if(TRACE) {
		fprintf(stderr,"Unknown key sequence: %d:%d:%d\n",c,b,a);
		sleep(1);
	   }
        }
#if defined(NO_KEYPAD) || defined(VMS)
    }
#else
    } else {

	/* convert keypad() mode keys into Lynx defined keys
	 */

	switch(c) {
	case KEY_DOWN:	           /* The four arrow keys ... */
	   c=DNARROW;
	   break;
	case KEY_UP:	
	   c=UPARROW;
	   break;
	case KEY_LEFT:	
	   c=LTARROW;
	   break;
	case KEY_RIGHT:	           /* ... */
	   c=RTARROW;
	   break;
	case KEY_HOME:	           /* Home key (upward+left arrow) */
	   c=HOME;
	   break;
	case KEY_CLEAR:	           /* Clear screen */
	   c=18; /* CTRL-R */
	   break;
	case KEY_NPAGE:	           /* Next page */
	   c=PGDOWN;
	   break;
	case KEY_PPAGE:	           /* Previous page */
	   c=PGUP;
	   break;
	case KEY_LL:	           /* home down or bottom (lower left) */
	   c=END;
	   break;
                                        /* The keypad is arranged like this:*/
                                        /*    a1    up    a3   */
                                        /*   left   b2  right  */
                                        /*    c1   down   c3   */
	case KEY_A1:	           /* upper left of keypad */
	   c=HOME;
	   break;
	case KEY_A3:	           /* upper right of keypad */
	   c=PGUP;
	   break;
	case KEY_B2:	           /* center of keypad */
	   c=DO_NOTHING;
	   break;
	case KEY_C1:	           /* lower left of keypad */
	   c=END;
	   break;
	case KEY_C3:	           /* lower right of keypad */
	   c=PGDOWN;
	   break;
#ifdef KEY_END
	case KEY_END:	           /* end key           001 */
	   c=END;
	   break;
#endif /* KEY_END */
#ifdef KEY_HELP
	case KEY_HELP:	           /* help key          001 */
	   c=F1;
	   break;
#endif /* KEY_HELP */
	}
    }
#endif /* defined(NO_KEYPAD) || defined(VMS) */

    return(c);
}

/*
 * display the current value of the string and allow the user
 * to edit it.
 */

#ifdef getyx
#define GetYX(y,x)   getyx(stdscr,y,x)
#else
#define GetYX(y,x)   y = stdscr->_cury, x = stdscr->_curx
#endif

PUBLIC int LYgetstr ARGS2(char *,inputline, int,hidden)
{
     int pointer = 0;
     int tmp_pointer;
     int ch;
     int startcol, startline;
     int cur_col;
     BOOLEAN line_extended=FALSE;  /* TRUE if the line was moved to accomadate
                                    * more text entry
                                    */
#ifdef VMS
    extern BOOLEAN term_letter;	   /* Flag from terminate_letter() AST	*/
    extern BOOLEAN term_options;   /* Flag from terminate_options() AST	*/
    extern BOOLEAN HadVMSInterrupt;/* Flag from cleanup_sig() AST       */
#endif

     /* get the initial position of the cursor */
     GetYX(startline, startcol);

     /*** Check to see if there's something in the inputline already ***/

     if(strlen(inputline)+startcol+5 > LYcols-1) {
        pointer = (strlen(inputline) - ((LYcols-1) - startcol)) + 10;
        line_extended = TRUE;
     }

     cur_col = startcol;
     while (inputline[pointer] != '\0') {
	  if(!hidden)
              addch(inputline[pointer]);
	  else
	      addch('*');
          pointer ++;
          cur_col++;
     }
     refresh();

     for (;;) {
          ch = LYgetch();
#ifdef VMS
	  if (term_letter || term_options || HadVMSInterrupt) {
	      HadVMSInterrupt = FALSE;
	      ch = 7;
	  }
#endif

          switch (ch) {

#ifdef AIX
          case '\227':
#endif
          case '\n':
          case '\r':
          case '\t':
          case DNARROW:
          case UPARROW:
               inputline[pointer] = '\0';
               return(ch);
               break;

          /* Control-G aborts */
          case 7:
               inputline[0] = '\0';
               return(-1);

                /* break */  /* implicit break */

          /* erase all the junk on the line and start fresh */
          case 21 :
                move(startline, startcol);
                clrtoeol();
                pointer = 0;  /* clear inputline */
                cur_col = startcol;
                line_extended = FALSE;
                refresh();
                break;

          /**  Backspace and delete **/

          case '\010':
          case '\177':
          case LTARROW:
               if (pointer > 0) {
                    addch('\010');
                    addch(' ');
                    addch('\010');

                    pointer--;
                    cur_col--;

                    if(line_extended && cur_col+15 < LYcols-1) {
                        if(pointer + startcol+5 > LYcols-1) {
                            tmp_pointer = (pointer - ((LYcols-1) - startcol)) + 5;
                        } else {
                            tmp_pointer = 0;
                            line_extended = FALSE;
                        }

                        move(startline, startcol);
                        clrtoeol();

                        cur_col = startcol;

                        while (tmp_pointer != pointer) {
	  			if(!hidden)
              			    addch(inputline[tmp_pointer]);
	  			else
	      			    addch('*');
                                tmp_pointer ++;
                                cur_col++;
                        }

                    }
                    refresh();

               } else if(ch == LTARROW) {
                   inputline[0] = '\0';
                   return(ch);
               }
               break;


          default:
               if (printable(ch)) {
                    inputline[pointer++]= ch;
                    cur_col++;
		    if(!hidden)
                        addch(ch);
		    else
                        addch('*');

                    if(cur_col+2 > LYcols-1) {
                        tmp_pointer = (pointer - ((LYcols-1) - startcol)) + 10;

			if(pointer - tmp_pointer > (LYcols-1)-startcol ||
							tmp_pointer > pointer)
			   tmp_pointer = pointer; 

                        line_extended = TRUE;

                        move(startline, startcol);
                        clrtoeol();

                        cur_col = startcol;

                        while (tmp_pointer < pointer) {
			 	if(!hidden)
                                    addch(inputline[tmp_pointer]);
                                else
                                    addch('*');

                                tmp_pointer ++;
                                cur_col++;
                        }

                    }
                    refresh();
               }
               /* else
                  return(ch); */
                /* just ignore unprintable charactors */
          }
     }

}

/*
 * LYstrstr will find the first occurence of the string pointed to by tarptr
 * in the string pointed to by chptr.  
 * It is a case insensitive search.
 */
PUBLIC char * LYstrstr ARGS2(char *,chptr, char *,tarptr)
{
    register char *tmpchptr, *tmptarptr;

    for(; *chptr != '\0'; chptr++) {
	if(toupper(*chptr) == toupper(*tarptr)) {	
	    /* see if they line up */ 
	    for(tmpchptr = chptr+1, tmptarptr = tarptr+1;
	         toupper(*tmpchptr) == toupper(*tmptarptr)
		 && *tmptarptr != '\0' && *tmpchptr != '\0';
	        			*tmpchptr++, *tmptarptr++)
		   ; /* null body */ 
	    if(*tmptarptr == '\0') 
	  	return(chptr);
	}
    } /* end for */

    return(NULL);
}	

/*
 * LYno_attr_char_strstr will find the first occurence of the string 
 * pointed to by tarptr in the string pointed to by chptr.
 * it ignores the characters: LY_UNDERLINE_START_CHAR and
 * 			      LY_UNDERLINE_END_CHAR
 * 			      LY_BOLD_START_CHAR
 * 			      LY_BOLD_END_CHAR
 * It is a case insensitive search.
 */
PUBLIC char * LYno_attr_char_case_strstr ARGS2(char *,chptr, char *,tarptr)
{
    register char *tmpchptr, *tmptarptr;
    register int offset=0;

    for(; *chptr != '\0'; chptr++) {

	/* treat UNDERLINE chars like they don't exist */
	if(*chptr == LY_UNDERLINE_START_CHAR || 
                        *tarptr == LY_UNDERLINE_END_CHAR)
	    offset++;

        if(toupper(*chptr) == toupper(*tarptr)) {

            /* see if they line up */
	    tmpchptr = chptr+1;
	    tmptarptr = tarptr+1;

	    if(*tmptarptr == '\0')  /* one char target */
		 return(chptr-offset);

	    while(1) {
		 if(!IsSpecialAttrChar(*tmpchptr)) {

                    if(toupper(*tmpchptr) != toupper(*tmptarptr))
			break;

		    *tmpchptr++;
		    *tmptarptr++;

		 } else {
		    tmpchptr++;
		 }

                 if(*tmptarptr == '\0')
		     return(chptr-offset);

		 if(*tmpchptr == '\0')
		     break;
	    }
        }
    } /* end for */

    return(NULL);
}

/*
 * LYno_attr_char_strstr will find the first occurence of the string
 * pointed to by tarptr in the string pointed to by chptr.
 * it ignores the characters: LY_UNDERLINE_START_CHAR and
 *                            LY_UNDERLINE_END_CHAR
 *                            LY_BOLD_START_CHAR
 *                            LY_BOLD_END_CHAR
 * It is a case sensitive search.
 */
PUBLIC char * LYno_attr_char_strstr ARGS2(char *,chptr, char *,tarptr)
{
    register char *tmpchptr, *tmptarptr;
    register int offset=0;

    for(; *chptr != '\0'; chptr++) {

	 /* treat UNDERLINE chars like they don't exist */
        if(*chptr == LY_UNDERLINE_START_CHAR || 
                        *tarptr == LY_UNDERLINE_END_CHAR)
            offset++;

        if((*chptr) == (*tarptr)) {

            /* see if they line up */
            tmpchptr = chptr+1;
            tmptarptr = tarptr+1;

	    if(*tmptarptr == '\0')  /* one char target */
		 return(chptr-offset);

            while(1) {
		 if(!IsSpecialAttrChar(*tmpchptr)) {

                    if((*tmpchptr) != (*tmptarptr))
                        break;

                    *tmpchptr++;
                    *tmptarptr++;

                 } else {
                    tmpchptr++;
                 }

                 if(*tmptarptr == '\0')
                     return(chptr-offset);

                 if(*tmpchptr == '\0')
                     break;
            }
        }
    } /* end for */

    return(NULL);
}

/*      Allocate a new copy of a string, and returns it
*/
PUBLIC char * SNACopy ARGS3 (char **,dest, CONST char *,src, int,n)
{
  if (*dest) free(*dest);
  if (! src)
    *dest = NULL;
  else {
    *dest = (char *) calloc (n + 1,1);
    if (*dest == NULL) {
	fprintf(stderr,"Tried to malloc %d bytes\n",n);
	outofmem(__FILE__, "SNACopy");
    }
    strncpy (*dest, src, n);
    *(*dest + n) = '\0'; /* terminate */
  }
  return *dest;
}

/*      String Allocate and Concatenate
*/
PUBLIC char * SNACat ARGS3 (char **,dest, CONST char *,src, int,n)
{
  if (src && *src) {
    if (*dest) {
      int length = strlen (*dest);
      *dest = (char *) realloc (*dest, length + n + 1);
      if (*dest == NULL) outofmem(__FILE__, "SNACat");
      strncpy (*dest + length, src, n);
      *(*dest + length + n) = '\0'; /* terminate */
    } else {
      *dest = (char *) calloc (strlen(src) + 1,1);
      if (*dest == NULL) outofmem(__FILE__, "SNACat");
      strncpy (*dest, src,n);
      *dest[n] = '\0'; /* terminate */
    }
  }
  return *dest;
}



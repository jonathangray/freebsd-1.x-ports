#include <sys/file.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <fcntl.h>
#include "pt.h"

static void VtermInsert();
static int trace_xterm = 0;
static char row_changed[256];

#define scr_col(n) ((n)%(ff->screen_cols+1))
#define scr_row(n) ((n)/(ff->screen_cols+1))
#define scr_pos(row,col) ((row)*(ff->screen_cols+1) + (col))

/*ARGSUSED*/
static void
ProcessShellInput( cd, mask )
	ClientData cd;
	int mask;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern struct window * scroll_window;
	extern int intervalRows;
	extern int scrollDown;
	extern struct openFile *files;

	struct window *w = (struct window *)cd;
	char ch;
	int n;
	int fid = w->fileId;
	struct openFile * ff = &files[fid];
	int selRow;

	if( ff->screen_image != NULL ) {
		for( n = 0; n < ff->screen_rows; ++n )
			row_changed[n] = 0;
	} else if( w != selWindow ) {
			Offset fs = fileSize(fid) - 1;
			selWindow = w;
			/* do all inserting at the end of the log file */
			selBegin = selEnd = fs;
	}
	OffsetToXY( selWindow, selBegin-1, &selRow, &n );
	if( selRow < 0 )
		selRow = selWindow->nRows;

	/* insert the characters into the window */
	while( 1 ) {
		n = read( w->toShellFD, &ch, 1 );
		if( n <= 0 ) {
			if( n < 0 ) {
				extern int errno;
				if( errno == EWOULDBLOCK )
					break;
				/* see if the process has terminated */
				if( waitpid(selWindow->childPID,NULL,WNOHANG)
						== selWindow->childPID){
					msg(
"Disconnected from pseudo-teletype.  Resuming normal Point input mode.", 0 );
					selWindow->isLogWindow = 0;
					Tk_DeleteFileHandler(
							selWindow->toShellFD );
					close( selWindow->toShellFD );
		
				}
			}
			break;
		}
		VtermInsert( ch, ff );
	}
	if( ff->screen_image != NULL ) {
		for( n = 0; n < ff->screen_rows; ++n )
			if( row_changed[n] )
				drawWindowFast( selWindow, n, n, 0,
						selWindow->nCols-1, 0 );
		return;
	}
	/* draw the new text */
	drawWindowFast( selWindow, selRow, selWindow->nRows-1,
					0, selWindow->nCols-1, 0 );

	/* make sure the end of the text is visible */
	while( 1 ) {
		Offset selminus1 = selBegin - 1;
		if( selminus1 < 0 )
			selminus1 = 0;
		OffsetToXY( selWindow, selBegin-1, &selRow, &n );
		if( selRow < 0 ) {
			intervalRows = 1;
			if( selBegin > selWindow->posTopline )
				scrollDown = 1;
			else
				scrollDown = 0;
			scroll_window = selWindow;
			DoOneVScroll();
		} else
			break;
	}
}

static void VtermInsert( ch, ff )
	char ch;
	struct openFile * ff;
{
	extern Display * MainDisplay;
	extern Offset selBegin;

	static int state = 0;
	static int count1;
	static int count2;
	int i, j;
	char * eol;

	switch( state ) {
	case 0:		/* no escape sequence pending */
		switch( ch ) {
		case '\007':	/* bel */
			XBell( MainDisplay, 0 );
			break;
		case '\010':	/* backspace */
			if( ff->screen_image == NULL ) {
				insChar( ch, 0, 2 );
				break;
			}
			if( trace_xterm )
				printf("<BS>");
			row_changed[scr_row(selBegin)] = 1;
			if( scr_col(selBegin) != 0 )
				--selBegin;
			break;
		case '\015':	/* CR */
			/* ignore */
			break;
		case '\033':	/* escape */
			state = 1;
			break;
		case '\011':	/* tab */
			printf("<TAB>");
			break;
		case '\012':	/* newline */
			if( ff->screen_image == NULL )
				goto not_full_screen;
			i = scr_row(selBegin);
			if( i < (ff->screen_rows-1) ) {
				row_changed[i] = 1;
				row_changed[i+1] = 1;
				/* move down a line */
				selBegin = scr_pos(i+1,0);
			} else {
				/* at the bottom line, must scroll */
				printf("<SCROLL DISPLAY 1 LINE-NL>\n");
			}
			break;
		default:
			if( ff->screen_image == NULL ) {
		not_full_screen:
				insertChar( ch );
			} else {
				row_changed[scr_row(selBegin)] = 1;
				/* store the new character */
				*(ff->screen_image + selBegin++) = ch;
			}
			break;
		}
		break;
	case 1:		/* seen: ESCAPE */
		switch( ch ) {
		case ']':	/* set text parameters */
			state = 2;
			break;
		case '7':	/* save cursor position */
			if( trace_xterm )
				printf("<SAVE cursor position>\n");
			state = 0;
			break;
		case '8':	/* restore cursor position */
			if( trace_xterm )
				printf("<RESTORE cursor position>\n");
			state = 0;
			break;
		case 'D':	/* scroll text up */
			if( ff->screen_image == NULL )
				break;
			printf("<SCROLL TEXT UP>\n");
			state = 0;
			break;
		case 'M':	/* scroll text down */
			if( ff->screen_image == NULL )
				break;
			printf("<SCROLL TEXT DOWN>\n");
			state = 0;
			break;
		case '>':	/* reset terminal to sane modes */
		case '<':	/* reset terminal to sane modes */
			if( trace_xterm )
				printf("<RESET to sane modes(>)>\n");
			state = 0;
			break;
		case '=':	/* put into keyboard transmit mode */
			if( trace_xterm )
				printf("<KEYBOARD transmit begin>\n");
			state = 0;
			break;
		case 'O':	/* cursor movement */
			state = 5;
			break;
		case '[':
			state = 3;
			count1 = -1;	/* the default */
			break;
		default:
			printf("<UNEXPECTED: ESCAPE then (%c:%x)>\n",ch, ch);
			state = 0;
			break;
		}
		break;
	case 2:		/* skip until bel */
		if( ch == '\7' ) {
			if( trace_xterm )
				printf("<SET TEXT PARAMETERS %d>\n", count1);
			state = 0;
		}
		break;
	case 3:		/* seen: ESCAPE [ */
			/* next is either a count Ps (default case:) */
			/* or if Ps is defaulted to 1 or we have already */
			/* seen Ps then it is the character that decides */
			/* what control sequence it is */
		/* state goes back to 0 in almost every case */
		/* do it once here and change it if necessary in the cases */
		state = 0;
		switch( ch ) {

		default:	/* Ps -- first count */
			printf("<UNEXPECTED: ESC [ (%c:%x)>\n",ch,ch);
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* Ps -- first count */
			if( count1 == -1 )
				count1 = 0;
			count1 = 10*count1 + ch - '0';
			state = 3;
			break;
		case '@':	/* Insert count1 (blank) characters */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<INSERT %d blank chars>\n",count1);
			/* find the end of the line */
			i = scr_row(selBegin);
			row_changed[i] = 1;
			j = scr_pos( i, 79 );
			eol = ff->screen_image + j;
			j = j - selBegin - count1;
			for( i = 0; i < j; ++i ) {
				*eol = *(eol-count1);
				--eol;
			}
			for( i = 0; i < count1; ++i ) {
				*eol-- = ' ';
			}
			break;
		case 'A':	/* cursor UP count1 times */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<UP %d blank lines>\n",count1);
			i = scr_row(selBegin);
			row_changed[i] = 1;
			row_changed[i-1] = 1;
			selBegin -= (ff->screen_cols+1)*count1;
			break;
		case 'B':	/* cursor DOWN count1 times */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<DOWN %d blank lines>\n",count1);
			i = scr_row(selBegin);
			row_changed[i] = 1;
			row_changed[i+1] = 1;
			selBegin += (ff->screen_cols+1)*count1;
			break;
		case 'C':	/* cursor FORWARD count1 times */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<FORWARD %d blank chars>\n",count1);
			i = scr_row(selBegin);
			row_changed[i] = 1;
			selBegin += count1;
			if( scr_col(selBegin) == ff->screen_rows )
				++selBegin;
			break;
		case 'D':	/* cursor BACKWARD count1 times */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<BACKWARD %d blank chars>\n",count1);
			i = scr_row(selBegin);
			row_changed[i] = 1;
			selBegin -= count1;
			if( scr_col(selBegin) == ff->screen_rows )
				++selBegin;
			break;
		case 'H':	/* Home */
			if( ff->screen_image == NULL )
				break;
			if( trace_xterm )
				printf("<HOME>\n");
			i = scr_row(selBegin);
			row_changed[i] = 1;
			selBegin = 0;
			row_changed[0] = 1;
			break;
		case 'J':	/* Erase in display */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			switch( count1 ) {
			case 0:		/* clear below */
				printf("<CLEAR below>\n");
				break;
			case 1:		/* clear above */
				printf("<CLEAR above>\n");
				break;
			case 2:		/* clear all */
				if( trace_xterm )
					printf("<CLEAR all>\n");
				eol = ff->screen_image;
				if( ff->screen_image == NULL )
					break;
				for( i = 0; i < ff->screen_rows; ++i ) {
					row_changed[i] = 1;
					for(j = 0; j < ff->screen_cols; ++j) {
						*eol++ = ' ';
					}
					*eol++ = '\n';
				}
				selBegin = 0;
				break;
			}
			break;
		case 'K':	/* Erase in line */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			switch( count1 ) {
			case 0:		/* clear to right */
				if( trace_xterm )
					printf("<CLEAR line right>\n");
				eol = ff->screen_image + selBegin;
				row_changed[scr_row(selBegin)] = 1;
				i = ff->screen_cols - scr_col(selBegin);
				while( i-- > 0 ) {
					*eol++ = ' ';
				}
				break;
			case 1:		/* clear to left */
				printf("<CLEAR line left>\n");
				break;
			case 2:		/* clear all */
				printf("<CLEAR line all>\n");
				break;
			}
			break;
		case 'L':	/* Insert Ps lines */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			printf("<INSERT %d lines>\n",count1);
			break;
		case 'M':	/* Delete Ps lines */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			printf("<DELETE %d lines>\n",count1);
			break;
		case 'P':	/* Delete Ps characters */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			if( trace_xterm )
				printf("<DELETE %d chars>\n",count1);
			row_changed[scr_row(selBegin)] = 1;
			eol = ff->screen_image + selBegin;
			j = scr_pos(scr_row(selBegin),79) - selBegin - count1;
			for( i = 0; i < j; ++i ) {
				*eol = *(eol+count1);
				++eol;
			}
			for( i = 0; i < count1; ++i )
				*eol++ = ' ';
			break;
		case 'c':	/* device attributes */
			printf("<DEVICE ATTRIBUTES>\n");
			break;
		case 'g':	/* tab clear */
			printf("<TAB CLEAR>\n");
			break;
		case 'h':	/* mode set */
			printf("<MODE SET:%d>\n",count1);
			break;
		case 'l':	/* mode reset */
			printf("<mode reset>\n");
			break;
		case 'm':	/* character attributes */
			if( ff->screen_image == NULL )
				break;
			if( count1 == -1 )
				count1 = 0;
			switch( count1 ) {
			case 0:		/* normal */
				if( trace_xterm )
					printf("<NORMAL mode>\n");
				break;
			case 1:		/* blink (appears as bold) */
				if( trace_xterm )
					printf("<BLINK mode>\n");
				break;
			case 4:		/* underscore */
				if( trace_xterm )
					printf("<UNDERSCORE mode>\n");
				break;
			case 5:		/* bold */
				if( trace_xterm )
					printf("<BOLD mode>\n");
				break;
			case 7:		/* inverse */
				if( trace_xterm )
					printf("<INVERSE mode>\n");
				break;
			}
			break;
		case '?':	/* DEC private mode */
			state = 6;
			count1 = -1;
			break;
		case 'n':	/* device status report */
			printf("<device status report>\n");
			break;
		case 'x':	/* request terminal parameters */
			printf("<request terminal parameters>\n");
			break;
		case 'r':	/* DEC private mode set */
			printf("<DEC private mode (r)>\n");
			break;
		case 's':	/* DEC private mode set */
			printf("<DEC private mode (s)>\n");
			break;
		case ';':	/* cursor positioning */
			state = 4;
			count2 = -1;
			break;
		}
		break;
	case 4:		/* seen: ESCAPE [ Ps1 ; Ps2 */
		switch( ch ) {
		case 'H':	/* cursor position to count1, count2 */
			if( ff->screen_image == NULL )
				break;
			if( count2 == -1 )
				count1 = 1;
			if( trace_xterm )
				printf("<MOVE TO row %d col %d>\n",
								count1,count2);
			if( ff->screen_image != NULL ) {
				row_changed[scr_row(selBegin)] = 1;
				selBegin = scr_pos(count1-1,count2-1);
				row_changed[scr_row(selBegin)] = 1;
			}
			state = 0;
			break;
		case 'T':	/* track mouse */
			printf("<track mouse>\n");
			state = 0;
			break;
		case 'f':	/* hor and vert position row,col */
			if( count2 == -1 )
				count1 = 1;
			printf("<HOR VERT row %d col %d>\n",count1,count2);
			state = 0;
			break;
		case 'r':	/* set scrolling region */
			printf("<set scrolling region %d %d>\n",count1,count2);
			state = 0;
			break;
		default:	/* Ps -- second count */
			printf("<UNEXPECTED: ESC [ Ps ; (%c:%x)>\n",ch,ch);
			state = 0;
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* Ps -- second count */
			if( count2 == -1 )
				count2 = 0;
			count2 = 10*count2 + ch - '0';
			break;
		}
		break;
	case 5:		/* seen: ESCAPE O */
		switch( ch ) {
		case 'A':	/* cursor up */
			if( ff->screen_image == NULL )
				break;
			if( trace_xterm )
				printf("<CURSOR UP>\n");
			i = scr_row(selBegin);
			row_changed[i] = 1;
			row_changed[i-1] = 1;
			selBegin -= ff->screen_cols+1;
			state = 0;
			break;
		case 'B':	/* cursor down */
			if( ff->screen_image == NULL )
				break;
			if( trace_xterm )
				printf("<CURSOR DOWN>\n");
			i = scr_row(selBegin);
			row_changed[i] = 1;
			row_changed[i+1] = 1;
			selBegin += ff->screen_cols+1;
			state = 0;
			break;
		case 'C':	/* cursor right */
			if( ff->screen_image == NULL )
				break;
			if( trace_xterm )
				printf("<CURSOR RIGHT>\n");
			row_changed[scr_row(selBegin)] = 1;
			++selBegin;
			state = 0;
			break;
		case 'D':	/* cursor left */
			if( ff->screen_image == NULL )
				break;
			if( trace_xterm )
				printf("<CURSOR LEFT>\n");
			row_changed[scr_row(selBegin)] = 1;
			--selBegin;
			state = 0;
			break;
		case 'P':	/* pf1 */
			printf("<PF1>\n");
			state = 0;
			break;
		case 'Q':	/* pf2 */
			printf("<PF2>\n");
			state = 0;
			break;
		case 'R':	/* pf3 */
			printf("<PF3>\n");
			state = 0;
			break;
		case 'S':	/* pf4 */
			printf("<PF4>\n");
			state = 0;
			break;
		}
		break;
	case 6:		/* seen: ESCAPE [ ? */
		switch( ch ) {

		default:	/* Ps -- first count */
			printf("<UNEXPECTED: ESC [ ? (%c:%x)>\n",ch,ch);
			state = 0;
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* Ps -- first count */
			if( count1 == -1 )
				count1 = 0;
			count1 = 10*count1 + ch - '0';
			break;
		case ';':	/* part of a string of ;-separated numbers */
			break;
		case 'h':	/* DEC private mode set */
			switch( count1 ) {
			case 1:		/* begin full screen mode */
				ff->screen_rows = 24;
				ff->screen_cols = 80;
				ff->screen_image = PtMalloc(
					(ff->screen_rows)*(ff->screen_cols+1),
					"Xterm screen image" );
				break;
			case 7:
				printf("<WRAPAROUND MODE>\n");
				break;
			case 47:
				if( trace_xterm )
					printf(
					    "<Use Alternate Screen Buffer>\n");
				break;
			default:
				printf("<DEC private mode SET %d>\n",count1);
				break;
			}
			state = 0;
			break;
		case 'l':	/* DEC private mode reset */
			switch( count1 ) {
			case 1:		/* end full screen mode */
				if( ff->screen_image != NULL ) {
					PtFree( ff->screen_image );
					ff->screen_image = NULL;
				}
				break;
			case 7:
				printf("<NO WRAPAROUND MODE>\n");
				break;
			case 47:
				if( trace_xterm )
					printf("<Use Normal Screen Buffer>\n");
				break;
			default:
				printf("<DEC private mode RESET %d>\n",count1);
				break;
			}
			state = 0;
			break;
		}
		break;
	}
}

#ifdef OLDSTUFF
	char tname[20];
	char * current_letter;

	/* generate a new file name */
	strcpy( tname, "shellaaaa.log" );
	current_letter = &(tname[8]);
	while( 1 ) {
		if( access(tname,F_OK) < 0 )
			break;
		while( 1 ) {
			char ch = *current_letter;
			if( ch == 'z' ) {
				--current_letter;
				continue;
			}
			*current_letter = ++ch;
			break;
		}
	}
	/* create the file */
	n = open( tname, O_RDWR|O_CREAT, 0644 );
	/* start with a newline in the log file */
	ch = '\n';
	write( n, &ch, 1 );
	if( n >= 0 )
		close( n );
#endif

int ConnectToPty( arg1, arg2, arg3, arg4, arg5, arg6 )
	char * arg1;
	char * arg2;
	char * arg3;
	char * arg4;
	char * arg5;
	char * arg6;
{
	extern struct window *selWindow;
	extern char msgBuffer[];

	int n;
	char ptyname[40];
	char * arglist[10];
	int pid;
	int ctty;
	char ch;
	char * env_in, *env_out, *p;
	int shellPty;

	/* make sure there is a selection window */
	if( selWindow == NULL ) {
		printf("ConnectToPty: selWindow is NULL\n");
		msg( "ConnectToPty: selWindow is NULL", 0 );
		return -1;
	}
	/* record the fact that this is connected to a pty */
	selWindow->isLogWindow = 1;
	
	/* find a pty that we can use */
	for( ch = 'p'; ch <= 's'; ++ch ) {
		for( n = 0; n <= 15; ++n ) {
			sprintf( ptyname, "/dev/pty%c%x", ch, n );
			shellPty = open( ptyname, O_RDWR );
			if( shellPty > 0 ) {
				ptyname[5] = 't';
				if( access(ptyname,R_OK|W_OK) == 0 )
					goto gotPty;
				else
					close(shellPty);
			}
		}
	}
	msg( "Could not connect to a pty", 0 );
	return 0;
gotPty:
	/* fix up the environment */
	/* force the window size to selWindow->nRows */
	env_in = getenv( "TERMCAP" );
	if( env_in == NULL )
		goto skipEnvironment;
	env_out = (char *)PtMalloc( strlen(env_in)+15, "" );
	if( env_out == NULL )
		goto skipEnvironment;
	strcpy( env_out, "TERMCAP=" );
	strcat( env_out, env_in );
	p = env_out;
	while( 1 ) {
		int ndigits = 0;
		char buffer[10];
		p = strstr( p, "li#" );
		if( p == NULL )
			break;
		/* move to the number */
		p += 3;
		while( isdigit( *(p+ndigits) ) )
			++ndigits;
		sprintf( buffer, "%0*d", ndigits, selWindow->nRows );
		for( n = 0; n < ndigits; ++n )
			*p++ = buffer[n];
	}
	putenv( env_out );
skipEnvironment:
	pid = fork();
	if( pid == 0 ) {
		/* relinquish the controlling terminal */
		/* If you don;t do this, the csh hangs up on an input */
		/* from the controlling terminal.  Other programs may */
		/* do this also although sh and zsh did not, only csh did. */
		if( (ctty = open("/dev/tty",O_RDWR)) >= 0 )
			ioctl( ctty, TIOCNOTTY, 0 );
			/* if this causes a compile error look in all your */
			/* system and standard include file for TIOCNOTTY */
			/* If you cannot find it then just comment our the */
			/* ioctl (not the open).  I have found it is only */
			/* necessary when using the csh so you can just */
			/* comment it out without looking at the includes */
			/* and things will probably work fine. */

		/* put in the args and remove empty args at the end */
		arglist[0] = arg1;
		arglist[1] = arg2;
		arglist[2] = arg3;
		arglist[3] = arg4;
		arglist[4] = arg5;
		arglist[5] = arg6;
		arglist[6] = NULL;
		for( n = 5; n > 0; --n ) {
			if( arglist[n][0] != '\0' )
				break;
			arglist[n] = NULL;
		}

		/* setup stdin, stdout and stderr */
		close( shellPty );	/* this is the shell's open pty */
		shellPty = open( ptyname, O_RDWR );
		close(0); dup(shellPty);
		close(1); dup(shellPty);
		close(2); dup(shellPty);
		close(shellPty);	/* this is now a duplicate */

		n = execvp( arg1, arglist );
		if( n < 0 ) {
			printf("Shell window child: exec of %s failed\n",arg1);
			exit(1);
		}
	}
	if( pid < 0 ) {
		printf("OpenShellWindow: fork failed\n");
		return 0;
	}
	selWindow->childPID = pid;

	/* set up to read and write from the shell */
	selWindow->toShellFD = shellPty;

	/* set up Tcl/Tk reading from pipe2[0] */
	n = fcntl( shellPty, F_SETFL, O_NDELAY );
	if( n < 0 ) {
		printf("OpenShellWindow: fcntl failed\n");
		return 0;
	}
	/* when data in pipe, call ProcessShellInput */
	Tk_CreateFileHandler( shellPty, TK_READABLE, ProcessShellInput,
						(ClientData)selWindow );
	sprintf( msgBuffer,
		"Connected to %s via pseudo-teletype. All input goes to %s",
		arg1, arg1 );
	msg( msgBuffer, 0 );
	return pid;
}

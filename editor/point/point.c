/* $Header: /a/cvs/386BSD/ports/editor/point/point.c,v 1.1 1994/02/15 22:12:39 jkh Exp $ */

#include <setjmp.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <string.h>
#include <time.h>
#include "pt.h"
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <stdio.h>
#ifdef uts
#include <fcntl.h>
#endif /* uts */
#include <tcl.h>

/*SUPPRESS 592*/
static char rcsid[] =
"$Header: /a/cvs/386BSD/ports/editor/point/point.c,v 1.1 1994/02/15 22:12:39 jkh Exp $";

/* X stuff */
Display *MainDisplay;
Window MainWindow;
Pixel ptWhitePixel, ptBlackPixel;
Cursor mainCursor;
Cursor dupCursor;
Cursor busyCursor;
Cursor currentCursor;
Pixmap active_background, inactive_background;

/* display sizes */
int display_width, display_height;

/* to support automatic saving */
long timeOfLastSave;

/* remember directories during getFileName */
char startDirectory[FILENAMESIZE];
char currentDirectory[FILENAMESIZE];
char homeDirectory[FILENAMESIZE];

/* file id for the "pt.msg" command descriptions file */
int descrFileId = -1;

/* text line buffer */
char textBuffer[MSGBUFFERSIZE];

/* message buffer */
char msgBuffer[MSGBUFFERSIZE];

int eventCounter = 0;
extern Tcl_Interp * pointsMainInterp;
extern Tk_Window TkMainWindow;

/*ARGSUSED*/
int
XErrorProc( client_data, err_event_ptr )
	ClientData client_data;
	XErrorEvent * err_event_ptr;
{
	extern Display *MainDisplay;
	char err_msg[80];

	XGetErrorText( MainDisplay, err_event_ptr->error_code, err_msg, 80 );
	fprintf( stderr, "X Protocol Error: %s\n", err_msg );
	return 0;
}

int
main(argc, argv)
	unsigned int argc;
	char **argv;
{
	if( strcmp(argv[0],"pt") == 0 )
		return pt( argc, argv );
	else
		return point( argc, argv );
}

int
point(argc, argv)
	unsigned int argc;
	char **argv;
{
/**************************************************************************/
/*   Declare the external variables                                       */
/**************************************************************************/
	extern struct window *windowList;
	extern char startDirectory[];
	extern char currentDirectory[];
	extern char homeDirectory[];
	extern struct window *selWindow;
	extern int addHandle;
	extern Offset addPosition;
	extern int descrFileId;
	extern int maxFiles;
	extern struct openFile *files;
	extern long timeOfLastSave;
	extern char *tmpnam();
	extern char *getenv();
	extern struct window *windowList;
	extern int display_width, display_height;
	extern int debug;
	extern Pixel ptWhitePixel, ptBlackPixel;
	extern Cursor currentCursor;
	extern Cursor mainCursor;
	extern Cursor dupCursor;
	extern BrowserData *mainBrowser;
	extern Display *MainDisplay;
	extern Window MainWindow;
	extern char * spriteBackground;
	extern char * spriteForeground;
	extern char * busySpriteName;
	extern char * copySpriteName;
	extern char * spriteName;
	extern char * textGeometry;
	extern int noBrowser;

/**************************************************************************/
/*   Declare the local variables                                          */
/**************************************************************************/
	char *p;
	char *add_file;
	int i, k;

	/*********************************************************************/
	/* Get the current directory and drive so we can restore then on exit*/
	/*********************************************************************/
	(void)getwd(startDirectory);
/* LATER: find the length anc PtMalloc the space */
	strcpy(currentDirectory, startDirectory);
	p = getenv("HOME");
	if( p == NULL ) {
		printf("Cannot find $HOME in the environment\n");
		homeDirectory[0] = '\0';
	} else
		strcpy( homeDirectory, p );

	/*********************************************************************/
	/*   See if "pt.msg" (the command descriptions file) is present.     */
	/*********************************************************************/

	p = findFile("pt.msg");
	if( p == NULL )
		descrFileId = -1;
	else
		descrFileId = getFileId(p);

	/*********************************************************************/
	/*   create the additions file                                       */
	/*********************************************************************/

	add_file = tmpnam( NULL );
	addPosition = 0;
	addHandle = open(add_file, O_RDWR | O_CREAT, 0644);
	if( addHandle < 0 ) {
		printf("Create of %s failed\n", add_file);
		exit(1);
	}

	selWindow = NULL;

	/* initialize the timeOfLastSave variable */
	timeOfLastSave = time(NULL);

	/* get application resources from the resource database */
	InitCommands();
	InitOptions();

	/*********************************************************************/
	/*   allocate the window structures                                  */
	/*********************************************************************/
/* LATER -- allocate these as needed */
	/* allocate an extra file structure for use in copymove.c (insScrap) */
	files = (struct openFile *)PtMalloc(
				(maxFiles+1) * sizeof(struct openFile),
				"open file structures");
	if( files == NULL ) {
		printf("Too many files (out of memory). Reduce maxFiles\n");
		exit(1);
	}

	/*********************************************************************/
	/*   call various initialization routines                            */
	/*********************************************************************/

	InitMouse();
	initWindows();
	initFileio();
	initChanges();

	/********************************************************************/
	/*   Process the command line options and arguments		    */
	/********************************************************************/

for(i = 1; i < argc; i++) {
	if( argv[i][0] == '-' ) {
		if( strcmp(argv[i],"-nb")==0
					|| strcmp(argv[i],"-nobrowser")==0 ) {
			noBrowser = 1;
		} else if( strcmp(argv[i],"-debug")==0 ) {
			debug = atoi( argv[++i] );
			printf("debug = %d\n", debug );
		}
	}
	else	/* the end of the comamnd line options */
		break;
}

	/* create the file list window */
	CreateFilelist();
	
#ifdef HYPERTEXT
	if( hypertextOn )
		InitHypertext();
#endif
	/* Initialize  */
	MainDisplay = Tk_Display( mainBrowser->tk_toplevel );
	MainWindow = Tk_WindowId( mainBrowser->tk_toplevel );
	
	/* create an X protocol error handler */
	(void)Tk_CreateErrorHandler( MainDisplay, -1, -1, -1, XErrorProc, 0 );

	/* get the size of the screen */
	display_width = DisplayWidth(MainDisplay, MainDisplay->default_screen);
	display_height = DisplayHeight(MainDisplay,MainDisplay->default_screen);

	/* get some screen information */
	ptBlackPixel = BlackPixel( MainDisplay, MainDisplay->default_screen );
	ptWhitePixel = WhitePixel( MainDisplay, MainDisplay->default_screen );

	/* create the necessary cursors */
	sprintf( msgBuffer, "%s %s %s", spriteName, spriteForeground,
							spriteBackground );
	mainCursor = Tk_GetCursor(pointsMainInterp, mainBrowser->tk_toplevel,
							Tk_GetUid(msgBuffer) );
	currentCursor = mainCursor;
	sprintf( msgBuffer, "%s %s %s", copySpriteName, spriteForeground,
							spriteBackground );
	dupCursor = Tk_GetCursor( pointsMainInterp, mainBrowser->tk_toplevel,
							Tk_GetUid(msgBuffer) );
	sprintf( msgBuffer, "%s %s %s", busySpriteName, spriteForeground,
							spriteBackground );
	busyCursor = Tk_GetCursor( pointsMainInterp, mainBrowser->tk_toplevel,
							Tk_GetUid(msgBuffer) );

	MakeMouseMenuCursors();
	
	/* create the selection handler */
	Tk_CreateSelHandler( mainBrowser->tk_toplevel, XA_STRING,
					SupplySelectionToX, 0, XA_STRING );

	/********************************************************************/
	/*   create the initial windows                                     */
	/********************************************************************/

	for(k = i; k < argc; k++) {
		if( access(argv[k], 0) == -1) {
			sprintf( msgBuffer,
				"MakeModalYesNo {%s} {%s %s %s} {%s} {%s}",
				"Create file?",
				"File", argv[k], "does not exist.",
				"Create it",
				"Skip this file name" );
			p = ExecTclCommand( msgBuffer, NULL );
			if( p[0] != 'y' ) {
				continue;
			}
			close(open(argv[k], O_CREAT, 0644));
		}
		if( createWindow( NULL, argv[k], textGeometry)==NULL ) {
			printf("Point ERROR: Cannot create initial window\n");
			exit(1);
		}
	}

	/* the top window is the first active window */
	MakeWindowActive( windowList );

	/* start the main processing loop */
	Tk_MainLoop();
	
	/* TCL/TK CLEANUP */
	Tk_DestroyWindow( TkMainWindow );
	Tcl_DeleteInterp( pointsMainInterp );
	return 0;
}

int msgInTitleLine = 0;

/*ARGSUSED*/
void
msg(s, putInPopup)
	char *s;
	int putInPopup;
{
	extern int messageFlags;
	extern struct window *windowList;
	
	char buffer[MSGBUFFERSIZE];
	char *from, *to;
	int result;
	
	/* just to be safe */
	if( s == NULL )
		return;

	/* put the message into the popup shell */

	if( messageFlags & TEXT_MSGS ) {
		struct window * w;
		for( w = windowList; w != NULL; w = w->nextWindow) {
			if( !(w->hasMsgLine) )
				continue;
			sprintf( buffer,
				"catch {%s.msg delete 0 end;%s.msg insert 0 {",
				w->tk_pathname, w->tk_pathname);
			/* copy in string and escape braces */
			from = s;
			to = buffer + strlen(buffer);
			while( 1 ) {
				char ch = *from++;
				if( ch == '\0' )
					break;
				if( ch == '{' || ch == '}' )
					*to++ = '\\';
				*to++ = ch;
			}
			*to++ = '}';
			*to++ = '}';
			*to = '\0';
			(void)ExecTclCommand( buffer, &result );
			if( result != TCL_OK ) {
				printf("Text message result is %d\n",
					result);
				w->hasMsgLine = 0;
			}
		}
	}
	if( (messageFlags & WINDOW_MSGS) || putInPopup ) {
		int result;
		sprintf( buffer, "MakeMessageBox {%s}", s );
		(void)ExecTclCommand( buffer, &result );
	}

	if( messageFlags & PRINTF_MSGS ) {
		printf( "%s\n", s );
	}
	
}

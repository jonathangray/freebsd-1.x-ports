#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "tk.h"
#include "tcl.h"

static int windows_opened = 0;
static int wait = 0;
static char * do_not_wait = "{}";
static int location = 1;
static char * interp_name = "point";
#ifdef CountRetries
static int retries = 0;
#endif

static Tk_Window w;
static Tcl_Interp *interp;
#define NumberOfRetries	4

/* ARGSUSED */
static int
WindowWasClosed( clientData, interp, argc, argv )
	ClientData clientData;
	Tcl_Interp * interp;
	int argc;
	char *argv[];
{
	--windows_opened;
	if( windows_opened <= 0 ) {
		Tk_DestroyWindow( w );
		Tcl_DeleteInterp( interp );
		exit(0);
	}
	return 0;
}


int
pt(argc, argv)
	int argc;
	char **argv;
{
	int result;
	char buffer[1024];
	char cwd[1024];
	char name[64];
	int iarg;
	char ch;

	/* the the current directory so we can prepend it to file names */
	/* Then we get the cwd of pt not point */
	(void)getwd( cwd );

	/* create the interpreter, set the class and add the command */
	interp = Tcl_CreateInterp();
	w = Tk_CreateMainWindow(interp, (char *) NULL, argv[0], "Point");
	if (w == NULL) {
		printf("%s\n", interp->result);
		exit(1);
	}
	Tk_SetClass(w, "Pt");
	Tcl_CreateCommand(interp, "WindowWasClosed", WindowWasClosed, 0, NULL);
	Tcl_Eval( interp, "wm withdraw ." );

for( iarg = 1; iarg < argc; ++iarg ) {
	if( argv[iarg][0] == '-' ) {
		ch = argv[iarg][1];
		if( strcmp(argv[iarg],"-c")==0
				|| strcmp(argv[iarg],"-create")==0 ) {
			do_not_wait = "doNotAsk";
		} else if( strcmp(argv[iarg],"-interp")==0 ) {
			interp_name = argv[++iarg];
		} else if( strcmp(argv[iarg],"-w")==0
				|| strcmp(argv[iarg],"-wait")==0 ) {
			wait = 1;
		} else if( '0' <= ch && ch <= '9' ) {
			location = ch - '0';
		} else {
			printf("%s: Unknown option: %s\n",argv[iarg], argv[0]);
		}
	} else {
		ch = argv[iarg][0];
		/* is this an absolute pathname? */
		if( ch == '/' || ch == '~' ) {
			sprintf( buffer,
				"send %s RemoteOpen %.200s %d %s",
				interp_name, argv[iarg], location,
				do_not_wait );
		} else {
			sprintf( buffer,
				"send %s RemoteOpen %.700s/%.200s %d %s",
				interp_name, cwd, argv[iarg], location,
				do_not_wait );
		}
#ifdef CountRetries
	try_again:
#endif
		result = Tcl_Eval( interp, buffer );
		if( *interp->result != 0 && result != TCL_OK ) {
#ifdef CountRetries
			if( retries++ < NumberOfRetries ) {
				printf(
"Point has not replied in the timeout period. Trying again (retry %d of %d)\n",
					retries, NumberOfRetries );
				goto try_again;
			}
#endif
			printf( "%s did not reply to open window %s (%s)\n",
				interp_name, argv[iarg], interp->result );
		} else if( wait ) {
			++windows_opened;
			strncpy( name, interp->result, 64 );
			sprintf( buffer,
			"send %s SendOnClose %.200s %s WindowWasClosed",
				interp_name, name, argv[0] );
			result = Tcl_Eval( interp, buffer );
		}
	}
}

	if( windows_opened > 0 )
		Tk_MainLoop();
	Tk_DestroyWindow( w );
	Tcl_DeleteInterp( interp );
	exit(0);
}

/* $Header: /a/cvs/386BSD/ports/editor/point/browser.c,v 1.1 1994/02/15 22:12:35 jkh Exp $ */

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "pt.h"
#ifdef SYSV
/*** NECESSARY? #include <libgen.h> ***/
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <sys/stat.h>
/***#include <stdio.h>***/
#include <X11/keysym.h>
#include <X11/StringDefs.h>


#define NO_OPEN_WINDOWS "No Open Windows"

#define FILENAMESSIZE		10000
#define FILELISTSIZE		 1000
#define FILETREECHARSIZE	10000
#define FILETREELISTSIZE	 1000
#define HOME			    1
#define N_OLD_DIRS		   30

BrowserData *mainBrowser = NULL;
BrowserData *activeBrowser = NULL;
BrowserData *browserList = NULL;

/* open window list data */
OpenWindowStruct *openWindowList;
int numberOfOpenWindows;

/* cache of old directory data */
int nextOldDirs = 0;
FileListData *oldDirs[N_OLD_DIRS];

#ifdef VAX_FIX
char *
getcwd( buffer, length )
	char * buffer;
	int length;
{
	char *getwd();

	return getwd( buffer );
}
int
isspace( c )
	char c;
{
	return c==' '||c=='\t'||c=='\n'||c=='\r'||c=='\f';
}
double
strtod( str, ptr )
	char * str;
	char ** ptr;
{
	double d = atof( str );

	/* this is not exactly correct put will do for here */
	if( ptr != NULL )
		*ptr = str + strlen(str);
	return d;
}
#endif

static void
InitializeDirectoryCache()
{
	int i;
	FileListData *fl;
	int size = sizeof(FileListData);
	
	for( i = 0; i < N_OLD_DIRS; ++i ) {
		fl = (FileListData *)PtMalloc( size, "list of files" );
		fl->fileNames = NULL;
		fl->listOfFilesShowing = NULL;
		fl->numberOfFilesShowing = 0;
		fl->ino = 0;
		fl->mtime = 0;
		fl->directoryName = NULL;
		fl->age = 0;
		fl->use_count = 0;
		fl->flags = 0;
		fl->filePattern[0] = '\0';
		oldDirs[i] = fl;
	}
}

BrowserData *
FindBrowserByTkName( name )
	char * name;
{
	extern BrowserData * browserList;

	BrowserData * browser = browserList;

	while( browser != NULL ) {
		if( strcmp( browser->tk_pathname, name ) == 0 )
			break;
		browser = browser->nextBrowser;
	}
	return browser;
}

void
ReduceUseCount( fl )
	FileListData *fl;
{
	int i;

	for( i = 0; i < N_OLD_DIRS; ++i ) {
		if( fl == oldDirs[i] ) {
			--(fl->use_count);
			break;
		}
	}
}

extern struct window *activeWindow;
extern Display *MainDisplay;

#ifdef LATERLATER
void
ptBrowserLetter(w, event, args, nargs)
	int w;
	XKeyEvent *event;
	String *args;
	Cardinal *nargs;
{
	extern char msgBuffer[];
	extern char * textGeometry;
	extern int debug;

	char buf[4];
	int ret;
	KeySym keysym;
	static char prefix[100];
	static int iprefix = 0;
	static int last_index = -1;

	if( event->type != KeyPress )
		return;
	ret = XLookupString(event, buf, 4, &keysym, NULL);
	buf[ret] = '\0';
	if( ret != 1 ) {
		switch( keysym ) {
		case XK_Shift_L:
		case XK_Shift_R:
		case XK_Control_L:
		case XK_Control_R:
		case XK_Caps_Lock:
		case XK_Shift_Lock:
		case XK_Meta_L:
		case XK_Meta_R:
		case XK_Alt_L:
		case XK_Alt_R:
		case XK_Super_L:
		case XK_Super_R:
		case XK_Hyper_L:
		case XK_Hyper_R:
			/* modifier keys, ignore them */
			return;
		}
	}
	if( ret == 1 ) {
		char **list = activeBrowser->fileListData->listOfFilesShowing;
		int num = activeBrowser->fileListData->numberOfFilesShowing;
		int i;
		if( buf[0] == '\r' ) {
			if( last_index != -1 ) {
#ifdef LATER
				OpenListFile( buf, textGeometry );
#endif
				printf("Unhighlight list item\n");
			} else if( iprefix == 0 ) {
				XBell( MainDisplay, 0 );
				printf("No file selected\n");
			} else {
				GetNewFile( NULL, prefix, textGeometry );
			}
		} else {
	startOver:
			prefix[iprefix++] = buf[0];
			prefix[iprefix] = '\0';
			for( i = 0; i < num; ++i ) {
				if( strncmp(prefix,list[i],iprefix) == 0 ) {
					printf("Highlight list item\n");
					last_index = i;
					break;
				}
			}
			if( i >= num ) {
				last_index = -1;
				printf("Unhighlight list item\n");
			}
		}
	} else {
		iprefix = 0;
		last_index = -1;
		printf("Unhighlight list item\n");
	}
}
#endif

void
ChangeBrowserFontTo( browser, fontName )
	BrowserData *browser;
	char *fontName;
{
	extern char msgBuffer[];
	extern Tcl_Interp * pointsMainInterp;

	XFontStruct *fontInfo;

retry_font:
	fontInfo = Tk_GetFontStruct( pointsMainInterp, browser->tk_toplevel,
						Tk_GetUid(fontName) );
	if( fontInfo == NULL ) {
		printf("Cannot load font %s, using \"fixed\"\n", fontName );
		fontName = "fixed";
		goto retry_font;
	}
	(browser->browserFont).height = fontInfo->ascent + fontInfo->descent;
	(browser->browserFont).width  = fontInfo->max_bounds.width;
	(browser->browserFont).ascent = fontInfo->ascent;
	Tk_FreeFontStruct( fontInfo );

	sprintf( msgBuffer, "%s.fileList.list configure -font %s",
					browser->tk_pathname, fontName );
	(void)ExecTclCommand( (char *)msgBuffer, NULL );

	sprintf( msgBuffer, "%s.openList.list configure -font %s",
					browser->tk_pathname, fontName );
	(void)ExecTclCommand( (char *)msgBuffer, NULL );
}

void
RaiseListWindow( n, geometry )
	int n;
	char * geometry;
{
	extern char msgBuffer[];

	struct window * w;

	if( n < 0 || n >= numberOfOpenWindows ) {
		printf("MakeActiveWindow: index=%d should be < %d\n",
			n, numberOfOpenWindows );
		return;
	}
	if( striccmp(openWindowList[n].name,NO_OPEN_WINDOWS) == 0 )
		return;
	w = openWindowList[n].w;
	if( w != NULL ) {
		activeWindow = w;
		sprintf( msgBuffer, "MoveWindow %s", geometry );
		(void)ExecTclCommand( (char *)msgBuffer, NULL );
	} else
		printf("ERROR: window %s not found in list of open windows\n",
			openWindowList[n].name);
}

static char *
FormatBrowserTitle( format, browser )
	char * format;
	BrowserData *browser;
{
	extern char msgBuffer[];
	extern char textBuffer[];

	char * to = msgBuffer;
	char * copy_in;
	char ch;

	while( (ch = *format++) != '\0' ) {
		if( ch != '%' ) {
			*to++ = ch;
			continue;
		}
		/* read a '%' */
		switch( ch = *format++ ) {
		default:	/* anything, including a literal '%' */
			textBuffer[0] = ch;
			textBuffer[1] = '\0';
			copy_in = textBuffer;
			break;
		case 'a':	/* active browser */
			{/* get the active browser indicator string */
				char * to = textBuffer;
				char delimiter = *format++;
				while( 1 ) {
					ch = *format++;
					if( ch == delimiter )
						break;
					/* allow for missing delimiters */
					if( ch == '\0' ) {
						--format;
						msg(
"Missing read only flag in title format", 0  );
						break;
					}
					*to++ = ch;
				}
				*to = '\0';
			}
			if( browser == activeBrowser ) {
				copy_in = textBuffer;
			} else
				copy_in = "";
			break;
		case 'd':	/* directory name */
			strcpy( textBuffer, browser->cwd );
			copy_in = tildefyFilename( (char *)textBuffer );
			break;
		}
		while( *copy_in != '\0' )
			*to++ = *copy_in++;
	}
	*to = '\0';
	
	return msgBuffer;
}

int
listComp( a, b )
	void *a;
	void *b;
{
	extern int showDirsFirst;
	extern int showSizes;

	if( showDirsFirst ) {
		char last_in_a;
		char last_in_b;
		char * end_of_a = strlen(*(char **)a) - 1 + *(char **)a;
		char * end_of_b = strlen(*(char **)b) - 1 + *(char **)b;
		/* ignore the file size if present */
		if( showSizes ) {
			/* back up past the ' (NNNk)' */
			while( *end_of_a-- != ' ' )
				/*EMPTY*/
				;
			while( *end_of_b-- != ' ' )
				/*EMPTY*/
				;
		}
		/* get the last character in the file name */
		last_in_a = *end_of_a;
		last_in_b = *end_of_b;
		if( last_in_a == '/' ) {
			if( last_in_b != '/' )
				return -1;
		} else {
			if( last_in_b == '/' )
				return 1;
		}
	}
	return strcmp(*(char **)a, *(char **)b);
}

int
openComp( a, b )
	void *a;
	void *b;
{
	return strcmp(  ((OpenWindowStruct *)a)->name,
			((OpenWindowStruct *)b)->name );
}

/*SUPPRESS 68*/ /*SUPPRESS 544*/

void
SetBrowserNames( browser )
	BrowserData *browser;
{
	extern char textBuffer[];
	extern char * browserIconFormat;
	extern char * browserTitleFormat;

	if( browser == NULL || browser->tk_pathname[0]== '\0')
		return;

	/* set the browser window and icon titles */
	sprintf( textBuffer, "wm title %s {%s}", browser->tk_pathname,
			FormatBrowserTitle( browserTitleFormat, browser) );
	(void)ExecTclCommand( textBuffer, NULL );
	sprintf( textBuffer, "wm iconname %s {%s}", browser->tk_pathname,
			FormatBrowserTitle( browserIconFormat, browser) );
	(void)ExecTclCommand( textBuffer, NULL );
}

void
NewFilelist( browser )
	BrowserData *browser;
{
	extern char * filePattern;
	extern char msgBuffer[];
	extern char currentDirectory[];
	extern int showSizes;
	extern int ignoreCase;

	DIR *dirp;
#ifdef SYSV
	struct dirent *dp;
#else
	struct direct *dp;
#endif
	int namelength;
	char *endFileNames, *curFileNames;
	char **endList, **curList;
	struct stat statbuf;
	int longestName;
	int i, oldest_unused, oldest_age, new_dir;
	char *p;
	FileListData *fl;
	char flags;
	char buffer[256];
	char * from, * to, * re_bad;
	int saveIgnoreCase;

	/* to save computation keep local copies of these */
	char *fileNames;
	char **listOfFilesShowing;
	int numberOfFilesShowing;

	/* protect myself (this happens during initialization) */
	if( browser == NULL )
		return;
	/* make up the flags byte */
	flags = 0;
	if( showSizes )
		flags |= SHOW_SIZES_FLAG;

	/* get the current directory and change the label to display it */
	(void)getcwd(currentDirectory, FILENAMESIZE);
	strncpy(browser->cwd, currentDirectory, FILENAMESIZE);

	/* see if this directory is in our cache (and has not changed) */
	stat( currentDirectory, &statbuf );	/* get inode and mtime */
	oldest_unused = 0;	/* always have a legal value */
	oldest_age = -1;
	new_dir = -1;
	for( i = 0; i < N_OLD_DIRS; ++i ) {
		fl = oldDirs[i];	/* constantly used so get ptr */
		/* increment the age and look for the oldest, unused one */
		++(fl->age);
		if( fl->use_count == 0 && fl->age > oldest_age ) {
			oldest_age = fl->age;
			oldest_unused = i;
		}
		/* look for the old directory in the cache */
		if( fl == browser->fileListData ) {
			--(fl->use_count);
		}
		/* if this directory is in the cache and unchanged, use it */
		if( fl->ino==statbuf.st_ino && fl->mtime==statbuf.st_mtime
		 && fl->flags==flags && fl->showSizes==showSizes
		 && strcmp(fl->filePattern,filePattern)==0 ) {
		 	fl->age = 0;	/* restart aging */
		 	++(fl->use_count);	/* another user of this one */
		 	new_dir = i;
		 }
	}
	if( new_dir >= 0 ) {	/* found it in the cache */
		fl = oldDirs[new_dir];
		browser->fileListData = fl;
		longestName = fl->longestname;
		listOfFilesShowing = fl->listOfFilesShowing;
		numberOfFilesShowing = fl->numberOfFilesShowing;
		goto changeDisplay;
	}
	/* else erase the oldest entry in the directory cache, */
	fl = oldDirs[oldest_unused];

	/* free the strings whose pointers we will write over */
	PtFree( fl->fileNames );
	PtFree( (char *)(fl->listOfFilesShowing) );
	PtFree( fl->directoryName );

	/* initialize the fields */
	browser->fileListData = fl;
	fl->age = 0;
	fl->use_count = 1;
	fl->ino = statbuf.st_ino;
	fl->mtime = statbuf.st_mtime;
	fl->flags = flags;
	strncpy( fl->filePattern, filePattern, 128 );

	/* remember the directory name */
	p = (char *)PtMalloc( strlen(currentDirectory)+1, "directory name" );
	strcpy( p, currentDirectory );
	fl->directoryName = (char *)p;
	
	/* allocate the space for the lists */
	fileNames = PtMalloc(FILENAMESSIZE*sizeof(char), "file names" );
	endFileNames = fileNames + FILENAMESSIZE;
	curFileNames = fileNames;
	listOfFilesShowing = (char **)PtMalloc(FILELISTSIZE*sizeof(char *),
						(char *)"files showing" );
	endList = listOfFilesShowing + FILELISTSIZE;
	curList = listOfFilesShowing;

	/* set up the regular expression to check the filenames against */
	to = buffer;
	from = fl->filePattern;
	*to++ = '^';
	while( 1 ) {
		char ch = *from++;
		switch( ch ) {
			case '\0':
				*to++ = '$';
				*to = '\0';
				goto out;
			case '*':
				*to++ = '.';
				break;
			case '?':
				ch = '.';
				break;
			case '.':
				*to++ = '\\';
				break;
		}
		*to++ = ch;
	}
out:
	/* case matters in file names */
	saveIgnoreCase = ignoreCase;
	ignoreCase = 0;
	if( (re_bad = re_comp(buffer)) != NULL ) {
		printf("RE error: %s\n", re_bad);
	}
	/* it is only the re_comp that looks at ignoreCase */
	ignoreCase = saveIgnoreCase;
	
	dirp = opendir(".");
	if( dirp == NULL ) {
		extern int errno;
		extern int sys_nerr;
		extern char *sys_errlist[];
		if( errno < sys_nerr )
			p = (char *)sys_errlist[errno];
		else
			p = (char *)"";
		sprintf( msgBuffer, "Open directory failed: %s", (char *)p );
		msg( (char *)msgBuffer, 0  );
		*curList = "Could not open directory";
		longestName = strlen(*curList);
		numberOfFilesShowing = 1;
		goto skipDirectorySearch;
	}
	/* put in the Tk command */
	numberOfFilesShowing = 0;
	longestName = 0;
	dp = readdir(dirp);	/* skip '.' (which is always first) */
	for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp) ) {
#ifdef SYSV
		namelength = strlen(dp->d_name);
#else
		namelength = dp->d_namlen;
#endif
		stat(dp->d_name, &statbuf);
		if( (statbuf.st_mode & S_IFMT)!=S_IFDIR &&  re_bad==NULL ) {
			i = re_match( dp->d_name );
			if( i < 1 ) {
				continue;
			}
		}
		if( curFileNames + namelength >= endFileNames ) {
			printf("name list overflow\n");
/* LATER REALLOCATE THE LIST LARGER */
			break;
		}
		if( curList >= endList ) {
			printf("pointer list overflow\n");
			/* later reallocate the list larger */
			break;
		}
		*curList++ = curFileNames;
		strcpy(curFileNames, dp->d_name);
		curFileNames += namelength + 1;	/* +1 for the nul */
		if( (statbuf.st_mode & S_IFMT) == S_IFDIR ) {
			*--curFileNames = '/';
			*++curFileNames = '\0';
			++curFileNames;
			++(namelength);
		}
		if( showSizes ) {
			char s[25];
			int i, l;
			sprintf( s, " (%dk)", (statbuf.st_size+1023)/1024 );
			l = strlen( s );
			--curFileNames;		/* back up over the '\0' */
			for( i = 0; i <= l; ++i ) {
				*curFileNames++ = s[i];
			}
			/* this loop will copy the '\0' also */
			namelength += l;
		}
		if( namelength > longestName )
			longestName = namelength;
		++numberOfFilesShowing;
	}
	closedir(dirp);

	/* sort the file names */
	qsort( (char *)listOfFilesShowing, numberOfFilesShowing,
			sizeof(char *), listComp);

skipDirectorySearch:
	/* put everything back into the structure */
	fl->fileNames = fileNames;
	fl->listOfFilesShowing = listOfFilesShowing;
	fl->numberOfFilesShowing = numberOfFilesShowing;
	fl->longestname = longestName;
	fl->showSizes = showSizes;

changeDisplay:
#if defined(USE_COLBOX)
	/* figure out how many columns to use */
		/* for big browsers only */
	{
		int columnSpacing = 7;
		int overhead = 46;

		wide = Tk_Width( browser->tk_toplevel ) - overhead;
		col = longestName*((browser->browserFont).width)+columnSpacing;
		nCols = (wide + columnSpacing) / col;
		if( nCols < 1 )
			nCols = 1;
		sprintf(msgBuffer,"%s.fileList.list configure -columns %d",
						browser->tk_pathname, nCols );
		(void)ExecTclCommand( (char *)msgBuffer, NULL );
	}
#endif

	/* make this the new list */
	/* first delete all the old list elements */
	sprintf( msgBuffer, "catch {%s.fileList.list delete 0 end}",
							browser->tk_pathname );
	(void)ExecTclCommand( (char *)msgBuffer, NULL );
	
	/* then insert each file name in the new list */
	for( i = 0; i < numberOfFilesShowing; ++i ) {
		sprintf( msgBuffer, "%s.fileList.list insert end \"%s\"",
				browser->tk_pathname, listOfFilesShowing[i] );
		(void)ExecTclCommand( (char *)msgBuffer, NULL );
	}

	SetBrowserNames( browser );
}

void
NewOpenList( )
{
	extern struct openFile *files;
	extern struct window *windowList;
	extern char msgBuffer[];
	extern int pathNames;
	extern struct openFile *files;
	extern BrowserData *browserList;

	int col;
	int n, len, longestName;
	struct window *w;
	char *s, *new_s;
	BrowserData *browser;

	/* free the old space first */
	if( openWindowList != NULL ) {
		int i = 0;
		OpenWindowStruct *sp = openWindowList;
		while( i++ < numberOfOpenWindows )
			PtFree( (sp++)->name );
		PtFree((char *)openWindowList);
	}
		
	/* see how many windows there are */
	w = windowList;
	numberOfOpenWindows = 0;
	while( w != NULL ) {
		++numberOfOpenWindows;
		w = w->nextWindow;
	}

	/* always have at least one space in openWindowList */
	if( (n = numberOfOpenWindows) == 0 )
		n = 1;
	openWindowList = (OpenWindowStruct *) PtMalloc(
			n * sizeof(OpenWindowStruct), "open window list" );

	if( numberOfOpenWindows > 0 ) {
		w = windowList;
		longestName = 0;
		n = 0;
		while( n < numberOfOpenWindows ) {
			/* see if we should use the full pathname */
			col = (pathNames ? 0 : w->nameOffset);
			s = &((files[w->fileId].origName)[col]);
			len = strlen(s) + 2;
			/* allocate space for the name and copy it in */
			new_s = (char *)PtMalloc(len, "file name");
			strncpy( new_s, s, len );
			if( files[w->fileId].flags & IS_CHANGED ) {
				/* if changed add a '*' at the end */
				--len;	/* we added one for the '\0' */
				new_s[len-1] = '*';
				new_s[len] = '\0';
			} else
				/* otherwise correct the length */
				len -= 2;
			openWindowList[n].name = new_s;
			if( len > longestName )
				longestName = len;
			/* remember the window ID so we can raise it easily */
			openWindowList[n].w = w;
			w = w->nextWindow;
			++n;
		}
		qsort( (char *)openWindowList, numberOfOpenWindows,
				sizeof(OpenWindowStruct), openComp );
	
	} else {
		s = (char *)PtMalloc( strlen(NO_OPEN_WINDOWS)+1, "file name" );
		strcpy( s, NO_OPEN_WINDOWS );
		openWindowList[numberOfOpenWindows].w = NULL;
		openWindowList[numberOfOpenWindows++].name = s;
		longestName = strlen(s);
	}

	/* loop through all the browsers and change their open lists */
	for( browser = browserList; browser != NULL;
					browser = browser->nextBrowser) {

		if( browser->tk_pathname[0] == '\0' )
			/* main browser was not created, so skip it */
			continue;
		/* make this the new list */
		/* first delete all the old list elements */
		sprintf( msgBuffer, "catch {%s.openList.list delete 0 end}",
							browser->tk_pathname );
		(void)ExecTclCommand( (char *)msgBuffer, NULL );

		/* then insert each file name in the new list */
		for( n = 0; n < numberOfOpenWindows; ++n ) {
			sprintf( msgBuffer, "%s.openList.list insert end {%s}",
				browser->tk_pathname, openWindowList[n].name );
			(void)ExecTclCommand( (char *) msgBuffer, NULL );
		}

#if defined(USE_COLBOX)
		int columnSpacing = 7;
		wide = Tk_Width( browser->tk_toplevel ) - 50;
		col = longestName*((browser->browserFont).width)+columnSpacing;
		nCols = (wide + columnSpacing) / col;
		if( nCols < 1 )
			nCols = 1;
		sprintf(msgBuffer,"%s.openList.list configure -columns %d",
						browser->tk_pathname, nCols );
		(void)ExecTclCommand( (char *) msgBuffer, NULL );
#endif
	}

}

void
CreateNewBrowser( geometry )
	char * geometry;
{
	extern BrowserData * activeBrowser;
	extern char * browserFont;
	extern Tcl_Interp * pointsMainInterp;

	BrowserData * browser;
	BrowserData * oldActiveBrowser;
	char * name;
	char buffer[100];

	/* create the browser data structure */
	browser = (BrowserData *)PtMalloc(sizeof(BrowserData),"browser data");
	browser->fileListData = NULL; /* this is allocated in NewFilelist */

	/* put on the browserList */
	browser->nextBrowser = browserList;
	browser->prevBrowser = NULL;
	browserList->prevBrowser = browser;
	browserList = browser;
	browser->tk_toplevel = NULL;

	sprintf (buffer, "BrowserWindow %s", geometry );
	name = ExecTclCommand( buffer, NULL );
	browser->hasMsgLine = 1;
	browser->tk_pathname = Tk_GetUid( name );
	browser->tk_toplevel = Tk_NameToWindow( pointsMainInterp,
			browser->tk_pathname, mainBrowser->tk_toplevel );

	Tk_SetWindowBackground( browser->tk_toplevel,
			WhitePixelOfScreen(Tk_Screen(browser->tk_toplevel)));
	XSetForeground( Tk_Display(browser->tk_toplevel),
			DefaultGCOfScreen(Tk_Screen(browser->tk_toplevel)),
			BlackPixelOfScreen(Tk_Screen(browser->tk_toplevel)));
			
	ChangeBrowserFontTo( browser, browserFont );

	oldActiveBrowser = activeBrowser;
	activeBrowser = browser;
	SetBrowserNames( oldActiveBrowser );
	(void)ExecTclCommand( "update", NULL );
	NewOpenList();

	/* Now create the list of files */
	NewFilelist( browser );
}


Tk_Window TkMainWindow;

void
CreateFilelist()
{
	extern Tcl_Interp * pointsMainInterp;
#if defined(USE_COLBOX)
	extern int Pt_ColboxCmd();
#endif
	extern char * browserGeometry;
	extern int noBrowser;
	extern char msgBuffer[];

	InitializeDirectoryCache();

	/* create the browser data structure */
	mainBrowser = (BrowserData *)PtMalloc( sizeof(BrowserData),
						"browser data" );
	mainBrowser->fileListData = NULL; /* this is allocated in NewFileList */
	numberOfOpenWindows = 0;
	openWindowList = NULL;

	/* make this the only entry in the browserList */
	mainBrowser->nextBrowser = NULL;
	mainBrowser->prevBrowser = NULL;
	browserList = mainBrowser;

	/* finish creating the browser */
	/* create the main tcl interpreter and add the Point commands */
	pointsMainInterp = Tcl_CreateInterp();
	AddPointCommands( pointsMainInterp );
	
	/* create the main window and set up some options */
	TkMainWindow = Tk_CreateMainWindow( pointsMainInterp, (char *) NULL,
							"point", "Point" );
	mainBrowser->tk_toplevel = TkMainWindow;
	if( mainBrowser->tk_toplevel == NULL ) {
		printf("%s\n", pointsMainInterp->result);
		exit(1);
	}

	Tk_SetClass( mainBrowser->tk_toplevel, "Point");

	/* get options that come for the X resource database */
	GetResourceOptions();

#if defined(USE_COLBOX)
	Tcl_CreateCommand( pointsMainInterp, "colbox", Pt_ColboxCmd,
			(ClientData)(mainBrowser->tk_toplevel), NULL );
#endif

	/* read in the Point proc's and create a browser window */
	sprintf( msgBuffer,
"global PointTclLibrary\n\
set PointTclLibrary \"%s\"\n\
if [file exists $PointTclLibrary/startup.tcl] \"\n\
	source $PointTclLibrary/startup.tcl\n\
\" {puts stderr \"CANNOT FIND A STARTUP FILE ($PointTclLibrary/startup.tcl). POINT WILL NOT WORK.\"}",
		POINT_LIBRARY );
	(void)ExecTclCommand( msgBuffer, NULL );

	(void)ExecTclCommand( "if [file exists .ptdirrc] {source .ptdirrc}",
								NULL );
	Tk_MakeWindowExist( mainBrowser->tk_toplevel );
	mainBrowser->tk_pathname = "";
	if( !noBrowser )
		CreateNewBrowser( browserGeometry );
}


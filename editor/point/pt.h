/* $Header: /a/cvs/386BSD/ports/editor/point/pt.h,v 1.1 1994/02/15 22:12:42 jkh Exp $ */

/****************  Define this if you have a System V system ***************/
/*** #define SYSV ***/
#include "ana.h"
#include "command.h"
#include "tcl.h"
#include "tk.h"
/**** #include "Intrinsic.h" -- just use a few typedefs ****/
typedef short Position;
typedef unsigned short  Dimension;
typedef char * String;
typedef unsigned long   Pixel;
typedef unsigned int	Cardinal;
/* "file.h" is included about 20 lines down */
/* "funcdecl.h" is included at the end of this file (pt.h) */

#define SCROLLBAR_LIMIT 10000

#define BLOCK_EOF	256

/******* parameters -- can be changed *****/
#define NBUFFERS      300  /* buffers for file blocks (in address space) */
#define NHISTORY      300  /* history kept in memory */
#define NBUFHASH      512  /* buffer hash chains */
#define MSGBUFFERSIZE 1024  /* size of two generally used buffers */
#define STRINGSIZE    256  /* size of input strings for search */

/****** constants that are rarely changed */
#define BUFFERSIZE	1024	/* size of a buffer in bytes */
#define BUFFERSHIFT	10	/* shift to convert: bytes<-->buffers */
#define MAXCOLS		1024	/* max number of columns on the display */
#define FILENAMESIZE	512

#include "file.h"

/* mouse menu data to keep */
struct mmData {
	Cursor cursor;
	char * tcl_command;
	char * label;
	int length;
};

/* browser related typedefs */
typedef struct {
	char *name;
	struct window *w;
} OpenWindowStruct;

#define SHOW_SIZES_FLAG		0x1
#define SHOW_DIR_SLASHES_FLAG	0x2

#define T_END_OF_TABLE	0
#define T_INTEGER	1
#define T_BOOLEAN	2
#define T_STRING	3
#define T_GEOMETRY	4

struct optionTableEntry {
	char * option_name;
	char * option_address;
	int option_type;
};

typedef struct {
	char *fileNames;
	char **listOfFilesShowing;
	int numberOfFilesShowing;
	int longestname;
	int showSizes;
	ino_t ino;
	time_t mtime;
	char *directoryName;
	int age;
	int use_count;
	char filePattern[512];
	char flags;
} FileListData;

struct fontDataStruct {
	int height, width, ascent;
	char *name;
	Font font;
	GC gc_normal, gc_selected, gc_deselected, gc_underline;
};

/* messageBits contains message producing flags */
/* 0 => no messages */
/* 1 => popup message windows */
/* 2 => browser message lines (if there is one) */
/* 4 => on window headers */
/* 8 => on the xterm that started Point */
/* 16 => window message lines (if there is one) */
#define POPUP_MSGS	1
#define BROWSER_MSGS	2
#define WINDOW_MSGS	4
#define PRINTF_MSGS	8
#define TEXT_MSGS	16

typedef struct browser_data {

	/* Tk stuff */
	Tk_Window tk_toplevel;
	Tk_Uid tk_pathname;
	int hasMsgLine;

	struct fontDataStruct browserFont;

	/* filename list data */
	FileListData *fileListData;
	char cwd[FILENAMESIZE];
	struct browser_data *nextBrowser;
	struct browser_data *prevBrowser;

} BrowserData;

/* string names */
typedef enum {
	HANDLEMSG,
	FULLMSG,
	MENUSPMSG,
	NOSPACEMSG,
	LOWSPACEMSG,
	OUTOFWINDOWS,
	NOBUFFERMEMORY,
	OUTOFFILESTRUCT,
	CANNOTOPEN,
	READONLYFILE,
	WRITINGFILE,
	CLOSEFAILED,
	RENAMEFAILED,
	DELETEFAILED,
	RENAMEFAILED2,
	WRITECANCELLED,
	FILEEXISTS,
	FILEWRITTEN,
	WRITEFAILED,
	CREATEFAILED,
	WRITEPROGRESS,
	WASREADONLY,
	YTOSAVE,
	CLOSECANCELLED,
	NOTOPEN,
	CLOSEFAILED2,
	REANMEFAILED2,
	DELETEFAILED2,
	RENAMEFAILED3
} StringNames;

/* used in the piece table -- original or new characters file */
typedef enum {
	ORIGFILE,
	ADDFILE
} WhichFile;

/* deleteSelection update type */
typedef enum {
	NOUPDATE,
	UPDATEWINDOWS
} DoUpdate;

/* directions */
typedef enum {
	FORWARD,
	REVERSED
} Direction;

/* copyMove modes */
typedef enum {
	COPY,
	MOVE,
	AGAIN
} CopyMoveMode;

/* selection modes */
typedef enum {
	SELCHAR,
	SELWORD,
	SELLINE,
	SELBLOCK
} SelectionMode;

struct keyTableEntry {
	char *key_name;
	int key_sym_value;
};

/* change types */
enum ChangeType {
	CNULL,
	CINSERT,
	CDELETE,
	CMOVE,
	CCOPY,
	CMOTION,
	CREPLACE
};

/* flags constants */
#define	CHANGE_WAS_UNDONE	0x1
#define FILE_WAS_CLOSED		0x2
#define BLOCK_UNDO_BEGIN	0x4
#define BLOCK_UNDO_END		0x8

/* change history */
struct changeItem {
	int fileId;
	struct window *w;
	Offset position;
	int  lineNumber;
	Offset length;
	struct piece *firstPiece;
	char type;
	char flags;
	struct changeItem * next;
	struct changeItem * prev;
};

/* option type values */
typedef enum {
	OBOOLEAN,
	OINTEGER,
	OOTHERS,
	UBOOLEAN,
	UINTEGER,
	UOTHERS,
	OSTRING,
	USTRING
} OptionType;

/* option index values */
typedef enum {
	OFILESORT,
	OMSGCOLORS,
	ONBUFFERS,
	OTEXTCOLORS,
	OBORDERCOLORS,
	OFSDIRS,
	OUNDOSIZE,
	O43LINES,
	OFSPATTERNS,
	OBACKUPDEPTH,
	OREDEFINE,
	OLASTITEM
} OptionIndex;

/* the window structure */
struct window {
	/* linked list of active windows */
	struct window *nextWindow;
	struct window *prevWindow;

	/* cursor position and line number */
	/* of the top and bottom lines in the window */
	Offset posTopline, posBotline;
	int numTopline, numBotline;
	
	/* options */
	int lineNumbers;
	int hasMsgLine;
	int isLogWindow;

	/* log window stuff */
	int toShellFD;
	int childPID;

	/* remember the line we last jumped from */
	int rowLastline;
	
	/* remember the last line the found the mouse cursor on */
	int rowCurLast;
	Offset posCurLast, lastPosTop;

	/* indent of the first character in each line visible */
	int indent;
	
	/* name and internal (to this program) identifier */
	/* of the file the window is displaying */
	int fileId;	/* of the file or view in the window */
	int realFileId;	/* of the file underlying the view */
	int nameOffset;	/* into the file name in the backing file */

	/* layout variables */
	int topMargin;		/* space to leave before the first line */
	int leftMargin;		/* space to leave left of each line */
	int nRows, nCols;	/* number of rows and columns */
	Position oldX, oldY;	/* before zooming */
	Dimension oldWidth, oldHeight, oldBorderWidth;
	
	/* X stuff */
	Window x_window_id;
	Window ln_window_id;
	
	/* keep an image of the contents of the screen */
	char * screen_image;

	/* font information */
	struct fontDataStruct font;

	/* Tk and Tcl stuff */
	Tk_Window tk_toplevel;
	Tk_Window tk_text;
	Tk_Uid tk_pathname;	/* interpreter name of the window */
	char * closeInform;	/* 'send $closeInform on close */

#ifdef HYPERTEXT
	/* Anasazi stuff */
	DBM *db;
	Document document;
	View view;
	Block block;
	Map blockMap;
	Map fromLinkMap;
	Map toLinkMap;
	File file;
#endif
};

#include "funcdecl.h"

/* $Header: /a/cvs/386BSD/ports/editor/point/cmdTable.c,v 1.1 1994/02/15 22:12:36 jkh Exp $ */

#include <ctype.h>
#include <stdlib.h>
#include "pt.h"

int commandTableSize;

struct commandTableEntry commandTable[] = {

/* Editing commands */
	{"Barrier",			FBARRIER},
	{"InsertFromScrap",		FINSERT},
	{"ChangeCaseOfSel",		FCHANGECASE},
	{"JustifySel",			FJUSTIFY},
	{"DeleteToScrap",		FDELETE},
	{"ExchangeWithScrap",		FEXCHSCRAP},
	{"CopySelToScrap",		FCOPYSCRAP},
	{"CopySelToMouse",		FCOPYTO},
	{"MoveSelToMouse",		FMOVETO},
	{"CopyToHereMode",		FCOPYFROM},
	{"MoveToHereMode",		FMOVEFROM},
/* Undo, Redo, Again commands */
	{"Again",			FAGAIN},
	{"Redo",			FREDO},
	{"Undo",			FUNDO},
/* Search commands */
	{"Search",			FSEARCHFORS},
	{"RegexSearch",			FREGEXSEARCH},
	{"CTag",			FCTAG},
	{"RepeatSearch",		FREPEATSEARCH},
	{"Replace",			FREPLACE},
	{"RegexReplaceOne",		FREGEXREPLACEONE},
	{"RegexReplaceAll",		FREGEXREPLACEALL},
	{"FindMatchingBracket",		FMATCHCHAR},
	{"SearchCharacter",		FSEARCHLETTER},
/* File positioning commands */
	{"MoveToEndFile",		FBOTFILE},
	{"ShowSelection",		FGOTOSELECTION},
	{"MoveToLastPlace",		FGOBACKTO},
	{"GotoLine",			FGOTOLINE},
	{"GotoDigit",			FGOTODIGIT},
/* Cursor positioning command */
	{"MoveSel",			FMOVESEL},
/* Window management commands */
	{"SetTextColor",		FSETTEXTCOLOR},
	{"Browser",			FBROWSER},
	{"TextFont",			FWINDOWFONT},
	{"BrowserFont",			FBROWSERFONT},
	{"RaiseWindow",			FRAISE},
	{"LowerWindow",			FLOWER},
	{"CD",				FCD},
	{"OpenWindow",			FOPENWINDOW},
	{"ConnectToPty",		FCONNECTTOPTY},
	{"CloseWindow",			FCLOSEWINDOW},
	{"RaiseListWindow",		FRAISELISTWINDOW},
	{"CloseBrowser",		FCLOSEBROWSER},
	{"SaveAs",			FWRITEFILE},
	{"SaveFile",			FSAVEFILE},
	{"SaveAllFiles",		FSAVEALL},
	{"Zoom",			FZOOM},
/* Other commands */
	{"ShowUndoStack",		FSHOWUNDOS},
	{"DoNothing",			FDONOTHING},
	{"Redraw",			FREDRAW},
	{"ToggleReadOnly",		FREADONLY},
	{"SetLineNumbers",		FLINENUMBERS},
	{"CancelModes",			FCANCEL},
	{"SendOnClose",			FINFORMONCLOSE},
	{"QuitPoint",			FQUITPOINT},
	{"PrintStats",			FPRINTSTATS},
	{"Option",			FOPTION},
	{"ChangeCursor",		FCHANGECURSOR},
/* Commands for use by event handlers */
	{"Configure",			FCONFIGURE},
	{"EnterText",			FENTERTEXT},
	{"EnterBrowser",		FENTERBROWSER},
	{"Expose",			FEXPOSE},
	{"HScroll",			FHSCROLL},
	{"Key",				FKEY},
	{"Mouse",			FMOUSE},
	{"VScroll",			FVSCROLL},
/* Commands primarily for use in macros */
	{"WaitForProcess",		FWAITFORPROCESS},
	{"Sel",				FPOINTSELECTION},
	{"GetFileChars",		FGETFILECHARS},
	{"ScrollWindow",		FSCROLLWINDOW},
	{"GetRowCol",			FGETROWCOL},
	{"WindowName",			FWINDOWNAME},
	{"GetWindowInfo",		FGETWINDOWINFO},
	{"GetFileInfo",			FGETFILEINFO},
	{"InsertString",		FINSERTSTRING},
	{"InsertAscii",			FINSASCII},
	{"MessageLine",			FMESSAGELINE},
/* Ana commands */
	{"InsertBlock",			FINSBLOCK},
	{"CreateBlock",			FCREATEBLOCK},
	{"CreateAttribute",		FCREATEATTRIBUTE},
	{"CreateLink",			FCREATELINK},
	{"CreateMap",			FCREATEMAP},
	{"CreateDocument",		FCREATEDOCUMENT},
	{"CreateView",			FCREATEVIEW},
	{"AddFileToDocument",		FADDFILETODOCUMENT},
	{"ShowAttributes",		FSHOWATTRIBUTES},
	{"ShowBlocks",			FSHOWBLOCKS},
	{"ShowDocuments",		FSHOWDOCUMENTS},
	{"ShowFiles",			FSHOWFILES},
	{"ShowLinks",			FSHOWLINKS},
	{"ShowMaps",			FSHOWMAPS},
	{"ShowTexts",			FSHOWTEXTS},
	{"ShowViews",			FSHOWVIEWS},
	{"ChangeMap",			FCHANGEMAP},
	{"CloseDocument",		FCLOSEDOCUMENT},
	{"zzzzzzzzz",			-2},
};

int
FindCommandInTable( s )
	char *s;
{
	extern struct commandTableEntry commandTable[];
	extern int commandTableSize;

	int i, low, high, mid;

	/* use a binary search on the command table */
	low = 0;
	high = commandTableSize - 1;
	while( low <= high ) {
		/* item in range low..high or not in commandTable */
		mid = (high+low)/2;
		i = striccmp( commandTable[mid].command_name, s );
		if( i == 0 )
			return mid;
		if( i < 0 )
			low = mid + 1;
		else
			high = mid - 1;
	}
	return -1;	/* failure return */
}

int
GetCommandNumber( s )
	char *s;
{
	extern struct commandTableEntry commandTable[];
	
	int i;
	
	if( isdigit(s[0]) )
		return atoi(s);
	i = FindCommandInTable( s );
	if( i == -1 ) {
		i = 0;
		printf("Could not find command `%s' in command table\n", s);
	}
	return commandTable[i].command_number;
}

void
AddPointCommands( interp )
	Tcl_Interp * interp;
{
        extern struct commandTableEntry commandTable[];
        extern int commandTableSize;

	int i;

	for( i = 0; i < commandTableSize; ++i )
		Tcl_CreateCommand(
			interp,
			commandTable[i].command_name,
			doPtCommand,
			(ClientData)(commandTable[i].command_number),
			NULL
		);
}

char *
CommandNumberToName( command_number )
	int command_number;
{
	extern struct commandTableEntry commandTable[];
	extern int commandTableSize;
	int n;

	for( n = 0; n < commandTableSize; ++n )
		if( commandTable[n].command_number == command_number )
			return commandTable[n].command_name;
	return "<unknown command number>";
}

static int
commandTableCompare( i, j )
        void *i, *j;
{
        return striccmp( ((struct commandTableEntry *)i)->command_name,
        		 ((struct commandTableEntry *)j)->command_name );
}

/*SUPPRESS 544*/ /*SUPPRESS 68*/

void InitCommands()
{
        extern struct commandTableEntry commandTable[];
        extern int commandTableSize;
        
        int n;

        /* sort the command table for easy lookup */
        /* first see how big it is */
        for( n = 0; commandTable[n].command_number != -2; ++n)
        	/*EMPTY*/
        	;
        commandTableSize = n;

        /* then sort it with qsort */
        qsort( (char *)commandTable, (size_t)commandTableSize,
        	sizeof(struct commandTableEntry), commandTableCompare);

}

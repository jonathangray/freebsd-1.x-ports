/* $Header: /a/cvs/386BSD/ports/editor/point/options.c,v 1.1 1994/02/15 22:12:38 jkh Exp $ */

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <setjmp.h>
#include "pt.h"

unsigned char beginMarkerChar = 0xFF;
unsigned char endMarkerChar = 0xFE;

/* some option flags */
int    autoIndent = 1;
int    autoSaveCount = 0;
int    autoZoom = 0;
int    backupByCopy = 0;
int    backupDepth = 1;
char * backupNameFormat = NULL;
char * browserFont = NULL;
char * browserGeometry = NULL;
char * browserIconFormat = NULL;
char * browserTitleFormat = NULL;
char * busySpriteName = NULL;
int    button1ScrollsDown = 0;
char * copySpriteName = NULL;
char * databaseName = NULL;
int    debug = 0;
int    eofChar = '\1';
char * filePattern = NULL;
int    findWholeWords = 0;
int    foldingOn = 0;
int    hypertextOn = 0;
int    ignoreCase = 1;
int    insertReplaces = 0;
int    keepSelectionVisible = 0;
char * keywordPattern = NULL;
int    linesOverFind = 999;
int    maxFiles = 200;
int    menuDelay = 600;
int    menuTolerance = 10;
int    messageFlags = BROWSER_MSGS | TEXT_MSGS;
struct mmData mm1Data[5];
struct mmData mm2Data[5];
char * mouseMenuFont = NULL;
int    mouseSpriteMenu = 0;
int    nBuffers = 100;
int    noBrowser = 0;
int    overType = 0;
int    pathNames = 0;
int    readOnly = 0;
char * returnString = NULL;
int    rightMargin = 999;
int    showPartialLines = 0;
char * deSelectedTextBackground = NULL;
char * selectedTextBackground = NULL;
char * deSelectedTextForeground = NULL;
char * selectedTextForeground = NULL;
int    showDirsFirst = 1;
int    showSizes = 0;
char * spriteBackground = NULL;
char * spriteForeground = NULL;
char * spriteName = NULL;
int    tabWidth = 8;
char * textBackground = NULL;
char * textFont = NULL;
char * textForeground = NULL;
char * textGeometry = NULL;
char * textIconFormat = NULL;
char * textTitleFormat = NULL;
int    tkScrolling = 0;
int    underlineSelection = 0;
int    undoMotion = 0;
int    wrapAroundSearches = 0;

/* option name and type table */
int optionTableSize;
struct optionTableEntry optionTable[] = {
	{"autoIndent",		(char *)&autoIndent,	T_BOOLEAN},
	{"autoSaveCount",	(char *)&autoSaveCount,	T_INTEGER},
	{"autoZoom",		(char *)&autoZoom,	T_BOOLEAN},
	{"backupByCopy",	(char *)&backupByCopy,	T_BOOLEAN},
	{"backupDepth",		(char *)&backupDepth,	T_INTEGER},
	{"backupNameFormat",	(char *)&backupNameFormat,T_STRING},
	{"browserFont",		(char *)&browserFont,	T_STRING},
	{"browserGeometry",	(char *)&browserGeometry,T_STRING},
	{"browserIconFormat",	(char *)&browserIconFormat,T_STRING},
	{"browserTitleFormat",	(char *)&browserTitleFormat,T_STRING},
	{"button1ScrollsDown",	(char *)&button1ScrollsDown,T_BOOLEAN},
	{"spriteBackground",	(char *)&spriteBackground,T_STRING},
	{"spriteForeground",	(char *)&spriteForeground,T_STRING},
	{"busySpriteName",	(char *)&busySpriteName,T_STRING},
	{"copySpriteName",	(char *)&copySpriteName,T_STRING},
	{"spriteName",		(char *)&spriteName,	T_STRING},
	{"databaseName",	(char *)&databaseName,	T_STRING},
	{"debug",		(char *)&debug,		T_INTEGER},
	{"eofChar",		(char *)&eofChar,	T_INTEGER},
	{"filePattern",		(char *)&filePattern,	T_STRING},
	{"findWholeWords",	(char *)&findWholeWords,T_BOOLEAN},
	{"foldingOn",		(char *)&foldingOn,	T_BOOLEAN},
	{"hypertextOn",		(char *)&hypertextOn,	T_BOOLEAN},
	{"ignoreCase",		(char *)&ignoreCase,	T_BOOLEAN},
	{"insertReplaces",	(char *)&insertReplaces,T_BOOLEAN},
	{"keepSelectionVisible",(char *)&keepSelectionVisible,T_BOOLEAN},
	{"keywordPattern",	(char *)&keywordPattern,T_STRING},
	{"linesOverFind",	(char *)&linesOverFind,	T_INTEGER},
	{"maxFiles",		(char *)&maxFiles,	T_INTEGER},
	{"menuDelay",		(char *)&menuDelay,	T_INTEGER},
	{"menuTolerance",	(char *)&menuTolerance,	T_INTEGER},
	{"messageFlags",	(char *)&messageFlags,	T_INTEGER},
	{"mouseSpriteMenu",	(char *)&mouseSpriteMenu,T_BOOLEAN},
	{"lmm1",		(char *)&(mm1Data[0].label),T_STRING},
	{"lmm1n",		(char *)&(mm1Data[1].label),T_STRING},
	{"lmm1e",		(char *)&(mm1Data[2].label),T_STRING},
	{"lmm1s",		(char *)&(mm1Data[3].label),T_STRING},
	{"lmm1w",		(char *)&(mm1Data[4].label),T_STRING},
	{"lmm2",		(char *)&(mm2Data[0].label),T_STRING},
	{"lmm2n",		(char *)&(mm2Data[1].label),T_STRING},
	{"lmm2e",		(char *)&(mm2Data[2].label),T_STRING},
	{"lmm2s",		(char *)&(mm2Data[3].label),T_STRING},
	{"lmm2w",		(char *)&(mm2Data[4].label),T_STRING},
	{"cmm1",		(char *)&(mm1Data[0].tcl_command),T_STRING},
	{"cmm1n",		(char *)&(mm1Data[1].tcl_command),T_STRING},
	{"cmm1e",		(char *)&(mm1Data[2].tcl_command),T_STRING},
	{"cmm1s",		(char *)&(mm1Data[3].tcl_command),T_STRING},
	{"cmm1w",		(char *)&(mm1Data[4].tcl_command),T_STRING},
	{"cmm2",		(char *)&(mm2Data[0].tcl_command),T_STRING},
	{"cmm2n",		(char *)&(mm2Data[1].tcl_command),T_STRING},
	{"cmm2e",		(char *)&(mm2Data[2].tcl_command),T_STRING},
	{"cmm2s",		(char *)&(mm2Data[3].tcl_command),T_STRING},
	{"cmm2w",		(char *)&(mm2Data[4].tcl_command),T_STRING},
	{"mouseMenuFont",	(char *)&mouseMenuFont,	T_STRING},
	{"nBuffers",		(char *)&nBuffers,	T_INTEGER},
	{"noBrowser",		(char *)&noBrowser,	T_BOOLEAN},
	{"overType",		(char *)&overType,	T_BOOLEAN},
	{"pathNames",		(char *)&pathNames,	T_BOOLEAN},
	{"readOnly",		(char *)&readOnly,	T_BOOLEAN},
	{"returnString",        (char *)&returnString,  T_STRING},
	{"rightMargin",		(char *)&rightMargin,	T_INTEGER},
	{"deSelectedTextBackground",(char *)&deSelectedTextBackground,T_STRING},
	{"selectedTextBackground",(char *)&selectedTextBackground,T_STRING},
	{"deSelectedTextForeground",(char *)&deSelectedTextForeground,T_STRING},
	{"selectedTextForeground",(char *)&selectedTextForeground,T_STRING},
	{"showDirsFirst",	(char *)&showDirsFirst,	T_BOOLEAN},
	{"showPartialLines",	(char *)&showPartialLines,T_BOOLEAN},
	{"showSizes",		(char *)&showSizes,	T_BOOLEAN},
	{"tabWidth",		(char *)&tabWidth,	T_INTEGER},
	{"textBackground",	(char *)&textBackground,T_STRING},
	{"textForeground",	(char *)&textForeground,T_STRING},
	{"textFont",		(char *)&textFont,	T_STRING},
	{"textGeometry",	(char *)&textGeometry,	T_STRING},
	{"textIconFormat",	(char *)&textIconFormat,T_STRING},
	{"textTitleFormat",	(char *)&textTitleFormat,T_STRING},
	{"tkScrolling",		(char *)&tkScrolling,	T_BOOLEAN},
	{"underlineSelection",	(char *)&underlineSelection,T_INTEGER},
	{"undoMotion",		(char *)&undoMotion,	T_BOOLEAN},
	{"wrapAroundSearches",	(char *)&wrapAroundSearches,T_BOOLEAN},
	{"zzzzzzzz",		NULL,			T_END_OF_TABLE}
};

static int
FindOptionInTable( s )
	char *s;
{
	extern struct optionTableEntry optionTable[];
	extern int optionTableSize;

	int i, low, high, mid;

	/* use a binary search on the option table */
	low = 0;
	high = optionTableSize - 1;
	while( low <= high ) {
		/* item in range low..high or not in optionTable */
		mid = (high+low)/2;
		i = striccmp( optionTable[mid].option_name, s );
		if( i == 0 )
			return mid;
		if( i < 0 )
			low = mid + 1;
		else
			high = mid - 1;
	}
	return -1;	/* failure return */
}

char *
GetPointOption( name )
	char * name;
{
	extern char msgBuffer[];

	int i;

	i = FindOptionInTable( name );

	if( i == -1 ) {
not_valid:
		printf("GetPointOption: (%s) is not a known Point option\n",
								name);
		sprintf( msgBuffer, "UnknownPointOption(%s)", name);
		return msgBuffer;
	}

	switch( optionTable[i].option_type ) {
	case T_END_OF_TABLE:
		printf("%s is not a valid point option\n", name);
		break;
	case T_STRING:
		sprintf( msgBuffer, "%s",
				*(char **)optionTable[i].option_address );
		return msgBuffer;
	case T_BOOLEAN:
	case T_INTEGER:
		sprintf( msgBuffer, "%d",
				*(int *)(optionTable[i].option_address) );
		return msgBuffer;
	}
	goto not_valid;
}


void
SetPointOption( name, value )
	char * name;
	char * value;
{
	extern BrowserData *activeBrowser;

	int i, n;
	char * p;
	char * option_address;

	i = FindOptionInTable( name );
	if( i == -1 ) {
		printf("SetPointOption: (%s) is not a known Point option\n",
							name);
		return;
	}

	option_address = optionTable[i].option_address;

	switch( optionTable[i].option_type ) {
	case T_END_OF_TABLE:
		printf("%s is not a valid point option\n", name);
		break;
	case T_STRING:
		p = (char *)PtMalloc( strlen(value)+1, "option string" );
		if( p == NULL ) { printf("OUT OF SPACE!!\n"); break; }
		strcpy( p, value );
		PtFree( *(char **)option_address );
		*(char **)option_address = p;
		break;
	case T_BOOLEAN:
		n = atoi( value );
		if( striccmp(value,"true")==0 )
			n = 1;
		*(int *)option_address = n;
		break;
	case T_INTEGER:
		*(int *)option_address = atoi( value );
		break;
	}
	
	/* special processing is required for some changes */
	if( strcmp(optionTable[i].option_name,"filePattern")==0 ) {
		NewFilelist( activeBrowser );
	}
}

static int
optionTableCompare( i, j )
        void *i, *j;
{
        return striccmp( ((struct optionTableEntry *)i)->option_name,
        			((struct optionTableEntry *)j)->option_name );
}

void
GetResourceOptions()
{
	extern BrowserData *mainBrowser;

	Tk_Uid uid;
	int i;

	/* check the user's default font, foreground and background. */
	/* Use those if they are defined */
	uid = Tk_GetOption( mainBrowser->tk_toplevel, "font", NULL );
	if( uid == NULL )
		uid = Tk_GetOption( mainBrowser->tk_toplevel, "Font", NULL );
	if( uid != NULL ) {
		SetPointOption( "browserFont", uid );
		SetPointOption( "textFont", uid );
	}
	uid = Tk_GetOption( mainBrowser->tk_toplevel, "foreground", NULL );
	if( uid != NULL ) {
		SetPointOption( "textForeground", uid );
		SetPointOption( "deSelectedTextForeground", uid );
		SetPointOption( "selectedTextBackground", uid );
	}
	uid = Tk_GetOption( mainBrowser->tk_toplevel, "background", NULL );
	if( uid != NULL ) {
		SetPointOption( "textBackground", uid );
		SetPointOption( "deSelectedTextBackground", uid );
		SetPointOption( "selectedTextForeground", uid );
	}

	/* go through the options table to find Point options */
	for( i = 0; i < optionTableSize; ++i ) {
		uid = Tk_GetOption( mainBrowser->tk_toplevel,
					optionTable[i].option_name, NULL );
		if( uid != NULL ) {
			SetPointOption( optionTable[i].option_name, uid );
		}
	}
}

/*SUPPRESS 544*/ /*SUPPRESS 68*/

void
InitOptions()
{
        /* sort the options table for easy lookup */
        /* first see how big it is */
        for( optionTableSize = 0;
                optionTable[optionTableSize].option_type != T_END_OF_TABLE;
                ++optionTableSize)
                        /*EMPTY*/
                        ;

        /* then sort it with qsort */
        qsort( (char *)optionTable, (size_t)optionTableSize,
        	sizeof(struct optionTableEntry), 
        	optionTableCompare);

	if( nBuffers < 25 )
		nBuffers = 25;
	else if( nBuffers > NBUFFERS )
		nBuffers = NBUFFERS;

	SetPointOption( "backupNameFormat",	"%n.%v" );
	SetPointOption( "browserFont",		"fixed" );
	SetPointOption( "spriteBackground",	"white" );
	SetPointOption( "spriteForeground",	"black" );
	SetPointOption( "spriteName",		"left_ptr" );
	SetPointOption( "busySpriteName",	"watch" );
	SetPointOption( "copySpriteName",	"hand1" );
	SetPointOption( "databaseName",		"anadoc" );
	SetPointOption( "textFont",		"fixed" );
	SetPointOption( "textGeometry",		"500x400+0+0" );
	SetPointOption( "browserTitleFormat",	"%a.CD:.%d" );
	SetPointOption( "browserIconFormat",	"Dir:%d" );
	SetPointOption( "textTitleFormat",
			"%a.@.%n%r. readOnly. [%l-%L]%c. (modified)." );
	SetPointOption( "textIconFormat",	"Edit:%n" );
	SetPointOption( "browserGeometry",	"135x445+510+0" );
	SetPointOption( "filePattern",		"*" );
	SetPointOption( "keywordPattern",	"*" );

	SetPointOption( "lmm1",  " Ext" );
	SetPointOption( "cmm1",  "ExtendSelection" );
	SetPointOption( "lmm1n", " << " );
	SetPointOption( "cmm1n", "Search [selection get] backward" );
	SetPointOption( "lmm1e", "Undo" );
	SetPointOption( "cmm1e", "Undo" );
	SetPointOption( "lmm1s", " >> " );
	SetPointOption( "cmm1s", "Search [selection get] forward" );
	SetPointOption( "lmm1w", "Again" );
	SetPointOption( "cmm1w", "Again" );

	SetPointOption( "lmm2",  "Dup " );
	SetPointOption( "cmm2",  "CopyToHereMode" );
	SetPointOption( "lmm2n", "Del " );
	SetPointOption( "cmm2n", "DeleteToScrap" );
	SetPointOption( "lmm2e", "Copy" );
	SetPointOption( "cmm2e", "CopySelToMouse" );
	SetPointOption( "lmm2s", "Ins " );
	SetPointOption( "cmm2s", "InsertFromScrap" );
	SetPointOption( "lmm2w", "Move" );
	SetPointOption( "cmm2w", "MoveSelToMouse" );

	SetPointOption( "mouseMenuFont",	    "fixed" );

	SetPointOption( "selectedTextForeground",   "white" );
	SetPointOption( "selectedTextBackground",   "black" );

	SetPointOption( "deSelectedTextBackground", "white" );
	SetPointOption( "textBackground",	    "white" );
	SetPointOption( "deSelectedTextForeground", "black" );
	SetPointOption( "textForeground",	    "black" );

	SetPointOption( "returnString",		  "" );
}

/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)usermenu.c	1.3 olvwm version 6/13/92"

/*
 * Based on
#ident	"@(#)usermenu.c	26.36	91/09/14 SMI"
 *
 */

/*
 * This file contains all of the functions for manipulating the user menu
 *
 * Global Functions:
 * InitUserMenu 	-- load the user menu and initialise
 * ReInitUserMenu 	-- reload the user menu and re-initialise
 * RootMenuShow		-- call MenuShow on the root menu
 *
 */

/*
 * Syntax of the user menu file should be identical to that used by
 *	buildmenu (SunView style RootMenu files).
 *
 *	NOTICE that SunView compatibility has resulted in old-style
 *	olwm menus no longer being supported.
 *
 *	There are two new reserved keywords:
 *
 *		DEFAULT tags a default button
 *		TITLE tags a title string for a menu (for titlebar)
 *
 *	One syntax in sunview menus is not supported:
 *		<icon_file> can not be used as a menu item
 *
 *	Here are the common reserved keywords:
 *		MENU and END are used to delimit a submenu
 *		PIN (appearing after END) indicates the menu is pinnable
 *		EXIT (built-in - olwm service)
 *		REFRESH (built-in - olwm service)
 *		POSTSCRIPT will invoke psh on the named command
 *
 * 	The file is line-oriented, however commands to be executed can
 *	extend to the next line if the newline is escaped (\).
 *
 *	Each line consists of up to three fields:  a label (a string
 *	corresponding to either the menu label or menu option label),
 *	up to two tags (keywords), and a command to be executed
 *	(or a file from which to read a submenu).  Two tags are allowed
 *	if one of them is "DEFAULT" or "END".
 *
 *	The tag is used to indicate the start and end of menu definitions,
 *	pinnability, built-in functions, and default options.
 *	The label indicates the text which appears on the user's menu,
 *	and the command describes what should be done when each item
 *	is selected.
 *
 *	Labels must be enclosed in double quotes if they contain
 *	whitespace.  Commands may be enclosed in double quotes (but
 *	do not have to be).
 *
 *	Comments can be embedded in a file by starting a line with a
 *	pound sign (#).  Comments may not be preserved as the file is
 *	used.
 *
 *	There are several functions which aren't invoked as programs;
 *	rather, they are built in to window manager.  These built-in
 *	services are each denoted by a single keyword.  The keywords are
 *	listed in the svctokenlookup[] array initialization.
 *
 *	example (will always have label: "Workspace Menu"):
 *
 *	"Workspace Menu"	TITLE
 *	Programs	MENU
 *		"Helpful Programs"	TITLE
 *		"Command Tool"	cmdtool
 *		"Blue Xterm"	DEFAULT xterm -fg white \
 *				-bg blue
 *	Programs	END	PIN
 *	Utilities	MENU
 *		"Refresh Screen" DEFAULT REFRESH
 *		"Clipboard"	 CLIPBOARD
 *	Utilities	END
 */

#ifdef SYSV
#include <sys/types.h>
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>	/* for stat(2) */
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include <string.h>
#include <pwd.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "globals.h"
#include "list.h"
#include "mem.h"
#include "win.h"
#include "menu.h"

static char *menuFileName	= "openwin-menu";
static char *workspaceTitle	= "Workspace";
static char *workspaceHelpStub	= "workspace";
static char *workspaceHelp 	= "workspace:DefaultMenu";

extern char *getenv();

#define TOKLEN		300

/* parseMenu return values */
#define MENU_FATAL     -1
#define MENU_NOTFOUND	0
#define MENU_OK		1
#define MENU_PINNABLE	2

typedef enum {
    UsrToken, MenuToken, EndToken, DefaultToken, PinToken,
    TitleToken, ServiceToken, PshToken
}           TokenType;

/* locally useful macro */
#define	APPEND_STRING(buf, str)	( strncat( buf, str, \
					( sizeof(buf) - strlen(buf) - 1 ) ) )
#define COUNT(x)	(sizeof(x) / sizeof(x[0]))

/* ---------------------------------------------------------------------
 * 	Externals
 * ---------------------------------------------------------------------
 */
extern int  RefreshFunc();
extern int  ClipboardFunc();
extern int  PrintScreenFunc();
extern int  ExitFunc();
extern int  ExitNoConfirmFunc();
extern int  PropertiesFunc();
extern int  SaveWorkspaceFunc();
extern int  FlipDragFunc();
extern int  AppMenuFunc();
extern int  PshFunc();
extern int  NopFunc();
extern int  WindowCtlFunc();
extern int  RestartOLWM();
extern int  FlipFocusFunc();
extern int  ReReadUserMenuFunc();
extern int  OpenCloseSelnFunc();
extern int  FullRestoreSizeSelnFunc();
extern int  BackSelnFunc();
extern int  QuitSelnFunc();
extern int  GenWinMenuFunc();
extern int  GenDirMenuFunc();
extern int  StickSelnFunc();

/* ---------------------------------------------------------------------
 *	local forward declarations
 * ---------------------------------------------------------------------
 */
static int  menuFromFile();
static int  parseMenu();
static void fillMenuStruct();
static TokenType lookupToken();
static Menu *buildFromSpec();
static void initMenu();
static void initButton();
static void freeButtonData();
static void freeMenuData();
static void freeUserMenu();
static Bool menuFileModified();
static void addToMenuInfo();
static void freeFileInfoList();

/* ---------------------------------------------------------------------
 *	local data
 * ---------------------------------------------------------------------
 */

typedef struct {
    char       *filename;	/* menu file path */
    dev_t       device;	/* device that the inode/file reside on */
    ino_t       inode;	/* inode of menu file */
    time_t      mtime;	/* modification time */
}           FileInfo;

typedef struct {
    char       *topfilename;	/* top-level menu file name */
    List       *fileinfoList;	/* list of FileInfo's for each menu file */
}           MenuFileInfo;

MenuFileInfo menuFileInfo;

typedef struct _buttondata {
    struct _buttondata *next;
    char       *name;
    Bool        isDefault;
    Bool        isLast;
    FuncPtr     func;
    char       *exec;	/* string to be executed, like "xterm" */
    void       *submenu;
    FuncPtr	generate;
    char	*generate_args;
}           buttondata;


typedef struct {
    char       *title;
    char       *menulabel;
    int         idefault;	/* index of default button */
    int         nbuttons;
    Bool        pinnable;
    buttondata *bfirst;
    int		columns;	/* number of desired columns */
}           menudata;


/* default Root menu should be quite minimal */
static Button xtermButton = {
    { { StringLabel, "Xterm", NULL },
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True, 
    {
	AppMenuFunc, 
	(void *) "xterm"
    },
};

static Button cmdtoolButton = {
    { { StringLabel, "Cmdtool", NULL},
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True, 
    {
	AppMenuFunc, 
	(void *) "cmdtool"
    },
};

static Button wsrefreshButton = {
    { { StringLabel, "Refresh", NULL}, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True,
    {
	RefreshFunc, 
	NULL
    },
};

static Button wsrestartButton = {
    { { StringLabel, "Restart WM", NULL}, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True,
    {
	RestartOLWM,
	NULL
    },
};

static Button wsrereadButton = {
    { { StringLabel, "Reread Menu File", NULL}, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True,
    {
	ReReadUserMenuFunc,
	NULL
    },
};

static Button exitWMButton = {
    { { StringLabel, "Exit WM", NULL}, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True,
    {
	ExitOLWM, 
	NULL
    },
};

static Button exitButton = {
    { { StringLabel, "Exit", NULL}, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    True, 
    True,
    {
	ExitFunc, 
	NULL
    },
};

static Button separatorButton = {
    { { NoType, NULL, NULL }, 
      { NoType, NULL, NULL } },
    {NULL, NULL}, 
    0, 
    False, 
    False, 
    True,
    {
	NULL, 
	NULL
    },
};

static Button *rootButtons[] = {
    &xtermButton,
    &cmdtoolButton,
    &separatorButton,
    &wsrefreshButton,
    &wsrestartButton,
    &wsrereadButton,
    &separatorButton,
    &exitWMButton,
    &exitButton,
};

/*
 *
 */
static menudata *makeRootMenu();

#ifdef notdef
DefaultsP   DefaultsPtr;
#endif


/* ---------------------------------------------------------------------
 * 	Global routines
 * ---------------------------------------------------------------------
 */

/********************************************************************************/

void SetWindowMenuLabels();

void
WindowMenuCreate(dpy)
    Display    *dpy;
{
    /*SetWindowMenuLabels();*/
    CreateScreenWindowMenuInfo(dpy);
}

void
WindowMenuDestroy(dpy)
    Display    *dpy;
{
    DestroyScreenWindowMenuInfo(dpy);
}

static menudata *
getUserMenu()
{
    menudata   *userroot;
    char        temp[MAXPATHLEN];
    char       *path;
    char       *homeEnv;
    char        homePath[MAXPATHLEN];
    char       *openwinhomeEnv;
    char        openwinhomePath[MAXPATHLEN];

    /*
     * Attempt to read menus from different sources: (1) the file named by the
     * OLWMMENU environment variable; (2) $HOME/.openwin-menu.<locale>; (3)
     * $HOME/.openwin-menu; (4) $OPENWINHOME/openwin-menu.<locale>; (5)
     * $OPENWINHOME/openwin-menu; (6) if all of the above fail, use olwm's
     * built-in menu.
     */

    /* try reading $OLWMMENU */
    path = getenv("OLWMMENU");
    if (path != NULL && (userroot = makeRootMenu(path)) != NULL)
	return userroot;

    /* get $HOME */
    homeEnv = getenv("HOME");
    if (!homeEnv)
	homeEnv = "";

    /* construct "$HOME/.openwin-menu" */
    strcpy(homePath, homeEnv);
    strcat(homePath, "/.");
    strcat(homePath, menuFileName);

#ifdef OW_I18N_L3
    /* try reading $HOME/.openwin-menu.<locale> */
    strcpy(temp, homePath);
    strcat(temp, ".");
    strcat(temp, GRV.LC.DisplayLang.locale);
    if ((userroot = makeRootMenu(temp)) != NULL) {
	return userroot;
    }
#endif	   /* OW_I18N_L3 */
    
    /* try reading $HOME/.openwin-menu */
    if ((userroot = makeRootMenu(homePath)) != NULL) {
	return userroot;
    }
    
    /* get $OPENWINHOME */
    openwinhomeEnv = getenv("OPENWINHOME");
    if (!openwinhomeEnv)
	openwinhomeEnv = "";
    
    /* construct "$OPENWINHOME/lib/openwin-menu" */
    strcpy(openwinhomePath, openwinhomeEnv);
    strcat(openwinhomePath, "/lib/");
    strcat(openwinhomePath, menuFileName);

#ifdef OW_I18N_L3
    /* try reading $OPENWINHOME/lib/openwin-menu.<locale> */
    strcpy(temp, openwinhomePath);
    strcat(temp, ".");
    strcat(temp, GRV.LC.DisplayLang.locale);
    if ((userroot = makeRootMenu(temp)) != NULL) {
	return userroot;
    }
#endif	   /* OW_I18N_L3 */
    
    /* try reading $OPENWINHOME/lib/openwin-menu */
    if ((userroot = makeRootMenu(openwinhomePath)) != NULL) {
	return userroot;
    }
    return NULL;
}

/*
 * InitUserMenu	-- load the user menu from a file using menuFromFile()
 *	and then create the actual RootMenu using buildFromSpec().
 *
 *	The file to be read is either in the directory specified by
 *	OLWMPATH or HOME, or OPENWINHOME/lib, and should be called
 *	MENUFILE.  If none of those three files exist,
 *	use the default menu.
 *
 */
static Menu *
getDefaultMenu()
{
    /* use default root menu */
    return CreateMenu(workspaceTitle, rootButtons, 
		      COUNT(rootButtons), True, workspaceHelp);
}

void
InitUserMenu(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo	*scrInfo;
{
    menudata *userroot;
#ifdef notyet
    DefaultsP curr = NULL;
#endif

    menuFileInfo.fileinfoList = NULL;
    menuFileInfo.topfilename = NULL;

    userroot = getUserMenu();

    if (userroot == NULL)
	scrInfo->menuTable[MENU_ROOT] = getDefaultMenu();
    else {
#ifdef notyet
	/* Add title of userroot to the top of the list */
	if (DefaultsPtr == (DefaultsP) 0) {
	    curr = DefaultsPtr = (DefaultsP) malloc(sizeof(struct _defaults));
	    curr->next = (DefaultsP) malloc(sizeof(struct _defaults));
	    curr->next->next = (DefaultsP) 0;
	}
	strcpy(curr->Name, userroot->title);
	strcpy(curr->next->Name, "Window");
#endif
	
	/* we read a menu from a file; now build it */
	scrInfo->menuTable[MENU_ROOT] =
			buildFromSpec(dpy, scrInfo, userroot, userroot->title);
    }
}


/*
 * ReInitUserMenu	- Re reads the user menu if changed.
 *	If the menu file has been modified since last looked at
 *	or if reRead is True, the attempt to create a new menu
 *	from the file.  If the new file is ok and a menu is created
 *	then use it, otherwise use the original.
 */
void
ReInitUserMenu(dpy, scrInfo, forceReRead)
    Display    *dpy;
    ScreenInfo	*scrInfo;
    Bool        forceReRead;
{
    menudata   *userroot;
    
    if (forceReRead || menuFileModified()) {
	freeFileInfoList(&menuFileInfo.fileinfoList);

	DestroyPinnedMenuClients();
	
	DestroyScreenUserMenuInfo(dpy);
	
	if (menuFileInfo.topfilename != NULL)
	    freeUserMenu(scrInfo->menuTable[MENU_ROOT]);
	else if (scrInfo->menuTable[MENU_ROOT] != NULL) {
	    /* default menu, most of it is just static data */
	    MemFree(scrInfo->menuTable[MENU_ROOT]->buttons);
	    MemFree(scrInfo->menuTable[MENU_ROOT]);
	}
	scrInfo->menuTable[MENU_ROOT] = NULL;

	if (! forceReRead && menuFileInfo.topfilename != NULL) {
	    userroot = makeRootMenu(menuFileInfo.topfilename);
	} else {
	    MemFree(menuFileInfo.topfilename);
	    menuFileInfo.topfilename = NULL;
	    userroot = getUserMenu();
	}

	if (userroot == NULL)
	    scrInfo->menuTable[MENU_ROOT] = getDefaultMenu();
	else
	    scrInfo->menuTable[MENU_ROOT] = buildFromSpec(dpy, scrInfo, userroot, userroot->title);

	CreateScreenUserMenuInfo(dpy);
    }
}


/*
 * RootMenuShow	- makes sure the user root menu is up to date and
 *	then calls MenuShow on the root menu to bring it up on the display.
 */
void
RootMenuShow(dpy, winInfo, pEvent)
    Display    *dpy;
    WinGeneric *winInfo;
    XEvent     *pEvent;
{
    ReInitUserMenu(dpy, winInfo->core.client->scrInfo, False);
    MenuShowSync(dpy, winInfo,
		 winInfo->core.client->scrInfo->menuTable[MENU_ROOT],
		 pEvent, NULL, NULL, False, False);
}


/* ---------------------------------------------------------------------
 * Local routines
 * ---------------------------------------------------------------------
 */

/*
 * expandPath - expand any environment variables in a path.
 *		returns a dynamically alloacted string with
 *		the expanded path.
 * Actually, this will also expand things of the nature:
 * $(OPENWINHOME)/include:${MUBMEL}/include:$FOOBLES/include
 */

char *
ExpandPath(pin, messages)
    char *pin;
    Bool messages;
{
    char pathname[1024];
    int haveslash;
    char envbuff[128];
    char *pend;
    char *penv;
    char *pstart;
    char *p;
    int len;
    struct passwd *ppw;
    char c;
    char *orig;

    if (pin == NULL)
	return NULL;

    orig = pin;

    *pathname = NULL;

    len = strlen(pin);

    if (pin[len] == '/')
	--len;

    haveslash = False;

    for (p = pathname;  len > 0;) {
	switch (*pin) {
	  case '~': /*overrides everything back to last :*/
	    ++pin;
	    --len;
	    if (len <= 1 || *pin == '/') {
		penv = getenv("HOME");
	    } else {
		int tmp;
		pend = strchr(pin, '/');
		if (pend == NULL) {
		    pend = strchr(pin, ':');
		    if (pend == NULL)
			pend = pin + strlen(pin);
		}
		tmp = pend - pin;
		memcpy(envbuff, pin, tmp);
		envbuff[tmp] = NULL;
		pin = pend;
		len -= tmp;
		ppw = getpwnam(envbuff);
		if (ppw == NULL && messages)
		    fprintf(stderr, 
			    gettext("olvwm: couldn't find user \"%s\" in \"%s\"\n"),
			    envbuff, orig);
		penv = (char *) (ppw? ppw->pw_dir : NULL);
	    }
	    if (penv) {
		for (pstart = p;  pstart > pathname;   pstart--)
		    if (*pstart == ':') {
			++pstart;
			break;
		    }
		strcpy(pstart, penv);
		p = pstart + strlen(penv);
	    }
	    haveslash = False;
	    continue;
	  case '$':
	    if ((c = *(pin+1)) == '(' || c == '{') {
		int tmp;

		pin += 2;
		pend = strpbrk(pin, "})");
		if (pend == NULL) {
		    if (messages)
			fprintf(stderr, 
				gettext("olvwm: no match for '%c' in pathname \"%s\"\n"), 
				c, orig);
		    return NULL;
		} else {
		    if (*pend == '}' && c != '{') {
			if (messages)
			    fprintf(stderr, 
				    gettext("olvwm: found a '}' before a ')' in \"%s\"\n"),
				    orig);
			return NULL;
		    } else if (*pend == ')' && c != '(') {
			if (messages)
			    fprintf(stderr, 
				    gettext("olvwm: found a ')' before a '}' in \"%s\"\n"),
				    orig);
			return NULL;
		    }
		}
		tmp = pend - pin;
		memcpy(envbuff, pin, tmp);
		envbuff[tmp] = NULL;
		len -= (2 + tmp + 1);
		pin = pend+1;
	    } else {
		--len;
		pend = strchr(++pin, '/');
		if (pend) {
		    int tmp = pend - pin;
		    memcpy(envbuff, pin, tmp);
		    envbuff[tmp] = NULL;
		    len -= tmp;
		    pin = pend;
		} else {
		    memcpy(envbuff, pin, len);
		    envbuff[len] = NULL;
		    len = 0;
		}
	    }
	    penv = getenv(envbuff);
	    if (penv) {
		int tmp = strlen(penv);
		if (haveslash && *penv == '/') {
		    /*if he put /usr//home, turn it into /home*/
		    /*/mumble:/usr//home -> /mumble:/home*/
		    for (; p > pathname;  p--)
			if (*p == ':') {
			    ++p;
			    break;
			}
		}
		memcpy(p, penv, tmp);
		p += tmp;
		haveslash = len > 0 && *(p-1) == '/';
	    }
	    if (len <= 0)
		break;
	    /*FALL THROUGH*/
	  default:
	    if (*pin != '/')
		haveslash = False;
	    else {
		if (!haveslash)
		    haveslash = True;
		else {
		    ++pin;
		    --len;
		    continue;
		}
	    }
	    *p++ = *pin++;
	    --len;
	}
    }
    *p = NULL;
    if (haveslash)
	pathname[strlen(pathname)-1] = NULL;

    return MemNewString(pathname);
}


/*
 * checkFile - check to see that a file (composed of named file and dir)
 *	is readable
 */
static      Bool
checkFile(location, file, path)
    char       *location, *file, *path;
{
    char       *dir;

    if ((dir = getenv(location)) == NULL)
	return False;
    strcpy(path, dir);
    strcat(path, file);
    return (access(path, R_OK) == 0);
}

/*
 *
 */
static menudata *
makeRootMenu(file)
    char       *file;
{
    menudata   userroot;
    menudata  *proot;

    userroot.title = NULL;
    userroot.menulabel = (char *) NULL;
    userroot.idefault = NOBUTTON;
    userroot.nbuttons = 0;
    userroot.pinnable = True;
    userroot.bfirst = (buttondata *) NULL;
    userroot.columns = 0;

    if (menuFromFile(file, &userroot, False) >= MENU_OK) {
	if (!menuFileInfo.topfilename)
	    menuFileInfo.topfilename = MemNewString(file);
	proot = MemNew(menudata);
	*proot = userroot;
	return proot;
    } else {
	freeFileInfoList(&menuFileInfo.fileinfoList);
	MemFree(userroot.title);
	return (menudata *) NULL;
    }
}

/*
 * menuFromFile - read a menu description from a file
 *
 *	Return values: same as parseMenu, with the addition of
 *		MENU_NOTFOUND = couldn't read submenu file
 */
static int
menuFromFile(file, menu, messages)
    char       *file;
    menudata   *menu;
    Bool        messages;
{
    char       *new;
    FILE       *stream;
    int         lineno = 1;	/* Needed for recursion */
    int         rval;

    /* expand any environment vars in path */
    if ((new = ExpandPath(file, messages)) != NULL)
	file = new;

    if ((stream = fopen(file, "r")) == NULL) {
	if (messages)
	    fprintf(stderr, "olvwm: can't open menu file %s\n", file);

	return MENU_NOTFOUND;
    }
    rval = parseMenu(file, stream, menu, &lineno);
    fclose(stream);

    if (rval >= MENU_OK) {
	addToMenuInfo(file);
	fillMenuStruct(menu);
    } else {
	freeMenuData(menu);
    }

    if (new)
	MemFree(new);

    return (rval);
}


/*
 * parseMenu -- read the user menu from the given stream and
 *	parse the stream into the menu structures defined locally.
 *	These structures (which are local to this module) are later
 *	used to build real menu structures.
 *
 *	Note that fillMenuStruct() needs to be called after parseMenu()
 *	is called (to finish filling out the menudata structure).
 *	If parseMenu() returns < 0, then freeMenuData() needs to be
 *	called instead, to free up unused memory.
 *
 *	Return values:
 *		MENU_OK		= an unpinnable menu was read successfully
 *		MENU_PINNABLE	= a pinnable menu was read successfully
 *		MENU_FATAL	= a fatal error was encountered
 *
 *	This is based heavily on buildmenu's getmenu() parsing routine.
 *
 */
static int
parseMenu(filename, stream, parent, lineno)
    char       *filename;
    FILE       *stream;
    menudata   *parent;
    int        *lineno;
{
    menudata   *currentMenu, *saveMenu;
    buttondata *currentButton;
    char        line[TOKLEN];
    char        label[TOKLEN];
    char        prog[TOKLEN];
    char        args[TOKLEN];
    static char localBuf[1024];
    char       *nqformat = "%[^ \t\n]%*[ \t]%[^ \t\n]%*[ \t]%[^\n]\n";
    char       *qformat =  "\"%[^\"]\"%*[ \t]%[^ \t\n]%*[ \t]%[^\n]\n";
    char       *format;
    register char *p;
    int         continuation;
    Bool        done;

    currentMenu = parent;
    initButton((buttondata **) & (currentMenu->bfirst));
    currentButton = currentMenu->bfirst;
    continuation = 0;

    for (; fgets(line, sizeof(line), stream); (*lineno)++) {
	if (line[0] == '#')
	    continue;

	for (p = line; isspace(*p); p++)
	    /* EMPTY */
	    ;

	if (*p == '\0')
	    continue;

	/*
	 * if we're already on a continuation line (the previous line ended in
	 * '\') then just copy the input through to the output until we get a
	 * line that doesn't end in '\' (nuke the vi backslash).
	 */
	if (continuation) {
	    /* fgets includes the newline in the string read */
	    while (line[strlen(line) - 2] == '\\') {
		/* get rid of backslash */
		line[strlen(line) - 2] = '\0';
		APPEND_STRING(localBuf, " ");
		APPEND_STRING(localBuf, p);
		if (!fgets(line, sizeof(line), stream))
		    break;
		(*lineno)++;
		for (p = line; isspace(*p); p++)
		    /* EMPTY */
		    ;
	    }
	    /* last line of continuation - replace \n with \0 */
	    line[strlen(line) - 1] = '\0';
	    APPEND_STRING(localBuf, " ");
	    APPEND_STRING(localBuf, p);
	    /* save it permanently in the buttondata structure */
	    currentButton->exec = MemNewString(localBuf);
	    localBuf[0] = '\0';
	    continuation = 0;
	    initButton((buttondata **) & (currentButton->next));
	    currentButton = currentButton->next;
	    continue;
	}
	/*
	 * if the line ends in '\' remember that continuation has started.
	 */
	if (line[strlen(line) - 2] == '\\') {
	    continuation = 1;
	    line[strlen(line) - 2] = '\0';
	}
	args[0] = '\0';
	format = (*p == '"') ? qformat : nqformat;

	if (sscanf(p, format, label, prog, args) < 2) {
	    /* seperator keyword appears alone on a line */
	    if (strcmp(label, "SEPARATOR") == 0) {
		currentButton->name = NULL;
		currentButton->isDefault = False;
		currentButton->func = NULL;
		currentButton->exec = NULL;
		currentButton->submenu = NULL;
		initButton((buttondata **) & (currentButton->next));
		currentButton = currentButton->next;
		continue;
	    }
	    /*otherwise...*/
	    fprintf(stderr,
		    gettext("olvwm: syntax error in menu file %s, line %d\n"),
		    filename, *lineno);
	    return (MENU_FATAL);
	}
	
	if (strcmp(prog, "END") == 0) {
	    /* currently allocated button is last for this menu */
	    currentButton->isLast = True;
	    if (currentMenu->menulabel != NULL &&
		    strcmp(label, currentMenu->menulabel) != 0) {
		fprintf(stderr,
		   gettext("olvwm: menu label mismatch in file %s, line %d\n"),
			filename, *lineno);
		return (MENU_FATAL);
	    }
	    /* compare PIN as # chars; args may have extra space */
	    if (strncmp(args, "PIN", 3) == 0)
		return (MENU_PINNABLE);
	    else
		return (MENU_OK);
	}
	if (strcmp(prog, "TITLE") == 0) {
	    currentMenu->title = MemNewString(label);

	    if (strncmp(args, "PIN", 3) == 0)
		currentMenu->pinnable = True;

	    /*
	     * we don't need to set up the next button, since the TITLE line
	     * didn't use up a button
	     */
	    continue;
	}
	currentButton->name = MemNewString(label);

	if (strcmp(prog, "DEFAULT") == 0) {
	    char       *t;
	    char       *u;

	    currentButton->isDefault = True;

	    /*
	     * Pull the first token from args into prog.
	     */
	    t = strtok(args, " \t");
	    if (t == NULL) {
		fprintf(stderr,
			gettext("olvwm: error in menu file %s, line %d\n"),
			filename, *lineno);
		/*
		 * STRING_EXTRACTION - Since DEFAULT is keyword, do not
		 * translate.
		 */
		fputs(gettext("missing item after DEFAULT keyword.\n"), stderr);
		return (MENU_FATAL);
	    }
	    strcpy(prog, t);
	    t = strtok(NULL, "");	/* get remainder of args */
	    if (t == NULL)
		args[0] = '\0';
	    else {
		u = args;
		/* can't use strcpy because they overlap */
		while (*u++ = *t++)
		    /* EMPTY */
		    ;
	    }
	}
	if (strcmp(prog, "MENU") == 0) {
	    int         rval;

	    initMenu((menudata **) & (currentButton->submenu));
	    saveMenu = currentMenu;
	    currentMenu = (menudata *) currentButton->submenu;
	    currentMenu->menulabel = MemNewString(label);

	    if (args[0] == '\0') {
		/*
		 * we haven't incremented lineno for this read loop yet, so we
		 * need to do it now. when END is read, parseMenu returns
		 * without incrementing lineno, so the count will be ok when
		 * this loop increments it before reading the next line of the
		 * file.
		 */
		(*lineno)++;
		if ((rval = parseMenu(filename, stream,
				      currentMenu, lineno)) < 0) {
		    freeMenuData(currentMenu);
		    return (MENU_FATAL);
		} else
		    fillMenuStruct(currentMenu);
	    } else {
		rval = menuFromFile(args, currentMenu, True);
		if (rval <= MENU_NOTFOUND)
		    return (MENU_FATAL);
	    }
	    if (rval == MENU_PINNABLE)
		currentMenu->pinnable = True;

	    currentMenu = saveMenu;
	    /* if submenu not found, reuse button */
	    if (rval != MENU_NOTFOUND) {
		initButton((buttondata **) & (currentButton->next));
		currentButton = currentButton->next;
	    }
	    continue;
	}
	else if (strcmp(prog, "WINMENU") == 0) {
	    initMenu((menudata **)&(currentButton->submenu));
	    saveMenu = currentMenu;
	    currentMenu = (menudata *) currentButton->submenu;
	    currentMenu->menulabel = MemNewString(label);
	    currentButton->generate = GenWinMenuFunc;
	    currentButton->generate_args = NULL;
	    currentMenu->columns = atoi(args);
	    currentMenu->idefault = NOBUTTON;
	    initButton((buttondata **)&(currentButton->next));
	    currentButton = currentButton->next;
	    currentMenu = saveMenu;
	    continue;
	}
	else if (strcmp(prog, "DIRMENU") == 0) {
	    initMenu((menudata **)&(currentButton->submenu));
	    saveMenu = currentMenu;
	    currentMenu = (menudata *) currentButton->submenu;
	    currentMenu->menulabel = MemNewString(label);
	    currentButton->generate = GenDirMenuFunc;
	    currentButton->generate_args = MemNewString(args);
	    currentMenu->columns = 0;
	    currentMenu->idefault = NOBUTTON;
	    initButton((buttondata **)&(currentButton->next));
	    currentButton = currentButton->next;
	    currentMenu = saveMenu;
	    continue;
	}
	else if (strcmp(prog, "COLUMNS") == 0) {
	    char	*t;

	    t = strtok(args, " \t");
	    if (t == NULL) {
	        fprintf(stderr,
		    gettext("olvwm: error in menu file %s, line %d\n"),
					    filename, *lineno);
		fputs(gettext("missing item after COLUMNS keyword.\n"), stderr);
		return (MENU_FATAL);
	    }
	    currentMenu->columns = atoi(t);
	    continue;
	}

	done = False;
	while (!done) {
	    switch (lookupToken(prog, &(currentButton->func))) {
	    case UsrToken:
		/*
		 * if UsrToken, that means that "prog" was just the first word
		 * of the command to be executed,
		 */
		strcpy(localBuf, prog);
		APPEND_STRING(localBuf, " ");
		APPEND_STRING(localBuf, args);
		/*
		 * copy current contents of localBuf back into args array so
		 * that PshToken code can be used
		 */
		strcpy(args, localBuf);
		localBuf[0] = '\0';
		/* fall through */
	    case PshToken:
		if (continuation)
		    strcpy(localBuf, args);
		else
		    currentButton->exec = MemNewString(args);
		done = True;
		break;
	    case PinToken:
		fprintf(stderr,
		     gettext("olvwm: format error in menu file %s, line %d\n"),
			filename, *lineno);
		fputs(gettext("menu title and END required before PIN keyword.\n"),
		      stderr);
		return (MENU_FATAL);
		/* NOTREACHED */
		break;
	    default:
		/* some other valid token found and returned */
		done = True;
		break;
	    }
	}

	if (!continuation) {
	    initButton((buttondata **) & (currentButton->next));
	    currentButton = currentButton->next;
	}
    }
    /* never used the last button created */
    currentButton->isLast = True;

    return (MENU_OK);
}


/*
 * fillMenuStruct - Once the menu structures have been filled out using
 * 	information in the menu description file (via parseMenu()), the
 * 	nbuttons and idefault elements need to be set.
 */
static void
fillMenuStruct(mptr)
    menudata   *mptr;
{
    buttondata *bptr;
    int         buttonIndex = 0;

    bptr = mptr->bfirst;
    if (bptr->isLast == True) {
	MemFree(bptr);
	bptr = mptr->bfirst = NULL;
    }
    for (; bptr != NULL && bptr->isLast == False; bptr = bptr->next) {
	if (bptr->isDefault == True)
	    mptr->idefault = buttonIndex;

	if ((bptr->next)->isLast == True) {
	    MemFree(bptr->next);
	    bptr->next = NULL;
	}
	buttonIndex++;
    }
    /* buttonIndex is one past end, but started at 0, so = number buttons */
    mptr->nbuttons = buttonIndex;
}


/*
 * Allowed menu keywords ("Token")
 */

struct _svctoken {
    char       *token;
    FuncPtr     func;
    TokenType   toktype;
}           svctokenlookup[] = {
    {
	"REFRESH", RefreshFunc, ServiceToken
    },
    {
	"CLIPBOARD", ClipboardFunc, ServiceToken
    },
    {
	"PRINT_SCREEN", PrintScreenFunc, ServiceToken
    },
    {
	"EXIT", ExitFunc, ServiceToken
    },
    {
	"EXIT_NO_CONFIRM", ExitNoConfirmFunc, ServiceToken
    },
    {
	"WMEXIT", ExitOLWM, ServiceToken
    },
    {
	"PROPERTIES", PropertiesFunc, ServiceToken
    },
    {
	"NOP", NopFunc, ServiceToken
    },
    {
	"DEFAULT", NULL, DefaultToken
    },
    {
	"MENU", NULL, MenuToken
    },
    {
	"END", NULL, EndToken
    },
    {
	"PIN", NULL, PinToken
    },
    {
	"TITLE", NULL, TitleToken
    },
    {
	"FLIPDRAG", FlipDragFunc, ServiceToken
    },
    {
	"SAVE_WORKSPACE", SaveWorkspaceFunc, ServiceToken
    },
    {
	"POSTSCRIPT", PshFunc, PshToken
    },
    {
	"RESTART", RestartOLWM, ServiceToken
    },
    {
	"FLIPFOCUS", FlipFocusFunc, ServiceToken
    },
    {
	"REREAD_MENU_FILE", ReReadUserMenuFunc, ServiceToken
    },
    {
	"OPEN_CLOSE_SELN", OpenCloseSelnFunc, ServiceToken
    },
    {
	"FULL_RESTORE_SIZE_SELN", FullRestoreSizeSelnFunc, ServiceToken
    },
    {
	"BACK_SELN", BackSelnFunc, ServiceToken
    },
    {
	"QUIT_SELN", QuitSelnFunc, ServiceToken
    },
    {
	"WINMENU", GenWinMenuFunc, MenuToken
    },
    {
	"STICK_UNSTICK_SELN", StickSelnFunc, ServiceToken
    },
};

#define NSERVICES COUNT(svctokenlookup)

/* lookupToken -- look up a token in the list of tokens
 *	given a supposed keyword or service name.  If the name doesn't
 *	match any existing token, return the user-defined token.
 */
static      TokenType
lookupToken(nm, ppf)
    char       *nm;
    FuncPtr    *ppf;
{
    int         ii;

    for (ii = 0; ii < NSERVICES; ii++) {
	if (!strcmp(nm, svctokenlookup[ii].token)) {
	    if (ppf != (FuncPtr *) 0)
		*ppf = svctokenlookup[ii].func;
	    return svctokenlookup[ii].toktype;
	}
    }
    if (ppf != (FuncPtr *) 0)
	*ppf = AppMenuFunc;
    return UsrToken;
}


/* buildFromSpec -- build the real menu structures, and create the
 * 	associated menus, from the specifications parsed from
 *	the menu layout.  Free up the specifications as we go
 *	along.
 */
static Menu *
buildFromSpec(dpy, scrInfo, pmenu, deftitle)
    Display    *dpy;
    ScreenInfo	*scrInfo;
    menudata   *pmenu;
    char       *deftitle;
{
    Menu       *m;
    Button     *b;
    int         ii;
    buttondata *bdata, *bsave;
    Bool flpin;
    char *tit;
    char *menuHelp;
    char helpbuff[255];

    if (pmenu->pinnable) {
	flpin = True;
	if (pmenu->title == NULL) {
	    if (deftitle == NULL)
		deftitle = workspaceTitle;
	    tit = MemNewString(deftitle);
	} else
	    tit = pmenu->title;
    } else {
	flpin = False;
	/* non-pinnable menus only get titles if they ask for them */
	/* m->title must be NULL if pmenu->title is NULL */
	tit = pmenu->title;
    }

     menuHelp = NULL;

     if (tit != NULL) {
 	sprintf(helpbuff, "%s:%s", workspaceHelpStub, tit);
 	menuHelp = MemNewString(helpbuff);
     }

     if (menuHelp == NULL && deftitle != NULL) {
 	sprintf(helpbuff, "%s:%s", workspaceHelpStub, deftitle);
 	menuHelp = MemNewString(helpbuff);
     }

     if (menuHelp != NULL)
 	ReplaceChars(menuHelp, " \t", '_');

     m = NewNamedMenu(tit, flpin, menuHelp);
     m->prefColSize = pmenu->columns;

    /*
     * If no default has been specified, set the first button in the menu to be
     * the default button. REMIND: The OL spec wants the pin, if one exists, to
     * be the default in such a cse. Fix this.
     */

    for (ii = 0, bdata = pmenu->bfirst; ii < pmenu->nbuttons; ii++) {
	b = (Button *) MemNew(Button);

	if ((bdata->name) && (bdata->name[0] == '<') &&
			(bdata->name[strlen(bdata->name) - 1] == '>')) {
	    /* Bitmap file */
	    b->label[0].kind = ImageLabel;
	    bdata->name[strlen(bdata->name) - 1] = '\0';
	    b->label[0].string = MemNewString(bdata->name + 1);
	    b->label[0].pixlabel = MemNew(Pixlabel);
	    if (!createPixmapFile(dpy, scrInfo, b->label[0].string,
					b->label[0].pixlabel))  {
		b->label[0].kind = StringLabel;
		MemFree(b->label[0].pixlabel);
	    }
	}
	else if (bdata->name) {
	    b->label[0].kind = StringLabel;
	    b->label[0].string = bdata->name;
	}
	else {
	    b->label[0].kind = NoType;
	    b->label[0].string = NULL;
	}
	/*right now, usermenus cannot have alternate items*/
	b->label[1].kind = NoType;
	b->label[1].string = NULL;
	b->which = 0;

	b->stacked = bdata->submenu != NULL;
	b->enabled = (bdata->name != NULL);
	b->visible = True;
	b->action.callback = bdata->func;
	b->generate_func = bdata->generate;
	b->generate_args = bdata->generate_args;

	if (! b->stacked) /* multi-purpose */
	    b->action.submenu = (void *) bdata->exec;
	else {
#ifdef notdef
	    DefaultsP   tail, curr;
	    /*
	     * Find the last node in DefaultsList and add this buttons label at
	     * the end
	     */
	    tail = DefaultsPtr;
	    while (tail->next)
		tail = tail->next;
	    tail->next = curr = (DefaultsP) malloc(sizeof(struct _defaults));
	    curr->next = (DefaultsP) 0;
	    strcpy(curr->Name, b->label[0].string);
#endif

	    b->action.submenu =
		(void *) buildFromSpec(dpy, scrInfo,
				       (menudata *) (bdata->submenu),
				       bdata->name);
	}
	bsave = bdata;
	bdata = bdata->next;
	MemFree(bsave);
	AppendMenuItem(m, b);
    }
		     
    if (pmenu->idefault == NOBUTTON)
	SetMenuDefault(m, -1);
    else
	SetMenuDefault(m, pmenu->idefault);

    MemFree(pmenu->menulabel);
    MemFree(pmenu);

    return (m);
}


/*
 * initMenu -
 */
static void
initMenu(newmenu)
    menudata  **newmenu;
{
    *newmenu = MemNew(menudata);
    (*newmenu)->title = NULL;
    (*newmenu)->menulabel = NULL;
    (*newmenu)->idefault = NOBUTTON;
    (*newmenu)->nbuttons = 0;
    (*newmenu)->pinnable = False;
    (*newmenu)->bfirst = (buttondata *) 0;
}

/*
 * initButton -
 */
static void
initButton(newButton)
    buttondata **newButton;
{
    *newButton = MemNew(buttondata);
    (*newButton)->next = NULL;
    (*newButton)->name = NULL;
    (*newButton)->isDefault = False;
    (*newButton)->isLast = False;
    (*newButton)->func = (FuncPtr) 0;
    (*newButton)->exec = NULL;
    (*newButton)->submenu = NULL;
    (*newButton)->generate = (FuncPtr) 0;
    (*newButton)->generate_args = NULL;
}

/*
 * freeMenuData - free any possibly allocated memory for this menudata
 *	structure (and its buttons), since it's not going to be used
 */
static void
freeMenuData(unusedMenu)
    menudata   *unusedMenu;
{
    buttondata *unusedButton;

    /* isLast probably isn't set, since this menu had an error */
    if ((unusedButton = unusedMenu->bfirst) != (buttondata *) 0)
	freeButtonData(unusedButton);

    MemFree(unusedMenu->title);
    MemFree(unusedMenu->menulabel);
    MemFree(unusedMenu);
    unusedMenu = NULL;
}

/*
 * freeButtonData - free any possibly allocated memory for this buttondata
 *	structure, since it's not going to be used
 */
static void
freeButtonData(unusedButton)
    buttondata *unusedButton;
{

    if (unusedButton->next != NULL)
	freeButtonData(unusedButton->next);

    MemFree(unusedButton->name);
    MemFree(unusedButton->exec);
    if (unusedButton->submenu != NULL)
	freeMenuData(unusedButton->submenu);
    MemFree(unusedButton);
    unusedButton = NULL;
}

/*
 * freeUserMenu	- free's a dynamically allocated menu and its buttons
 *	This assumes that all components of the menu structure are
 *	unique and dynamically allocated.
 */
static void
freeUserMenu(menu)
    Menu       *menu;
{
    int         i;

    if (menu == NULL)
	return;

    for (i = 0; i < menu->buttonCount; i++) {
	if (menu->buttons[i]->stacked)
	    freeUserMenu(menu->buttons[i]->action.submenu);
	else /*was holding string to exec */
	    MemFree(menu->buttons[i]->action.submenu);
	/*NOTE: user menus cannot yet have alternates,
	 * nor specify help on a per item basis*/
	MemFree(menu->buttons[i]->label[0].string);
	MemFree(menu->buttons[i]);
    }
    MemFree(menu->buttons);
    MemFree(menu->title);
    MemFree(menu->helpstring);
    MemFree(menu);
}

/*
 * menuFileModified	- check to see if any of the menu files have
 *	been changed.  Modifed is defined as any change in either
 *	the inode or modification time of the file.  A change in the
 *	device/inode indicates a change in a symbolic link while a change
 *	in the modification time indicates that the file has be edited.
 *	Only true if the AutoReReadMenuFile resource is also true.
 */
static      Bool
menuFileModified()
{
    FileInfo   *fi;
    List       *lp;
    struct stat statbuf;

    if (!GRV.AutoReReadMenuFile)
	return False;

    lp = menuFileInfo.fileinfoList;
    for (fi = ListEnum(&lp); fi != NULL; fi = ListEnum(&lp)) {
	if (stat(fi->filename, &statbuf) < 0) {
	    return True;
	}
	if (statbuf.st_mtime != fi->mtime ||
		statbuf.st_dev != fi->device ||
		statbuf.st_ino != fi->inode) {
	    return True;
	}
    }
    return False;
}

/*
 * addToMenuInfo	- adds file and it's stat info onto the list
 *	of FileInfo in menuFileInfo.
 */
static void
addToMenuInfo(file)
    char       *file;
{
    FileInfo   *fi;
    struct stat statbuf;

    if (stat(file, &statbuf) < 0) {
	return;
    }
    fi = MemNew(FileInfo);
    fi->filename = MemNewString(file);
    fi->device = statbuf.st_dev;
    fi->inode = statbuf.st_ino;
    fi->mtime = statbuf.st_mtime;

    menuFileInfo.fileinfoList = ListCons(fi, menuFileInfo.fileinfoList);
}


/*
 * freeFileInfoList	- free's all the FileInfo structs in a list
 *	and frees the list itself.
 */
static void
freeFileInfoList(plist)
    List       **plist;
{
    FileInfo   *fi;
    List       *lp;
    List       *list = *plist;

    if (!list)
	return;

    lp = list;
    for (fi = ListEnum(&lp); fi != NULL; fi = ListEnum(&lp)) {
	MemFree(fi->filename);
	MemFree(fi);
    }

    ListDestroy(list);

    *plist = NULL;
}


/********************************************************************************/

/*
 *	Table of menus -- now located in the scrInfo structures
Menu       *MenuTable[NUM_MENUS];
 */

/*
 *	Title and help strings
 */
static char *windowTitle = "Window";
static char *frameHelpString = "window:FrameMenu";

extern Button *MakeOpenButton(), *MakeOwnerButton(), *MakeFullSizeButton(),
	      *MakePropertiesButton(), *MakeBackButton(),
	      *MakeDismissThisButton(), *MakeRefreshButton(),
	      *MakeDismissButton(), *MakeResizeButton(), *MakeQuitButton(),
	      *MakeMoveButton(), *MakeStickyButton(), *MakeDismissAllButton();

/*
 * ===========================================================================

void
SetWindowMenuLabels()
{
    windowTitle = gettext(windowTitle);

    backButton.label[0].string = gettext(backButton.label[0].string);

    dismissAllButton.label[0].string = gettext(dismissAllButton.label[0].string);

    dismissButton.label[0].string = gettext(dismissButton.label[0].string);

    dismissThisButton.label[0].string = gettext(dismissThisButton.label[0].string);

    propertiesButton.label[0].string = gettext(propertiesButton.label[0].string);

    quitButton.label[0].string = gettext(quitButton.label[0].string);

    refreshButton.label[0].string = gettext(refreshButton.label[0].string);

    resizeButton.label[0].string = gettext(resizeButton.label[0].string);

    openButton.label[0].string = gettext(openButton.label[0].string);
    openButton.label[1].string = gettext(openButton.label[1].string);

    fullSizeButton.label[0].string = gettext(fullSizeButton.label[0].string);
    fullSizeButton.label[1].string = gettext(fullSizeButton.label[1].string);

    moveButton.label[0].string = gettext(moveButton.label[0].string);
    ownerButton.label[0].string = gettext(ownerButton.label[0].string);

    stickyButton.label[0].string = gettext(stickyButton.label[0].string);
    stickyButton.label[1].string = gettext(stickyButton.label[1].string);
}

*/

initMenuButtons(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
int	i;

    scrInfo->menuButtons.frameFullButtons =
				(Button **) MemAlloc(9 * sizeof(Button *));
    scrInfo->menuButtons.frameFullButtons[0] = MakeOpenButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[1] = MakeFullSizeButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[2] = MakeMoveButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[3] = MakeResizeButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[4] = MakePropertiesButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[5] = MakeBackButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[6] = MakeRefreshButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[7] = MakeStickyButton(dpy, scrInfo);
    scrInfo->menuButtons.frameFullButtons[8] = MakeQuitButton(dpy, scrInfo);

    scrInfo->menuButtons.frameDismissButtons =
				(Button **) MemAlloc(2 * sizeof(Button *));
    scrInfo->menuButtons.frameDismissButtons[0] = MakeDismissThisButton(dpy, scrInfo);
    scrInfo->menuButtons.frameDismissButtons[1] = MakeDismissAllButton(dpy, scrInfo);

    scrInfo->menuButtons.frameLimitedButtons =
				(Button **) MemAlloc(6 * sizeof(Button *));
    scrInfo->menuButtons.frameLimitedButtons[0] = MakeDismissButton(dpy, scrInfo);
    scrInfo->menuButtons.frameLimitedButtons[1] = MakeMoveButton(dpy, scrInfo);
    scrInfo->menuButtons.frameLimitedButtons[2] = MakeResizeButton(dpy, scrInfo);
    scrInfo->menuButtons.frameLimitedButtons[3] = MakeBackButton(dpy, scrInfo);
    scrInfo->menuButtons.frameLimitedButtons[4] = MakeRefreshButton(dpy, scrInfo);
    scrInfo->menuButtons.frameLimitedButtons[5] = MakeOwnerButton(dpy, scrInfo);
}

/*
 * InitMenus	-- Creates the built-in screen-independent menus
 *
 * No longer screen independent since they can contain bitmaps
 */
void
InitMenus(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo	*scrInfo;
{
    /* SetWindowMenuLabels();	/*usually, a NOOP*/

    initMenuButtons(dpy, scrInfo);
    scrInfo->menuTable[MENU_FULL] = CreateMenu(windowTitle,
			scrInfo->menuButtons.frameFullButtons, 
			basewin_ctbuttons, False, frameHelpString);
    scrInfo->menuTable[MENU_LIMITED] = CreateMenu(windowTitle,
			scrInfo->menuButtons.frameLimitedButtons, 
			popup_ctbuttons, False, frameHelpString);
    SetMenuHier(scrInfo->menuTable[MENU_LIMITED], popup_dismissitem,
		CreateMenu(windowTitle,
			   scrInfo->menuButtons.frameDismissButtons, 
			   limit_ctbuttons, False, frameHelpString));
    InitVDMMenu(dpy, scrInfo);

    /* this sets ROOT_MENU */
    InitUserMenu(dpy, scrInfo);
}



/*
 *	Assumes that Destroy called before Create.
 *	Assumes that the window menus will take up the first 6 slots
 */
int
CreateWindowMenuInfo(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    int         origNextSlot = scrInfo->menuCache->nextSlot;

    int j, x;

    scrInfo->menuCache->nextSlot = 0;

    (void) MenuInfoCreate(scrInfo->menuCache, scrInfo->rootwin,
			  scrInfo->menuTable[MENU_FULL], 1);
    (void) MenuInfoCreate(scrInfo->menuCache, scrInfo->rootwin,
			  scrInfo->menuTable[MENU_LIMITED], 1);

    scrInfo->menuCache->nextSlot = origNextSlot;
}

/************************************************************************************/

static int
firstEnabledItem(menu)
    Menu *menu;
{
    int i;

    for (i = 0;  i < menu->buttonCount;  i++)
	if (menu->buttons[i]->enabled && menu->buttons[i]->visible)
	    return i;

    return 0;	/*can't do anything else...*/
}

Menu *
GetEnabledMenu(cli, flfull, flnotitle, flsticky)
    Client *cli;
    Bool flfull;
    Bool flnotitle;
    Bool flsticky;
{
    WMDecorations *decor = cli->wmDecors;
    Menu *menu;
    static Bool lastmouseless = True;
    Bool flmouseless = ! IsMouselessSuspended();
    Bool flicon = cli->wmState != NormalState;
    Bool flresizable = decor->flags & WMDecorationResizeable;
    ScreenInfo	*scrInfo = cli->scrInfo;

    if (flmouseless != lastmouseless) {
	scrInfo->menuTable[MENU_FULL]->buttons[basewin_moveitem]->visible =
	scrInfo->menuTable[MENU_FULL]->buttons[basewin_resizeitem]->visible =
	scrInfo->menuTable[MENU_LIMITED]->buttons[popup_moveitem]->visible  =
	scrInfo->menuTable[MENU_LIMITED]->buttons[popup_resizeitem]->visible =
			flmouseless; 
	DirtyMenu(scrInfo->menuTable[MENU_FULL]);
	DirtyMenu(scrInfo->menuTable[MENU_LIMITED]);
	lastmouseless = flmouseless;
    }
    switch (decor->menu_type) {
      case MENU_FULL:
	menu = scrInfo->menuTable[MENU_FULL];
	ToggleEnabled(menu, basewin_resizeitem, flresizable && ! flicon);
	ToggleEnabled(menu, basewin_zoomitem, flresizable);
	ToggleItem(menu, basewin_openitem, ! flicon);
	ToggleItem(menu, basewin_zoomitem, flfull);
	ToggleItem(menu, basewin_stickyitem, flsticky);
	break;
      case MENU_LIMITED:
	menu = scrInfo->menuTable[MENU_LIMITED];
	ToggleEnabled(menu, popup_resizeitem, flresizable && ! flicon);
	break;
      default:
	menu = NULL;
	break;
    }
    if (menu) {
	if (menu->buttons[decor->def_item]->visible)
	    menu->buttonDefault = decor->def_item;
	else
	    menu->buttonDefault = firstEnabledItem(menu);

	if (flnotitle)
	    SetMenuTitle(menu, NULL);
	else
	    SetMenuTitle(menu, windowTitle);
    }
    return menu;
}

struct _setdefinfo {
    WinGenericFrame *win;
    Menu *menu;
    void (*proc)();
    void *data;
#ifdef DEBUG
    Bool flinuse;
#endif
};

static void
setFrameDefault(sdi)
    struct _setdefinfo *sdi;
{
    sdi->win->core.client->wmDecors->def_item = sdi->menu->buttonDefault;
    if (sdi->proc) {
	(*sdi->proc)(SYNC_DONE, 0, sdi->data);
	SetClickCallback(NULL, NULL);
    }
#ifdef DEBUG
    sdi->flinuse = False;
#endif
}

static void
doClickCallback(clickmode, sdi)
    MenuTrackMode clickmode;
    struct _setdefinfo *sdi;
{
    (*sdi->proc)(SYNC_CHANGECLICK, clickmode, sdi->data);
}

/*
 * assemble a menu and show it from one of the base types
 * if the menu came by hitting MENU on a button, pass flbutton
 */
void
ShowStandardMenuSync(win, eve, flbutton, proc, data)
    WinGenericFrame *win;
    XEvent *eve;
    Bool flbutton;
    void (*proc)();
    void *data;
{
    static struct _setdefinfo sdi;

#ifdef DEBUG
    if (sdi.flinuse)
	fprintf(stderr, "showstandardmenusync: stranding defitem!\n");
    sdi.flinuse = True;
#endif
    sdi.menu = GetEnabledMenu(win->core.client, win->fcore.fullsize,
				flbutton, win->core.client->sticky);
    sdi.win = win;
    sdi.proc = proc;
    sdi.data = data;

    if (proc != NULL) {
	SetClickCallback(doClickCallback, &sdi);
    }
    MenuShowSync(win->core.client->dpy, win, sdi.menu, eve, setFrameDefault, &sdi, 
		 (eve->type == KeyPress) || (eve->type == KeyRelease),
		 flbutton);
}

void
ShowStandardMenu(win, eve, flbutton)
    WinGenericFrame *win;
    XEvent *eve;
    Bool flbutton;
{
    ShowStandardMenuSync(win, eve, flbutton, NULL, NULL);
}

/*
 * createPixmapFile
 *
 *  Given a filename, return a pixmap.  Currently only supports X11 bitmap
 *  files, but will someday support Sun Icon files too
 *
 * REMIND:  What about different screens and colors?
 */

typedef enum { BadFormat, XBitmapFormat,
		SunIconFormat, XPixmapFormat } ImageFormat;

ImageFormat
ImageFileFormat(filename)
    char	*filename;

{
FILE	*fp;
char	s[256];
int	dummy;

    if ((fp = fopen(filename, "r")) == NULL)
	return BadFormat;
    fgets(s, 255, fp);
    fclose(fp);
    if (s[0] == '#')
	return XBitmapFormat;
    if (sscanf(s, "/* Format_version=%d", &dummy) < 1)
	return BadFormat;
    return SunIconFormat;
}

Bool
createPixmapFile(dpy, scrInfo, filename, pixlabel)
Display		*dpy;
ScreenInfo	*scrInfo;
char		*filename;
Pixlabel	*pixlabel;	/* RETURN */
{
Drawable  drawable = scrInfo->rootid;
int	  depth    = scrInfo->depth;
Pixmap	  bitmap;
int	  xhot, yhot;
GC	gc;
XGCValues	gcv;

#ifdef NOTDEF
	switch ( ImageFileFormat(filename) ) {
	    case XBitmapFormat:
#endif
		if (XReadBitmapFile(dpy, drawable,
			    filename, &(pixlabel->width), &(pixlabel->height),
			    &bitmap, &xhot, &yhot) != BitmapSuccess) {
			return False;
		}
		pixlabel->pixmap = XCreatePixmap(dpy, drawable,
			pixlabel->width, pixlabel->height, depth);
		gcv.foreground = scrInfo->colorInfo.bg1Color;
		gcv.background = scrInfo->colorInfo.workspaceColor;
		if (gcv.foreground == gcv.background)
		    gcv.foreground = scrInfo->colorInfo.fgColor;
		gc = XCreateGC(dpy, drawable, GCForeground|GCBackground, &gcv);
		XCopyPlane(dpy, bitmap, pixlabel->pixmap,
			gc,
			0, 0, pixlabel->width, pixlabel->height, 0, 0, 1);
		XFreePixmap(dpy, bitmap);
		XFreeGC(dpy, gc);
#ifdef FUTURE
		break;
	    case SunIconFormat:
		if (!SunReadIconFile(dpy, drawable, filename,
			&pixlabel->width, &pixlabel->height, &bitmap))
		    return False;
		pixlabel->pixmap = XCreatePixmap(dpy, drawable,
			pixlabel->width, pixlabel->height, depth);
		XCopyPlane(dpy, bitmap, pixlabel->pixmap,
			scrInfo->gc[ROOT_GC], 0, 0,
			pixlabel->width, pixlabel->height, 0, 0, 1);
		XFreePixmap(dpy, bitmap);
		break;
	    case XPixmapFormat:
		if (XReadPixmapFile(dpy, ?Visual?, drawable, ?Colormap?,
			    filename, depth, &(pixlabel->pixmap),
			    &(pixlabel->width), &(pixlabel->height),
			    (unsigned long **)0, 0, /*don't save pixel info*/
			    (ColorSymbol *)0, 0, /*don't override colors*/
			    (XpmInfo *)0)	 /*don't save xpm info*/
			    != PixmapSuccess) {
			return False;
		}
		break;
	    default:
		return False;
	}
#endif
	return True;
}

#ifdef NOTDEF
SunReadIconFile(dpy, drawable, filename, width, height, bitmap)
    Display	*dpy;
    Window	drawable;
    char	*filename;
    int		*width, *height;
    Pixmap	*bitmap;

{
FILE	*fp;
char	s[256];
int	version = 0, depth = 0, bits = 0;
char	*data, *pd;
int	i, temp;

    fp = fopen(filename, "r");
    if (!fp)
	return False;
    fgets(s, 255, fp);
    sscanf(s, "/* Format_version=%d, Width=%d, Height=%d, Depth=%d, Valid_bits_per_item=%d",
	   &version, width, height, &depth, &bits);
    if (version != 1 || depth != 1 || (bits != 16 && bits != 32)) {
	ErrorWarning(gettext("An invalid SunIcon file was named as a bitmap"));
	fclose(fp);
	return False;
    }
    if (*width % 16) {
	ErrorWarning(gettext("An SunIcon file with an invalid width was named as a bitmap"));
	fclose(fp);
	return False;
    }
    data = MemAlloc((*width / 8) * *height);
    pd = data;
    fgets(s, 255, fp);		/* get end of comment */
    for (i = 0; i < *height; i++) {
	fscanf(fp, "0x%lx", &temp);
	switch(bits) {
	    case 16:
		*pd++ = (temp >> 8);
		*pd++ = (temp & 0xff);
		break;
	    case 32:
		*pd++ = (temp >> 24);
		*pd++ = ((temp>>16) & 0xff);
		*pd++ = ((temp>>8) & 0xff);
		*pd++ = (temp & 0xff);
		break;
	}
    }
    *bitmap = XCreateBitmapFromData(dpy, drawable, data, *width, *height);
    fclose(fp);
    if (!*bitmap)
	return False;
    return True;
}
#endif

/*
 *      (c) Copyright 1991 Scott Oaks
 *      See LEGAL_NOTICE file for terms of the license.
 */ 

%{
#ident  "@(#)olvwmrc.y	1.3 olvwm version 6/13/92"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "list.h"
#include "mem.h"
#include "win.h"
#include "menu.h"
#include "kbdfuncs.h"
#include "olwm.h"
#include "ollocale.h"
#include "globals.h"
#include "virtual.h"
#include "i18n.h"

#define OLVWM_USE_SELECTION	"OLVWM_USE_SELECTION"

extern CheckForKeyProg();
char	*LookupToken();
char	*FindOlvwmRC();

typedef struct progscreen {
    char        *target;
    int         screen;
} ProgScreen;

typedef enum { Warp, Open, Raise, Execute, Goto, Close, Quit,
		RaiseLower, Lower } Action;

typedef struct progkeynode {
    Action	action;
    char	*parameter;
} ProgKeyNode;

typedef struct progkey {
    int         modmask;
    KeyCode     keycode;
    List	*todo;
} ProgKey;

typedef struct winmenuactions {
    char	*key;
    List	*actions;
} WinMenuActions;

List    *ProgScreenList = NULL;
List    *ProgKeyList = NULL;
List	*WinMenuActionsList = NULL;

static Display	*dpy;
%}


%start file

%union {
    int		ival;
    void	*sval;
}

%token	<ival> WARP
%token	<ival> OPEN
%token	<ival> RAISE
%token	<ival> CLOSE
%token	<ival> QUIT
%token	<ival> EXECUTE
%token	<ival> GOTO
%token	<ival> MODIFIER
%token	<sval> KEY
%token	<sval> WORD
%token	<ival> COLON
%token	<ival> OPENBRACE
%token	<ival> CLOSEBRACE
%token	<ival> COMMA
%token	<ival> SCREEN
%token	<ival> INT
%token	<sval> STARTSQUOTE
%token	<sval> STARTDQUOTE
%token	<sval> ENDSQUOTE
%token	<sval> ENDDQUOTE
%token	<ival> WINMENU
%token  <ival> PLUS
%token	<ival> RAISELOWER
%token	<ival> LOWER

%type	<sval> List
%type	<ival> Modifier
%type	<sval> Actions
%type	<sval> KeySpec
%type	<sval> WarpAction
%type	<sval> OpenAction
%type	<sval> RaiseAction
%type	<sval> RaiseLowerAction
%type	<sval> LowerAction
%type	<sval> GotoAction
%type	<sval> ExecuteAction
%type	<sval> CloseAction
%type	<sval> QuitAction
%type	<ival> ScreenStart
%type	<sval> String
%type	<sval> WinMenuActions
%type	<sval> Key

%%
/* Rules */

file	:	/* empty */
	|	file KeyProg
	|	file ScreenProg
	|	file WinMenuProg
	|	error CLOSEBRACE

KeyProg	:	KeySpec OPENBRACE Actions CLOSEBRACE
		{
		    ProgKey	*p;
		    int		ret;
		    KeyDescriptor	*d;

		    p = $1;
		    p->todo = $3;
		    d = (KeyDescriptor *) MemAlloc(sizeof(*d));
		    d->rsrc_name = NULL;
		    d->dflt_binding = NULL;
		    d->function = (void (*)()) CheckForKeyProg;
		    d->action = ACTION_VIRTUAL;
		    d->flags = KD_DYNAMIC;
		    AddKeyBinding(p->keycode, p->modmask, d);
		    ProgKeyList = ListCons($1, ProgKeyList);
		}

ScreenProg :	ScreenStart List CLOSEBRACE
		{
		    ProgScreen	*p;

		    p = (ProgScreen *) MemAlloc(sizeof(ProgScreen));
		    p->screen = $1 - 1;		/* count internally from 0 */
		    p->target = $2;
		    ProgScreenList = ListCons(p, ProgScreenList);
		}

WinMenuProg :	WINMENU OPENBRACE WinMenuActions CLOSEBRACE
		{
		    static int DoneWinMenuActions = False;

		    if (DoneWinMenuActions) {
			ErrorWarning(gettext("Duplicate WINMENU entry in .olvwmrc"));
			YYERROR;
		    }
		    WinMenuActionsList = $3;
		}

WinMenuActions : /* empty */
		{ $$ = NULL; }
	|	WinMenuActions String OPENBRACE Actions CLOSEBRACE
		{ 
		    WinMenuActions	*p;

		    p = (WinMenuActions *) MemAlloc(sizeof(WinMenuActions));
		    p->key = strdup($2);
		    p->actions = $4;
		    $$ = ListCons(p, $1);
		}

Actions :	/* empty */
		{ $$ = NULL; }
	|	Actions WarpAction
		{ $$ = ListCons($2, $1); }
	|	Actions OpenAction
		{ $$ = ListCons($2, $1); }
	|	Actions RaiseAction
		{ $$ = ListCons($2, $1); }
	|	Actions ExecuteAction
		{ $$ = ListCons($2, $1); }
	|	Actions GotoAction
		{ $$ = ListCons($2, $1); }
	|	Actions CloseAction
		{ $$ = ListCons($2, $1); }
	|	Actions QuitAction
		{ $$ = ListCons($2, $1); }
	|	Actions RaiseLowerAction
		{ $$ = ListCons($2, $1); }
	|	Actions LowerAction
		{ $$ = ListCons($2, $1); }

WarpAction :	WARP COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Warp;
			    p->parameter = strdup($3);
			    $$ = p;
			}
	
CloseAction:	CLOSE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Close;
			    p->parameter = $3;
			    $$ = p;
			}
	
QuitAction:	QUIT COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Quit;
			    p->parameter = $3;
			    $$ = p;
			}

OpenAction:	OPEN COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Open;
			    p->parameter = $3;
			    $$ = p;
			}
			
RaiseLowerAction:	RAISELOWER COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = RaiseLower;
			    p->parameter = $3;
			    $$ = p;
			}
			
LowerAction:	LOWER COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Lower;
			    p->parameter = $3;
			    $$ = p;
			}
			
RaiseAction:	RAISE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Raise;
			    p->parameter = $3;
			    $$ = p;
			}
			
ExecuteAction:	EXECUTE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Execute;
			    p->parameter = $3;
			    $$ = p;
			}

GotoAction:	GOTO COLON String
			{
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Goto;
			    p->parameter = strdup($3);
			    $$ = p;
			}

KeySpec : 	Key Modifier
			{ 
			    KeySym	ks;
			    KeyCode	kc;
			    ProgKey	*p;
			    char	msg[80];

			    ks = XStringToKeysym($1);
			    if (ks == NoSymbol) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    $1);
				ErrorWarning(msg);
				YYERROR;
			    }
			    kc = XKeysymToKeycode(dpy, ks);
			    if (kc == 0) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    $1);
				ErrorWarning(msg);
				YYERROR;
			    }
			    p = (ProgKey *) MemAlloc(sizeof(ProgKey));
			    p->keycode = kc;
			    p->modmask = $2;

			    $$ = p;
			    free($1);
			}

Key	:	KEY
		{ $$ = strdup($1); }

Modifier :	/* empty */
			{ $$ = 0; }
	|	Modifier PLUS MODIFIER
			{
			    KeyCode	kc;
			    char msg[80];

			    if ($3 == -1)
			        $$ = AnyModifier;
			    else {
				kc = XKeysymToKeycode(dpy, $3);
				if (kc == 0) {
				    sprintf(msg,
				    gettext("Unknown modifer %d\n in .olvwmrc"),
				    $2);
				    ErrorWarning(msg);
				    YYERROR;
				}
			    }
			    $$ |= FindModifierMask(kc);
			}

List	:	String
			{ $$ = $1; }
	|	List COMMA String
			{
			    char	*s;

			    s = MemAlloc(strlen($1) + strlen($3) + 2);
			    sprintf(s, "%s,%s", $1, $3);
			    free($1);
			    free($3);
			    $$ = s;
			}

ScreenStart :	SCREEN INT OPENBRACE
		{
		    $$ = $2;
		}

String :    WORD
	    {
		char	*t;

		t = $1;
		if (*t == '\"' || *t == '\'') {
		    /* word in quotes; get rid of them */
		    t++;
		    t[strlen(t) - 1] = '\0';
		}
		$$ = strdup(t);
		free($1);
	    }
%%
/* Programs */
/* #define YYDEBUG 1 */
#include "parse.c"

extern List	*ActiveClientList;

yyerror(s)
    char	*s;

{
char	msg[256];

    sprintf(msg, gettext("Syntax error near %s in .olvwmrc -- entry ignored\n"),
		yytext);
    ErrorWarning(msg);
}

static ProgKey	*
matchProgKey(p, ev)
    ProgKey	*p;
    XEvent	*ev;

{
    if (p->keycode == ev->xkey.keycode &&
	(p->modmask == AnyModifier || p->modmask == ev->xkey.state))
	return p;
    return NULL;
}

static ProgScreen *
matchProgString(p, s)
    ProgScreen	*p;
    char	*s;
{
char	*t, *t1;

    t1= strdup(p->target);
    t = LookupToken(t1, ",");
    while (t) {
        if (!strncmp(t, s, strlen(t))) {
	    free(t1);
	    return p;
	}
	t = LookupToken(NULL, ",");
    }
    free(t1);
    return NULL;
}

static int findClient_rootid;

static Client	*
findClient(c, s)
    Client	*c;
    char	*s;
{
    if (findClient_rootid && findClient_rootid != c->scrInfo->rootid)
	return NULL;
    if (c->framewin && c->framewin->fcore.name)
        if (!strncmp(c->framewin->fcore.name, s, strlen(s)))
	    return c;
    if (c->wmClass)
        if (!strcmp(c->wmClass, s))
	    return c;
    if (c->wmInstance)
        if (!strcmp(c->wmInstance, s))
	    return c;
    return NULL;
}

static int applyIsKey = False;
static void clientWarp();

static int
applyAction(p, cli)
    ProgKeyNode	*p;
    Client	*cli;

{
Client	*c;
char	*s, *t;
List	*l;

    switch(p->action) {
	case Warp:
	    /*
	     * We only allow one lookup per warp, but we can't use p.parameter
	     * directly, since it may contain special characters which
	     * LookupToken will remove
	     */
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    if (!strcmp(t, OLVWM_USE_SELECTION)) {
		if (cli)
		    clientWarp(cli);
		}
		else {
		    extern List *ScreenInfoList;
		    List *l = ScreenInfoList;
		    ScreenInfo *scr;

		    /* allow one warp per screen */
		    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l)) {
			findClient_rootid = scr->rootid;
			c = (Client *) ListApply(ActiveClientList, findClient, t);
			if (c)
			    clientWarp(c);
		    }
		    findClient_rootid = 0;
		}
		free(s);
	    break;

	case Open:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    if (cli->wmState == IconicState)
			StateIconNorm(cli);
		    else ;
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t) && c->wmState == IconicState)
		    	    StateIconNorm(c);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Close:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    if (cli->wmState == NormalState)
			StateNormIcon(cli);
		    else ;
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t) && c->wmState == NormalState)
		    	    StateNormIcon(c);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Quit:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    ClientKill(cli, True);
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    ClientKill(c, True);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Raise:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    if (cli->wmState == IconicState)
			 RaiseWindow(cli->iconwin);
		    else RaiseWindow(cli->framewin);
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    if (c->wmState == IconicState)
			        RaiseWindow(c->iconwin);
			    else RaiseWindow(c->framewin);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case RaiseLower:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    ClientToggleStacking(cli);
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    ClientToggleStacking(c);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Lower:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        l = ActiveClientList;
		if (!strcmp(t, OLVWM_USE_SELECTION))
		    if (cli->wmState == IconicState)
			 LowerWindow(cli->iconwin);
		    else LowerWindow(cli->framewin);
		else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    if (c->wmState == IconicState)
			        LowerWindow(c->iconwin);
			    else LowerWindow(c->framewin);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Execute:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
	        ExecCommand(cli->scrInfo->environment, t);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;
	
	case Goto:
	    VDMGoto(dpy, cli, atoi(p->parameter));
	    break;
    }
    return False;
}

static void
clientWarp(c)
    Client	*c;

{
int	rootX, rootY, root, child, winX, winY, keys;

    VDMMoveTo(dpy, c, c->framewin->core.x, c->framewin->core.y);
    /*
     * If we got here via a key, move the mouse to the window and
     * give it focus
     */
    if (applyIsKey) {
	if (!XQueryPointer(dpy, PANEWINOFCLIENT(c),
		&root, &child, &rootX, &rootY, &winX, &winY,
		&keys))
	    return;
	XWarpPointer(dpy, None, None, 0, 0, 0, 0,
		     c->framewin->core.x - rootX +
			 c->framewin->core.width / 2,
		     c->framewin->core.y - rootY +
			 c->framewin->core.height / 2);
    }
    if (!GRV.FocusFollowsMouse)
	ClientSetFocus(c, True, CurrentTime);
}

char *
FindOlvwmRC(buf)
    struct stat	*buf;
{
char	*path;
static char	s[256];
extern char	*getenv();
struct stat	tmp;

    if (buf == NULL)
	buf = &tmp;

    /* try reading OLVWMRC */
    path = getenv("OLVWMRC");
    if (path && stat(path, buf) == 0)
	return path;

    /* try reading current directory */
    sprintf(s, ".olvwmrc");
    if (stat(s, buf) == 0)
	return s;

    if ((path = getenv("HOME")) != NULL) {
        sprintf(s, "%s/.olvwmrc", getenv("HOME"));
	if (stat(s, buf) == 0)
	    return s;
    }
    return NULL;
}

static struct stat olvwmBuf;
static struct stat olvwmOldBuf;

InitOlvwmRC(ldpy)
    Display	*ldpy;

{
char	*path = FindOlvwmRC(&olvwmBuf);

    if (path == NULL || (yyin = fopen(path, "r")) == NULL)
	return;
    olvwmOldBuf = olvwmBuf;
    dpy = ldpy;
    yyparse();
#ifdef DEBUG
    DumpProgKeyList();
    DumpScreenList();
    DumpWinMenuList();
#endif
    fclose(yyin);
}

/*
 * Check to see if olvwmrc has been changed since we last read it
 */
CheckOlvwmRC(dpy)
    Display *dpy;
{
char *p = FindOlvwmRC(&olvwmBuf);

    if (p == NULL
          || olvwmBuf.st_mtime != olvwmOldBuf.st_mtime
          || olvwmBuf.st_size != olvwmOldBuf.st_size
          || olvwmBuf.st_dev != olvwmOldBuf.st_dev
          || olvwmBuf.st_ino != olvwmOldBuf.st_ino) {
      RefreshKeyGrabs(dpy);
      olvwmOldBuf = olvwmBuf;
    }
}

DestroyProgScreen(p)
    ProgScreen	*p;
{
    MemFree(p->target);
    return False;
}

DestroyProgKeyNode(p)
    ProgKeyNode	*p;
{
    MemFree(p->parameter);
    return False;
}

DestroyProgKey(p)
    ProgKey	*p;
{
    ListApply(p->todo, DestroyProgKeyNode, 0);
    ListDestroy(p->todo);
    return False;
}

DestroyWinMenuActions(p)
    WinMenuActions	*p;
{
    ListApply(p->actions, DestroyProgKeyNode, 0);
    ListDestroy(p->actions);
    return False;
}

ReInitOlvwmRC(ldpy)
    Display	*ldpy;

{
    ListApply(ProgScreenList, DestroyProgScreen, 0);
    ListDestroy(ProgScreenList);
    ListApply(ProgKeyList, DestroyProgKey, 0);
    ListDestroy(ProgKeyList);
    ListApply(WinMenuActionsList, DestroyWinMenuActions, 0);
    ListDestroy(WinMenuActionsList);
    ProgScreenList = NULL;
    ProgKeyList = NULL;
    WinMenuActionsList = NULL;
    InitOlvwmRC(ldpy);
}

SearchProgString(dpy, scrInfo, name, inst, wm_class,
		 frame_x, frame_y, icon_x, icon_y)
    Display	*dpy;
    ScreenInfo	*scrInfo;
    char	*name, *inst, *wm_class;
    int		*frame_x, *frame_y, *icon_x, *icon_y;
{
ProgScreen	*p = NULL;
int		dw = DisplayWidth(dpy, scrInfo->screen);
int		dh = DisplayHeight(dpy, scrInfo->screen);

    if (name)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, name);
    if (!p && inst)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, inst);
    if (!p && wm_class)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, wm_class);
    if (p) {
	*frame_x = (*frame_x % dw) + dw * (p->screen % scrInfo->vdm->columns) +
				scrInfo->vdm->offsetX;
	*icon_x = (*icon_x % dw) + dw * (p->screen % scrInfo->vdm->columns) +
				scrInfo->vdm->offsetX;
	*frame_y = (*frame_y % dh) + dh * (p->screen / scrInfo->vdm->columns) +
				scrInfo->vdm->offsetY;
	*icon_y = (*icon_y % dh) + dh * (p->screen / scrInfo->vdm->columns) +
				scrInfo->vdm->offsetY;
    }
}

CheckForKeyProg(dpy, ev)
    Display	*dpy;
    XEvent	*ev;
{
ProgKey	*p;
WinGeneric	*win;

    p = (ProgKey *) ListApply(ProgKeyList, matchProgKey, ev);
    if (!p)
	return False;
    if (ev->xkey.type != KeyPress)
	return False;
    win = (WinGeneric *) WIGetInfo(ev->xkey.root);
    if (!win)
        win = (WinGeneric *) VGetInfo(ev->xkey.root);
    if (!win)
	ErrorWarning(gettext("Unexpected window keyboard event"));
    else {
	applyIsKey = True;
	ListApply(p->todo, applyAction, win->core.client);
    }
    return True;
}

DumpProgKeyNode(n)
    ProgKeyNode	*n;
{
    printf(gettext("Action %d parameter %s\n"), n->action, n->parameter);
    return False;
}

DumpProgKey(p)
    ProgKey	*p;
{
    printf(gettext("Actions for key %d mask %x\n"), p->keycode, p->modmask);
    ListApply(p->todo, DumpProgKeyNode, 0);
    return False;
}

DumpProgKeyList()
{
    ListApply(ProgKeyList, DumpProgKey, 0);
}

DumpProgScreen(p)
    ProgScreen	*p;
{
    printf(gettext("Screen %d:  %s\n"), p->screen, p->target);
    return False;
}

DumpWinMenu(p)
    WinMenuActions	*p;
{
    printf(gettext("Menu key %s\n"), p->key);
    ListApply(p->actions, DumpProgKeyNode, 0);
    return False;
}

DumpScreenList()
{
    ListApply(ProgScreenList, DumpProgScreen, 0);
}

DumpWinMenuList()
{
    ListApply(WinMenuActionsList, DumpWinMenu, 0);
}

char *
LookupToken(src, delim)
    char	*src;
    char	*delim;

{
static char	last[128], *next, *final;
char	*s, *t;
int	idx = 0;

    if (src) {
	next = src;
	final = src + strlen(src);
    }
    if (next > final)
	return NULL;
    s = next;
    while (*s && !strchr(delim, *s)) {
	if (*s == '\\')
	    s++;
	else if (*s == '\"') {
	    last[idx++] = *s++;
	    while (*s && *s != '\"')
		last[idx++] = *s++;
	}
	else if (*s == '\'') {
	    last[idx++] = *s++;
	    while (*++s && *s != '\'')
		last[idx++] = *s++;
	}
	last[idx++] = *s++;
    }
    *s = '\0';
    last[idx] = '\0';
    next = s + 1;
    return last;
}

MenuOfWindowsAction(dpy,winInfo,menuInfo,idx)
Display 	*dpy;
WinGeneric      *winInfo;
MenuInfo    	*menuInfo;
int     	idx;
{
Client	*cli;
List	*l;

    cli = (Client *) menuInfo->menu->buttons[idx]->action.submenu;
    l = (List *) ListApply(WinMenuActionsList,
			matchProgString, cli->framewin->fcore.name);
    if (!l)
        l = (List *) ListApply(WinMenuActionsList, matchProgString, cli->wmInstance);
    if (!l)
        l = (List *) ListApply(WinMenuActionsList, matchProgString, cli->wmClass);
    if (l) {
	applyIsKey = False;
	ListApply(l, applyAction, cli);
    }
    else {
	/* Warp */
	VDMMoveTo(dpy, cli, cli->framewin->core.x, cli->framewin->core.y);
	/* Open */
	if (cli && cli->wmState == IconicState)
	    StateIconNorm(cli);
	/* Raise */
	RaiseWindow(cli->framewin);
    }
}

static void *
addButton(cli, menu)
    Client	*cli;
    Menu	*menu;

{
Button	*b;
int	len;

#define MENU_LENGTH	(32)

    if (!cli->framewin)
	return NULL;
    if (findClient_rootid != cli->screen)
	return NULL;
    if (!menu->buttonCount++)
	menu->buttons = (Button **) MemAlloc(sizeof(Button *));
    else menu->buttons = (Button **)
		MemRealloc(menu->buttons, menu->buttonCount * sizeof(Button *));
    b = (Button *) MemAlloc(sizeof(Button));
    menu->buttons[menu->buttonCount - 1] = b;

    len = strlen(cli->framewin->fcore.name);
    if (len > MENU_LENGTH)
	len = MENU_LENGTH;

    b->label[0].kind = StringLabel;
    b->label[1].kind = NoType;
    b->label[0].string = MemAlloc(len + 4);
    b->label[0].string[0] = '\0';
    if (cli->wmState == IconicState)
        strcat(b->label[0].string, "\244");
    else strcat(b->label[0].string, "  ");
    strcat(b->label[0].string, " ");
    strncat(b->label[0].string, cli->framewin->fcore.name, MENU_LENGTH);
    b->label[1].string = NULL;
    b->helpstring[0] = b->helpstring[1] = NULL;
    b->which = 0;
    b->stacked = False;
    b->enabled = True;
    b->visible = True;
    b->action.callback = MenuOfWindowsAction;
    b->action.submenu = (Menu *) cli;
    b->generate_func = NULL;
    return NULL;
}

static int
cmpButton(b1, b2)
    Button	**b1, **b2;

{
int	type;
char	buf1[256], buf2[256];

    type = ((*b1)->label[0].string[0] == '\244') |
	   (((*b2)->label[0].string[0] == '\244') << 1);

    switch(type) {
	default:
	case 0:
    	    return strcmp(gettext((*b1)->label[0].string),
		  	  gettext((*b2)->label[0].string));
	case 1:
	    return 1;
	case 2:
	    return -1;
	case 3:
	    strnlower(buf1, gettext((*b1)->label[0].string + 1),
		      strlen(gettext((*b1)->label[0].string + 1)));
	    strnlower(buf2, gettext((*b2)->label[0].string + 1),
		      strlen(gettext((*b2)->label[0].string + 1)));
    	    return strcmp(buf1, buf2);
    }
}

GenWinMenuFunc(dpy, menuInfo, bindex, cache, winInfo, depth)
    Display	*dpy;
    MenuInfo	*menuInfo;
    int		bindex;
    MenuCache	*cache;
    WinGeneric	*winInfo;
    int		depth;
{
Menu	*menu;
int	columns;
extern MenuInfo	*MenuInfoCreate();

    columns = menuInfo->buttons[bindex].subMenu->menu->prefColSize;
    MenuInfoDestroy(menuInfo->buttons[bindex].subMenu);
    menu = (Menu *) MemAlloc(sizeof(Menu));
    menu->buttons = NULL;
    menu->buttonCount = 0;
    menu->buttonDefault = NOBUTTON;
    menu->hasPushPin = False;
    menu->menudirty = True;
    menu->helpstring = "olvwm:WinMenu";
    menu->btnPerCol = 0;
    menu->maxLabWidth = 0;
    menu->prefColSize = columns;

    findClient_rootid = winInfo->core.client->screen;
    ListApply(ActiveClientList, addButton, menu);
    if (GRV.VirtualMenuSort == SortAlpha)
        qsort(menu->buttons, menu->buttonCount, sizeof(Button *), cmpButton);

    menuInfo->buttons[bindex].subMenu =
			MenuInfoCreate(cache, winInfo, menu, depth);
}

/*
 * $XConsortium: charproc.c,v 1.176.1.1 93/11/03 17:24:20 gildea Exp $
 * $Id: charproc.c,v 1.3 1994/06/27 17:46:15 asami Exp $
 */

/*
 * Copyright 1988 Massachusetts Institute of Technology
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *                         All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

/* charproc.c */

#include "ptyx.h"
#include "VTparse.h"
#include "data.h"
#include "error.h"
#include "menu.h"
#include "main.h"
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <ctype.h>

/*
 * Check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN.
 * Note that this macro may evaluate its argument more than once.
 */
#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define E_TEST(err) ((err) == EAGAIN || (err) == EWOULDBLOCK)
#else
#ifdef EAGAIN
#define E_TEST(err) ((err) == EAGAIN)
#else
#define E_TEST(err) ((err) == EWOULDBLOCK)
#endif
#endif

extern jmp_buf VTend;

extern XtAppContext app_con;
extern Widget toplevel;
extern void exit();
extern char *malloc();
extern char *realloc();

static void VTallocbuf();
static int finput();
static void dotext();
static void WriteText();

static void bitset(), bitclr();
    
#define	DEFAULT		-1
#define	TEXT_BUF_SIZE	256
#define TRACKTIMESEC	4L
#define TRACKTIMEUSEC	0L
#define BELLSUPPRESSMSEC 200

#define XtNalwaysHighlight "alwaysHighlight"
#define XtNappcursorDefault "appcursorDefault"
#define XtNappkeypadDefault "appkeypadDefault"
#define XtNbellSuppressTime "bellSuppressTime"
#define XtNboldFont "boldFont"
#define XtNc132 "c132"
#define XtNcharClass "charClass"
#define XtNcurses "curses"
#define XtNcursorColor "cursorColor"
#ifdef KTERM_COLOR
#define XtNtextColor0 "textColor0"
#define XtNtextColor1 "textColor1"
#define XtNtextColor2 "textColor2"
#define XtNtextColor3 "textColor3"
#define XtNtextColor4 "textColor4"
#define XtNtextColor5 "textColor5"
#define XtNtextColor6 "textColor6"
#define XtNtextColor7 "textColor7"
#endif /* KTERM_COLOR */
#define XtNcutNewline "cutNewline"
#define XtNcutToBeginningOfLine "cutToBeginningOfLine"
#define XtNeightBitInput "eightBitInput"
#define XtNeightBitOutput "eightBitOutput"
#define XtNgeometry "geometry"
#define XtNtekGeometry "tekGeometry"
#define XtNinternalBorder "internalBorder"
#define XtNjumpScroll "jumpScroll"
#ifdef ALLOWLOGGING
#define XtNlogFile "logFile"
#define XtNlogging "logging"
#define XtNlogInhibit "logInhibit"
#endif
#define XtNloginShell "loginShell"
#define XtNmarginBell "marginBell"
#define XtNpointerColor "pointerColor"
#define XtNpointerColorBackground "pointerColorBackground"
#define XtNpointerShape "pointerShape"
#define XtNmultiClickTime "multiClickTime"
#define XtNmultiScroll "multiScroll"
#define XtNnMarginBell "nMarginBell"
#define XtNresizeGravity "resizeGravity"
#define XtNreverseWrap "reverseWrap"
#define XtNautoWrap "autoWrap"
#define XtNsaveLines "saveLines"
#define XtNscrollBar "scrollBar"
#define XtNscrollTtyOutput "scrollTtyOutput"
#define XtNscrollKey "scrollKey"
#define XtNscrollLines "scrollLines"
#define XtNscrollPos "scrollPos"
#define XtNsignalInhibit "signalInhibit"
#ifdef STATUSLINE
#define XtNstatusLine "statusLine"
#define XtNstatusNormal "statusNormal"
#endif /* STATUSLINE */
#define XtNtekInhibit "tekInhibit"
#define XtNtekSmall "tekSmall"
#define XtNtekStartup "tekStartup"
#define XtNtiteInhibit "titeInhibit"
#define XtNvisualBell "visualBell"
#define XtNallowSendEvents "allowSendEvents"
#ifdef KTERM
#define XtNromanKanaFont "romanKanaFont"
#define XtNromanKanaBoldFont "romanKanaBoldFont"
# ifdef KTERM_MBCS
#define XtNkanjiFont "kanjiFont"
#define XtNkanjiBoldFont "kanjiBoldFont"
# endif /* KTERM_MBCS */
# ifdef KTERM_KANJI
#define XtNkanjiMode "kanjiMode"
# endif /* KTERM_KANJI */
#define XtNfontList "fontList"
#define XtNboldFontList "boldFontList"
#define XtNlineSpace "lineSpace"
#endif /* KTERM */

#define XtCAlwaysHighlight "AlwaysHighlight"
#define XtCAppcursorDefault "AppcursorDefault"
#define XtCAppkeypadDefault "AppkeypadDefault"
#define XtCBellSuppressTime "BellSuppressTime"
#define XtCBoldFont "BoldFont"
#define XtCC132 "C132"
#define XtCCharClass "CharClass"
#define XtCCurses "Curses"
#define XtCCutNewline "CutNewline"
#define XtCCutToBeginningOfLine "CutToBeginningOfLine"
#define XtCEightBitInput "EightBitInput"
#define XtCEightBitOutput "EightBitOutput"
#define XtCGeometry "Geometry"
#define XtCJumpScroll "JumpScroll"
#ifdef ALLOWLOGGING
#define XtCLogfile "Logfile"
#define XtCLogging "Logging"
#define XtCLogInhibit "LogInhibit"
#endif
#define XtCLoginShell "LoginShell"
#define XtCMarginBell "MarginBell"
#define XtCMultiClickTime "MultiClickTime"
#define XtCMultiScroll "MultiScroll"
#define XtCColumn "Column"
#define XtCResizeGravity "ResizeGravity"
#define XtCReverseWrap "ReverseWrap"
#define XtCAutoWrap "AutoWrap"
#define XtCSaveLines "SaveLines"
#define XtCScrollBar "ScrollBar"
#define XtCScrollLines "ScrollLines"
#define XtCScrollPos "ScrollPos"
#define XtCScrollCond "ScrollCond"
#define XtCSignalInhibit "SignalInhibit"
#ifdef STATUSLINE
#define XtCStatusLine "StatusLine"
#define XtCStatusNormal "StatusNormal"
#endif /* STATUSLINE */
#define XtCTekInhibit "TekInhibit"
#define XtCTekSmall "TekSmall"
#define XtCTekStartup "TekStartup"
#define XtCTiteInhibit "TiteInhibit"
#define XtCVisualBell "VisualBell"
#define XtCAllowSendEvents "AllowSendEvents"
#ifdef KTERM
#define XtCRomanKanaFont "RomanKanaFont"
# ifdef KTERM_MBCS
#define XtCKanjiFont "KanjiFont"
# endif /* KTERM_MBCS */
# ifdef KTERM_KANJI
#define XtCKanjiMode "KanjiMode"
# endif /* KTERM_KANJI */
#define XtCFontList "FontList"
#define XtCLineSpace "LineSpace"
#endif /* KTERM */

#define	doinput()		(bcnt-- > 0 ? *bptr++ : in_put())

static int nparam;
static ANSI reply;
static int param[NPARAM];

static unsigned long ctotal;
static unsigned long ntotal;
static jmp_buf vtjmpbuf;

extern int groundtable[];
extern int csitable[];
extern int dectable[];
extern int eigtable[];
extern int esctable[];
extern int iestable[];
extern int igntable[];
extern int scrtable[];
extern int scstable[];
#ifdef KTERM_MBCS
extern int mbcstable[];
static Char pending_byte;
#endif /* KTERM_MBCS */


/* event handlers */
extern void HandleKeyPressed(), HandleEightBitKeyPressed();
extern void HandleStringEvent();
extern void HandleEnterWindow();
extern void HandleLeaveWindow();
extern void HandleBellPropertyChange();
extern void HandleFocusChange();
static void HandleKeymapChange();
extern void HandleInsertSelection();
extern void HandleSelectStart(), HandleKeyboardSelectStart();
extern void HandleSelectExtend(), HandleSelectSet();
extern void HandleSelectEnd(), HandleKeyboardSelectEnd();
extern void HandleStartExtend(), HandleKeyboardStartExtend();
static void HandleBell();
static void HandleVisualBell();
static void HandleIgnore();
extern void HandleSecure();
extern void HandleScrollForward();
extern void HandleScrollBack();
extern void HandleCreateMenu(), HandlePopupMenu();
extern void HandleSetFont();
extern void SetVTFont();
#ifdef KTERM_KCONV
extern void BeginConversion();
#endif /* KTERM_KCONV */

extern Boolean SendMousePosition();
extern void ScrnSetAttributes();

/*
 * NOTE: VTInitialize zeros out the entire ".screen" component of the 
 * XtermWidget, so make sure to add an assignment statement in VTInitialize() 
 * for each new ".screen" field added to this resource list.
 */

/* Defaults */
static  Boolean	defaultFALSE	   = FALSE;
static  Boolean	defaultTRUE	   = TRUE;
static  int	defaultBorderWidth = DEFBORDERWIDTH;
static  int	defaultIntBorder   = DEFBORDER;
static  int	defaultSaveLines   = SAVELINES;
static	int	defaultScrollLines = SCROLLLINES;
static  int	defaultNMarginBell = N_MARGINBELL;
static  int	defaultMultiClickTime = MULTICLICKTIME;
#ifdef KTERM
static  int	defaultLineSpace    = LINESPACE;
#endif /* KTERM */
static  int	defaultBellSuppressTime = BELLSUPPRESSMSEC;
static	char *	_Font_Selected_ = "yes";  /* string is arbitrary */

/*
 * Warning, the following must be kept under 1024 bytes or else some 
 * compilers (particularly AT&T 6386 SVR3.2) will barf).  Workaround is to
 * declare a static buffer and copy in at run time (the the Athena text widget
 * does).  Yuck.
 */
#ifdef KTERM_KCONV
static char defaultTranslations[] =
"\
  Ctrl <KeyPress> Kanji:	begin-conversion(_JAPANESE_CONVERSION) \n\
  Shift <KeyPress> Prior:	scroll-back(1,halfpage) \n\
  Shift <KeyPress> Next:	scroll-forw(1,halfpage) \n\
  Shift <KeyPress> Select:	select-cursor-start() select-cursor-end(PRIMARY, CUT_BUFFER0) \n\
  Shift <KeyPress> Insert:	insert-selection(PRIMARY, CUT_BUFFER0) \n\
       ~Meta<KeyPress>: 	insert-seven-bit()	\n\
        Meta<KeyPress>: 	insert-eight-bit()	\n\
 Ctrl ~Meta<Btn1Down>:          popup-menu(mainMenu) \n\
      ~Meta <Btn1Down>:		select-start()	\n\
      ~Meta <Btn1Motion>:	select-extend() \n\
 Ctrl ~Meta <Btn2Down>:         popup-menu(vtMenu) \n\
~Ctrl ~Meta <Btn2Down>:		ignore()	\n\
~Ctrl ~Meta <Btn2Up>:		insert-selection(PRIMARY, CUT_BUFFER0) \n\
 Ctrl ~Meta <Btn3Down>:         popup-menu(fontMenu) \n\
~Ctrl ~Meta <Btn3Down>:		start-extend()	\n\
      ~Meta <Btn3Motion>:	select-extend()	\n\
~Ctrl ~Meta <BtnUp>:		select-end(PRIMARY, CUT_BUFFER0) \n\
	    <BtnDown>:		bell(0)		\
";
#else /* !KTERM_KCONV */
static char defaultTranslations[] =
"\
 Shift <KeyPress> Prior:scroll-back(1,halfpage) \n\
  Shift <KeyPress> Next:scroll-forw(1,halfpage) \n\
Shift <KeyPress> Select:select-cursor-start() select-cursor-end(PRIMARY, CUT_BUFFER0) \n\
Shift <KeyPress> Insert:insert-selection(PRIMARY, CUT_BUFFER0) \n\
       ~Meta <KeyPress>:insert-seven-bit() \n\
        Meta <KeyPress>:insert-eight-bit() \n\
       !Ctrl <Btn1Down>:popup-menu(mainMenu) \n\
  !Lock Ctrl <Btn1Down>:popup-menu(mainMenu) \n\
       ~Meta <Btn1Down>:select-start() \n\
     ~Meta <Btn1Motion>:select-extend() \n\
       !Ctrl <Btn2Down>:popup-menu(vtMenu) \n\
  !Lock Ctrl <Btn2Down>:popup-menu(vtMenu) \n\
 ~Ctrl ~Meta <Btn2Down>:ignore() \n\
   ~Ctrl ~Meta <Btn2Up>:insert-selection(PRIMARY, CUT_BUFFER0) \n\
       !Ctrl <Btn3Down>:popup-menu(fontMenu) \n\
  !Lock Ctrl <Btn3Down>:popup-menu(fontMenu) \n\
 ~Ctrl ~Meta <Btn3Down>:start-extend() \n\
     ~Meta <Btn3Motion>:select-extend()	\n\
                <BtnUp>:select-end(PRIMARY, CUT_BUFFER0) \n\
	      <BtnDown>:bell(0) \
";
#endif /* !KTERM_KCONV */

static XtActionsRec actionsList[] = { 
#ifdef KTERM_KCONV
    { "begin-conversion", BeginConversion },
#endif /* KTERM_KCONV */
    { "bell",		  HandleBell },
    { "create-menu",	  HandleCreateMenu },
    { "ignore",		  HandleIgnore },
    { "insert",		  HandleKeyPressed },  /* alias for insert-seven-bit */
    { "insert-seven-bit", HandleKeyPressed },
    { "insert-eight-bit", HandleEightBitKeyPressed },
    { "insert-selection", HandleInsertSelection },
    { "keymap", 	  HandleKeymapChange },
    { "popup-menu",	  HandlePopupMenu },
    { "secure",		  HandleSecure },
    { "select-start",	  HandleSelectStart },
    { "select-extend",	  HandleSelectExtend },
    { "select-end",	  HandleSelectEnd },
    { "select-set",	  HandleSelectSet },
    { "select-cursor-start",	  HandleKeyboardSelectStart },
    { "select-cursor-end",	  HandleKeyboardSelectEnd },
    { "set-vt-font",	  HandleSetFont },
    { "start-extend",	  HandleStartExtend },
    { "start-cursor-extend",	  HandleKeyboardStartExtend },
    { "string",		  HandleStringEvent },
    { "scroll-forw",	  HandleScrollForward },
    { "scroll-back",	  HandleScrollBack },
    /* menu actions */
    { "allow-send-events",	HandleAllowSends },
    { "set-visual-bell",	HandleSetVisualBell },
#ifdef ALLOWLOGGING
    { "set-logging",		HandleLogging },
#endif
    { "redraw",			HandleRedraw },
    { "send-signal",		HandleSendSignal },
    { "quit",			HandleQuit },
    { "set-scrollbar",		HandleScrollbar },
    { "set-jumpscroll",		HandleJumpscroll },
    { "set-reverse-video",	HandleReverseVideo },
    { "set-autowrap",		HandleAutoWrap },
    { "set-reversewrap",	HandleReverseWrap },
    { "set-autolinefeed",	HandleAutoLineFeed },
    { "set-appcursor",		HandleAppCursor },
    { "set-appkeypad",		HandleAppKeypad },
    { "set-scroll-on-key",	HandleScrollKey },
    { "set-scroll-on-tty-output",	HandleScrollTtyOutput },
    { "set-allow132",		HandleAllow132 },
    { "set-cursesemul",		HandleCursesEmul },
    { "set-marginbell",		HandleMarginBell },
    { "set-altscreen",		HandleAltScreen },
    { "soft-reset",		HandleSoftReset },
    { "hard-reset",		HandleHardReset },
    { "clear-saved-lines",	HandleClearSavedLines },
    { "set-terminal-type",	HandleSetTerminalType },
    { "set-visibility",		HandleVisibility },
    { "set-tek-text",		HandleSetTekText },
    { "tek-page",		HandleTekPage },
    { "tek-reset",		HandleTekReset },
    { "tek-copy",		HandleTekCopy },
    { "visual-bell",		HandleVisualBell },
#ifdef STATUSLINE
    { "set-statusline",		HandleStatusLine },
    { "set-reversestatus",	HandleStatusReverse },
#endif /* STATUSLINE */
#ifdef KTERM_KANJI
    { "set-kanji-mode",		HandleSetKanjiMode },
#endif /* KTERM_KANJI */
};

static XtResource resources[] = {
#ifdef KTERM
{XtNfont, XtCFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_n[F_ISO8859_1]), XtRString,
	(caddr_t) NULL},
{XtNboldFont, XtCFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_b[F_ISO8859_1]), XtRString,
	(caddr_t) NULL},
{XtNromanKanaFont, XtCRomanKanaFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_n[F_JISX0201_0]), XtRString,
	(caddr_t) NULL},
{XtNromanKanaBoldFont, XtCRomanKanaFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_b[F_JISX0201_0]), XtRString,
	(caddr_t) NULL},
#  ifdef KTERM_MBCS
{XtNkanjiFont, XtCKanjiFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_n[F_JISX0208_0]), XtRString,
	(caddr_t) NULL},
{XtNkanjiBoldFont, XtCKanjiFont, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc._f_b[F_JISX0208_0]), XtRString,
	(caddr_t) NULL},
#  endif /* KTERM_MBCS */
#  ifdef KTERM_KANJI
{XtNkanjiMode, XtCKanjiMode, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc.k_m), XtRString,
	(caddr_t) NULL},
#  endif /* KTERM_KANJI */
{XtNfontList, XtCFontList, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc.fontlist), XtRString,
	(caddr_t) NULL},
{XtNboldFontList, XtCFontList, XtRString, sizeof(char *),
	XtOffset(XtermWidget, misc.bfontlist), XtRString,
	(caddr_t) NULL},
{XtNlineSpace, XtCLineSpace, XtRInt, sizeof(int),
	XtOffset(XtermWidget, screen.linespace),
	XtRInt, (caddr_t) &defaultLineSpace},
#else /* !KTERM */
{XtNfont, XtCFont, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, misc.f_n), XtRString,
	DEFFONT},
{XtNboldFont, XtCBoldFont, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, misc.f_b), XtRString,
	DEFBOLDFONT},
#endif /* !KTERM */
{XtNc132, XtCC132, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.c132),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNcharClass, XtCCharClass, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, screen.charClass),
	XtRString, (caddr_t) NULL},
{XtNcurses, XtCCurses, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.curses),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNcutNewline, XtCCutNewline, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.cutNewline),
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNcutToBeginningOfLine, XtCCutToBeginningOfLine, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.cutToBeginningOfLine),
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, core.background_pixel),
	XtRString, "XtDefaultBackground"},
{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.foreground),
	XtRString, "XtDefaultForeground"},
{XtNcursorColor, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.cursorcolor),
	XtRString, "XtDefaultForeground"},
#ifdef KTERM_COLOR
{XtNtextColor0, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[0]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor1, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[1]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor2, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[2]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor3, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[3]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor4, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[4]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor5, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[5]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor6, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[6]),
	XtRString, "XtDefaultForeground"},
{XtNtextColor7, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.textcolor[7]),
	XtRString, "XtDefaultForeground"},
#endif /* KTERM_COLOR */
{XtNeightBitInput, XtCEightBitInput, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.input_eight_bits), 
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNeightBitOutput, XtCEightBitOutput, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.output_eight_bits), 
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNgeometry,XtCGeometry, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, misc.geo_metry),
	XtRString, (caddr_t) NULL},
{XtNalwaysHighlight,XtCAlwaysHighlight,XtRBoolean,
        sizeof(Boolean),XtOffsetOf(XtermWidgetRec, screen.always_highlight),
        XtRBoolean, (caddr_t) &defaultFALSE},
{XtNappcursorDefault,XtCAppcursorDefault,XtRBoolean,
        sizeof(Boolean),XtOffsetOf(XtermWidgetRec, misc.appcursorDefault),
        XtRBoolean, (XtPointer) &defaultFALSE},
{XtNappkeypadDefault,XtCAppkeypadDefault,XtRBoolean,
        sizeof(Boolean),XtOffsetOf(XtermWidgetRec, misc.appkeypadDefault),
        XtRBoolean, (XtPointer) &defaultFALSE},
{XtNbellSuppressTime, XtCBellSuppressTime, XtRInt, sizeof(int),
        XtOffsetOf(XtermWidgetRec, screen.bellSuppressTime),
        XtRInt, (XtPointer) &defaultBellSuppressTime},
{XtNtekGeometry,XtCGeometry, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, misc.T_geometry),
	XtRString, (caddr_t) NULL},
{XtNinternalBorder,XtCBorderWidth,XtRInt, sizeof(int),
	XtOffsetOf(XtermWidgetRec, screen.border),
	XtRInt, (caddr_t) &defaultIntBorder},
{XtNjumpScroll, XtCJumpScroll, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.jumpscroll),
	XtRBoolean, (caddr_t) &defaultTRUE},
#ifdef ALLOWLOGGING
{XtNlogFile, XtCLogfile, XtRString, sizeof(char *),
	XtOffsetOf(XtermWidgetRec, screen.logfile),
	XtRString, (caddr_t) NULL},
{XtNlogging, XtCLogging, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.log_on),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNlogInhibit, XtCLogInhibit, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.logInhibit),
	XtRBoolean, (caddr_t) &defaultFALSE},
#endif
{XtNloginShell, XtCLoginShell, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.login_shell),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNmarginBell, XtCMarginBell, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.marginbell),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNpointerColor, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.mousecolor),
	XtRString, "XtDefaultForeground"},
{XtNpointerColorBackground, XtCBackground, XtRPixel, sizeof(Pixel),
	XtOffsetOf(XtermWidgetRec, screen.mousecolorback),
	XtRString, "XtDefaultBackground"},
{XtNpointerShape,XtCCursor, XtRCursor, sizeof(Cursor),
	XtOffsetOf(XtermWidgetRec, screen.pointer_cursor),
	XtRString, (caddr_t) "xterm"},
{XtNmultiClickTime,XtCMultiClickTime, XtRInt, sizeof(int),
	XtOffsetOf(XtermWidgetRec, screen.multiClickTime),
	XtRInt, (caddr_t) &defaultMultiClickTime},
{XtNmultiScroll,XtCMultiScroll, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.multiscroll),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNnMarginBell,XtCColumn, XtRInt, sizeof(int),
	XtOffsetOf(XtermWidgetRec, screen.nmarginbell),
	XtRInt, (caddr_t) &defaultNMarginBell},
{XtNreverseVideo,XtCReverseVideo,XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.re_verse),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNresizeGravity, XtCResizeGravity, XtRGravity, sizeof(XtGravity),
	XtOffsetOf(XtermWidgetRec, misc.resizeGravity),
	XtRImmediate, (XtPointer) SouthWestGravity},
{XtNreverseWrap,XtCReverseWrap, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.reverseWrap),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNautoWrap,XtCAutoWrap, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.autoWrap),
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNsaveLines, XtCSaveLines, XtRInt, sizeof(int),
	XtOffsetOf(XtermWidgetRec, screen.savelines),
	XtRInt, (caddr_t) &defaultSaveLines},
{XtNscrollBar, XtCScrollBar, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.scrollbar),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNscrollTtyOutput,XtCScrollCond, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.scrollttyoutput),
	XtRBoolean, (caddr_t) &defaultTRUE},
{XtNscrollKey, XtCScrollCond, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.scrollkey),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNscrollLines, XtCScrollLines, XtRInt, sizeof(int),
	XtOffsetOf(XtermWidgetRec, screen.scrolllines),
	XtRInt, (caddr_t) &defaultScrollLines},
{XtNsignalInhibit,XtCSignalInhibit,XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.signalInhibit),
	XtRBoolean, (caddr_t) &defaultFALSE},
#ifdef STATUSLINE
{XtNstatusLine, XtCStatusLine, XtRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.statusline),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNstatusNormal, XtCStatusNormal, XtRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.statusnormal),
	XtRBoolean, (caddr_t) &defaultFALSE},
#endif /* STATUSLINE */
{XtNtekInhibit, XtCTekInhibit, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.tekInhibit),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNtekSmall, XtCTekSmall, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.tekSmall),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNtekStartup, XtCTekStartup, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.TekEmu),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNtiteInhibit, XtCTiteInhibit, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, misc.titeInhibit),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNvisualBell, XtCVisualBell, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.visualbell),
	XtRBoolean, (caddr_t) &defaultFALSE},
{XtNallowSendEvents, XtCAllowSendEvents, XtRBoolean, sizeof(Boolean),
	XtOffsetOf(XtermWidgetRec, screen.allowSendEvents),
	XtRBoolean, (caddr_t) &defaultFALSE},
#ifdef KTERM
{"fontList1", "FontList1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"fontList2", "FontList2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"fontList3", "FontList3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"fontList4", "FontList4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"fontList5", "FontList5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"fontList6", "FontList6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_list[fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"boldFontList1", "FontList1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"boldFontList2", "FontList2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"boldFontList3", "FontList3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"boldFontList4", "FontList4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"boldFontList5", "FontList5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"boldFontList6", "FontList6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_bfont_list[fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"font1", "Font1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"font2", "Font2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"font3", "Font3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"font4", "Font4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"font5", "Font5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"font6", "Font6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_ISO8859_1][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"boldFont1", "Font1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"boldFont2", "Font2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"boldFont3", "Font3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"boldFont4", "Font4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"boldFont5", "Font5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"boldFont6", "Font6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_ISO8859_1][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont1", "RomanKanaFont1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont2", "RomanKanaFont2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont3", "RomanKanaFont3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont4", "RomanKanaFont4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont5", "RomanKanaFont5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"romanKanaFont6", "RomanKanaFont6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0201_0][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont1", "RomanKanaFont1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont2", "RomanKanaFont2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont3", "RomanKanaFont3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont4", "RomanKanaFont4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont5", "RomanKanaFont5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"romanKanaBoldFont6", "RomanKanaFont6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0201_0][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
# ifdef KTERM_MBCS
{"kanjiFont1", "KanjiFont1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"kanjiFont2", "KanjiFont2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"kanjiFont3", "KanjiFont3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"kanjiFont4", "KanjiFont4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"kanjiFont5", "KanjiFont5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"kanjiFont6", "KanjiFont6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_font_names[F_JISX0208_0][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont1", "KanjiFont1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont2", "KanjiFont2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont3", "KanjiFont3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont4", "KanjiFont4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont5", "KanjiFont5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"kanjiBoldFont6", "KanjiFont6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen._menu_bfont_names[F_JISX0208_0][fontMenu_font6]),
	XtRString, (caddr_t) NULL},
# endif /* KTERM_MBCS */
#else /* !KTERM */
{"font1", "Font1", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font1]),
	XtRString, (caddr_t) NULL},
{"font2", "Font2", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font2]),
	XtRString, (caddr_t) NULL},
{"font3", "Font3", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font3]),
	XtRString, (caddr_t) NULL},
{"font4", "Font4", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font4]),
	XtRString, (caddr_t) NULL},
{"font5", "Font5", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font5]),
	XtRString, (caddr_t) NULL},
{"font6", "Font6", XtRString, sizeof(String),
	XtOffsetOf(XtermWidgetRec, screen.menu_font_names[fontMenu_font6]),
	XtRString, (caddr_t) NULL},
#endif /* !KTERM */
};

static void VTClassInit();
static void VTInitialize();
static void VTRealize();
static void VTExpose();
static void VTResize();
static void VTDestroy();
static Boolean VTSetValues();

static WidgetClassRec xtermClassRec = {
  {
/* core_class fields */	
    /* superclass	  */	(WidgetClass) &widgetClassRec,
    /* class_name	  */	"VT100",
    /* widget_size	  */	sizeof(XtermWidgetRec),
    /* class_initialize   */    VTClassInit,
    /* class_part_initialize */ NULL,
    /* class_inited       */	FALSE,
    /* initialize	  */	VTInitialize,
    /* initialize_hook    */    NULL,				
    /* realize		  */	VTRealize,
    /* actions		  */	actionsList,
    /* num_actions	  */	XtNumber(actionsList),
    /* resources	  */	resources,
    /* num_resources	  */	XtNumber(resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	FALSE,
    /* compress_enterleave */   TRUE,
    /* visible_interest	  */	FALSE,
    /* destroy		  */	VTDestroy,
    /* resize		  */	VTResize,
    /* expose		  */	VTExpose,
    /* set_values	  */	VTSetValues,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    NULL,
    /* get_values_hook    */    NULL,
    /* accept_focus	  */	NULL,
    /* version            */    XtVersion,
    /* callback_offsets   */    NULL,
    /* tm_table           */    defaultTranslations,
    /* query_geometry     */    XtInheritQueryGeometry,
    /* display_accelerator*/    XtInheritDisplayAccelerator,
    /* extension          */    NULL
  }
};

WidgetClass xtermWidgetClass = (WidgetClass)&xtermClassRec;

#ifdef KTERM
doSS(gset)
short gset;
{
	Char dotextbuf[2];
	register Char c, min, max;
	register Char *cp = bptr;
	register Char *dcp = dotextbuf;

	if (gset & CS96) {
		min = 0x20; max = 0x7f;
	} else {
		min = 0x21; max = 0x7e;
	}
	if (*cp & 0x80) {
		min |= 0x80; max |= 0x80;
	}
# ifdef KTERM_MBCS
	if (gset & MBCS) {
		if (bcnt == 1) {
			c = *cp;
			if (min <= c && c <= max) {
				pending_byte = c;
				bcnt = 0;
				bptr++;
				return;
			}
		} else {
			/* first byte */
			c = *cp++;
			if (c < min || max < c) {
				cp--;
				goto end;
			}
			*dcp++ = c;
			/* second byte */
			c = *cp++;
			if (c < min || max < c) {
				/*
				 * Incomplete multi-byte character.
				 * Skip its first byte.
				 */
				dcp--;
				cp--;
				goto end;
			}
			*dcp++ = c;
			dotext(&term->screen, term->flags, gset, dotextbuf, dcp);
		}
	} else
# endif /* KTERM_MBCS */
	{
		min &= 0xa0;
		c = *cp++;
		if (c < min || max < c) {
			cp--;
			goto end;
		}
		*dcp++ = c;
		dotext(&term->screen, term->flags, gset, dotextbuf, dcp);
	}
    end:
	bcnt -= cp - bptr;
	bptr = cp;
	/*
	 * process SPACE or DEL characters in 94chars gset string.
	 */
	if (c < min || max < c) {
	    if ((c & ~0x80) == 0x20) {
		dcp = dotextbuf;
		cp++;
		*dcp++ = c;
		dotext(&term->screen, term->flags, gset, dotextbuf, dcp);
	    } else if ((c & ~0x80) == 0x7f) {
		cp++;
	    }
	}
	bcnt -= cp - bptr;
	bptr = cp;
}

doLS(gset)
short gset;
{
	Char dotextbuf[TEXT_BUF_SIZE];
	register int top;
	register Char c, min, max;
	register Char *cp = bptr;
	register Char *dcp = dotextbuf;

	if (gset & CS96) {
		min = 0x20; max = 0x7f;
	} else {
		min = 0x21; max = 0x7e;
	}
	if (*cp & 0x80) {
		min |= 0x80; max |= 0x80;
	}
# ifdef KTERM_MBCS
	if (gset & MBCS) {
		if (bcnt == 1) {
			c = *cp;
			if (min <= c && c <= max) {
				pending_byte = c;
				bcnt = 0;
				bptr++;
				return;
			}
		} else {
			top = (bcnt >TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt) / 2;
			while (top > 0) {
				/* first byte */
				c = *cp++;
				if (c < min || max < c) {
					cp--;
					break;
				}
				*dcp++ = c;
				/* second byte */
				c = *cp++;
				if (c < min || max < c) {
					/*
					 * Incomplete multi-byte character.
					 * Skip its first byte.
					 */
					dcp--;
					cp--;
					break;
				}
				*dcp++ = c;
				top--;
			}
			dotext(&term->screen, term->flags, gset, dotextbuf, dcp);
		}
	} else
# endif /* KTERM_MBCS */
	{
		min &= 0xa0;
		top = bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt;
		while (top > 0) {
			c = *cp++;
			if (c < min || max < c) {
				cp--;
				break;
			}
			*dcp++ = c;
			top--;
		}
		dotext(&term->screen, term->flags, gset, dotextbuf, dcp);
	}
	bcnt -= cp - bptr;
	bptr = cp;
	/*
	 * process SPACE or DEL characters in 94chars gset string.
	 */
	if (c < min || max < c) {
	    if ((c & ~0x80) == 0x20) {
		dcp = dotextbuf;
		top = bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt;
		while (top > 0 && *cp == c) {
			*dcp++ = c;
			cp++; top--;
		}
		dotext(&term->screen, term->flags, GSET_ASCII, dotextbuf, dcp);
	    } else if ((c & ~0x80) == 0x7f) {
		top = bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt;
		while (top > 0 && *cp == c) {
			cp++; top--;
		}
	    }
	}
	bcnt -= cp - bptr;
	bptr = cp;
}

# ifdef KTERM_KANJI
doSJIS()
{
	Char dotextbuf[TEXT_BUF_SIZE];
	register int top;
	register Char HI, LO;
	register Char *cp = bptr;
	register Char *dcp = dotextbuf;

#  define SJIS1(c) ((0x81 <= c && c <= 0x9F) || (0xE0 <= c && c <= 0xEF))
#  define SJIS2(c) (0x40 <= c && c <= 0xFC && c != 0x7F)
	if (bcnt == 1) {
		pending_byte = *cp;
		bcnt = 0;
		bptr++;
		return;
	}
	top = (bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt) / 2;
	while (top > 0) {
		HI = *cp++;
		if (!SJIS1(HI)) {
			cp--;
			break;
		}
		LO = *cp++;
		if (!SJIS2(LO)) {
			/*
			 * Incomplete shift-jis character.
			 * Skip its first byte.
			 */
			cp--;
			break;
		}
		/*
		 * SJIS to JIS code conversion:
		 */
		if (HI <= 0x9f)	HI = (HI-0x81)*2 + 0x21;
		else		HI = (HI-0xc1)*2 + 0x21;
		if (LO <= 0x7e)	     LO -= 0x1f;
		else if (LO <= 0x9e) LO -= 0x20;
		else		     LO -= 0x7e, HI += 1;
		*dcp++ = HI;
		*dcp++ = LO;
		top--;
	}
	dotext(&term->screen, term->flags, GSET_KANJI, dotextbuf, dcp);
	bcnt -= cp - bptr;
	bptr = cp;
}
# endif /* KTERM_KANJI */
#endif /* KTERM */

static void VTparse()
{
	register TScreen *screen = &term->screen;
	register int *parsestate = groundtable;
	register unsigned int c;
	register unsigned char *cp;
	register int row, col, top, bot, scstype;
#ifdef KTERM
	register Bchr *xp;
	int cs96 = 0;
#endif /* KTERM */
#ifdef KTERM_MBCS
	int mbcs = 0;
	register int ps;
#endif /* KTERM_MBCS */
	extern int TrackMouse();

	if(setjmp(vtjmpbuf))
		parsestate = groundtable;
#ifdef KTERM_MBCS
	pending_byte = 0;
#endif /* KTERM_MBCS */
	for( ; ; ) {
#ifdef KTERM_MBCS
	        ps = parsestate[c = doinput()];
# ifdef KTERM_KANJI
		if (term->flags & SJIS_KANJI && SJIS1(c)) {
			bcnt++;
			*--bptr = c;
			doSJIS();
			screen->curss = 0;
			continue;
		}
# endif /* KTERM_KANJI */
	        switch (ps) {
#else /* !KTERM_MBCS */
	        switch (parsestate[c = doinput()]) {
#endif /* !KTERM_MBCS */
		 case CASE_PRINT:
			/* printable characters */
#ifdef KTERM
			bcnt++;
			*--bptr = c;
			if(screen->curss) {
				doSS(screen->gsets[screen->curss]);
# ifdef KTERM_MBCS
				if (pending_byte == 0)
# endif /* KTERM_MBCS */
					screen->curss = 0;
				break; /* switch */
			}
			doLS(screen->gsets[c & 0x80 ? screen->curgr
						    : screen->curgl]);
#else /* KTERM */
			top = bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt;
			cp = bptr;
			*--bptr = c;
			while(top > 0 && isprint(*cp & 0x7f)) {
				top--;
				bcnt--;
				cp++;
			}
			if(screen->curss) {
				dotext(screen, term->flags,
				 screen->gsets[screen->curss], bptr, bptr + 1);
				screen->curss = 0;
				bptr++;
			}
			if(bptr < cp)
				dotext(screen, term->flags,
				 screen->gsets[screen->curgl], bptr, cp);
			bptr = cp;
#endif /* !KTERM */
			break;

		 case CASE_GROUND_STATE:
			/* exit ignore mode */
			parsestate = groundtable;
			break;

		 case CASE_IGNORE_STATE:
			/* Ies: ignore anything else */
			parsestate = igntable;
			break;

		 case CASE_IGNORE_ESC:
			/* Ign: escape */
			parsestate = iestable;
			break;

		 case CASE_IGNORE:
			/* Ignore character */
			break;

		 case CASE_BELL:
			/* bell */
			Bell();
			break;

		 case CASE_BS:
			/* backspace */
			CursorBack(screen, 1);
			break;

		 case CASE_CR:
			/* carriage return */
			CarriageReturn(screen);
			parsestate = groundtable;
			break;

		 case CASE_ESC:
			/* escape */
			parsestate = esctable;
			break;

		 case CASE_VMOT:
			/*
			 * form feed, line feed, vertical tab
			 */
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			Index(screen, 1);
			if (term->flags & LINEFEED)
				CarriageReturn(screen);
			if (XtAppPending(app_con) ||
			    GetBytesAvailable (ConnectionNumber(screen->display)) > 0)
			  xevents();
			parsestate = groundtable;
			break;

		 case CASE_TAB:
			/* tab */
			screen->cur_col = TabNext(term->tabs, screen->cur_col);
			if (screen->cur_col > screen->max_col)
				screen->cur_col = screen->max_col;
			break;

		 case CASE_SI:
			screen->curgl = 0;
			break;

		 case CASE_SO:
			screen->curgl = 1;
			break;

		 case CASE_SCR_STATE:
			/* enter scr state */
			parsestate = scrtable;
			break;

		 case CASE_SCS0_STATE:
			/* enter scs state 0 */
			scstype = 0;
			parsestate = scstable;
			break;

		 case CASE_SCS1_STATE:
			/* enter scs state 1 */
#ifdef KTERM
			if (c == '-') cs96 = 1;
#endif /* KTERM */
			scstype = 1;
			parsestate = scstable;
			break;

		 case CASE_SCS2_STATE:
			/* enter scs state 2 */
#ifdef KTERM
			if (c == '.') cs96 = 1;
#endif /* KTERM */
			scstype = 2;
			parsestate = scstable;
			break;

		 case CASE_SCS3_STATE:
			/* enter scs state 3 */
#ifdef KTERM
			if (c == '/') cs96 = 1;
#endif /* KTERM */
			scstype = 3;
			parsestate = scstable;
			break;

#ifdef KTERM_MBCS
		 case CASE_MBCS:
			/* enter mbcs state */
			mbcs = 1;
			scstype = 0;
			parsestate = mbcstable;
			break;
#endif /* KTERM_MBCS */

		 case CASE_ESC_IGNORE:
			/* unknown escape sequence */
			parsestate = eigtable;
			break;

		 case CASE_ESC_DIGIT:
			/* digit in csi or dec mode */
			if((row = param[nparam - 1]) == DEFAULT)
				row = 0;
			param[nparam - 1] = 10 * row + (c - '0');
			break;

		 case CASE_ESC_SEMI:
			/* semicolon in csi or dec mode */
			param[nparam++] = DEFAULT;
			break;

		 case CASE_DEC_STATE:
			/* enter dec mode */
			parsestate = dectable;
			break;

		 case CASE_ICH:
			/* ICH */
			if((row = param[0]) < 1)
				row = 1;
			InsertChar(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_CUU:
			/* CUU */
			if((row = param[0]) < 1)
				row = 1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			CursorUp(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_CUD:
			/* CUD */
			if((row = param[0]) < 1)
				row = 1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			CursorDown(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_CUF:
			/* CUF */
			if((row = param[0]) < 1)
				row = 1;
			CursorForward(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_CUB:
			/* CUB */
			if((row = param[0]) < 1)
				row = 1;
			CursorBack(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_CUP:
			/* CUP | HVP */
			if((row = param[0]) < 1)
				row = 1;
			if(nparam < 2 || (col = param[1]) < 1)
				col = 1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			CursorSet(screen, row-1, col-1, term->flags);
			parsestate = groundtable;
			break;

		 case CASE_ED:
			/* ED */
			switch (param[0]) {
			 case DEFAULT:
			 case 0:
#ifdef STATUSLINE
			      if (screen->instatus)
				ClearRight(screen);
			      else
#endif /* STATUSLINE */
				ClearBelow(screen);
				break;

			 case 1:
#ifdef STATUSLINE
			      if (screen->instatus)
				ClearLeft(screen);
			      else
#endif /* STATUSLINE */
				ClearAbove(screen);
				break;

			 case 2:
#ifdef STATUSLINE
			      if (screen->instatus)
				ClearLine(screen);
			      else
#endif /* STATUSLINE */
				ClearScreen(screen);
				break;
			}
			parsestate = groundtable;
			break;

		 case CASE_EL:
			/* EL */
			switch (param[0]) {
			 case DEFAULT:
			 case 0:
				ClearRight(screen);
				break;
			 case 1:
				ClearLeft(screen);
				break;
			 case 2:
				ClearLine(screen);
				break;
			}
			parsestate = groundtable;
			break;

		 case CASE_IL:
			/* IL */
			if((row = param[0]) < 1)
				row = 1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			InsertLine(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_DL:
			/* DL */
			if((row = param[0]) < 1)
				row = 1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			DeleteLine(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_DCH:
			/* DCH */
			if((row = param[0]) < 1)
				row = 1;
			DeleteChar(screen, row);
			parsestate = groundtable;
			break;

		 case CASE_TRACK_MOUSE:
		 	/* Track mouse as long as in window and between
			   specified rows */
			TrackMouse(param[0], param[2]-1, param[1]-1,
			 param[3]-1, param[4]-2);
			break;

		 case CASE_DECID:
			param[0] = -1;		/* Default ID parameter */
			/* Fall through into ... */
		 case CASE_DA1:
			/* DA1 */
			if (param[0] <= 0) {	/* less than means DEFAULT */
				reply.a_type   = CSI;
				reply.a_pintro = '?';
				reply.a_nparam = 2;
				reply.a_param[0] = 1;		/* VT102 */
				reply.a_param[1] = 2;		/* VT102 */
				reply.a_inters = 0;
				reply.a_final  = 'c';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_TBC:
			/* TBC */
			if ((row = param[0]) <= 0) /* less than means default */
				TabClear(term->tabs, screen->cur_col);
			else if (row == 3)
				TabZonk(term->tabs);
			parsestate = groundtable;
			break;

		 case CASE_SET:
			/* SET */
			ansi_modes(term, bitset);
			parsestate = groundtable;
			break;

		 case CASE_RST:
			/* RST */
			ansi_modes(term, bitclr);
			parsestate = groundtable;
			break;

		 case CASE_SGR:
			/* SGR */
			for (row=0; row<nparam; ++row) {
				switch (param[row]) {
				 case DEFAULT:
				 case 0:
#ifdef KTERM_COLOR
					term->flags &= ~(INVERSE|BOLD|UNDERLINE|FORECOLORED|BACKCOLORED);
#else /* !KTERM_COLOR */
					term->flags &= ~(INVERSE|BOLD|UNDERLINE);
#endif /* !KTERM_COLOR */
					break;
				 case 1:
				 case 5:	/* Blink, really.	*/
					term->flags |= BOLD;
					break;
				 case 4:	/* Underscore		*/
					term->flags |= UNDERLINE;
					break;
				 case 7:
					term->flags |= INVERSE;
					break;
#ifdef KTERM_COLOR
				 case 30:
				 case 31:
				 case 32:
				 case 33:
				 case 34:
				 case 35:
				 case 36:
				 case 37:
					term->flags &= ~FORECOLORMASK;
					term->flags |= FORECOLORED|FORECOLOR(param[row]-30);
					break;
				 case 39:
					term->flags &= ~FORECOLORED;
					break;
				 case 40:
				 case 41:
				 case 42:
				 case 43:
				 case 44:
				 case 45:
				 case 46:
				 case 47:
					term->flags &= ~BACKCOLORMASK;
					term->flags |= BACKCOLORED|BACKCOLOR(param[row]-40);
					break;
				 case 49:
					term->flags &= ~BACKCOLORED;
					break;
#endif /* KTERM_COLOR */
				}
			}
			parsestate = groundtable;
			break;

		 case CASE_CPR:
			/* CPR */
			if ((row = param[0]) == 5) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 1;
				reply.a_param[0] = 0;
				reply.a_inters = 0;
				reply.a_final  = 'n';
				unparseseq(&reply, screen->respond);
			} else if (row == 6) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 2;
				reply.a_param[0] = screen->cur_row+1;
				reply.a_param[1] = screen->cur_col+1;
				reply.a_inters = 0;
				reply.a_final  = 'R';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_DECSTBM:
			/* DECSTBM - set scrolling region */
			if((top = param[0]) < 1)
				top = 1;
			if(nparam < 2 || (bot = param[1]) == DEFAULT
			   || bot > screen->max_row + 1
			   || bot == 0)
				bot = screen->max_row+1;
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			if (bot > top) {
				if(screen->scroll_amt)
					FlushScroll(screen);
				screen->top_marg = top-1;
				screen->bot_marg = bot-1;
				CursorSet(screen, 0, 0, term->flags);
			}
			parsestate = groundtable;
			break;

		 case CASE_DECREQTPARM:
			/* DECREQTPARM */
			if ((row = param[0]) == DEFAULT)
				row = 0;
			if (row == 0 || row == 1) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 7;
				reply.a_param[0] = row + 2;
				reply.a_param[1] = 1;	/* no parity */
				reply.a_param[2] = 1;	/* eight bits */
				reply.a_param[3] = 112;	/* transmit 9600 baud */
				reply.a_param[4] = 112;	/* receive 9600 baud */
				reply.a_param[5] = 1;	/* clock multiplier ? */
				reply.a_param[6] = 0;	/* STP flags ? */
				reply.a_inters = 0;
				reply.a_final  = 'x';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_DECSET:
			/* DECSET */
			dpmodes(term, bitset);
			parsestate = groundtable;
			if(screen->TekEmu)
				return;
			break;

		 case CASE_DECRST:
			/* DECRST */
			dpmodes(term, bitclr);
			parsestate = groundtable;
			break;

		 case CASE_DECALN:
			/* DECALN */
			if(screen->cursor_state)
				HideCursor();
			for(row = screen->max_row ; row >= 0 ; row--) {
#ifdef KTERM
				col = screen->max_col + 1;
				for(xp = screen->buf[row] ; col > 0 ; col--) {
					xp->code = (unsigned char) 'E';
					xp->gset = GSET_ASCII;
					xp->attr = 0;
					xp++;
				}
#else /* !KTERM */
				bzero(screen->buf[2 * row + 1],
				 col = screen->max_col + 1);
				for(cp = (unsigned char *)screen->buf[2 * row] ; col > 0 ; col--)
					*cp++ = (unsigned char) 'E';
#endif /* !KTERM */
			}
			ScrnRefresh(screen, 0, 0, screen->max_row + 1,
			 screen->max_col + 1, False);
			parsestate = groundtable;
			break;

		 case CASE_GSETS:
#ifdef KTERM
			c = GSET(c);
			if (cs96) {
				c |= CS96;
				cs96 = 0;
			}
# ifdef KTERM_MBCS
			if (mbcs) {
				c |= MBCS;
				mbcs = 0;
			}
# endif /* KTERM_MBCS */
#endif /* KTERM */
			screen->gsets[scstype] = c;
			parsestate = groundtable;
			break;

		 case CASE_DECSC:
			/* DECSC */
			CursorSave(term, &screen->sc);
			parsestate = groundtable;
			break;

		 case CASE_DECRC:
			/* DECRC */
			CursorRestore(term, &screen->sc);
			parsestate = groundtable;
			break;

		 case CASE_DECKPAM:
			/* DECKPAM */
			term->keyboard.flags |= KYPD_APL;
			update_appkeypad();
			parsestate = groundtable;
			break;

		 case CASE_DECKPNM:
			/* DECKPNM */
			term->keyboard.flags &= ~KYPD_APL;
			update_appkeypad();
			parsestate = groundtable;
			break;

		 case CASE_IND:
			/* IND */
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			Index(screen, 1);
			if (XtAppPending(app_con) ||
			    GetBytesAvailable (ConnectionNumber(screen->display)) > 0)
			  xevents();
			parsestate = groundtable;
			break;

		 case CASE_NEL:
			/* NEL */
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			Index(screen, 1);
			CarriageReturn(screen);
			
			if (XtAppPending(app_con) ||
			    GetBytesAvailable (ConnectionNumber(screen->display)) > 0)
			  xevents();
			parsestate = groundtable;
			break;

		 case CASE_HTS:
			/* HTS */
			TabSet(term->tabs, screen->cur_col);
			parsestate = groundtable;
			break;

		 case CASE_RI:
			/* RI */
#ifdef STATUSLINE
		      if (!screen->instatus)
#endif /* STATUSLINE */
			RevIndex(screen, 1);
			parsestate = groundtable;
			break;

		 case CASE_SS2:
			/* SS2 */
			screen->curss = 2;
			parsestate = groundtable;
			break;

		 case CASE_SS3:
			/* SS3 */
			screen->curss = 3;
			parsestate = groundtable;
			break;

		 case CASE_CSI_STATE:
			/* enter csi state */
			nparam = 1;
			param[0] = DEFAULT;
			parsestate = csitable;
			break;

		 case CASE_OSC:
			/* Operating System Command: ESC ] */
			do_osc(finput);
			parsestate = groundtable;
			break;

		 case CASE_RIS:
			/* RIS */
			VTReset(TRUE);
			parsestate = groundtable;
			break;

		 case CASE_LS2:
			/* LS2 */
			screen->curgl = 2;
			parsestate = groundtable;
			break;

		 case CASE_LS3:
			/* LS3 */
			screen->curgl = 3;
			parsestate = groundtable;
			break;

		 case CASE_LS3R:
			/* LS3R */
			screen->curgr = 3;
			parsestate = groundtable;
			break;

		 case CASE_LS2R:
			/* LS2R */
			screen->curgr = 2;
			parsestate = groundtable;
			break;

		 case CASE_LS1R:
			/* LS1R */
			screen->curgr = 1;
			parsestate = groundtable;
			break;

#ifdef STATUSLINE
		 case CASE_TO_STATUS:
			if ((c = param[0]) < 1)
				c = 1;
			ToStatus(c - 1);
			parsestate = groundtable;
			break;

		 case CASE_FROM_STATUS:
			FromStatus();
			parsestate = groundtable;
			break;

		 case CASE_SHOW_STATUS:
			ShowStatus();
			parsestate = groundtable;
			break;

		 case CASE_HIDE_STATUS:
			HideStatus();
			parsestate = groundtable;
			break;

		 case CASE_ERASE_STATUS:
			EraseStatus();
			parsestate = groundtable;
			break;
#endif /* STATUSLINE */

		 case CASE_XTERM_SAVE:
			savemodes(term);
			parsestate = groundtable;
			break;

		 case CASE_XTERM_RESTORE:
			restoremodes(term);
			parsestate = groundtable;
			break;
#ifdef KTERM_MBCS
		}
#else /* !KTERM_MBCS */
		}
#endif /* !KTERM_MBCS */
	}
}

static finput()
{
	return(doinput());
}


static char *v_buffer;		/* pointer to physical buffer */
static char *v_bufstr = NULL;	/* beginning of area to write */
static char *v_bufptr;		/* end of area to write */
static char *v_bufend;		/* end of physical buffer */
#define	ptymask()	(v_bufptr > v_bufstr ? pty_mask : 0)

/* Write data to the pty as typed by the user, pasted with the mouse,
   or generated by us in response to a query ESC sequence. */

v_write(f, d, len)
    int f;
    char *d;
    int len;
{
	int riten;
	int c = len;

	if (v_bufstr == NULL  &&  len > 0) {
	        v_buffer = XtMalloc(len);
		v_bufstr = v_buffer;
		v_bufptr = v_buffer;
		v_bufend = v_buffer + len;
	}
#ifdef DEBUG
	fprintf(stderr, "v_write called with %d bytes (%d left over)",
		len, v_bufptr - v_bufstr);
	if (len > 1  &&  len < 10) fprintf(stderr, " \"%.*s\"", len, d);
	fprintf(stderr, "\n");
#endif

	if ((1 << f) != pty_mask)
		return(write(f, d, len));

	/*
	 * Append to the block we already have.
	 * Always doing this simplifies the code, and
	 * isn't too bad, either.  If this is a short
	 * block, it isn't too expensive, and if this is
	 * a long block, we won't be able to write it all
	 * anyway.
	 */

	if (len > 0) {
	    if (v_bufend < v_bufptr + len) { /* we've run out of room */
		if (v_bufstr != v_buffer) {
		    /* there is unused space, move everything down */
		    /* possibly overlapping bcopy here */
#ifdef DEBUG
		    fprintf(stderr, "moving data down %d\n",
			    v_bufstr - v_buffer);
#endif
		    bcopy(v_bufstr, v_buffer, v_bufptr - v_bufstr);
		    v_bufptr -= v_bufstr - v_buffer;
		    v_bufstr = v_buffer;
		}
		if (v_bufend < v_bufptr + len) {
		    /* still won't fit: get more space */
		    /* Don't use XtRealloc because an error is not fatal. */
		    int size = v_bufptr - v_buffer; /* save across realloc */
		    v_buffer = realloc(v_buffer, size + len);
		    if (v_buffer) {
#ifdef DEBUG
			fprintf(stderr, "expanded buffer to %d\n",
				size + len);
#endif
			v_bufstr = v_buffer;
			v_bufptr = v_buffer + size;
			v_bufend = v_bufptr + len;
		    } else {
			/* no memory: ignore entire write request */
			fprintf(stderr, "%s: cannot allocate buffer space\n",
				xterm_name);
			v_buffer = v_bufstr; /* restore clobbered pointer */
			c = 0;
		    }
		}
	    }
	    if (v_bufend >= v_bufptr + len) {
		/* new stuff will fit */
		bcopy(d, v_bufptr, len);
		v_bufptr += len;
	    }
	}

	/*
	 * Write out as much of the buffer as we can.
	 * Be careful not to overflow the pty's input silo.
	 * We are conservative here and only write
	 * a small amount at a time.
	 *
	 * If we can't push all the data into the pty yet, we expect write
	 * to return a non-negative number less than the length requested
	 * (if some data written) or -1 and set errno to EAGAIN,
	 * EWOULDBLOCK, or EINTR (if no data written).
	 *
	 * (Not all systems do this, sigh, so the code is actually
	 * a little more forgiving.)
	 */

#define MAX_PTY_WRITE 128	/* 1/2 POSIX minimum MAX_INPUT */

	if (v_bufptr > v_bufstr) {
	    riten = write(f, v_bufstr, v_bufptr - v_bufstr <= MAX_PTY_WRITE ?
			  	       v_bufptr - v_bufstr : MAX_PTY_WRITE);
	    if (riten < 0) {
#ifdef DEBUG
		perror("write");
#endif
		riten = 0;
	    }
#ifdef DEBUG
	    fprintf(stderr, "write called with %d, wrote %d\n",
		    v_bufptr - v_bufstr <= MAX_PTY_WRITE ?
		    v_bufptr - v_bufstr : MAX_PTY_WRITE,
		    riten);
#endif
	    v_bufstr += riten;
	    if (v_bufstr >= v_bufptr) /* we wrote it all */
		v_bufstr = v_bufptr = v_buffer;
	}

	/*
	 * If we have lots of unused memory allocated, return it
	 */
	if (v_bufend - v_bufptr > 1024) { /* arbitrary hysteresis */
	    /* save pointers across realloc */
	    int start = v_bufstr - v_buffer;
	    int size = v_bufptr - v_buffer;
	    int allocsize = size ? size : 1;
	    
	    v_buffer = realloc(v_buffer, allocsize);
	    if (v_buffer) {
		v_bufstr = v_buffer + start;
		v_bufptr = v_buffer + size;
		v_bufend = v_buffer + allocsize;
#ifdef DEBUG
		fprintf(stderr, "shrunk buffer to %d\n", allocsize);
#endif
	    } else {
		/* should we print a warning if couldn't return memory? */
		v_buffer = v_bufstr - start; /* restore clobbered pointer */
	    }
	}
	return(c);
}

static int select_mask;
static int write_mask;
static int pty_read_bytes;

in_put()
{
    register TScreen *screen = &term->screen;
    register int i;
    static struct timeval select_timeout;

    for( ; ; ) {
	if (select_mask & pty_mask && eventMode == NORMAL) {
#ifdef ALLOWLOGGING
	    if (screen->logging)
		FlushLog(screen);
#endif
	    bcnt = read(screen->respond, (char *)(bptr = buffer), BUF_SIZE);
	    if (bcnt < 0) {
		if (errno == EIO)
		    Cleanup (0);
		else if (!E_TEST(errno))
		    Panic(
			  "input: read returned unexpected error (%d)\n",
			  errno);
	    } else if (bcnt == 0)
		Panic("input: read returned zero\n", 0);
	    else {
		/* read from pty was successful */
#ifdef KTERM_MBCS
		if (pending_byte) {
		    /*
		     * restore pending_byte to the top of
		     * the text which just read.
		     */
		    *--bptr = pending_byte;
		    bcnt++;
		    pending_byte = 0;
		}
#endif /* KTERM_MBCS */
		if (!screen->output_eight_bits) {
		    register int bc = bcnt;
		    register Char *b = bptr;

		    for (; bc > 0; bc--, b++) {
			*b &= (Char) 0x7f;
		    }
		}
		if ( screen->scrollWidget && screen->scrollttyoutput &&
		     screen->topline < 0)
		    WindowScroll(screen, 0);  /* Scroll to bottom */
		pty_read_bytes += bcnt;
		/* stop speed reading at some point to look for X stuff */
		/* (4096 is just a random large number.) */
		if (pty_read_bytes > 4096) {
		    select_mask &= ~pty_mask;
		}
		break;
	    }
	}
	pty_read_bytes = 0;
	/* update the screen */
	if (screen->scroll_amt)
	    FlushScroll(screen);
	if (screen->cursor_set && (screen->cursor_col != screen->cur_col
				   || screen->cursor_row != screen->cur_row)) {
	    if (screen->cursor_state)
		HideCursor();
	    ShowCursor();
	} else if (screen->cursor_set != screen->cursor_state) {
	    if (screen->cursor_set)
		ShowCursor();
	    else
		HideCursor();
	}

	XFlush(screen->display); /* always flush writes before waiting */

	/* 
	 * Update the masks and, unless X events are already in the 
	 * queue, wait for I/O to be possible. 
	 */
	select_mask = Select_mask;
	write_mask = ptymask();
	select_timeout.tv_sec = 0;
	/*
	 * if there's either an XEvent or an XtTimeout pending, just take
	 * a quick peek, i.e. timeout from the select() immediately.  If
	 * there's nothing pending, let select() block a little while, but
	 * for a shorter interval than the arrow-style scrollbar timeout.
	 */
	if (XtAppPending(app_con))
		select_timeout.tv_usec = 0;
	else
		select_timeout.tv_usec = 50000;
	i = select(max_plus1, 
		   &select_mask, &write_mask, NULL,
		   &select_timeout);
	if (i < 0) {
	    if (errno != EINTR)
		SysError(ERROR_SELECT);
	    continue;
	} 

	/* if there is room to write more data to the pty, go write more */
	if (write_mask & ptymask()) {
	    v_write(screen->respond, 0, 0); /* flush buffer */
	}

	/* if there are X events already in our queue, it
	   counts as being readable */
	if (XtAppPending(app_con) || (select_mask & X_mask)) {
	    xevents();
	}

    }
    bcnt--;
    return(*bptr++);
}

/*
 * process a string of characters according to the character set indicated
 * by charset.  worry about end of line conditions (wraparound if selected).
 */
static void
dotext(screen, flags, charset, buf, ptr)
    register TScreen	*screen;
    unsigned	flags;
#ifdef KTERM
    Char	charset;
    Char	*buf;		/* start of characters to process */
    Char	*ptr;		/* end */
#else /* !KTERM */
    char	charset;
    char	*buf;		/* start of characters to process */
    char	*ptr;		/* end */
#endif /* !KTERM */
{
#ifdef KTERM
	int	do_wrap = 0;
	register Char	*s;
#else /* !KTERM */
	register char	*s;
#endif /* !KTERM */
	register int	len;
	register int	n;
	register int	next_col;

#ifdef KTERM
	switch (charset) {
	case GSET_UK:		/* United Kingdom set		*/
		for (s=buf; s<ptr; ++s) {
			*s &= ~NEEDMAP;
			if (*s == '#') /* UK pound sign */
				*s |= NEEDMAP;
		}
		break;

	case GSET_LATIN1R:	/* Right part of ISO8859-1	*/
	case GSET_KANA:		/* JIS Kana set			*/
		for (s=buf; s<ptr; ++s) {
			*s |= NEEDMAP;
		}
		break;

	case GSET_GRAPH:	/* special graphics (line drawing)	*/
	case GSET_ASCII:	/* ASCII set			*/
	case GSET_JISROMAN:	/* JIS Roman set		*/
	case GSET_KANJI:	/* JIS Kanji set		*/
	case GSET_OLDKANJI:	/* JIS Kanji set		*/
		for (s=buf; s<ptr; ++s) {
			*s &= ~NEEDMAP;
		}
		break;

	default:	/* any character sets we don't recognize*/
		return;
	}
#else /* !KTERM */
	switch (charset) {
	case 'A':	/* United Kingdom set			*/
		for (s=buf; s<ptr; ++s)
			if (*s == '#')
				*s = '\036';	/* UK pound sign*/
		break;

	case 'B':	/* ASCII set				*/
		break;

	case '0':	/* special graphics (line drawing)	*/
		for (s=buf; s<ptr; ++s)
			if (*s>=0x5f && *s<=0x7e)
				*s = *s == 0x5f ? 0x7f : *s - 0x5f;
		break;

	default:	/* any character sets we don't recognize*/
		return;
	}
#endif /* !KTERM */

	len = ptr - buf; 
	ptr = buf;
	while (len > 0) {
		n = screen->max_col - screen->cur_col +1;
		if (n <= 1) {
#ifdef STATUSLINE
			if (screen->do_wrap && (flags&WRAPAROUND) &&
			    !screen->instatus) {
#else /* !STATUSLINE */
			if (screen->do_wrap && (flags&WRAPAROUND)) {
#endif /* !STATUSLINE */
			    /* mark that we had to wrap this line */
			    ScrnSetAttributes(screen, screen->cur_row, 0,
					      LINEWRAPPED, LINEWRAPPED, 1);
			    Index(screen, 1);
			    screen->cur_col = 0;
			    screen->do_wrap = 0;
			    n = screen->max_col+1;
			} else
			    n = 1;
		}
		if (len < n)
			n = len;
#ifdef KTERM_MBCS
		if (charset & MBCS) {
			if (n == 1) {
				if (flags & WRAPAROUND) {
					n--; do_wrap = 1;
				} else
					n++;
			} else
				if (n & 1)
					n--;
		}
#endif /* KTERM_MBCS */
		next_col = screen->cur_col + n;
#ifdef KTERM
		WriteText(screen, ptr, n, flags, charset);
#else /* !KTERM */
		WriteText(screen, ptr, n, flags);
#endif /* !KTERM */
		/*
		 * the call to WriteText updates screen->cur_col.
		 * If screen->cur_col != next_col, we must have
		 * hit the right margin, so set the do_wrap flag.
		 */
		screen->do_wrap = (screen->cur_col < next_col);
#ifdef KTERM_MBCS
		screen->do_wrap |= do_wrap;
#endif /* KTERM_MBCS */
		len -= n;
		ptr += n;
	}
}
 
/*
 * write a string str of length len onto the screen at
 * the current cursor position.  update cursor position.
 */
static void
#ifdef KTERM
WriteText(screen, str, len, flags, gset)
#else /* !KTERM */
WriteText(screen, str, len, flags)
#endif /* !KTERM */
    register TScreen	*screen;
    register char	*str;
    register int	len;
    unsigned		flags;
#ifdef KTERM
    register Char	gset;
#endif /* KTERM */
{
	register int cx, cy;
	register unsigned fgs = flags;
#ifdef KTERM
	register int	i, n;
	XChar2b drawbuf[256], *dbuf;

	dbuf = (len > 256) ? (XChar2b *)XtMalloc(len * sizeof(XChar2b))
			   : drawbuf;
#else /* !KTERM */
	GC	currentGC;
#endif /* !KTERM */
 
#ifdef STATUSLINE
   if(screen->instatus && screen->reversestatus)
	fgs ^= INVERSE;
   if(screen->cur_row - screen->topline <= screen->max_row ||
      screen->instatus) {
#else /* !STATUSLINE */
   if(screen->cur_row - screen->topline <= screen->max_row) {
#endif /* !STATUSLINE */
	/*
	if(screen->cur_row == screen->cursor_row && screen->cur_col <=
	 screen->cursor_col && screen->cursor_col <= screen->cur_col + len - 1)
		screen->cursor_state = OFF;
	 */
	if(screen->cursor_state)
		HideCursor();

#ifndef KTERM
	/*
	 *	make sure that the correct GC is current
	 */

	if (fgs & BOLD)
		if (fgs & INVERSE)
			currentGC = screen->reverseboldGC;
		else	currentGC = screen->normalboldGC;
	else  /* not bold */
		if (fgs & INVERSE)
			currentGC = screen->reverseGC;
		else	currentGC = screen->normalGC;
#endif /* !KTERM */

	if (fgs & INSERT)
		InsertChar(screen, len);
      if (!(AddToRefresh(screen))) {
		if(screen->scroll_amt)
			FlushScroll(screen);
#ifdef KTERM
# ifdef KTERM_MBCS
	if (gset & MBCS) {
		for (i = n = 0; i < len; n++) {
			dbuf[n].byte1 = str[i++];
			dbuf[n].byte2 = str[i++];
		}
	} else
# endif /* KTERM_MBCS */
	for (n = 0; n < len; n++) {
		dbuf[n].byte1 = 0;
		dbuf[n].byte2 = MapOnFont(gset, str[n]);
	}
#endif /* KTERM */
	cx = CursorX(screen, screen->cur_col);
#ifdef KTERM
	cy = CursorY(screen, screen->cur_row);
	BreakMBchar(screen);
	screen->cur_col += len;
	BreakMBchar(screen);
	screen->cur_col -= len;
	ScreenDraw(screen, cx, cy, dbuf, n, gset, fgs, False);
#else /* !KTERM */
	cy = CursorY(screen, screen->cur_row)+screen->fnt_norm->ascent;
 	XDrawImageString(screen->display, TextWindow(screen), currentGC,
			cx, cy, str, len);

	if((fgs & BOLD) && screen->enbolden) 
		if (currentGC == screen->normalGC || screen->reverseGC)
			XDrawString(screen->display, TextWindow(screen),
			      	currentGC,cx + 1, cy, str, len);

	if(fgs & UNDERLINE) 
		XDrawLine(screen->display, TextWindow(screen), currentGC,
			cx, cy+1,
			cx + len * FontWidth(screen), cy+1);
#endif /* !KTERM */
	/*
	 * the following statements compile data to compute the average 
	 * number of characters written on each call to XText.  The data
	 * may be examined via the use of a "hidden" escape sequence.
	 */
	ctotal += len;
	++ntotal;
      }
    }
#ifdef KTERM
	ScreenWrite(screen, str, flags, gset, len);
#else /* !KTERM */
	ScreenWrite(screen, str, flags, len);
#endif /* !KTERM */
	CursorForward(screen, len);
}
 
/*
 * process ANSI modes set, reset
 */
ansi_modes(termw, func)
    XtermWidget	termw;
    int		(*func)();
{
	register int	i;

	for (i=0; i<nparam; ++i) {
		switch (param[i]) {
		case 4:			/* IRM				*/
			(*func)(&termw->flags, INSERT);
			break;

		case 20:		/* LNM				*/
			(*func)(&termw->flags, LINEFEED);
			update_autolinefeed();
			break;
		}
	}
}

/*
 * process DEC private modes set, reset
 */
dpmodes(termw, func)
    XtermWidget	termw;
    void (*func)();
{
	register TScreen	*screen	= &termw->screen;
	register int	i, j;

	for (i=0; i<nparam; ++i) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			(*func)(&termw->keyboard.flags, CURSOR_APL);
			update_appcursor();
			break;
		case 2:			/* ANSI/VT52 mode		*/
			if (func == bitset) {
				screen->gsets[0] =
					screen->gsets[1] =
					screen->gsets[2] =
					screen->gsets[3] = 'B';
				screen->curgl = 0;
				screen->curgr = 2;
			}
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132) {
				ClearScreen(screen);
				CursorSet(screen, 0, 0, termw->flags);
				if((j = func == bitset ? 132 : 80) !=
				 ((termw->flags & IN132COLUMNS) ? 132 : 80) ||
				 j != screen->max_col + 1) {
				        Dimension replyWidth, replyHeight;
					XtGeometryResult status;

					status = XtMakeResizeRequest (
					    (Widget) termw, 
					    (Dimension) FontWidth(screen) * j
					        + 2*screen->border
						+ screen->scrollbar,
					    (Dimension) FontHeight(screen)
						* (screen->max_row + 1)
						+ 2 * screen->border,
					    &replyWidth, &replyHeight);

					if (status == XtGeometryYes ||
					    status == XtGeometryDone) {
					    ScreenResize (&termw->screen,
							  replyWidth,
							  replyHeight,
							  &termw->flags);
					}
				}
				(*func)(&termw->flags, IN132COLUMNS);
			}
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			if (func == bitset) {
				screen->jumpscroll = 0;
				if (screen->scroll_amt)
					FlushScroll(screen);
			} else
				screen->jumpscroll = 1;
			(*func)(&termw->flags, SMOOTHSCROLL);
			update_jumpscroll();
			break;
		case 5:			/* DECSCNM			*/
			j = termw->flags;
			(*func)(&termw->flags, REVERSE_VIDEO);
			if ((termw->flags ^ j) & REVERSE_VIDEO)
				ReverseVideo(termw);
			/* update_reversevideo done in RevVid */
			break;

		case 6:			/* DECOM			*/
			(*func)(&termw->flags, ORIGIN);
			CursorSet(screen, 0, 0, termw->flags);
			break;

		case 7:			/* DECAWM			*/
			(*func)(&termw->flags, WRAPAROUND);
			update_autowrap();
			break;
		case 8:			/* DECARM			*/
			/* ignore autorepeat */
			break;
		case 9:			/* MIT bogus sequence		*/
			if(func == bitset)
				screen->send_mouse_pos = 1;
			else
				screen->send_mouse_pos = 0;
			break;
		case 38:		/* DECTEK			*/
			if(func == bitset && !(screen->inhibit & I_TEK)) {
#ifdef ALLOWLOGGING
				if(screen->logging) {
					FlushLog(screen);
					screen->logstart = Tbuffer;
				}
#endif
				screen->TekEmu = TRUE;
			}
			break;
		case 40:		/* 132 column mode		*/
			screen->c132 = (func == bitset);
			update_allow132();
			break;
		case 41:		/* curses hack			*/
			screen->curses = (func == bitset);
			update_cursesemul();
			break;
		case 44:		/* margin bell			*/
			screen->marginbell = (func == bitset);
			if(!screen->marginbell)
				screen->bellarmed = -1;
			update_marginbell();
			break;
		case 45:		/* reverse wraparound	*/
			(*func)(&termw->flags, REVERSEWRAP);
			update_reversewrap();
			break;
#ifdef ALLOWLOGGING
		case 46:		/* logging		*/
#ifdef ALLOWLOGFILEONOFF
			/*
			 * if this feature is enabled, logging may be 
			 * enabled and disabled via escape sequences.
			 */
			if(func == bitset)
				StartLog(screen);
			else
				CloseLog(screen);
#else
			Bell();
			Bell();
#endif /* ALLOWLOGFILEONOFF */
			break;
#endif
		case 47:		/* alternate buffer */
			if (!termw->misc.titeInhibit) {
			    if(func == bitset)
				ToAlternate(screen);
			    else
				FromAlternate(screen);
			}
			break;
#ifdef STATUSLINE
		case 48:		/* reverse statusline		*/
			j = screen->reversestatus;
			(*func)(&screen->reversestatus, 1);
			if (j != screen->reversestatus)
				ScrnRefresh(screen, screen->max_row + 1, 0, 1,
					screen->max_col + 1, False);
			break;
#endif /* STATUSLINE */
		case 1000:		/* xterm bogus sequence		*/
			if(func == bitset)
				screen->send_mouse_pos = 2;
			else
				screen->send_mouse_pos = 0;
			break;
		case 1001:		/* xterm sequence w/hilite tracking */
			if(func == bitset)
				screen->send_mouse_pos = 3;
			else
				screen->send_mouse_pos = 0;
			break;
		}
	}
}

/*
 * process xterm private modes save
 */
savemodes(termw)
    XtermWidget termw;
{
	register TScreen	*screen	= &termw->screen;
	register int i;

	for (i = 0; i < nparam; i++) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			screen->save_modes[0] = termw->keyboard.flags &
			 CURSOR_APL;
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132)
			    screen->save_modes[1] = termw->flags & IN132COLUMNS;
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			screen->save_modes[2] = termw->flags & SMOOTHSCROLL;
			break;
		case 5:			/* DECSCNM			*/
			screen->save_modes[3] = termw->flags & REVERSE_VIDEO;
			break;
		case 6:			/* DECOM			*/
			screen->save_modes[4] = termw->flags & ORIGIN;
			break;

		case 7:			/* DECAWM			*/
			screen->save_modes[5] = termw->flags & WRAPAROUND;
			break;
		case 8:			/* DECARM			*/
			/* ignore autorepeat */
			break;
		case 9:			/* mouse bogus sequence */
			screen->save_modes[7] = screen->send_mouse_pos;
			break;
		case 40:		/* 132 column mode		*/
			screen->save_modes[8] = screen->c132;
			break;
		case 41:		/* curses hack			*/
			screen->save_modes[9] = screen->curses;
			break;
		case 44:		/* margin bell			*/
			screen->save_modes[12] = screen->marginbell;
			break;
		case 45:		/* reverse wraparound	*/
			screen->save_modes[13] = termw->flags & REVERSEWRAP;
			break;
#ifdef ALLOWLOGGING
		case 46:		/* logging		*/
			screen->save_modes[14] = screen->logging;
			break;
#endif
		case 47:		/* alternate buffer		*/
			screen->save_modes[15] = screen->alternate;
			break;
#ifdef STATUSLINE
		case 48:		/* reverse statusline		*/
			screen->save_modes[16] = screen->reversestatus;
			break;
#endif /* STATUSLINE */
		case 1000:		/* mouse bogus sequence		*/
		case 1001:
			screen->save_modes[7] = screen->send_mouse_pos;
			break;
		}
	}
}

/*
 * process xterm private modes restore
 */
restoremodes(termw)
    XtermWidget termw;
{
	register TScreen	*screen	= &termw->screen;
	register int i, j;

	for (i = 0; i < nparam; i++) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			termw->keyboard.flags &= ~CURSOR_APL;
			termw->keyboard.flags |= screen->save_modes[0] &
			 CURSOR_APL;
			update_appcursor();
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132) {
				ClearScreen(screen);
				CursorSet(screen, 0, 0, termw->flags);
				if((j = (screen->save_modes[1] & IN132COLUMNS)
				 ? 132 : 80) != ((termw->flags & IN132COLUMNS)
				 ? 132 : 80) || j != screen->max_col + 1) {
				        Dimension replyWidth, replyHeight;
					XtGeometryResult status;
					status = XtMakeResizeRequest (
					    (Widget) termw,
					    (Dimension) FontWidth(screen) * j 
						+ 2*screen->border
						+ screen->scrollbar,
					    (Dimension) FontHeight(screen)
						* (screen->max_row + 1)
						+ 2*screen->border,
					    &replyWidth, &replyHeight);

					if (status == XtGeometryYes ||
					    status == XtGeometryDone) {
					    ScreenResize (&termw->screen,
							  replyWidth,
							  replyHeight,
							  &termw->flags);
					}
				}
				termw->flags &= ~IN132COLUMNS;
				termw->flags |= screen->save_modes[1] &
				 IN132COLUMNS;
			}
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			if (screen->save_modes[2] & SMOOTHSCROLL) {
				screen->jumpscroll = 0;
				if (screen->scroll_amt)
					FlushScroll(screen);
			} else
				screen->jumpscroll = 1;
			termw->flags &= ~SMOOTHSCROLL;
			termw->flags |= screen->save_modes[2] & SMOOTHSCROLL;
			update_jumpscroll();
			break;
		case 5:			/* DECSCNM			*/
			if((screen->save_modes[3] ^ termw->flags) & REVERSE_VIDEO) {
				termw->flags &= ~REVERSE_VIDEO;
				termw->flags |= screen->save_modes[3] & REVERSE_VIDEO;
				ReverseVideo(termw);
				/* update_reversevideo done in RevVid */
			}
			break;
		case 6:			/* DECOM			*/
			termw->flags &= ~ORIGIN;
			termw->flags |= screen->save_modes[4] & ORIGIN;
			CursorSet(screen, 0, 0, termw->flags);
			break;

		case 7:			/* DECAWM			*/
			termw->flags &= ~WRAPAROUND;
			termw->flags |= screen->save_modes[5] & WRAPAROUND;
			update_autowrap();
			break;
		case 8:			/* DECARM			*/
			/* ignore autorepeat */
			break;
		case 9:			/* MIT bogus sequence		*/
			screen->send_mouse_pos = screen->save_modes[7];
			break;
		case 40:		/* 132 column mode		*/
			screen->c132 = screen->save_modes[8];
			update_allow132();
			break;
		case 41:		/* curses hack			*/
			screen->curses = screen->save_modes[9];
			update_cursesemul();
			break;
		case 44:		/* margin bell			*/
			if(!(screen->marginbell = screen->save_modes[12]))
				screen->bellarmed = -1;
			update_marginbell();
			break;
		case 45:		/* reverse wraparound	*/
			termw->flags &= ~REVERSEWRAP;
			termw->flags |= screen->save_modes[13] & REVERSEWRAP;
			update_reversewrap();
			break;
#ifdef ALLOWLOGGING
		case 46:		/* logging		*/
#ifdef ALLOWLOGFILEONOFF
			if(screen->save_modes[14])
				StartLog(screen);
			else
				CloseLog(screen);
#endif /* ALLOWLOGFILEONOFF */
			/* update_logging done by StartLog and CloseLog */
			break;
#endif
		case 47:		/* alternate buffer */
			if (!termw->misc.titeInhibit) {
			    if(screen->save_modes[15])
				ToAlternate(screen);
			    else
				FromAlternate(screen);
			    /* update_altscreen done by ToAlt and FromAlt */
			}
			break;
#ifdef STATUSLINE
		case 48:		/* reverse statusline		*/
			if (screen->save_modes[16] != screen->reversestatus) {
				screen->reversestatus = screen->save_modes[16];
				ScrnRefresh(screen, screen->max_row + 1, 0, 1,
					    screen->max_col + 1, False);
			}
			break;
#endif /* STATUSLINE */
		case 1000:		/* mouse bogus sequence		*/
		case 1001:
			screen->send_mouse_pos = screen->save_modes[7];
			break;
		}
	}
}

/*
 * set a bit in a word given a pointer to the word and a mask.
 */
static void bitset(p, mask)
    unsigned *p;
    int mask;
{
	*p |= mask;
}

/*
 * clear a bit in a word given a pointer to the word and a mask.
 */
static void bitclr(p, mask)
    unsigned *p;
    int mask;
{
	*p &= ~mask;
}

unparseseq(ap, fd)
    register ANSI *ap;
    int fd;
{
	register int	c;
	register int	i;
	register int	inters;

	c = ap->a_type;
	if (c>=0x80 && c<=0x9F) {
		unparseputc(ESC, fd);
		c -= 0x40;
	}
	unparseputc(c, fd);
	c = ap->a_type;
	if (c==ESC || c==DCS || c==CSI || c==OSC || c==PM || c==APC) {
		if (ap->a_pintro != 0)
			unparseputc((char) ap->a_pintro, fd);
		for (i=0; i<ap->a_nparam; ++i) {
			if (i != 0)
				unparseputc(';', fd);
			unparseputn((unsigned int) ap->a_param[i], fd);
		}
		inters = ap->a_inters;
		for (i=3; i>=0; --i) {
			c = (inters >> (8*i)) & 0xff;
			if (c != 0)
				unparseputc(c, fd);
		}
		unparseputc((char) ap->a_final, fd);
	}
}

unparseputn(n, fd)
unsigned int	n;
int fd;
{
	unsigned int	q;

	q = n/10;
	if (q != 0)
		unparseputn(q, fd);
	unparseputc((char) ('0' + (n%10)), fd);
}

unparseputc(c, fd)
char c;
int fd;
{
	char	buf[2];
	register i = 1;
	extern XtermWidget term;

	if((buf[0] = c) == '\r' && (term->flags & LINEFEED)) {
		buf[1] = '\n';
		i++;
	}
	v_write(fd, buf, i);
}

unparsefputs (s, fd)
    register char *s;
    int fd;
{
    if (s) {
	while (*s) unparseputc (*s++, fd);
    }
}

static void SwitchBufs();

ToAlternate(screen)
register TScreen *screen;
{
	extern ScrnBuf Allocate();

	if(screen->alternate)
		return;
	if(!screen->altbuf)
		screen->altbuf = Allocate(screen->max_row + 1, screen->max_col
		 + 1, &screen->abuf_address);
	SwitchBufs(screen);
	screen->alternate = TRUE;
	update_altscreen();
}

FromAlternate(screen)
register TScreen *screen;
{
	if(!screen->alternate)
		return;
	screen->alternate = FALSE;
	SwitchBufs(screen);
	update_altscreen();
}

static void
SwitchBufs(screen)
    register TScreen *screen;
{
	register int rows, top;

	if(screen->cursor_state)
		HideCursor();
	rows = screen->max_row + 1;
	SwitchBufPtrs(screen);
	TrackText(0, 0, 0, 0);	/* remove any highlighting */
	if((top = -screen->topline) <= screen->max_row) {
		if(screen->scroll_amt)
			FlushScroll(screen);
#ifdef STATUSLINE
		if(top == 0 && !screen->statusline)
#else /* !STATUSLINE */
		if(top == 0)
#endif /* !STATUSLINE */
			XClearWindow(screen->display, TextWindow(screen));
		else
			XClearArea(
			    screen->display,
			    TextWindow(screen),
			    (int) screen->border + screen->scrollbar,
			    (int) top * FontHeight(screen) + screen->border,
			    (unsigned) Width(screen),
			    (unsigned) (screen->max_row - top + 1)
				* FontHeight(screen),
			    FALSE);
	}
	ScrnRefresh(screen, 0, 0, rows, screen->max_col + 1, False);
}

/* swap buffer line pointers between alt and regular screens */

SwitchBufPtrs(screen)
    register TScreen *screen;
{
    register int rows = screen->max_row + 1;
#ifdef KTERM
    Bchr *save [MAX_ROWS];
#else /* !KTERM */
    char *save [2 * MAX_ROWS];
#endif /* !KTERM */

#ifdef KTERM
    bcopy((char *)screen->buf, (char *)save, sizeof(Bchr *) * rows);
    bcopy((char *)screen->altbuf, (char *)screen->buf,
	  sizeof(Bchr *) * rows);
    bcopy((char *)save, (char *)screen->altbuf, sizeof(Bchr *) * rows);
#else /* !KTERM */
    bcopy((char *)screen->buf, (char *)save, 2 * sizeof(char *) * rows);
    bcopy((char *)screen->altbuf, (char *)screen->buf,
	  2 * sizeof(char *) * rows);
    bcopy((char *)save, (char *)screen->altbuf, 2 * sizeof(char *) * rows);
#endif /* !KTERM */
}

VTRun()
{
	register TScreen *screen = &term->screen;
	register int i;
	
	if (!screen->Vshow) {
	    set_vt_visibility (TRUE);
	} 
	update_vttekmode();
	update_vtshow();
	update_tekshow();
	set_vthide_sensitivity();

	if (screen->allbuf == NULL) VTallocbuf ();

	screen->cursor_state = OFF;
	screen->cursor_set = ON;

	bcnt = 0;
	bptr = buffer;
	while(Tpushb > Tpushback) {
		*bptr++ = *--Tpushb;
		bcnt++;
	}
	bcnt += (i = Tbcnt);
	for( ; i > 0 ; i--)
		*bptr++ = *Tbptr++;
	bptr = buffer;
	if(!setjmp(VTend))
		VTparse();
	HideCursor();
	screen->cursor_set = OFF;
}

/*ARGSUSED*/
static void VTExpose(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
	register TScreen *screen = &term->screen;

#ifdef DEBUG
	if(debug)
		fputs("Expose\n", stderr);
#endif	/* DEBUG */
	if (event->type == Expose)
		HandleExposure (screen, event);
}

static void VTGraphicsOrNoExpose (event)
    XEvent *event;
{
	register TScreen *screen = &term->screen;
	if (screen->incopy <= 0) {
		screen->incopy = 1;
		if (screen->scrolls > 0)
			screen->scrolls--;
	}
	if (event->type == GraphicsExpose)
	  if (HandleExposure (screen, event))
		screen->cursor_state = OFF;
	if ((event->type == NoExpose) || ((XGraphicsExposeEvent *)event)->count == 0) {
		if (screen->incopy <= 0 && screen->scrolls > 0)
			screen->scrolls--;
		if (screen->scrolls)
			screen->incopy = -1;
		else
			screen->incopy = 0;
	}
}

/*ARGSUSED*/
static void VTNonMaskableEvent (w, closure, event, cont)
Widget w;			/* unused */
XtPointer closure;		/* unused */
XEvent *event;
Boolean *cont;			/* unused */
{
    switch (event->type) {
       case GraphicsExpose:
       case NoExpose:
	  VTGraphicsOrNoExpose (event);
	  break;
      }
}




static void VTResize(w)
    Widget w;
{
    if (XtIsRealized(w))
      ScreenResize (&term->screen, term->core.width, term->core.height,
		    &term->flags);
}

				
extern Atom wm_delete_window;	/* for ICCCM delete window */

static String xterm_trans =
    "<ClientMessage>WM_PROTOCOLS: DeleteWindow()\n\
     <MappingNotify>: KeyboardMapping()\n";

int VTInit ()
{
    register TScreen *screen = &term->screen;
    Widget vtparent = term->core.parent;

    XtRealizeWidget (vtparent);
    XtOverrideTranslations(vtparent, XtParseTranslationTable(xterm_trans));
    (void) XSetWMProtocols (XtDisplay(vtparent), XtWindow(vtparent),
			    &wm_delete_window, 1);

    if (screen->allbuf == NULL) VTallocbuf ();
    return (1);
}

static void VTallocbuf ()
{
    register TScreen *screen = &term->screen;
    int nrows = screen->max_row + 1;
    extern ScrnBuf Allocate();

    /* allocate screen buffer now, if necessary. */
    if (screen->scrollWidget)
      nrows += screen->savelines;
    screen->allbuf = Allocate (nrows, screen->max_col + 1,
     &screen->sbuf_address);
    if (screen->scrollWidget)
#ifdef KTERM
      screen->buf = &screen->allbuf[screen->savelines];
#else /* !KTERM */
      screen->buf = &screen->allbuf[2 * screen->savelines];
#endif /* !KTERM */
    else
      screen->buf = screen->allbuf;
    return;
}

static void VTClassInit ()
{
    XtAddConverter(XtRString, XtRGravity, XmuCvtStringToGravity,
		   (XtConvertArgList) NULL, (Cardinal) 0);
}


/* ARGSUSED */
static void VTInitialize (wrequest, wnew, args, num_args)
   Widget wrequest, wnew;
   ArgList args;
   Cardinal *num_args;
{
   XtermWidget request = (XtermWidget) wrequest;
   XtermWidget new     = (XtermWidget) wnew;
   int i;
#ifdef KTERM
   int fnum;
#endif /* KTERM */

   /* Zero out the entire "screen" component of "new" widget,
      then do field-by-field assigment of "screen" fields
      that are named in the resource list. */

   bzero ((char *) &new->screen, sizeof(new->screen));
   new->screen.c132 = request->screen.c132;
   new->screen.curses = request->screen.curses;
   new->screen.foreground = request->screen.foreground;
   new->screen.cursorcolor = request->screen.cursorcolor;
#ifdef KTERM_COLOR
   bcopy(request->screen.textcolor, new->screen.textcolor,
	 sizeof new->screen.textcolor);
#endif /* KTERM_COLOR */
   new->screen.border = request->screen.border;
   new->screen.jumpscroll = request->screen.jumpscroll;
#ifdef ALLOWLOGGING
   new->screen.logfile = request->screen.logfile;
#endif
   new->screen.marginbell = request->screen.marginbell;
   new->screen.mousecolor = request->screen.mousecolor;
   new->screen.mousecolorback = request->screen.mousecolorback;
   new->screen.multiscroll = request->screen.multiscroll;
   new->screen.nmarginbell = request->screen.nmarginbell;
   new->screen.savelines = request->screen.savelines;
   new->screen.scrolllines = request->screen.scrolllines;
   new->screen.scrollttyoutput = request->screen.scrollttyoutput;
   new->screen.scrollkey = request->screen.scrollkey;
   new->screen.visualbell = request->screen.visualbell;
   new->screen.TekEmu = request->screen.TekEmu;
   new->misc.re_verse = request->misc.re_verse;
   new->screen.multiClickTime = request->screen.multiClickTime;
   new->screen.bellSuppressTime = request->screen.bellSuppressTime;
   new->screen.charClass = request->screen.charClass;
   new->screen.cutNewline = request->screen.cutNewline;
   new->screen.cutToBeginningOfLine = request->screen.cutToBeginningOfLine;
   new->screen.always_highlight = request->screen.always_highlight;
   new->screen.pointer_cursor = request->screen.pointer_cursor;
   new->screen.input_eight_bits = request->screen.input_eight_bits;
   new->screen.output_eight_bits = request->screen.output_eight_bits;
   new->screen.allowSendEvents = request->screen.allowSendEvents;
   new->misc.titeInhibit = request->misc.titeInhibit;
#ifdef KTERM
  for (fnum = F_ISO8859_1; fnum < FCNT; fnum++) {
#endif /* KTERM */
   for (i = fontMenu_font1; i <= fontMenu_lastBuiltin; i++) {
       new->screen.menu_font_names[i] = request->screen.menu_font_names[i];
   }
   /* set default in realize proc */
   new->screen.menu_font_names[fontMenu_fontdefault] = NULL;
   new->screen.menu_font_names[fontMenu_fontescape] = NULL;
   new->screen.menu_font_names[fontMenu_fontsel] = NULL;
#ifdef KTERM
   new->screen.menu_bfont_names[fontMenu_fontdefault] = NULL;
   new->screen.menu_bfont_names[fontMenu_fontescape] = NULL;
   new->screen.menu_bfont_names[fontMenu_fontsel] = NULL;
  }
   for (i = 0; i < NMENUFONTS; i++) {
       new->screen.menu_font_list[i] = request->screen.menu_font_list[i];
       new->screen.menu_bfont_list[i] = request->screen.menu_bfont_list[i];
   }
   /* set default in realize proc */
   new->screen.menu_font_list[fontMenu_fontdefault] = NULL;
   new->screen.menu_font_list[fontMenu_fontescape] = NULL;
   new->screen.menu_font_list[fontMenu_fontsel] = NULL;
   new->screen.menu_bfont_list[fontMenu_fontdefault] = NULL;
   new->screen.menu_bfont_list[fontMenu_fontescape] = NULL;
   new->screen.menu_bfont_list[fontMenu_fontsel] = NULL;
#endif /* KTERM */
   new->screen.menu_font_number = fontMenu_fontdefault;
#ifdef STATUSLINE
   new->screen.statusline = request->screen.statusline;
   new->screen.reversestatus = request->screen.reversestatus;
#endif /* STATUSLINE */
#ifdef KTERM
   new->screen.linespace = request->screen.linespace;
#endif /* KTERM */

    /*
     * The definition of -rv now is that it changes the definition of 
     * XtDefaultForeground and XtDefaultBackground.  So, we no longer
     * need to do anything special.
     */
   new->keyboard.flags = 0;
   new->screen.display = new->core.screen->display;
   new->core.height = new->core.width = 1;
      /* dummy values so that we don't try to Realize the parent shell 
	 with height or width of 0, which is illegal in X.  The real
	 size is computed in the xtermWidget's Realize proc,
	 but the shell's Realize proc is called first, and must see
	 a valid size. */

   /* look for focus related events on the shell, because we need
    * to care about the shell's border being part of our focus.
    */
   XtAddEventHandler(XtParent(new), EnterWindowMask, FALSE,
		HandleEnterWindow, (Opaque)NULL);
   XtAddEventHandler(XtParent(new), LeaveWindowMask, FALSE,
		HandleLeaveWindow, (Opaque)NULL);
   XtAddEventHandler(XtParent(new), FocusChangeMask, FALSE,
		HandleFocusChange, (Opaque)NULL);
   XtAddEventHandler((Widget)new, 0L, TRUE,
		VTNonMaskableEvent, (Opaque)NULL);
   XtAddEventHandler((Widget)new, PropertyChangeMask, FALSE,
		     HandleBellPropertyChange, (Opaque)NULL);
   new->screen.bellInProgress = FALSE;

   set_character_class (new->screen.charClass);

   /* create it, but don't realize it */
   ScrollBarOn (new, TRUE, FALSE);

   /* make sure that the resize gravity acceptable */
   if ( new->misc.resizeGravity != NorthWestGravity &&
        new->misc.resizeGravity != SouthWestGravity) {
       Cardinal nparams = 1;

       XtAppWarningMsg(app_con, "rangeError", "resizeGravity", "XTermError",
		       "unsupported resizeGravity resource value (%d)",
		       (String *) &(new->misc.resizeGravity), &nparams);
       new->misc.resizeGravity = SouthWestGravity;
   }

   return;
}


static void VTDestroy (w)
Widget w;
{
#ifdef KTERM
    XtFree((char *)(((XtermWidget)w)->screen.selection));
#else /* !KTERM */
    XtFree(((XtermWidget)w)->screen.selection);
#endif /* !KTERM */
}

/*ARGSUSED*/
static void VTRealize (w, valuemask, values)
    Widget w;
    XtValueMask *valuemask;
    XSetWindowAttributes *values;
{
	unsigned int width, height;
	register TScreen *screen = &term->screen;
	int xpos, ypos, pr;
	XSizeHints		sizehints;
	int scrollbar_width;
#ifdef KTERM
	int fnum = F_ISO8859_1;
#endif /* KTERM */

	TabReset (term->tabs);

#ifdef KTERM
	screen->menu_font_list[fontMenu_fontdefault] = term->misc.fontlist;
	screen->menu_bfont_list[fontMenu_fontdefault] = term->misc.bfontlist;
	for (fnum = F_ISO8859_1; fnum < FCNT; fnum ++) {
	    screen->menu_font_names[fontMenu_fontdefault] = term->misc.f_n;
	    screen->menu_bfont_names[fontMenu_fontdefault] = term->misc.f_b;
	    screen->fnt_norm = screen->fnt_bold = NULL;
	}
	fnum = F_ISO8859_1;
	if (!LoadNewFont(screen, NULL, NULL, False, 0)) {
	    if (term->misc.f_n == NULL
	     || XmuCompareISOLatin1(term->misc.f_n, "fixed") != 0) {
#else /* !KTERM */
	screen->menu_font_names[fontMenu_fontdefault] = term->misc.f_n;
	screen->fnt_norm = screen->fnt_bold = NULL;
	if (!LoadNewFont(screen, term->misc.f_n, term->misc.f_b, False, 0)) {
	    if (XmuCompareISOLatin1(term->misc.f_n, "fixed") != 0) {
#endif /* !KTERM */
		fprintf (stderr, 
		     "%s:  unable to open font \"%s\", trying \"fixed\"....\n",
		     xterm_name, term->misc.f_n);
#ifdef KTERM
		screen->menu_font_names[fontMenu_fontdefault] = "fixed";
		(void) LoadNewFont (screen, NULL, NULL, False, 0);
#else /* !KTERM */
		(void) LoadNewFont (screen, "fixed", NULL, False, 0);
		screen->menu_font_names[fontMenu_fontdefault] = "fixed";
#endif /* !KTERM */
	    }
	}

	/* really screwed if we couldn't open default font */
	if (!screen->fnt_norm) {
	    fprintf (stderr, "%s:  unable to locate a suitable font\n",
		     xterm_name);
	    Exit (1);
	}

	/* making cursor */
	if (!screen->pointer_cursor) 
	  screen->pointer_cursor = make_colored_cursor(XC_xterm, 
						       screen->mousecolor,
						       screen->mousecolorback);
	else 
	  recolor_cursor (screen->pointer_cursor, 
			  screen->mousecolor, screen->mousecolorback);

	scrollbar_width = (term->misc.scrollbar ?
			   screen->scrollWidget->core.width /* +
			   screen->scrollWidget->core.border_width */ : 0);

	/* set defaults */
	xpos = 1; ypos = 1; width = 80; height = 24;
	pr = XParseGeometry (term->misc.geo_metry, &xpos, &ypos,
			     &width, &height);
	screen->max_col = (width - 1);	/* units in character cells */
	screen->max_row = (height - 1);	/* units in character cells */
	update_font_info (&term->screen, False);

	width = screen->fullVwin.fullwidth;
	height = screen->fullVwin.fullheight;

	if ((pr & XValue) && (XNegative&pr)) 
	  xpos += DisplayWidth(screen->display, DefaultScreen(screen->display))
			- width - (term->core.parent->core.border_width * 2);
	if ((pr & YValue) && (YNegative&pr))
	  ypos += DisplayHeight(screen->display,DefaultScreen(screen->display))
			- height - (term->core.parent->core.border_width * 2);

	/* set up size hints for window manager; min 1 char by 1 char */
	sizehints.base_width = 2 * screen->border + scrollbar_width;
	sizehints.base_height = 2 * screen->border;
	sizehints.width_inc = FontWidth(screen);
	sizehints.height_inc = FontHeight(screen);
	sizehints.min_width = sizehints.base_width + sizehints.width_inc;
	sizehints.min_height = sizehints.base_height + sizehints.height_inc;
	sizehints.flags = (PBaseSize|PMinSize|PResizeInc);
	sizehints.x = xpos;
	sizehints.y = ypos;
	if ((XValue&pr) || (YValue&pr)) {
	    sizehints.flags |= USSize|USPosition;
	    sizehints.flags |= PWinGravity;
	    switch (pr & (XNegative | YNegative)) {
	      case 0:
		sizehints.win_gravity = NorthWestGravity;
		break;
	      case XNegative:
		sizehints.win_gravity = NorthEastGravity;
		break;
	      case YNegative:
		sizehints.win_gravity = SouthWestGravity;
		break;
	      default:
		sizehints.win_gravity = SouthEastGravity;
		break;
	    }
	} else {
	    /* set a default size, but do *not* set position */
	    sizehints.flags |= PSize;
	}
	sizehints.width = width;
	sizehints.height = height;
	if ((WidthValue&pr) || (HeightValue&pr)) 
	  sizehints.flags |= USSize;
	else sizehints.flags |= PSize;
#ifdef STATUSLINE
	sizehints.base_height += screen->statusheight;
	sizehints.min_height += screen->statusheight;
#endif /* STATUSLINE */

	(void) XtMakeResizeRequest((Widget) term,
				   (Dimension)width, (Dimension)height,
				   &term->core.width, &term->core.height);

	/* XXX This is bogus.  We are parsing geometries too late.  This
	 * is information that the shell widget ought to have before we get
	 * realized, so that it can do the right thing.
	 */
        if (sizehints.flags & USPosition)
	    XMoveWindow (XtDisplay(term), term->core.parent->core.window,
			 sizehints.x, sizehints.y);

	XSetWMNormalHints (XtDisplay(term), term->core.parent->core.window,
			   &sizehints);
	XFlush (XtDisplay(term));	/* get it out to window manager */

#ifdef STATUSLINE
	values->bit_gravity = ForgetGravity;
#else /* !STATUSLINE */
	/* use ForgetGravity instead of SouthWestGravity because translating
	   the Expose events for ConfigureNotifys is too hard */
	values->bit_gravity = term->misc.resizeGravity == NorthWestGravity ?
	    NorthWestGravity : ForgetGravity;
#endif /* !STATUSLINE */
	term->screen.fullVwin.window = term->core.window =
	  XCreateWindow(XtDisplay(term), XtWindow(term->core.parent),
		term->core.x, term->core.y,
		term->core.width, term->core.height, term->core.border_width,
		(int) term->core.depth,
		InputOutput, CopyFromParent,	
		*valuemask|CWBitGravity, values);

#ifdef KTERM
	set_cursor_gcs (screen, F_ISO8859_1);
#else /* !KTERM */
	set_cursor_gcs (screen);
#endif /* !KTERM */

	/* Reset variables used by ANSI emulation. */

#ifdef KTERM
	screen->gsets[0] = GSET_ASCII;
# ifdef KTERM_KANJI
	screen->gsets[1] = (term->flags & EUC_KANJI) ? GSET_KANJI : GSET_KANA;
	screen->gsets[2] = (term->flags & EUC_KANJI) ? GSET_KANA : GSET_ASCII;
# else /* !KTERM_KANJI */
	screen->gsets[1] = GSET_KANA;
	screen->gsets[2] = GSET_ASCII;
# endif /* !KTERM_KANJI */
	screen->gsets[3] = GSET_ASCII;
#else /* !KTERM */
	screen->gsets[0] = 'B';			/* ASCII_G		*/
	screen->gsets[1] = 'B';
	screen->gsets[2] = 'B';			/* DEC supplemental.	*/
	screen->gsets[3] = 'B';
#endif /* !KTERM */
	screen->curgl = 0;			/* G0 => GL.		*/
#ifdef KTERM
	screen->curgr = 1;			/* G1 => GR.		*/
#else /* !KTERM */
	screen->curgr = 2;			/* G2 => GR.		*/
#endif /* !KTERM */
	screen->curss = 0;			/* No single shift.	*/

	XDefineCursor(screen->display, VShellWindow, screen->pointer_cursor);

        screen->cur_col = screen->cur_row = 0;
#ifdef KTERM
	screen->max_col = Width(screen)/FontWidth(screen) - 1;
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row = Height(screen) /
				FontHeight(screen) - 1;
#else /* !KTERM */
	screen->max_col = Width(screen)/screen->fullVwin.f_width - 1;
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row = Height(screen) /
				screen->fullVwin.f_height - 1;
#endif /* !KTERM */

	screen->sc.row = screen->sc.col = screen->sc.flags = 0;

	/* Mark screen buffer as unallocated.  We wait until the run loop so
	   that the child process does not fork and exec with all the dynamic
	   memory it will never use.  If we were to do it here, the
	   swap space for new process would be huge for huge savelines. */
	if (!tekWidget)			/* if not called after fork */
	  screen->buf = screen->allbuf = NULL;

	screen->do_wrap = 0;
	screen->scrolls = screen->incopy = 0;
	set_vt_box (screen);
#ifdef STATUSLINE
	status_box[0].x = screen->border - 1;
#endif /* STATUSLINE */

	screen->savedlines = 0;

	if (term->misc.scrollbar) {
		screen->scrollbar = 0;
		ScrollBarOn (term, FALSE, TRUE);
	}
	CursorSave (term, &screen->sc);
	return;
}

static Boolean VTSetValues (cur, request, new, args, num_args)
    Widget cur, request, new;
    ArgList args;
    Cardinal *num_args;
{
    XtermWidget curvt = (XtermWidget) cur;
    XtermWidget newvt = (XtermWidget) new; 
    Boolean refresh_needed = FALSE;
    Boolean fonts_redone = FALSE;
#ifdef KTERM
    int fnum = F_ISO8859_1;
#endif /* KTERM */

    if(curvt->core.background_pixel != newvt->core.background_pixel
       || curvt->screen.foreground != newvt->screen.foreground
       || curvt->screen.menu_font_names[curvt->screen.menu_font_number]
          != newvt->screen.menu_font_names[newvt->screen.menu_font_number]
       || curvt->misc.f_n != newvt->misc.f_n) {
	if(curvt->misc.f_n != newvt->misc.f_n)
	    newvt->screen.menu_font_names[fontMenu_fontdefault] = newvt->misc.f_n;
#ifdef KTERM
	fprintf(stderr, "kterm(VTSetValues): Font changing to %s\n",
		newvt->screen.menu_font_names[curvt->screen.menu_font_number]);
#endif /* KTERM */
	if (LoadNewFont(&newvt->screen,
			newvt->screen.menu_font_names[curvt->screen.menu_font_number],
			newvt->screen.menu_font_names[curvt->screen.menu_font_number],
			TRUE, newvt->screen.menu_font_number)) {
	    /* resizing does the redisplay, so don't ask for it here */
	    refresh_needed = TRUE;
	    fonts_redone = TRUE;
	} else
	    if(curvt->misc.f_n != newvt->misc.f_n)
		newvt->screen.menu_font_names[fontMenu_fontdefault] = curvt->misc.f_n;
    }
    if(!fonts_redone
       && curvt->screen.cursorcolor != newvt->screen.cursorcolor) {
	set_cursor_gcs(&newvt->screen);
	refresh_needed = TRUE;
    }
    if(curvt->misc.re_verse != newvt->misc.re_verse) {
	newvt->flags ^= REVERSE_VIDEO;
	ReverseVideo(newvt);
	newvt->misc.re_verse = !newvt->misc.re_verse; /* ReverseVideo toggles */
	refresh_needed = TRUE;
    }
    if(curvt->screen.mousecolor != newvt->screen.mousecolor
       || curvt->screen.mousecolorback != newvt->screen.mousecolorback) {
	recolor_cursor (newvt->screen.pointer_cursor, 
			newvt->screen.mousecolor,
			newvt->screen.mousecolorback);
	refresh_needed = TRUE;
    }
    if (curvt->misc.scrollbar != newvt->misc.scrollbar) {
	if (newvt->misc.scrollbar) {
	    ScrollBarOn (newvt, FALSE, FALSE);
	} else {
	    ScrollBarOff (&newvt->screen);
	}
	update_scrollbar();
    }

    return refresh_needed;
}

/*
 * Shows cursor at new cursor position in screen.
 */
ShowCursor()
{
	register TScreen *screen = &term->screen;
	register int x, y, flags;
#ifdef KTERM
	XChar2b c;
	Char gset;
#else /* !KTERM */
	Char c;
	GC	currentGC;
#endif /* !KTERM */
	Boolean	in_selection;

	if (eventMode != NORMAL) return;

#ifdef STATUSLINE
	if (!screen->instatus &&
	    screen->cur_row - screen->topline > screen->max_row)
#else /* !STATUSLINE */
	if (screen->cur_row - screen->topline > screen->max_row)
#endif /* !STATUSLINE */
		return;
#ifdef KTERM
	gset = screen->buf[y = screen->cursor_row = screen->cur_row]
				[x = screen->cursor_col = screen->cur_col].gset;
	if (gset == MBC2) {
		gset = screen->buf[y][x-1].gset;
		x --;
	}
	if (gset & MBCS) {
		c.byte1 = screen->buf[y][x].code;
		c.byte2 = screen->buf[y][x+1].code;
	} else {
		c.byte1 = 0;
		c.byte2 = screen->buf[y][x].code;
		if (c.byte2 == 0) {
			c.byte2 = ' ';
			gset = GSET_ASCII;
		}
	}
	flags = screen->buf[y][x].attr;
#else /* !KTERM */
	c = screen->buf[y = 2 * (screen->cursor_row = screen->cur_row)]
	 [x = screen->cursor_col = screen->cur_col];
	flags = screen->buf[y + 1][x];
	if (c == 0)
		c = ' ';
#endif /* !KTERM */

	if (screen->cur_row > screen->endHRow ||
	    (screen->cur_row == screen->endHRow &&
	     screen->cur_col >= screen->endHCol) ||
	    screen->cur_row < screen->startHRow ||
	    (screen->cur_row == screen->startHRow &&
	     screen->cur_col < screen->startHCol))
	    in_selection = False;
	else
	    in_selection = True;

#ifdef KTERM
# ifdef STATUSLINE
	if ((screen->select || screen->always_highlight) ^ in_selection
	    ^ (screen->instatus && screen->reversestatus))
# else /* !STATUSLINE */
	if ((screen->select || screen->always_highlight) ^ in_selection)
# endif /* !STATUSLINE */
		flags ^= INVERSE;
	x = CursorX(screen, x);
	y = CursorY(screen, y);
	ScreenDraw(screen, x, y, &c, 1, gset, flags, True);
#else /* !KTERM */
# ifdef STATUSLINE
	if (screen->instatus && screen->reversestatus)
		flags ^= INVERSE;
# endif /* STATUSLINE */

	if(screen->select || screen->always_highlight) {
		if (( (flags & INVERSE) && !in_selection) ||
		    (!(flags & INVERSE) &&  in_selection)){
		    /* text is reverse video */
		    if (screen->cursorGC) {
			currentGC = screen->cursorGC;
		    } else {
			if (flags & BOLD) {
				currentGC = screen->normalboldGC;
			} else {
				currentGC = screen->normalGC;
			}
		    }
		} else { /* normal video */
		    if (screen->reversecursorGC) {
			currentGC = screen->reversecursorGC;
		    } else {
			if (flags & BOLD) {
				currentGC = screen->reverseboldGC;
			} else {
				currentGC = screen->reverseGC;
			}
		    }
		}
	} else { /* not selected */
		if (( (flags & INVERSE) && !in_selection) ||
		    (!(flags & INVERSE) &&  in_selection)) {
		    /* text is reverse video */
			currentGC = screen->reverseGC;
		} else { /* normal video */
			currentGC = screen->normalGC;
		}
	    
	}

	x = CursorX (screen, screen->cur_col);
	y = CursorY(screen, screen->cur_row) + 
	  screen->fnt_norm->ascent;
	XDrawImageString(screen->display, TextWindow(screen), currentGC,
		x, y, (char *) &c, 1);

	if((flags & BOLD) && screen->enbolden) /* no bold font */
		XDrawString(screen->display, TextWindow(screen), currentGC,
			x + 1, y, (char *) &c, 1);
	if(flags & UNDERLINE) 
		XDrawLine(screen->display, TextWindow(screen), currentGC,
			x, y+1, x + FontWidth(screen), y+1);
	if (!screen->select && !screen->always_highlight) {
		screen->box->x = x;
		screen->box->y = y - screen->fnt_norm->ascent;
		XDrawLines (screen->display, TextWindow(screen), 
			    screen->cursoroutlineGC ? screen->cursoroutlineGC 
			    			    : currentGC,
			    screen->box, NBOX, CoordModePrevious);
	}
#endif /* !KTERM */
	screen->cursor_state = ON;
#ifdef KTERM_KCONV
	SendSpot();
#endif /* KTERM_KCONV */
}

/*
 * hide cursor at previous cursor position in screen.
 */
HideCursor()
{
	register TScreen *screen = &term->screen;
#ifdef KTERM
	register int x, y, flags;
	XChar2b c;
	Char gset;
#else /* !KTERM */
	GC	currentGC;
	register int x, y, flags;
	char c;
#endif /* !KTERM */
	Boolean	in_selection;

#ifdef STATUSLINE
	Boolean instatus;

	if(!(instatus = screen->cursor_row > screen->max_row) &&
	   screen->cursor_row - screen->topline > screen->max_row)
#else /* !STATUSLINE */
	if(screen->cursor_row - screen->topline > screen->max_row)
#endif /* !STATUSLINE */
		return;
#ifdef KTERM
	gset = screen->buf[y = screen->cursor_row][x = screen->cursor_col].gset;
	if (gset == MBC2) {
		gset = screen->buf[y][x-1].gset;
		x --;
	}
	if (gset & MBCS) {
		c.byte1 = screen->buf[y][x].code;
		c.byte2 = screen->buf[y][x+1].code;
	} else {
		c.byte1 = 0;
		c.byte2 = screen->buf[y][x].code;
		if (c.byte2 == 0) {
			c.byte2 = ' ';
			gset = GSET_ASCII;
		}
	}
	flags = screen->buf[y][x].attr;
#else /* !KTERM */
	c = screen->buf[y = 2 * screen->cursor_row][x = screen->cursor_col];
	flags = screen->buf[y + 1][x];
#endif /* !KTERM */

	if (screen->cursor_row > screen->endHRow ||
	    (screen->cursor_row == screen->endHRow &&
	     screen->cursor_col >= screen->endHCol) ||
	    screen->cursor_row < screen->startHRow ||
	    (screen->cursor_row == screen->startHRow &&
	     screen->cursor_col < screen->startHCol))
	    in_selection = False;
	else
	    in_selection = True;

#ifdef KTERM
	x = CursorX(screen, x);
#else /* !KTERM */
# ifdef STATUSLINE
	if (screen->instatus && screen->reversestatus)
		flags ^= INVERSE;
# endif /* STATUSLINE */

	if (( (flags & INVERSE) && !in_selection) ||
	    (!(flags & INVERSE) &&  in_selection)) {
		if(flags & BOLD) {
			currentGC = screen->reverseboldGC;
		} else {
			currentGC = screen->reverseGC;
		}
	} else {
		if(flags & BOLD) {
			currentGC = screen->normalboldGC;
		} else {
			currentGC = screen->normalGC;
		}
	}

	if (c == 0)
		c = ' ';
	x = CursorX (screen, screen->cursor_col);
#endif /* !KTERM */
#ifdef STATUSLINE
	y = (instatus ? (screen->cursor_row * FontHeight(screen) + 1) :
	     ((screen->cursor_row - screen->topline) * FontHeight(screen))) +
#else /* !STATUSLINE */
	y = (((screen->cursor_row - screen->topline) * FontHeight(screen))) +
#endif /* !STATUSLINE */
	 screen->border;
#ifdef KTERM
# ifdef STATUSLINE
	if (in_selection ^ (screen->instatus && screen->reversestatus))
# else /* !STATUSLINE */
	if (in_selection)
# endif /* !STATUSLINE */
		flags ^= INVERSE;
	ScreenDraw(screen, x, y, &c, 1, gset, flags, False);
#else /* !KTERM */
	y = y+screen->fnt_norm->ascent;
	XDrawImageString(screen->display, TextWindow(screen), currentGC,
		x, y, &c, 1);
	if((flags & BOLD) && screen->enbolden)
		XDrawString(screen->display, TextWindow(screen), currentGC,
			x + 1, y, &c, 1);
	if(flags & UNDERLINE) 
		XDrawLine(screen->display, TextWindow(screen), currentGC,
			x, y+1, x + FontWidth(screen), y+1);
#endif /* !KTERM */
	screen->cursor_state = OFF;
}

VTReset(full)
    Boolean full;
{
	register TScreen *screen = &term->screen;

	/* reset scrolling region */
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row;
	term->flags &= ~ORIGIN;
	if(full) {
		TabReset (term->tabs);
		term->keyboard.flags = 0;
		update_appcursor();
		update_appkeypad();
#ifdef KTERM
		screen->gsets[0] = GSET_ASCII;
# ifdef KTERM_KANJI
		screen->gsets[1] = (term->flags & EUC_KANJI)
					? GSET_KANJI : GSET_KANA;
		screen->gsets[2] = (term->flags & EUC_KANJI)
					? GSET_KANA : GSET_ASCII;
# else /* !KTERM_KANJI */
		screen->gsets[1] = GSET_KANA;
		screen->gsets[2] = GSET_ASCII;
# endif /* !KTERM_KANJI */
		screen->gsets[3] = GSET_ASCII;
#else /* !KTERM */
		screen->gsets[0] = 'B';
		screen->gsets[1] = 'B';
		screen->gsets[2] = 'B';
		screen->gsets[3] = 'B';
#endif /* !KTERM */
		screen->curgl = 0;
#ifdef KTERM
		screen->curgr = 1;
#else /* !KTERM */
		screen->curgr = 2;
#endif /* !KTERM */
		screen->curss = 0;
		FromAlternate(screen);
		ClearScreen(screen);
#ifdef STATUSLINE
		EraseStatus();
#endif /* STATUSLINE */
		screen->cursor_state = OFF;
		if (term->flags & REVERSE_VIDEO)
			ReverseVideo(term);

		term->flags = term->initflags;
		update_reversevideo();
		update_autowrap();
		update_reversewrap();
		update_autolinefeed();
		screen->jumpscroll = !(term->flags & SMOOTHSCROLL);
		update_jumpscroll();
		if(screen->c132 && (term->flags & IN132COLUMNS)) {
		        Dimension junk;
			XtMakeResizeRequest(
			    (Widget) term,
			    (Dimension) 80*FontWidth(screen)
				+ 2 * screen->border + screen->scrollbar,
#ifdef STATUSLINE
			    (Dimension) screen->statusheight +
#endif /* STATUSLINE */
			    (Dimension) FontHeight(screen)
			        * (screen->max_row + 1) + 2 * screen->border,
			    &junk, &junk);
			XSync(screen->display, FALSE);	/* synchronize */
			if(XtAppPending(app_con))
				xevents();
		}
		CursorSet(screen, 0, 0, term->flags);
	}
	longjmp(vtjmpbuf, 1);	/* force ground state in parser */
}


#ifdef STATUSLINE
ToStatus(col)
int col;
{
	register TScreen *screen = &term->screen;

	if (screen->cursor_state)
		HideCursor();
	if (col > screen->max_col)
		col = screen->max_col;
	if (!screen->instatus) {
		if (!screen->statusline)
			ShowStatus();
		CursorSave(term, &screen->statussc);
		screen->instatus = TRUE;
		screen->cur_row = screen->max_row + 1;
	}
	screen->cur_col = col;
}

FromStatus()
{
	register TScreen *screen = &term->screen;

	if (!screen->instatus)
		return;
	screen->instatus = FALSE;
	CursorRestore(term, &screen->statussc);
}

ShowStatus()
{
	register TScreen *screen = &term->screen;
	register int border = 2 * screen->border;

	if (screen->statusline)
		return;
	screen->statusline = 1;
	screen->statusheight = FontHeight(screen) + 2;
	ResizeScreen(term, border + screen->scrollbar, border);
}

HideStatus()
{
	register TScreen *screen = &term->screen;
	register int border = 2 * screen->border;
#ifndef KTERM
	register int i, j;
#endif /* !KTERM */

	if (!screen->statusline)
		return;
	if (screen->instatus)
		FromStatus();
	screen->statusline = 0;
	screen->statusheight = 0;
#ifdef KTERM
	bzero(screen->buf[screen->max_row + 1],
		sizeof(Bchr) * (screen->max_col+1));
#else /* !KTERM */
	bzero(screen->buf[i = 2 * (screen->max_row + 1)],
		j = screen->max_col + 1);
	bzero(screen->buf[i + 1], j);
#endif /* !KTERM */
	ResizeScreen(term, border + screen->scrollbar, border);
}

EraseStatus()
{
	register TScreen *screen = &term->screen;
	register int j, pix;
#ifdef KTERM
	int fnum = F_ISO8859_1; /* referd by normalGC and reverseGC */
#else /* !KTERM */
	register int i;
#endif /* !KTERM */

	if (!screen->statusline)
		return;
#ifdef KTERM
	bzero(screen->buf[screen->max_row + 1],
		j = sizeof(Bchr) * (screen->max_col+1));
#else /* !KTERM */
	bzero(screen->buf[i = 2 * (screen->max_row + 1)],
		j = screen->max_col + 1);
	bzero(screen->buf[i + 1], j) ;
#endif /* !KTERM */
	XFillRectangle(screen->display, TextWindow(screen),
		screen->reversestatus ? screen->normalGC : screen->reverseGC,
		screen->border - 1 + screen->scrollbar,
		(screen->max_row + 1) * FontHeight(screen) +
		screen->border,
		j * FontWidth(screen) + 2, screen->statusheight);
	if (!screen->reversestatus)
		StatusBox(screen);
}

StatusBox(screen)
register TScreen *screen;
{
#ifdef KTERM
	int fnum = F_ISO8859_1; /* refered by normalGC */
#endif /* KTERM */

	status_box[0].x = screen->scrollbar + screen->border - 1;
	status_box[0].y = (screen->max_row + 1) * FontHeight(screen) +
		screen->border;
	status_box[3].x = -(status_box[1].x = (screen->max_col + 1) *
		FontWidth(screen) + 1);
	status_box[4].y = -(status_box[2].y = FontHeight(screen) + 1);
	XDrawLines(screen->display, TextWindow(screen), screen->normalGC,
		status_box, NBOX, CoordModePrevious);
}
#endif /* STATUSLINE */

/*
 * set_character_class - takes a string of the form
 * 
 *                 low[-high]:val[,low[-high]:val[...]]
 * 
 * and sets the indicated ranges to the indicated values.
 */

int set_character_class (s)
    register char *s;
{
    register int i;			/* iterator, index into s */
    int len;				/* length of s */
    int acc;				/* accumulator */
    int low, high;			/* bounds of range [0..127] */
    int base;				/* 8, 10, 16 (octal, decimal, hex) */
    int numbers;			/* count of numbers per range */
    int digits;				/* count of digits in a number */
    static char *errfmt = "%s:  %s in range string \"%s\" (position %d)\n";
    extern char *ProgramName;

    if (!s || !s[0]) return -1;

    base = 10;				/* in case we ever add octal, hex */
    low = high = -1;			/* out of range */

    for (i = 0, len = strlen (s), acc = 0, numbers = digits = 0;
	 i < len; i++) {
	char c = s[i];

	if (isspace(c)) {
	    continue;
	} else if (isdigit(c)) {
	    acc = acc * base + (c - '0');
	    digits++;
	    continue;
	} else if (c == '-') {
	    low = acc;
	    acc = 0;
	    if (digits == 0) {
		fprintf (stderr, errfmt, ProgramName, "missing number", s, i);
		return (-1);
	    }
	    digits = 0;
	    numbers++;
	    continue;
	} else if (c == ':') {
	    if (numbers == 0)
	      low = acc;
	    else if (numbers == 1)
	      high = acc;
	    else {
		fprintf (stderr, errfmt, ProgramName, "too many numbers",
			 s, i);
		return (-1);
	    }
	    digits = 0;
	    numbers++;
	    acc = 0;
	    continue;
	} else if (c == ',') {
	    /*
	     * now, process it
	     */

	    if (high < 0) {
		high = low;
		numbers++;
	    }
	    if (numbers != 2) {
		fprintf (stderr, errfmt, ProgramName, "bad value number", 
			 s, i);
	    } else if (SetCharacterClassRange (low, high, acc) != 0) {
		fprintf (stderr, errfmt, ProgramName, "bad range", s, i);
	    }

	    low = high = -1;
	    acc = 0;
	    digits = 0;
	    numbers = 0;
	    continue;
	} else {
	    fprintf (stderr, errfmt, ProgramName, "bad character", s, i);
	    return (-1);
	}				/* end if else if ... else */

    }

    if (low < 0 && high < 0) return (0);

    /*
     * now, process it
     */

    if (high < 0) high = low;
    if (numbers < 1 || numbers > 2) {
	fprintf (stderr, errfmt, ProgramName, "bad value number", s, i);
    } else if (SetCharacterClassRange (low, high, acc) != 0) {
	fprintf (stderr, errfmt, ProgramName, "bad range", s, i);
    }

    return (0);
}

/* ARGSUSED */
static void HandleKeymapChange(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    static XtTranslations keymap, original;
    static XtResource key_resources[] = {
	{ XtNtranslations, XtCTranslations, XtRTranslationTable,
	      sizeof(XtTranslations), 0, XtRTranslationTable, (caddr_t)NULL}
    };
    char mapName[1000];
    char mapClass[1000];

    if (*param_count != 1) return;

    if (original == NULL) original = w->core.tm.translations;

    if (strcmp(params[0], "None") == 0) {
	XtOverrideTranslations(w, original);
	return;
    }
    (void) sprintf( mapName, "%sKeymap", params[0] );
    (void) strcpy( mapClass, mapName );
    if (islower(mapClass[0])) mapClass[0] = toupper(mapClass[0]);
    XtGetSubresources( w, (XtPointer)&keymap, mapName, mapClass,
		       key_resources, (Cardinal)1, NULL, (Cardinal)0 );
    if (keymap != NULL)
	XtOverrideTranslations(w, keymap);
}


/* ARGSUSED */
static void HandleBell(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* [0] = volume */
    Cardinal *param_count;	/* 0 or 1 */
{
    int percent = (*param_count) ? atoi(params[0]) : 0;

    XBell( XtDisplay(w), percent );
}


/* ARGSUSED */
static void HandleVisualBell(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* unused */
    Cardinal *param_count;	/* unused */
{
    VisualBell();
}


/* ARGSUSED */
static void HandleIgnore(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* unused */
    Cardinal *param_count;	/* unused */
{
    /* do nothing, but check for funny escape sequences */
    (void) SendMousePosition(w, event);
}


/* ARGSUSED */
static void
DoSetSelectedFont(w, client_data, selection, type, value, length, format)
    Widget w;
    XtPointer client_data;
    Atom *selection, *type;
    XtPointer value;
    unsigned long *length;
    int *format;
{
    char *val = (char *)value;
    int len;
    if (*type != XA_STRING  ||  *format != 8) {
	Bell();
	return;
    }
    len = strlen(val);
    if (len > 0) {
	if (val[len-1] == '\n') val[len-1] = '\0';
	/* Do some sanity checking to avoid sending a long selection
	   back to the server in an OpenFont that is unlikely to succeed.
	   XLFD allows up to 255 characters and no control characters;
	   we are a little more liberal here. */
	if (len > 1000  ||  strchr(val, '\n'))
	    return;
	if (!LoadNewFont (&term->screen, val, NULL, True, fontMenu_fontsel))
	    Bell();
    }
}

void FindFontSelection (atom_name, justprobe)
    char *atom_name;
    Bool justprobe;
{
    static AtomPtr *atoms;
    static int atomCount = 0;
    AtomPtr *pAtom;
    int a;
    Atom target;
#ifdef KTERM
    int fnum = F_ISO8859_1;
#endif /* KTERM */

    if (!atom_name) atom_name = "PRIMARY";

    for (pAtom = atoms, a = atomCount; a; a--, pAtom++) {
	if (strcmp(atom_name, XmuNameOfAtom(*pAtom)) == 0) break;
    }
    if (!a) {
	atoms = (AtomPtr*) XtRealloc ((char *)atoms,
				      sizeof(AtomPtr)*(atomCount+1));
	*(pAtom = &atoms[atomCount++]) = XmuMakeAtom(atom_name);
    }

    target = XmuInternAtom(XtDisplay(term), *pAtom);
    if (justprobe) {
	term->screen.menu_font_names[fontMenu_fontsel] = 
	  XGetSelectionOwner(XtDisplay(term), target) ? _Font_Selected_ : NULL;
    } else {
	XtGetSelectionValue((Widget)term, target, XA_STRING,
			    DoSetSelectedFont, NULL,
			    XtLastTimestampProcessed(XtDisplay(term)));
    }
    return;
}


/* ARGSUSED */
void HandleSetFont(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* unused */
    Cardinal *param_count;	/* unused */
{
    int fontnum;
    char *name1 = NULL, *name2 = NULL;

    if (*param_count == 0) {
	fontnum = fontMenu_fontdefault;
    } else {
	int maxparams = 1;		/* total number of params allowed */

	switch (params[0][0]) {
	  case 'd': case 'D': case '0':
	    fontnum = fontMenu_fontdefault; break;
	  case '1':
	    fontnum = fontMenu_font1; break;
	  case '2':
	    fontnum = fontMenu_font2; break;
	  case '3':
	    fontnum = fontMenu_font3; break;
	  case '4':
	    fontnum = fontMenu_font4; break;
	  case '5':
	    fontnum = fontMenu_font5; break;
	  case '6':
	    fontnum = fontMenu_font6; break;
	  case 'e': case 'E':
	    fontnum = fontMenu_fontescape; maxparams = 3; break;
	  case 's': case 'S':
	    fontnum = fontMenu_fontsel; maxparams = 2; break;
	  default:
	    Bell();
	    return;
	}
	if (*param_count > maxparams) {	 /* see if extra args given */
	    Bell();
	    return;
	}
	switch (*param_count) {		/* assign 'em */
	  case 3:
	    name2 = params[2];
	    /* fall through */
	  case 2:
	    name1 = params[1];
	    break;
	}
    }

    SetVTFont (fontnum, True, name1, name2);
}


void SetVTFont (i, doresize, name1, name2)
    int i;
    Bool doresize;
    char *name1, *name2;
{
    TScreen *screen = &term->screen;

    if (i < 0 || i >= NMENUFONTS) {
	Bell();
	return;
    }
    if (i == fontMenu_fontsel) {	/* go get the selection */
	FindFontSelection (name1, False);  /* name1 = atom, name2 is ignored */
	return;
    }
#ifndef KTERM
    if (!name1) name1 = screen->menu_font_names[i];
#endif /* !KTERM */
    if (!LoadNewFont(screen, name1, name2, doresize, i)) {
	Bell();
    }
    return;
}


#ifdef KTERM
static char *search_font_matching(screen, fontpattern, fonttail)
TScreen *screen;
char *fontpattern;
char *fonttail;
{
	static char **fontnamelist;
	char *tmptail;
	int count, i;
	int fonttaillen;

	if (!fontpattern) return NULL;

	fonttaillen = strlen(fonttail);
	if (fontnamelist) {
		XFreeFontNames(fontnamelist);
	}
	fontnamelist = XListFonts(screen->display, fontpattern, 1000, &count);
	for (i = 0; i < count; i ++) {
		tmptail = fontnamelist[i]+strlen(fontnamelist[i])-fonttaillen;
		if (XmuCompareISOLatin1(fonttail, tmptail) == 0) {
			return fontnamelist[i];
		}
	}
	return NULL;
}
#endif /* KTERM */

#ifdef KTERM
int LoadNewFont (screen, nfontpat, bfontpat, doresize, fontnum)
    TScreen *screen;
    char *nfontpat, *bfontpat;
#else /* !KTERM */
int LoadNewFont (screen, nfontname, bfontname, doresize, fontnum)
    TScreen *screen;
    char *nfontname, *bfontname;
#endif /* !KTERM */
    Bool doresize;
    int fontnum;
{
    XFontStruct *nfs = NULL, *bfs = NULL;
    XGCValues xgcv;
    unsigned long mask;
    GC new_normalGC = NULL, new_normalboldGC = NULL;
    GC new_reverseGC = NULL, new_reverseboldGC = NULL;
    char *tmpname = NULL;
#ifdef KTERM
    char *tmpbname = NULL;
    XFontStruct *asciinfs = NULL;
    int fnum;
    char *nfontname, *bfontname;
    static char *fonttail[FCNT] = {
	"-iso8859-1",
	"-jisx0201.1976-0",
#ifdef KTERM_MBCS
	"-jisx0208.1983-0",
#endif /* KTERM_MBCS */
    };
#endif /* KTERM */

#ifdef KTERM
  for (fnum = F_ISO8859_1; fnum < FCNT; fnum ++) {

    nfontname = search_font_matching(screen, nfontpat, fonttail[fnum]);
    if (!nfontname) nfontname = screen->menu_font_names[fontnum];

    if (!(nfontname && (nfs = XLoadQueryFont (screen->display, nfontname)))) {
	nfontname = search_font_matching(screen,
			screen->menu_font_list[fontnum], fonttail[fnum]);
	if (!(nfontname &&
	      (nfs = XLoadQueryFont (screen->display, nfontname)))) {
		if (fnum > F_ISO8859_1) {
		    nfs = asciinfs;
		} else goto bad;
	}
    }
    if (fnum == F_ISO8859_1) asciinfs = nfs;

    if (nfontname && fontnum == fontMenu_fontescape &&
	nfontname != screen->menu_font_names[fontnum]) {
	tmpname = (char *) malloc (strlen(nfontname) + 1);
	if (!tmpname) return 0;
	strcpy (tmpname, nfontname);
    }
#else /* !KTERM */
    if (!nfontname) return 0;

    if (fontnum == fontMenu_fontescape &&
	nfontname != screen->menu_font_names[fontnum]) {
	tmpname = (char *) malloc (strlen(nfontname) + 1);
	if (!tmpname) return 0;
	strcpy (tmpname, nfontname);
    }

    if (!(nfs = XLoadQueryFont (screen->display, nfontname))) goto bad;
#endif /* !KTERM */

#ifdef KTERM
    bfontname = search_font_matching(screen, bfontpat, fonttail[fnum]);
    if (!bfontname) bfontname = screen->menu_bfont_names[fontnum];

    if (!(bfontname && (bfs = XLoadQueryFont (screen->display, bfontname)))) {
	bfontname = search_font_matching(screen,
			screen->menu_bfont_list[fontnum], fonttail[fnum]);
	if (!(bfontname &&
	      (bfs = XLoadQueryFont (screen->display, bfontname)))) {
      		bfs = nfs;
	}
    }

    if (bfontname && fontnum == fontMenu_fontescape &&
	bfontname != screen->menu_bfont_names[fontnum]) {
	tmpbname = (char *) malloc (strlen(bfontname) + 1);
	if (!tmpbname) return 0;
	strcpy (tmpbname, bfontname);
    }
#else /* !KTERM */
    if (!(bfontname && 
	  (bfs = XLoadQueryFont (screen->display, bfontname))))
      bfs = nfs;
#endif /* !KTERM */

    mask = (GCFont | GCForeground | GCBackground | GCGraphicsExposures |
	    GCFunction);

    xgcv.font = nfs->fid;
    xgcv.foreground = screen->foreground;
    xgcv.background = term->core.background_pixel;
    xgcv.graphics_exposures = TRUE;	/* default */
    xgcv.function = GXcopy;

    new_normalGC = XtGetGC((Widget)term, mask, &xgcv);
    if (!new_normalGC) goto bad;

    if (nfs == bfs) {			/* there is no bold font */
	new_normalboldGC = new_normalGC;
    } else {
	xgcv.font = bfs->fid;
	new_normalboldGC = XtGetGC((Widget)term, mask, &xgcv);
	if (!new_normalboldGC) goto bad;
    }

    xgcv.font = nfs->fid;
    xgcv.foreground = term->core.background_pixel;
    xgcv.background = screen->foreground;
    new_reverseGC = XtGetGC((Widget)term, mask, &xgcv);
    if (!new_reverseGC) goto bad;

    if (nfs == bfs) {			/* there is no bold font */
	new_reverseboldGC = new_reverseGC;
    } else {
	xgcv.font = bfs->fid;
	new_reverseboldGC = XtGetGC((Widget)term, mask, &xgcv);
	if (!new_reverseboldGC) goto bad;
    }

    if (screen->normalGC != screen->normalboldGC)
	XtReleaseGC ((Widget) term, screen->normalboldGC);
    XtReleaseGC ((Widget) term, screen->normalGC);
    if (screen->reverseGC != screen->reverseboldGC)
	XtReleaseGC ((Widget) term, screen->reverseboldGC);
    XtReleaseGC ((Widget) term, screen->reverseGC);
    screen->normalGC = new_normalGC;
    screen->normalboldGC = new_normalboldGC;
    screen->reverseGC = new_reverseGC;
    screen->reverseboldGC = new_reverseboldGC;
    screen->fnt_norm = nfs;
    screen->fnt_bold = bfs;
    screen->enbolden = (nfs == bfs);
    set_menu_font (False);
    screen->menu_font_number = fontnum;
    set_menu_font (True);
    if (tmpname) {			/* if setting escape or sel */
	if (screen->menu_font_names[fontnum])
	  free (screen->menu_font_names[fontnum]);
	screen->menu_font_names[fontnum] = tmpname;
	if (fontnum == fontMenu_fontescape) {
	    set_sensitivity (term->screen.fontMenu,
			     fontMenuEntries[fontMenu_fontescape].widget,
			     TRUE);
	}
    }
#ifdef KTERM
    set_cursor_gcs (screen, fnum);
  }
#else /* !KTERM */
    set_cursor_gcs (screen);
#endif /* !KTERM */
    update_font_info (screen, doresize);
#ifdef KTERM_KCONV
    SendFonts();
#endif /* KTERM_KCONV */
    return 1;

  bad:
    if (tmpname) free (tmpname);
    if (new_normalGC)
      XtReleaseGC ((Widget) term, screen->normalGC);
    if (new_normalGC && new_normalGC != new_normalboldGC)
      XtReleaseGC ((Widget) term, new_normalboldGC);
    if (new_reverseGC)
      XtReleaseGC ((Widget) term, new_reverseGC);
    if (new_reverseGC && new_reverseGC != new_reverseboldGC)
      XtReleaseGC ((Widget) term, new_reverseboldGC);
    if (nfs) XFreeFont (screen->display, nfs);
    if (nfs && nfs != bfs) XFreeFont (screen->display, bfs);
    return 0;
}


update_font_info (screen, doresize)
    TScreen *screen;
    Bool doresize;
{
    int i, j, width, height, scrollbar_width;
#ifdef KTERM
    int fnum;
    int max_ascent = 0, max_descent = 0, max_width = 0;

    for (fnum = F_ISO8859_1; fnum < FCNT; fnum ++) {
	if (screen->_fnt_norm[fnum]) {
# ifdef KTERM_MBCS
	  if (screen->_fnt_norm[fnum]->max_byte1 > 0) { /* MB font */
	    if (max_width < screen->_fnt_norm[fnum]->max_bounds.width/2) {
		max_width = screen->_fnt_norm[fnum]->max_bounds.width/2;
	    }
	  } else
# endif /* KTERM_MBCS */
	    if (max_width < screen->_fnt_norm[fnum]->max_bounds.width) {
		max_width = screen->_fnt_norm[fnum]->max_bounds.width;
	    }
	}
    }
    screen->fullVwin.f_width = max_width;
    for (fnum = F_ISO8859_1; fnum < FCNT; fnum ++) {
	if (screen->_fnt_norm[fnum]) {
	    if (max_ascent < screen->_fnt_norm[fnum]->ascent) {
		max_ascent = screen->_fnt_norm[fnum]->ascent;
	    }
	    if (max_descent < screen->_fnt_norm[fnum]->descent) {
		max_descent = screen->_fnt_norm[fnum]->descent;
	    }
	}
    }
    screen->max_ascent = max_ascent;
    screen->max_descent = max_descent;
    screen->fullVwin.f_height = max_ascent + max_descent;
#else /* !KTERM */

    screen->fullVwin.f_width = screen->fnt_norm->max_bounds.width;
    screen->fullVwin.f_height = (screen->fnt_norm->ascent +
				 screen->fnt_norm->descent);
#endif /* !KTERM */
    scrollbar_width = (term->misc.scrollbar ? 
		       screen->scrollWidget->core.width +
		       screen->scrollWidget->core.border_width : 0);
    i = 2 * screen->border + scrollbar_width;
    j = 2 * screen->border;
#ifdef STATUSLINE
    if (screen->statusline)
# ifdef KTERM
	j += (screen->statusheight = FontHeight(screen) + 2);
# else /* !KTERM */
	j += (screen->statusheight = screen->fullVwin.f_height + 2);
# endif /* !KTERM */
#endif /* STATUSLINE */
#ifdef KTERM
    width = (screen->max_col + 1) * FontWidth(screen) + i;
    height = (screen->max_row + 1) * FontHeight(screen) + j;
#else /* !KTERM */
    width = (screen->max_col + 1) * screen->fullVwin.f_width + i;
    height = (screen->max_row + 1) * screen->fullVwin.f_height + j;
#endif /* !KTERM */
    screen->fullVwin.fullwidth = width;
    screen->fullVwin.fullheight = height;
    screen->fullVwin.width = width - i;
    screen->fullVwin.height = height - j;

    if (doresize) {
	if (VWindow(screen)) {
	    XClearWindow (screen->display, VWindow(screen));
	}
	DoResizeScreen (term);		/* set to the new natural size */
	if (screen->scrollWidget)
	  ResizeScrollBar (screen->scrollWidget, -1, -1,
			   Height(screen) + screen->border * 2);
	Redraw ();
    }
    set_vt_box (screen);
#ifdef KTERM
    set_vt_graphics (screen);
#endif /* KTERM */
}

set_vt_box (screen)
	TScreen *screen;
{
	XPoint	*vp;

#ifdef KTERM
	vp = &VTbox[1];
	(vp++)->x = FontWidth(screen) - 1;
	(vp++)->y = screen->fullVwin.f_height - 1;
	(vp++)->x = -(FontWidth(screen) - 1);
	vp->y = -(screen->fullVwin.f_height - 1);
	screen->_box[F_ISO8859_1] = VTbox;
	screen->_box[F_JISX0201_0] = VTbox;
# ifdef KTERM_MBCS
	vp = &VTwbox[1];
	(vp++)->x = FontWidth(screen) * 2 - 1;
	(vp++)->y = screen->fullVwin.f_height - 1;
	(vp++)->x = -(FontWidth(screen) * 2 - 1);
	vp->y = -(screen->fullVwin.f_height - 1);
	screen->_box[F_JISX0208_0] = VTwbox;
# endif /* KTERM_MBCS */
#else /* !KTERM */
	vp = &VTbox[1];
	(vp++)->x = FontWidth(screen) - 1;
	(vp++)->y = FontHeight(screen) - 1;
	(vp++)->x = -(FontWidth(screen) - 1);
	vp->y = -(FontHeight(screen) - 1);
	screen->box = VTbox;
#endif /* !KTERM */
}


#ifdef KTERM
set_vt_graphics (screen)
	TScreen *screen;
{
	static GC bmgc;
	static Pixmap gray;
	static Pixmap vtgraphics[256]; /* Bitmaps */
	XPoint pts[4];
	Display *dpy = screen->display;
	Window win = RootWindowOfScreen(XtScreen(term));
	int W = FontWidth(screen), H = FontHeight(screen);
	int w = W - 1, h = H - 1;
	int w2 = w/2, h2 = h/2;
	int i;

	if (!gray) {
		/*
		static char gray_bits[] = { 0x08, 0x02, 0x04, 0x01 };
		gray = XCreateBitmapFromData(dpy, win, gray_bits, 4, 4);
		*/
		static char gray_bits[] = { 0x11, 0x44 };
		gray = XCreateBitmapFromData(dpy, win, gray_bits, 8, 2);
	}
	if (!bmgc) {
		bmgc = XCreateGC(dpy, gray, 0, NULL);
	}

	for (i = 0; i < 256; i ++) {
		if (vtgraphics[i]) {
			XFreePixmap(dpy, vtgraphics[i]);
			vtgraphics[i] = 0;
		}
	}

	vtgraphics['`'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['a'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['j'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['k'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['l'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['m'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['n'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['o'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['p'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['q'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['r'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['s'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['t'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['u'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['v'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['w'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['x'] = XCreatePixmap(dpy, win, W, H, 1);
	vtgraphics['~'] = XCreatePixmap(dpy, win, W, H, 1);

	XSetForeground(dpy, bmgc, 0);
	XSetFillStyle(dpy, bmgc, FillSolid);
	for (i = 0; i < 256; i ++) {
		if (vtgraphics[i]) {
			XFillRectangle(dpy, vtgraphics[i], bmgc, 0, 0, W, H);
		}
	}

	XSetForeground(dpy, bmgc, 1);

	pts[0].x = w2;   pts[0].y = 0;
	pts[1].x = 0;    pts[1].y = h2;
	pts[2].x = w2;   pts[2].y = h2*2;
	pts[3].x = w2*2; pts[3].y = h2;
	XFillPolygon(dpy, vtgraphics['`'], bmgc, pts, 4, Nonconvex, CoordModeOrigin);

	XSetFillStyle(dpy, bmgc, FillStippled);
	XSetStipple(dpy, bmgc, gray);
	XFillRectangle(dpy, vtgraphics['a'], bmgc, 0, 0, W, H);

	XSetFillStyle(dpy, bmgc, FillSolid);
	XDrawLine(dpy, vtgraphics['j'], bmgc, 0, h2, w2, h2);
	XDrawLine(dpy, vtgraphics['j'], bmgc, w2, 0, w2, h2);

	XDrawLine(dpy, vtgraphics['k'], bmgc, 0, h2, w2, h2);
	XDrawLine(dpy, vtgraphics['k'], bmgc, w2, h2, w2, h);

	XDrawLine(dpy, vtgraphics['l'], bmgc, w2, h2, w, h2);
	XDrawLine(dpy, vtgraphics['l'], bmgc, w2, h2, w2, h);

	XDrawLine(dpy, vtgraphics['m'], bmgc, w2, h2, w, h2);
	XDrawLine(dpy, vtgraphics['m'], bmgc, w2, 0, w2, h2);

	XDrawLine(dpy, vtgraphics['n'], bmgc, 0, h2, w, h2);
	XDrawLine(dpy, vtgraphics['n'], bmgc, w2, 0, w2, h);

	XDrawLine(dpy, vtgraphics['o'], bmgc, 0, 0, w, 0);

	XDrawLine(dpy, vtgraphics['p'], bmgc, 0, h/4, w, h/4);

	XDrawLine(dpy, vtgraphics['q'], bmgc, 0, h2, w, h2);

	XDrawLine(dpy, vtgraphics['r'], bmgc, 0, h*3/4, w, h*3/4);

	XDrawLine(dpy, vtgraphics['s'], bmgc, 0, h, w, h);

	XDrawLine(dpy, vtgraphics['t'], bmgc, w2, h2, w, h2);
	XDrawLine(dpy, vtgraphics['t'], bmgc, w2, 0, w2, h);

	XDrawLine(dpy, vtgraphics['u'], bmgc, 0, h2, w2, h2);
	XDrawLine(dpy, vtgraphics['u'], bmgc, w2, 0, w2, h);

	XDrawLine(dpy, vtgraphics['v'], bmgc, 0, h2, w, h2);
	XDrawLine(dpy, vtgraphics['v'], bmgc, w2, 0, w2, h2);

	XDrawLine(dpy, vtgraphics['w'], bmgc, 0, h2, w, h2);
	XDrawLine(dpy, vtgraphics['w'], bmgc, w2, h2, w2, h);

	XDrawLine(dpy, vtgraphics['x'], bmgc, w2, 0, w2, h);

	XDrawLine(dpy, vtgraphics['~'], bmgc, w2-1, h2, w2+1, h2);
	XDrawLine(dpy, vtgraphics['~'], bmgc, w2, h2-1, w2, h2+1);

	screen->graphics = vtgraphics;
}
#endif /* KTERM */


#ifdef KTERM
set_cursor_gcs (screen, fnum)
    int fnum;
#else /* !KTERM */
set_cursor_gcs (screen)
#endif /* !KTERM */
    TScreen *screen;
{
    XGCValues xgcv;
    unsigned long mask;
    unsigned long cc = screen->cursorcolor;
    unsigned long fg = screen->foreground;
    unsigned long bg = term->core.background_pixel;
    GC new_cursorGC = NULL, new_reversecursorGC = NULL;
    GC new_cursoroutlineGC = NULL;

    /*
     * Let's see, there are three things that have "color":
     *
     *     background
     *     text
     *     cursorblock
     *
     * And, there are four situation when drawing a cursor, if we decide
     * that we like have a solid block of cursor color with the letter
     * that it is highlighting shown in the background color to make it
     * stand out:
     *
     *     selected window, normal video - background on cursor
     *     selected window, reverse video - foreground on cursor
     *     unselected window, normal video - foreground on background
     *     unselected window, reverse video - background on foreground
     *
     * Since the last two are really just normalGC and reverseGC, we only
     * need two new GC's.  Under monochrome, we get the same effect as
     * above by setting cursor color to foreground.
     */

    xgcv.font = screen->fnt_norm->fid;
    mask = (GCForeground | GCBackground | GCFont);
    if (cc != fg && cc != bg) {
	/* we have a colored cursor */
	xgcv.foreground = fg;
	xgcv.background = cc;
	new_cursorGC = XtGetGC ((Widget) term, mask, &xgcv);

	if (screen->always_highlight) {
	    new_reversecursorGC = (GC) 0;
	    new_cursoroutlineGC = (GC) 0;
	} else {
	    xgcv.foreground = bg;
	    xgcv.background = cc;
	    new_reversecursorGC = XtGetGC ((Widget) term, mask, &xgcv);
	    xgcv.foreground = cc;
	    xgcv.background = bg;
	    new_cursoroutlineGC = XtGetGC ((Widget) term, mask, &xgcv);
		}
    } else {
	new_cursorGC = (GC) 0;
	new_reversecursorGC = (GC) 0;
	new_cursoroutlineGC = (GC) 0;
    }
    if (screen->cursorGC) XtReleaseGC ((Widget)term, screen->cursorGC);
    if (screen->reversecursorGC)
	XtReleaseGC ((Widget)term, screen->reversecursorGC);
    if (screen->cursoroutlineGC)
	XtReleaseGC ((Widget)term, screen->cursoroutlineGC);
    screen->cursorGC = new_cursorGC;
    screen->reversecursorGC = new_reversecursorGC;
    screen->cursoroutlineGC = new_cursoroutlineGC;
}

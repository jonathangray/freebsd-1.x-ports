#ifndef UNIX
#ifndef MACINTOSH
#define MACINTOSH
#endif
#endif

#ifndef _IVIEWWINDOW_
#define WindowPtr long
#define Window long
#define MenuHandle long
#define Menu long
#else
#ifdef MACINTOSH
#ifdef MPWC
#include <Quickdraw.h>
#include <Windows.h>
#include <Menus.h>
#include <ToolUtils.h>
#include <Events.h>
#include <Controls.h>
#include <Fonts.h>
#else
#include <QuickDraw.h>
#include <WindowMgr.h>
#include <MenuMgr.h>
#include <ToolboxUtil.h>
#include <EventMgr.h>
#include <ControlMgr.h>
#include <FontMgr.h>
#endif MPWC
#endif MACINTOSH
#endif _IVIEWWINDOW_

#ifdef MACINTOSH
#define IVIEW_WINDOW WindowPtr
#define IVIEW_MENU MenuHandle
#endif MACINTOSH
#ifdef UNIX
#define IVIEW_WINDOW Window
#define IVIEW_MENU Menu
#endif UNIX

#define nil 0L

#define BASIC_IVIEW 0

typedef int ColorCode;

typedef enum {
  MouseClick = 0,
  MouseMove = 1
} MouseEventType;

typedef enum {
  NoModifiers = 0,
  ExtendModifier = 1,
  OptionModifier = 2,
  OptionExtendModifier = 3
} MouseClickModifier;

typedef enum {
  pointInvisible,
  pointNormal,
  pointHilited,
  pointSelected
} PointState;

typedef enum {
  selecting,
  brushing,
  usermode
} MouseMode;

extern char *StCalloc();
extern char *StGWObWinInfo();
extern long StGWGetRefCon();
extern long StGWGetColRefCon();
extern long StGWGetCursRefCon();
extern long StGWGetSymRefCon();

extern IVIEW_WINDOW IViewWindowNew();
extern IVIEW_MENU IViewWindowGetMenu();
extern ColorCode StGWDrawColor();
extern ColorCode StGWBackColor();
extern long IViewWindowGetObject();
extern long IViewGetRefCon();
char *IViewWindowWinInfo();

extern IVIEW_WINDOW IViewNew();
extern IVIEW_WINDOW IViewSpinNew();
extern IVIEW_WINDOW IViewScatmatNew();
extern IVIEW_WINDOW IViewListNew();
extern IVIEW_WINDOW IViewHistNew();

#define ARROW_CURSOR      0
#define WATCH_CURSOR      1
#define CROSS_CURSOR      2
#define BRUSH_CURSOR      3
#define HAND_CURSOR       4
#define FINGER_CURSOR     5
#define HOUR_GLASS_CURSOR 6
#define TRASH_BAG_CURSOR  7
#define TRASH_CAN_CURSOR  8

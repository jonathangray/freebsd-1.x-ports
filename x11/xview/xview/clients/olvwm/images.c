/*
 * (c) Copyright 1991 Scott Oaks.  See LEGAL_NOTICE file for terms of the
 * license
 */

#ident "@(#)images.c	1.2 olvwm version 6/13/92"

/* Use small images */
#define SMALL_IMAGES

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <olgx/olgx.h>
#include <stdio.h>

#include "i18n.h"
#include "olwm.h"
#include "ollocale.h"
#include "globals.h"
#include "win.h"
#include "menu.h"

/*
 *      Frame/icon menu action procs
 */
extern int  WindowOpenCloseAction(), WindowFullRestoreSizeAction();
extern int  WindowMoveAction(), WindowResizeAction();
extern int  WindowPropsAction(), WindowBackAction(), WindowRefreshAction();
extern int  WindowQuitAction(), WindowDismissThisAction();
extern int  WindowDismissAllAction(), WindowFlashOwnerAction();
extern int  WindowStickAction();

/*
 *      Buttons used to build the frame and icon menus
 *      REMIND: right now, toggles always use the same actions!
 *
 *  These buttons are more or less "prototype" buttons; if the UseImages
 *  resource is set, their pixlabels may be adjusted
 */

static Button
openButton = {
    { { StringLabel, "Open", NULL },
      { StringLabel, "Close", NULL } },
    {"window:Open", "window:Close"},
    0,
    False,
    True,
    True,
    {
        WindowOpenCloseAction,
        NULL
    },
    NULL,
};

static Button
fullSizeButton = {
    { { StringLabel, "Full Size", NULL },
      { StringLabel, "Restore Size", NULL } },
    {"window:FullSize", "window:RestoreSize"},
    0,
    False,
    True,
    True,
    {
        WindowFullRestoreSizeAction,
        NULL
    },
    NULL,
};

static Button
moveButton = {
    { { StringLabel, "Move", NULL },
      { NoType, NULL, NULL } },
    {"window:Move", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowMoveAction,
        NULL
    },
    NULL,
};

static Button
resizeButton = {
    { { StringLabel, "Resize", NULL },
      { NoType, NULL, NULL } },
    {"window:Resize", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowResizeAction,
        NULL
    },
    NULL,
};

static Button
stickyButton = {
    { { StringLabel, "Stick", NULL },
      { StringLabel, "Unstick", NULL } },
    {"window:Stick", "window:Unstick"},
    0,
    False,
    True,
    True,
    {    
        WindowStickAction,
        NULL
    },
    NULL,
};

static Button
propertiesButton = {
    { { StringLabel, "Properties", NULL },
      { NoType, NULL, NULL } },
    {"window:Properties", NULL},
    0,
    False,
    False,
    True,
    {    
        WindowPropsAction,
        NULL
    },
    NULL,
};

static Button
backButton = {
    { { StringLabel, "Back", NULL },    /****WINDOW FRONT?*/
      { NoType, NULL, NULL } },
    {"window:Back", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowBackAction,
        NULL
    },
    NULL,
};

static Button
refreshButton = {
    { { StringLabel, "Refresh", NULL },
      { NoType, NULL, NULL } },
    {"window:Refresh", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowRefreshAction,
        NULL
    },
    NULL,
};

static Button
quitButton = {
    { { StringLabel, "Quit", NULL },
      { NoType, NULL, NULL } },
    {"window:Quit", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowQuitAction,
        NULL
    },
    NULL,
};

static Button
dismissButton = {
    { { StringLabel, "Dismiss", NULL },
      { NoType, NULL, NULL } },
    {"window:Dismiss", NULL},
    0,
    False,
    True,
    True,
    {    
        NULL,
        NULL,
    },
    NULL,
};

static Button
dismissThisButton = {
    { { StringLabel, "This Window", NULL},
      { NoType, NULL, NULL } },
    {"window:DismissThis", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowDismissThisAction,
        NULL
    },
    NULL,
};

static Button
dismissAllButton = {
    { { StringLabel, "All Pop-ups", NULL},
      { NoType, NULL, NULL } },
    {"window:DismissAll", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowDismissAllAction,
        NULL
    },
    NULL,
};

static Button
ownerButton = {
    { { StringLabel, "Owner?", NULL},
      { NoType, NULL, NULL } },
    {"window:Owner", NULL},
    0,
    False,
    True,
    True,
    {    
        WindowFlashOwnerAction,
        NULL
    },
    NULL,
};

extern int	VDMMenuAction();
/*
 * Menu buttons for VDM motion menu
 */
Button moveRightButton = {
    { { StringLabel, "Move East", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};
 
Button moveLeftButton = {
    { { StringLabel, "Move West", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};
 
Button moveUpButton = {
    { { StringLabel, "Move North", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveDownButton = {
    { { StringLabel, "Move South", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveUpRightButton = {
    { { StringLabel, "Move NorthEast", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveDownRightButton = {
    { { StringLabel, "Move SouthEast", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveUpLeftButton = {
    { { StringLabel, "Move NorthWest", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveDownLeftButton = {
    { { StringLabel, "Move SouthWest", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

Button moveHomeButton = {
    { { StringLabel, "Move Home", NULL },
      { NoType, NULL, NULL } },
    { "virtual:Move", NULL },
    0,
    False,
    True,
    True,
    { VDMMenuAction, NULL },
    NULL,
};

#ifdef SMALL_IMAGES 
/*
 * Images for Frame Menus and VDM Menus
 */

#define Close_width 16
#define Close_height 14
static char     Close_bits[] = {
    0xc0, 0x3f, 0x40, 0x20, 0x40, 0x20, 0x40, 0xa1, 0xc0, 0x60, 0x64, 0xa0,
    0x5c, 0x60, 0x5c, 0xa0, 0xff, 0x7f, 0x05, 0xaa, 0x07, 0x55, 0x14, 0xaa,
    0x08, 0x00, 0x14, 0x00,
};

#define Open_width 16
#define Open_height 14
static char     Open_bits[] = {
    0xc0, 0x3f, 0x40, 0x20, 0xc0, 0x27, 0x40, 0xa7, 0x40, 0x67, 0xc0, 0xa4,
    0x60, 0x60, 0x50, 0xa0, 0xcf, 0x7f, 0x05, 0xaa, 0x07, 0x55, 0x14, 0xaa,
    0x08, 0x00, 0x14, 0x00,
};

#define FullSize_width 16
#define FullSize_height 14
static char     FullSize_bits[] = {
    0xf8, 0x7, 0x8, 0x4, 0x8, 0x4, 0x8, 0x14, 0x8, 0xc, 0x8, 0x14, 0xc8, 0xc,
    0xc8, 0x14, 0xc8, 0xc, 0xf8, 0x17, 0xe8, 0xd, 0xc8, 0x14, 0xf8, 0xf, 0x40,
    0x15,
};

#define NormalSize_width 16
#define NormalSize_height 14
static char     NormalSize_bits[] = {
    0xf8, 0x7, 0x8, 0x4, 0x8, 0x4, 0x8, 0x14, 0x8, 0xc, 0x8, 0x14, 0x8, 0xc,
    0x8, 0x14, 0xf8, 0xf, 0xc0, 0x14, 0xe0, 0xb, 0xf0, 0x17, 0xc0, 0x0, 0xc0, 0x0,
};

#define Props_width 16
#define Props_height 14
static char     Props_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0xfe, 0x1f, 0x02, 0x10, 0xfa, 0x57, 0x02, 0x30,
    0xda, 0x56, 0x02, 0x30, 0x02, 0x50, 0xfe, 0x3f, 0x50, 0x55, 0xa8, 0x2a,
    0x50, 0x55, 0x00, 0x00,
};

#define Back_width 16
#define Back_height 14
static char     Back_bits[] = {
    0xff, 0x00, 0x81, 0x0f, 0x81, 0x3f, 0x81, 0x72, 0x81, 0xc2, 0x81, 0xaa,
    0x81, 0xb6, 0x81, 0xfa, 0xff, 0x7e, 0x04, 0x3a, 0xfc, 0x37, 0xa0, 0x2a,
    0x40, 0x05, 0xa0, 0x0a,
};

#define Refresh_width 16
#define Refresh_height 14
static char     Refresh_bits[] = {
    0x00, 0x00, 0xf8, 0x07, 0x48, 0x05, 0xc8, 0x07, 0x08, 0x14, 0x08, 0x0c,
    0x48, 0x14, 0x28, 0x0d, 0x08, 0x14, 0xf8, 0x0f, 0x40, 0x15, 0xa0, 0x0a,
    0x40, 0x15, 0x00, 0x00,
};

#define Stick_width 16
#define Stick_height 14
static char     Stick_bits[] = {
    0xb0, 0x0f, 0xf0, 0x1f, 0xb0, 0x23, 0x40, 0x29, 0x80, 0x57, 0x87, 0x8b,
    0x82, 0x07, 0x82, 0x0b, 0x96, 0x07, 0x8a, 0x0b, 0x82, 0x07, 0x8a, 0x0b,
    0x00, 0x04, 0x08, 0x0a,
};

#define UnStick_width 16
#define UnStick_height 14
static char     UnStick_bits[] = {
    0x04, 0x00, 0x02, 0x00, 0x03, 0x00, 0x13, 0x00, 0xff, 0x1f, 0xff, 0x1f,
    0xff, 0x1f, 0x56, 0x55, 0xaf, 0x2a, 0x57, 0x55, 0x08, 0x00, 0x17, 0x00,
    0x0a, 0x00, 0x02, 0x00,
};

#define Quit_width 16
#define Quit_height 14
static char     Quit_bits[] = {
    0x10, 0x04, 0x38, 0x0e, 0x7c, 0x1f, 0xf8, 0x0f, 0xf0, 0x17, 0xe8, 0x2b,
    0xf0, 0x17, 0xf8, 0x0f, 0x7c, 0x1f, 0xb8, 0x0e, 0x50, 0x15, 0xa8, 0x2a,
    0x50, 0x14, 0x20, 0x08,
};

#define Resize_width 16
#define Resize_height 14
static char Resize_bits[] = {
   0x2e, 0x00, 0x4a, 0x01, 0x8e, 0x01, 0xc0, 0x01, 0x02, 0x00, 0x94, 0xff,
   0x98, 0x80, 0x9c, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
   0x80, 0x80, 0x80, 0xff};

#define Move_width 16
#define Move_height 14
static char Move_bits[] = {
   0xfe, 0x00, 0x82, 0x00, 0x82, 0x02, 0x82, 0x24, 0x82, 0x28, 0xfe, 0x30,
   0x00, 0x3c, 0x04, 0x00, 0x48, 0x7f, 0x50, 0x41, 0x60, 0x41, 0x78, 0x41,
   0x00, 0x41, 0x00, 0x7f};

#else SMALL_IMAGES

#define Back_width 32
#define Back_height 32
static char Back_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0xff, 0x00, 0x00,
   0x14, 0x80, 0x00, 0x00, 0xfc, 0xff, 0x00, 0x00, 0x04, 0x80, 0xff, 0x01,
   0x04, 0x80, 0xff, 0x07, 0x04, 0x80, 0xff, 0x0f, 0x04, 0x80, 0xff, 0x0f,
   0x04, 0x80, 0x58, 0x1f, 0x04, 0x80, 0xa9, 0x1e, 0x04, 0x80, 0x58, 0x1c,
   0x04, 0x80, 0xa9, 0x18, 0x04, 0x80, 0x58, 0x19, 0x04, 0x80, 0xa9, 0x19,
   0x04, 0x80, 0xd8, 0x0d, 0x04, 0x80, 0xe9, 0x0f, 0xfc, 0xff, 0xf8, 0x07,
   0x40, 0x55, 0xe9, 0x03, 0x40, 0x00, 0xd8, 0x01, 0xc0, 0xff, 0xaf, 0x01,
   0x00, 0x54, 0x55, 0x01, 0x00, 0xa8, 0xaa, 0x00, 0x00, 0x54, 0x55, 0x00,
   0x00, 0xa8, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define Close_width 32
#define Close_height 32
static char Close_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xff, 0x0f, 0x00, 0x40, 0x01, 0x08,
   0x00, 0xc0, 0xff, 0x0f, 0x00, 0x40, 0x00, 0x08, 0x00, 0x40, 0x04, 0x58,
   0x00, 0x40, 0x0e, 0xa8, 0x00, 0x40, 0x07, 0x58, 0x00, 0xc0, 0x03, 0xa8,
   0x00, 0xc0, 0x01, 0x58, 0x00, 0xe0, 0x00, 0xa8, 0x00, 0x70, 0x00, 0x58,
   0x40, 0x78, 0x00, 0xa8, 0xc0, 0x5c, 0x00, 0x58, 0xc0, 0x4f, 0x00, 0xa8,
   0xc0, 0x47, 0x00, 0x58, 0xc0, 0xc7, 0xff, 0xaf, 0xc0, 0x0f, 0x54, 0x55,
   0xc0, 0x1f, 0xa8, 0xaa, 0x3f, 0x00, 0x54, 0x55, 0x21, 0x00, 0xa8, 0xaa,
   0x21, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x61, 0x01, 0x00, 0x00,
   0xbf, 0x02, 0x00, 0x00, 0x50, 0x01, 0x00, 0x00, 0xa0, 0x02, 0x00, 0x00,
   0x50, 0x01, 0x00, 0x00, 0xa0, 0x02, 0x00, 0x00
};

#define FullSize_width 32
#define FullSize_height 32
static char FullSize_bits[] = {
   0x00, 0xfc, 0xff, 0x00, 0x00, 0x14, 0x80, 0x00, 0x00, 0xfc, 0xff, 0x00,
   0x00, 0x04, 0x80, 0x00, 0x00, 0x04, 0x80, 0x05, 0xc0, 0xff, 0x8f, 0x0a,
   0x40, 0x01, 0x88, 0x05, 0xc0, 0xff, 0x9f, 0x0a, 0x40, 0x00, 0x88, 0x05,
   0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a,
   0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05,
   0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a,
   0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a, 0xc0, 0xff, 0x8f, 0x05,
   0x00, 0x54, 0x95, 0x0a, 0x00, 0xc4, 0x81, 0x05, 0x00, 0xc4, 0x81, 0x0a,
   0x00, 0xc4, 0x81, 0x05, 0x00, 0xc4, 0x81, 0x0a, 0x00, 0xc4, 0x81, 0x05,
   0x00, 0xf4, 0x87, 0x0a, 0x00, 0xe4, 0x83, 0x05, 0x00, 0xc4, 0x81, 0x0a,
   0x00, 0x84, 0x80, 0x05, 0x00, 0xfc, 0xff, 0x0a
};

#define Move_width 32
#define Move_height 32
static char Move_bits[] = {
   0x00, 0x00, 0xfc, 0xff, 0x00, 0x00, 0x14, 0x80, 0x00, 0x00, 0xfc, 0xff,
   0x00, 0x00, 0x04, 0x80, 0x00, 0x00, 0x04, 0x80, 0x00, 0x00, 0xfe, 0x80,
   0x00, 0x00, 0xfc, 0x80, 0x00, 0x00, 0xf4, 0x80, 0x00, 0x00, 0xfc, 0x80,
   0x00, 0x00, 0xfc, 0x80, 0x00, 0x00, 0xce, 0x80, 0x00, 0x00, 0x87, 0x80,
   0x00, 0x80, 0x07, 0x80, 0x00, 0xc0, 0x05, 0x80, 0x00, 0xe0, 0x04, 0x80,
   0x00, 0x70, 0xfc, 0xff, 0xff, 0x3f, 0x00, 0x00, 0x05, 0x3c, 0x00, 0x00,
   0xff, 0x3f, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00,
   0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00,
   0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00,
   0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00, 0x01, 0x20, 0x00, 0x00,
   0x01, 0x20, 0x00, 0x00, 0xff, 0x3f, 0x00, 0x00
};

#define NormalSize_width 32
#define NormalSize_height 32
static char NormalSize_bits[] = {
   0x00, 0xfc, 0xff, 0x00, 0x00, 0x14, 0x80, 0x00, 0x00, 0xfc, 0xff, 0x00,
   0x00, 0x04, 0x80, 0x00, 0x00, 0x04, 0x80, 0x05, 0xc0, 0xff, 0x8f, 0x0a,
   0x40, 0x01, 0x88, 0x05, 0xc0, 0xff, 0x9f, 0x0a, 0x40, 0x00, 0x88, 0x05,
   0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a,
   0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05,
   0x40, 0x00, 0x98, 0x0a, 0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a,
   0x40, 0x00, 0x88, 0x05, 0x40, 0x00, 0x98, 0x0a, 0xc0, 0xff, 0x8f, 0x05,
   0x00, 0x54, 0x95, 0x0a, 0x00, 0x84, 0x80, 0x05, 0x00, 0xc4, 0x81, 0x0a,
   0x00, 0xe4, 0x83, 0x05, 0x00, 0xf4, 0x87, 0x0a, 0x00, 0xc4, 0x81, 0x05,
   0x00, 0xc4, 0x81, 0x0a, 0x00, 0xc4, 0x81, 0x05, 0x00, 0xc4, 0x81, 0x0a,
   0x00, 0xc4, 0x81, 0x05, 0x00, 0xfc, 0xff, 0x0a
};

#define Open_width 32
#define Open_height 32
static char Open_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xff, 0x0f, 0x00, 0x40, 0x01, 0x08,
   0x00, 0xc0, 0xff, 0x0f, 0x00, 0x40, 0x00, 0x08, 0x00, 0x40, 0x00, 0x58,
   0x00, 0xe0, 0x0f, 0xa8, 0x00, 0xc0, 0x0f, 0x58, 0x00, 0x40, 0x0f, 0xa8,
   0x00, 0xc0, 0x0f, 0x58, 0x00, 0xc0, 0x0f, 0xa8, 0x00, 0xe0, 0x0c, 0x58,
   0x00, 0x70, 0x08, 0xa8, 0x00, 0x78, 0x00, 0x58, 0x00, 0x5c, 0x00, 0xa8,
   0x00, 0x4e, 0x00, 0x58, 0x00, 0xc7, 0xff, 0xaf, 0x80, 0x03, 0x54, 0x55,
   0xc0, 0x01, 0xa8, 0xaa, 0xbf, 0x00, 0x54, 0x55, 0x21, 0x00, 0xa8, 0xaa,
   0x21, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x61, 0x01, 0x00, 0x00,
   0xbf, 0x02, 0x00, 0x00, 0x50, 0x01, 0x00, 0x00, 0xa0, 0x02, 0x00, 0x00,
   0x50, 0x01, 0x00, 0x00, 0xa0, 0x02, 0x00, 0x00
};

#define Props_width 32
#define Props_height 32
static char Props_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xc0, 0xff, 0x3f, 0x00, 0x40, 0x02, 0x20, 0x00, 0x40, 0x53, 0x25, 0x00,
   0x40, 0x00, 0x20, 0x00, 0x40, 0x00, 0x20, 0x00, 0x40, 0x00, 0xa0, 0x02,
   0x40, 0x04, 0x25, 0x00, 0x40, 0x0a, 0xa2, 0x02, 0x40, 0x00, 0x20, 0x00,
   0xc0, 0xff, 0xbf, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa8, 0xaa, 0x02,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xa8, 0xaa, 0x02, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define Quit_width 32
#define Quit_height 32
static char Quit_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0xff, 0x07, 0x00,
   0xc0, 0x03, 0x1e, 0x00, 0xe0, 0x00, 0x38, 0x00, 0x70, 0x00, 0x70, 0x00,
   0xb8, 0xff, 0xff, 0x00, 0x98, 0x02, 0xdc, 0x00, 0x8c, 0xff, 0x9f, 0x01,
   0x8c, 0x00, 0x97, 0x01, 0x86, 0x80, 0xb3, 0x03, 0x86, 0xc0, 0x51, 0x03,
   0x86, 0xe0, 0xb0, 0x03, 0x86, 0x70, 0x50, 0x03, 0x86, 0x38, 0xb0, 0x03,
   0x86, 0x1c, 0x50, 0x03, 0x86, 0x0e, 0xb0, 0x03, 0x8c, 0x07, 0xd0, 0x01,
   0x8c, 0x03, 0xb0, 0x01, 0xd8, 0x01, 0xd0, 0x01, 0xf8, 0x00, 0xf0, 0x00,
   0xf0, 0xff, 0x7f, 0x01, 0xe0, 0xa8, 0xba, 0x00, 0xc0, 0x53, 0x5f, 0x01,
   0x00, 0xff, 0xaf, 0x00, 0x00, 0xfc, 0x55, 0x01, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define Refresh_width 32
#define Refresh_height 32
static char Refresh_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x80, 0xff, 0x1f, 0x00, 0x80, 0x02, 0x10, 0x00, 0x80, 0xff, 0x18, 0x00,
   0x80, 0x00, 0x10, 0x00, 0x80, 0x00, 0xb0, 0x00, 0x80, 0x00, 0x50, 0x01,
   0x80, 0xa0, 0xb6, 0x00, 0x80, 0xc8, 0x50, 0x01, 0x80, 0x00, 0xb2, 0x00,
   0x80, 0x00, 0x50, 0x01, 0x80, 0x00, 0xb0, 0x00, 0x80, 0x7f, 0x50, 0x01,
   0x80, 0x55, 0xb0, 0x00, 0x80, 0x7f, 0x56, 0x01, 0x80, 0x55, 0xb0, 0x00,
   0x80, 0xff, 0x5f, 0x01, 0x00, 0xa8, 0xaa, 0x00, 0x00, 0x50, 0x55, 0x01,
   0x00, 0xa8, 0xaa, 0x00, 0x00, 0x50, 0x55, 0x01, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define Resize_width 32
#define Resize_height 32
static char Resize_bits[] = {
   0xff, 0x03, 0x00, 0x00, 0x05, 0x02, 0x00, 0x00, 0xff, 0x03, 0x00, 0x00,
   0x01, 0x02, 0x00, 0x00, 0x01, 0x02, 0x00, 0x00, 0x01, 0x02, 0x00, 0x00,
   0x41, 0x02, 0x00, 0x00, 0xe1, 0x02, 0x00, 0x00, 0xc1, 0x03, 0x00, 0x00,
   0xff, 0x03, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00,
   0x00, 0x1c, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0xf0, 0xff, 0xff,
   0x00, 0xe0, 0x01, 0x80, 0x00, 0xc0, 0xff, 0xff, 0x00, 0xc0, 0x33, 0x80,
   0x00, 0x40, 0x3f, 0x80, 0x00, 0x40, 0x3e, 0x80, 0x00, 0x40, 0x3e, 0x80,
   0x00, 0x40, 0x3f, 0x80, 0x00, 0xc0, 0x3f, 0x80, 0x00, 0x40, 0x00, 0x80,
   0x00, 0x40, 0x00, 0x80, 0x00, 0x40, 0x00, 0x80, 0x00, 0x40, 0x00, 0x80,
   0x00, 0x40, 0x00, 0x80, 0x00, 0x40, 0x00, 0x80, 0x00, 0x40, 0x00, 0x80,
   0x00, 0x40, 0x00, 0x80, 0x00, 0xc0, 0xff, 0xff
};

#define Stick_width 32
#define Stick_height 32
static char Stick_bits[] = {
   0x00, 0x0e, 0xff, 0x00, 0x00, 0x9e, 0xff, 0x03, 0x00, 0xfe, 0xff, 0x07,
   0x00, 0xfe, 0xff, 0x0f, 0x00, 0x9e, 0x5f, 0x0d, 0x00, 0x6e, 0xaf, 0x3a,
   0x00, 0xd0, 0x56, 0x51, 0x00, 0xa0, 0xae, 0x20, 0x00, 0x50, 0x5f, 0x40,
   0x00, 0x00, 0x2f, 0x80, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x80, 0x3f, 0x00,
   0x00, 0x80, 0x5f, 0x00, 0x00, 0x80, 0xbf, 0x00, 0x00, 0x80, 0x5f, 0x00,
   0x00, 0x80, 0xbf, 0x00, 0x7e, 0x80, 0x5f, 0x00, 0x7e, 0x80, 0xbf, 0x00,
   0x18, 0x80, 0x5f, 0x00, 0xb8, 0x82, 0xbf, 0x00, 0x58, 0x81, 0x5f, 0x00,
   0x98, 0x80, 0xbf, 0x00, 0x58, 0x80, 0x5f, 0x00, 0x98, 0x80, 0xbf, 0x00,
   0x58, 0x80, 0x5f, 0x00, 0x98, 0x80, 0xbf, 0x00, 0x58, 0x80, 0x5f, 0x00,
   0x98, 0x00, 0xaf, 0x00, 0x40, 0x00, 0x54, 0x00, 0x80, 0x00, 0xa8, 0x00,
   0x40, 0x00, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define UnStick_width 32
#define UnStick_height 32
static char UnStick_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00,
   0x38, 0x00, 0x00, 0x00, 0x1c, 0x04, 0x00, 0x00, 0x9c, 0x02, 0x00, 0x00,
   0x5e, 0x01, 0x00, 0x00, 0xbe, 0x00, 0x00, 0x00, 0x5e, 0x00, 0x00, 0x00,
   0xbe, 0xf8, 0xff, 0x0f, 0x7e, 0xfe, 0xff, 0x1f, 0xfe, 0xff, 0xff, 0x1f,
   0xfe, 0xff, 0xff, 0x5f, 0x7e, 0xfe, 0xff, 0xbf, 0xbc, 0xfd, 0xff, 0x5f,
   0xd8, 0xaa, 0xaa, 0xaa, 0x58, 0x51, 0x55, 0x55, 0xbc, 0x80, 0xaa, 0x2a,
   0x7e, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x7e, 0x01, 0x00, 0x00,
   0xa0, 0x02, 0x00, 0x00, 0x7e, 0x01, 0x00, 0x00, 0xfe, 0x02, 0x00, 0x00,
   0x18, 0x00, 0x00, 0x00, 0xb8, 0x02, 0x00, 0x00, 0x58, 0x01, 0x00, 0x00,
   0x98, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00,
   0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
#endif SMALL_IMAGES

/*
 *	Images for Virtual Window Menu
 */
#define ArwUpLeft_width 16
#define ArwUpLeft_height 14
static char ArwUpLeft_bits[] = {
0xfe,0x07, 0x02,0x02, 0x02,0x01, 0x82,0x00, 0x02,0x01, 0x02,0x02, 0x12,0x04,
0x2a,0x08, 0x46,0x10, 0x82,0x20, 0x00,0x11, 0x00,0x0a, 0x00,0x04, 0x00,0x00,
};

#define ArwLeft_width 16
#define ArwLeft_height 14
static char ArwLeft_bits[] = {
0x80,0x00, 0xc0,0x00, 0xa0,0x00, 0x90,0x00, 0x88,0x7f, 0x04,0x40, 0x02,0x40,
0x04,0x40, 0x88,0x7f, 0x90,0x00, 0xa0,0x00, 0xc0,0x00, 0x80,0x00, 0x00,0x00,
};

#define ArwDownLeft_width 16
#define ArwDownLeft_height 14
static char ArwDownLeft_bits[] = {
0x00,0x00, 0x00,0x04, 0x00,0x0a, 0x00,0x11, 0x82,0x20, 0x46,0x10, 0x2a,0x08,
0x12,0x04, 0x02,0x02, 0x02,0x01, 0x82,0x00, 0x02,0x01, 0x02,0x02, 0xfe,0x07,
};

#define ArwUp_width 16
#define ArwUp_height 14
static char ArwUp_bits[] = {
0x80,0x00, 0x40,0x01, 0x20,0x02, 0x10,0x04, 0x08,0x08, 0x04,0x10, 0x3e,0x3e,
0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0xe0,0x03,
};

#define ArwHome_width 16
#define ArwHome_height 14
static char ArwHome_bits[] = {
0x80,0x00, 0x40,0x0d, 0x20,0x0e, 0x10,0x0c, 0x08,0x08, 0x04,0x10, 0xfe,0x3f,
0x04,0x10, 0x74,0x17, 0x54,0x15, 0x54,0x17, 0x54,0x10, 0xfc,0x1f, 0x00,0x00,
};

#define ArwDown_width 16
#define ArwDown_height 14
static char ArwDown_bits[] = {
0xe0,0x03, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02, 0x20,0x02,
0x3e,0x3e, 0x04,0x10, 0x08,0x08, 0x10,0x04, 0x20,0x02, 0x40,0x01, 0x80,0x00,
};

#define ArwUpRight_width 16
#define ArwUpRight_height 14
static char ArwUpRight_bits[] = {
0xe0,0x7f, 0x40,0x40, 0x80,0x40, 0x00,0x41, 0x80,0x40, 0x40,0x40, 0x20,0x48,
0x10,0x54, 0x08,0x62, 0x04,0x41, 0x88,0x00, 0x50,0x00, 0x20,0x00, 0x00,0x00,
};

#define ArwRight_width 16
#define ArwRight_height 14
static char ArwRight_bits[] = {
0x00,0x01, 0x00,0x03, 0x00,0x05, 0x00,0x09, 0xfe,0x11, 0x02,0x20, 0x02,0x40,
0x02,0x20, 0xfe,0x11, 0x00,0x09, 0x00,0x05, 0x00,0x03, 0x00,0x01, 0x00,0x00,
};

#define ArwDownRight_width 16
#define ArwDownRight_height 14
static char ArwDownRight_bits[] = {
0x00,0x00, 0x20,0x00, 0x50,0x00, 0x88,0x00, 0x04,0x41, 0x08,0x62, 0x10,0x54,
0x20,0x48, 0x40,0x40, 0x80,0x40, 0x00,0x41, 0x80,0x40, 0x40,0x40, 0xe0,0x7f,
};

Button *
MakeOpenButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;
static int openbuttoninit = 0;

    openbuttoninit++;
    b = (Button *) MemAlloc(sizeof(Button));
    *b = openButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Open_bits, Open_width, Open_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Open_width;
	b->label[0].pixlabel->height = Open_height;
	b->label[0].kind = ComboLabel;
	b->label[1].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[1].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Close_bits, Close_width, Close_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[1].pixlabel->width = Close_width;
	b->label[1].pixlabel->height = Close_height;
	b->label[1].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeFullSizeButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = fullSizeButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				FullSize_bits, FullSize_width, FullSize_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = FullSize_width;
	b->label[0].pixlabel->height = FullSize_height;
	b->label[0].kind = ComboLabel;
	b->label[1].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[1].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, NormalSize_bits,
				NormalSize_width, NormalSize_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[1].pixlabel->width = NormalSize_width;
	b->label[1].pixlabel->height = NormalSize_height;
	b->label[1].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeStickyButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = stickyButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Stick_bits, Stick_width, Stick_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Stick_width;
	b->label[0].pixlabel->height = Stick_height;
	b->label[0].kind = ComboLabel;
	b->label[1].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[1].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, UnStick_bits,
				UnStick_width, UnStick_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[1].pixlabel->width = UnStick_width;
	b->label[1].pixlabel->height = UnStick_height;
	b->label[1].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakePropertiesButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = propertiesButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Props_bits, Props_width, Props_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Props_width;
	b->label[0].pixlabel->height = Props_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeBackButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = backButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Back_bits, Back_width, Back_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Back_width;
	b->label[0].pixlabel->height = Back_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeResizeButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = resizeButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Resize_bits, Resize_width, Resize_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Resize_width;
	b->label[0].pixlabel->height = Resize_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeMoveButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Move_bits, Move_width, Move_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Move_width;
	b->label[0].pixlabel->height = Move_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeQuitButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = quitButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Quit_bits, Quit_width, Quit_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Quit_width;
	b->label[0].pixlabel->height = Quit_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeRefreshButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = refreshButton;
    if (GRV.UseImageMenu == UseAll) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				Refresh_bits, Refresh_width, Refresh_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = Refresh_width;
	b->label[0].pixlabel->height = Refresh_height;
	b->label[0].kind = ComboLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDismissButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = dismissButton;
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDismissAllButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = dismissAllButton;
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDismissThisButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = dismissThisButton;
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeOwnerButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = ownerButton;
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeUpLeftButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveUpLeftButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, ArwUpLeft_bits,
				ArwUpLeft_width, ArwUpLeft_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwUpLeft_width;
	b->label[0].pixlabel->height = ArwUpLeft_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeLeftButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveLeftButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				ArwLeft_bits, ArwLeft_width, ArwLeft_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwLeft_width;
	b->label[0].pixlabel->height = ArwLeft_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDownLeftButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveDownLeftButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, ArwDownLeft_bits,
				ArwDownLeft_width, ArwDownLeft_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwDownLeft_width;
	b->label[0].pixlabel->height = ArwDownLeft_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeUpButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveUpButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				ArwUp_bits, ArwUp_width, ArwUp_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwUp_width;
	b->label[0].pixlabel->height = ArwUp_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeHomeButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveHomeButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				ArwHome_bits, ArwHome_width, ArwHome_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwHome_width;
	b->label[0].pixlabel->height = ArwHome_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDownButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveDownButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				ArwDown_bits, ArwDown_width, ArwDown_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwDown_width;
	b->label[0].pixlabel->height = ArwDown_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeUpRightButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveUpRightButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, ArwUpRight_bits,
				ArwUpRight_width, ArwUpRight_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwUpRight_width;
	b->label[0].pixlabel->height = ArwUpRight_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeRightButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveRightButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid,
				ArwRight_bits, ArwRight_width, ArwRight_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwRight_width;
	b->label[0].pixlabel->height = ArwRight_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

Button *
MakeDownRightButton(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
Button	*b;

    b = (Button *) MemAlloc(sizeof(Button));
    *b = moveDownRightButton;
    if (GRV.UseImageMenu != UseNone) {
	b->label[0].pixlabel = (Pixlabel *) MemAlloc(sizeof(Pixlabel));
	b->label[0].pixlabel->pixmap = XCreatePixmapFromBitmapData(dpy,
				scrInfo->rootid, ArwDownRight_bits,
				ArwDownRight_width, ArwDownRight_height,
				scrInfo->colorInfo.fgColor,
				scrInfo->colorInfo.bg1Color,
				scrInfo->depth);
	b->label[0].pixlabel->width = ArwDownRight_width;
	b->label[0].pixlabel->height = ArwDownRight_height;
	b->label[0].kind = ImageLabel;
    }
#ifdef OW_I18N_L3
    b->label[0].string = gettext(b->label[0].string);
    b->label[1].string = gettext(b->label[1].string);
#endif
    return b;
}

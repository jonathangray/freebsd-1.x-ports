#ifndef _STAGE_H_
#define _STAGE_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define MAIN_WIDTH  70
#define MAIN_HEIGHT 130

#define PLAY_WIDTH  495
#define PLAY_HEIGHT 580

#define TOTAL_WIDTH  (MAIN_WIDTH + PLAY_WIDTH)
#define TOTAL_HEIGHT (MAIN_HEIGHT + PLAY_HEIGHT)

#define MESS_HEIGHT 30

#define BACKGROUND_WHITE   -2
#define BACKGROUND_BLACK   -1
#define BACKGROUND_0    	0
#define BACKGROUND_1    	1
#define BACKGROUND_2    	2
#define BACKGROUND_3    	3
#define BACKGROUND_4    	4
#define BACKGROUND_5    	5
#define BACKGROUND_6    	6
#define BACKGROUND_7    	7
#define BACKGROUND_8    	8
#define BACKGROUND_9    	9
#define BACKGROUND_SEE_THRU 10
#define BACKGROUND_SPACE 	11

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

extern Window mainWindow;
extern Window scoreWindow;
extern Window levelWindow;
extern Window playWindow, bufferWindow;
extern Window messWindow;
extern Window specialWindow;
extern Window timeWindow;

#if NeedFunctionPrototypes
void CreateAllWindows(Display *display, Colormap colormap, char **argv, 
	int argc);
void RedrawPlayWindow(Display *display, Window window);
void MapAllWindows(Display *display);
void ClearMainWindow(Display *display, Window window);
void DrawStageBackground(Display *display, Window window, int stageType,
	int clear);
void SetBackgrounds(Display *display, Colormap colormap);
#else
void CreateAllWindows();
void RedrawPlayWindow();
void MapAllWindows();
void ClearMainWindow();
void DrawStageBackground();
void SetBackgrounds();
#endif


#endif

/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#include <stdio.h>

#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xos.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#endif

#include "general.h"
#include "jewel.h"
#include "xw.h"
#include "xhscore.h"
#include "hscore.h"

#define HSCORE_X_START	75
#define HSCORE_X_END	(SCREEN_X - HSCORE_X_START)
#define HSCORE_X_SIZE (HSCORE_X_END - HSCORE_X_START)
#define HSCORE_Y_START 100
#define HSCORE_Y_SIZE	((HighScoreFont->ascent + HighScoreFont->descent)*3/2)
#define ONE_HSCORE_START(x) (HSCORE_Y_START+(2+x)*HSCORE_Y_SIZE)
#define ONE_HSCORE_TOP(x) (ONE_HSCORE_START(x)-HighScoreFont->ascent)

#define HSCORE_AVG_WIDTH 14
/* left */
#define HSCORE_COL1 (HSCORE_X_START + (4 * (HSCORE_AVG_WIDTH)) )
/* right */
#define HSCORE_COL3 HSCORE_X_END
/* center */
#define HSCORE_COL2 (HSCORE_COL3 - (12 * (HSCORE_AVG_WIDTH)) )

#include "bitmaps/skule.xbm"
#include "bitmaps/skulemask.xbm"

Pixmap SkulePM;
Pixmap SkulemaskPM;
GC SkuleGC;

Pixel red;

char buf[50];

void Init_Draw_High_Scores()
	{
	XGCValues gcv;
	unsigned long gcvm = (GCGraphicsExposures | GCForeground | GCBackground);

	red = xw_alloc_color("red");

	gcv.graphics_exposures = False;
	gcv.background = black;
    gcv.foreground = red;
	SkuleGC = XCreateGC(xw_display, xw_window, gcvm, &gcv);
	SkulePM = XCreateBitmapFromData(xw_display, xw_window,
			skule_bits, skule_width, skule_height);
	SkulemaskPM = XCreateBitmapFromData(xw_display, xw_window,
			skulemask_bits, skulemask_width, skulemask_height);
	/*
	XSetClipMask(xw_display,SkuleGC,SkulemaskPM);
	*/
	}

void Draw_High_Score_Title()
	{
	XCharStruct Sizes;
	int dir, asc, dsc;
	int len;

	sprintf(buf, "HIGH SCORES");
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		(SCREEN_X - Sizes.width)/2, HSCORE_Y_START, buf, len);
	/* left */
	sprintf(buf, "   Name");
	len=strlen(buf);
	XDrawImageString(xw_display, xw_window, HighScoreGC, HSCORE_COL1,
		HSCORE_Y_START + 2*HSCORE_Y_SIZE, buf, len);
	/* center */
	sprintf(buf, "Stage");
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL2 - (Sizes.width/2),
		HSCORE_Y_START + 2*HSCORE_Y_SIZE, buf, len);
	/* right */
	sprintf(buf, "Score");
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL3 - Sizes.width,
		HSCORE_Y_START + 2*HSCORE_Y_SIZE, buf, len);
	XFlush(xw_display);
	}

void Draw_One_High_Score(i)
int i;
	{
	XCharStruct Sizes;
	int dir, asc, dsc;
	int len;

	/* right */
	sprintf(buf,"%d-",i);
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL1 - Sizes.width,
		ONE_HSCORE_START(i), buf, len);
	/* left */
	sprintf(buf, " %.20s", high_scores[i-1].name);
	len=strlen(buf);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL1,
		ONE_HSCORE_START(i), buf, len);
	/* center */
	sprintf(buf, "%d", high_scores[i-1].stage);
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL2 - (Sizes.width/2),
		HSCORE_Y_START + (2 + i)*HSCORE_Y_SIZE, buf, len);
	/* right */
	sprintf(buf, "%d", high_scores[i-1].score);
	len=strlen(buf);
	XTextExtents(HighScoreFont,buf,len,&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HighScoreGC,
		HSCORE_COL3 - Sizes.width,
		ONE_HSCORE_START(i), buf, len);
	XFlush(xw_display);
	}


void Show_High_Scores(start)
int start;
	{
	int i;

	Draw_High_Score_Title();
	for (i=start; i<=num_high_scores; i++)
		{
		Draw_One_High_Score(i);
		}
	XFlush(xw_display);
	}

void Wipeout_Last_High_Score()
	{
	int i;

	for (i=0; i<(HSCORE_X_SIZE - (HSCORE_COL1 - HSCORE_X_START)); i=i+3)
		{
		/*
		XSetClipOrigin(xw_display, SkuleGC,
			HSCORE_COL1+i, ONE_HSCORE_TOP(MAX_HIGH_SCORES));
		*/
		XCopyPlane(xw_display, SkulePM, xw_window, SkuleGC,
			0, 0, skule_width, skule_height, HSCORE_COL1+i,
			ONE_HSCORE_TOP(MAX_HIGH_SCORES), 1L);
		xw_sync_sleep(50L);
		}
	}

void Move_Down_High_Scores(number)
int number;
	{
	int width, height;
	int i;
	/*
	move area starting HSCORE_COL1, ONE_HSCORE_TOP(i)
	ending   HSCORE_COL3, ONE_HSCORE_TOP(MAX_HIGH_SCORES)
	down by HSCORE_Y_SIZE
	*/
	if (number < MAX_HIGH_SCORES)
		{
		width = HSCORE_COL3 - HSCORE_COL1; 
		height = ONE_HSCORE_TOP(MAX_HIGH_SCORES) - ONE_HSCORE_TOP(number);
		for (i=0; i<HSCORE_Y_SIZE; i++)
			{
			XCopyArea(xw_display, xw_window, xw_window, xw_gc, HSCORE_COL1,
				ONE_HSCORE_TOP(number)+i, width, height,
				HSCORE_COL1, ONE_HSCORE_TOP(number) + i + 1);
			xw_sync_sleep(20L);
			}
		}
	}


void Set_State_High_Score()
	{
	XClearWindow(xw_display, xw_window);

	Refresh_High_Scores();

	JewelState=HIGHSCORE;
	xw_set_timer(10000L);
	}

/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xos.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#endif
#include <stdlib.h>

#include "general.h"
#include "logic.h"
#include "jewel.h"
#include "xhscore.h"
#include "xw.h"

static enum IStates { PRESENT, LOGO, BY, SHINE, FINI} IntroState;


#define PRES_FONT "-*-*-bold-o-*-*-24-*-*-*-p-*-iso8859-1"
#define PRESENT_LOC_X 100
#define PRESENT_LOC_Y 100
static char *PresentString="Presenting...";
#define THANK_LOC_Y 500
static char *ThankString="Originally by Yoshihiro Satoh of hp";
#define PHASE_LOC_X ((SCREEN_X-biglogo_width)/2)
#define PHASE_LOC_Y 300


#define NUM_SHINE 4
#define STEP 2
#define YELSTEP 20
#define SHINESTEP 10
#include "bitmaps/biglogo.xbm"
static GC GreenGC, YelGC, PresGC;
static XFontStruct *PresFont;
static Pixmap BigLogoPM;
static int count=0;
static int ShineX=0;
static BOOL dir_forward=0;
#define LM (biglogo_width/2)
#define XM (PHASE_LOC_X + LM)
#define GWIDTH ((biglogo_width/STEP)+1)
#define GHEIGHT ((biglogo_height/STEP)+1)

void Start_Intro()
	{
	if (JewelState != NULL_STATE)
	    { XClearWindow(xw_display, xw_window); }

	IntroState=PRESENT;
	JewelState=INTRO;

	count=0;
	xw_set_timer(200L);
	}

void Intro_Timeout()
	{
	switch(IntroState)
		{
		case PRESENT:
			/* start logo */
			count++;
			if (count <= strlen(PresentString))
				{
				XDrawString(xw_display, xw_window, PresGC, PRESENT_LOC_X,
					PRESENT_LOC_Y+((PresFont->ascent)*3/2),
					PresentString, count);
				XFlush(xw_display);
				}
			else
				{
				IntroState=LOGO;
				/* set up */
				count=0;
				}
			xw_set_timer(200L);
			break;
		case LOGO:
			/* put more logo up */
			XCopyPlane(xw_display, BigLogoPM, xw_window, GreenGC,
				(LM-count*STEP), 0, STEP, biglogo_height,
				(XM-count*STEP), PHASE_LOC_Y, 1L);
			
			XCopyPlane(xw_display, BigLogoPM, xw_window, GreenGC,
				(LM+count*STEP), 0, STEP, biglogo_height,
				(XM+count*STEP), PHASE_LOC_Y, 1L);
			
			XCopyPlane(xw_display, BigLogoPM, xw_window, YelGC,
				(LM-count*STEP)-YELSTEP, 0, YELSTEP, biglogo_height,
				(XM-count*STEP)-YELSTEP, PHASE_LOC_Y, 1L);

			XCopyPlane(xw_display, BigLogoPM, xw_window, YelGC,
				(LM+count*STEP)+STEP, 0, YELSTEP, biglogo_height,
				(XM+count*STEP)+STEP, PHASE_LOC_Y, 1L);
			XFlush(xw_display);

			count++;

			XFlush(xw_display);
			if (count < (GWIDTH/2))
				{ xw_set_timer(2L); }
			else
				{
				IntroState=BY;
				xw_set_timer(200L);
				}
			break;
		case BY:
			{
			XCharStruct Sizes;
			int dir, asc, dsc;

			XTextExtents(VerFont,ThankString, strlen(ThankString),
				&dir,&asc,&dsc,&Sizes);
			XDrawImageString(xw_display, xw_window, VerGC,
				(SCREEN_X - Sizes.width)/2,
				THANK_LOC_Y+((VerFont->ascent)*3/2),
				ThankString, strlen(ThankString));

			XTextExtents(VerFont,StartString, strlen(StartString),
				&dir,&asc,&dsc,&Sizes);
			XDrawImageString(xw_display, xw_window, VerGC,
				(SCREEN_X - Sizes.width)/2,
				START_LOC_Y+((VerFont->ascent)*3/2),
				StartString, strlen(StartString));

			XDrawImageString(xw_display, xw_window, VerGC, VER_LOC_X,
				VER_LOC_Y+((VerFont->ascent)*3/2),
				VerString, strlen(VerString));
			XFlush(xw_display);
			count=0;
			ShineX=0;
			dir_forward=TRUE;
			IntroState=SHINE;
			xw_set_timer(200L);
			}
			break;
		case SHINE:
			XCopyPlane(xw_display, BigLogoPM, xw_window, GreenGC,
				(ShineX+ ((dir_forward)?0:SHINESTEP)), 0,
				STEP, biglogo_height,
				(ShineX+PHASE_LOC_X + ((dir_forward)?0:SHINESTEP)),
				PHASE_LOC_Y, 1L);
			
			XCopyPlane(xw_display, BigLogoPM, xw_window, YelGC,
				(ShineX+((dir_forward)?STEP:0)), 0,
				SHINESTEP, biglogo_height,
				(ShineX+PHASE_LOC_X+((dir_forward)?STEP:0)),
				PHASE_LOC_Y, 1L);

			XFlush(xw_display);

			ShineX+=((dir_forward)?(STEP):(-STEP));
			xw_set_timer(2L);
			if (ShineX>biglogo_width)
				{
				ShineX=biglogo_width;
				dir_forward=FALSE;
				}
			if (ShineX<0)
				{
				ShineX=0;
				dir_forward=TRUE;
				count++;
				if (count>NUM_SHINE)
					{
					IntroState=FINI;
					xw_set_timer(1000L);
					}
				}
			break;
		case FINI:
			Set_State_High_Score();
			JewelState=HIGHSCORE;
			break;
		}
	}

void Init_Intro()
	{
	XGCValues gcv;
	unsigned long gcvm;
	
	gcvm=(GCGraphicsExposures | GCForeground | GCBackground | GCPlaneMask);

	gcv.graphics_exposures=False;
	gcv.plane_mask=(green | yellow | black);
	gcv.foreground=yellow;
	gcv.background=black;
	YelGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	gcv.foreground=green;
	GreenGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);

	BigLogoPM=XCreateBitmapFromData(xw_display, xw_window,
				biglogo_bits, biglogo_width, biglogo_height);

	if ( (PresFont=XLoadQueryFont(xw_display,PRES_FONT)) == NULL)
		{ xw_fatal("Cannot load PRESENT font.\n",__LINE__,__FILE__); }
	gcvm|=GCFont;
	gcv.font=PresFont->fid;
	gcv.foreground=yellow;
	PresGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	}

void Expose_Intro()
	{
	switch(IntroState)
		{
		case PRESENT:
			XDrawString(xw_display, xw_window, PresGC, PRESENT_LOC_X,
				PRESENT_LOC_Y+((PresFont->ascent)*3/2),
				PresentString, count);
			break;
		case LOGO:
			XDrawString(xw_display, xw_window, PresGC, PRESENT_LOC_X,
				PRESENT_LOC_Y+((PresFont->ascent)*3/2),
				PresentString, strlen(PresentString));
			XCopyPlane(xw_display, BigLogoPM, xw_window, GreenGC,
				0, 0, biglogo_width, biglogo_height,
				PHASE_LOC_X, PHASE_LOC_Y, 1L);
			break;
		case BY:
		case SHINE:
			{
			XCharStruct Sizes;
			int dir, asc, dsc;

			XDrawString(xw_display, xw_window, PresGC, PRESENT_LOC_X,
				PRESENT_LOC_Y+((PresFont->ascent)*3/2),
				PresentString, strlen(PresentString));

			XCopyPlane(xw_display, BigLogoPM, xw_window, GreenGC,
				0, 0, biglogo_width, biglogo_height,
				PHASE_LOC_X, PHASE_LOC_Y, 1L);

			XTextExtents(VerFont,ThankString, strlen(ThankString),
				&dir,&asc,&dsc,&Sizes);
			XDrawImageString(xw_display, xw_window, VerGC,
				(SCREEN_X - Sizes.width)/2,
				THANK_LOC_Y+((VerFont->ascent)*3/2),
				ThankString, strlen(ThankString));

			XTextExtents(VerFont,StartString, strlen(StartString),
				&dir,&asc,&dsc,&Sizes);
			XDrawImageString(xw_display, xw_window, VerGC,
				(SCREEN_X - Sizes.width)/2,
				START_LOC_Y+((VerFont->ascent)*3/2),
				StartString, strlen(StartString));

			XDrawImageString(xw_display, xw_window, VerGC, VER_LOC_X,
				VER_LOC_Y+((VerFont->ascent)*3/2),
				VerString, strlen(VerString));
			}
			break;
		}
	XFlush(xw_display);
	}


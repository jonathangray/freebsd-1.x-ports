/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#define XK_MISCELLANY
#define XK_LATIN1
#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xos.h>
#include <decw$include/keysymdef.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysymdef.h>
#endif

#include <stdlib.h>

#include "general.h"
#include "logic.h"
#include "jewel.h"
#include "xw.h"
#include "xhscore.h"
#include "hscore.h"
#include "intro.h"
#include "game.h"
#include "panel.h"
#include "help.h"

#include "version.h"
char *StartString="Press \140Space\047 to begin, or \140H\047 for Help";

enum JStates JewelState;
/* event functions */

void xw_focus_event(xfev)
XFocusChangeEvent *xfev;
	{
	/* handle focus change */
	if ((JewelState == GAME) && (xfev->type == FocusOut))
		{
		/*printf("FocusOut->PAUSED\n");*/
		Set_Pause();
		}
	}


void xw_leave_event(xlev)
XLeaveWindowEvent *xlev;
	{
#ifdef LEAVE_PAUSE
	if (JewelState == GAME)
		{
		/*printf("LeaveOut->PAUSED\n");*/
		Set_Pause();
		}
#endif
	}


static enum JStates OldState=NULL_STATE;

static void StartIcon()
    {
    OldState=JewelState;
    JewelState=ICON;
    xw_set_timer(2000l);
    }

static void EndIcon()
    {
    JewelState=OldState;
    xw_set_timer(100L);
    }

void xw_map_event(xmev)
XMapEvent *xmev;
	{
	/*printf("GOT MAP EVENT. win=%ld, type=%d\n",xmev->window,
	xmev->type);*/
	if (xmev->window == xw_window)
		{
		if ((JewelState!=ICON) && (xmev->type == UnmapNotify))
			{
			StartIcon();
			}
		if ((JewelState==ICON) && (xmev->type == MapNotify))
			{
			EndIcon();
			}
		}
	}

void xw_key_event(xkev)
XKeyEvent *xkev;
	{
	/* deal with key event */
	KeySym key;

	key=XLookupKeysym(xkev,xkev->state);
	switch(JewelState)
		{
		case GAME:
			switch(key)
				{
				/* HIDDEN KEYS USED FOR DEBUGGING 
				case XK_plus:
				case XK_KP_Add:
					Inc_Stage();	
					break;
				case XK_minus:
				case XK_KP_Subtract:
					Dec_Stage();	
					break; 
				********************************/
				case XK_U:
				case XK_u:
				case XK_Menu:/* do */
				case XK_F1:
				case XK_F20:
					XIconifyWindow(xw_display, xw_window, xw_screen);
					break;
				case XK_Cancel:
				case XK_E:
				case XK_e:
					Melt_Down();
					End_Game();
					break;
				case XK_P:
				case XK_p:
					Toggle_Pause();
					break;
				case XK_S:
				case XK_s:
					Toggle_Sound();
					break;
				case XK_X:
				case XK_x:
				case XK_Q:
				case XK_q:
					printf("Thankyou for playing...try again sometime!\n");
					xw_exit_main();
					break;
				case XK_Left:
				case XK_4:
				case XK_KP_4:
				case XK_J:
				case XK_j:
					if (!Paused()) Move_Left();
					break;
				case XK_Right:
				case XK_6:
				case XK_KP_6:
				case XK_L:
				case XK_l:
					if (!Paused()) Move_Right();
					break;
				case XK_Next:
				case XK_Up:
				case XK_5:
				case XK_KP_5:
				case XK_K:
				case XK_k:
					if (!Paused()) Rotate();
					break;
				case XK_Down:
				case XK_0:
				case XK_KP_0:
				case XK_space:
				case XK_KP_Space:
					if (!Paused()) Drop();
					break;
				}
			break;
		case INTRO:
		case HIGHSCORE:
		case HELP:
			switch (key)
				{
				case XK_Select:
				case XK_Execute:
				case XK_S:
				case XK_s:
				case XK_space:
				case XK_KP_Space:
					Start_New_Game();
					break;
				case XK_P:
				case XK_p:
					Set_State_High_Score();
					break;
				case XK_H:
				case XK_h:
				case XK_F1:
				case XK_Help:
				case XK_Menu:
					Start_Help();
					break;
				case XK_X:
				case XK_x:
				case XK_Q:
				case XK_q:
					printf("Thankyou for playing...try again sometime!\n");
					xw_exit_main();
					break;
				}
			break;
		}
	XFlush(xw_display);
	
	/*xw_ev.x = xkev->x;
	xw_ev.y = xkev->y;*/
	}



void xw_but_event(xbev)
XButtonEvent *xbev;
	{
	/* deal with key event */
	/*printf("buttonpress:%d\n",xbev->button); */
	/*xw_ev.button = xbev->button;
	xw_ev.x = xbev->x;
	xw_ev.y = xbev->y;*/
	}



void xw_expose_event(xev)
XExposeEvent *xev;
	{
	/* do screen update */
	/*printf("GOT EXPOS EVENT, win=%ld, jstate=%d\n", xev->window,
	JewelState);*/
#ifdef ICON_WINDOW
	if (xev->window != xw_window)
	    { /* must be icon window */
	    if (JewelState != ICON)
		{ StartIcon(); }
	    return;
	    }
	else
	    {
	    if (JewelState == ICON)
		{ EndIcon(); }
	    }
#endif
	if (!(xev->count))
		{
		switch(JewelState)
			{
			case NULL_STATE:
				Start_Intro();
				break;
			case GAME:
				Expose_Game();
				break;
			case HELP:
				Expose_Help();
				break;
			case INTRO:
				Expose_Intro();
				break;
			case HIGHSCORE:
				Refresh_High_Scores();
				break;
			}
		}
	}


void Rot_Icons()
	{
#ifdef ICON_WINDOW
	extern Window iw_window;
	extern int iw_width, iw_height;
	static int IconPiece=PIECE1;

	XCopyPlane(xw_display, PiecesPM[IconPiece], iw_window, PiecesGC[IconPiece],
		0, 0, SIZE_PIECE, SIZE_PIECE,
		(iw_width - SIZE_PIECE)/2, (iw_height - SIZE_PIECE)/2, 1L);

	IconPiece++;
	if (IconPiece == NUM_REAL_PIECES) { IconPiece=PIECE1; }
#endif

	xw_set_timer(2000L);
	}

void xw_timeout()
	{
	/* handle timer */
	switch(JewelState)
		{
		case GAME:
			Game_Timeout();
			break;
		case HIGHSCORE:
			Start_Help();
			break;
		case HELP:
			Start_Intro();
			break;
		case INTRO:
			Intro_Timeout();
			break;
		case ICON:
			Rot_Icons();
			break;
		}
	}



/********************************************************/

#include "bitmaps/jewellogo.xbm"
#include "bitmaps/jewellogo2.xbm"
/* border */
#include "bitmaps/border.xbm"
/*#include "bitmaps/border1.xbm"*/
/* pieces */
#include "bitmaps/piece1.xbm"
#include "bitmaps/piece2.xbm"
#include "bitmaps/piece3.xbm"
#include "bitmaps/piece4.xbm"
#include "bitmaps/piece5.xbm"
#include "bitmaps/piece6.xbm"
#include "bitmaps/jewel.xbm"
#include "bitmaps/flash1.xbm"
#include "bitmaps/flash2.xbm"
#include "bitmaps/flash3.xbm"
#include "bitmaps/flash4.xbm"
/* backdrops */
#include "bitmaps/back1.xbm"
#include "bitmaps/back2.xbm"
#include "bitmaps/back3.xbm"
#include "bitmaps/back4.xbm"

int Border_Width=border_width;
int Border_Height=border_height;
int Jewellogo2_Width=jewellogo2_width;
int Jewellogo2_Height=jewellogo2_height;
int Jewellogo_Width=jewellogo_width;
int Jewellogo_Height=jewellogo_height;

Pixmap LogoPM;
Pixmap Logo2PM;
GC LogoGC;
GC Logo2GC;

Pixmap BorderPM;
GC BorderGC;

Pixmap PiecesPM[NUM_PIECES];
GC PiecesGC[NUM_PIECES];

GC ScoreGC;
GC HighScoreGC;
int Score_Char_MHeight;
int Score_x_right;
GC VerGC;
XFontStruct *VerFont, *ScoreFont, *HighScoreFont;

Pixel colors[NUM_PIECES];

Pixel green;
Pixel yellow;
Pixel black;
Pixel white;

void Init_Jewel()
	{
	XGCValues gcv;
	unsigned long gcvm=(GCGraphicsExposures | GCForeground | GCBackground);

	green=xw_alloc_color("green");
	yellow=xw_alloc_color("yellow");
	black=BlackPixel(xw_display,xw_screen);
	white=WhitePixel(xw_display,xw_screen);

	gcv.graphics_exposures=False;
	gcv.background=black;

	/* Logo */
	gcv.foreground=green;
	LogoGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	LogoPM=XCreateBitmapFromData(xw_display, xw_window,
			jewellogo_bits, jewellogo_width, jewellogo_height);
	XSetClipMask(xw_display,LogoGC,LogoPM);

	gcv.foreground=white;
	Logo2GC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	Logo2PM=XCreateBitmapFromData(xw_display, xw_window,
			jewellogo2_bits, jewellogo2_width, jewellogo2_height);

	/* border */
	gcv.foreground=white;
	BorderGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	BorderPM=XCreateBitmapFromData(xw_display, xw_window, border_bits, 
			border_width, border_height);

	/* Pieces */
	colors[WILD_PIECE]=xw_alloc_color(WILD_C);
	gcv.foreground=colors[WILD_PIECE];
	PiecesGC[WILD_PIECE]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[WILD_PIECE]=XCreateBitmapFromData(xw_display, xw_window,
			jewel_bits, jewel_width, jewel_height);

	colors[PIECE1]=xw_alloc_color(PIECE1_C);
	gcv.foreground=colors[PIECE1];
	PiecesGC[PIECE1]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE1]=XCreateBitmapFromData(xw_display, xw_window,
			piece1_bits, piece1_width, piece1_height);

	colors[PIECE2]=xw_alloc_color(PIECE2_C);
	gcv.foreground=colors[PIECE2];
	PiecesGC[PIECE2]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE2]=XCreateBitmapFromData(xw_display, xw_window,
			piece2_bits, piece2_width, piece2_height);

	colors[PIECE3]=xw_alloc_color(PIECE3_C);
	gcv.foreground=colors[PIECE3];
	PiecesGC[PIECE3]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE3]=XCreateBitmapFromData(xw_display, xw_window,
			piece3_bits, piece3_width, piece3_height);

	colors[PIECE4]=xw_alloc_color(PIECE4_C);
	gcv.foreground=colors[PIECE4];
	PiecesGC[PIECE4]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE4]=XCreateBitmapFromData(xw_display, xw_window,
			piece4_bits, piece4_width, piece4_height);

	colors[PIECE5]=xw_alloc_color(PIECE5_C);
	gcv.foreground=colors[PIECE5];
	PiecesGC[PIECE5]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE5]=XCreateBitmapFromData(xw_display, xw_window,
			piece5_bits, piece5_width, piece5_height);

	colors[PIECE6]=xw_alloc_color(PIECE6_C);
	gcv.foreground=colors[PIECE6];
	PiecesGC[PIECE6]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[PIECE6]=XCreateBitmapFromData(xw_display, xw_window,
			piece6_bits, piece6_width, piece6_height);

	/* Flash pieces */
	colors[FLASH1]=xw_alloc_color(FLASH_C);
	gcv.foreground=colors[FLASH1];
	PiecesGC[FLASH1]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[FLASH1]=XCreateBitmapFromData(xw_display, xw_window,
			flash1_bits, flash1_width, flash1_height);

	colors[FLASH2]=colors[FLASH1];
	PiecesGC[FLASH2]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[FLASH2]=XCreateBitmapFromData(xw_display, xw_window,
			flash2_bits, flash2_width, flash2_height);

	colors[FLASH3]=colors[FLASH1];
	PiecesGC[FLASH3]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[FLASH3]=XCreateBitmapFromData(xw_display, xw_window,
			flash3_bits, flash3_width, flash3_height);

	colors[FLASH4]=colors[FLASH1];
	PiecesGC[FLASH4]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[FLASH4]=XCreateBitmapFromData(xw_display, xw_window,
			flash4_bits, flash4_width, flash4_height);

	/* Backdrops */
	colors[BACKGND1]=xw_alloc_color(BACKGND_C);
	gcv.foreground=colors[BACKGND1];
	PiecesGC[BACKGND1]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[BACKGND1]=XCreateBitmapFromData(xw_display, xw_window,
			back1_bits, back1_width, back1_height);

	colors[BACKGND2]=colors[BACKGND1];
	PiecesGC[BACKGND2]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[BACKGND2]=XCreateBitmapFromData(xw_display, xw_window,
			back2_bits, back2_width, back2_height);

	colors[BACKGND3]=colors[BACKGND1];
	PiecesGC[BACKGND3]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[BACKGND3]=XCreateBitmapFromData(xw_display, xw_window,
			back3_bits, back3_width, back3_height);

	colors[BACKGND4]=colors[BACKGND1];
	PiecesGC[BACKGND4]=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	PiecesPM[BACKGND4]=XCreateBitmapFromData(xw_display, xw_window,
			back4_bits, back4_width, back4_height);

	/* fonts */
	gcvm|=GCFont;

	if ( (VerFont=XLoadQueryFont(xw_display,VER_FONT)) == NULL)
		{ xw_fatal("Cannot load VERSION font.\n",__LINE__,__FILE__); }
	gcv.font=VerFont->fid;
	gcv.foreground=white;
	VerGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);

	if ( (HighScoreFont=XLoadQueryFont(xw_display,HIGHSC_FONT)) == NULL)
		{ xw_fatal("Cannot load HIGHSCORE font.\n",__LINE__,__FILE__); }
	gcv.foreground=white;
	gcv.font=HighScoreFont->fid;
	HighScoreGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);

	if ( (ScoreFont=XLoadQueryFont(xw_display,SCORE_FONT)) == NULL)
		{
		if ( (ScoreFont=XLoadQueryFont(xw_display,ALT_SC_FONT)) == NULL)
			{ xw_fatal("Cannot load SCORE font.\n",__LINE__,__FILE__); }
		}
	gcv.foreground=white;
	gcv.font=ScoreFont->fid;
	ScoreGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	Score_Char_MHeight=(ScoreFont->ascent + ScoreFont->descent)*3/2;
	Score_x_right=SCORE_LOC_X + (ScoreFont->max_bounds.width*SCORE_WIDTH);
	}



int main(argc,argv)
int argc;
char **argv;
	{
	XKeyboardControl Values;
	struct timeval curtime;

	gettimeofday(&curtime,NULL);
	srandom((unsigned int)(curtime.tv_usec>>8));

	JewelState=NULL_STATE;

	xw_init(argc,argv,SCREEN_X, SCREEN_Y);

	Init_Jewel();

	Values.bell_duration=50;
	XChangeKeyboardControl(xw_display,KBBellDuration, &Values);
	xw_start();
	Init_Draw_High_Scores();
	Init_Intro();
	Init_Help();

	xw_main_loop();
	XCloseDisplay(xw_display);
	exit(0);
	}

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

#include "general.h"
#include "logic.h"
#include "hscore.h"
#include "xhscore.h"
#include "jewel.h"
#include "game.h"
#include "panel.h"
#include "xw.h"


/* util functions */
void Key_Bell()
	{
	if (Sound())
		{ XBell(xw_display,100); }
	}
#define STRBUFSIZE 128


#include <stdlib.h>
#define RANDY(range) (random()%(range))

void Melt_Down()
	{
	int cycle;
	int offx,offy;
	int srcx,srcy, destx,desty;
	unsigned int width,height;
	unsigned int GWIDTH=NUM_COLS*SIZE_PIECE, GHEIGHT=NUM_ROWS*SIZE_PIECE;

	struct timeval curtime;
	gettimeofday(&curtime,NULL);
	srandom((unsigned int)(curtime.tv_usec>>8));

#define NUM_CYCLES 12
	for(cycle=0;cycle<NUM_CYCLES;cycle++)
	  for(srcy=1+cycle ;srcy<(GHEIGHT-10);srcy+=(SIZE_PIECE/2))
		{
		int count;
		for (count=0;count<((NUM_COLS*SIZE_PIECE)/40);count++)
			{
			offx=RANDY(5)-2;
			offy=RANDY(4)+1;

			width=RANDY(GWIDTH/2);
			height=RANDY(GHEIGHT-srcy-offy);

			/* MAXwidth+MAXoffx must be << GWIDTH */
			srcx=RANDY(GWIDTH-width-(abs(offx)));
			if (offx<0) { srcx-=offx; }

			destx=srcx+offx;
			desty=srcy+offy;
			if (desty>(GHEIGHT-1)) break;

			XCopyArea(xw_display,xw_window,xw_window,xw_gc,
				srcx+(BRD_LOC_X+SIZE_PIECE), srcy+BRD_LOC_Y,
				width,height,
				destx+(BRD_LOC_X+SIZE_PIECE),desty+BRD_LOC_Y);
			}
		}
	xw_sync_sleep(150L);

	/* erase area */
#	define NUM_SCAT 10 /* SIZE_PIECE should be an integral mult of this */
	offx=RANDY(NUM_SCAT);
	offy=RANDY(NUM_SCAT);
	for (srcx=0; srcx < NUM_SCAT; srcx++)
		{
		static int xscat[NUM_SCAT]={ 1, 9, 3, 6, 2, 4, 0, 7, 5, 8 };
		static int yscat[NUM_SCAT]={ 2, 1, 0, 8, 6, 4, 9, 3, 7, 5 };
		for (srcy=0; srcy < NUM_SCAT; srcy++)
			{
			for (destx=0;destx<NUM_COLS;destx++)
			  for (desty=0;desty<NUM_ROWS;desty++)
				{
				/*XClearArea(xw_display,xw_window,*/
				XFillRectangle(xw_display,xw_window,PiecesGC[FLASH1],
					((destx+1)*SIZE_PIECE)+BRD_LOC_X+
						xscat[(srcx+srcy+offx)%NUM_SCAT]*(SIZE_PIECE/NUM_SCAT),
					((desty)*SIZE_PIECE)+BRD_LOC_Y+
						yscat[(srcy+offy)%NUM_SCAT]*(SIZE_PIECE/NUM_SCAT),
					(SIZE_PIECE/NUM_SCAT), (SIZE_PIECE/NUM_SCAT)/*,False*/);
				}
			}
		}
	xw_sync_sleep(150L);
	}


void Draw_Piece(piece,x,y)
int piece, x, y;
	{
	/* draw piece at loc x,y */
	/* locations here are not raw board, but the active board */
	if (x<0)
		{
		XCopyPlane(xw_display, PiecesPM[piece], xw_window, PiecesGC[piece],
			0, 0, SIZE_PIECE, SIZE_PIECE,
			PREV_LOC_X+(SIZE_PIECE*1), PREV_LOC_Y+(SIZE_PIECE*(y+1)), 1L);
		}
	else
		{
		XCopyPlane(xw_display, PiecesPM[piece], xw_window, PiecesGC[piece],
			0, 0, SIZE_PIECE, SIZE_PIECE,
			BRD_LOC_X+(SIZE_PIECE*(x+1)), BRD_LOC_Y+(SIZE_PIECE*y), 1L);
		}
	}


void Flash_Pieces(p_remove, numflash, background)
struct rem_piece p_remove[];
int numflash,background;
	{
#ifndef SLOW_DRAW
#  define SLOW_DRAW 0
#endif
	int reps;
	int findex;

	for (reps=0;reps<NUM_FLASH;reps++)
		{
		for (findex=0;findex < numflash;findex++)
			{
			Draw_Piece(FLASH1+reps, p_remove[findex].x, p_remove[findex].y);
			}
		xw_sync_sleep(70L+SLOW_DRAW);
		for (findex=0;findex < numflash;findex++)
			{
			Draw_Piece(p_remove[findex].piece,
				p_remove[findex].x, p_remove[findex].y);
			}
		xw_sync_sleep(160L+SLOW_DRAW);
		}
	for (findex=0;findex < numflash;findex++)
		{
		Draw_Piece(background, p_remove[findex].x, p_remove[findex].y);
		}
	xw_sync_sleep(100L+SLOW_DRAW);
	}

void Redraw_Add_Score(pts,mult)
int pts, mult;
	{
#ifndef SLOW_FONTS
#   define SLOW_FONTS 0
#endif
	char buf[80];
	int y=SCORE_LOC_Y;
	XCharStruct Sizes;
	int dir, asc, dsc;
	/* write points */
	sprintf(buf,"POINTS");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%d",pts);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;

	/* write multiple */
	sprintf(buf,"X");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%d",mult);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;

	/* wait a while */
	xw_sync_sleep(160L+SLOW_FONTS);
	
	/* put total points up */
	y=SCORE_LOC_Y;
	sprintf(buf,"%6d",(pts * mult));
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));

	for (y=SCORE_LOC_Y+1; y<(SCORE_LOC_Y + (Score_Char_MHeight*2)) ; y++)
			/* location of SCORE */
		{
		char *buf2="POINTS ";
		XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
			buf2, strlen(buf2));
		XDrawImageString(xw_display, xw_window, ScoreGC,
			(Score_x_right - Sizes.width), y, buf, strlen(buf));
		if (SLOW_FONTS > 0)
		    {
		    xw_sync_sleep(10L+SLOW_FONTS);
		    /* IF THE X-SERVER IS FAST WITH FONTS */
		    }
		}
	xw_sync_sleep(110L+SLOW_FONTS);
	}


void Redraw_Score(Score)
int Score;
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*2);
	/* write Score */
	sprintf(buf,"SCORE  ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%d",Score);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}

void Redraw_Lives(Lives)
int Lives;
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*3);
	/* write Lives */
	sprintf(buf,"LIVES  ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%d",Lives);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}

void Redraw_Speed(Speed)
float Speed;
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*4);
	/* write Speed */
	sprintf(buf,"SPEED  ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%.5f",Speed);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}


void Redraw_Stage(Stage)
int Stage;
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*5);
	/* write Stage */
	sprintf(buf,"STAGE  ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf," %d",Stage);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}


void Redraw_Rest(Rest)
int Rest;
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*6);
	/* write Rest */
	sprintf(buf,"REST  ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"  %d",Rest);
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}

void Redraw_Sound()
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*7);
	/* write Sound */
	sprintf(buf,"SOUND ");
	XDrawImageString(xw_display, xw_window, ScoreGC, SCORE_LOC_X, y,
		buf, strlen(buf));
	sprintf(buf,"%s",((Sound()) ? "  ON" : "OFF"));
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	y+=Score_Char_MHeight;
	}

void Redraw_Pause()
	{
	char buf[80];
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*8);
	/* write Pause */
	if (Paused())
		{ sprintf(buf,"PAUSED"); }
	else
		{ sprintf(buf,"            "); }
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));
	}

void Redraw_Game_Board()
	{
	int x,y;
	/* build border */
	for (y=0;y<BOARD_HEIGHT;y++)
		{
		XCopyPlane(xw_display, BorderPM, xw_window, BorderGC, 0, 0,
			Border_Width, Border_Height,
			BRD_LOC_X, BRD_LOC_Y+(SIZE_PIECE*y),1L);
		XCopyPlane(xw_display, BorderPM, xw_window, BorderGC, 0, 0,
			Border_Width, Border_Height,
			BRD_LOC_X+(SIZE_PIECE*(BOARD_WIDTH-1)), BRD_LOC_Y+(SIZE_PIECE*y),1L);
		}
	for (x=1;x<(BOARD_WIDTH-1);x++)
		{
		XCopyPlane(xw_display, BorderPM, xw_window, BorderGC, 0, 0,
			Border_Width, Border_Height,
			BRD_LOC_X+(SIZE_PIECE*x),BRD_LOC_Y+(SIZE_PIECE*(BOARD_HEIGHT-1)),1L);
		}
	for (x=0;x<(PREVIEW_WIDTH);x++)
	  for (y=0;y<(PREVIEW_HEIGHT);y++)
		{
		XCopyPlane(xw_display, BorderPM, xw_window, BorderGC, 0, 0,
			Border_Width, Border_Height,
			PREV_LOC_X+(SIZE_PIECE*x),PREV_LOC_Y+(SIZE_PIECE*y),1L);
		}
	/* put pieces on it */
	Redraw_Board();
	}

void Start_New_Game()
	{
	extern void Expose_Game();
	XClearWindow(xw_display,xw_window);
	xw_set_timer(Get_Speed_ms());
	JewelState=GAME;
	New_Game();
	Init_Logic();
	Expose_Game();
	}

void End_Game()
	{
	char *buf;
	XCharStruct Sizes;
	int dir, asc, dsc;
	int y=SCORE_LOC_Y + (Score_Char_MHeight*8);

	buf="GAME OVER";
	XTextExtents(ScoreFont,buf,strlen(buf),&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, ScoreGC,
		(Score_x_right - Sizes.width), y, buf, strlen(buf));

	xw_sync_sleep(500L);
	Set_State_High_Score();
	Update_High_Scores(Get_Stage(), Get_Score());
	}


void Expose_Game()
	{
	XCopyPlane(xw_display, Logo2PM, xw_window, Logo2GC, 0, 0,
		Jewellogo2_Width, Jewellogo2_Height,
		LOGO_LOC_X, LOGO_LOC_Y, 1L);

	XSetClipOrigin(xw_display, LogoGC, LOGO_LOC_X, LOGO_LOC_Y );
	XCopyPlane(xw_display, LogoPM, xw_window, LogoGC, 0, 0,
		Jewellogo_Width, Jewellogo_Height,
		LOGO_LOC_X, LOGO_LOC_Y, 1L);

	XDrawImageString(xw_display, xw_window, VerGC, VER_LOC_X,
		VER_LOC_Y+((VerFont->ascent)*3/2),
		VerString, strlen(VerString));

	Redraw_Text();
	Redraw_Game_Board();
	XFlush(xw_display);
	}

void Game_Timeout()
	{
	if (!Paused())
		{
		Logic_Timeout();
		XFlush(xw_display);
		}
	if (JewelState==GAME)
		{ xw_set_timer(Get_Speed_ms()); }
	}

/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/


enum JStates { GAME, INTRO, HELP, HIGHSCORE, ICON, NULL_STATE };
extern enum JStates JewelState;

/* functions in game.c */
extern void Start_New_Game();


/* window shape:

 ######## ###
 ######## ###
 ######## ###
 ######## ###
 ######## ###
 ########
 ########
 ########
 ########
 ######## SCORE     6350
 ######## LIVES        2
 ######## SPEED    1.500
 ######## STAGE        0
 ######## REST        50
 ######## SOUND       ON

 Jewel 
 Version 1.0 (5/22/92) By David Cooper and Jose Guterman

 */

#define MARGINX 10
#define MARGINY 10
  
#define SIZE_PIECE 40 /* should match the bitmaps */
#define BOARD_WIDTH (NUM_COLS+2) /* including side columns */
#define BOARD_HEIGHT (NUM_ROWS+1) /* including bottom row */
#define PREVIEW_WIDTH 3 /* including sides */
#define PREVIEW_HEIGHT (BLOCK_SIZE + 2) /* including top and bot */
#define SCORE_WIDTH 14 /* in characters */
#define SCORE_HEIGHT 6 /* in characters */
#define VER_FONT "-*-*-bold-o-*-*-18-*-*-*-p-*-iso8859-1"
#define HIGHSC_FONT "-*-*-bold-r-*-*-24-*-*-*-p-*-iso8859-1"
extern char *VerString;
#define SCORE_FONT "seven_seg"
#define ALT_SC_FONT "-*-*-bold-r-*-*-24-*-*-*-m-*-iso8859-1"

#define BRD_LOC_X (MARGINX)
#define BRD_LOC_Y (MARGINY)
#define BRD_WIDTH (BOARD_WIDTH * SIZE_PIECE)
#define BRD_HEIGHT (BOARD_HEIGHT * SIZE_PIECE)
#define PREV_LOC_X (BRD_LOC_X+SIZE_PIECE*(BOARD_WIDTH+1))
#define PREV_LOC_Y (BRD_LOC_Y)
#define LOGO_LOC_X (BRD_LOC_X+SIZE_PIECE/2)
#define LOGO_LOC_Y (BRD_LOC_Y+SIZE_PIECE*BOARD_HEIGHT+(SIZE_PIECE/2))
#define VER_LOC_X (BRD_LOC_X+SIZE_PIECE/2)
#define VER_LOC_Y (LOGO_LOC_Y+Jewellogo_Height)
#define START_LOC_Y 600
extern char *StartString;
#define SCORE_LOC_X (PREV_LOC_X)
#define SCORE_LOC_Y (PREV_LOC_Y+(SIZE_PIECE*PREVIEW_HEIGHT)+SIZE_PIECE)

#define SCREEN_X 650
#define SCREEN_Y 728

extern Pixmap LogoPM;
extern Pixmap Logo2PM;
extern GC LogoGC;
extern GC Logo2GC;

extern Pixmap BorderPM;
extern GC BorderGC;

extern Pixmap PiecesPM[];
extern GC PiecesGC[];

extern Pixel colors[];

extern Pixel green;
extern Pixel yellow;
extern Pixel black;
extern Pixel white;

extern int Score_Char_MHeight;
extern int Score_x_right;
extern XFontStruct *VerFont, *ScoreFont, *HighScoreFont;
extern GC ScoreGC;
extern GC HighScoreGC;
extern GC VerGC;

/* bitmap dimensions */
extern int Border_Width;
extern int Border_Height;
extern int Jewellogo2_Width;
extern int Jewellogo2_Height;
extern int Jewellogo_Width;
extern int Jewellogo_Height;

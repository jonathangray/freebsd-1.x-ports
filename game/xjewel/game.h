/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

/* functions declared in game.c */

extern void Draw_Piece(/* piece,x,y */);
extern void End_Game();
extern void Expose_Game();
extern void Flash_Pieces(/* p_remove, numflash, background */);
extern void Key_Bell();
extern void Melt_Down();
extern void Redraw_Add_Score(/* pts,mult */);
extern void Redraw_Game_Board();
extern void Redraw_Lives(/* Lives */);
extern void Redraw_Pause();
extern void Redraw_Rest(/* Rest */);
extern void Redraw_Score(/* Score */);
extern void Redraw_Sound();
extern void Redraw_Speed(/* Speed */);
extern void Redraw_Stage(/* Stage */);
extern void Start_New_Game();
extern void Game_Timeout();

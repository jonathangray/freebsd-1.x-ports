/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#define JEWELSCORE 300
#define POINTS(x) (300 + (x-3)*150)
#define DROP_POINTS 10

#define INITIAL_LIVES 3
#define DEF_SOUND FALSE

#define MAX_STAGE	25
#define PIECES_PER_STAGE 50

/* functions provided by panel */
extern void Reset_Score();
extern void Add_Raw_Score(/*pts,mult*/);
extern void Add_Score(/*pts,iteration*/);
extern void Reset_Lives();
extern void Dec_Lives();
extern void Reset_Score();
extern void Add_Raw_Score(/*pts,mult*/);
extern void Add_Score(/*pts,iteration*/);
extern int  Get_Score();
extern void Reset_Lives();
extern void Dec_Lives();
extern void Reset_Stage();
extern void Inc_Stage();
extern void Dec_Stage();
extern int  Get_Stage();
extern unsigned long Get_Speed_ms();
extern void Reset_Rest();
extern BOOL Dec_Rest(/*val*/);
extern void Reset_Pause();
extern void Set_Pause();
extern BOOL Toggle_Pause();
extern BOOL Paused();
extern BOOL Toggle_Sound();
extern BOOL Sound();
extern void Redraw_Text();
extern void New_Game();

/* functions provided by jewel */
extern void Redraw_Score();
extern void Redraw_Add_Score(/*pts,mult*/);
extern void Redraw_Lives();
extern void Redraw_Speed();
extern void Redraw_Stage();
extern void Redraw_Rest();
extern void Redraw_Sound();
extern void Redraw_Pause();
extern void End_Game();

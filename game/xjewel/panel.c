/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#include "general.h"
#include "hscore.h"
#include "panel.h"

static int Score;
static int Lives;
static float Speed;
static int Stage;
static int Rest;
static BOOL _Sound = DEF_SOUND;
static BOOL _Paused;

static float Speeds[]=
	{
	1.500,		/* Stage 1 */
	1.250,		/* Stage 2 */
	1.000,		/* Stage 3 */
	0.750,		/* Stage 4 */
	0.500,		/* Stage 5 */
	0.250,		/* Stage 6 */
	0.2375,		/* Stage 7 */
	0.2250,		/* Stage 8 */
	0.2125,		/* Stage 9 */
	0.2000,		/* Stage 10 */
	0.1875,		/* Stage 11 */
	0.1750,		/* Stage 12 */
	0.1625,		/* Stage 13 */
	0.1500,		/* Stage 14 */
	0.1375,		/* Stage 15 */
	0.1250,		/* Stage 16 */
	0.1125,		/* Stage 17 */
	0.1000,		/* Stage 18 */
	0.0875,		/* Stage 19 */
	0.0750,		/* Stage 20 */
	0.0625,		/* Stage 21 */
	0.0500,		/* Stage 22 */
	0.0375,		/* Stage 23 */
	0.0250,		/* Stage 24 */
	0.0125,		/* Stage 25 */
	0.0000,		/* Stage 26 */
	0.0000		/* Stage 27 */
	};

void Reset_Score()
	{
	/* set to zero */
	Score=0;
	Redraw_Score(Score);
	}

void Add_Raw_Score(pts,mult)
int pts, mult;
	{
	/* a pts is any value */
	/*  mult is simply the number of times pts is to be added */
	/* Redraw_Add_Score(pts,mult);*/
	Score+=(pts*mult);
	Redraw_Score(Score);
	}


void Add_Score(pts,iteration)
int pts, iteration;
	{
	int Mult=1<<(iteration-1);
	/* mult is simply the iteration of match algorithm */
	Redraw_Add_Score(pts,Mult);
	Score+=(pts*Mult);
	Redraw_Score(Score);
	}

int Get_Score()
	{
	return(Score);
	}

void Reset_Lives()
	{
	/* set to default */
	Lives=INITIAL_LIVES;
	Redraw_Lives(Lives);
	}

void Dec_Lives()
	{
	/* obvious */
	Lives--;
	Redraw_Lives(Lives);
	if (Lives == 0)		/* Sorry my friend you are history... */
		{
		End_Game();
		}
	}

void Reset_Stage()
	{
	/* set stage and speed */
	Stage=1;
	Speed=Speeds[0];
	Redraw_Speed(Speed);
	Redraw_Stage(Stage);
	}

void Inc_Stage()
	{
	/* set stage and speed */
	Stage++;
	if (Stage > MAX_STAGE)
		{
		Stage = 1;
		}
	Speed=Speeds[Stage-1];
	Redraw_Speed(Speed);
	Redraw_Stage(Stage);
	}

void Dec_Stage()
	{
	/* set stage and speed */
	if (Stage > 1)
		{
		Stage--;
		Speed=Speeds[Stage-1];
		Redraw_Speed(Speed);
		Redraw_Stage(Stage);
		}
	}

int Get_Stage()
	{
	return(Stage);
	}

unsigned long Get_Speed_ms()
	{
	return((unsigned long)(Speed*1000));
	}

void Reset_Rest()
	{
	Rest=PIECES_PER_STAGE;
	Redraw_Rest(Rest);
	}

BOOL Dec_Rest(val)
	{
	BOOL new_stage;

	new_stage=FALSE;
	Rest = Rest-val;
    if (Rest <= 0)           /* Time for next stage */
        {
		Inc_Stage();
        Rest = PIECES_PER_STAGE + Rest;
		new_stage=TRUE;
        }
    Redraw_Rest(Rest);
	return(new_stage);
    }

void Reset_Pause()
	{
	_Paused=FALSE;
	Redraw_Pause();
	}

void Set_Pause()
	{
	_Paused=TRUE;
	Redraw_Pause();
	}

BOOL Toggle_Pause()
	{
	_Paused=((_Paused) ? FALSE : TRUE);
	Redraw_Pause();
	return(_Paused);
	}

BOOL Paused()
	{
	return(_Paused);
	}

BOOL Toggle_Sound()
	{
	_Sound=((_Sound) ? FALSE : TRUE);
	Redraw_Sound();
	return(_Sound);
	}

BOOL Sound()
	{
	return(_Sound);
	}

void Redraw_Text()
	{
	/* do all of above */
	Redraw_Score(Score);
	Redraw_Lives(Lives);
	Redraw_Speed(Speed);
	Redraw_Stage(Stage);
	Redraw_Rest(Rest);
	Redraw_Sound();
	Redraw_Pause();
	}

void New_Game()
	{
	Reset_Score();
	Reset_Lives();
	Reset_Stage();
	Reset_Rest();
	Reset_Pause();
	}

/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#include <stdio.h>

#ifndef SYSV
#   include <string.h>
#   ifdef VMS
#	include <file.h>
#   else
#	include <sys/file.h>
#   endif
#else
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <fcntl.h>
#   include <strings.h>
#endif

#ifdef GETPWENT
#   include <pwd.h>
#endif

#include <errno.h>

#include "general.h"
#include "hscore.h"
#include "xhscore.h"

int hscorefd;
FILE *hscorefile;


int num_high_scores;
struct record high_scores[MAX_HIGH_SCORES];

static int Stage, Score;

void Write_Scores()
	{
	int i;

	rewind(hscorefile);
	fflush(hscorefile);
	fprintf(hscorefile, "%d\n", num_high_scores);
	for (i=0; i<num_high_scores; i++)
		{
		fprintf(hscorefile, "%s %d %d\n", high_scores[i].name,
				high_scores[i].stage, high_scores[i].score);
		}
	fflush(hscorefile);
	}

void Read_Scores()
	{
	int i;

	rewind(hscorefile);
	fscanf(hscorefile, "%d\n", &num_high_scores);
	for (i=0; i<num_high_scores; i++)
		{
		fscanf(hscorefile, "%s %d %d\n", high_scores[i].name,
				&high_scores[i].stage, &high_scores[i].score);
		high_scores[i].name[MAX_NAME_LENGTH] = '\0';
		}
	}

void File_Lock()
	{
	}

void File_Unlock()
	{
	}

BOOL Open_High_Score_File()
	{
	if ((hscorefd = open(HSCORE_FILE, O_RDWR)) <0)
		{
		if (errno == ENOENT) 
			{
			/* File  does no exist */
			if ((hscorefd = open(HSCORE_FILE, O_RDWR|O_CREAT, 0666)) <0)
				{
				/* Unsuccesful creation of file */
				perror("CANNOT OPEN HIGHSCOREFILE:");
				return(FALSE);
				}
			else
				{
				/* Succesful creation of new file */
				File_Lock();
				hscorefile = fdopen(hscorefd, "r+");
				num_high_scores = 0;
				Write_Scores();
				return(TRUE);
				}
			}
		else
			{
			/* File exist and unsuccesful open */
			return(FALSE);
			}
		}
	else
		{
		/* File exists and succesful open */
		File_Lock();
		hscorefile = fdopen(hscorefd, "r+");
		Read_Scores();
		return(TRUE);
		}
	}

void Close_High_Score_File()
	{
	File_Unlock();
	fclose(hscorefile);
	close(hscorefd);
	}

void Add_High_Score(i)
int i;
	{
	high_scores[i].stage = Stage;
	high_scores[i].score = Score;
#ifndef GETPWENT
    /*printf("%s\n",cuserid(0l));*/
    memcpy(high_scores[i].name,cuserid(0l),8);
#else
	memcpy(high_scores[i].name,getpwuid(getuid())->pw_name,MAX_NAME_LENGTH+1);
#endif
	if (num_high_scores < MAX_HIGH_SCORES)
		{
		num_high_scores++;
		}
	Write_Scores();
	Draw_One_High_Score(i+1);
	}

void Update_High_Scores(NewStage, NewScore)
int NewStage, NewScore;
	{
	int i, j;
	BOOL updated;
	
	Stage=NewStage;
	Score=NewScore;

	if (!Open_High_Score_File())
		{
		printf("Could not open high scores file\n");
		}

	Show_High_Scores(1);

	updated = FALSE;
	for (i=0; i<num_high_scores; i++)
		{
		if (Score > high_scores[i].score)
			{
			for (j=MAX_HIGH_SCORES-2; j>=i; j--)
				{
				high_scores[j+1].stage = high_scores[j].stage;
				high_scores[j+1].score = high_scores[j].score;
				memcpy(high_scores[j+1].name, high_scores[j].name,
						MAX_NAME_LENGTH+1);
				}
			if (num_high_scores == MAX_HIGH_SCORES)
				{
				Wipeout_Last_High_Score();
				}
			Move_Down_High_Scores(i+1);
			/*ms_sleep(1000L);*/
			Add_High_Score(i);
			Show_High_Scores(i+1);
			updated = TRUE;
			break;
			}
		}

	if ((!updated) && (num_high_scores < MAX_HIGH_SCORES))
		{
		Add_High_Score(num_high_scores);
		}
	
	Close_High_Score_File();
	}

void Refresh_High_Scores()
	{
	if (!Open_High_Score_File())
		{
		printf("Could not open high scores file\n");
		}

	Show_High_Scores(1);
	Close_High_Score_File();
	}

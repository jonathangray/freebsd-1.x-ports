/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#define MAX_HIGH_SCORES 10
#define MAX_NAME_LENGTH 20
#ifndef HSCORE_FILE
#  define HSCORE_FILE "xjewel.scores"
#endif

extern int num_high_scores;
struct record {
	char name[MAX_NAME_LENGTH+1];
	int stage;
	int score;
	};
extern struct record high_scores[MAX_HIGH_SCORES];


/* functions provided by score */
extern void Update_High_Scores(/*Stage, Score*/);
extern void Refresh_High_Scores();

/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "golddig.h"

char *getenv() /* ,*sprintf() */;

#define NUMHIGH 15  /* Number of high scores that will be remembered */

/* Add a high score to the high score list */
void add_score(whydie)
char *whydie;       /* Textual description of reason for death */
{
  /* Structure containing top game results */
  struct {
    int score;      /* Final score */
    int slev,elev;  /* Starting and ending level */
    int uid;        /* Player account uid */
    char desc[80];  /* Text description */
  } tops[NUMHIGH],next;
  FILE *sfile;      /* High score file */
  char buf[200];
  register int i;
  int numscore,cur,numgame;

  /* Generate name of high score file */
  sprintf(buf,"%s/scores",LIB);
  /* Open high score file */
  sfile = fopen(buf,"r");
  /* Set default values for number of games and high scores */
  numscore = 0;
  numgame = 0;
  /* If file is readable, load in old high score list */
  if(sfile != NULL) {
    /* Load a line on text */
    if(fgets(buf,200,sfile))
      sscanf(buf,"%d",&numgame);
    /* Extract score information from line */
    while(fgets(buf,200,sfile) && numscore < NUMHIGH) {
      sscanf(buf,"%d %d %d %d %[^\n]",&(next.score),&(next.slev),&(next.elev),
         &(next.uid),next.desc);
      tops[numscore] = next;
      numscore ++;
    }
    fclose(sfile);
  }

  /* Contruct the structure containing the score for this game */
  next.score = score;
  next.slev = levelstart;
  next.elev = levelnum;
  next.uid = getuid();
  sprintf(next.desc,"%s %s",getenv("USER"),whydie);
  cur = -1;
  /* Insert new score in old high score list */
  if(numscore < NUMHIGH || tops[NUMHIGH - 1].score < next.score) {
    /* Iterate through high score list */
    for(i = (numscore >= NUMHIGH ? NUMHIGH-2 : numscore-1);i >= 0;i--) {
      /* Look for place for insertion */
      if(next.score > tops[i].score)
        tops[i+1] = tops[i];    /* Move old scores down one place in list */
      else
        break;                  /* Found spot for insertion */
    }
    tops[i+1] = next;   /* Overwrite entry in high score list */
    cur = i+1;          /* Remember where new high score was inserted */
    /* Increment the number of high scores */
    if(numscore < NUMHIGH)
      numscore ++;
  }

  /* Increment and print the number of games played */
  numgame ++;
  printf("High scores after %d games played:\n",numgame);
  /* Print out new high score list */
  for(i=0;i<numscore;++i) {
    /* Flag new high score with a leading > */
    if(i == cur)
      putchar('>');
    else
      putchar(' ');
    printf("%s on level %d (started at %d).  Final score was %d.\n",
       tops[i].desc,tops[i].elev,tops[i].slev,tops[i].score);
  }
  /* If current game did not make it to the high score list, print it */
  /* afterwords */
  if(cur == -1) {
    puts("...");
    printf(">%s on level %d (started at %d).  Final score was %d.\n",
       next.desc,next.elev,next.slev,next.score);
  }

  /* Save new high score list to score file */
  sprintf(buf,"%s/scores",LIB);
  sfile = fopen(buf,"w");
  if(sfile == NULL) {
    perror(buf);
    return;
  }
  fprintf(sfile,"%d\n",numgame);
  for(i=0;i<numscore;++i)
    fprintf(sfile,"%d %d %d %d %s\n",tops[i].score,tops[i].slev,
         tops[i].elev,tops[i].uid,tops[i].desc);
  fclose(sfile);
}

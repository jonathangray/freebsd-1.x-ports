/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "golddig.h"

extern char badguy_bits[];
#include "bitmap/badguy2.bits"
#include "bitmap/badguy3.bits"

long random();

#define MAXBADGUY 30     /* Maximum number of bad guys */
#define MAXBADSCORE 20   /* Maximum amount of score given for */
                         /* killing bad guys */
int numbadguy = 0;       /* Total number of bad guys. */
struct thing_s badguys[MAXBADGUY];  /* Array describing state of all */
                                    /* bad guys */
char movetree[MAXLEVEL]; /* Direction that badguy should go at each location */
int badscore;            /* Score given during current level due to */
                         /* killing bad guys */

/* Graphics cursors for drawing the possible states of the badguys */
GC badguy1gc,badguy2gc,badguy3gc;

/* Initialize graphics cursors for bad guys */
void init_badguy()
{
  int gcfunc;

  /* Decide whether to use GXand or GXor depending on screen type */
  if((BlackPixel(disp,0) & WhitePixel(disp,0)) == BlackPixel(disp,0))
    gcfunc = GXand;
  else
    gcfunc = GXor;
  /* Generate graphics cursors for drawing bad guy */
  badguy1gc = makegc(gcfunc,badguy_bits);
  badguy2gc = makegc(gcfunc,badguy2_bits);
  badguy3gc = makegc(gcfunc,badguy3_bits);
}

/* Initialize data structure for bad guys from level data */
void start_badguy()
{
  register int x,y,pos;

  /* Reset the limit on bad guy score given */
  badscore = 0;
  /* Initialize number of bad guys to 0 */
  numbadguy = 0;
  pos = 0;
  /* Iterate through each position in level array */
  for(x=0;x<xsize;++x)
    for(y=0;y<ysize;++y) {
      /* Check if level holds a bad guys block at that position */
      if(level[pos] == BADGUY) {
        /* Replace bad guys block with space */
        level[pos] = SPACE;
        if(numbadguy < MAXBADGUY) {
          /* Add bad guy to global array.  Note the multiplication by */
          /* 2 when producing position. */
          badguys[numbadguy].xstart = badguys[numbadguy].xpos = x << 1;
          badguys[numbadguy].ystart = badguys[numbadguy].ypos = y << 1;
          badguys[numbadguy].dir = STAND;
          /* Bad guys start holding nothing */
          badguys[numbadguy].hold = SPACE;
          numbadguy ++;
        }
      }
      pos ++;
    }
}

/* Draw all the bad guys out to the game window */
void draw_badguys()
{
  register int i;
  GC drawgc;

  /* Iterate through each bad guy */
  for(i=0;i<numbadguy;++i) {
    /* If bad guy is horizontally offset, use the second graphics */
    /* cursor */
    if(badguys[i].xpos & 1)
      drawgc = badguy2gc;
    /* If bad guy is vertically offset, use the third graphics cursor */
    else if(badguys[i].ypos & 1)
      drawgc = badguy3gc;
    /* Otherwise use the normal graphics cursor */
    else
      drawgc = badguy1gc;
    /* Fill rectangle surrounding bad guy */
    XFillRectangle(disp,wind,drawgc,badguys[i].xpos << 3,
                   badguys[i].ypos << 3,16,16);
  }
}

/* Return 1 if the specified position overlaps a bad guy, 0 otherwise. */
/* Another parameter is provided so that this routine can ignore one */
/* bad guy. */
int overlap_badguy(x,y,num)
register int x,y;   /* Position which will be checked for overlap. */
register int num;   /* Number of bad guy to ignore.  Use a -1 for no */
                    /* ignoring. */
{
  register int i,d;

  /* Iterate through each bad guy */
  for(i=0;i<numbadguy;++i) {
    /* If this bad guy is the one that I am supposed to ignore, */
    /* continue to the next */
    if(i == num)
      continue;
    /* Compute horizontal distance between position and bad guy and */
    /* check it.  Things are two positions wide in each direction */
    d = x - badguys[i].xpos;
    if(d < -1 || d > 1)
      continue;
    /* Check vertical distance */
    d = y - badguys[i].ypos;
    if(d < -1 || d > 1)
      continue;
    /* Found overlap, return a 1 */
    return(1);
  }
  /* Could not find overlap */
  return(0);
}

/* Erase and redraw all badguys who have moved in the previous */
/* movement.  Movement is detected when (xpos,ypos) is different from */
/* (xold,yold). */
void drawmove_badguys()
{
  register int x,y,i;
  GC drawgc;

  /* iterate through each bad guy */
  for(i=0;i<numbadguy;++i) {
    /* do not erase bad guys who did not move */
    if(! badguys[i].redraw)
      continue;
    /* get position of bad guy in normal coordinates */
    x = badguys[i].xold >> 1;
    y = badguys[i].yold >> 1;
    /* draw block underneath bad guy */
    draw_block(x,y);
    /* if horizontally offset, draw block to the right */
    if(badguys[i].xold & 1)
      draw_block(x + 1,y);
    /* if vertically offset, draw block below */
    if(badguys[i].yold & 1)
      draw_block(x,y + 1);
  }

  /* iterate through each bad guy */
  for(i=0;i<numbadguy;++i) {
    /* do not draw bad guys who did not move */
    if(! badguys[i].redraw)
      continue;
    /* Put badguy coordinates in registers */
    x = badguys[i].xpos;
    y = badguys[i].ypos;
    /* if bad guy is horizontally offset, use the second graphics */
    /* cursor */
    if(x & 1)
      drawgc = badguy2gc;
    /* If bad guy is vertically offset, use the third graphics cursor */
    else if(y & 1)
      drawgc = badguy3gc;
    /* Otherwise use the normal graphics cursor */
    else
      drawgc = badguy1gc;
    /* Fill rectangle surrounding bad guy */
    XFillRectangle(disp,wind,drawgc,x << 3,y << 3,16,16);
  }
}

/* Maximum size for FIFO queue used by breadth first search algorithm */
#define QSIZE 1000

/* Regenerate the movement tree used by bad guys when determining */
/* movement.  The basic algorithm is a breadth first search of the */
/* graph produced by interpreting the moveallow array as a directed */
/* graph.  The root of the tree is the player position.  The result of */
/* this algorithm is that the shortest path from each position to the */
/* player is described by the directions in the movement tree. */
void regen_tree()
{
  register int head,tail,lpos,dist;
  /* This array is used for the queue of graph nodes.  The head and */
  /* tail variables describe the head and tail of the queue in the */
  /* array. */
  struct goals_s {
    short x,y,dir;
  } goals[QSIZE];

  /* Do a fast pre-initialization of the movement tree */
  dist = xsize * ysize;
  for(lpos=0;lpos<dist;++lpos) {
    /* Zero out each position.  A 0 in a position signifies that the */
    /* direction of movement for that position is unknown. */
    movetree[lpos] = 0;
    /* If can only move in one direction, set movetree value */
    /* immediately.  Do not set it if is a force into a STOPBAD space */
    /* to allow bad guys to try to run over holes.  This loop screws up the */
    /* search algorithm and the result is that bad guys will not jump */
    /* off of things.  It makes the search run much faster, and the */
    /* bad guys were too smart otherwise. */
    if((moveallow[lpos] & FORCEDOWN) &&
       ! (fast_lookup[level[lpos + 1]].code & STOPBAD))
      movetree[lpos] = MOVEDOWN;
    if((moveallow[lpos] & FORCEUP) &&
       ! (fast_lookup[level[lpos - 1]].code & STOPBAD))
      movetree[lpos] = MOVEUP;
    if((moveallow[lpos] & FORCERIGHT) &&
       ! (fast_lookup[level[lpos + ysize]].code & STOPBAD))
      movetree[lpos] = MOVERIGHT;
    if((moveallow[lpos] & FORCELEFT) &&
       ! (fast_lookup[level[lpos - ysize]].code & STOPBAD))
      movetree[lpos] = MOVELEFT;
    /* If no movement is possible, set a -1 in the movement tree. */
    if(moveallow[lpos] == 0)
      movetree[lpos] = -1;
  }
  /* Initialize the head and tail of the FIFO queue. */
  head = 0;
  tail = 1;
  /* Set the first goal node to be the player */
  goals[0].x = player.xpos >> 1;
  goals[0].y = player.ypos >> 1;
  goals[0].dir = -1;    /* Make sure every direction is possible */
  /* While there are still goal nodes, continue */
  while(head != tail) {
    /* Compute position of position of goal */
    lpos = goals[head].x * ysize + goals[head].y;
    /* If the suggested movement direction is valid and the movement */
    /* has for that position has not been set, set it. */
    if(movetree[lpos] == 0 && (moveallow[lpos] & goals[head].dir) != 0) {
      movetree[lpos] = goals[head].dir;
      /* Suggest that the block to the left has to rightward pointer */
      if(goals[head].x > 0 && movetree[lpos - ysize] == 0)

/* Add a suggested movement direction to the FIFO queue */
#define ADDGOAL(dx,dy,ndir)  { goals[tail].x = goals[head].x + dx; \
                             goals[tail].y = goals[head].y + dy; \
                             goals[tail++].dir = ndir; \
                             if(tail == QSIZE) tail = 0; }

        ADDGOAL(-1,0,MOVERIGHT)
      /* Suggest that the block to the right has a leftware pointer */
      if(goals[head].x < xsize - 1 && movetree[lpos + ysize] == 0)
        ADDGOAL(1,0,MOVELEFT)
      /* Suggest that the block above has a downward pointer */
      if(goals[head].y > 0 && movetree[lpos - 1] == 0)
        ADDGOAL(0,-1,MOVEDOWN)
      /* Suggest that the block below has an upward pointer */
      if(goals[head].y < ysize - 1 && movetree[lpos + 1] == 0)
        ADDGOAL(0,1,MOVEUP)
    }
    /* Remove current goal node from head of queue */
    head ++;
    if(head == QSIZE)
      head = 0;
  }
}

/* Move all the bad guys one position */
void move_badguys()
{
  int i,x,y,allow;
  register int lpos;
  enum directs dir;
  register char curchar,below;

  /* Iterate through all the bad guys */
  for(i=0;i<numbadguy;++i) {
    /* Get old position for bad guys */
    x = badguys[i].xold = badguys[i].xpos;
    y = badguys[i].yold = badguys[i].ypos;
    /* Initially assume that the badguy does not need to be redrawn */
    badguys[i].redraw = 0;
    /* Get the character underneath the bad guy */
    lpos = (x >> 1)*ysize + (y >> 1);
    curchar = level[lpos];
    /* Get the character below the bad guy.  If it is off the level, */
    /* assume it is a stone */
    if(y < (ysize - 1) << 1)
      below = level[lpos + 1];
    else
      below = STONE;

    /* If the current character is a killer block, move the bad guy to */
    /* its starting position */
    if(fast_lookup[curchar].code & KILLIN) {
      /* Prevent getting too many points from kill bad guys on a */
      /* single level */
      if(badscore < MAXBADSCORE) {
        /* Increment the score */
        score += 2;
        badscore += 2;
      }
      /* Redraw the score line */
      draw_score();
      /* Move the bad guy back to its start */
      badguys[i].xpos = badguys[i].xstart;
      badguys[i].ypos = badguys[i].ystart;
      lpos = (badguys[i].xpos >> 1)*ysize + (badguys[i].ypos >> 1);
      /* Prevent a bad guy from forming on top of another */
      while(overlap_badguy(badguys[i].xpos,badguys[i].ypos,i) ||
            level[lpos] != SPACE) {
        badguys[i].xpos = random() % xsize;
        badguys[i].ypos = random() % ysize;
        lpos = (badguys[i].xpos >> 1)*ysize + (badguys[i].ypos >> 1);
      }
      /* Redraw bad guy */
      badguys[i].redraw = 1;
      /* Destroy whatever the bad guy was holding */
      if(badguys[i].hold != SPACE) {
        if(fast_lookup[badguys[i].hold].code & TREASURE) {
          goldleft --;
          /* If that was the last gold piece, escape ladder and other */
          /* stuff may need to appear. */
          if(goldleft == 0) {
            regen_allow();      /* Regenerate the allowable movement array */
            redrawall();        /* Refresh the entire screen */
          }
        }
        badguys[i].hold = SPACE;
      }
      /* Continue to the next bad guy */
      continue;
    }

    /* If the bad guy is stuck in a STOPBAD block, do not move him */
    if((x & 1) == 0 && (y & 1) == 0 &&
       (fast_lookup[curchar].code & STOPBAD)) {
      /* Redraw bad guy since underlying block gets redrawn occasionally */
      badguys[i].redraw = 1;      
      continue;
    }

    /* Check if the bad guy is above a hole. */
    if((x & 1) == 0 && (y & 1) == 0 && fast_lookup[below].code & STOPBAD) {
      /* If the hole is premature, fill it */
      if(below >= HOLE1 && below <= HOLE1+2)
        fill_hole(x >> 1,(y >> 1) + 1);
    }

    /* Complete previous movement */
    if((x & 1) != 0 || (y & 1) != 0)
      dir = badguys[i].dir;

    /* Can only drop things when at a non-offset position and when */
    /* there is space underneath */
    else if(curchar == SPACE && badguys[i].hold != SPACE && 
       /* Drop stuff with a 1 in 20 chance */
       random() % 20 == 0)
      dir = PUTDOWN;

    /* Compute next hopeful direction of movement, since badguy is at */
    /* an even spot */
    else {
      /* Get allowable movement direction */
      allow = moveallow[lpos];
      /* Prevent bad guy from spontaneously switching direction */
      if(badguys[i].dir == UP)
        allow &= ~MOVEDOWN;
      else if(badguys[i].dir == DOWN)
        allow &= ~MOVEUP;
      else if(badguys[i].dir == LEFT)
        allow &= ~MOVERIGHT;
      else if(badguys[i].dir == RIGHT)
        allow &= ~MOVELEFT;

      /* Prevent bad guy from trying to run into somebody else */
      if((allow & MOVEUP) &&
         overlap_badguy(badguys[i].xpos,badguys[i].ypos - 2,i))
        allow &= ~MOVEUP;
      /* Prevent bad guy from trying to run into somebody else */
      if((allow & MOVEDOWN) &&
         overlap_badguy(badguys[i].xpos,badguys[i].ypos + 2,i))
        allow &= ~MOVEDOWN;
      /* Prevent bad guy from trying to run into somebody else */
      if((allow & MOVELEFT) &&
         overlap_badguy(badguys[i].xpos - 2,badguys[i].ypos,i))
        allow &= ~MOVELEFT;
      /* Prevent bad guy from trying to run into somebody else */
      if((allow & MOVERIGHT) &&
         overlap_badguy(badguys[i].xpos + 2,badguys[i].ypos,i))
        allow &= ~MOVERIGHT;
      
      /* Try to move according to movement tree */
      if((allow & MOVEUP) && movetree[lpos] == MOVEUP)
        dir = UP;
      else if((allow & MOVEDOWN) && movetree[lpos] == MOVEDOWN)
        dir = DOWN;
      else if((allow & MOVELEFT) && movetree[lpos] == MOVELEFT)
        dir = LEFT;
      else if((allow & MOVERIGHT) && movetree[lpos] == MOVERIGHT)
        dir = RIGHT;
      
      /* Try circling clockwise around player */
      else if((allow & MOVEUP) && badguys[i].xpos < player.xpos)
        dir = UP;
      else if((allow & MOVEDOWN) && badguys[i].xpos > player.xpos)
        dir = DOWN;
      else if((allow & MOVELEFT) && badguys[i].ypos > player.ypos)
        dir = LEFT;
      else if((allow & MOVERIGHT) && badguys[i].ypos < player.ypos)
        dir = RIGHT;

      /* Try approaching player */
      else if((allow & MOVEUP) && badguys[i].ypos > player.ypos)
        dir = UP;
      else if((allow & MOVEDOWN) && badguys[i].ypos < player.ypos)
        dir = DOWN;
      else if((allow & MOVELEFT) && badguys[i].xpos > player.xpos)
        dir = LEFT;
      else if((allow & MOVERIGHT) && badguys[i].xpos < player.xpos)
        dir = RIGHT;
      
      /* Try moving in any possible direction */
      else if(allow & MOVEUP)
        dir = UP;
      else if(allow & MOVEDOWN)
        dir = DOWN;
      else if(allow & MOVELEFT)
        dir = LEFT;
      else if(allow & MOVERIGHT)
        dir = RIGHT;

      /* If totally stuck, just stand in place */
      else
        dir = STAND;
    }

    /* Reverse bad guys direction is REVERSE item is held by player */
    if((fast_lookup[player.hold].code & REVERSE) &&
       (x & 1) == 0 && (y & 1) == 0)
      switch(dir) {
      case LEFT:  dir = RIGHT; break;
      case RIGHT: dir = LEFT;  break;
      case UP:    dir = DOWN;  break;
      case DOWN:  dir = UP;    break;
      }
    
    /* Execute computed movement. */
    if(movething(&(badguys[i]),dir,i) == 3) {
      /* If the bad guy dropped something, the block will have been */
      /* overwritten and now the bad guy needs to be redrawn */
      badguys[i].redraw = 1;
      break;
    }

    /* Redraw bad guy if it has moved */
    if(x != badguys[i].xpos || y != badguys[i].ypos)
      badguys[i].redraw = 1;
  }
  /* Redraw bad guys in new positions */
  drawmove_badguys();
}

/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "golddig.h"

long random();

#define MAXHOLE 100     /* Maximum number of holes at once */

int holedecay[] = {2,4,6,62,66,70,74};  /* Number of ticks after which */
                                        /* hole will change form. */
int numholes;           /* Total number of holes */
/* Structure to describe a hole */
struct {
  int x,y;      /* Position of hole */
  int ticmade;  /* Value of curtick when hole was created.  Used to */
                /* time hole transitions. */
} holes[MAXHOLE];   /* Array for holes. */

/* Compute the allowable directions of movement out of a block.  The */
/* value in the moveallow array is updated at that position. */
void allow_posit(x,y)
register int x,y;       /* Position of block which is to be checked */
{
  register int pos,code,code2,allow;

  /* Get position of block in level array */
  pos = x*ysize + y;
  /* Initially assume that all directions of movement are acceptable */
  allow = MOVEUP | MOVEDOWN | MOVELEFT | MOVERIGHT;
  /* Prevent movement off of the level into nowhere land */
  if(x == 0)
    allow &= ~MOVELEFT;
  if(y == 0)
    allow &= ~MOVEUP;
  if(x == xsize - 1)
    allow &= ~MOVERIGHT;
  if(y == ysize - 1)
    allow &= ~MOVEDOWN;
  
  /* Get the control code for the block at the position */
  code = fast_lookup[level[pos]].code;
  /* Use the control code for space if the block is inactive */
  if((code & INACTIVE) && goldleft > 0)
    code = fast_lookup[SPACE].code;
  /* Use the control bits for the block to reduce the allowable */
  /* movement directions */
  if(! (code & ULEAVE))
    allow &= ~MOVEUP;
  if(! (code & DLEAVE))
    allow &= ~MOVEDOWN;
  if(! (code & LLEAVE))
    allow &= ~MOVELEFT;
  if(! (code & RLEAVE))
    allow &= ~MOVERIGHT;

  /* Check block to the left to make sure that it is possible to enter */
  /* that block from the current position. */
  if(x > 0) {
    /* Put control code for block to the left into register */
    code2 = fast_lookup[level[pos - ysize]].code;
    if((code2 & INACTIVE) && goldleft > 0)
      code2 = fast_lookup[SPACE].code;
    /* Determine if it possible to enter */
    if(! (code2 & HENTER))
      allow &= ~MOVELEFT;
    /* Prevent falling back into it if something can walk back from it */
    if((code2 & RLEAVE) && ! (code2 & CANFALL))
      code &= ~LFALL;
  }
  /* Check block to the right */
  if(x < xsize - 1) {
    code2 = fast_lookup[level[pos + ysize]].code;
    if((code2 & INACTIVE) && goldleft > 0)
      code2 = fast_lookup[SPACE].code;
    /* Determine if it possible to enter */
    if(! (code2 & HENTER))
      allow &= ~MOVERIGHT;
    /* Prevent falling back into it if something can walk back from it */
    if((code2 & LLEAVE) && ! (code2 & CANFALL))
      code &= ~RFALL;
  }
  /* Check block above */
  if(y > 0) {
    code2 = fast_lookup[level[pos - 1]].code;
    if((code2 & INACTIVE) && goldleft > 0)
      code2 = fast_lookup[SPACE].code;
    /* Determine if it possible to enter */
    if(! (code2 & VENTER))
      allow &= ~MOVEUP;
    /* Prevent falling back into it if something can walk back from it */
    if((code2 & DLEAVE) && ! (code2 & CANFALL))
      code &= ~UFALL;
  }
  /* Check block below */
  if(y < ysize - 1) {
    code2 = fast_lookup[level[pos + 1]].code;
    if((code2 & INACTIVE) && goldleft > 0)
      code2 = fast_lookup[SPACE].code;
    /* Determine if it possible to enter */
    if(! (code2 & VENTER))
      allow &= ~MOVEDOWN;
    /* Prevent falling back into it if something can walk back from it */
    /* Note that things will stick up if there is upward gravity */
    /* pointing into downward gravity. */
    if(code2 & ULEAVE)
      code &= ~DFALL;
  }

  /* Add gravity force */
  if((code & DFALL) && (allow & MOVEDOWN))
    allow |= FORCEDOWN;
  if((code & UFALL) && (allow & MOVEUP))
    allow |= FORCEUP;
  if((code & LFALL) && (allow & MOVELEFT))
    allow |= FORCELEFT;
  if((code & RFALL) && (allow & MOVERIGHT))
    allow |= FORCERIGHT;

  /* Store value back into moveallow array */
  moveallow[pos] = allow;
}

/* Call allow_posit on a position and all surrounding positions */
void allow_area(x,y)
int x,y;        /* Position to call allow_posit at */
{
  allow_posit(x,y);
  if(x < xsize - 1)
    allow_posit(x+1,y);
  if(x > 0)
    allow_posit(x-1,y);
  if(y < ysize - 1)
    allow_posit(x,y+1);
  if(y > 0)
    allow_posit(x,y-1);
}

/* Regenerate entire moveallow array */
void regen_allow()
{
  int x,y;

  /* Iterate through every possible position and call allow_posit on */
  /* it. */
  for(x=0;x<xsize;++x)
    for(y=0;y<ysize;++y)
      allow_posit(x,y);
}

/* Form a hole. */
void make_hole(x,y,code)
int x,y;        /* Position where hole is to be formed */
long code;      /* Code of item held while digging */
{
  register int pos;

  /* Compute position in level array where hole is to be created */
  pos = x*ysize + y;
  /* Make sure that the position is inside the level array */
  if(x < 0 || x >= xsize || y < 1 || y >= ysize)
    return;
  /* Check for reverse shovel */
  if(code & RSHOVEL) {
    /* Make sure that there is space is the specified position */
    if(level[pos] != SPACE)
      return;
    /* Replace space with brick */
    setchar(x,y,BRICK);
  }
  else {
    /* Make sure that the block can be dug */
    if(! (fast_lookup[level[pos]].code & CANDIG) ||
       /* Make sure that the block above it allows for digging */
       /* underneath. */
       ! (fast_lookup[level[pos - 1]].code & DIGUND))
      return;
    
    /* Check for power shovel */
    if(code & PSHOVEL)
      setchar(x,y,SPACE);
    else {
      /* Prevent the creation of too many holes */
      if(numholes >= MAXHOLE)
        return;
      /* Replace the character at the position with the first hole block */
      setchar(x,y,HOLE1);
      /* Add that hole to the hole array */
      holes[numholes].x = x;
      holes[numholes].y = y;
      holes[numholes].ticmade = curtick;
      numholes ++;
    }
  }
  /* Recompute the allowable movement direction for that position and */
  /* all surrounding positions */
  allow_area(x,y);
}

/* Fill up a hole with brick */
void fill_hole(x,y)
int x,y;        /* Position specifying hole */
{
  register int i;

  /* Look through all the holes until the right one is found */
  for(i=0;i<numholes;++i)
    if(holes[i].x == x && holes[i].y == y) {
      /* Change the block to a brick */
      setchar(holes[i].x,holes[i].y,BRICK);
      /* Recompute the allowable movement for the specified position */
      /* and all surrounding positions. */
      allow_area(holes[i].x,holes[i].y);
      /* Remove the hole from the holes array */
      holes[i] = holes[numholes - 1];
      numholes --;
      return;
    }
}

/* Age all holes by one clock tick */
void change_holes()
{
  register int i,j;

  /* Look for decaying holes.  Iterate through each hole. */
  for(i=0;i<numholes;++i) {
    /* Check if the hole is exactly at any transition point */
    for(j=0;j<7;++j)
      if(holes[i].ticmade + holedecay[j] == curtick) {
        /* If it is a normal transition point, just change the block */
        /* type */
        if(j < 6)
          setchar(holes[i].x,holes[i].y,(char) (HOLE1 + j + 1));
        /* If it is the last transition point, fill the hole in */
        else {
          fill_hole(holes[i].x,holes[i].y);
          /* Back up one hole since the hole that was at this position */
          /* has now been replaced by another. */
          i --;
        }
        break;
      }
  }
}

/* Try to move a thing (player or bad guy) in a given direction.  The */
/* structure describing the things is updated in place with the new */
/* position and apparent active command.  A code value is returned */
/* describing what type of movement actually occurred. */
int movething(thing,newdir,num)
register struct thing_s *thing;     /* Pointer to struct describing */
                                    /* current state of thing */
enum directs newdir;                /* New command being attempted */
int num;                            /* Number of bad guy or -1 for */
                                    /* player */
{
  register int lpos,code;
  int nx,ny;

  /* Compute useful local values */
  lpos = (thing->xpos >> 1)*ysize + (thing->ypos >> 1);
  code = fast_lookup[level[lpos]].code;
  if((code & INACTIVE) && goldleft > 0)
    code = fast_lookup[SPACE].code;

  /* Complete previous initiated movement */
  if((thing->xpos & 1) || (thing->ypos & 1)) {
    /* Allow partial horizontal movement to complete */
    if(thing->xpos & 1) {
      /* Continue in old direction */
      switch(thing->dir) {
      case LEFT:
        thing->xpos -= 1;
        thing->dir = LEFT;
        break;
      default:
        thing->xpos += 1;
        thing->dir = RIGHT;
        break;
      }
    }
    
    /* Allow partial vertical movement to complete */
    if(thing->ypos & 1) {
      /* Continue in old direction */
      switch(thing->dir) {
      case UP:
        thing->ypos -= 1;
        thing->dir = UP;
        break;
      default:
        thing->ypos += 1;
        thing->dir = DOWN;
        break;
      }
    }

    /* Pickup things which are laying around */
    lpos = (thing->xpos >> 1)*ysize + (thing->ypos >> 1);
    code = fast_lookup[level[lpos]].code;
    if(newdir != PUTDOWN && (code & PICKUP) && thing->hold == SPACE) {
      thing->hold = level[lpos];
      setchar(thing->xpos >> 1,thing->ypos >> 1,SPACE);
      allow_area(thing->xpos >> 1,thing->ypos >> 1);
    }

    /* Activate teleporter if standing on one and not holding an */
    /* anchor */
    if((code & TELEPORT) &&
       ! (fast_lookup[thing->hold].code & ANCHOR)) {
      do {
        lpos ++;
        thing->ypos += 2;
        if(thing->ypos >> 1 == ysize) {
          thing->ypos = 0;
          thing->xpos += 2;
          if(thing->xpos >> 1 == xsize) {
            lpos = 0;
            thing->xpos = 0;
          }
        }
      } while(! (fast_lookup[level[lpos]].code & TELEPORT));
    }

    /* Activate jump pad if standing on one and not holding an anchor */
    if((code & UPTWO) &&
       ! (fast_lookup[thing->hold].code & ANCHOR)) {
      thing->ypos -= 4;
      if(thing->ypos < 0)
        thing->ypos = 0;
    }

    /* Activate super jump pad */
    if(code & UPALL) {
      thing->ypos -= 4;
      /* Jump to the top of screen if not holding an anchor */
      if(thing->ypos < 0 ||
         ! (fast_lookup[thing->hold].code & ANCHOR))
        thing->ypos = 0;
    }

    return(1);
  }

  /* Allow creature to fall */
  if((moveallow[lpos] & FORCEALL) &&
     ! (fast_lookup[thing->hold].code & STOPFALL)) {
    /* Compute position offset in direction of fall */
    nx = ny = 0;
    if(moveallow[lpos] & FORCEDOWN) {
      ny = 1;
      thing->dir = DOWN;
    }
    if(moveallow[lpos] & FORCEUP) {
      ny = -1;
      thing->dir = UP;
    }
    if(moveallow[lpos] & FORCERIGHT) {
      nx = 1;
      thing->dir = RIGHT;
    }
    if(moveallow[lpos] & FORCELEFT) {
      nx = -1;
      thing->dir = LEFT;
    }

    /* Prevent falling into another thing */
    if(! overlap_badguy(thing->xpos + nx + nx,thing->ypos + ny + ny,num)) {
      /* Drop item behind if falling into a hole */
      if(level[lpos] == SPACE && thing->hold != SPACE) {
        /* Compute level position of space that thing is falling into */
        lpos = ((thing->xpos >> 1)+nx)*ysize + 
          ((thing->ypos >> 1)+ny);
        /* Check to see if next position is a hole */
        if(fast_lookup[level[lpos]].code & STOPBAD) {
          setchar(thing->xpos >> 1,thing->ypos >> 1,thing->hold);
          thing->hold = SPACE;
          allow_area(thing->xpos >> 1,thing->ypos >> 1);
        }
      }

      /* Increment position */
      thing->xpos += nx;
      thing->ypos += ny;
      return(2);
    }
  }

  /* Slow movement down if holding an anchor */
  if((fast_lookup[thing->hold].code & ANCHOR) && (random() % 4 != 0))
    newdir = STAND;

  /* Continue previous movement if it was at right angles to intended */
  /* movement, and intended movement is impossible */
  switch(newdir) {
  /* Check for possible upward movement. */
  case UP:
    if(! (moveallow[lpos] & MOVEUP)) {
      if(thing->dir == LEFT || thing->dir == RIGHT)
        newdir = thing->dir;
    }
    break;
  /* Check for possible downward movement. */
  case DOWN:
    if(! (moveallow[lpos] & MOVEDOWN)) {
      if(thing->dir == LEFT || thing->dir == RIGHT)
        newdir = thing->dir;
    }
    break;
  /* Check for possible left movement. */
  case LEFT:
    if(! (moveallow[lpos] & MOVELEFT)) {
      if(thing->dir == UP || thing->dir == DOWN)
        newdir = thing->dir;
    }
    break;
  /* Check for possible right movement. */
  case RIGHT:
    if(! (moveallow[lpos] & MOVERIGHT)) {
      if(thing->dir == UP || thing->dir == DOWN)
        newdir = thing->dir;
    }
    break;
  }

  /* By default, the thing is standing in place */
  thing->dir = STAND;
  /* Try to execute the intended movement */
  switch(newdir) {
  /* Put something down if that is the order */
  case PUTDOWN:
    if(level[lpos] == SPACE && thing->hold != SPACE) {
      setchar(thing->xpos >> 1,thing->ypos >> 1,thing->hold);
      thing->hold = SPACE;
      allow_area(thing->xpos >> 1,thing->ypos >> 1);
    }
    return(3);
  /* Dig holes left or right if that is the command.  The make_hole */
  /* command will fail if it is impossible to dig a hole at the */
  /* specified location. */
  case DIGLEFT:
    make_hole((thing->xpos >> 1) - 1,(thing->ypos >> 1) + 1,
              fast_lookup[thing->hold].code);
    if(fast_lookup[thing->hold].code & NSHOVEL) {
      make_hole((thing->xpos >> 1) - 2,(thing->ypos >> 1) + 1,
                fast_lookup[thing->hold].code);
      make_hole((thing->xpos >> 1) - 1,(thing->ypos >> 1) + 2,
                fast_lookup[thing->hold].code);
      make_hole((thing->xpos >> 1) - 2,(thing->ypos >> 1) + 2,
                fast_lookup[thing->hold].code);
    }
    return(4);
  case DIGRIGHT:
    make_hole((thing->xpos >> 1) + 1,(thing->ypos >> 1) + 1,
              fast_lookup[thing->hold].code);
    if(fast_lookup[thing->hold].code & NSHOVEL) {
      make_hole((thing->xpos >> 1) + 2,(thing->ypos >> 1) + 1,
                fast_lookup[thing->hold].code);
      make_hole((thing->xpos >> 1) + 1,(thing->ypos >> 1) + 2,
                fast_lookup[thing->hold].code);
      make_hole((thing->xpos >> 1) + 2,(thing->ypos >> 1) + 2,
                fast_lookup[thing->hold].code);
    }
    return(4);
  /* Since the thing is not falling or completing a previous movement, */
  /* it can start off in a new direction.  The moveallow array is used */
  /* to determine which directions are possible. */
  /* Check for possible upward movement. */
  case UP:
    if(moveallow[lpos] & MOVEUP) {
      thing->ypos -= 1;
      thing->dir = UP;
    }
    break;
  /* Check for possible downward movement. */
  case DOWN:
    if(moveallow[lpos] & MOVEDOWN) {
      thing->ypos += 1;
      thing->dir = DOWN;
    }
    break;
  /* Check for possible left movement. */
  case LEFT:
    if(moveallow[lpos] & MOVELEFT) {
      thing->xpos -= 1;
      thing->dir = LEFT;
    }
    break;
  /* Check for possible right movement. */
  case RIGHT:
    if(moveallow[lpos] & MOVERIGHT) {
      thing->xpos += 1;
      thing->dir = RIGHT;
    }
    break;
  }
  return(0);
}

/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

/* Modified by Josh Siegel to work with NeWS/X11 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <sys/time.h>
#include <signal.h>
#include "golddig.h"

extern char player_bits[];
#include "bitmap/fly.bits"
#include "bitmap/hang1.bits"
#include "bitmap/hang2.bits"
#include "bitmap/up1.bits"
#include "bitmap/up2.bits"
#include "bitmap/left1.bits"
#include "bitmap/left2.bits"
#include "bitmap/right1.bits"
#include "bitmap/right2.bits"

long random();

#define EVMASK KeyPressMask | ExposureMask | ButtonPressMask | FocusChangeMask

int newlevel = 0;       /* Non-zero if a new level was just drawn */
struct itimerval cycletime; /* Structure used when setting up timer */
/* These are the graphics cursors used for drawing the player at */
/* various times. */
GC standgc,flygc,hang1gc,hang2gc,up1gc,up2gc;
GC left1gc,left2gc,right1gc,right2gc;

enum directs curorder = STAND;  /* Current order which player has */
                                /* typed at the keyboard. */

/* Plug into original block type definitions in shared.c */
extern struct symbs_s symbs[];
extern int numholes;        /* Total number of holes */

/* This routine is called whenever the player dies. */
void died(whydie)
char *whydie;       /* Textual description of reason for death */
{
  /* Prevent timer from firing inside of sleep */
  cycletime.it_value.tv_sec = cycletime.it_interval.tv_sec = 10;
  setitimer(ITIMER_REAL,&cycletime,(struct itimerval *) NULL);
  signal(SIGALRM,SIG_DFL);      /* Turn off timer signal */
  XSync(disp,False);            /* Synchronize with display */
  if(strcmp(whydie,"was abandoned"))
    sleep(2);                   /* Pause for 2 seconds to let player */
                                /* see situation */
  xend();                       /* Terminate X windows */
  /* Add score to high score list */
  add_score(whydie);
  exit(0);
}

/* Redraw the player.  The graphics cursors all use the GXor function */
/* so they will not erase what is underneath. */
void draw_player()
{
  GC drawgc;
  register int lpos,code;

  /* Get position of level array of player */
  lpos = (player.xpos >> 1)*ysize + (player.ypos >> 1);
  /* Get the control code describing block underneath player */
  code = fast_lookup[level[lpos]].code;
  /* If the block is inactive, use the code for empty space */
  if((code & INACTIVE) && goldleft > 0)
    code = fast_lookup[SPACE].code;

  /* Compute the graphics cursor appropriate to the player's current */
  /* state */
  drawgc = NULL;
  /* Check if player is hanging from a rope */
  if((player.ypos & 1) == 0) {
    if((code & DLEAVE) && ! (code & (ULEAVE | DFALL))) {
      if(player.xpos & 1)
        drawgc = hang2gc;
      else
        drawgc = hang1gc;
    }
  }
  else if((player.ypos & 1) && (code & DFALL))
    drawgc = flygc;

  if(drawgc == NULL)
    switch(player.dir) {
    case UP:    case DOWN:
      if(player.ypos & 1)
        drawgc = up2gc;
      else
        drawgc = up1gc;
      break;
    case LEFT:
      if(player.xpos & 1)
        drawgc = left2gc;
      else
        drawgc = left1gc;
      break;
    case RIGHT:
      if(player.xpos & 1)
        drawgc = right2gc;
      else
        drawgc = right1gc;
      break;
    case STAND:
      if(code & ULEAVE)
        drawgc = up1gc;
      else
        drawgc = standgc;
      break;
    default:
      drawgc = standgc;
    }
  /* Fill the rectangle surrounding the player with the chosen */
  /* graphics cursor. */
  if(drawgc != NULL)
    XFillRectangle(disp,wind,drawgc,player.xpos << 3,player.ypos << 3,16,16);
}

/* Erase the player by redrawing the block(s) underneath him */
void drawmove_player()
{
  register int x,y; 

  /* Do not erase redraw player if it is not nessary */
  if(! player.redraw)
    return;
  /* Draw block covering at least half of player */
  x = player.xold;
  y = player.yold;
  draw_block(x >> 1,y >> 1);
  /* If player is offset horizontally, redraw block to the right */
  if(x & 1)
    draw_block((x >> 1) + 1,y >> 1);
  /* If player is offset vertically, redraw block below */
  if(y & 1)
    draw_block(x >> 1,(y >> 1) + 1);

  draw_player();
}

/* Handle a key stroke by the user. */
void handle_key(keyhit)
KeySym keyhit;     /* Key symbol for key stroke provided by X windows */
{
  /* Now that a key is hit, really begin the level */
  newlevel = 0;
  /* Do action depending on which key was hit */
  switch(keyhit) {
  /* If it is a 'h', '?', or '/', print out a list of commands */
  case XK_H:    case XK_h:    case XK_question:   case XK_slash:
    puts("Control the player using keyboard keys or the mouse.");
    puts("<space>,R11 - stop");
    puts("a,j,left arrow - move left");
    puts("d,l,right arrow - move right");
    puts("w,i,up arrow - move up");
    puts("s,k,down arrow - move down");
    puts("z,<,q,u,R13 - make hole left");
    puts("x,>,e,o,R15 - make hole right");
    puts("r,y,R7 - put down any held item");
    puts("1-9 - change the game speed");
    puts("\n^S,^Z - pause the game");
    puts("^Q,^Y - reactivate the game");
    puts("^C - kill the game");
    puts("^R - redraw the screen");
    break;
  /* A space bar changes the command to STAND */
  case XK_space:    case XK_R11:
    curorder = STAND; break;
  /* A 'z', ',', '<', 'q', or 'u' digs holes to the left */
  case XK_Z:    case XK_comma:  case XK_less:   case XK_Q: case XK_U:
  case XK_R13:  case XK_z:      case XK_q:      case XK_u:
    curorder = DIGLEFT; break;
  /* A 'x', '.', '>', 'e', or 'o' digs holes to the right */
  case XK_X:    case XK_period: case XK_greater: case XK_E: case XK_O:
  case XK_R15:  case XK_x:      case XK_e: case XK_o:
    curorder = DIGRIGHT; break;
  /* A 'j' or 'a' changes the command to LEFT */
  case XK_J:    case XK_A:  case XK_Left:   case XK_j:  case XK_a:
    curorder = LEFT; break;
  /* A 'i' or 'w' changes the command to UP */
  case XK_I:    case XK_W:  case XK_Up:     case XK_i:  case XK_w:
    curorder = UP; break;
  /* A 'k' or 's' changes the command to DOWN */
  case XK_K:    case XK_S:  case XK_Down:   case XK_k:  case XK_s:
    curorder = DOWN; break;
  /* A 'l' or 'd' changes the command to RIGHT */
  case XK_L:    case XK_D:  case XK_Right:  case XK_l:  case XK_d:
    curorder = RIGHT; break;
  /* A 'r' or 'y' drops whatever is being held */
  case XK_R:    case XK_Y:  case XK_R7:     case XK_r:  case XK_y:
    curorder = PUTDOWN; break;
  }
}

/* Redraw everything.  This routine is called whenever something major */
/* changes or the window is exposed. */
void redrawall()
{
  draw_level();
  draw_player();
  draw_badguys();
  XFlush(disp);
}

/* Initialize a level from the current level file */
void init_level()
{
  register int x,y,pos;
  
  /* Allow level sizes to be changes by new level */
  xsize = ysize = -1;
  /* Load the level data itself from the data file. */
  load_level();
  numholes = 0;

  /* Initialize player information */
  player.xpos = player.ypos = player.xstart = player.ystart = goldleft = 0;
  player.dir = STAND;
  player.hold = SPACE;
  curorder = STAND;
  pos = 0;
  for(x=0;x<xsize;++x)
    for(y=0;y<ysize;++y) {
      /* Count the total number of treasures */
      if(fast_lookup[level[pos]].code & TREASURE)
        goldleft ++;
      /* Look for player blocks and remove them.  The last one */
      /* encountered sets the player position. */
      if(level[pos] == PLAYER) {
        player.xpos = player.xstart = x << 1;
        player.ypos = player.ystart = y << 1;
        level[pos] = SPACE;
      }
      pos ++;
    }
  printf("Collect %d gold dubloons.\n",goldleft);

  /* Initialize bad guy information and other things. */
  start_badguy();
  regen_allow();
  regen_tree();
  /* Freeze action until a key is pressed */
  newlevel = 1;
}

/* Move player one movement */
void move_player()
{
  register int i,code;

  /* Attempt to move player according to his standing orders */
  code = movething(&player,curorder,-1);
  /* If digging completed, or if the player fell, and he was trying to move */
  /* in the same direction, change the current order to STAND */
  if(code == 4 || (code == 2 && curorder == player.dir))
    curorder = STAND;
  /* Redraw player if he dropped something (which will overwrite the */
  /* block) */
  if(code == 3)
    player.redraw = 1;
  /* If player is in the middle of a block, interesting things can */
  /* happen. */
  if((player.xpos & 1) == 0 && (player.ypos & 1) == 0)  {
    /* If the player has picked up a gold piece, consume it and */
    /* increment the score. */
    if(fast_lookup[player.hold].code & TREASURE) {
      player.hold = SPACE;
      score++;
      goldleft--;
      /* If that was the last gold piece, escape ladder and other */
      /* stuff may need to appear. */
      if(goldleft == 0) {
        regen_allow();      /* Regenerate the allowable movement array */
        redrawall();        /* Refresh the entire screen */
      }
      /* Redraw the score line */
      else
        draw_score();
    }
    /* Get the control code for the block direction underneath the */
    /* player */
    i = (player.xpos >> 1)*ysize + (player.ypos >> 1);
    code = fast_lookup[level[i]].code;
    /* If the control code shows an active UPLEVEL block, or the */
    /* player is at the top of the screen, and there is no more gold */
    /* left, goto the next level. */
    if((goldleft == 0 && 
    (player.ypos == 0 || (code & UPLEVEL))) ||
       ((code & UPLEVEL) && ! (code & INACTIVE)))  {
      /* Increment the level number */
      levelnum ++;
      /* Load the next level in if the current one is done */
      init_level();
      /* Redraw the level */
      redrawall();
      /* Flush all the many X windows operations out to the server.  This */
      /* is the only flush in all the operations in this procedure. */
      XFlush(disp);
      return;
    }
    /* If the block is a killer block, kill the player */
    if(code & KILLIN)
      died("was crushed");
  }
  /* Do not let PUTDOWN order stay after movement has started */
  else if(curorder == PUTDOWN)
    curorder = STAND;
}

/* Move everything one movement (or less).  This is the basic function */
/* which is called on every timer signal. */
void moveall()
{
  /* Remember old position of player */
  player.xold = player.xpos;
  player.yold = player.ypos;
  /* Assume that the player does not need to be redrawn initially */
  player.redraw = 0;
  /* Do player movement */
  move_player();
  /* If the level has changed, do not move other stuff */
  if(newlevel)
    return;
  /* Do secondary movement if player is sped up */
  if(fast_lookup[player.hold].code & SPEED)
    move_player();
  /* If the level has changed, do not move other stuff */
  if(newlevel)
    return;
  /* Prevent time from advancing for bad guys if a TIMESTOP item is */
  /* held by player */
  if(! (fast_lookup[player.hold].code & TIMESTOP)) {
    /* Regenerate bad guys movement tree periodically */
    if((curtick & 0xf) == 0)
      regen_tree();
    /* Only move bad guys every other tick */
    if(curtick & 1)
      move_badguys();
  }
  /* Check if the player is overlapping one of the bad guys while not */
  /* holding armor. */
  if(! (fast_lookup[player.hold].code & ARMOR) &&
     overlap_badguy(player.xpos,player.ypos,-1))
    died("was eaten");
  /* Redraw player if he moved.  Redraw occasionally anyway. */
  if(player.xpos != player.xold || player.ypos != player.yold ||
     (curtick & 0xf) == 0)
    player.redraw = 1;
  /* Erase and draw player if necessary */
  drawmove_player();
  /* Flush all the many X windows operations out to the server.  This */
  /* is the only flush in all the operations in this procedure. */
  XFlush(disp);
}

/* Function which is called whenever the timer signal goes off */
void ticker(sig)
int sig;
{
  /* Ignore any signal which is not an alarm.  Ignore alarm signals */
  /* after a new level has been drawn until a key is hit. */
  if(sig != SIGALRM || newlevel)
    return;
  
  /* increment the tick counter if time is advancing */
  if(! (fast_lookup[player.hold].code & TIMESTOP))
    curtick ++;

  /* age all the holes */
  change_holes();

  /* move the player and all the bad guys. */
  moveall();
}

/* main procedure for game */
void main(argc,argv)
int argc;
char **argv;
{
  int keycount,i,gamestop = 0,gcfunc,firstevent;
  static XEvent xev;
  KeySym keyhit;
  char buf[50];

  printf("type h for help.\n");

  /* set up level and world description defaults */
  worldname = DEFWORLD;
  levelnum = 1;
  score = 0;
  speed = 5;
  /* scan the command line for executing parameters and flags */
  for(i=1;i<argc;++i) {
    if(argv[i][0] == '-') {
      /* look for the level number */
      if(argv[i][1] == 'l') {
        if(argv[i][2] == '\0' && i+1 < argc) {
          sscanf(argv[i+1],"%d",&levelnum);
          i++;
        }
        else
          sscanf(argv[i]+2,"%d",&levelnum);
      }
      /* look for the level number */
      else if(argv[i][1] == 's') {
        if(argv[i][2] == '\0' && i+1 < argc) {
          sscanf(argv[i+1],"%d",&speed);
          i++;
        }
        else
          sscanf(argv[i]+2,"%d",&speed);
      }
      else {
        printf("usage: golddig [-l <level>] [-s <speed 1-9>] [<world name>]\n");
        exit(1);
      }
    }
    /* if it doesn't start with a -, it must be the name of the world */
    else {
      worldname = argv[i];
      break;
    }
  }
  /* remember what the starting level was */
  levelstart = levelnum;

  /* start up x windows and all graphics cursors for drawing level */
  xstart(EVMASK);
  /* reassemble the graphics cursors to prepare for actual play */
  for(i=0;symbs[i].symb != '\0';++i)
    fast_lookup[symbs[i].symb].gc  =
      fast_lookup[symbs[i].inplay].gc;

  /* Decide whether to use GXand or GXor depending on screen type */
  if((BlackPixel(disp,0) & WhitePixel(disp,0)) == BlackPixel(disp,0))
    gcfunc = GXand;
  else
    gcfunc = GXor;
  /* compute all the graphics cursors for drawing the player in his */
  /* various states. */
  standgc = makegc(gcfunc,player_bits);
  flygc = makegc(gcfunc,fly_bits);
  hang1gc = makegc(gcfunc,hang1_bits);
  hang2gc = makegc(gcfunc,hang2_bits);
  up1gc = makegc(gcfunc,up1_bits);
  up2gc = makegc(gcfunc,up2_bits);
  left1gc = makegc(gcfunc,left1_bits);
  left2gc = makegc(gcfunc,left2_bits);
  right1gc = makegc(gcfunc,right1_bits);
  right2gc = makegc(gcfunc,right2_bits);
  /* initialize the bad guy's graphics cursors */
  init_badguy();
  /* name the game window */
  XStoreName(disp,wind,"gold digger 2.0");
  /* do the rest of the level initialization */
  init_level();
 
  /* initialize timer structure according to speed */
  if(speed <= 0)
    speed = 1;
  if(speed <= 5)
    cycletime.it_interval.tv_usec = (5-speed) * 50000 + 125000;
  else
    cycletime.it_interval.tv_usec = 625000 / speed;
  cycletime.it_interval.tv_sec = 0;
  cycletime.it_value = cycletime.it_interval;
  /* start the system timer.  the timer signal catcher will be set */
  /* after the first x event is received. */
  signal(SIGALRM,SIG_IGN);
  setitimer(ITIMER_REAL,&cycletime,(struct itimerval *) NULL);

  /* main event loop */
  firstevent = 1;
  while(1) {
    /* get the next x window event */
    XWindowEvent(disp,wind,EVMASK,&xev);
    /* suppress the timer to prevent race conditions */
    signal(SIGALRM,SIG_IGN);
    /* If the window is exposed or the level is complete redraw the */
    /* entire level.  Also redraw everything if it is the first window */
    /* event to handle window managers which capture expose events in */
    /* an unfriendly way. */
    if((xev.type == Expose && xev.xexpose.count == 0) || firstevent) {
      /* Redraw the level */
      redrawall();
      /* Events after this are not the first event */
      firstevent = 0;
    }
    else if(xev.type == KeyPress) {
      keycount = XLookupString(&xev,buf,50,&keyhit,(XComposeStatus *) NULL);
      /* Check for special control command */
      if(xev.xkey.state & ControlMask)
        switch(keyhit)  {
        /* ^S and ^Z freeze the game in place */
        case XK_S: case XK_Z: case XK_s: case XK_z:
          gamestop = 1;
          break;
        /* ^Q and ^Y reactivate the game */
        case XK_Q: case XK_Y: case XK_q: case XK_y:
          gamestop = 0;
          break;
        /* ^C, ^U, and ^/ kill the game */
        case XK_C: case XK_U: case XK_c: case XK_u: case XK_backslash:
          goto game_over;
        /* ^R redraws the level */
        case XK_R: case XK_r:
          redrawall();
          break;
        }
      /* Pressing a number changes the game speed */
      else if(keyhit >= XK_1 && keyhit <= XK_9) {
        speed = (int) (keyhit - XK_0);
        /* Compute new cycle delay */
        if(speed <= 5)
          cycletime.it_interval.tv_usec = (5-speed) * 50000 + 125000;
        else
          cycletime.it_interval.tv_usec = 625000 / speed;
        cycletime.it_value = cycletime.it_interval;
        /* Reset the timer cycle time */
        setitimer(ITIMER_REAL,&cycletime,(struct itimerval *) NULL);
        /* Redraw score line with new speed */
        draw_score();
      }
      /* If it was a normal key stroke, hand it off to the handle_key */
      /* procedure */
      else
        handle_key(keyhit);
    }
    /* flush out pending x windows commands */
    XFlush(disp);
    /* reenable the alarm signal if game should be active */
    if(! gamestop)
      signal(SIGALRM,ticker);
  }

  /* go to died procedure */
 game_over:
  died("was abandoned");
}

/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

/* Modified by Josh Siegel to work with NeWS/X11                       */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "golddig.h"

#define EVMASK KeyPressMask | ExposureMask | ButtonMotionMask | ButtonPressMask

/* Plug into original block type definitions in shared.c */
extern struct symbs_s symbs[];

/* Current drawing character */
char curchar = ' ';

/* Redraw the entire level */
void redrawall()
{
  draw_level();
  XFlush(disp);
}

/* Set the score to be equal to the number of gold pieces */
void count_gold()
{
  register int i,levsize;

  levsize = xsize*ysize;
  score = 0;
  for(i=0;i<levsize;++i)
    if(fast_lookup[level[i]].code & TREASURE)
      score ++;
}

/* Main routine for editing levels */
void main(argc,argv)
int argc;
char **argv;
{
  register int i;
  static XEvent xev;
  KeySym keyhit;
  int keycount;
  char buf[50];

  /* Set up default values for describing world */
  worldname = DEFWORLD;
  levelnum = -1;
  goldleft = 0;
  score = 0;
  speed = 0;
  /* Use size of default level as default level size */
  xsize = -1;
  ysize = -1;

  /* Read in command line options */
  for(i=1;i<argc;++i) {
    if(argv[i][0] == '-') {
      /* -w sets the level width */
      if(argv[i][1] == 'w') {
        if(argv[i][2] == '\0' && i+1 < argc) {
          sscanf(argv[i+1],"%d",&xsize);
          i++;
        }
        else
          sscanf(argv[i]+2,"%d",&xsize);
      }
      /* -h sets the level height */
      else if(argv[i][1] == 'h') {
        if(argv[i][2] == '\0' && i+1 < argc) {
          sscanf(argv[i+1],"%d",&ysize);
          i++;
        }
        else
          sscanf(argv[i]+2,"%d",&ysize);
      }
      /* -l sets the level number */
      else if(argv[i][1] == 'l') {
        if(argv[i][2] == '\0' && i+1 < argc) {
          sscanf(argv[i+1],"%d",&levelnum);
          i++;
        }
        else
          sscanf(argv[i]+2,"%d",&levelnum);
      }
      else {
        printf("usage: makelev [-h <height>] [-w <width>] -l <level> [<world name>]\n");
        exit(1);
      }
    }
    /* If it doesn't start with a dash, it must a the world name */
    else {
      worldname = argv[i];
      break;
    }
  }
  /* Make sure some value was chosen for the level number.  This */
  /* discourages everybody editing the same level all the time. */
  if(levelnum == -1) {
    printf("usage: levelmake [-h <height>] [-w <width>] -l <level> [<world name>]\n");
    exit(1);
  }

  /* Load in level data from file. */
  load_level();

  printf("Welcome.  Type h for help.\n");

  /* Start up X windows and create all graphics cursors */
  xstart(EVMASK);
  /* Set the name of the output window */
  XStoreName(disp,wind,"Gold Digger 2.0 Level Generator");

  /* Main event loop */
  do {
    /* Get the next X window event */
    XWindowEvent(disp,wind,EVMASK,&xev);
    /* If it was an expose event, redraw everything */
    if(xev.type == Expose) {
      /* Count the number of gold pieces in level */
      count_gold();
      /* Redraw the level */
      redrawall();
    }
    else if(xev.type == KeyPress) {
      keycount = XLookupString(&xev,buf,50,&keyhit,(XComposeStatus *) NULL);
      /* If the 'h', '?' or '/' key was hit, print out the text */
      /* descriptions of each block type */
      if(keyhit == XK_H || keyhit == XK_h || keyhit == XK_question ||
         keyhit == XK_slash) {
        for(i=0;symbs[i].symb != '\0';++i)
          if(! (symbs[i].code & NODRAW))
            printf("%c - draw %ss\n",symbs[i].symb,symbs[i].name);
        puts("Type ^W to finish editing and save the level.");
        puts("Type ^C to quit editing.");
        puts("Type ^E to erase level.");
        puts("Use the left mouse button to paint blocks.");
        puts("Use the right mouse button to erase blocks.");
        putchar('\n');
      }
      /* A ^E erases the entire level */
      else if((keyhit == XK_E || keyhit == XK_e) &&
              (xev.xkey.state & ControlMask)) {
        /* Replice level contents with space */
        for(i=0;i<xsize*ysize;++i)
          level[i] = SPACE;
        /* There is no gold now */
        score = 0;
        /* Redraw empty level */
        redrawall();
      }
      else {
        /* Search through the block descriptions for one which has a */
        /* key code which matches the key code of the key which was */
        /* hit. */
        for(i=0;symbs[i].symb != '\0';++i)
          if(! (symbs[i].code & NODRAW))
            if(keyhit == symbs[i].xkey1 || keyhit == symbs[i].xkey2) {
              /* Change the current drawing character to the symbol */
              /* which was found. */
              curchar = symbs[i].symb;
              /* Count and display the number of gold pieces in level */
              count_gold();
              draw_score();
              break;
            }
      }
    }
    /* If the mouse moves with the button pressed, or the button is */
    /* pressed, draw the current block at that position */
    else if(xev.type == MotionNotify) {
      if(xev.xmotion.state & Button3Mask)
        setchar(xev.xmotion.x >> 4,xev.xmotion.y >> 4,SPACE);
      else
        setchar(xev.xmotion.x >> 4,xev.xmotion.y >> 4,curchar);
    }
    else if(xev.type == ButtonPress) {
      if(xev.xbutton.button == Button3)
        setchar(xev.xbutton.x >> 4,xev.xbutton.y >> 4,SPACE);
      else
        setchar(xev.xbutton.x >> 4,xev.xbutton.y >> 4,curchar);
    }
    /* Flush the graphics commands out to the server */
    XFlush(disp);
  /* Loop until a control key is pressed */
  } while(xev.type != KeyPress ||
          (keyhit != XK_C && keyhit != XK_c &&
           keyhit != XK_W && keyhit != XK_w) ||
          ! (xev.xkey.state & ControlMask));
  
  /* Terminated X windows connection */
  xend();
  /* Save level to data file */
  if(keyhit == XK_W || keyhit == XK_w)
    save_level();
  exit(0);
}

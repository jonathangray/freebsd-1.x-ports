/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <errno.h>
#include <string.h>
#include "golddig.h"

/* Include all the bitmaps for the terrain blocks */
#include "bitmap/player.bits"
#include "bitmap/badguy.bits"
#include "bitmap/ladder.bits"
#include "bitmap/space.bits"
#include "bitmap/wall.bits"
#include "bitmap/hole1.bits"
#include "bitmap/hole2.bits"
#include "bitmap/hole3.bits"
#include "bitmap/hole4.bits"
#include "bitmap/hole5.bits"
#include "bitmap/hole6.bits"
#include "bitmap/hole7.bits"
#include "bitmap/stone.bits"
#include "bitmap/ghost.bits"
#include "bitmap/gold.bits"
#include "bitmap/escape.bits"
#include "bitmap/rope.bits"
#include "bitmap/uplevel.bits"
#include "bitmap/invis.bits"
#include "bitmap/steplad.bits"
#include "bitmap/tube.bits"
#include "bitmap/chute.bits"
#include "bitmap/vrope.bits"
#include "bitmap/teleport.bits"
#include "bitmap/armor.bits"
#include "bitmap/parac.bits"
#include "bitmap/nshovel.bits"
#include "bitmap/hourgl.bits"
#include "bitmap/reverse.bits"
#include "bitmap/portable.bits"
#include "bitmap/larrow.bits"
#include "bitmap/rarrow.bits"
#include "bitmap/darrow.bits"
#include "bitmap/uarrow.bits"
#include "bitmap/kill.bits"
#include "bitmap/jump.bits"
#include "bitmap/anchor.bits"
#include "bitmap/sjump.bits"
#include "bitmap/speed.bits"
#include "bitmap/pshovel.bits"
#include "bitmap/rshovel.bits"
#include "bitmap/fog.bits"
#include "bitmap/window.bits"
#include "bitmap/anti.bits"

/* char *sprintf();    /* UNIX brain damage */

/* All in and out movements except up */
#define NOUPBITS   DLEAVE | LLEAVE | RLEAVE | HENTER | VENTER
/* All in and out movements */
#define MOVEBITS   NOUPBITS | ULEAVE
/* Standard bit pattern for empty space */
#define SPACEBITS  NOUPBITS | DIGUND | DFALL
/* Generic item which can be picked up */
#define ITEMBITS   SPACEBITS | PICKUP
/* Bit pattern used for dug holes */
#define HOLEBITS   SPACEBITS | STOPBAD | NODRAW

/* Structure describing all the characteristics of all blocks.  Refer */
/* to the defines and structure definition in golddig.h. */
struct symbs_s symbs[] = 
  {{SPACE,SPACE,"space",space_bits,SPACEBITS,XK_space,0},
   {'!','|',"escape",escape_bits,MOVEBITS | DIGUND | INACTIVE,XK_exclam,XK_1},
   {BRICK,BRICK,"wall",wall_bits,CANDIG | KILLIN,XK_3,XK_numbersign},
   {'$','$',"gold",gold_bits,ITEMBITS | TREASURE,XK_4,XK_dollar},
   {'-','-',"rope",rope_bits,NOUPBITS | DIGUND,XK_minus,0},
   {HOLE1,HOLE1,"hole1",hole1_bits,HOLEBITS,0,0},
   {HOLE1+1,HOLE1+1,"hole2",hole2_bits,HOLEBITS,0,0},
   {HOLE1+2,HOLE1+2,"hole3",hole3_bits,HOLEBITS,0,0},
   {HOLE1+3,HOLE1+3,"hole4",hole4_bits,HOLEBITS,0,0},
   {HOLE1+4,HOLE1+4,"hole5",hole5_bits,HOLEBITS,0,0},
   {HOLE1+5,HOLE1+5,"hole6",hole6_bits,HOLEBITS,0,0},
   {HOLE1+6,HOLE1+6,"hole7",hole7_bits,HOLEBITS,0,0},
   {'<','<',"left arrow",larrow_bits,LLEAVE | HENTER | VENTER | LFALL | DIGUND,
      XK_comma,XK_less},
   {'=','=',"tube",tube_bits,RLEAVE | LLEAVE | HENTER,XK_equal,0},
   {'>','>',"right arrow",rarrow_bits,RLEAVE | HENTER | VENTER | RFALL | 
      DIGUND,XK_period,XK_greater},
   {STONE,STONE,"stone",stone_bits,KILLIN,XK_2,XK_at},
   {'^','^',"anti-space",anti_bits,ULEAVE | LLEAVE | RLEAVE | HENTER |
      VENTER | DIGUND | UFALL,XK_6,XK_asciicircum},
   {'a','a',"armor",armor_bits,ITEMBITS | ARMOR,XK_A,XK_a},
   {BADGUY,BADGUY,"bad guy",badguy_bits,SPACEBITS,XK_B,XK_b},
   {'c','c',"chute",chute_bits,DLEAVE | DFALL | VENTER,XK_C,XK_c},
   {'d','d',"down arrow",darrow_bits,DLEAVE | HENTER | VENTER | DIGUND |
      DFALL,XK_D,XK_d},
   {'e','e',"reverse monster",reverse_bits,ITEMBITS | REVERSE, XK_E,XK_e},
   {'f','f',"up arrow",uarrow_bits,ULEAVE | HENTER | VENTER | UFALL |
      DIGUND,XK_F,XK_f},
   {'g',BRICK,"ghost brick",ghost_bits,(SPACEBITS) & ~HENTER,XK_G,XK_g},
   {'i',SPACE,"invisible block",invis_bits,KILLIN,XK_I,XK_i},
   {'j','j',"jump pad",jump_bits,NOUPBITS | UPTWO,XK_J,XK_j},
   {'k','k',"kill zone",kill_bits,HENTER | VENTER | DIGUND |
      KILLIN,XK_K,XK_k},
   {'l','l',"power shovel",pshovel_bits,ITEMBITS | PSHOVEL,XK_L,XK_l},
   {'m','m',"super jump pad",sjump_bits,NOUPBITS | UPALL,XK_M,XK_m},
   {'n','n',"nuclear shovel",nshovel_bits,ITEMBITS | NSHOVEL,XK_N,XK_n},
   {'o','o',"anchor",anchor_bits,ITEMBITS | ANCHOR,XK_O,XK_o},
   {PLAYER,PLAYER,"player",player_bits,SPACEBITS,XK_P,XK_p},
   {'q','q',"speed boot",speed_bits,ITEMBITS | SPEED,XK_Q,XK_q},
   {'r','r',"parachute",parac_bits,ITEMBITS | STOPFALL,XK_R,XK_r},
   {'s','s',"step ladder",steplad_bits,MOVEBITS | PICKUP,XK_S,XK_s},
   {'t','t',"teleporter",teleport_bits,SPACEBITS | TELEPORT,XK_T,XK_t},
   {'u','u',"leave level",uplevel_bits,SPACEBITS | UPLEVEL |
      INACTIVE,XK_U,XK_u},
   {'v','v',"vertical rope",vrope_bits,ULEAVE | DLEAVE |
      HENTER | VENTER,XK_V,XK_v},
   {'w','w',"window",window_bits,SPACEBITS | UPLEVEL,XK_W,XK_w},
   {'x','x',"extra brick",rshovel_bits,ITEMBITS | RSHOVEL,XK_X,XK_x},
   {'y','y',"heavy fog",fog_bits,SPACEBITS,XK_Y,XK_y},
   {'z','z',"time stop",hourgl_bits,ITEMBITS | TIMESTOP,XK_Z,XK_z},
   {'|','|',"ladder",ladder_bits,MOVEBITS,XK_backslash,XK_bar},
   {'~','-',"portable rope",portable_bits,NOUPBITS | DIGUND |
      PICKUP,XK_asciitilde,XK_quoteleft},

   /* List terminator */
   {'\0','\0',(char *) 0,(char *) 0,0,0,0}};

Font scorefont;     /* Font used to display score */
GC scoregc;         /* GC used to draw score */
GC blackgc;         /* Simple black foreground GC */

/* Manufaction a 16x16 graphics cursor used in a XFill... operation. */
GC makegc(func,bits)
int func;       /* Drawing function such as GXcopy or GXor. */
char bits[];    /* Bits describing fill pattern.  Produced in an X11 */
                /* bitmap file usually. */
{
  static XGCValues gcv;
  Pixmap pmap;

  /* Build X11 bitmap from data in bits */
  pmap = XCreatePixmapFromBitmapData(disp,wind,bits,16,16,BlackPixel(disp,0),
                                     WhitePixel(disp,0),DisplayPlanes(disp,0));
  /* Assign the graphics cursor parameters */
  gcv.function = func;
  gcv.foreground = BlackPixel(disp,0);
  gcv.background = WhitePixel(disp,0);
  gcv.tile = pmap;
  gcv.fill_style = FillTiled;
  /* Return the created graphics cursor */
  return(XCreateGC(disp,wind,GCFunction | GCForeground | GCBackground | 
                   GCTile | GCFillStyle,&gcv));
}

/* Start X11 and do some basic initialization */
void xstart(evmask)
long evmask;    /* Event mask which will be used in XSelectInput */
{
  register int i;
  XGCValues xgcv;
  XWMHints wmhints;

  /* Open up the display */
  disp = XOpenDisplay(NULL);
  /* Check to see if the open display succeeded */
  if(disp == NULL) {
    fprintf(stderr,"Display open failed.  Check DISPLAY environment variable.\n");
    exit(-1);
  }

  /* Create the game window */
  wind = XCreateSimpleWindow(disp,DefaultRootWindow(disp),20,20,
                             50 << 4,(30 << 4) + SCORESIZE,
                             2,WhitePixel(disp,0),BlackPixel(disp,0));
  /* Check to see if the open window succeeded */
  if(wind == 0) {
    fprintf(stderr,"Window open failed.\n");
    XCloseDisplay(disp);
    exit(-1);
  }

  /* Clear fast block type lookup table */
  for(i=0;i<256;++i) {
    fast_lookup[i].gc = NULL;
    /* Everything starts out looking like a space */
    fast_lookup[i].code = SPACEBITS;
  }
  /* Generate block type lookup table from symbs array defined above. */
  /* After this the symbs array will be used very rarely. */
  for(i=0;symbs[i].symb != '\0';++i) {
    fast_lookup[symbs[i].symb].gc  =
      makegc(GXcopy,symbs[i].bits);
    fast_lookup[symbs[i].symb].code = symbs[i].code;
  }
  /* Load in the font used to display the score */
  scorefont = XLoadFont(disp,SCOREFONT);
  /* Create GC which will be used from drawing score */
  xgcv.function = GXcopy;
  xgcv.font = scorefont;
  xgcv.foreground = WhitePixel(disp,0);
  xgcv.background = BlackPixel(disp,0);
  scoregc = XCreateGC(disp,wind,
                      GCFunction | GCFont | GCForeground | GCBackground,
                      &xgcv);
  /* Create GC which will be used for clearing score line */
  xgcv.function = GXcopy;
  xgcv.foreground = BlackPixel(disp,0);
  xgcv.background = WhitePixel(disp,0);
  blackgc = XCreateGC(disp,wind,
                      GCFunction | GCForeground | GCBackground,
                      &xgcv);

  /* Tell the WM that we want input... */
  wmhints.input = True;
  wmhints.flags = InputHint;
  XSetWMHints(disp, wind, &wmhints);

  /* Select the interesting window events */
  XSelectInput(disp,wind,evmask);

  /* Name and raise the window */
  XMapRaised(disp,wind);

  /* Flush and synchronize the server */
  XFlush(disp);
  XSync(disp,False);
}

/* Gracefully shut X windows down.  It is not strictly necessary to */
/* call this function. */
void xend()
{
  XUnloadFont(disp,scorefont);
  XUnmapWindow(disp,wind);
  XDestroyWindow(disp,wind);
  XCloseDisplay(disp);
}

/* Draw a block from the level array in the output window. */
void draw_block(x,y)
int x,y;    /* Position of block in array */
{
  register char curchar;
  GC drawgc;

  /* Get the block character out of the level array */
  curchar = level[x*ysize + y];
  /* If there is gold left and this block is inactive, replace it with */
  /* a space. */
  if(goldleft > 0 && (fast_lookup[curchar].code & INACTIVE))
    curchar = SPACE;
  /* Get the graphics cursor */
  drawgc = fast_lookup[curchar].gc;
  /* Replace questionable characters with spaces */
  if(drawgc == NULL)
    drawgc = fast_lookup[SPACE].gc;
  /* Fill the block */
  XFillRectangle(disp,wind,drawgc,x << 4,y << 4,16,16);
}

/* Change a block character in the level array.  The block is redrawn. */
void setchar(x,y,ch)
int x,y;    /* Position of block character to change. */
char ch;    /* Character to change it to */
{
  if(level[x*ysize + y] != ch) {
    level[x*ysize + y] = ch;
    draw_block(x,y);
  }
}

/* Draw the score and level number */
void draw_score()
{
  char buf[50];

  /* Build the output string */
  sprintf(buf,"score: %d  level: %d  speed: %d",score,levelnum,speed);
  /* Clear the current score line */
  XFillRectangle(disp,wind,blackgc,0,ysize << 4,xsize << 4,SCORESIZE);
  /* Actually draw the text */
  XDrawString(disp,wind,scoregc,0,(ysize << 4) + SCORESIZE - 1,buf,
              strlen(buf));
}

/* Redraw the entire level */
void draw_level()
{
  int x,y;

  /* Change the window size */
  XResizeWindow(disp,wind,xsize << 4,(ysize << 4) + SCORESIZE);
  /* Draw the score and level number */
  draw_score();
  /* Iterate through each block position and draw it */
  for(x=0;x < xsize;++x)
    for(y=0;y < ysize;++y)
      draw_block(x,y);
}

/* Load a level out of a file.  The global variables worldname and */
/* levelnum are used to produce the file name which is stored in */
/* filename. */
void load_level()
{
  FILE *levelfile;
  register int i,j;
  int x,y;
  char buf[300];

  /* Manufaction the file name by starting with the world name and */
  /* appending the level number to it. */
  strcpy(filename,LIB);
  strcat(filename,"/");
  strcat(filename,worldname);
  sprintf(filename + strlen(filename),"%03d",levelnum);
  /* Open level file for reading */
  levelfile = fopen(filename,"r");
  /* If level file does not exist, use the default level file. */
  if(levelfile == NULL) {
    /* Build the default level name */
    strcpy(buf,LIB);
    strcat(buf,"/default");
    /* Open default level file for reading */
    levelfile = fopen(buf,"r");
    if(levelfile == NULL) {
      perror(worldname);
      exit(1);
    }
  }

  /* Load the first line of the level file */
  if(fgets(buf,300,levelfile) == NULL) {
    x = 50;
    y = 30;
  }
  else {
    /* Extract the level size */
    sscanf(buf,"%d %d",&x,&y);
  }
  /* Change level size only if it is necessary */
  if(xsize == -1)
    xsize = x;
  if(ysize == -1)
    ysize = y;
  /* Carefully check the sanity of the size parameters */
  if(xsize < 5)
    xsize = 5;
  if(xsize > 250)
    xsize = 250;
  if(ysize < 5)
    ysize = 5;
  if(ysize > 250)
    ysize = 250;
  if(xsize * ysize > MAXLEVEL) {
    if(xsize > ysize)
      xsize = MAXLEVEL / ysize;
    else
      ysize = MAXLEVEL / xsize;
  }
  /* Iterate through each horizontal line */
  for(i=0;i<ysize;++i) {
    /* Load the next line from the file */
    if(fgets(buf,300,levelfile) != NULL) {
      /* Go through each horizontal position and copy the data into */
      /* the level array. */
      for(j=0;j<xsize;++j) {
        /* Break out if line ends prematurely */
        if(buf[j] == '\n' || buf[j] == '\0')
          break;
        level[j*ysize + i] = buf[j];
      }
    }
    else
      j = 0;
    /* Fill in rest of premature lines with spaces */
    for(;j<xsize;++j)
      level[j*ysize + i] = SPACE;
  }
  /* Close the level file */
  fclose(levelfile);
}

/* Save the current level back into a file.  The global variable */
/* filename is used to determine the file name. */
void save_level()
{
  FILE *levelfile;
  char buf[300];
  register int i,j;

  /* Open the data file */
  levelfile = fopen(filename,"w");
  if(levelfile == NULL) {
    perror(worldname);
    exit(1);
  }
  /* Write out the size of the level.  Normal text is used so that */
  /* levels can be easily copied across architectures. */
  fprintf(levelfile,"%d %d\n",xsize,ysize);
  /* Terminate the lines for writing out the horizontal level lines */
  buf[xsize] = '\n';
  buf[xsize+1] = '\0';
  /* Iterate through each vertical position */
  for(i=0;i<ysize;++i) {
    /* Copy each horizontal line into the output buffer */
    for(j=0;j<xsize;++j)
      buf[j] = level[j*ysize + i];
    /* Write the line out to the file */
    fputs(buf,levelfile);
  }
  /* Close the data file */
  fclose(levelfile);
}



/*  Copyright 1992 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */
/*
 * This module has heavily modifiedby R. Nation
 * (nation@rocket.sanders.lockheed.com).
 * No additional restrictions are applied
 *
 * Additional modifications by Garrett D'Amore (garrett@netcom.com).
 * No additional restrictions are applied.
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/cursorfont.h>
#include "rxvt.h"
#include "command.h"
#include "xsetup.h"
#include "screen.h"
#include "sbar.h"
#include "debug.h"

extern WindowInfo MyWinInfo;

#define XVT_CLASS	"XTerm"
#define SBAR_WIDTH	8	/* width of scroll bar */
#define FAT_SBAR_WIDTH	16	/* width of scroll bar */

#define VT_EVENTS	(	ExposureMask |\
				ButtonPressMask |\
				ButtonReleaseMask |\
				Button1MotionMask |\
				Button3MotionMask \
			)

#define MW_EVENTS	(	KeyPressMask |\
				FocusChangeMask |\
				StructureNotifyMask |\
			        VisibilityChangeMask \
			)

#define SB_EVENTS	(	ExposureMask |\
				Button2MotionMask |\
				Button1MotionMask |\
				Button3MotionMask |\
				ButtonReleaseMask |\
				ButtonPressMask \
			)
#define SB_ARROW_EVENTS (	ExposureMask |\
				EnterWindowMask|\
				LeaveWindowMask |\
				ButtonPressMask \
			)

/*  External global variables that are initialised at startup.
 */
Display		*display;
Window		vt_win;		/* vt100 window */
Window		main_win;	/* parent window */
Colormap	colormap;
XFontStruct	*mainfont;	/* main font structure */
GC 		gc;		/* GC for drawing text */
GC 		rvgc;		/* GC for drawing text */
unsigned long	foreground;	/* foreground pixel value */
unsigned long	background;	/* background pixel value */

struct sbar_info sbar;

static char		*xvt_name;	/* the name the program is run under */
static char		*window_name;	/* window name for titles etc. */
static char		*icon_name;	/* name to display in the icon */
static int		screen;		/* the X screen number */
static XColor		background_color;
static XColor		foreground_color;

extern KeySym SecureKeysym;
extern KeySym BigFontKeysym;
extern KeySym SmallFontKeysym;
extern KeySym PageUpKeysym;
extern KeySym PageDownKeysym;
#ifdef GREEK_ELOT_KBD
extern KeySym GreekSwitchKeysym; 
#endif

XSizeHints sizehints = {
	PMinSize | PResizeInc | PBaseSize | PWinGravity,
	0, 0, 80, 24,	/* x, y, width and height */
	1, 1,		/* Min width and height */
	0, 0,		/* Max width and height */
	1, 1,		/* Width and height increments */
	{0, 0}, {0, 0},	/* Aspect ratio - not used */
	2 * MARGIN, 2 * MARGIN,		/* base size */
	NorthWestGravity                /* gravity */
};

int console = 0;

extern unsigned char mask;
int MetaHandling = ESCAPE;
int login_shell = 0;

char *display_name = NULL;
char *bg_string = "white";
char *fg_string = "black";
char *geom_string = "80x24";

#ifdef PRINT_PIPE
char *print_pipe = "lpr";
#endif

#define NUM_FONTS 5
#define DEFAULT_FONT 1
char *reg_fonts[]=
{"5x8","6x13",     "8x13",   "9x15",    "12x24"};

extern char *command;
int fat_sbar;
char iconic = 0;

static void create_window(int,char **);
static void extract_colors_and_fonts(char *,char *, char *, char *);
void extract_resources(void);

XErrorHandler RxvtErrorHandler(Display *, XErrorEvent *);

/*  Open the display, initialise the rDB resources database and create the
 *  window.  If title is non null then it is used as the window and icon title.
 *  iargc and iargv are the original argc, argv so the can be written to a
 *  COMMAND resource.
 */
void init_display(int argc,char **argv)
{
  XGCValues gcv;
  int i,len;
  char *display_string;

  xvt_name = argv[0];
  window_name = argv[0];
  icon_name = argv[0];
  display_name = getenv("DISPLAY");
  if(display_name == NULL)
    display_name = ":0";

  /* make a quick pass to find the display name */
  for(i=1;i<argc;i++)
    {
      if((strcmp(argv[i],"-display")==0)&&(i+1<argc))
	display_name = argv[++i];
    }

  if (!(display = XOpenDisplay(display_name)))
    {
      error("can't open display %s",display_name);
      clean_exit(1);
    }

  screen = DefaultScreen(display);
  colormap = DefaultColormap(display,screen);

  extract_resources();

  /* now get all the options */
  for(i=1;i<argc;i++)
    {
      if((strcmp(argv[i],"-display")==0)&&(i+1<argc))
	display_name = argv[++i];
      else if((strcmp(argv[i],"-geometry")==0)&&(i+1<argc))
	geom_string = argv[++i];
#ifdef PRINT_PIPE
      else if((strcmp(argv[i],"-print-pipe")==0)&&(i+1<argc))
	print_pipe = argv[++i];
#endif
      else if((strcmp(argv[i],"-fg")==0)&&(i+1<argc))
	fg_string = argv[++i];
      else if((strcmp(argv[i],"-bg")==0)&&(i+1<argc))
	bg_string = argv[++i];
      else if((strcmp(argv[i],"-font")==0)&&(i+1<argc))
	reg_fonts[DEFAULT_FONT] = argv[++i];
      else if(strcmp(argv[i],"-C")==0)
	console = 1;
      else if((strcmp(argv[i],"-T")==0)&&(i+1<argc))
	window_name = argv[++i];
      else if((strcmp(argv[i],"-n")==0)&&(i+1<argc))
	icon_name = argv[++i];
      else if(strcmp(argv[i],"-7")==0)
	mask = 0x7f;
      else if(strcmp(argv[i],"-8")==0)
	mask = 0xff;
      else if(strcmp(argv[i],"-ls")==0)
	login_shell = 1;
      else if(strcmp(argv[i],"-ls-")==0)
	login_shell = 0;
      else if(strcmp(argv[i],"-fat")==0)
	fat_sbar = 1;
      else if(strcmp(argv[i],"-thin")==0)
	fat_sbar = 0;
      else if(strncmp(argv[i],"-ic",3)==0)
	iconic = 1;
      else if(strncmp(argv[i],"-meta",5)==0)
	{
	  if(strcasecmp(argv[i+1],"escape")==0)
	    MetaHandling = ESCAPE;
	  else if(strcasecmp(argv[i+1],"8thbit")==0)
	    MetaHandling = BIT;
	  else
	    MetaHandling = 0;
	  i++;
	}
      else if((strcmp(argv[i],"-secure")==0)&&(i+1<argc))
	SecureKeysym = XStringToKeysym(argv[++i]);
      else if((strcmp(argv[i],"-bigfont")==0)&&(i+1<argc))
	BigFontKeysym = XStringToKeysym(argv[++i]);
      else if((strcmp(argv[i],"-smallfont")==0)&&(i+1<argc))
	SmallFontKeysym = XStringToKeysym(argv[++i]);
      else if((strcmp(argv[i],"-pageup")==0)&&(i+1<argc))
	PageUpKeysym = XStringToKeysym(argv[++i]);	
      else if((strcmp(argv[i],"-pagedown")==0)&&(i+1<argc))
	PageDownKeysym = XStringToKeysym(argv[++i]);
      else if((strcmp(argv[i],"-sl")==0)&&(i+1<argc))
	{
	  sscanf(argv[++i],"%d\n",&MyWinInfo.saved_lines);
	  if(MyWinInfo.saved_lines < 0 )
	    MyWinInfo.saved_lines = 0;
	}
      else 
	{
	  fprintf(stderr,"rxvt Version 1.94\n\n");
	  fprintf(stderr,"Permitted arguments are:\n");
	  fprintf(stderr,
"-e <command> <arg> ...	execute command with ars - must be last argument\n");
	  fprintf(stderr,
"-display <name>	specify the display (server)\n");
	  fprintf(stderr,
"-geometry <spec>	the initial window geometry\n");
#ifdef PRINT_PIPE
	  fprintf(stderr,
"-print-pipe <name>	specify pipe for vt100 printer\n");
#endif
	  fprintf(stderr,
"-bg <colour>		background color\n");
	  fprintf(stderr,
"-fg <colour>		foreground color\n");
	  fprintf(stderr,
"-font <fontname>	normal font\n");
	  fprintf(stderr,
"-T <text>		text in window titlebar\n");
	  fprintf(stderr,
"-C                     Capture system console message\n");
	  fprintf(stderr,
"-n <text>		name in icon or icon window\n");
	  fprintf(stderr,
"-7		        run in 7 bit mode\n");
	  fprintf(stderr,
"-8                     run in 8 bit mode\n");
	  fprintf(stderr,
"-ls                    initiate the window's shell as a login shell\n");
	  fprintf(stderr,
"-ls-                   initiate the window's shell as a non-login shell\n");
	  fprintf(stderr,
"-fat                   use xterm-style fat scrollbar\n");
	  fprintf(stderr,
"-thin                  use the native thin-scrollbar\n");
	  fprintf(stderr,
"-ic                    start iconic\n");
	  fprintf(stderr,
"-meta                  handle Meta key with ESCAPE prefix, 8THBIT set, or ignore\n");
	  fprintf(stderr,
"-sl <number>           save number lines in scroll-back buffer\n");
	  fprintf(stderr,
"-secure <keysym>       use hot key alt-keysym to enter secure mode\n");
	  fprintf(stderr,
"-pageup <keysym>       use hot key alt-keysym to scroll up through the buffer\n");
	  fprintf(stderr,
"-pagedown <keysym>     use hot key alt-keysym to scroll down through buffer\n");
	  fprintf(stderr,
"-bigfont <keysym>      use hot key alt-keysym to switch to a bigger font\n");
	  fprintf(stderr,
"-small <keysym>        use hot key alt-keysym to switch to a smaller font\n");



	  clean_exit(1);
	}
    }

  extract_colors_and_fonts(reg_fonts[DEFAULT_FONT],
			   fg_string, bg_string, geom_string);

  create_window(argc,argv);
  
  /*  Create the graphics contexts.
   */
  gcv.foreground = background;
  gcv.background = foreground;
  gcv.font = mainfont->fid;
  rvgc = XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);
  gcv.foreground = foreground;
  gcv.background = background;
  gc = XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);
  
  sbar.sbgc = 
    XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);
  sbar.sbupgc = 
    XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);
  sbar.sbdowngc = 
    XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);

  /*  Add a DISPLAY entry to the environment, incase we were started
   * with rxvt -display term:0.0
   */
  len = strlen(XDisplayString(display));
  display_string = safemalloc(len+10,"display_string");
  sprintf(display_string,"DISPLAY=%s",XDisplayString(display));
  putenv(display_string);

  /*  initialise the screen data structures.
   */
  scr_init();
  sbar_init();

  XSetErrorHandler((XErrorHandler)RxvtErrorHandler);
}

/*  Extract the resource fields that are needed to open the window.
 */
static void extract_colors_and_fonts(char *font_string,
				     char *fg_string, char *bg_string,
				     char *geom_string)
{
  int x, y, width, height;
  int flags;
  
  /*  First get the font since we need it to set the size.
   */
  if ((mainfont = XLoadQueryFont(display,font_string)) == NULL) 
    {
      error("can't access font %s\n",font_string);
      clean_exit(1);
    }


  if(fat_sbar)
    sizehints.base_width += FAT_SBAR_WIDTH;
  else
    sizehints.base_width += SBAR_WIDTH;
  sizehints.width_inc = XTextWidth(mainfont,"M",1);
  sizehints.height_inc = mainfont->ascent + mainfont->descent;
  flags = XParseGeometry(geom_string,&x,&y,&width,&height);
  if (flags & WidthValue) 
    {
      sizehints.width = width;
      sizehints.flags |= USSize;
    }
  if (flags & HeightValue) 
    {
      sizehints.height = height;
      sizehints.flags |= USSize;
    }

  MyWinInfo.fheight = sizehints.height_inc;
  MyWinInfo.fwidth = sizehints.width_inc;
  MyWinInfo.cwidth = sizehints.width;
  MyWinInfo.cheight = sizehints.height;
  MyWinInfo.pwidth = MyWinInfo.cwidth*MyWinInfo.fwidth;
  MyWinInfo.pheight = MyWinInfo.cheight*MyWinInfo.fheight;

  sizehints.width = sizehints.width * sizehints.width_inc +
    sizehints.base_width;
  sizehints.height = sizehints.height * sizehints.height_inc + 
    sizehints.base_height;
  sizehints.min_width = sizehints.width_inc + sizehints.base_width;
  sizehints.min_height = sizehints.height_inc + sizehints.base_height;
  if (flags & XValue) 
    {
      if (flags & XNegative)
	{
	  x = DisplayWidth(display,screen) + x - sizehints.width - 2;
	  sizehints.win_gravity = NorthEastGravity;
	}
      sizehints.x = x;
      sizehints.flags |= USPosition;
    }
  if (flags & YValue) 
    {
      if (flags & YNegative)
	{
	  y = DisplayHeight(display,screen) + y - sizehints.height - 2;
	  sizehints.win_gravity = SouthWestGravity;
	  if((flags&XValue)&&(flags&XNegative))
	    sizehints.win_gravity = SouthEastGravity;	
	}
      sizehints.y = y;
      sizehints.flags |= USPosition;
    }

  /*  Do the foreground, and background colors.
   */
  if (XParseColor(display,colormap,fg_string,&foreground_color) == 0)
    error("invalid foreground color %s",fg_string);
  else if (XAllocColor(display,colormap,&foreground_color) == 0)
    error("can't allocate color %s",bg_string);
  else
    foreground = foreground_color.pixel;

  if (XParseColor(display,colormap,bg_string,&background_color) == 0)
    error("invalid background color %s",bg_string);
  else if (XAllocColor(display,colormap,&background_color) == 0)
    error("can't allocate color %s",bg_string);
  else
    background = background_color.pixel;
}


/* read the resources files */
void extract_resources(void)
{
  char *tmp;

#ifdef PRINT_PIPE
  if((tmp = XGetDefault(display,xvt_name,"print-pipe")) != (char *)0)
    print_pipe = tmp;
#endif
  if((tmp = XGetDefault(display,xvt_name,"geometry")) != (char *)0)
    geom_string = tmp;

  if((tmp = XGetDefault(display,xvt_name,"foreground")) != (char *)0)
    fg_string = tmp;

  if((tmp = XGetDefault(display,xvt_name,"background")) != (char *)0)
    bg_string = tmp;

  if((tmp = XGetDefault(display,xvt_name,"font")) != (char *)0)
    reg_fonts[DEFAULT_FONT] = tmp;

  if((tmp = XGetDefault(display,xvt_name,"title")) != (char *)0)
    window_name = tmp;

  if((tmp = XGetDefault(display,xvt_name,"icon_name")) != (char *)0)
    icon_name = tmp;

  if((tmp = XGetDefault(display,xvt_name,"bits")) != (char *)0)
    if(tmp[0]=='7')
      mask = 0x7f;
    else
      mask = 0xff;

  if((tmp = XGetDefault(display,xvt_name,"login_shell")) != (char *)0)
    if(strcasecmp(tmp,"true")==0)
      login_shell = 1;
    else
      login_shell = 0;

  if((tmp = XGetDefault(display,xvt_name,"scrollbar")) != (char *)0)
    if(strcasecmp(tmp,"fat")==0)
      fat_sbar = 1;
    else
      fat_sbar = 0;

  if(command == (char *)0)
    {
      if((tmp = XGetDefault(display,xvt_name,"command")) != (char *)0)
	command = tmp;
    }
  if((tmp = XGetDefault(display,xvt_name,"secure_key")) != (char *)0)
    SecureKeysym = XStringToKeysym(tmp);
  if((tmp = XGetDefault(display,xvt_name,"bigger_font_key")) != (char *)0)
    BigFontKeysym = XStringToKeysym(tmp);
  if((tmp = XGetDefault(display,xvt_name,"smaller_font_key")) != (char *)0)
    SmallFontKeysym = XStringToKeysym(tmp);
  if((tmp = XGetDefault(display,xvt_name,"page_up_key")) != (char *)0)
    PageUpKeysym= XStringToKeysym(tmp);
  if((tmp = XGetDefault(display,xvt_name,"page_down_key")) != (char *)0)
    PageDownKeysym = XStringToKeysym(tmp);
  if((tmp = XGetDefault(display,xvt_name,"saveLines")) != (char *)0)
    {
      sscanf(tmp,"%d\n",&MyWinInfo.saved_lines);
      if(MyWinInfo.saved_lines < 0 )
	MyWinInfo.saved_lines = 0;
    }
  else
    MyWinInfo.saved_lines = DEF_SAVED_LINES;    

  if((tmp = XGetDefault(display,xvt_name,"meta")) != (char *)0)
    if(strcasecmp(tmp,"escape")==0)
      MetaHandling = ESCAPE;
    else if(strcasecmp(tmp,"8thbit")==0)
      MetaHandling = BIT;
    else
      MetaHandling = 0;
#ifdef GREEK_ELOT_KBD
  if((tmp = XGetDefault(display,xvt_name,"greek_switch_keysym")) != (char *)0)
    GreekSwitchKeysym = XStringToKeysym(tmp);
#endif
  return;
}


/*  Open and map the window.
 */
XClassHint class;
static void create_window(int argc,char **argv)
{
  XWMHints wmhints;
  Cursor cursor;
  
  main_win = XCreateSimpleWindow(display,DefaultRootWindow(display),
				 sizehints.x,sizehints.y,
				 sizehints.width,sizehints.height,
				 1,foreground,background);
  
  change_window_name(window_name);
  change_icon_name(icon_name);
  class.res_name = xvt_name;
  class.res_class = XVT_CLASS;
  wmhints.input = True;
  if(iconic)
    wmhints.initial_state = IconicState;
  else
    wmhints.initial_state = NormalState;
  wmhints.flags = InputHint | StateHint;
  XSetWMProperties(display,main_win,NULL,NULL,argv,argc,
		   &sizehints,&wmhints,&class);

  XSelectInput(display,main_win,MW_EVENTS);
  
  if(fat_sbar)
    {
      sbar.width = FAT_SBAR_WIDTH - 1;
      sbar.height = sizehints.height;
      sbar.sb_win = XCreateSimpleWindow(display,main_win,-1,-1,
					sbar.width,
					sbar.height,1,
					foreground,background);
    }
  else
    {
      sbar.width = SBAR_WIDTH - 1 ;
      sbar.height = sizehints.height - 2*SBAR_WIDTH;
      sbar.sb_win = XCreateSimpleWindow(display,main_win,-1,sbar.width,
					sbar.width,
					sbar.height,1,
					foreground,background);
    }
  cursor = XCreateFontCursor(display,XC_sb_v_double_arrow);
  XRecolorCursor(display,cursor,&foreground_color,&background_color);
  XDefineCursor(display,sbar.sb_win,cursor);
  XSelectInput(display,sbar.sb_win,SB_EVENTS);
  
  if(!fat_sbar)
    {
      sbar.sb_up_win = XCreateSimpleWindow(display,main_win,-1,-1,
					   sbar.width, sbar.width+1,1,
					   foreground,background);
      cursor = XCreateFontCursor(display,XC_sb_left_arrow);
      XRecolorCursor(display,cursor,&foreground_color,&background_color);
      XDefineCursor(display,sbar.sb_up_win,cursor);
      XSelectInput(display,sbar.sb_up_win,SB_ARROW_EVENTS);

      sbar.sb_down_win = XCreateSimpleWindow(display,main_win,-1,
					     sizehints.height-sbar.width-2,
					     sbar.width, sbar.width+1,
					     1,foreground,background);
      cursor = XCreateFontCursor(display,XC_sb_left_arrow);
      XRecolorCursor(display,cursor,&foreground_color,&background_color);
      XDefineCursor(display,sbar.sb_down_win,cursor);
      XSelectInput(display,sbar.sb_down_win,SB_ARROW_EVENTS);
    }

  vt_win = XCreateSimpleWindow(display,main_win,sbar.width+1,0,
			       sizehints.width-sbar.width - 1,
			       sizehints.height,0,foreground,background);
  cursor = XCreateFontCursor(display,XC_xterm);
  XRecolorCursor(display,cursor,&foreground_color,&background_color);
  XDefineCursor(display,vt_win,cursor);
  XSelectInput(display,vt_win,VT_EVENTS);
  
  XMapWindow(display,vt_win);
  XMapWindow(display,sbar.sb_win);
  if(!fat_sbar)
    {
      XMapWindow(display,sbar.sb_up_win);
      XMapWindow(display,sbar.sb_down_win);
    }
  XMapWindow(display,main_win);
}

/*  Redraw the whole window after an exposure or size change.
 */
void resize_window(int width, int height)
{
  Window root;
  int x, y;
  extern int current_screen;
  static Bool first = True;
  unsigned int nw,nh, border_width, depth,prev_screen;
  XEvent dummy;
  static int old_width=-1,old_height=-1;

  if(width ==0)
    {
      while (XCheckTypedWindowEvent (display, main_win, ConfigureNotify, &dummy)); 
      XGetGeometry(display,main_win,&root,&x,&y,&width,&height,&border_width,
		   &depth);
    }
  nw = (width -sbar.width - 2*MARGIN)/MyWinInfo.fwidth;
  nh = (height - 2*MARGIN)/MyWinInfo.fheight;

  if((first)||(nw != MyWinInfo.cwidth)||(nh != MyWinInfo.cheight)||
     (old_width != width)||(old_height!=height))
    {
      old_width = width;
      old_height = height;
      prev_screen = current_screen;
      /* scr_reset will only work if I'm on the low screen! */
      if(!first)
	{
	  scr_clear_selection();
	  scr_change_screen(LOW);
	}

      MyWinInfo.cheight = nh;
      MyWinInfo.cwidth = nw;
      MyWinInfo.pwidth = MyWinInfo.cwidth*MyWinInfo.fwidth;
      MyWinInfo.pheight = MyWinInfo.cheight*MyWinInfo.fheight;
      
      if(fat_sbar)
	{
	  sbar.height = height;
	  XResizeWindow(display,sbar.sb_win,sbar.width,sbar.height);
	}
      else
	{
	  XMoveWindow(display,sbar.sb_down_win,-1,height-sbar.width-2);
	  sbar.height = height - 2*sbar.width;
	  XResizeWindow(display,sbar.sb_win,sbar.width,sbar.height);
	}
      XResizeWindow(display,vt_win,width - sbar.width,height+1);
      XClearWindow(display,vt_win);
      XSync(display,0);
      scr_reset();
      if(!first)
	scr_change_screen(prev_screen);
    }
  first = False;
}

/*  Change the window name displayed in the title bar.
 */
void change_window_name(char *str)
{
  XTextProperty name;
  
  if (XStringListToTextProperty(&str,1,&name) == 0) 
    {
      error("cannot allocate window name");
      return;
    }
  XSetWMName(display,main_win,&name);
  XFree(name.value);
}

/*  Change the icon name.
 */
void
change_icon_name(str)
char *str;
{
  XTextProperty name;
  
  if (XStringListToTextProperty(&str,1,&name) == 0) 
    {
      error("cannot allocate icon name");
      return;
    }
  XSetWMIconName(display,main_win,&name);
  XFree(name.value);
}

/*  Print an error message.
 */
void error(char *fmt,...)
{
  va_list args;
  
  va_start(args,fmt);

  fprintf(stderr,"%s: ",xvt_name);
  vfprintf(stderr,fmt,args);
  va_end(args);
  fprintf(stderr,"\n");
}

XErrorHandler RxvtErrorHandler(Display *dpy, XErrorEvent *event)
{
  clean_exit(1);
  return 0;
}

/****************************************************************************
 * 
 * Switch to a new font
 *
 ***************************************************************************/
int font_num = DEFAULT_FONT;

void NewFont(int direction)
{
  int w,h;

  if(direction == BIGGER)
    {
      font_num++;
      if(font_num >= NUM_FONTS)
	{
	  font_num = NUM_FONTS-1;
	  return;
	}
    }

  if(direction == SMALLER)
    {
      font_num--;
      if(font_num <0)
	{
	  font_num = 0;
	  return;
	}
    }
  
  XFreeFont(display,mainfont);
  if ((mainfont = XLoadQueryFont(display,reg_fonts[font_num]))==NULL) 
    {
      error("can't access font %s\n",reg_fonts[font_num]);
      mainfont = XLoadQueryFont(display,reg_fonts[DEFAULT_FONT]);
    }
  XSetFont(display,gc,mainfont->fid);
  XSetFont(display,rvgc,mainfont->fid);
  MyWinInfo.fheight = mainfont->ascent + mainfont->descent;
  MyWinInfo.fwidth = XTextWidth(mainfont,"M",1);
  w = MyWinInfo.cwidth*MyWinInfo.fwidth + sizehints.base_width;
  h = MyWinInfo.cheight*MyWinInfo.fheight + sizehints.base_height;
  sizehints.width_inc = MyWinInfo.fwidth;
  sizehints.height_inc = MyWinInfo.fheight;
  sizehints.width = w;
  sizehints.height = h;
  sizehints.min_width = sizehints.width_inc + sizehints.base_width;
  sizehints.min_height = sizehints.height_inc + sizehints.base_height;
  sizehints.flags = PMinSize | PResizeInc | PBaseSize | PWinGravity;
  XSetWMNormalHints(display,main_win,&sizehints);
  XResizeWindow(display, main_win, w, h);
  MyWinInfo.pwidth = MyWinInfo.cwidth*MyWinInfo.fwidth;
  MyWinInfo.pheight = MyWinInfo.cheight*MyWinInfo.fheight;
  XClearWindow(display,vt_win);
  XSync(display,0);
}




/*  Copyright 1992, 1993 Robert Nation (nation@rocket.sanders.lockheed.com)
 *
 *  You can do what you like with this source code  as long as you include an
 *  unaltered copy of this message (including the copyright).
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 */
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#define CLASS	        "Decoration Tester"
#define MESS_CLASS	"Decoration Tester"

#define MW_EVENTS   (ExposureMask | StructureNotifyMask | KeyPressMask )
#define MESS_EVENTS (ExposureMask | StructureNotifyMask | ButtonPressMask)

/* screen info */
Display		*display;
int             screen;		   /* the X screen number */
int             MyDisplayWidth, MyDisplayHeight;
int             x_fd;              /* file descriptor of X server connection */

/* windows and their sizes */
Window		main_win;               /* parent window */
int             main_height=100, main_width = 100;



/* assorted graphics info */
GC 		gc;	                 /* GC for drawing text */

XColor          background_color;
XColor          foreground_color;
#define BW 1                            /* Window border width */

char            *app_name = CLASS;      /* the resource name */

static XSizeHints sizehints = {
	PMinSize | PResizeInc | PBaseSize | PWinGravity,
	0, 0, 80, 80,	/* x, y, width and height */
	1, 1,		/* Min width and height */
	0, 0,		/* Max width and height */
	1, 1,		/* Width and height increments */
	{1, 1},           /* x,y increments */
	{1, 1},	        /* Aspect ratio */
	0, 0 ,		/* base size */
	NorthWestGravity
};

/* save from computing sine(x), use pre-computed values
 * There are *100, to avoid using floats */
short sine[]={0,105,208,309,407,500,588,669,743,809,866,914,951,
	      978,995,999,995,978,951,914,866,809,743,669,588,500,
	      407,309,208,105,0,-105,-208,-309,-407,-500,-588,-669,
	      -743,-809,-866,-914,-951,-978,-995,-999,-994,-978,-951,
	      -914,-866,-809,-743,-669,-588,-500,-407,-309,-208,-105 };

/* subroutine declarations */
void extractResources(char *, char *, char *);
void createWindows(int,char **);
void getEvent(void);
void error(char *);
void DrawWindow(Window, int, int);
void SetOffAlarm(void);
void GetNewAlarm(int);
int GetOneNum(char **,int);
void CantAlloc(char *);
void NoColor(char *);
void NoProp(void);

/* Arguments for GetNewAlarm() */
#define REPLACE 0
#define UPDATE 1

unsigned long mwm_hints = 1 ;
unsigned long mwm_funcs = 1;
unsigned long mwm_input = 0;

Atom MwmAtom = None;

/****************************************************************************
 *
 * rclock - Robs clock - simple X windows clock with appointment reminder
 *
 ****************************************************************************/
void main(int argc,char **argv)
{
  int i;
  char *display_name = NULL;
  char *bg_color = "white";
  char *fg_color = "black";
  char *geom_string = "100x100";
  XGCValues gcv;

  display_name = getenv("DISPLAY");

  /* parse the command line */
  for(i=1;i<argc;i+=2)
    {
      if(strcmp(argv[i],"-display")==0)
	display_name = argv[i+1];
      else if(strcmp(argv[i],"-geometry")==0)
	geom_string = argv[i+1];
      else if(strcmp(argv[i],"-fg")==0)  
	fg_color = argv[i+1];
      else if(strcmp(argv[i],"-bg")==0)
	bg_color = argv[i+1];
      else if(strcmp(argv[i],"-decor")==0)
	mwm_hints = atoi(argv[i+1]);
      else if(strcmp(argv[i],"-func")==0)
	mwm_funcs = atoi(argv[i+1]);
      else if(strcmp(argv[i],"-inp")==0)
	mwm_input = atoi(argv[i+1]);
      else 
	{
	  fprintf(stderr,"rclock arguments:
-display <name>	        specify the display (server)
-geometry <spec>	the initial window geometry\n");
	  fprintf(stderr,"-bg <colour>		background color
-fg <colour>		foreground color\n");
	  error("exiting");
	}
    }

  /* open display */
  if (!(display = XOpenDisplay(display_name)))
    error("Can't open display");

  /* get display info */
  x_fd = XConnectionNumber(display);
  screen = DefaultScreen(display);
  MyDisplayWidth = DisplayWidth(display, screen);
  MyDisplayHeight = DisplayHeight(display, screen);

  extractResources(geom_string, fg_color, bg_color);
  createWindows(argc,argv);
  /*  Create the graphics contexts. */
  gcv.foreground = foreground_color.pixel;
  gcv.background = background_color.pixel;
  gc = XCreateGC(display,main_win,GCForeground|GCBackground,&gcv);

  getEvent();

}


/*****************************************************************************
 *
 *  Extract the resource fields that are needed to open the window.
 *  (geometry and colors)
 *
 *****************************************************************************/
void extractResources(char *geom_string, char *fg_color, char *bg_color)
{
  int x, y, width, height;
  int flags;
  Colormap	colormap;

  colormap = DefaultColormap(display,screen);

  flags = XParseGeometry(geom_string,&x,&y,&width,&height);
  
  if (flags & WidthValue) 
    {
      main_width = width + sizehints.base_width;
      sizehints.flags |= USSize;
    }
  if (flags & HeightValue) 
    {
      main_height = height + sizehints.base_height;
      sizehints.flags |= USSize;
    }
  if (flags & XValue) 
    {
      if (flags & XNegative)
	{
	  x = MyDisplayWidth + x - main_width - 2*BW;
	  sizehints.win_gravity = NorthEastGravity;
	}
      sizehints.x = x;
      sizehints.flags |= USPosition;
    }
  if (flags & YValue) 
    {
      if (flags & YNegative)
	{
	  y = DisplayHeight(display,screen) + y - main_height - 2*BW;
	  sizehints.win_gravity = SouthWestGravity;
	  if((flags&XValue)&&(flags&XNegative))
	    sizehints.win_gravity = SouthEastGravity;	
	}
      sizehints.y = y;
      sizehints.flags |= USPosition;
    }
  
  /*  Do the foreground, and background colors.*/
  if (XParseColor(display,colormap,fg_color,&foreground_color) == 0)
    NoColor(fg_color);
  else 
    XAllocColor(display,colormap,&foreground_color);

  if (XParseColor(display,colormap,bg_color,&background_color) == 0)
    NoColor(bg_color);
  else
    XAllocColor(display,colormap,&background_color);

}


/**************************************************************************
 *
 *  Open and map the windows.
 *
 **************************************************************************/
static Atom wm_del_win;
void createWindows(int argc,char **argv)
{
  XTextProperty wname, iname,mname;
  XClassHint class1, class2;
  XWMHints wmhints;
  
  sizehints.width = main_width;
  sizehints.height = main_height;
  main_win = XCreateSimpleWindow(display,DefaultRootWindow(display),
				 sizehints.x,sizehints.y,
				 sizehints.width,sizehints.height,
				 BW,foreground_color.pixel,
				 background_color.pixel);
  XStringListToTextProperty(&app_name,1,&wname);

  XStringListToTextProperty(&app_name,1,&iname);

  class1.res_name = app_name;
  class1.res_class = CLASS;
  wmhints.input = True;
  wmhints.flags = InputHint;

  XSetWMProperties(display,main_win,&wname,&iname,argv,argc,
		   &sizehints,&wmhints,&class1);

  SetMwmHints(mwm_hints,mwm_funcs, mwm_input);
  XSelectInput(display,main_win,MW_EVENTS);
  XMapWindow(display,main_win);

  /* want to accept the delete window protocol */
  wm_del_win = XInternAtom(display,"WM_DELETE_WINDOW",False);
  XSetWMProtocols(display,main_win,&wm_del_win,1);

}



/****************************************************************************
 *
 *  Redraw the whole window after an exposure or size change.
 *  Sound an alarm if needed 
 *
 ****************************************************************************/
void DrawWindow(Window win, int width, int height)
{
  time_t t;
  struct tm *tmval;
  int i,j;
  int angle1,angle2;
  int h_x,h_y,m_x,m_y,center_x,center_y;

  XClearWindow(display,win);

  center_x = width>>1;
  center_y = height>>1;

  /* get the current time */
  t=time(0);
  tmval = localtime(&t);

  /* draw the hands */
  angle1 = (tmval->tm_hour%12)*5 + tmval->tm_min/12;
  h_x =   sine[angle1] * width  *60/200000 + center_x;
  h_y =   -(sine[(angle1+15)%60])* height *60/200000 + center_y;

  angle2 = tmval->tm_min;
  m_x =   sine[angle2] * width  *85/200000 + center_x;
  m_y = -(sine[(angle2+15)%60])* height *85/200000 + center_y;
  for(i=-1;i<2;i++)
    for(j=-1;j<2;j++)
      {
	XDrawLine(display,win,gc,center_x+i,center_y+j,h_x,h_y);
	XDrawLine(display,win,gc,center_x+i,center_y+j,m_x,m_y);
      }

  /* draw the clock face */
  for(i=0;i<60;i+=5)
    {
      angle1  = sine[i]*width;
      angle2  = -sine[(i+15)%60]*height;
      h_x = angle1 * 9 / 20000 + center_x;
      h_y = angle2 * 9 / 20000 + center_y;
      m_x = angle1 *10 / 20000 + center_x;
      m_y = angle2 *10 / 20000 + center_y;
      XDrawLine(display,win,gc,m_x,m_y,h_x,h_y);
    }

}


/***************************************************************************
 *
 * Loops forever, looking for stuff to do. Sleeps 1 minute if nothing to do
 *
 **************************************************************************/
void getEvent(void)
{
  fd_set in_fdset;
  XEvent event;
  int fd_width = 10;
  struct timeval tm;
  Window root;
  int x,y,border_width,depth;
  int up = 1;

  for (;;) 
    {
      /* take care of all pending X events */
      while(XPending(display))
	{
	  XNextEvent(display,&event); 
	  switch(event.type)
	    {
	    case ClientMessage:
	      /* check for delete window requests */
	      if((event.xclient.format == 32)&&
		 (event.xclient.data.l[0] == wm_del_win))
		{
		  if(event.xany.window == main_win)
		    exit(0);
		}
	      break;
	    case Expose:
	    case GraphicsExpose:
	      /* need to re-draw a window */
	      if (event.xany.window == main_win)
		DrawWindow(main_win,main_width,main_height);
	      break;
	    case ConfigureNotify:
	      /* window has been re-sized */
	      if (event.xany.window == main_win)
		XGetGeometry(display,main_win,&root,&x,&y,
			     &main_width,&main_height,&border_width,&depth);
	      break;
	    case KeyPress:
	      if(up)
		XLowerWindow(display,main_win);
	      else
		XRaiseWindow(display,main_win);
	      up = 1 - up;
	      break;
	    case ButtonPress:
	      break;
	    }
	}

      /* Now wait for time out or new X event */
      FD_ZERO(&in_fdset);
      FD_SET(x_fd,&in_fdset);
      tm.tv_sec = 60;
      tm.tv_usec = 0;
      select(fd_width,&in_fdset,NULL,NULL,&tm);

      DrawWindow(main_win,main_width,main_height);
    }
}


/***************************************************************************
 *
 * Assorted error message routines 
 * 
 **************************************************************************/
void error(char *message)
{
  fprintf(stderr,"rclock: %s\n",message);
  exit(1);
}

void NoColor(char *s)
{
  fprintf(stderr,"rclock: invalid color %s",s);
}


/**************************************************************************
 *
 * Sets mwm hints 
 *
 *************************************************************************/
/* Motif  window hints */
typedef struct
{
    CARD32      flags;
    CARD32      functions;
    CARD32      decorations;
    INT32       inputMode;
} PropMotifWmHints;

typedef PropMotifWmHints        PropMwmHints;

PropMwmHints  prop;


/* Motif window hints */
#define MWM_HINTS_FUNCTIONS     (1L << 0)
#define MWM_HINTS_DECORATIONS   (1L << 1)
#define MWM_HINTS_INPUT_MODE    (1L << 2)

/* bit definitions for MwmHints.functions */
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)       

/* values for MwmHints.input_mode */
#define MWM_INPUT_MODELESS                      0
#define MWM_INPUT_PRIMARY_APPLICATION_MODAL     1
#define MWM_INPUT_SYSTEM_MODAL                  2
#define MWM_INPUT_FULL_APPLICATION_MODAL        3         

/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL                 (1L << 0)
#define MWM_DECOR_BORDER              (1L << 1)
#define MWM_DECOR_RESIZEH             (1L << 2)
#define MWM_DECOR_TITLE               (1L << 3)
#define MWM_DECOR_MENU                (1L << 4)
#define MWM_DECOR_MINIMIZE            (1L << 5)
#define MWM_DECOR_MAXIMIZE            (1L << 6)

#define PROP_MOTIF_WM_HINTS_ELEMENTS  4
#define PROP_MWM_HINTS_ELEMENTS       PROP_MOTIF_WM_HINTS_ELEMENTS

/* 
 *  Now, if we (hopefully) have MWW - compatible window manager ,
 *  say, mwm, ncdwm, or else, we will set useful decoration style.
 *  Never check for MWM_RUNNING property.May be considered bad.
 */

SetMwmHints(unsigned int value, unsigned int funcs, unsigned int input)
{
  if (MwmAtom==None)
    {
      MwmAtom=XInternAtom(display,"_MOTIF_WM_HINTS",False);  
    }
  if (MwmAtom!=None)
    {
      /* sh->mwm.decorations contains OR of the MWM_DECOR_XXXXX */
      prop.decorations= value;
      prop.functions = funcs;
      prop.inputMode = input;
      prop.flags = MWM_HINTS_DECORATIONS| MWM_HINTS_FUNCTIONS | MWM_HINTS_INPUT_MODE;
      
      /* HOP - LA! */
      XChangeProperty (display,main_win,
		       MwmAtom, MwmAtom,
		       32, PropModeReplace,
		       (unsigned char *)&prop,
		       PROP_MWM_HINTS_ELEMENTS);
    }
}

/* This module, and the entire GoodStuff program, and the concept for
 * interfacing this module to the Window Manager, are all original work
 * by Robert Nation
 *
 * Copyright 1993, Robert Nation. No guarantees or warantees or anything
 * are provided or implied in any way whatsoever. Use this program at your
 * own risk. Permission to use this program for any purpose is given,
 * as long as the copyright is kept intact. */

#define TRUE 1
#define FALSE 

#include "../../configure.h"

#ifdef ISC
#include <sys/bsdtypes.h> /* Saul */
#endif

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/time.h>
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include "../../fvwm/module.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>

#include "GoodStuff.h"
#include "../../version.h"
char *MyName;

XFontStruct *font;

Display *dpy;			/* which display are we talking to */
int x_fd,fd_width;

Window Root;
int screen;
int d_depth;

char *GoodStuffBack = "#908090";
char *GoodStuffFore = "black";
char *font_string = "fixed";

Pixel hilite_pix, back_pix, shadow_pix, fore_pix;
GC  NormalGC,ShadowGC,ReliefGC;
Window main_win;
int Width, Height,win_x,win_y;

#define MW_EVENTS   (ExposureMask | StructureNotifyMask| ButtonReleaseMask |\
		     ButtonPressMask)

int num_buttons = 0;
int num_rows = 0;
int num_columns = 0;
int max_icon_width = 30,max_icon_height = 0;
int ButtonWidth,ButtonHeight;
int x= -100000,y= -100000,w= -1,h= -1,gravity = NorthWestGravity;
int new_desk = 0;
int pageing_enabled = 1;
int ready = 0;

int CurrentButton = -1;
int fd[2];

struct button_info Buttons[MAX_BUTTONS];
char *iconPath = NULL;
char *pixmapPath = NULL;

static Atom wm_del_win;
Atom _XA_WM_PROTOCOLS;

/***********************************************************************
 *
 *  Procedure:
 *	main - start of fvwm
 *
 ***********************************************************************
 */
void main(int argc, char **argv)
{
  char *display_name = NULL;
  int i,j;
  Window root;
  int x,y,border_width,depth,button;
  char *temp, *s;

  temp = argv[0];

  s=strrchr(argv[0], '/');
  if (s != NULL)
    temp = s + 1;


  MyName = safemalloc(strlen(temp)+1);
  strcpy(MyName, temp);

  for(i=0;i<MAX_BUTTONS;i++)
    {
      Buttons[i].title = NULL;
      Buttons[i].action = NULL;
      Buttons[i].icon_file = NULL;
      Buttons[i].icon_w = 0;
      Buttons[i].icon_h = 0;
      Buttons[i].IconWin = None;
      Buttons[i].icon_maskPixmap = None;	/* pixmap for the icon mask */
      Buttons[i].iconPixmap = None;
      Buttons[i].icon_depth = 0;
      Buttons[i].up = 1;                        /* Buttons start up */
      Buttons[i].hangon = NULL;                 /* don't wait on anything yet*/
    }
  signal (SIGPIPE, DeadPipe);  
  
  if((argc != 6)&&(argc != 7))
    {
      fprintf(stderr,"%s Version %s should only be executed by fvwm!\n",MyName,
	      VERSION);
      exit(1);
    }

  fd[0] = atoi(argv[1]);
  fd[1] = atoi(argv[2]);
  
  if (!(dpy = XOpenDisplay(display_name))) 
    {
      fprintf(stderr,"%s: can't open display %s", MyName,
	      XDisplayName(display_name));
      exit (1);
    }
  x_fd = XConnectionNumber(dpy);

#ifndef NO_SYSCONF
  fd_width = sysconf(_SC_OPEN_MAX);
#else
  fd_width = getdtablesize();
#endif

  screen= DefaultScreen(dpy);
  Root = RootWindow(dpy, screen);
  if(Root == None) 
    {
      fprintf(stderr,"%s: Screen %d is not valid ", MyName, screen);
      exit(1);
    }
  d_depth = DefaultDepth(dpy, screen);

  ParseOptions(argv[3]);
  if(num_buttons == 0)
    {
      fprintf(stderr,"%s: No Buttons defined. Quitting\n", MyName);
      exit(0);
    }

  for(i=0;i<num_buttons;i++)
    {
      LoadIconFile(i);
      if(Buttons[i].icon_w > max_icon_width)
	max_icon_width = Buttons[i].icon_w;
      if(Buttons[i].icon_h > max_icon_height)
	max_icon_height = Buttons[i].icon_h;
    }

  CreateWindow();
  for(i=0;i<num_buttons;i++)
    {
      CreateIconWindow(i);
    }

  XGetGeometry(dpy,main_win,&root,&x,&y,
	       (unsigned int *)&Width,(unsigned int *)&Height,
	       (unsigned int *)&border_width,(unsigned int *)&depth);
  ButtonWidth = (Width+1) / num_columns;
  ButtonHeight = (Height+1) / num_rows;

  for(i=0;i<num_rows;i++)
    for(j=0;j<num_columns; j++)
      {
	button = i*num_columns + j;
	ConfigureIconWindow(button,i,j);
      }
  XMapSubwindows(dpy,main_win);
  XMapWindow(dpy,main_win);

  /* request a window list, since this triggers a response which
   * will tell us the current desktop and paging status, needed to
   * indent buttons correctly */
  SendText(fd,"Send_WindowList",0);
  
  Loop();

}

/***********************************************************************
 *
 *  Procedure:
 *	Loop - wait for data to process
 *
 ***********************************************************************/
void Loop(void)
{
  Window root;
  int x,y,border_width,depth;
  XEvent Event;
  int NewButton,i,j,button,tw,th,i2;
  char *temp;

  while(1)
    {
      if(My_XNextEvent(dpy,&Event))
	{
	  switch(Event.type)
	    {
	    case Expose:
	      if((Event.xexpose.count == 0)&&
		(Event.xany.window == main_win))
		{
		  if(ready < 1)
		    ready ++;
		  RedrawWindow(-1);
		}
	      break;
	      
	    case ConfigureNotify:
	      XGetGeometry(dpy,main_win,&root,&x,&y,
			   (unsigned int *)&tw,(unsigned int *)&th,
			   (unsigned int *)&border_width,
			   (unsigned int *)&depth);
	      if((tw != Width)||(th!= Height))
		{
		  Width = tw;
		  Height = th;
		  ButtonWidth = (Width+1) / num_columns;
		  ButtonHeight = (Height+1) / num_rows;
		  XClearWindow(dpy,main_win);
		  
		  for(i=0;i<num_rows;i++)
		    for(j=0;j<num_columns; j++)
		      {
			button = i*num_columns + j;
			if(Buttons[button].swallow == 0)
			  ConfigureIconWindow(button,i,j);
			else if(Buttons[button].swallow == 3)
			  {
			    XMoveWindow(dpy,Buttons[button].IconWin,
					    j*ButtonWidth+4, i*ButtonHeight+4);
			    XResizeWindow(dpy,Buttons[button].IconWin,
					  ButtonWidth-8, 
					  ButtonHeight - 8 - font->ascent - font->descent);
			  }
		      }
		  RedrawWindow(-1);
		}
	      break;
	      
	    case ButtonPress:
	      CurrentButton = (Event.xbutton.x/ButtonWidth)+
		(Event.xbutton.y/ButtonHeight)*num_columns;
	      RedrawWindow(CurrentButton);
	      break;
	      
	    case ButtonRelease:
	      NewButton = (Event.xbutton.x/ButtonWidth)+
		(Event.xbutton.y/ButtonHeight)*num_columns;
	      if(NewButton == CurrentButton)
		{
		  SendInfo(fd,CurrentButton);

		  if((Buttons[CurrentButton].action)&&
		     (strncasecmp(Buttons[CurrentButton].action,"exec",4)== 0))
		    {
		      /* Look for Exec "identifier", in which
		       * case the button stays down until window
		       * "identifier" materializes */
		      i=4;
		      while((Buttons[CurrentButton].action[i] != 0)&&
			    (Buttons[CurrentButton].action[i] != '"'))
			i++;
		      i2=i+1;
		      while((Buttons[CurrentButton].action[i2] != 0)&&
			    (Buttons[CurrentButton].action[i2] != '"'))
			i2++;
		      if(i2 - i >1)
			{
			  Buttons[CurrentButton].hangon = safemalloc(i2-i);
			  strncpy(Buttons[CurrentButton].hangon,
				  &Buttons[CurrentButton].action[i+1],i2-i-1);
			  Buttons[CurrentButton].hangon[i2-i-1] = 0;
			  Buttons[CurrentButton].up = 0;
			  Buttons[CurrentButton].swallow = 0;
			}

		    }
		}
	      CurrentButton = -1;
	      RedrawWindow(CurrentButton);
	      break;
	    case ClientMessage:
	      if ((Event.xclient.format==32) && 
		  (Event.xclient.data.l[0]==wm_del_win))
		{
		  DeadPipe(1);
		}
	      break;
	    case PropertyNotify:
	      for(i=0;i<num_rows;i++)
		for(j=0;j<num_columns; j++)
		  {
		    button = i*num_columns + j;
		    if(Buttons[button].swallow == 3)
		      {
			XFetchName(dpy, Buttons[button].IconWin, &temp);
			CopyString(&Buttons[button].title, temp);
			XFree(temp);
			XClearArea(dpy,main_win,j*ButtonWidth,
				   i*ButtonHeight, ButtonWidth,ButtonHeight,0);
			RedrawWindow(button);
		      }
		  }
	      break;

	    default:
	      break;
	    }
	}
    }
  return;
}

/************************************************************************
 *
 * Draw the window 
 *
 ***********************************************************************/
void RedrawWindow(int newbutton)
{
  int i,j,w,yoff,button,len,yoff2;
  XEvent dummy;
  int val1,val2;

  if(ready < 1)
    return;

  while (XCheckTypedWindowEvent (dpy, main_win, Expose, &dummy));

  yoff = (ButtonHeight + font->ascent - font->descent)>>1;

  for(i=0;i<num_rows;i++)
    for(j=0;j<num_columns; j++)
      {
	button = i*num_columns + j;

	if((newbutton == -1)||(newbutton == button))
	  {
	    if((Buttons[button].swallow == 3)&&
	       (Buttons[button].IconWin != None))
	      XSetWindowBorderWidth(dpy,Buttons[button].IconWin,0);
	    if(Buttons[button].title != NULL)
	      {
		len = strlen(Buttons[button].title);
		w=XTextWidth(font,Buttons[button].title,len);
		while((w > (ButtonWidth-4))&&(len>0))
		  {
		    len--;
		    w=XTextWidth(font,Buttons[button].title,len);
		  }
		if(len>0)
		  {
		    if((Buttons[button].icon_w>0)&&(Buttons[button].icon_h>0))
		      {
			yoff2 = ButtonHeight - font->descent - 4;
			XDrawString(dpy,main_win,NormalGC,
				    j*ButtonWidth+((ButtonWidth - w)>>1),
				    i*ButtonHeight+yoff2,
				    Buttons[button].title, len);
		      }
		    else
		      {
			XDrawString(dpy,main_win,NormalGC,
				    j*ButtonWidth+((ButtonWidth - w)>>1),
				    i*ButtonHeight+yoff,
				    Buttons[button].title, len);
		      }
		  }
		if((Buttons[CurrentButton].action)&&
		   (strncasecmp(Buttons[button].action,"TogglePage",10)==0)&&
		   (pageing_enabled == 0))
		  {
		    RelieveWindow(main_win,j*ButtonWidth, i*ButtonHeight,
				  ButtonWidth, ButtonHeight,
				  ShadowGC,ReliefGC);
		  }
		else if((Buttons[CurrentButton].action)&&
			(strncasecmp(Buttons[button].action,"Desk",4)==0))
		  {
		    sscanf(&Buttons[button].action[4],"%d %d",&val1,&val2);
		    if((val1 == 0)&&(val2 == new_desk))
		      {
			RelieveWindow(main_win,j*ButtonWidth, i*ButtonHeight,
				      ButtonWidth, ButtonHeight,
				      ShadowGC,ReliefGC);
		      }
		    else
		      RelieveWindow(main_win,j*ButtonWidth, i*ButtonHeight,
				    ButtonWidth, ButtonHeight,
				    (CurrentButton==button)?ShadowGC:ReliefGC,
				    (CurrentButton==button)?ReliefGC:ShadowGC);
		  }
		else if(Buttons[button].up == 1)
		  {
		    RelieveWindow(main_win,j*ButtonWidth, i*ButtonHeight,
				  ButtonWidth, ButtonHeight,
				  (CurrentButton == button)?ShadowGC:ReliefGC,
				  (CurrentButton == button)?ReliefGC:ShadowGC);
		  }
		else
		  {
		    RelieveWindow(main_win,j*ButtonWidth, i*ButtonHeight,
				  ButtonWidth, ButtonHeight,
				  ShadowGC,ReliefGC);
		  }
	      }
	  }
      }
}

/****************************************************************************
 *
 *  Draws the relief pattern around a window
 *
 ****************************************************************************/
FVWM_INLINE void RelieveWindow(Window win,int x,int y,int w,int h,
		   GC rgc,GC sgc)
{
  XSegment seg[4];
  int i;

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y;

  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = x;        seg[i++].y2 = h+y-1;

  seg[i].x1 = x+1;      seg[i].y1   = y+1;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+1;

  seg[i].x1 = x+1;      seg[i].y1   = y+1;
  seg[i].x2 = x+1;      seg[i++].y2 = y+h-2;
  XDrawSegments(dpy, win, rgc, seg, i);

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y+h-1;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y+h-1;

  seg[i].x1 = x+w-1;    seg[i].y1   = y;
  seg[i].x2 = x+w-1;    seg[i++].y2 = y+h-1;
  if(d_depth<2)
    XDrawSegments(dpy, win, ShadowGC, seg, i);
  else
    XDrawSegments(dpy, win, sgc, seg, i);

  i=0;
  seg[i].x1 = x+1;      seg[i].y1   = y+h-2;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;

  seg[i].x1 = x+w-2;    seg[i].y1   = y+1;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;

  XDrawSegments(dpy, win, sgc, seg, i);
}

/************************************************************************
 *
 * Sizes and creates the window 
 *
 ***********************************************************************/
XSizeHints mysizehints;
void CreateWindow(void)
{
  XGCValues gcv;
  unsigned long gcm;


  wm_del_win = XInternAtom(dpy,"WM_DELETE_WINDOW",False);
  _XA_WM_PROTOCOLS = XInternAtom (dpy, "WM_PROTOCOLS", False);

  /* load the font */
  if ((font = XLoadQueryFont(dpy, font_string)) == NULL)
    {
      if ((font = XLoadQueryFont(dpy, "fixed")) == NULL)
	{
	  fprintf(stderr,"%s: No fonts available\n",MyName);
	  exit(1);
	}
    };

  /* size and create the window */
  if((num_rows == 0)&&(num_columns == 0))
    num_rows = 2;
  if(num_columns == 0)
    {
      num_columns = num_buttons/num_rows;
      while(num_rows * num_columns < num_buttons)
	num_columns++;
    }
  if(num_rows == 0)
    {
      num_rows = num_buttons/num_columns;
      while(num_rows * num_columns < num_buttons)
	num_rows++;
    }

  while(num_rows * num_columns < num_buttons)
    num_columns++;
  mysizehints.flags = PWinGravity| PResizeInc | PBaseSize;
  /* subtract one for the right/bottom border */
  mysizehints.width = (max_icon_width+8)*num_columns-1;
  mysizehints.height=(font->ascent+font->descent+12+max_icon_height)*
    num_rows-1;
  mysizehints.width_inc = num_columns;
  mysizehints.height_inc = num_rows;
  mysizehints.base_height = num_rows - 1;
  mysizehints.base_width = num_columns - 1;

  if(w > -1)
    {
      w = w - w%num_columns;
      mysizehints.width = w;
      h = h - w%num_rows;
      mysizehints.height = h;
      mysizehints.flags |= USSize;
    }

  if(x > -100000)
    {
      if (x< 0)
	{
	  mysizehints.x = DisplayWidth(dpy,screen) + x - mysizehints.width - 2;
	  gravity = NorthEastGravity;
	}
      else 
	mysizehints.x = x;
      if ( y<0)
	{
	  mysizehints.y = DisplayHeight(dpy,screen) + y - mysizehints.height-2;
	  gravity = SouthWestGravity;
	}
      else 
	mysizehints.y = y;

      if((x < 0) && (y < 0))
	gravity = SouthEastGravity;	
      mysizehints.flags |= USPosition;
    }

  mysizehints.win_gravity = gravity;
#define BW 1
  if(d_depth < 2)
    {
      back_pix = GetColor("white");
      fore_pix = GetColor("black");
      hilite_pix = back_pix;
      shadow_pix = fore_pix;
    }
  else
    {
      back_pix = GetColor(GoodStuffBack);
      fore_pix = GetColor(GoodStuffFore);
      hilite_pix = GetHilite(back_pix);
      shadow_pix = GetShadow(back_pix);

    }

  main_win = XCreateSimpleWindow(dpy,Root,mysizehints.x,mysizehints.y,
				 mysizehints.width,mysizehints.height,
				 BW,fore_pix,back_pix);
  XSetWMProtocols(dpy,main_win,&wm_del_win,1);

  XSetWMNormalHints(dpy,main_win,&mysizehints);
  XSelectInput(dpy,main_win,MW_EVENTS);
  change_window_name(MyName);

  gcm = GCForeground|GCBackground;
  gcv.foreground = hilite_pix;
  gcv.background = back_pix;
  ReliefGC = XCreateGC(dpy, Root, gcm, &gcv);  

  gcm = GCForeground|GCBackground;
  gcv.foreground = shadow_pix;
  gcv.background = back_pix;
  ShadowGC = XCreateGC(dpy, Root, gcm, &gcv);  

  gcm = GCForeground|GCBackground|GCFont;
  gcv.foreground = fore_pix;
  gcv.background = back_pix;
  gcv.font =  font->fid;
  NormalGC = XCreateGC(dpy, Root, gcm, &gcv);  
}







/****************************************************************************
 *
 * This routine computes the shadow color from the background color
 *
 ****************************************************************************/
Pixel GetShadow(Pixel background) 
{
  XColor bg_color;
  XWindowAttributes attributes;
  
  XGetWindowAttributes(dpy,Root,&attributes);
  
  bg_color.pixel = background;
  XQueryColor(dpy,attributes.colormap,&bg_color);
  
  bg_color.red = (unsigned short)((bg_color.red*50)/100);
  bg_color.green = (unsigned short)((bg_color.green*50)/100);
  bg_color.blue = (unsigned short)((bg_color.blue*50)/100);
  
  if(!XAllocColor(dpy,attributes.colormap,&bg_color))
    nocolor("alloc shadow","");
  
  return bg_color.pixel;
}

/****************************************************************************
 *
 * This routine computes the hilight color from the background color
 *
 ****************************************************************************/
Pixel GetHilite(Pixel background) 
{
  XColor bg_color, white_p;
  XWindowAttributes attributes;
  
  XGetWindowAttributes(dpy,Root,&attributes);
  
  bg_color.pixel = background;
  XQueryColor(dpy,attributes.colormap,&bg_color);

  white_p.pixel = GetColor("white");
  XQueryColor(dpy,attributes.colormap,&white_p);
  
#ifndef min
#define min(a,b) (((a)<(b)) ? (a) : (b))
#define max(a,b) (((a)>(b)) ? (a) : (b))
#endif

  bg_color.red = max((white_p.red/5), bg_color.red);
  bg_color.green = max((white_p.green/5), bg_color.green);
  bg_color.blue = max((white_p.blue/5), bg_color.blue);
  
  bg_color.red = min(white_p.red, (bg_color.red*140)/100);
  bg_color.green = min(white_p.green, (bg_color.green*140)/100);
  bg_color.blue = min(white_p.blue, (bg_color.blue*140)/100);
  
  if(!XAllocColor(dpy,attributes.colormap,&bg_color))
    nocolor("alloc hilight","");
  
  return bg_color.pixel;
}


void nocolor(char *a, char *b)
{
 fprintf(stderr,"%s: can't %s %s\n", MyName, a,b);
}


/****************************************************************************
 * 
 * Loads a single color
 *
 ****************************************************************************/ 
Pixel GetColor(char *name)
{
  XColor color;
  XWindowAttributes attributes;

  XGetWindowAttributes(dpy,Root,&attributes);
  color.pixel = 0;
   if (!XParseColor (dpy, attributes.colormap, name, &color)) 
     {
       nocolor("parse",name);
     }
   else if(!XAllocColor (dpy, attributes.colormap, &color)) 
     {
       nocolor("alloc",name);
     }
  return color.pixel;
}

/************************************************************************
 *
 * Sends a button command to fvwm 
 *
 ***********************************************************************/
void SendInfo(int *fd,int message)
{
  Window window;
  int w;

  if((message >=0)&&(message<num_buttons)&&
     (Buttons[message].action != NULL))
    {
      window = None;
      write(fd[0],&window,sizeof(Window));
      
      w=strlen(Buttons[message].action);
      write(fd[0],&w,sizeof(int));
      write(fd[0],Buttons[message].action,w);

      /* keep going */
      w=1;
      write(fd[0],&w,sizeof(int));
    }
}

/************************************************************************
 *
 * Sends arbitrary text to fvwm
 *
 ***********************************************************************/
void SendText(int *fd,char *message,unsigned long window)
{
  int w;

  if(message != NULL)
    {
      write(fd[0],&window, sizeof(unsigned long));
      
      w=strlen(message);
      write(fd[0],&w,sizeof(int));
      write(fd[0],message,w);

      /* keep going */
      w=1;
      write(fd[0],&w,sizeof(int));
    }
}


/************************************************************************
 *
 * Dead pipe handler
 *
 ***********************************************************************/
void DeadPipe(int nonsense)
{
  int i,j,button;

  for(i=0;i<num_rows;i++)
    for(j=0;j<num_columns; j++)
      {
	button = i*num_columns + j;
	if(Buttons[button].swallow == 3)
	  {
	    send_clientmessage (Buttons[button].IconWin,wm_del_win, CurrentTime);
	    XSync(dpy,0);
	  }
      }
  XSync(dpy,0);
  exit(0);
}


/************************************************************************
 *
 * Concatentates 3 strings
 *
 ***********************************************************************/
char *CatString3(char *a, char *b, char *c)
{
  char CatS[256];
  if (strlen(a)+strlen(b)+strlen(c) > 255)
    {
      return NULL;
    }
  strcpy(CatS, a);
  strcat(CatS, b);
  strcat(CatS, c);
  return CatS;
}

/*****************************************************************************
 * 
 * This routine is responsible for reading and parsing the config file
 *
 ****************************************************************************/
void ParseOptions(char *filename)
{
  FILE *fd = (FILE *)0;
  char line[256];
  char *tline,*orig_tline,*tmp;
  int Clength, len;

  fd = fopen(filename,"r");
  if(fd == (FILE *)0)
    {
      fprintf(stderr,"%s: can't open config file %s",MyName,filename);
      exit(1);
    }

  tline = fgets(line,(sizeof line)-1,fd);
  orig_tline = tline;
  Clength = strlen(MyName);
  while(tline != (char *)0)
    {
      int g_x, g_y, flags;
      unsigned width,height;

      while(isspace(*tline))tline++;

      if((strlen(&tline[0])>1)&&
	 (strncasecmp(tline,CatString3("*", MyName, "Geometry"),Clength+9)==0))
	{
	  tmp = &tline[Clength+9];
	  while(((isspace(*tmp))&&(*tmp != '\n'))&&(*tmp != 0))
	    {
	      tmp++;
	    }
	  tmp[strlen(tmp)-1] = 0;

	  flags = XParseGeometry(tmp,&g_x,&g_y,&width,&height);
	  if (flags & WidthValue) 
	    w = width;
	  if (flags & HeightValue) 
	    h = height;
	  if (flags & XValue) 
	    x = g_x;
	  if (flags & YValue) 
	    y = g_y;
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Font"),Clength+5)==0))
	{
	  CopyString(&font_string,&tline[Clength+5]);
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Rows"),Clength+5)==0))
	{
	  len=sscanf(&tline[Clength+5],"%d",&num_rows);
	  if(len < 1)
	    num_rows = 0;
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Columns"),Clength+8)==0))
	{
	  len=sscanf(&tline[Clength+8],"%d",&num_columns);
	  if(len < 1)
	    num_columns = 0;
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Fore"),Clength+5)==0))
	{
	  CopyString(&GoodStuffFore,&tline[Clength+5]);
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName, "Back"),Clength+5)==0))
	{
	  CopyString(&GoodStuffBack,&tline[Clength+5]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*", MyName, ""),Clength+1)==0)&&
	      (num_buttons < MAX_BUTTONS))
	{
	  match_string(&tline[Clength+1]);
	}
      else if((strlen(&tline[0])>1)&&(strncasecmp(tline,"IconPath",8)==0))
	{
	  CopyString(&iconPath,&tline[8]);
	}
      else if((strlen(&tline[0])>1)&&(strncasecmp(tline,"PixmapPath",10)==0))
	{
	  CopyString(&pixmapPath,&tline[10]);
	}
      tline = fgets(line,(sizeof line)-1,fd);
      orig_tline = tline;
    }
  
  return;
}


/**************************************************************************
 *
 * Parses a button command line from the config file 
 *
 *************************************************************************/
void match_string(char *tline)
{
  int len,i,i2;
  char *ptr,*start,*end,*tmp;

  /* skip spaces */
  while(isspace(*tline)&&(*tline != '\n')&&(*tline != 0))
    tline++;

  /* read next word. Its the button label. Users can specify "" 
   * NoIcon, or whatever to skip the label */
  /* read to next space */
  start = tline;
  end = tline;
  while((!isspace(*end))&&(*end!='\n')&&(*end!=0))
    end++;
  len = end - start;
  ptr = safemalloc(len+1);
  strncpy(ptr,start,len);
  ptr[len] = 0;
  Buttons[num_buttons].title = ptr;      

  /* read next word. Its the icon bitmap/pixmap label. Users can specify "" 
   * NoIcon, or whatever to skip the label */
  /* read to next space */
  start = end;
  /* skip spaces */
  while(isspace(*start)&&(*start != '\n')&&(*start != 0))
    start++;
  end = start;
  while((!isspace(*end))&&(*end!='\n')&&(*end!=0))
    end++;
  len = end - start;
  ptr = safemalloc(len+1);
  strncpy(ptr,start,len);
  ptr[len] = 0;
  Buttons[num_buttons].icon_file = ptr;      

  tline = end;
  /* skip spaces */
  while(isspace(*tline)&&(*tline != '\n')&&(*tline != 0))
    tline++;

  if(strncasecmp(tline,"swallow",7)==0)
    {
      /* Look for swallow "identifier", in which
       * case GoodStuff spawns and gobbles up window */
      i=7;
      while((tline[i] != 0)&&
	    (tline[i] != '"'))
	i++;
      i2=i+1;
      while((tline[i2] != 0)&&
	    (tline[i2] != '"'))
	i2++;
      if(i2 - i >1)
	{
	  Buttons[num_buttons].hangon = safemalloc(i2-i);
	  strncpy(Buttons[num_buttons].hangon,&tline[i+1],i2-i-1);
	  Buttons[num_buttons].hangon[i2-i-1] = 0;
	  Buttons[num_buttons].swallow = 1;
	}
      len = strlen(tline);
      tmp = tline + len -1;
      while(((isspace(*tmp))||(*tmp == '\n'))&&(tmp >=tline))
	{
	  tmp--;
	  len--;
	}
      ptr = safemalloc(len+1);
      strcpy(ptr,"Exec");
      strncat(ptr,&tline[7],len-3);
      ptr[len-3]=0;
      SendText(fd,ptr,0);     
      free(ptr);
      Buttons[num_buttons++].action = NULL;
    }
  else
    {
      len = strlen(tline);
      tmp = tline + len -1;
      while(((isspace(*tmp))||(*tmp == '\n'))&&(tmp >=tline))
	{
	  tmp--;
	  len--;
	}
      ptr = safemalloc(len+1);
      strncpy(ptr,tline,len);
      ptr[len]=0;
      Buttons[num_buttons++].action = ptr;
    }
  return;
}

/***********************************************************************
 *
 *  Procedure:
 *	safemalloc - mallocs specified space or exits if there's a 
 *		     problem
 *
 ***********************************************************************/
char *safemalloc(int length)
{
  char *ptr;
  
  if(length <= 0)
    length = 1;

  ptr = malloc(length);
  if(ptr == (char *)0)
    {
      fprintf(stderr,"%s:malloc failed",MyName);
      exit(1);
    }
  return ptr;
}

/**************************************************************************
 *  Change the window name displayed in the title bar.
 **************************************************************************/
void change_window_name(char *str)
{
  XTextProperty name;
  
  if (XStringListToTextProperty(&str,1,&name) == 0) 
    {
      fprintf(stderr,"%s: cannot allocate window name",MyName);
      return;
    }
  XSetWMName(dpy,main_win,&name);
  XSetWMIconName(dpy,main_win,&name);
  XFree(name.value);
}



/***************************************************************************
 *
 * Waits for next X event, or for an auto-raise timeout.
 *
 ****************************************************************************/
int My_XNextEvent(Display *dpy, XEvent *event)
{
  fd_set in_fdset;
  unsigned long header[3];
  int body_length;
  int count,count2 = 0;
  static int miss_counter = 0;
  unsigned long *body;
  int total;
  char *cbody;

  if(XPending(dpy))
    {
      XNextEvent(dpy,event);
      return 1;
    }

  FD_ZERO(&in_fdset);
  FD_SET(x_fd,&in_fdset);
  FD_SET(fd[1],&in_fdset);

  select(fd_width,&in_fdset, 0, 0, NULL);


  if(FD_ISSET(x_fd, &in_fdset))
    {
      if(XPending(dpy))
	{
	  XNextEvent(dpy,event);
	  miss_counter = 0;
	  return 1;
	}
      else
	miss_counter++;
      if(miss_counter > 100)
	DeadPipe(0);
    }
  
  if(FD_ISSET(fd[1], &in_fdset))
    {
      if((count = read(fd[1],header,3*sizeof(unsigned long))) >0)
	{
	  if(header[0] == START_FLAG)
	    {
	      body_length = header[2]-3;
	      body = (unsigned long *)
		safemalloc(body_length * sizeof(unsigned long));
	      cbody = (char *)body;
	      total = 0;
	      while(total < body_length*sizeof(unsigned long))
		{
		  if((count2=
		      read(fd[1],&cbody[total],
			   body_length*sizeof(unsigned long)-total)) >0)
		    {
		      total += count2;
		    }
		  else if(count2 < 0)
		    {
		      DeadPipe(0);
		    }
		}
	      process_message(header[1],body);
	      free(body);
	    }
	}
      if(count <= 0)
	{
	  DeadPipe(1);
	}
    }
  return 0;
}

/***************************************************************************
 *
 * A simple routine to copy a string, stripping spaces and mallocing
 * space for the new string 
 ***************************************************************************/
void CopyString(char **dest, char *source)
{
  int len;
  char *start;

  while(((isspace(*source))&&(*source != '\n'))&&(*source != 0))
    {
      source++;
    }
  len = 0;
  start = source;
  while((*source != '\n')&&(*source != 0))
    {
      len++;
      source++;
    }

  source--;
  while((isspace(*source))&&(*source != 0)&&(len >0))
    {
      len--;
      source--;
    }
  *dest = safemalloc(len+1);
  strncpy(*dest,start,len);
  (*dest)[len]=0;	  
}


#ifdef NEEDS_STRNCASECMP
int strncasecmp(char *s1,char *s2,int n)
{
  register int c1,c2;
  
  for (;;)
    {
      if (!n) return(0);
      c1 = *s1,c2 = *s2;
      if (!c1 || !c2) return(c1 - c2);
      if (isupper(c1)) c1 = 'a' - 1 + (c1 & 31);
      if (isupper(c2)) c2 = 'a' - 1 + (c2 & 31);
      if (c1 != c2) return(c1 - c2);
      n--,s1++,s2++;
    }
}
#endif



#ifdef NEEDS_STRCASECMP
int strcasecmp(char *s1,char *s2)
{
  register int c1,c2;
  
  for (;;)
    {
      c1 = *s1,c2 = *s2;
      if (!c1 || !c2) return(c1 - c2);
      if (isupper(c1)) c1 = 'a' - 1 + (c1 & 31);
      if (isupper(c2)) c2 = 'a' - 1 + (c2 & 31);
      if (c1 != c2) return(c1 - c2);
      s1++,s2++;
    }
}
#endif


void CheckForHangon(unsigned long *body)
{
  int button,i,j;
  char *cbody;

  cbody = (char *)&body[3];
  for(i=0;i<num_rows;i++)
    for(j=0;j<num_columns; j++)
      {
	button = i*num_columns + j;      
	if(Buttons[button].hangon != NULL)
	  {
	    if(strcmp(cbody,Buttons[button].hangon)==0)
	      {
		if(Buttons[button].swallow ==  1)
		  {
		    Buttons[button].swallow = 2;
		    Buttons[button].IconWin = (Window)body[0];
		    free(Buttons[button].hangon);
		    Buttons[button].hangon = NULL;
		  }
		else
		  {
		    Buttons[button].up = 1;
		    free(Buttons[button].hangon);
		    Buttons[button].hangon = NULL;
		    RedrawWindow(button);
		  }
	      }
	  }
      }
}

/**************************************************************************
 *
 * Process window list messages 
 *
 *************************************************************************/
void process_message(unsigned long type,unsigned long *body)
{
  switch(type)
    {
    case M_TOGGLE_PAGING:
      pageing_enabled = body[0];
      RedrawWindow(-1);
      break;
    case M_NEW_DESK:
      new_desk = body[0];
      RedrawWindow(-1);
      break;
    case M_END_WINDOWLIST:
      RedrawWindow(-1);
    case M_MAP:
      swallow(body);
    case M_RES_NAME:
    case M_RES_CLASS:
    case M_WINDOW_NAME:
      CheckForHangon(body);
    default:
      break;
    }
}





/***************************************************************************
 *
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type	ClientMessage
 *     message type	_XA_WM_PROTOCOLS
 *     window		tmp->w
 *     format		32
 *     data[0]		message atom
 *     data[1]		time stamp
 *
 ****************************************************************************/
void send_clientmessage (Window w, Atom a, Time timestamp)
{
  XClientMessageEvent ev;
  
  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = _XA_WM_PROTOCOLS;
  ev.format = 32;
  ev.data.l[0] = a;
  ev.data.l[1] = timestamp;
  XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
}


void swallow(unsigned long *body)
{
  char *temp;
  int button,i,j;
  
  for(i=0;i<num_rows;i++)
    for(j=0;j<num_columns; j++)
      {
	button = i*num_columns + j;      
	if((Buttons[button].IconWin == (Window)body[0])&&
	  (Buttons[button].swallow == 2))
	  {
	    Buttons[button].swallow = 3;
	    /* "Swallow" the window! */
	    XReparentWindow(dpy,Buttons[button].IconWin, main_win, 
			    j*ButtonWidth+4, i*ButtonHeight+4);
	    XMapWindow(dpy,Buttons[button].IconWin);
	    XSelectInput(dpy,(Window)body[0],
			 PropertyChangeMask|StructureNotifyMask);
	    XResizeWindow(dpy,(Window)body[0], ButtonWidth-8, 
			  ButtonHeight - 8 - font->ascent - font->descent);
	    Buttons[button].icon_w = ButtonWidth - 4;
	    Buttons[button].icon_h = ButtonHeight - 8-font->ascent-font->descent;
	    XFetchName(dpy, Buttons[button].IconWin, &temp);
	    XClearArea(dpy,main_win,j*ButtonWidth, i*ButtonHeight,
		       ButtonWidth,ButtonHeight,0);
	    CopyString(&Buttons[button].title, temp);
	    RedrawWindow(button);
	    XFree(temp);
	  }
      }
}

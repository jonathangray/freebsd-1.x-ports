/* This module, and the entire ModuleDebugger program, and the concept for
 * interfacing this module to the Window Manager, are all original work
 * by Robert Nation
 *
 * Copyright 1994, Robert Nation. No guarantees or warantees or anything
 * are provided or implied in any way whatsoever. Use this program at your
 * own risk. Permission to use this program for any purpose is given,
 * as long as the copyright is kept intact. */

#define TRUE 1
#define FALSE 

#include "../../configure.h"

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#ifdef ISC /* Saul */
#include <sys/bsdtypes.h> /* Saul */
#endif /* Saul */
 
#include <stdlib.h>
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>

#include "../../fvwm/module.h"

#include "FvwmPager.h"
#include "../../version.h"
#include "../../fvwm/fvwm.h"

char *MyName;
int fd_width;
int fd[2];

/*************************************************************************
 * 
 * Screen, font, etc info
 *
 **************************************************************************/
ScreenInfo Scr;
PagerWindow *Start = NULL;
PagerWindow *FocusWin = NULL;

Display *dpy;			/* which display are we talking to */
int x_fd,fd_width;

char *PagerFore = "black";
char *PagerBack="white";
char *font_string = "fixed";
char *smallFont = NULL;
char *HilightC = "black";

char *HiBack = "white";
char *HiFore = "black";
char *StdBack = "white";
char *StdFore = "black";
char *StickyBack = NULL;
char *StickyFore = NULL;

int window_w=0, window_h=0, window_x=0, window_y=0;
int icon_x=-10000, icon_y=-10000, icon_w=0, icon_h=0;
int usposition = 0,uselabel = 1;
extern DeskInfo *Desks;
int StartIconic = 0;
int Rows = -1, Columns = -1;
int desk1=0, desk2 =0;
int ndesks = 0;
int StickyIcons = 0;
/***********************************************************************
 *
 *  Procedure:
 *	main - start of module
 *
 ***********************************************************************/
void main(int argc, char **argv)
{
  char *temp, *s, *cptr;
  char *display_name = NULL;
  int itemp,i;
  char line[100];
  
  /* Save our program  name - for error messages */
  temp = argv[0];
  s=strrchr(argv[0], '/');
  if (s != NULL)
    temp = s + 1;
  
  MyName = safemalloc(strlen(temp)+2);
  strcat(MyName, temp);
  
  if((argc != 7)&&(argc != 6))
    {
      fprintf(stderr,"%s Version %s should only be executed by fvwm!\n",MyName,
	      VERSION);
      exit(1);
    }
  if(argc != 7)
    { 
      fprintf(stderr,"%s Version %s requires an argument: %s n m\n",
	      MyName,VERSION,MyName);
      fprintf(stderr,"   where desktops n through m are displayed\n");
      exit(1);
    }
  
  /* Dead pipe == Fvwm died */
  signal (SIGPIPE, DeadPipe);  
  
  fd[0] = atoi(argv[1]);
  fd[1] = atoi(argv[2]);
  
#ifndef NO_SYSCONF
  fd_width = sysconf(_SC_OPEN_MAX);
#else
  fd_width = getdtablesize();
#endif
  
  cptr = argv[6];
  while((isspace(*cptr))&&(*cptr != 0))cptr++;
  desk1 = atoi(cptr);
  while(!(isspace(*cptr))&&(*cptr != 0))cptr++;
  while((isspace(*cptr))&&(*cptr != 0))cptr++;
  desk2 = atoi(cptr);
  if(desk2 < desk1)
    {
      itemp = desk1;
      desk1 = desk2;
      desk2 = itemp;
    }
  ndesks = desk2 - desk1 + 1;
  
  Desks = (DeskInfo *)malloc(ndesks*sizeof(DeskInfo));
  for(i=0;i<ndesks;i++)
    {
      sprintf(line,"Desk %d",i+desk1);
      CopyString(&Desks[i].label,line);
    }
  
  /* Initialize X connection */
  if (!(dpy = XOpenDisplay(display_name))) 
    {
      fprintf(stderr,"%s: can't open display %s", MyName,
	      XDisplayName(display_name));
      exit (1);
    }
  x_fd = XConnectionNumber(dpy);
  
  Scr.screen= DefaultScreen(dpy);
  Scr.Root = RootWindow(dpy, Scr.screen);
  if(Scr.Root == None) 
    {
      fprintf(stderr,"%s: Screen %d is not valid ", MyName, (int)Scr.screen);
      exit(1);
    }
  Scr.d_depth = DefaultDepth(dpy, Scr.screen);
  
  ParseOptions(argv[3]);
  
  /* open a pager window */
  initialize_pager();
  
  /* Create a list of all windows */
  /* Request a list of all windows,
   * wait for ConfigureWindow packets */
  SendInfo(fd,"Send_WindowList",0);
  
  Loop(fd);
}

/***********************************************************************
 *
 *  Procedure:
 *	Loop - wait for data to process
 *
 ***********************************************************************/
void Loop(int *fd)
{
  XEvent Event;
  
  while(1)
    {
      if(My_XNextEvent(dpy,&Event))
	DispatchEvent(&Event);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	Process message - examines packet types, and takes appropriate action
 *
 ***********************************************************************/
void process_message(unsigned long type,unsigned long *body)
{
  switch(type)
    {
    case M_ADD_WINDOW:
      list_configure(body);
      break;
    case M_CONFIGURE_WINDOW:
      list_configure(body);
      break;
    case M_DESTROY_WINDOW:
      list_destroy(body);
      break;
    case M_FOCUS_CHANGE:
      list_focus(body);
      break;
    case M_TOGGLE_PAGING:
      list_toggle(body);
      break;
    case M_NEW_PAGE:
      list_new_page(body);
      break;
    case M_NEW_DESK:
      list_new_desk(body);
      break;
    case M_RAISE_WINDOW:
      list_raise(body);
      break;
    case M_LOWER_WINDOW:
      list_lower(body);
      break;
    case M_ICONIFY:
    case M_ICON_LOCATION:
      list_iconify(body);
      break;
    case M_DEICONIFY:
      list_deiconify(body);
      break;
    case M_WINDOW_NAME:
      list_window_name(body);
      break;
    case M_ICON_NAME:
      list_icon_name(body);
      break;
    case M_RES_CLASS:
      list_class(body);
      break;
    case M_RES_NAME:
      list_res_name(body);
      break;
    case M_END_WINDOWLIST:
      list_end();
      break;
    default:
      list_unknown(body);
      break;
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	SendInfo - send a command back to fvwm 
 *
 ***********************************************************************/
void SendInfo(int *fd,char *message,unsigned long window)
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

/***********************************************************************
 *
 *  Procedure:
 *	SIGPIPE handler - SIGPIPE means fvwm is dying
 *
 ***********************************************************************/
void DeadPipe(int nonsense)
{
  exit(0);
}

/***********************************************************************
 *
 *  Procedure:
 *	list_add - displays packet contents to stderr
 *
 ***********************************************************************/
void list_add(unsigned long *body)
{
  PagerWindow *t,**prev;
  int i=0;
  
  t = Start;
  prev = &Start;
  while(t!= NULL)
    {
      prev = &(t->next);
      t = t->next;
      i++;
    }
  *prev = (PagerWindow *)safemalloc(sizeof(PagerWindow));
  (*prev)->w = body[0];
  (*prev)->t = (char *)body[2];
  (*prev)->frame = body[1];
  (*prev)->x = body[3];
  (*prev)->y = body[4];
  (*prev)->width = body[5];
  (*prev)->height = body[6];
  (*prev)->desk = body[7];
  (*prev)->next = NULL;
  (*prev)->flags = body[8];
  (*prev)->icon_name = NULL;
  (*prev)->title_height = body[9];
  (*prev)->border_width = body[10];
  AddNewWindow(*prev);
}

/***********************************************************************
 *
 *  Procedure:
 *	list_configure - displays packet contents to stderr
 *
 ***********************************************************************/
void list_configure(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t== NULL)
    {
      list_add(body);
    }
  else
    {
      t->t = (char *)body[2];
      t->frame = body[1];
      t->frame_x = body[3];
      t->frame_y = body[4];
      t->frame_width = body[5];
      t->frame_height = body[6];
      t->title_height = body[9];
      t->border_width = body[10];
      t->flags = body[8];
      t->icon_w = body[19];
      t->icon_pixmap_w = body[20];
      
      if(t->flags & ICONIFIED)
	{
	  t->x = t->icon_x;
	  t->y = t->icon_y;
	  t->width = t->icon_width;
	  t->height = t->icon_height;
	}
      else
	{
	  t->x = t->frame_x;
	  t->y = t->frame_y;
	  t->width = t->frame_width;
	  t->height = t->frame_height;
	}
      if(t->desk != body[7])
	ChangeDeskForWindow(t,body[7]);
      else
	MoveResizePagerView(t);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_destroy - displays packet contents to stderr
 *
 ***********************************************************************/
void list_destroy(unsigned long *body)
{
  PagerWindow *t,**prev;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  prev = &Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      prev = &(t->next);
      t = t->next;
    }
  if(t!= NULL)
    {
      if(prev != NULL)
	*prev = t->next;
      /* remove window from the chain */
      if(t->PagerView != None)
	XDestroyWindow(dpy,t->PagerView);
      XDestroyWindow(dpy,t->IconView);
      if(FocusWin == t)
	FocusWin = NULL;
      
      free(t);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_focus - displays packet contents to stderr
 *
 ***********************************************************************/
void list_focus(unsigned long *body)
{
  PagerWindow *t,*temp;
  Window target_w;
  
  target_w = body[0];
  
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if((t!= NULL)&&(t != FocusWin))
    {
      temp = FocusWin;
      FocusWin = t;
      
      if(temp != NULL)
	Hilight(temp,OFF);
      Hilight(FocusWin,ON);
    }
  else
    {
      temp = FocusWin;
      FocusWin = NULL;
      if(temp != NULL)
	Hilight(temp,OFF);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_toggle - displays packet contents to stderr
 *
 ***********************************************************************/
void list_toggle(unsigned long *body)
{
  /*  fprintf(stderr,"toggle\n");
      fprintf(stderr,"\t value %ld\n",body[0]);
      */
}

/***********************************************************************
 *
 *  Procedure:
 *	list_new_page - displays packet contents to stderr
 *
 ***********************************************************************/
void list_new_page(unsigned long *body)
{
  Scr.Vx = (long)body[0];
  Scr.Vy = (long)body[1];
  Scr.CurrentDesk = (long)body[2];
  MovePage();
  MoveStickyWindows();
}

/***********************************************************************
 *
 *  Procedure:
 *	list_new_desk - displays packet contents to stderr
 *
 ***********************************************************************/
void list_new_desk(unsigned long *body)
{
  int oldDesk;
  
  oldDesk = Scr.CurrentDesk;
  Scr.CurrentDesk = (long)body[0];
  
  MovePage();
  
  DrawGrid(oldDesk - desk1,1);
  DrawGrid(Scr.CurrentDesk - desk1,1);
  MoveStickyWindows();
}

/***********************************************************************
 *
 *  Procedure:
 *	list_raise - displays packet contents to stderr
 *
 ***********************************************************************/
void list_raise(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t!= NULL)
    {
      if(t->PagerView != None)
	XRaiseWindow(dpy,t->PagerView);
      XRaiseWindow(dpy,t->IconView);      
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_lower - displays packet contents to stderr
 *
 ***********************************************************************/
void list_lower(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t!= NULL)
    {
      if(t->PagerView != None)
	XLowerWindow(dpy,t->PagerView);
      if((t->desk - desk1>=0)&&(t->desk - desk1<ndesks))
	XLowerWindow(dpy,Desks[t->desk - desk1].CPagerWin);
      XLowerWindow(dpy,t->IconView);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_unknow - handles an unrecognized packet.
 *
 ***********************************************************************/
void list_unknown(unsigned long *body)
{
  /*  fprintf(stderr,"Unknown packet type\n");*/
}

/***********************************************************************
 *
 *  Procedure:
 *	list_iconify - displays packet contents to stderr
 *
 ***********************************************************************/
void list_iconify(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t== NULL)
    {
      return;
    }
  else
    {
      t->t = (char *)body[2];
      t->frame = body[1];
      t->icon_x = body[3];
      t->icon_y = body[4];
      t->icon_width = body[5];
      t->icon_height = body[6];
      t->flags |= ICONIFIED;
      t->x = t->icon_x;
      t->y = t->icon_y;
      t->width = t->icon_width;
      t->height = t->icon_height;
      MoveResizePagerView(t);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_deiconify - displays packet contents to stderr
 *
 ***********************************************************************/

void list_deiconify(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t== NULL)
    {
      return;
    }
  else
    {
      t->flags &= ~ICONIFIED;
      t->x = t->frame_x;
      t->y = t->frame_y;
      t->width = t->frame_width;
      t->height = t->frame_height;
      MoveResizePagerView(t);
      if(FocusWin == t)
	Hilight(t,ON);
      else
	Hilight(t,OFF);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_window_name - displays packet contents to stderr
 *
 ***********************************************************************/

void list_window_name(unsigned long *body)
{
  /*  fprintf(stderr,"window name\n");
      fprintf(stderr,"\t ID %lx\n",body[0]);
      fprintf(stderr,"\t frame ID %lx\n",body[1]);
      fprintf(stderr,"\t fvwm ptr %lx\n",body[2]);
      fprintf(stderr,"\t window name %s\n",(char *)(&body[3]));
      */
}


/***********************************************************************
 *
 *  Procedure:
 *	list_icon_name - displays packet contents to stderr
 *
 ***********************************************************************/
void list_icon_name(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;
  
  target_w = body[0];
  t = Start;
  while((t!= NULL)&&(t->w != target_w))
    {
      t = t->next;
    }
  if(t!= NULL)
    {
      if(t->icon_name != NULL)
	free(t->icon_name);
      CopyString(&t->icon_name,(char *)(&body[3]));
      LabelWindow(t);
      LabelIconWindow(t);
    }
}



/***********************************************************************
 *
 *  Procedure:
 *	list_class - displays packet contents to stderr
 *
 ***********************************************************************/
void list_class(unsigned long *body)
{
  /*  fprintf(stderr,"window class\n");
      fprintf(stderr,"\t ID %lx\n",body[0]);
      fprintf(stderr,"\t frame ID %lx\n",body[1]);
      fprintf(stderr,"\t fvwm ptr %lx\n",body[2]);
      fprintf(stderr,"\t window class %s\n",(char *)(&body[3]));
      */
}


/***********************************************************************
 *
 *  Procedure:
 *	list_res_name - displays packet contents to stderr
 *
 ***********************************************************************/
void list_res_name(unsigned long *body)
{
  /*  fprintf(stderr,"class resource name\n");
      fprintf(stderr,"\t ID %lx\n",body[0]);
      fprintf(stderr,"\t frame ID %lx\n",body[1]);
      fprintf(stderr,"\t fvwm ptr %lx\n",body[2]);
      fprintf(stderr,"\t resource name %s\n",(char *)(&body[3]));
      */
}

/***********************************************************************
 *
 *  Procedure:
 *	list_end - displays packet contents to stderr
 *
 ***********************************************************************/
void list_end(void)
{
  unsigned int nchildren,i;
  Window root, parent, *children;
  PagerWindow *ptr;
  
  if(!XQueryTree(dpy, Scr.Root, &root, &parent, &children, &nchildren))
    return;
  
  for(i=0; i<nchildren;i++)
    {
      ptr = Start;
      while(ptr != NULL)
	{
	  if((ptr->frame == children[i])||(ptr->icon_w == children[i])||
	     (ptr->icon_pixmap_w == children[i]))
	    {
	      if(ptr->PagerView != None)
		XRaiseWindow(dpy,ptr->PagerView);
	      XRaiseWindow(dpy,ptr->IconView);
	    }
	  ptr = ptr->next;
	}
    }
  
  
  if(nchildren > 0)
    XFree((char *)children);
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
  int Clength,n,desk;
  
  Scr.FvwmRoot = NULL;
  Scr.Hilite = NULL;
  Scr.VScale = 32;
  
  Scr.MyDisplayWidth = DisplayWidth(dpy, Scr.screen);
  Scr.MyDisplayHeight = DisplayHeight(dpy, Scr.screen);
  
  Scr.VxMax = 3*Scr.MyDisplayWidth - Scr.MyDisplayWidth;
  Scr.VyMax = 3*Scr.MyDisplayHeight - Scr.MyDisplayHeight;
  if(Scr.VxMax <0)
    Scr.VxMax = 0;
  if(Scr.VyMax <0)
    Scr.VyMax = 0;
  Scr.Vx = 0;
  Scr.Vy = 0;
  
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
	 (strncasecmp(tline, CatString3("*", MyName, "Geometry"),Clength+9)==0))
	{
	  tmp = &tline[Clength+9];
	  while(((isspace(*tmp))&&(*tmp != '\n'))&&(*tmp != 0))
	    {
	      tmp++;
	    }
	  tmp[strlen(tmp)-1] = 0;
	  
	  flags = XParseGeometry(tmp,&g_x,&g_y,&width,&height);
	  if (flags & WidthValue) 
	    window_w = width;
	  if (flags & HeightValue) 
	    window_h = height;
	  if (flags & XValue) 
	    {
	      window_x = g_x;
	      usposition = 1;
	    }
	  if (flags & YValue) 
	    {
	      window_y = g_y;
	      usposition = 1;
	    }
	}
      else if((strlen(&tline[0])>1)&&
	 (strncasecmp(tline, CatString3("*", MyName, "IconGeometry"),
		      Clength+13)==0))
	{
	  tmp = &tline[Clength+13];
	  while(((isspace(*tmp))&&(*tmp != '\n'))&&(*tmp != 0))
	    {
	      tmp++;
	    }
	  tmp[strlen(tmp)-1] = 0;
	  
	  flags = XParseGeometry(tmp,&g_x,&g_y,&width,&height);
	  if (flags & WidthValue) 
	    icon_w = width;
	  if (flags & HeightValue) 
	    icon_h = height;
	  if (flags & XValue) 
	    {
	      icon_x = g_x;
	    }
	  if (flags & YValue) 
	    {
	      icon_y = g_y;
	    }
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Label"),Clength+6)==0))
	{
	  desk = desk1;
	  sscanf(&tline[Clength+6],"%d",&desk);
	  if((desk >= desk1)&&(desk <=desk2))
	    {
	      n = 0;
	      while(isspace(tline[Clength+6+n]))n++;
	      while(!isspace(tline[Clength+6+n]))n++;
	      free(Desks[desk - desk1].label);
	      CopyString(&Desks[desk - desk1].label,&tline[Clength+6+n]);
	    }
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline, CatString3("*", MyName, "Font"),Clength+5)==0))
	{
	  CopyString(&font_string,&tline[Clength+5]);
	  if(strncasecmp(font_string,"none",4) == 0)
	    uselabel = 0;

	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline, CatString3("*", MyName, "Fore"),Clength+5)==0))
	{
	  CopyString(&PagerFore,&tline[Clength+5]);
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*", MyName, "Back"),Clength+5)==0))
	{
	  CopyString(&PagerBack,&tline[Clength+5]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Hilight"),Clength+8)==0))
	{
	  CopyString(&HilightC,&tline[Clength+8]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"SmallFont"),
			   Clength+10)==0))
	{
	  CopyString(&smallFont,&tline[Clength+10]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"StartIconic"),
			   Clength+12)==0))
	{
	  StartIconic = 1;
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Rows"),
			   Clength+5)==0))
	{
	  sscanf(&tline[Clength+5],"%d",&Rows);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,CatString3("*",MyName,"Columns"),
			   Clength+8)==0))
	{
	  sscanf(&tline[Clength+8],"%d",&Columns);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"StdBackColor",12)==0))
	{
	  CopyString(&StdBack,&tline[12]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"HiBackColor",11)==0))
	{
	  CopyString(&HiBack,&tline[11]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"StickyBackColor",15)==0))
	{
	  CopyString(&StickyBack,&tline[15]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"StdForeColor",12)==0))
	{
	  CopyString(&StdFore,&tline[12]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"HiForeColor",11)==0))
	{
	  CopyString(&HiFore,&tline[11]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"StickyForeColor",15)==0))
	{
	  CopyString(&StickyFore,&tline[15]);
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"StickyIcons",11)==0))
	{
	  StickyIcons = 1;
	}	
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"DeskTopSize",11)==0))
	{
	  sscanf(&tline[11],"%dx%d",&Scr.VxMax,&Scr.VyMax);
	  Scr.VxMax = Scr.VxMax*Scr.MyDisplayWidth - Scr.MyDisplayWidth;
	  Scr.VyMax = Scr.VyMax*Scr.MyDisplayHeight - Scr.MyDisplayHeight;
	}
      else if((strlen(&tline[0])>1)&&
	      (strncasecmp(tline,"DeskTopScale",12)==0))
	{
	  sscanf(&tline[12],"%d",&Scr.VScale);
	}
      tline = fgets(line,(sizeof line)-1,fd);
      orig_tline = tline;
    }
  return;
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


/************************************************************************
 *
 * Concatentates 3 strings
 *
 *************************************************************************/
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

/****************************************************************************
 * This module is all new
 * by Rob Nation (nation@rocket.sanders.lockheed.com)
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/
/***********************************************************************
 *
 * fvwm window-list popup code
 *
 ***********************************************************************/

#include "configure.h"

#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "fvwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"

/* I tried to include "limits.h" to get these values, but it
 * didn't work for some reason */
/* Minimum and maximum values a `signed int' can hold.  */
#define MY_INT_MIN (- MY_INT_MAX - 1)
#define MY_INT_MAX 2147483647

extern XContext MenuContext;
/*
 * Change by PRB (pete@tecc.co.uk), 31/10/93.  Prepend a hot key
 * specifier to each item in the list.  This means allocating the
 * memory for each item (& freeing it) rather than just using the window
 * title directly. (Only if HOTKEYS is defined)
 */
void do_windowList(int val1, int val2)
{
#ifndef NO_WINDOWLIST
  MenuRoot *mr;
  MenuItem *mi,*tmp;
  FvwmWindow *t;
  char *tname,*junk;
  char loc[40],*name=NULL;
  int dwidth,dheight;
  char tlabel[50];
  int last_desk_done = MY_INT_MIN;
  int next_desk;
  int junkD;

#ifdef HOTKEYS
  char *t_hot;			/* Menu label with hotkey added */
  char scut = '0';		/* Current short cut key */
#endif /* HOTKEYS */

  sprintf(tlabel,"CurrentDesk: %d",Scr.CurrentDesk);
  mr=NewMenuRoot(tlabel);
  AddToMenu(mr, tlabel, "Geometry", NULL, F_TITLE,0,0);      

  next_desk = 0;
  while(next_desk != MY_INT_MAX)
    {
      /* Sort window list by desktop number */
      if((val1 < 2)&&(val1 > -2))
	{
	  next_desk = MY_INT_MAX;
	  for (t = Scr.FvwmRoot.next; t != NULL; t = t->next)
	    {
	      if((t->Desk >last_desk_done)&&(t->Desk < next_desk))
		next_desk = t->Desk;
	    }
	}
      else if((val1 <4)&&(val1 > -4))
	{
	  if(last_desk_done  == MY_INT_MIN)
	    next_desk = Scr.CurrentDesk;
	  else
	    next_desk = MY_INT_MAX;
	}
      else 
	{
	  if(last_desk_done  == MY_INT_MIN)
	    next_desk = val2;
	  else
	    next_desk = MY_INT_MAX;
	}
      last_desk_done = next_desk;
      for (t = Scr.FvwmRoot.next; t != NULL; t = t->next)
	{
	  if((t->Desk == next_desk)&&
	    (!(LookInList(Scr.TheList,t->name,&t->class,&junk,&junkD)&LISTSKIP_FLAG)))
	    {
#ifdef HOTKEYS
	      if (++scut == ('9' + 1)) scut = 'A';	/* Next shortcut key */
#endif /* HOTKEYS */
	      if(val1%2 != 0)
		name = t->icon_name;
	      else
		name = t->name;
#ifdef HOTKEYS
	      t_hot = safemalloc(strlen(name) + 8);
	      sprintf(t_hot, "&%c.  %s", scut, name); /* Generate label */
#endif /* HOTKEYS */
	      
	      tname = safemalloc(40);
	      tname[0]=0;
	      if(t->flags & ICONIFIED)
		strcpy(tname, "(");
	      sprintf(loc,"%d:",t->Desk);
	      strcat(tname,loc);
	      if(t->frame_x >=0)
		sprintf(loc,"+%d",t->frame_x);
	      else
		sprintf(loc,"%d",t->frame_x);
	      strcat(tname, loc);
	      if(t->frame_y >=0)
		sprintf(loc,"+%d",t->frame_y);
	      else
		sprintf(loc,"%d",t->frame_y);
	      strcat(tname, loc);
	      dheight = t->frame_height - t->title_height - 2*t->boundary_width;
	      dwidth = t->frame_width - 2*t->boundary_width;
	      
	      dwidth -= t->hints.base_width;
	      dheight -= t->hints.base_height;
	      
	      dwidth /= t->hints.width_inc;
	      dheight /= t->hints.height_inc;

	      sprintf(loc,"x%d",dwidth);
	      strcat(tname, loc);
	      sprintf(loc,"x%d",dheight);
	      strcat(tname, loc);
	      if(t->flags & ICONIFIED)
		strcat(tname, ")");
		  
#ifdef HOTKEYS
	      AddToMenu(mr, t_hot, tname, NULL, F_RAISE_IT,(int)t->w,0);
#else
	      AddToMenu(mr, name, tname, NULL, F_RAISE_IT,(int)t->w,0);
#endif /* HOTKEYS */
	    }
	}
    }
  MakeMenu(mr);

  do_menu(mr);

  XDestroyWindow(dpy,mr->w);
  XDeleteContext(dpy, mr->w, MenuContext);  
  /* need to free the window list ? */
  mi = mr->first;
  while(mi != NULL)
    {
      tmp = mi->next;
      if (mi->func != F_TITLE)
	{
#ifdef HOTKEYS
	  if (mi->item != NULL) free(mi->item);
#endif /* HOTKEYS */
	  if (mi->item2 != NULL) free(mi->item2);
	}
      free(mi);
      mi = tmp;
    }
  free(mr);
#endif
}

void RaiseThisWindow(int w1)
{
#ifndef NO_WINDOWLIST
  FvwmWindow *t;
#ifndef NON_VIRTUAL
  int x,y;
#endif
  int mx,my;

  if (XFindContext(dpy, (Window)w1, FvwmContext, (caddr_t *)&t) == XCNOENT)
    return;

  if(t->Desk != Scr.CurrentDesk)
    {
      changeDesks(0,t->Desk);
    }
  /* De-iconify if its iconified */
  if (t->flags & ICONIFIED)
    DeIconify(t);

  /* raise to top */
  RaiseWindow(t);  

  /* If its on the virtual desktop, move to there */
  mx = t->frame_x + (t->frame_width>>1);
  my = t->frame_y + (t->frame_height>>1);
#ifndef NON_VIRTUAL
  if(((mx + Scr.Vx) < (Scr.VxMax + Scr.MyDisplayWidth))&&
     ((my + Scr.Vy) < (Scr.VyMax + Scr.MyDisplayHeight))&&
     ((my + Scr.Vy) >= 0)&&((mx + Scr.Vx) >= 0))
    {
      /* don't move the viewport unless the window is currently
       * off the viewport */
      if((mx<0)||(my < 0)||(mx>Scr.MyDisplayWidth)||(my>Scr.MyDisplayHeight))
	{
	  x = ((Scr.Vx + mx) /Scr.MyDisplayWidth)*Scr.MyDisplayWidth;
	  y = ((Scr.Vy + my) /Scr.MyDisplayHeight)*Scr.MyDisplayHeight;
	  MoveViewport(x,y,True);
	}
    }
  else
#endif
    {
      /* not on the desktop - move the window */
      if((mx < 0)||(my<0)||(mx > Scr.MyDisplayWidth)||(my>Scr.MyDisplayHeight))
	SetupFrame(t,0,0,t->frame_width,t->frame_height,FALSE);
    }

  XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, t->frame_x+2,t->frame_y+2);
  
  if(Scr.flags & ClickToFocus)
    SetFocus(t->w,t,False);
  KeepOnTop();    
#ifndef NO_PAGER
  RedrawPager();
#endif
#endif
}


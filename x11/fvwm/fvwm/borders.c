/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation (nation@rocket.sanders.lockheed.com 
 ****************************************************************************/
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 *
 * fvwm window border drawing code
 *
 ***********************************************************************/

#include "../configure.h"

#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "fvwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"

#ifdef SHAPE
#include <X11/extensions/shape.h>
#endif

/* macro to change window background color/pixmap */
#define ChangeWindowColor(window) {\
        if(NewColor)\
           {\
             XChangeWindowAttributes(dpy,window,valuemask, &attributes);\
             XClearWindow(dpy,window);\
           }\
         }

extern Window PressedW;

/****************************************************************************
 *
 * Redraws the windows borders
 *
 ****************************************************************************/
void SetBorder (FvwmWindow *t, Bool onoroff,Bool force,Bool Mapped, 
		Window expose_win)
{
  Window w=None;
  int y, i, x;
  GC ReliefGC,ShadowGC;
  Pixel BorderColor,BackColor;
  Pixmap BackPixmap,TextColor;
  Bool NewColor = False;
  XSetWindowAttributes attributes;
  unsigned long valuemask;
  static unsigned int corners[4];
  
  corners[0] = TOP_HILITE | LEFT_HILITE;
  corners[1] = TOP_HILITE | RIGHT_HILITE;
  corners[2] = BOTTOM_HILITE | LEFT_HILITE;
  corners[3] = BOTTOM_HILITE | RIGHT_HILITE;
  
  if(!t)
    return;
  
  if (onoroff) 
    {
      /* don't re-draw just for kicks */
      if((!force)&&(Scr.Hilite == t))
	return;
      
      
      if(Scr.Hilite != t)
	NewColor = True;
      
      /* make sure that the previously highlighted window got unhighlighted */
      if((Scr.Hilite != t)&&(Scr.Hilite != NULL))
	SetBorder(Scr.Hilite,False,False,True,None);
      
      /* set the keyboard focus */
      if((Mapped)&&(t->flags&MAPPED)&&(Scr.Hilite != t))
	w = t->w;
      else if((t->flags&ICONIFIED)&&
	      (Scr.Hilite !=t)&&(!(Scr.flags &SuppressIcons)))
	w = t->icon_w;
      Scr.Hilite = t;
      
      TextColor = Scr.HiColors.fore;
      BackPixmap= Scr.gray_pixmap;
      BackColor = Scr.HiColors.back;
      ReliefGC = Scr.HiReliefGC;
      ShadowGC = Scr.HiShadowGC;
      BorderColor = Scr.HiRelief.back;
    }
  else
    {
      /* don't re-draw just for kicks */
      if((!force)&&(Scr.Hilite != t))
	return;
      
      if(Scr.Hilite == t)
	{
	  Scr.Hilite = NULL;
	  NewColor = True;
	}
      
      TextColor = Scr.StdColors.fore;
      BackPixmap = Scr.light_gray_pixmap;
      BackColor = Scr.StdColors.back;
      ReliefGC = Scr.StdReliefGC;
      ShadowGC = Scr.StdShadowGC;
      BorderColor = Scr.StdRelief.back;
      if(t->flags & STICKY)
	{
	  TextColor = Scr.StickyColors.fore;
	  BackPixmap = Scr.sticky_gray_pixmap;
	  BackColor = Scr.StickyColors.back;
	  ReliefGC = Scr.StickyReliefGC;
	  ShadowGC = Scr.StickyShadowGC;
	  BorderColor = Scr.StickyRelief.back;	  
	}
    }
  
#ifndef NO_PAGER
  if((Scr.Pager_w) && !(t->flags & STICKY))
    {
      if(NewColor)
	{
	  if(Scr.d_depth < 2)
	    XSetWindowBackgroundPixmap(dpy,t->pager_view,BackPixmap);
	  else
	    XSetWindowBackground(dpy,t->pager_view,BackColor);
	  XClearWindow(dpy,t->pager_view);
	}
      if((t->icon_name != NULL)&&(Scr.PagerFont.height > 0))
	{
	  NewFontAndColor(Scr.PagerFont.font->fid,TextColor,BackColor);
	  XDrawImageString(dpy, t->pager_view, Scr.FontGC, 2,Scr.PagerFont.y+2,
			   t->icon_name, strlen(t->icon_name));
	}
    }
#endif
  
  if(t->flags & ICONIFIED)
    {
      DrawIconWindow(t);
      return;
    }
  
  valuemask = CWBorderPixel;
  attributes.border_pixel = BorderColor;
  if(Scr.d_depth < 2)
    {
      attributes.background_pixmap = BackPixmap;
      valuemask |= CWBackPixmap;
    }
  else
    {
      attributes.background_pixel = BackColor;
      valuemask |= CWBackPixel;
    }
  
  if(t->flags & (TITLE|BORDER))
    {
      XSetWindowBorder(dpy,t->Parent,BorderColor);
      XSetWindowBorder(dpy,t->frame,BorderColor);
    }
  if(t->flags & TITLE)
    {
      ChangeWindowColor(t->title_w);
      for(i=0;i<Scr.nr_left_buttons;i++)
	{
	  ChangeWindowColor(t->left_w[i]);
	  if(flush_expose(t->left_w[i])||(expose_win == t->left_w[i])||
	     (expose_win == None))
	    {	
	      RelieveWindow(t->left_w[i],0,0,t->title_height,
			    t->title_height,
			    (PressedW==t->left_w[i]?ShadowGC:ReliefGC), 
			    (PressedW==t->left_w[i]?ReliefGC:ShadowGC), 
			    BOTTOM_HILITE);
	      DrawPattern(t->left_w[i],ReliefGC,ShadowGC,
			  Scr.left_button_styles[1][i],
			  Scr.left_button_styles[0][i],t->title_height);
	    }
	  
	}	     
      for(i=0;i<Scr.nr_right_buttons;i++)
	{
	  if(t->right_w[i] != None)
	    {
	      ChangeWindowColor(t->right_w[i]);
	      if(flush_expose(t->right_w[i])||(expose_win==t->right_w[i])||
		 (expose_win == None))
		{
		  GC sgc,rgc;
		  
		  sgc=ShadowGC;
		  rgc=ReliefGC;
		  if((Scr.flags & MWMButtons)&&(!i)&&(t->flags&MAXIMIZED))
		    {
		      sgc = ReliefGC;
		      rgc = ShadowGC;
		    }
		  RelieveWindow(t->right_w[i],0,0,t->title_height,
				t->title_height,
				(PressedW==t->right_w[i]
				 ?ShadowGC:ReliefGC),
				(PressedW==t->right_w[i]
				 ?ReliefGC:ShadowGC), 
				BOTTOM_HILITE);
		  DrawPattern(t->right_w[i],rgc,sgc,
			      Scr.right_button_styles[1][i],
			      Scr.right_button_styles[0][i],
			      t->title_height);
		}
	      
	    }
	}
      SetTitleBar(t,onoroff, False);

    }
      
  if(t->flags & BORDER)
    {
      /* draw relief lines */
      y= t->frame_height - 2*t->corner_width - 2*t->bw+2;
      x = t->frame_width-  2*t->corner_width +1;
      
      for(i=0;i<4;i++)
	{
	  ChangeWindowColor(t->sides[i]);
	  if((flush_expose (t->sides[i]))||(expose_win == t->sides[i])||
	     (expose_win == None))
	    {
	      GC sgc,rgc;
	      
	      sgc=ShadowGC;
	      rgc=ReliefGC;
	      if(!(Scr.flags & MWMButtons)&&(PressedW == t->sides[i]))
		{
		  sgc = ReliefGC;
		  rgc = ShadowGC;
		}
	      /* index    side
	       * 0        TOP
	       * 1        RIGHT
	       * 2        BOTTOM
	       * 3        LEFT
	       */
	      
	      RelieveWindow(t->sides[i],0,0,
			    ((i%2)?t->boundary_width:x),
			    ((i%2)?y:t->boundary_width),
			    rgc, sgc, (0x0001<<i));
	    }
	  ChangeWindowColor(t->corners[i]);
	  if((flush_expose(t->corners[i]))||(expose_win==t->corners[i])||
	     (expose_win == None))
	    {
	      GC rgc,sgc;
	      
	      rgc = ReliefGC;
	      sgc = ShadowGC;
	      if(!(Scr.flags & MWMButtons)&&(PressedW == t->corners[i]))
		{
		  sgc = ReliefGC;
		  rgc = ShadowGC;
		}
	      RelieveWindow(t->corners[i],0,0,t->corner_width,
			    ((i/2)?t->corner_width+1:t->corner_width),
			    rgc,sgc, corners[i]);
	      RelieveParts(t,i,((i/2)?rgc:sgc),((i%2)?rgc:sgc));
	    }
	}
    }
  else      /* no decorative border */
    {
      /* for mono - put a black border on 
       * for color, make it the color of the decoration background */
      if(t->boundary_width < 4)
	{
	  flush_expose (t->frame);
      	  if(Scr.d_depth <2)
	    {
	      XSetWindowBorder(dpy,t->frame,TextColor);
	      XSetWindowBorder(dpy,t->Parent,TextColor);
	      XSetWindowBackgroundPixmap(dpy,t->frame,BackPixmap);
	      XClearWindow(dpy,t->frame);
	      XSetWindowBackgroundPixmap(dpy,t->Parent,BackPixmap);
	      XClearWindow(dpy,t->Parent);
	    }
	  else
	    {
	      if(t->boundary_width <2)
		{
		  XSetWindowBackground(dpy,t->frame,BorderColor);
		  XSetWindowBorder(dpy,t->frame,BorderColor);
		  XClearWindow(dpy,t->frame);
		  XSetWindowBackground(dpy,t->Parent,BorderColor);
		  XSetWindowBorder(dpy,t->Parent,BorderColor);
		  XClearWindow(dpy,t->Parent);
		  XSetWindowBorder(dpy,t->w,BorderColor);
		}
	      else
		{
		  XSetWindowBackground(dpy,t->frame,BackColor);
		  XSetWindowBorder(dpy,t->frame,BackColor);
		  XClearWindow(dpy,t->frame);
		  XSetWindowBackground(dpy,t->Parent,BackColor);
		  XSetWindowBorder(dpy,t->Parent,BackColor);
		  XClearWindow(dpy,t->Parent);
		  XSetWindowBorder(dpy,t->w,BackColor);
		}
	    }
	}
      else
	{
	  GC rgc,sgc;

	  XSetWindowBorder(dpy,t->Parent,BorderColor);
	  XSetWindowBorder(dpy,t->frame,BorderColor);	  

	  rgc=ReliefGC;
	  sgc=ShadowGC;
	  if(!(Scr.flags & MWMButtons)&&(PressedW == t->frame))
	    {
	      sgc=ReliefGC;
	      rgc=ShadowGC;
	    }
	  ChangeWindowColor(t->frame);
	  if((flush_expose(t->frame))||(expose_win == t->frame)||
	     (expose_win == None))
	    {
	      RelieveWindow(t->frame,0,0,t->frame_width+1,
			    t->frame_height+1,rgc,sgc,
			    TOP_HILITE|LEFT_HILITE|RIGHT_HILITE|BOTTOM_HILITE);
	      RelieveWindow(t->frame,t->boundary_width-2,t->boundary_width-2,
			    t->frame_width-(t->boundary_width<<1)+5,
			    t->frame_height-(t->boundary_width<<1)+5,sgc,rgc,
			    TOP_HILITE|LEFT_HILITE|RIGHT_HILITE|BOTTOM_HILITE);
	    }
	}
    }
}


/****************************************************************************
 *
 *  Redraws just the title bar
 *
 ****************************************************************************/
void SetTitleBar (FvwmWindow *t,Bool onoroff, Bool NewTitle)
{
  int hor_off, w;
  GC ReliefGC,ShadowGC;
  Pixel Forecolor, BackColor;

  if(!t)
    return;
  if(!(t->flags & TITLE))
    return;

  if (onoroff) 
    {
      Forecolor = Scr.HiColors.fore;
      BackColor = Scr.HiColors.back;
      ReliefGC = (PressedW==t->title_w?Scr.HiShadowGC:Scr.HiReliefGC);
      ShadowGC = (PressedW==t->title_w?Scr.HiReliefGC:Scr.HiShadowGC);
    }
  else
    {
      Forecolor = Scr.StdColors.fore;
      BackColor = Scr.StdColors.back;
      ReliefGC= (PressedW==t->title_w?Scr.StdShadowGC:Scr.StdReliefGC);
      ShadowGC= (PressedW==t->title_w?Scr.StdReliefGC:Scr.StdShadowGC);
      if(t->flags & STICKY)
	{
	  Forecolor = Scr.StickyColors.fore;
	  BackColor = Scr.StickyColors.back;
	  ReliefGC=(PressedW==t->title_w?Scr.StickyShadowGC:Scr.StickyReliefGC);
	  ShadowGC=(PressedW==t->title_w?Scr.StickyReliefGC:Scr.StickyShadowGC);
	}
    }
  flush_expose(t->title_w);
  
  w=XTextWidth(Scr.WindowFont.font,t->name,strlen(t->name));
  if(w > t->title_width-12)
    w = t->title_width-4;
  if(w < 0)
    w = 0;

  hor_off = (t->title_width - w)/2;
  
  NewFontAndColor(Scr.WindowFont.font->fid,Forecolor, BackColor);
  
  if(NewTitle)
    XClearWindow(dpy,t->title_w);
  
  /* for mono, we clear an area in the title bar where the window
   * title goes, so that its more legible. For color, no need */
  if(Scr.d_depth<2)
    {
      RelieveWindow(t->title_w,0,0,hor_off-2,t->title_height,
		    ReliefGC, ShadowGC, BOTTOM_HILITE);
      RelieveWindow(t->title_w,hor_off+w+2,0,
		    t->title_width - w - hor_off-2,t->title_height,
		    ReliefGC, ShadowGC, BOTTOM_HILITE);
      XFillRectangle(dpy,t->title_w,
		     (PressedW==t->title_w?ShadowGC:ReliefGC),
		     hor_off - 2, 0, w+4,t->title_height);
      
      XDrawLine(dpy,t->title_w,ShadowGC,hor_off+w+1,0,hor_off+w+1,
		t->title_height);
      XDrawString (dpy, t->title_w,Scr.FontGC,hor_off, Scr.WindowFont.y+1, 
		   t->name, strlen(t->name));
    }
  else
    { 
      XDrawString (dpy, t->title_w,Scr.FontGC,hor_off, Scr.WindowFont.y+1, 
		   t->name, strlen(t->name));
      RelieveWindow(t->title_w,0,0,t->title_width,t->title_height,
		    ReliefGC, ShadowGC, BOTTOM_HILITE);
    }
  
  XFlush(dpy);
}




/****************************************************************************
 *
 *  Draws the relief pattern around a window
 *
 ****************************************************************************/
FVWM_INLINE void RelieveWindow(Window win,int x,int y,int w,int h,
		   GC ReliefGC,GC ShadowGC, int hilite)
{
  XSegment seg[4];
  int i;

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y;

  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = x;        seg[i++].y2 = h+y-1;

  if (!(Scr.flags & MWMBorders)||(hilite & TOP_HILITE))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+1;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+1;
    }
  if (!(Scr.flags & MWMBorders)||(hilite & LEFT_HILITE))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+1;
      seg[i].x2 = x+1;      seg[i++].y2 = y+h-2;
    }
  XDrawSegments(dpy, win, ReliefGC, seg, i);

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y+h-1;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y+h-1;

  if (!(Scr.flags & MWMBorders)||(hilite & BOTTOM_HILITE))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+h-2;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;
    }

  seg[i].x1 = x+w-1;    seg[i].y1   = y;
  seg[i].x2 = x+w-1;    seg[i++].y2 = y+h-1;

  if (!(Scr.flags & MWMBorders)||(hilite & RIGHT_HILITE))
    {
      seg[i].x1 = x+w-2;    seg[i].y1   = y+1;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;
    }
  XDrawSegments(dpy, win, ShadowGC, seg, i);
}



void RelieveParts(FvwmWindow *t,int i,GC hor, GC vert)
{
  XSegment seg[2];
  
  if(Scr.flags & MWMBorders)
    {
      switch(i)
	{
	case 0:
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->corner_width;
	  seg[0].y1 = t->boundary_width-1;
	  seg[0].y2 = t->boundary_width-1;
	  break;
	case 1:
	  seg[0].x1 = 0;
	  seg[0].x2 = t->corner_width - t->boundary_width /* -1*/ ;
	  seg[0].y1 = t->boundary_width-1;
	  seg[0].y2 = t->boundary_width-1;
	  break;
	case 2:
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->corner_width-2;
	  seg[0].y1 = t->corner_width - t->boundary_width+1;
	  seg[0].y2 = t->corner_width - t->boundary_width+1;
	  break;
	case 3:
	  seg[0].x1 = 0;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  seg[0].y1 = t->corner_width - t->boundary_width+1;
	  seg[0].y2 = t->corner_width - t->boundary_width+1;
	  break;
	}
      XDrawSegments(dpy, t->corners[i], hor, seg, 1);
      switch(i)
	{
	case 0:
	  seg[0].y1 = t->boundary_width-1;
	  seg[0].y2 = t->corner_width;
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->boundary_width-1;
	  break;
	case 1:
	  seg[0].y1 = t->boundary_width -1;
	  seg[0].y2 = t->corner_width-2;
	  seg[0].x1 = t->corner_width - t->boundary_width;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  break;
	case 2:
	  seg[0].y1 = 0;
	  seg[0].y2 = t->corner_width - t->boundary_width;
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->boundary_width-1;
	  break;
	case 3:
	  seg[0].y1 = 0;
	  seg[0].y2 = t->corner_width - t->boundary_width + 1;
	  seg[0].x1 = t->corner_width - t->boundary_width;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  break;
	}
      XDrawSegments(dpy, t->corners[i], vert, seg, 1);
    }
  else
    {
      switch(i)
	{
	case 0:
	  seg[0].x1 = t->boundary_width-2;
	  seg[0].x2 = t->corner_width;
	  seg[0].y1 = t->boundary_width-2;
	  seg[0].y2 = t->boundary_width-2;
	  
	  seg[1].x1 = t->boundary_width-2;
	  seg[1].x2 = t->corner_width;
	  seg[1].y1 = t->boundary_width-1;
	  seg[1].y2 = t->boundary_width-1;
	  break;
	case 1:
	  seg[0].x1 = 1;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  seg[0].y1 = t->boundary_width-2;
	  seg[0].y2 = t->boundary_width-2;
	
	  seg[1].x1 = 0;
	  seg[1].x2 = t->corner_width - t->boundary_width-1;
	  seg[1].y1 = t->boundary_width-1;
	  seg[1].y2 = t->boundary_width-1;
	  break;
	case 2:
	  seg[0].x1 = t->boundary_width-2;
	  seg[0].x2 = t->corner_width-3;
	  seg[0].y1 = t->corner_width - t->boundary_width + 2;
	  seg[0].y2 = t->corner_width - t->boundary_width + 2;
	  
	  seg[1].x1 = t->boundary_width-1;
	  seg[1].x2 = t->corner_width-2;
	  seg[1].y1 = t->corner_width - t->boundary_width+1;
	  seg[1].y2 = t->corner_width - t->boundary_width+1;
	  break;
	case 3:
	  seg[0].x1 = 0;
	  seg[0].x2 = t->corner_width - t->boundary_width + 1;
	  seg[0].y1 = t->corner_width - t->boundary_width + 2;
	  seg[0].y2 = t->corner_width - t->boundary_width + 2;
	  
	  seg[1].x1 = 0;
	  seg[1].x2 = t->corner_width - t->boundary_width + 1;
	  seg[1].y1 = t->corner_width - t->boundary_width+1;
	  seg[1].y2 = t->corner_width - t->boundary_width+1;
	  break;
	}
      XDrawSegments(dpy, t->corners[i], hor, seg, 2);
      switch(i)
	{
	case 0:
	  seg[0].y1 = t->boundary_width-2;
	  seg[0].y2 = t->corner_width;
	  seg[0].x1 = t->boundary_width-2;
	  seg[0].x2 = t->boundary_width-2;
	  
	  seg[1].y1 = t->boundary_width-2;
	  seg[1].y2 = t->corner_width;
	  seg[1].x1 = t->boundary_width-1;
	  seg[1].x2 = t->boundary_width-1;
	  break;
	case 1:
	  seg[0].y1 = t->boundary_width-1;
	  seg[0].y2 = t->corner_width-2;
	  seg[0].x1 = t->corner_width - t->boundary_width;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  
	  seg[1].y1 = t->boundary_width-2;
	  seg[1].y2 = t->corner_width-3;
	  seg[1].x1 = t->corner_width - t->boundary_width+1;
	  seg[1].x2 = t->corner_width - t->boundary_width+1;
	  break;
	case 2:
	  seg[0].y1 = 1;
	  seg[0].y2 = t->corner_width - t->boundary_width+1;
	  seg[0].x1 = t->boundary_width-2;
	  seg[0].x2 = t->boundary_width-2;
	  
	  seg[1].y1 = 0;
	  seg[1].y2 = t->corner_width - t->boundary_width;
	  seg[1].x1 = t->boundary_width-1;
	  seg[1].x2 = t->boundary_width-1;
	  break;
	case 3:
	  seg[0].y1 = 0;
	  seg[0].y2 = t->corner_width - t->boundary_width + 2;
	  seg[0].x1 = t->corner_width - t->boundary_width + 1;
	  seg[0].x2 = t->corner_width - t->boundary_width + 1;
	  
	  seg[1].y1 = 0;
	  seg[1].y2 = t->corner_width - t->boundary_width + 2;
	  seg[1].x1 = t->corner_width - t->boundary_width;
	  seg[1].x2 = t->corner_width - t->boundary_width;
	  
	  break;
	}
      XDrawSegments(dpy, t->corners[i], vert, seg, 2);
    }
}
/****************************************************************************
 *
 *  Draws a little pattern within a window
 *
 ****************************************************************************/
FVWM_INLINE void DrawPattern(Window win, GC ShadowGC, GC ReliefGC, int h1, int w1, int t1)
{
  XSegment seg[2];
  int i,h,b,u,w,r,l;

  h = t1*h1/200;
  b = (t1>>1) + h;
  u = t1 - b-1;
  w = t1*w1/200;
  r = (t1>>1) + w;
  l = t1 - r-1;  

  i=0;
  seg[i].x1 = l;    seg[i].y1   = u;
  seg[i].x2 = r;    seg[i++].y2 =  u;

  seg[i].x1 = l;    seg[i].y1   = u;
  seg[i].x2 = l;    seg[i++].y2 = b;
  XDrawSegments(dpy, win, ShadowGC, seg, i);

  i=0;
  seg[i].x1 = l;    seg[i].y1   = b;
  seg[i].x2 = r;    seg[i++].y2 =  b;

  seg[i].x1 = r;    seg[i].y1   = u;
  seg[i].x2 = r;    seg[i++].y2 = b;
  XDrawSegments(dpy, win, ReliefGC, seg, i);
}



/***********************************************************************
 *
 *  Procedure:
 *      Setupframe - set window sizes, this was called from either
 *              AddWindow, EndResize, or HandleConfigureNotify.
 *
 *  Inputs:
 *      tmp_win - the FvwmWindow pointer
 *      x       - the x coordinate of the upper-left outer corner of the frame
 *      y       - the y coordinate of the upper-left outer corner of the frame
 *      w       - the width of the frame window w/o border
 *      h       - the height of the frame window w/o border
 *
 *  Special Considerations:
 *      This routine will check to make sure the window is not completely
 *      off the display, if it is, it'll bring some of it back on.
 *
 *      The tmp_win->frame_XXX variables should NOT be updated with the
 *      values of x,y,w,h prior to calling this routine, since the new
 *      values are compared against the old to see whether a synthetic
 *      ConfigureNotify event should be sent.  (It should be sent if the
 *      window was moved but not resized.)
 *
 ************************************************************************/

void SetupFrame(FvwmWindow *tmp_win,int x,int y,int w,int h,Bool sendEvent)
{
  XEvent client_event;
  XWindowChanges frame_wc, xwc;
  unsigned long frame_mask, xwcm;
  int cx,cy,i;
  Bool Resized = False;
#ifndef NO_PAGER
  FvwmWindow *t;
#endif
  int xwidth,ywidth,left,right;
  
  /* if windows is not being maximized, save size in case of maximization */
  if (!(tmp_win->flags & MAXIMIZED))
    {
      tmp_win->orig_x = x;
      tmp_win->orig_y = y;
      tmp_win->orig_wd = w;
      tmp_win->orig_ht = h;
    }
  if(Scr.flags & DontMoveOff)
    {
      if (x + Scr.Vx + w < 16)
	x = 16 - Scr.Vx - w;
      if (y + Scr.Vy + h < 16)
	y = 16 - Scr.Vy - h;
    }
  if (x >= Scr.MyDisplayWidth + Scr.VxMax - Scr.Vx-16)
    x = Scr.MyDisplayWidth + Scr.VxMax -Scr.Vx - 16;
  if (y >= Scr.MyDisplayHeight+Scr.VyMax - Scr.Vy -16)
    y = Scr.MyDisplayHeight + Scr.VyMax - Scr.Vy - 16;

  /*
   * According to the July 27, 1988 ICCCM draft, we should send a
   * "synthetic" ConfigureNotify event to the client if the window
   * was moved but not resized.
   */
  if ((x != tmp_win->frame_x || y != tmp_win->frame_y) &&
      (w == tmp_win->frame_width && h == tmp_win->frame_height))
    sendEvent = TRUE;

  if((w != tmp_win->frame_width) || (h != tmp_win->frame_height))
    Resized = True;

  if(Resized)
    {
      left = tmp_win->nr_left_buttons;
      right = tmp_win->nr_right_buttons;

      tmp_win->title_width= w- 
	(left+right)*tmp_win->title_height 
	  -2*tmp_win->boundary_width+1;


      if(tmp_win->title_width < 1) 
	tmp_win->title_width = 1;

      if (tmp_win->flags & TITLE) 
	{
	  xwcm = CWWidth | CWX | CWY;
	  tmp_win->title_x = tmp_win->boundary_width+
	    (left)*tmp_win->title_height;
	  if(tmp_win->title_x >=  w - tmp_win->boundary_width)
	    tmp_win->title_x = -10;
	  tmp_win->title_y = tmp_win->boundary_width;
	  
	  xwc.width = tmp_win->title_width;
	  xwc.x = tmp_win->title_x;
	  xwc.y = tmp_win->title_y;
	  XConfigureWindow(dpy, tmp_win->title_w, xwcm, &xwc);


	  xwcm = CWX | CWY;
	  xwc.y = tmp_win->boundary_width;
	  
	  xwc.x = tmp_win->boundary_width;
	  for(i=0;i<Scr.nr_left_buttons;i++)
	    {
	      if(tmp_win->left_w[i] != None)
		{
		  if(xwc.x + tmp_win->title_height < w - tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->left_w[i], xwcm, &xwc);
		  else
		    {
		      xwc.x = -tmp_win->title_height;
		      XConfigureWindow(dpy, tmp_win->left_w[i], xwcm, &xwc);
		    }
		  xwc.x += tmp_win->title_height;
		}
	    }
	  
	  xwc.x=w-tmp_win->boundary_width+1;
	  for(i=0;i<Scr.nr_right_buttons;i++)
	    {
	      if(tmp_win->right_w[i] != None)
		{
		  xwc.x -=tmp_win->title_height;
		  if(xwc.x > tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->right_w[i], xwcm, &xwc);
		  else
		    {
		      xwc.x = -tmp_win->title_height;
		      XConfigureWindow(dpy, tmp_win->right_w[i], xwcm, &xwc);
		    }
		}
	    }
	}

      if(tmp_win->flags & BORDER)
	{
	  xwidth = w- 2*tmp_win->corner_width+1;
	  ywidth = h - 2*tmp_win->corner_width;
	  xwcm = CWWidth | CWHeight | CWX | CWY;
	  if(xwidth<2)
	    xwidth = 2;
	  if(ywidth<2)
	    ywidth = 2;

	  for(i=0;i<4;i++)
	    {
	      if(i==0)
		{
		  xwc.x = tmp_win->corner_width;
		  xwc.y = 0;
		  xwc.height = tmp_win->boundary_width;
		  xwc.width = xwidth;
		}
	      else if (i==1)
		{
		  xwc.x = w - tmp_win->boundary_width+1;		
		  xwc.y = tmp_win->corner_width;
		  xwc.width = tmp_win->boundary_width;
		  xwc.height = ywidth;

		}
	      else if(i==2)
		{
		  xwc.x = tmp_win->corner_width;
		  xwc.y = h - tmp_win->boundary_width+1;
		  xwc.height = tmp_win->boundary_width+1;
		  xwc.width = xwidth;
		}
	      else
		{
		  xwc.x = 0;
		  xwc.y = tmp_win->corner_width;
		  xwc.width = tmp_win->boundary_width;
		  xwc.height = ywidth;
		}
	      XConfigureWindow(dpy, tmp_win->sides[i], xwcm, &xwc);
	    }

	  xwcm = CWX|CWY;
	  for(i=0;i<4;i++)
	    {
	      if(i%2)
		xwc.x = w - tmp_win->corner_width+1;
	      else
		xwc.x = 0;
	      
	      if(i/2)
		xwc.y = h - tmp_win->corner_width;
	      else
		xwc.y = 0;

	      XConfigureWindow(dpy, tmp_win->corners[i], xwcm, &xwc);
	    }

	}
    }
  tmp_win->attr.width = w - 2*tmp_win->boundary_width;
  tmp_win->attr.height = h - tmp_win->title_height 
    - 2*tmp_win->boundary_width;
  /* may need to omit the -1 for shaped windows, next two lines*/
  cx = tmp_win->boundary_width-tmp_win->bw;
  cy = tmp_win->title_height + tmp_win->boundary_width-tmp_win->bw;

  XResizeWindow(dpy, tmp_win->w, tmp_win->attr.width,
		tmp_win->attr.height);
  XMoveResizeWindow(dpy, tmp_win->Parent, cx,cy,
		    tmp_win->attr.width, tmp_win->attr.height);

  /* 
   * fix up frame and assign size/location values in tmp_win
   */
  frame_wc.x = tmp_win->frame_x = x;
  frame_wc.y = tmp_win->frame_y = y;
  frame_wc.width = tmp_win->frame_width = w;
  frame_wc.height = tmp_win->frame_height = h;
  frame_mask = (CWX | CWY | CWWidth | CWHeight);
  XConfigureWindow (dpy, tmp_win->frame, frame_mask, &frame_wc);


#ifdef SHAPE
  if ((Resized)&&(tmp_win->wShaped))
    {
      SetShape(tmp_win,w);
    }
#endif /* SHAPE */
  if (sendEvent)
    {
      client_event.type = ConfigureNotify;
      client_event.xconfigure.display = dpy;
      client_event.xconfigure.event = tmp_win->w;
      client_event.xconfigure.window = tmp_win->w;
      
      client_event.xconfigure.x = x + tmp_win->boundary_width;
      client_event.xconfigure.y = y + tmp_win->title_height+
	tmp_win->boundary_width;
      client_event.xconfigure.width = w-2*tmp_win->boundary_width;
      client_event.xconfigure.height =h-2*tmp_win->boundary_width -
	tmp_win->title_height;

      client_event.xconfigure.border_width =tmp_win->bw;
      /* Real ConfigureNotify events say we're above title window, so ... */
      /* what if we don't have a title ????? */
      client_event.xconfigure.above = tmp_win->frame;
      client_event.xconfigure.override_redirect = False;
      XSendEvent(dpy, tmp_win->w, False, StructureNotifyMask, &client_event);
    }
#ifndef NO_PAGER
  if(tmp_win == Scr.FvwmPager)
    {
      MoveResizeViewPortIndicator();
      for (t = Scr.FvwmRoot.next; t != NULL; t = t->next)
	{
	  MoveResizePagerView(t);
	}
    }
  else
    MoveResizePagerView(tmp_win);
#endif

  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
}


/****************************************************************************
 *
 * Sets up the shaped window borders 
 * 
 ****************************************************************************/
void SetShape(FvwmWindow *tmp_win, int w)
{
#ifdef SHAPE
  XRectangle rect;

  XShapeCombineShape (dpy, tmp_win->frame, ShapeBounding,
		      tmp_win->boundary_width,
		      tmp_win->title_height+tmp_win->boundary_width,
		      tmp_win->w,
		      ShapeBounding, ShapeSet);
  if (tmp_win->title_w) 
    {
      /* windows w/ titles */
      rect.x = tmp_win->boundary_width;
      rect.y = tmp_win->title_y;
      rect.width = w - 2*tmp_win->boundary_width+1;
      rect.height = tmp_win->title_height;
      
      
      XShapeCombineRectangles(dpy,tmp_win->frame,ShapeBounding,
			      0,0,&rect,1,ShapeUnion,Unsorted);
    }
#endif
}

/********************************************************************
 *
 * Sets the input focus to the indicated window.
 *
 * In a couple of cases CurrentTime is used (bad). These are
 * in click-to-focus mode, in response to MapNotify and UnmapNotify
 * events, which have no time-stamps and do not always relate to an
 * event that fvwm knows about
 *
 **********************************************************************/

void SetFocus(Window w, FvwmWindow *Fw, Bool Current)
{
  int i;
  extern Time lastTimestamp;
  Time tstamp;

#ifdef MULTIPLE_SCREENS
  if(Scr.NumberOfScreens > 1)
    {
      XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);
      if(JunkRoot != Scr.Root)
	{
	  /* Need to grab buttons for focus window */
	  if(Scr.Focus != NULL)
	    {
	      XSync(dpy,0);
	      for(i=0;i<3;i++)
		if(Scr.buttons2grab & (1<<i))
		  XGrabButton(dpy,(i+1),0,Scr.Focus->frame,True,
			      ButtonPressMask, GrabModeSync,GrabModeAsync,None,
			      Scr.FvwmCursors[SYS]);
	      Scr.Focus = NULL;
	      XSetInputFocus(dpy, Scr.NoFocusWin,RevertToParent,lastTimestamp);
	    }
	  return;
	}
    }
#endif

  if((Fw != NULL)&&(Fw->Desk != Scr.CurrentDesk))
    return;

  if((Scr.flags & ClickToFocus) && (Scr.Focus != Fw))
    {
      /* need to grab all buttons for window that we are about to
       * unfocus */
      if(Scr.Focus != NULL)
	{
	  XSync(dpy,0);
	  for(i=0;i<3;i++)
	    if(Scr.buttons2grab & (1<<i))
	      XGrabButton(dpy,(i+1),0,Scr.Focus->frame,True,
			  ButtonPressMask, GrabModeSync,GrabModeAsync,None,
			  Scr.FvwmCursors[SYS]);
	}
      /* if we do click to focus, remove the grab on mouse events that
       * was made to detect the focus change */
      if((Scr.flags & ClickToFocus)&&(Fw != NULL))
	{
	  for(i=0;i<3;i++)
	    if(Scr.buttons2grab & (1<<i))
	      XUngrabButton(dpy,(i+1),0,Fw->frame);
	}
    }
  if((Fw)&&(Fw->flags & ICONIFIED)&&(Fw->icon_w))
    w= Fw->icon_w;

  if(Current)
    tstamp = CurrentTime;
  else
    tstamp = lastTimestamp;

  if(Scr.flags & Lenience)
    {
      XSetInputFocus (dpy, w, RevertToParent, tstamp);
    }
  else if(!((Fw)&&(Fw->wmhints)&&(Fw->wmhints->flags & InputHint)&&
	    (Fw->wmhints->input == False)))
    {
      XSetInputFocus (dpy, w, RevertToParent, tstamp);
    }

  if ((Fw)&&(Fw->flags & DoesWmTakeFocus))
    send_clientmessage (w,_XA_WM_TAKE_FOCUS, lastTimestamp);

  Scr.Focus = Fw;
  XSync(dpy,0);

}




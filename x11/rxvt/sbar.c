/*  Copyright 1992 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */
/*
 * This module has been heavily modifiedby R. Nation
 * (nation@rocket.sanders.lockheed.com).
 * No additional restrictions are applied
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 */
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include "rxvt.h"
#include "xsetup.h"
#include "command.h"
#include "sbar.h"

/*  External global variables that are initialised at startup.
 */
extern Display		*display;
extern struct sbar_info sbar;

static unsigned char stipple_bits[] = {0x3e, 0x3e, 0x3e, 0x3e, 
					 0x3e, 0x3e, 0x3e, 0x3e};
static unsigned char fat_stipple_bits[] = 
{0xaa, 0x2a, 0x54, 0x15, 0xaa, 0x2a, 0x54, 0x15,0xaa, 0x2a, 0x54, 0x15,
   0xaa, 0x2a, 0x54, 0x15, 0xaa, 0x2a, 0x54, 0x15, 0xaa, 0x2a, 0x54, 0x15,
   0xaa, 0x2a, 0x54, 0x15, 0xaa, 0x2a , 0x54, 0x15};

static char uparrow_bits[] = {0x00, 0x08, 0x08, 0x1c, 0x1c, 0x3e, 0x3e, 0x00};
static char downarrow_bits[]={0x00, 0x3e, 0x3e, 0x1c, 0x1c, 0x08, 0x08, 0x00};

extern int fat_sbar;
/*  Initialise scrollbar data structures - called just once.
 */
void sbar_init()
{
  Pixmap stipple,up,down;
  XGCValues gcv;

  if(fat_sbar)
    {
      stipple=XCreateBitmapFromData(display,sbar.sb_win,fat_stipple_bits,16,16);
    }
  else
    {
      stipple = XCreateBitmapFromData(display,sbar.sb_win,stipple_bits,8,8);
    }
  if (stipple == (Pixmap)NULL) 
    {
      error("Cannot create scrollbar bitmap");
      clean_exit(1);
    }
  gcv.fill_style = FillOpaqueStippled;
  gcv.stipple = stipple;
  XChangeGC(display,sbar.sbgc,GCFillStyle|GCStipple,&gcv);

  if(!fat_sbar)
    {
      up = XCreateBitmapFromData(display,sbar.sb_up_win,uparrow_bits,8,8);
      if (up == (Pixmap)NULL) 
	{
	  error("Cannot create up bitmap");
	  clean_exit(1);
	}
      gcv.stipple = up;
      XChangeGC(display,sbar.sbupgc,GCFillStyle|GCStipple,&gcv);
      
      down = XCreateBitmapFromData(display,sbar.sb_down_win,downarrow_bits,8,8);
      if (down == (Pixmap)NULL) 
	{
	  error("Cannot create downarrow bitmap");
	  clean_exit(1);
	}
      gcv.stipple = down;
      XChangeGC(display,sbar.sbdowngc,GCFillStyle|GCStipple,&gcv);
    }

  sbar_show(100,0,100);
  sbar_down_reset();
  sbar_up_reset();
}

/*  Redraw the scrollbar to show the area from low to high proportional to
 *  length. */
void sbar_show(int length,int low,int high)
{
  static int last_length,last_low,last_high;
  static int last_drawn_length,last_drawn_low,last_drawn_high;
  int top, bot;

  if(length > 0)
    {
      last_length = length;
      last_low = low;
      last_high = high;
    }
  
  if((length >= 0)&&
     (last_drawn_length == last_length)&&
      (last_low == last_drawn_low)&&
      (last_high == last_drawn_high))
    {
      return;
    }

  last_drawn_length = last_length;
  last_drawn_low = last_low;
  last_drawn_high = last_high;
  
  top = sbar.height - 1 - sbar.height * last_high / last_length;
  bot = sbar.height - 1 - sbar.height * last_low / last_length;

  if (top > 0)
    XClearArea(display,sbar.sb_win,0,0,sbar.width,top - 1,False);
  if (bot >= top)
    XFillRectangle(display,sbar.sb_win,sbar.sbgc,0,top,sbar.width, bot-top+1);
  if (bot < sbar.height - 1)
    XClearArea(display,sbar.sb_win,0,bot+1,sbar.width,sbar.height-bot-1,False);
}

/*  Redraw the scrollbar's up arrow 
 */
void sbar_up_reset()
{
  if(!fat_sbar)
    XFillRectangle(display,sbar.sb_up_win,sbar.sbupgc,0,0,sbar.width,
		   sbar.width);
}

/*  Redraw the scrollbar's down arrow 
 */
void sbar_down_reset()
{
  if(!fat_sbar)
    XFillRectangle(display,sbar.sb_down_win,sbar.sbdowngc,0,0,sbar.width,
		   sbar.width);
}

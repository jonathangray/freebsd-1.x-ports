/* * Last edited: May 31 14:36 1991 (om) */
/* File: sather/lib/user_interface/C/sunwind_.c
   Author: Stephen M. Omohundro
   Created: Wed Oct 10 16:37:19 1990
   Copyright (C) International Computer Science Institute, 1990

   COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
   and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
   LICENSE contained in the file: "sather/doc/license.txt" of the Sather
   distribution. The license is also available from ICSI, 1947 Center
   St., Suite 600, Berkeley CA 94704, USA.

   Sather interface to suntools for use with "user_interface/sunwind.sa".
*/

#include "all_.h"
#include <stdio.h>
#include <suntool/sunview.h>
#include <pixrect/pixrect.h>
#include <suntool/panel.h>
#include <suntool/canvas.h>
#include <suntool/textsw.h>
#include <suntool/walkmenu.h>
#include <suntool/scrollbar.h>
#include <math.h>

#ifndef MIN
#define MIN(x,y) (((x)<(y))?(x):(y))
#endif
#ifndef ABS
#define ABS(x) (((x)<0)?-(x):(x))
#endif

extern void win_create(), win_loop();
extern void mouse_routine(), button_routine(), slider_routine();
extern void display_panel_message();
extern void draw_rect(),show_image(), show_image_xy(), clear_canvas();
extern void set_frame_label(), set_button_label(), set_button_fun();
extern void set_slider_label(), set_slider_params(), set_slider_fun();
extern int slider_val();
extern void set_text_label(), set_text_fun(), set_text_value();
extern char *get_text_value();
extern void draw_line(), draw_line_xor();
extern void draw_point(), draw_box_xor(), draw_box();
extern void draw_circle(), draw_circle_xor();

#define PANEL_WIDTH 320
#define IMAGE_WIDTH 512
#define IMAGE_HEIGHT 512
#define MAX_ITEMS 10

static struct pixfont *Font_14, *Font_10;
static Frame frame;
static Canvas canvas;
static Panel Control_Panel;
static Pixwin *pw;
static struct Rect *rec;
static Panel_item button[MAX_ITEMS], slider[MAX_ITEMS], text[MAX_ITEMS];
static Panel_item Message_Item;
static void (*(but_routines[10]))();
static void (*(sli_routines[10]))();
static void int_mouse_routine(), quit_routine();
static void void_fun(){}

static void but0() {button_routine(0,0);} 
static void but1() {button_routine(0,1);} 
static void but2() {button_routine(0,2);} 
static void but3() {button_routine(0,3);} 
static void but4() {button_routine(0,4);} 
static void but5() {button_routine(0,5);} 
static void but6() {button_routine(0,6);} 
static void but7() {button_routine(0,7);} 
static void but8() {button_routine(0,8);} 
static void but9() {button_routine(0,9);} 

static void sli0() {slider_routine(0,0,slider_val(0));}
static void sli1() {slider_routine(0,1,slider_val(1));}
static void sli2() {slider_routine(0,2,slider_val(2));}
static void sli3() {slider_routine(0,3,slider_val(3));}
static void sli4() {slider_routine(0,4,slider_val(4));}
static void sli5() {slider_routine(0,5,slider_val(5));}
static void sli6() {slider_routine(0,6,slider_val(6));}
static void sli7() {slider_routine(0,7,slider_val(7));}
static void sli8() {slider_routine(0,8,slider_val(8));}
static void sli9() {slider_routine(0,9,slider_val(9));}

/* Create a new window and display it */
void win_create(butnum, slinum, txtnum)
     int butnum, slinum, txtnum; /* number of buttons, sliders, and txt ins*/
{
  int i,j;
  char str[40];
  Pixwin *panel_pixwin, *frame_pixwin;
  Cursor visual_cursor;
  int y_offset=0;
  char message[64];
  
  Font_10=pf_open("/usr/lib/fonts/fixedwidthfonts/serif.r.10");
  Font_14=pf_open("/usr/lib/fonts/fixedwidthfonts/serif.r.14");
  frame = window_create(NULL, FRAME,
      FRAME_LABEL, "Generic Window",
		WIN_X, (1100-(IMAGE_WIDTH+PANEL_WIDTH))/2,
		WIN_Y, (900-IMAGE_HEIGHT)/2, 0);
  window_set(frame, WIN_FONT, Font_14, 0);
  
  frame_pixwin = (Pixwin *)window_get(frame, WIN_PIXWIN);

  canvas = window_create(frame, CANVAS, 
			 WIN_CONSUME_PICK_EVENTS,
			 /* WIN_NO_EVENTS, WIN_MOUSE_BUTTONS,*/ LOC_DRAG, 0,
			 WIN_EVENT_PROC, int_mouse_routine,
			 WIN_HEIGHT, IMAGE_HEIGHT,
			 WIN_WIDTH, IMAGE_WIDTH,
			 WIN_X, 0,
			 0);
  pw=canvas_pixwin(canvas);
  rec = (struct Rect *)window_get(canvas, WIN_RECT);

  Control_Panel = window_create(frame, PANEL,
				WIN_HEIGHT, IMAGE_HEIGHT,
				WIN_WIDTH, PANEL_WIDTH,
				WIN_RIGHT_OF, canvas,
				WIN_Y, 0,
				0);  
  window_set(Control_Panel, WIN_FONT, Font_10, 0);
  panel_pixwin = (Pixwin *)window_get(Control_Panel, WIN_PIXWIN);
  visual_cursor=window_get(canvas, WIN_CURSOR);
  cursor_set(visual_cursor, CURSOR_OP, PIX_SRC ^ PIX_DST, 0);
  window_set(canvas, WIN_CURSOR, visual_cursor, 0);

  but_routines[0]=but0;  but_routines[1]=but1;  but_routines[2]=but2;
  but_routines[3]=but3;  but_routines[4]=but4;  but_routines[5]=but5;
  but_routines[6]=but6;  but_routines[7]=but7;  but_routines[8]=but8;
  but_routines[9]=but9;  


  for(i=0; i<butnum; i++)
    {
      sprintf(str, "BUTTON %d", i);
      button[i]=
	panel_create_item(Control_Panel, PANEL_BUTTON,
		    PANEL_LABEL_IMAGE,
		    panel_button_image(Control_Panel, str,
				       strlen(str)+2, Font_10),
		    PANEL_ITEM_X, 5,
		    PANEL_ITEM_Y, y_offset+=20,
		    PANEL_NOTIFY_PROC, but_routines[i],
		    0);      
    }
  set_button_label(butnum-1, "QUIT");
  set_button_fun(butnum-1, quit_routine);

  sli_routines[0]=sli0;  sli_routines[1]=sli1;  sli_routines[2]=sli2;
  sli_routines[3]=sli3;  sli_routines[4]=sli4;  sli_routines[5]=sli5;
  sli_routines[6]=sli6;  sli_routines[7]=sli7;  sli_routines[8]=sli8;
  sli_routines[9]=sli9;  

  for(i=0; i<slinum; i++)
    {
      sprintf(str, "SLIDER %d", i);
      slider[i]=
	panel_create_item(Control_Panel, PANEL_SLIDER,
			  PANEL_LABEL_STRING, str,
		      PANEL_ITEM_X, 5,
		      PANEL_ITEM_Y, y_offset+=20,
		      PANEL_FONT, Font_10,
		      PANEL_VALUE, 0,
		      PANEL_MIN_VALUE, 0,
		      PANEL_MAX_VALUE, 100,
		      PANEL_NOTIFY_LEVEL, PANEL_DONE, 
		      PANEL_NOTIFY_PROC, sli_routines[i],
		      0);      
    }

  for(i=0; i<txtnum; i++)
    {
      sprintf(str, "TEXT %d:", i);
      text[i]=
	panel_create_item(Control_Panel, PANEL_TEXT, 
		      PANEL_ITEM_X, 5, 
		      PANEL_ITEM_Y, y_offset+=20,
		      PANEL_LABEL_STRING, str,
		      PANEL_VALUE_DISPLAY_LENGTH, 20,
		      PANEL_VALUE, "", 
		      PANEL_NOTIFY_PROC, panel_text_notify,
		      PANEL_NOTIFY_STRING, "\n\r\t",
		      PANEL_NOTIFY_LEVEL, PANEL_SPECIFIED,
		      0);
      
    }

  Message_Item=
    panel_create_item(Control_Panel, PANEL_MESSAGE, 
		      PANEL_ITEM_X, 5,
		      PANEL_ITEM_Y, y_offset+=20,
		      PANEL_LABEL_STRING, "", 0);
  window_fit(frame);
}
  

/* Do main window loop */
void win_loop()
{
  window_main_loop(frame);
}

static void quit_routine(item, event)
     Panel_item item;
     Event *event;
{
  exit(0);
}

/* This calls the Sather routine with args but, x, y, evt. but is 1,2,or 3
   depending on the button, x,y are ints giving coords in the canvas,
   evt is 1 for dn, 2 for up and 3 for drag */
static void int_mouse_routine(window, event, arg)
     Window window;
     Event *event;
     caddr_t arg;
{
  static int but,x,y,evt;      /*static so remembers which button for drags*/
  
  x=event_x(event); y=event_y(event);

  if(event_id(event)==MS_LEFT)
    {
      but=1;
      if(event_is_down(event)) evt=1; /* done this way to fix first click */
      else if (event_is_up(event)) evt=2;
      mouse_routine(0,but,x,y,evt); 
    }
  else if (event_id(event)==MS_MIDDLE)
    {
      but=2;      
      if(event_is_down(event)) evt=1;
      else if (event_is_up(event)) evt=2;
      mouse_routine(0,but,x,y,evt);
    }
  else if (event_id(event)==MS_RIGHT)
    {
      but=3;      
      if(event_is_down(event)) evt=1;
      else if (event_is_up(event)) evt=2;
      mouse_routine(0,but,x,y,evt);
    }
  else if (event_id(event)==LOC_DRAG)
    {
    evt=3;
    mouse_routine(0,but,x,y,evt);      
    }
}

void display_panel_message(message)
char *message;
{
  panel_set(Message_Item, PANEL_LABEL_STRING, message, 0);
}

void draw_rect(xa, ya, xb, yb)	/* xor a rectangle with coords in window */
     int xa, ya, xb, yb;
{
  if((xa+1 < xb) && (ya+1 < yb)) /* only draw if ok direction and size */
    {
      pw_batch_on(pw);
      pw_lock(pw, rec);
      pw_vector(pw, xa, ya, xb, ya, PIX_SRC ^ PIX_DST, 1); /* xor */
      pw_vector(pw, xb, ya+1, xb, yb-1, PIX_SRC ^ PIX_DST, 1);
      pw_vector(pw, xa, yb-1, xa, ya+1, PIX_SRC ^ PIX_DST, 1);
      pw_vector(pw, xb, yb, xa, yb, PIX_SRC ^ PIX_DST, 1);
      pw_unlock(pw);
      pw_batch_off(pw);
    }
}

/* This function puts an image up in the window. */
void show_image(pr)
     struct pixrect *pr;
{
  pw_rop(pw, 0, 0,
	 MIN(pr->pr_size.x, IMAGE_WIDTH),
	 MIN(pr->pr_size.y, IMAGE_HEIGHT),
	 PIX_SRC, pr, 0, 0);
}

/* This function puts an image up in the window with origin at x,y. */
void show_image_xy(pr,x,y)
     struct pixrect *pr;
     int x,y;
{
  pw_rop(pw, 0, 0,
	 MIN(pr->pr_size.x-x, IMAGE_WIDTH),
	 MIN(pr->pr_size.y-y, IMAGE_HEIGHT),
	 PIX_SRC, pr, x, y);
}

void clear_canvas()
{
    pw_writebackground(pw, 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT, PIX_CLR);
}

void set_frame_label(s)
     char *s;			/* new name */
{
  window_set(frame, FRAME_LABEL, s, 0);
}

void set_button_label(b,s)
     int b;
     char *s;
{
  panel_set(button[b], PANEL_LABEL_IMAGE,
	    panel_button_image(Control_Panel, s, strlen(s)+2, Font_10),0);
}
    
void set_button_fun(b,f)
     int b;
     void (*f)();
{
  panel_set(button[b], PANEL_NOTIFY_PROC, f, 0);
}

void set_slider_label(sl,s)
     int sl;
     char *s;
{
  panel_set(slider[sl], PANEL_LABEL_STRING, s, 0);
}

void set_slider_params(sl, minval, maxval, val)
     int sl,minval,maxval,val;
{
  panel_set(slider[sl],
	    PANEL_MIN_VALUE, minval,
	    PANEL_MAX_VALUE, maxval,
	    PANEL_VALUE, val, 0);
}

void set_slider_fun(sl,f)
     int sl;
     void (*f)();
{
  panel_set(slider[sl], PANEL_NOTIFY_PROC, f, 0);
}
     
int slider_val(sl)
     int sl;
{
  return((int)panel_get(slider[sl], PANEL_VALUE));
}

void set_text_label(t,s)
     int t;
     char *s;
{
  panel_set(text[t],PANEL_LABEL_STRING, s,0);
}

void set_text_value(t,s)
     int t;
     char *s;
{
  panel_set_value(text[t],s);
}

extern char *get_text_value(t)
     int t;
{
  return((char *)panel_get_value(text[t]));
}

void set_text_fun(t,f)
     int t;
     void (*f)();
{
  panel_set(text[t], PANEL_NOTIFY_PROC, f,0);
}

void draw_line(xa, ya, xb, yb) /* xor a line with coords in window */
     int xa, ya, xb, yb;
{
  pw_lock(pw, rec);
  pw_vector(pw, xa, ya, xb, yb, PIX_SRC, 1); 
  pw_unlock(pw);
}

void draw_circle(x,y,r)
     int x,y,r;
{
  int i,n,xl,yl,xn,yn;
  float c;

  n=(int)(5 * sqrt((float)ABS(r))); 
  c=2*3.1415/n;
  xl=(int)(r*cos(n*c))+x; yl=(int)(r*sin(n*c))+y;
  pw_lock(pw, rec);
  for(i=1; i<=n; i++)
    {
      xn=(int)(r*cos(i*c))+x; yn=(int)(r*sin(i*c))+y;
      pw_vector(pw, xl, yl, xn, yn, PIX_SRC, 1); 
      xl=xn; yl=yn;
    }
  pw_unlock(pw);
}

void draw_circle_xor(x,y,r)
     int x,y,r;
{
  int i,n,xl,yl,xn,yn;
  float c;

  n=(int)(5 * sqrt((float)ABS(r))); 
  c=2*3.1415/n;
  xl=(int)(r*cos(n*c))+x; yl=(int)(r*sin(n*c))+y;
  pw_lock(pw, rec);
  for(i=1; i<=n; i++)
    {
      xn=(int)(r*cos(i*c))+x; yn=(int)(r*sin(i*c))+y;
      pw_vector(pw, xl, yl, xn, yn, PIX_SRC ^ PIX_DST, 1); 
      xl=xn; yl=yn;
    }
  pw_unlock(pw);
}

void draw_line_xor(xa, ya, xb, yb)	/* xor a line with coords in window */
     int xa, ya, xb, yb;
{
  pw_lock(pw, rec);
  pw_vector(pw, xa, ya, xb, yb, PIX_SRC ^ PIX_DST, 1); /* xor */
  pw_unlock(pw);
}

void draw_point(x,y)
     int x,y;
{
  pw_lock(pw, rec);
  pw_put(pw,x,y,1);
  pw_unlock(pw);
}

/* static void init_pr_filled()
  int i,j,xb;
  unsigned char *pra,*lst;

  if((int) pr_filled != 0) return; /* already there * /
  pr_filled=mem_create(16,16,1);
  xb=mpr_d(pr_filled)->md_linebytes;
  pra=(unsigned char *)mpr_d(pr_filled)->md_image;
  lst= pra+16*xb;		/* pointer just after image * /
  for(; pra<lst; pra++)		/* loop over bytes in pixrect * /
    *pra= ~0;			/* set to 1 * /
}*/

/* draw an xored box on the screen of size sz and loc x,y */
void draw_box_xor(sz,x,y)
     int sz,x,y;
{
  pw_rop(pw, x, y, sz, sz, PIX_NOT(PIX_DST), pw, 0, 0);
}

/* draw a box on the screen of size sz and loc x,y */
void draw_box(sz,x,y)
     int sz,x,y;
{
  pw_rop(pw,x,y,sz,sz,PIX_SET,pw,0,0);
}

/* Put some text up */
void draw_text(x,y,s)
     int x,y;
     char *s;
{
  pw_text(pw,x,y,PIX_SRC,Font_10,s);
}

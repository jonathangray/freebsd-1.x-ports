#ifdef MPWC
#include <Windows.h>
#include <Menus.h>
#include <ToolUtils.h>
#include <Events.h>
#include <Controls.h>
#include <Fonts.h>
#include <Script.h>
#else
#include <WindowMgr.h>
#include <MenuMgr.h>
#include <ToolboxUtil.h>
#include <EventMgr.h>
#include <ControlMgr.h>
#include <FontMgr.h>
#include <Color.h>
#endif MPWC

#include "StGWWindow.h"
#include "macgraphwindow.h"

/**************************************************************************/
/**                                                                      **/
/**                    Action Installation Functions                     **/
/**                                                                      **/
/**************************************************************************/

StGWSetFreeMem(gwinfo, FreeMem)
	StGWWinInfo *gwinfo;
	int (*FreeMem)();
{
  if (gwinfo == nil) return;
  else gwinfo->FreeMem =  FreeMem;
}

StGWIdleOn(gwinfo)
	StGWWinInfo *gwinfo;
{
  
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->idleOn);
}

StGWSetIdleOn(gwinfo, on)
	StGWWinInfo *gwinfo;
	int on;
{
  if (gwinfo != nil) gwinfo->idleOn = on;
}

/**************************************************************************/
/**                                                                      **/
/**                      Window Management Functions                     **/
/**                                                                      **/
/**************************************************************************/

StGWShowWindow(gwinfo)
	StGWWinInfo *gwinfo;
{
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else {
    MyShowWindow(w);
	if (! gwinfo->initialized) StGWInitialDraw(gwinfo);
  }
}

StGWRemove(gwinfo)
	StGWWinInfo *gwinfo;
{
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else SkelRmveWind(w);
}

StGWWhileButtonDown(gwinfo, action, motionOnly)
	StGWWinInfo *gwinfo;
	int (*action)(), motionOnly;
{
  Point pt;
  GrafPtr savePort;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  GetPort(&savePort);
  SetPort(w);
  while (StillDown()) {
    SetPort(w);
    mac_do_cursor(gwinfo);
    GetMouse(&pt);
    if (gwinfo->mouse_x != pt.h || gwinfo->mouse_y != pt.v || ! motionOnly) {
      gwinfo->mouse_x = pt.h; gwinfo->mouse_y = pt.v;
      if (action != nil) (*action)(w, pt.h, pt.v);
    }
  }
  SetPort(savePort);
}

static TitleBarHeight(w)
	WindowPtr w;
{
  Rect r;
  Point pt;
  WindowPeek wind = (WindowPeek) w;
  GrafPtr savePort;
  
  GetPort(&savePort);
  SetPort(w);
  SetPt(&pt, 0, 0);
  LocalToGlobal(&pt);
  r = (*(wind->strucRgn))->rgnBBox;
  SetPort(savePort);
  return(pt.v - r.top);
}

StWSetLocation(w, left, top, frame)
	WindowPtr w;
	int left, top, frame;
{
  int adjust;
  
  if (w == nil) return;
  adjust = (frame) ? MBarHeight + TitleBarHeight(w) : MBarHeight;
  MoveWindow(w, left, top + adjust, FALSE);
}

StWGetLocation(w, left, top, frame)
	WindowPtr w;
	int *left, *top, frame;
{
  GrafPtr savePort;
  Point pt;
  int adjust;
  
  if (w == nil) return;
  else {
    adjust = (frame) ? MBarHeight + TitleBarHeight(w) : MBarHeight;
    GetPort(&savePort);
    SetPort(w);
    pt.h = w->portRect.left;
    pt.v = w->portRect.top;
    LocalToGlobal(&pt);
    if (left != nil) *left = pt.h;
    if (top != nil) *top = pt.v - adjust;
    SetPort(savePort);
  }
}

StGWSetSize(gwinfo, width, height, frame)
	StGWWinInfo *gwinfo;
	int width, height;
{
  WindowPtr w;
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else StWSetSize(w, width, height, frame);
}

StWSetSize(w, width, height, frame)
	WindowPtr w;
	int width, height;
{
  GrafPtr savePort;
  
  if (w == nil) return;
  GetPort(&savePort);
  SetPort(w);
  SizeWindow(w, width, height - ((frame) ? TitleBarHeight(w) : 0), TRUE);
  InvalRect(&w->portRect);
  mac_update_action(TRUE);
  SetPort(savePort);
}

StWGetSize(w, width, height, frame)
	WindowPtr w;
	int *width, *height;
{
  
  if (w == nil) return;
  else {
    if (width != nil) *width = w->portRect.right - w->portRect.left;
    if (height != nil) *height = w->portRect.bottom - w->portRect.top 
                               + ((frame) ? TitleBarHeight(w) : 0);
  }
}

/**************************************************************************/
/**                                                                      **/
/**             Window State Access and Mutation Functions               **/
/**                                                                      **/
/**************************************************************************/

SetHardwareState(gwinfo)
	StGWWinInfo *gwinfo;
{
  GrafPtr savePort;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;

  GetPort(&savePort);
  SetPort(w);
  
  set_fore_color(gwinfo);
  set_back_color(gwinfo);
  if (gwinfo->drawMode == 0) w->pnMode = patCopy;
  else w->pnMode = patXor;
  if (gwinfo->drawMode == 0) {
    w->txMode = srcOr;
    gwinfo->symbolMode = srcCopy;
  }
  else {
    w->txMode = srcXor;
    gwinfo->symbolMode = srcXor;
  }
  
  /* set line type */
  if (gwinfo->lineType != 0 && gwinfo->drawColor != gwinfo->backColor)
    PenPat(gray);
  else PenPat(black);
  
  /* set pen size */
  PenSize(gwinfo->lineWidth, gwinfo->lineWidth);
  
  adjust_graph_workport(gwinfo);
  SetPort(savePort);
}

static get_state(gwinfo, which)
    StGWWinInfo *gwinfo;
	int which;
{
  if (gwinfo == nil) return(0);
  switch (which) {
  case 'W': return(gwinfo->canvasWidth);
  case 'H': return(gwinfo->canvasHeight);
  case 'L': return(gwinfo->lineType);
  case 'M': return(gwinfo->drawMode);
  case 'D': return(gwinfo->drawColor);
  case 'B': return(gwinfo->backColor);
  case 'C': return(gwinfo->use_color);
  }
}

StGWCanvasWidth(gwinfo) StGWWinInfo *gwinfo; { return (get_state(gwinfo, 'W')); }
StGWCanvasHeight(gwinfo) StGWWinInfo *gwinfo;{ return (get_state(gwinfo, 'H')); }
StGWLineType(gwinfo) StGWWinInfo *gwinfo;    { return (get_state(gwinfo, 'L')); }
StGWDrawMode(gwinfo) StGWWinInfo *gwinfo;    { return (get_state(gwinfo, 'M')); }

ColorCode StGWDrawColor(gwinfo) 
	StGWWinInfo *gwinfo;
{
  return ((ColorCode) get_state(gwinfo, 'D'));
}

ColorCode StGWBackColor(gwinfo)
	StGWWinInfo *gwinfo;
{
  return ((ColorCode) get_state(gwinfo, 'B'));
}

StGWUseColor(gwinfo) StGWWinInfo *gwinfo; { return (get_state(gwinfo, 'C')); }

StGWGetLineWidth(gwinfo, width)
	StGWWinInfo *gwinfo;
	int *width;
{
  if (gwinfo == nil) return;
  if (width != nil) *width = gwinfo->lineWidth;
}

static set_state(gwinfo, which, value)
	StGWWinInfo *gwinfo;
	int which, value;
{
  int changed;
  
  if (gwinfo == nil) return;
  switch (which) {
  case 'L': if ((changed = (value != gwinfo->lineType))) gwinfo->lineType = value;  break;
  case 'M': if ((changed = (value != gwinfo->drawMode))) gwinfo->drawMode = value;  break;
  case 'D': if ((changed = (value != gwinfo->drawColor))) gwinfo->drawColor = value; break;
  case 'B': if ((changed = (value != gwinfo->backColor))) gwinfo->backColor = value; break;
  case 'C': if ((changed = (value != gwinfo->use_color))) 
              gwinfo->use_color = (StScreenHasColor()) ? value : FALSE;
            break;
  }
  if (changed) SetHardwareState(gwinfo);
  if (changed && which == 'B') DrawGWGrowBox(gwinfo);
}

StGWSetLineType(gwinfo, type)
	StGWWinInfo *gwinfo;
	int type;
{
  set_state(gwinfo, 'L', type);
}

StGWSetDrawMode(gwinfo, mode)
	StGWWinInfo *gwinfo;
	int mode;
{
  set_state(gwinfo, 'M', mode);
}

StGWSetDrawColor(gwinfo, color)
	StGWWinInfo *gwinfo;
	ColorCode color;
{
  set_state(gwinfo, 'D', (ColorCode) color);
}

StGWSetBackColor(gwinfo, color)
	StGWWinInfo *gwinfo;
	ColorCode color;
{
  set_state(gwinfo, 'B', (ColorCode) color);
}

StGWSetUseColor(gwinfo, use)
	StGWWinInfo *gwinfo;
	int use;
{
  set_state(gwinfo, 'C', use);
}

StGWSetLineWidth(gwinfo, width)
	StGWWinInfo *gwinfo;
	int width;
{
  int changed;
  
  if (gwinfo == nil) return;
  changed = (width != gwinfo->lineWidth);
  if (changed) {
    gwinfo->lineWidth = width;
    SetHardwareState(gwinfo);
  }
}

StGWReverseColors(gwinfo)
	StGWWinInfo *gwinfo;
{
  ColorCode backColor, drawColor;
  char *object;
  
  object = (char *) StGWGetObject(gwinfo);

  backColor = StGWBackColor(gwinfo);
  drawColor = StGWDrawColor(gwinfo);
  if (backColor != drawColor) {
    StGWSetBackColor(gwinfo, drawColor);
    StGWSetDrawColor(gwinfo, backColor);
    StGWObRedraw(object);
  }
}

StGWGetViewRect(gwinfo, left, top, width, height)
	StGWWinInfo *gwinfo;
	int *left, *top, *width, *height;
{
  WindowPtr w;
  
  if (gwinfo != nil && (w = gwinfo->window) != nil) {
    if (left != nil) *left = w->portRect.left;
    if (top != nil) *top = w->portRect.top;
    if (width != nil) *width = w->portRect.right - w->portRect.left - 15;
    if (height != nil) *height = w->portRect.bottom - w->portRect.top;
    if (height != nil && StGWHasHscroll(gwinfo)) *height -= 15;
  }
  else {
    if (left != nil) *left = 0;
    if (top != nil) *top = 0;
    if (width != nil) *width = 1;
    if (height != nil) *height = 1;
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       Window Scrolling Functions                     **/
/**                                                                      **/
/**************************************************************************/

static set_has_scroll(gwinfo, which, has, size)
	StGWWinInfo *gwinfo;
	int which, has, size;
{
  WindowPtr w;
  GrafPtr savePort;
  int view_size, value, old_has;
  ControlHandle ctl;
  char *object;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;

  GetPort(&savePort);
  SetPort(w);
  
  if (has && size <= 0) StPerror("size must be positive");
  object = (char *) StGWGetObject(gwinfo);

  ClipRect(&w->portRect);

  ctl = (which == 'H') ? gwinfo->hscroll : gwinfo->vscroll;
   
  old_has = (which == 'H') ? gwinfo->hasHscroll : gwinfo->hasVscroll;
  if (which == 'H') gwinfo->hasHscroll = has;
  else gwinfo->hasVscroll = has;
    
  if (has) {
    if (w == FrontWindow()) HiliteControl(ctl, 0);
    else HiliteControl(ctl, 255);
    if (which == 'H') {
      if (! old_has) gwinfo->canvasHeight -= 15;
      if (gwinfo->hasVscroll) {
        StGWGetViewRect(gwinfo, nil, nil, nil, &view_size);
        value = (gwinfo->canvasHeight - view_size > 0)
              ? gwinfo->canvasHeight - view_size : 0;
        SetCtlMax(gwinfo->vscroll, value);
      }
      gwinfo->canvasWidth = size;
      StGWGetViewRect(gwinfo, nil, nil, &view_size, nil);
    }
    else {
      gwinfo->canvasHeight = size;
      StGWGetViewRect(gwinfo, nil, nil, nil, &view_size);
    }
    value = (size - view_size > 0) ? size - view_size : 0;
    SetCtlMax(ctl, value);
    ShowControl(ctl);
  }
  else {
    if (which == 'H') {
      StGWGetViewRect(gwinfo, nil, nil, &gwinfo->canvasWidth, nil);
      if (old_has) gwinfo->canvasHeight += 15;
      StGWSetScroll(gwinfo, 0, gwinfo->view_v, TRUE);
    }
    else {
      StGWGetViewRect(gwinfo, nil, nil, nil, &gwinfo->canvasHeight);
      StGWSetScroll(gwinfo, gwinfo->view_h, 0, TRUE);
    }
    HideControl(ctl);
    SetCtlMax(ctl, 0);
  }
  InvalRect(&w->portRect);
  reset_clip_rect(gwinfo);
  SetPort(savePort);

  StGWObResize(object);
}

StGWSetHasHscroll(gwinfo, has, size)
	StGWWinInfo *gwinfo;
	int has, size;
{
  set_has_scroll(gwinfo, 'H', has, size);
}

StGWSetHasVscroll(gwinfo, has, size)
	StGWWinInfo *gwinfo;
	int has, size;
{
  set_has_scroll(gwinfo, 'V', has, size);
}

StGWHasHscroll(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->hasHscroll);
}

StGWHasVscroll(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->hasVscroll);
}

StGWSetScroll(gwinfo, h, v, move)
	StGWWinInfo *gwinfo;
	int h, v, move;
{
  WindowPtr w;
  GrafPtr savePort;
  Rect r;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  gwinfo->view_h = (gwinfo->hasHscroll) ? h : 0;
  gwinfo->view_v = (gwinfo->hasVscroll) ? v : 0;
    
  GetPort(&savePort);
  SetPort(w);
  ClipRect(&w->portRect);
  SetCtlValue(gwinfo->hscroll, h);
  SetCtlValue(gwinfo->vscroll, v);
  gwinfo->view_h = GetCtlValue(gwinfo->hscroll);
  gwinfo->view_v = GetCtlValue(gwinfo->vscroll);
  SetOrigin(gwinfo->view_h, gwinfo->view_v);
  reset_clip_rect(gwinfo);
  if (move) {
    r = scroll_bar_bounds(w, 'H');
    MoveControl(gwinfo->hscroll, r.left, r.top);
    r = scroll_bar_bounds(w, 'V');
    MoveControl(gwinfo->vscroll, r.left, r.top);
  }
  SetPort(savePort);
}

StGWGetScroll(gwinfo, h, v)
	StGWWinInfo *gwinfo;
	int *h, *v;
{
  
  if (gwinfo != nil) {
    if (h != nil) *h = gwinfo->view_h;
    if (v != nil) *v = gwinfo->view_v;
  }
}

StGWSetHscrollIncs(gwinfo, inc, pageInc)
	StGWWinInfo *gwinfo;
	int inc, pageInc;
{
  if (gwinfo == nil) return;
  gwinfo->h_scroll_inc[0] = inc;
  gwinfo->h_scroll_inc[1] = pageInc;
}

StGWGetHscrollIncs(gwinfo, inc, pageInc)
	StGWWinInfo *gwinfo;
	int *inc, *pageInc;
{
  if (gwinfo == nil) return;
  if (inc != 0) *inc = gwinfo->h_scroll_inc[0];
  if (pageInc != 0) *pageInc = gwinfo->h_scroll_inc[1];
}

StGWSetVscrollIncs(gwinfo, inc, pageInc)
	StGWWinInfo *gwinfo;
	int inc, pageInc;
{
  if (gwinfo == nil) return;
  gwinfo->v_scroll_inc[0] = inc;
  gwinfo->v_scroll_inc[1] = pageInc;
}

StGWGetVscrollIncs(gwinfo, inc, pageInc)
	StGWWinInfo *gwinfo;
	int *inc, *pageInc;
{
  if (gwinfo == nil) return;
  if (inc != 0) *inc = gwinfo->v_scroll_inc[0];
  if (pageInc != 0) *pageInc = gwinfo->v_scroll_inc[1];
}

/**************************************************************************/
/**                                                                      **/
/**                    Graph Window RefCon Functions                     **/
/**                                                                      **/
/**************************************************************************/

StGWSetRefCon(gwinfo, x)
	StGWWinInfo *gwinfo;
	long x;
{
  if (gwinfo == nil) return;
  else gwinfo->RefCon = x;
}

long StGWGetRefCon(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo == nil) return(nil);
  else return(gwinfo->RefCon);
}
 
StGWSetObject(gwinfo, x)
	StGWWinInfo *gwinfo;
	long x;
{
  if (gwinfo == nil) return;
  else gwinfo->Object = x;
}

long IViewWindowGetObject(w)
	WindowPtr w;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  if (gwinfo == nil) return(nil);
  else return(gwinfo->Object);
}

long StGWGetObject(gwinfo)
	StGWWinInfo *gwinfo;
{
  return((gwinfo != nil) ? gwinfo->Object : nil);
}
       
StGWInitialDraw(gwinfo)
	StGWWinInfo *gwinfo;
{
  WindowPtr w;
  
  if (gwinfo != nil && (w = gwinfo->window) != nil) { 
    graph_update_action(gwinfo, TRUE);
    /*ValidRect(&w->portRect);*//* causes problems with new initial draw */
  }
}

/**************************************************************************/
/**                                                                      **/
/**                    Graph Window Color Functions                      **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicColors 8
#define NumRGBColors 100
# define MULTIPLIER 62535

static int NumColors;

typedef struct {
  union {
    long old;
    RGBColor rgb;
  } value;
  long refcon;
} ctab_entry;

static ctab_entry *ctable;

init_mac_colors()
{
  NumColors = NumBasicColors;
  if (StScreenHasColor()) NumColors += NumRGBColors;
  ctable = (ctab_entry *) StCalloc(NumColors, sizeof(ctab_entry));
  
  ctable[0].value.old = whiteColor;
  ctable[1].value.old = blackColor;
  ctable[2].value.old = redColor;
  ctable[3].value.old = greenColor;
  ctable[4].value.old = blueColor;
  ctable[5].value.old = cyanColor;
  ctable[6].value.old = magentaColor;
  ctable[7].value.old = yellowColor;
}

StGWMakeColor(red, green, blue, refcon)
	double red, green, blue;
	long refcon;
{
  int index;
  
  for (index = NumBasicColors; 
       index < NumColors && StGWGetColRefCon(index) != nil; 
       index++);
  if (index >= NumColors) return(-1);
  else {
    StGWSetColRefCon(index, refcon);
    ctable[index].value.rgb.red = MULTIPLIER * red;
    ctable[index].value.rgb.green = MULTIPLIER * green;
    ctable[index].value.rgb.blue = MULTIPLIER * blue;
    return(index);
  }
}

StGWFreeColor(index)
	unsigned int index;
{
  if (index < NumColors && index >= NumBasicColors)
    StGWSetColRefCon(index, nil);
  else StPerror("can't free standard color");
}

StGWSetColRefCon(index, rc)
	unsigned int index;
	long rc;
{
  if (index < NumColors) ctable[index].refcon = rc;
}

long StGWGetColRefCon(index)
	unsigned int index;
{	
  if (index < NumColors) return(ctable[index].refcon);
  else return(nil);
}

static long get_color(index)
	unsigned int index;
{
  if (index < NumBasicColors) return(ctable[index].value.old);
  else return(ctable[1].value.old);
}

static RGBColor *get_rgb_color(index)
	unsigned int index;
{
  if (index < NumColors) return(&ctable[index].value.rgb);
  else StPerror("bad rgb color");
}

set_fore_color(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo->drawColor < NumBasicColors)
    ForeColor(get_color(gwinfo->drawColor));
  else RGBForeColor(get_rgb_color(gwinfo->drawColor));
}

set_back_color(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo->backColor < NumBasicColors)
    BackColor(get_color(gwinfo->backColor));
  else RGBForeColor(get_rgb_color(gwinfo->backColor));
}

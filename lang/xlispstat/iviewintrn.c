/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "iview.h"

#define BRUSH_WIDTH 20
#define BRUSH_HEIGHT 40
#define AXIS_LABEL_GAP 4
#define AXIS_TICK_LENGTH 3
#define AXIS_LABEL_TEMPLATE "12345"
#define CLICK_WIDTH 4
#define CLICK_HEIGHT 4

#ifndef TRUE
#define TRUE 1
#endif TRUE
#ifndef FALSE
#define FALSE 0
#endif FALSE

extern char *IViewDataNew();
extern PointState IViewPointState();
extern char *IViewVariableLabel();

/* Forward Declarations */
char *IViewData();

typedef struct brush {
  int left, top, width, height, showing;
} Brush;

typedef struct clickrange {
  int width, height;
} ClickRange;

typedef struct content {
  int left, top, width, height, origin_x, origin_y, x_variable, y_variable;
} Content;

typedef struct {
  int left, top, right, bottom;
} Margin;

typedef struct {
  int showing, labeled, ticks, height, edge;
} Axis;

typedef struct iview {
  char *data;
  Content content;
  Margin margin;
  Axis x_axis, y_axis;
  Brush brush;
  ClickRange clickrange;
  MouseMode mouseMode;
  int showingLabels, fixed_aspect, dirty;
  long links;
  double *scale, *shift;
} *IView;

typedef IView StGrInfo;

#define IViewGetIView(w) ((IView) StGWGetRefCon(IViewWindowWinInfo(w)))

/**************************************************************************/
/**                                                                      **/
/**                       IView Creation Functions                       **/
/**                                                                      **/
/**************************************************************************/

IViewFreeMem(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);

  if (iview != nil) {
    if (IViewData(w) != nil) {
      IViewDataFree(IViewData(w));
      IViewSetData(w, nil);
    }
    StFree(iview->scale);
    StFree(iview->shift);
    StFree(iview);
    StGWSetRefCon(IViewWindowWinInfo(w), nil);
  }
}

IVIEW_WINDOW IViewNew(object)
     char *object;
{
  IVIEW_WINDOW w = (IVIEW_WINDOW) IViewWindowNew(object, FALSE);
  IView iview;
  char *gwinfo;
  int vars, i;

  gwinfo = StGWObWinInfo(object);
  get_iview_ivars(object, &vars);

  iview = (IView) StCalloc(sizeof(struct iview), 1);
  StGWSetRefCon(gwinfo, iview);
  IViewSetData(w, IViewDataNew(vars));
  iview->scale = (double *) StCalloc(vars, sizeof(double));
  iview->shift = (double *) StCalloc(vars, sizeof(double));
  
  StGWSetFreeMem(gwinfo, IViewFreeMem);
  StGrSetContentVariables(gwinfo, 0, 1);
  IViewSetBrush(w, 0, 0, BRUSH_WIDTH, BRUSH_HEIGHT);
  StGrSetClickRange(gwinfo, CLICK_WIDTH, CLICK_HEIGHT);
  IViewSetShowingLabels(w, FALSE);
  IViewSetMouseMode(w, selecting);
  IViewSetIsLinked(w, FALSE);
  StGrSetMargin(gwinfo, 0, 0, 0, 0);
  IViewSetFixedAspect(w, TRUE);
  iview->brush.showing = FALSE;
  
  for (i = 0; i < vars; i++) {
    IViewSetScaledRange(w, i, 0.0, 1.0);
    IViewSetScale(w, i, 1.0);
    IViewSetShift(w, i, 0.0);
  }
  return(w);
}

/**************************************************************************/
/**                                                                      **/
/**                 IView State Accessors and Mutators                   **/
/**                                                                      **/
/**************************************************************************/

StGrDirty(gwinfo) 
	char *gwinfo;
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == nil) StPerror("no graph installed in this window");
  return(gr->dirty);
}

StGrSetDirty(gwinfo, dirty) 
	char *gwinfo;
	int dirty;
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == nil) StPerror("no graph installed in this window");
  gr->dirty = dirty;
}

static  StGrInfo StGrGetGrInfo(gwinfo) 
	char *gwinfo;
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == nil) StPerror("no graph installed in this window");
  return(gr);
}

StGrSetContentRect(gwinfo, left, top, width, height)
     char *gwinfo;
     int left, top, width, height;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->content.left = left; gr->content.top = top;
  gr->content.width = width; gr->content.height = height;
}

StGrGetContentRect(gwinfo, left, top, width, height)
     char *gwinfo;
     int *left, *top, *width, *height;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (left != nil) *left = gr->content.left;
  if (top != nil) *top = gr->content.top;
  if (width != nil) *width = gr->content.width;
  if (height != nil) *height = gr->content.height;
}

StGrSetContentOrigin(gwinfo, x, y)
     char *gwinfo;
     int x, y;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->content.origin_x = x;
  gr->content.origin_y = y;
}

StGrGetContentOrigin(gwinfo, x, y)
     char *gwinfo;
     int *x, *y;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (x != nil) *x = gr->content.origin_x;
  if (y != nil) *y = gr->content.origin_y;
}

StGrSetContentVariables(gwinfo, x, y)
     char *gwinfo;
     int x, y;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  int vars = StGrNumVariables(gwinfo);
  
  gr->content.x_variable = (vars > x && x >= 0) ? x : 0;
  gr->content.y_variable = (vars > y && y >= 0) ? y : 1;
}

StGrGetContentVariables(gwinfo, x, y)
     char *gwinfo;
     int *x, *y;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (x != nil) *x = gr->content.x_variable;
  if (y != nil) *y = gr->content.y_variable;
}

StGrSetClickRange(gwinfo, width, height)
     char *gwinfo;
     int width, height;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->clickrange.width = width;
  gr->clickrange.height = height;
}

StGrGetClickRange(gwinfo, width, height)
     char *gwinfo;
     int *width, *height;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (width != nil) *width = gr->clickrange.width;
  if (height != nil) *height = gr->clickrange.height;
}

IViewSetMouseMode(w, mode)
	IVIEW_WINDOW w;
	MouseMode mode;
{
  char *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  if (iview == nil) return;

  if (iview->mouseMode == brushing) IViewEraseBrush(w);
  iview->mouseMode = mode;
  if (iview->mouseMode == brushing) IViewDrawBrush(w);
  switch (mode) {
  case brushing:  StGWSetCursor(gwinfo, BRUSH_CURSOR); break;
  case usermode:  StGWSetCursor(gwinfo, HAND_CURSOR);  break;
  case selecting:
  default:        StGWSetCursor(gwinfo, ARROW_CURSOR);
  }
}

MouseMode IViewMouseMode(w)
	IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) return((MouseMode) 0);

  return(iview->mouseMode);
}

IViewSetShowingLabels(w, show)
	IVIEW_WINDOW w;
	int show;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) return;
  
  IViewUnselectAllPoints(w);
  iview->showingLabels = show;
}

IViewShowingLabels(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);

  return(iview != nil && iview->showingLabels);
}

IViewSetData(w, data)
	IVIEW_WINDOW w;
	char *data;
{
  IView iview = IViewGetIView(w);

  if (iview != nil) iview->data = data;
}

char *StGrData(gwinfo)
	char *gwinfo;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  if (gr->data == nil) StPerror("No data in this IView");
  return(gr->data);
}

char *IViewData(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (iview->data == nil) StPerror("No data in this IView");
  return(iview->data);
}

StGrSetMargin(gwinfo, left, top, right, bottom)
     char *gwinfo;
     int left, top, right, bottom;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  
  gr->margin.left = left;
  gr->margin.top = top;
  gr->margin.right = right;
  gr->margin.bottom = bottom;
}

StGrGetMargin(gwinfo, left, top, right, bottom)
     char *gwinfo;
     int *left, *top, *right, *bottom;
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  
  if (left != nil) *left = gr->margin.left;
  if (top != nil) *top = gr->margin.top;
  if (right != nil) *right = gr->margin.right;
  if (bottom != nil) *bottom = gr->margin.bottom;
}

IViewSetFixedAspect(w, fixed)
     IVIEW_WINDOW w;
     int fixed;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  iview->fixed_aspect = fixed;
}

IViewFixedAspect(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  return(iview->fixed_aspect);
}

IViewSetScale(w, var, scale)
     IVIEW_WINDOW w;
     unsigned var;
     double scale;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (var >= IViewNumVariables(w) || scale <= 0.0) return;
  else iview->scale[var] = scale;
}

double IViewScale(w, var)
     IVIEW_WINDOW w;
     unsigned var;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return(0.0);
  else return(iview->scale[var]);
}

IViewSetShift(w, var, shift)
     IVIEW_WINDOW w;
     unsigned var;
     double shift;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return;
  else iview->shift[var] = shift;
}

double IViewShift(w, var)
     IVIEW_WINDOW w;
     unsigned var;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return(0.0);
  else return(iview->shift[var]);
}

/**************************************************************************/
/**                                                                      **/
/**                            Axis Functions                            **/
/**                                                                      **/
/**************************************************************************/

static set_axis(w, which, showing, labeled, ticks)
	IVIEW_WINDOW w;
	int which, showing, labeled, ticks;
{
  char *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  Axis *axis;
  
  if (iview == nil) StPerror("No IView installed in this window");
   
  switch (which) {
  case 'X': axis = &iview->x_axis; break;
  case 'Y': axis = &iview->y_axis; break;
  }

  axis->showing = showing;
  axis->labeled = labeled;
  axis->ticks = ticks;

  if (axis->showing) {
    axis->height = StGWTextAscent(gwinfo)
                 + StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 2
                 + AXIS_LABEL_GAP + AXIS_TICK_LENGTH;
    if (axis->labeled)
      axis->height += StGWTextAscent(gwinfo) + AXIS_LABEL_GAP;
    axis->edge = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE);
  }
  else {
    axis->height = 0;
    axis->edge = 0;
  }
}  
  
IViewGetAxisMargin(w, left, top, right, bottom)
	IVIEW_WINDOW w;
	int *left, *top, *right, *bottom;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");

  if (left != nil) 
    *left = (iview->x_axis.edge > iview->y_axis.height)
          ? iview->x_axis.edge : iview->y_axis.height;
  if (bottom != nil)
    *bottom = (iview->y_axis.edge > iview->x_axis.height)
            ? iview->y_axis.edge : iview->x_axis.height;
  if (top != nil) *top = iview->y_axis.edge;
  if (right != nil) *right = iview->x_axis.edge;
}

IViewSetXaxis(w, showing, labeled, ticks)
	IVIEW_WINDOW w;
	int showing, labeled, ticks;
{
  set_axis(w, 'X', showing, labeled, ticks);
}

IViewGetXaxis(w, showing, labeled, ticks)
	IVIEW_WINDOW w;
	int *showing, *labeled, *ticks;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (showing != nil) *showing = iview->x_axis.showing;
  if (labeled != nil) *labeled = iview->x_axis.labeled;
  if (ticks != nil) *ticks = iview->x_axis.ticks;
}

IViewSetYaxis(w, showing, labeled, ticks)
	IVIEW_WINDOW w;
	int showing, labeled, ticks;
{
  set_axis(w, 'Y', showing, labeled, ticks);
}

IViewGetYaxis(w, showing, labeled, ticks)
	IVIEW_WINDOW w;
	int *showing, *labeled, *ticks;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  if (showing != nil) *showing = iview->y_axis.showing;
  if (labeled != nil) *labeled = iview->y_axis.labeled;
  if (ticks != nil) *ticks = iview->y_axis.ticks;
}

static draw_tick(w, x, y, value, axis)
	IVIEW_WINDOW w;
	int x, y, axis;
	double value;
{
  char s[100];
  int offset;
  char *gwinfo = IViewWindowWinInfo(w);
  
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  switch (axis) {
  case 'X':
    offset += AXIS_TICK_LENGTH + StGWTextAscent(gwinfo);
    StGWDrawLine(gwinfo, x, y, x, y + AXIS_TICK_LENGTH);
    sprintf(s, "%.3g", value);
    StGWDrawText(gwinfo, s, x, y + offset, 1, 0);
    break;
  case 'Y':
    offset += AXIS_TICK_LENGTH + AXIS_LABEL_GAP;
    StGWDrawLine(gwinfo, x, y, x - AXIS_TICK_LENGTH, y);
    sprintf(s, "%.3g", value);
    StGWDrawTextUp(gwinfo, s, x - offset, y, 1, 0);
    break;
  }
}
  
IViewDrawAxes(w)
	IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  int left, top, width, height, right, bottom, x, y;
  double low, high, value;
  int offset, tick, i;
  char *gwinfo = IViewWindowWinInfo(w);
  
  if (iview == nil) StPerror("No IView installed in this window");
  StGrGetContentVariables(gwinfo, &x, &y);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  right = left + width;
  bottom = top + height;
  
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  if (iview->x_axis.showing) {
    StGWDrawLine(gwinfo, left, bottom + 1, right, bottom + 1);
    IViewGetRange(w, x, &low, &high);
    if (iview->x_axis.ticks >= 2) {
      draw_tick(w, left, bottom + 1, low, 'X'); 
      draw_tick(w, right, bottom + 1, high, 'X');
      for (i = 1; i < iview->x_axis.ticks - 1; i++) {
        tick = left + (((double) i) * width) / (iview->x_axis.ticks - 1);
        value = low + i * (high - low) / (iview->x_axis.ticks - 1);
        draw_tick(w, tick, bottom + 1, value, 'X');
      }
    }
    if (iview->x_axis.labeled) {
      offset += AXIS_TICK_LENGTH + AXIS_LABEL_GAP + 2 * StGWTextAscent(gwinfo);
      StGWDrawText(gwinfo, IViewVariableLabel(w, x),
                           (left + right) / 2, bottom + offset, 1, 0);
    }
  }
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  if (iview->y_axis.showing) {
    StGWDrawLine(gwinfo, left - 1, bottom, left - 1, top);
    IViewGetRange(w, y, &low, &high);
    if (iview->y_axis.ticks >= 2) {
      draw_tick(w, left - 1, bottom, low, 'Y'); 
      draw_tick(w, left - 1, top, high, 'Y');
      for (i = 1; i < iview->y_axis.ticks - 1; i++) {
        tick = bottom - (((double) i) * height) / (iview->y_axis.ticks - 1);
        value = low + i * (high - low) / (iview->y_axis.ticks - 1);
        draw_tick(w, left - 1, tick, value, 'Y');
      }
    }
    if (iview->y_axis.labeled) {
      offset += AXIS_TICK_LENGTH + 2 * AXIS_LABEL_GAP + StGWTextAscent(gwinfo);
      StGWDrawTextUp(gwinfo, IViewVariableLabel(w, y),
                            left - offset, (top + bottom) / 2, 1, 0);
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                           Brush Functions                            **/
/**                                                                      **/
/**************************************************************************/

IViewSetBrush(w, x, y, width, height)
     IVIEW_WINDOW w;
     int x, y, width, height;
{
  IView iview = IViewGetIView(w);
  int showing = iview->brush.showing;
  if (iview == nil) return;

  if (showing) IViewEraseBrush(w);
  iview->brush.left = x - width;
  iview->brush.top = y - height;
  iview->brush.width = width;
  iview->brush.height = height;
  if (showing) IViewDrawBrush(w);
}

IViewGetBrush(w, x, y, width, height)
     IVIEW_WINDOW w;
     int *x, *y, *width, *height;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) return;

  if (x != nil) *x = iview->brush.left + iview->brush.width;
  if (y != nil) *y = iview->brush.top + iview->brush.height;
  if (width != nil) *width = iview->brush.width;
  if (height != nil) *height = iview->brush.height;
}

IViewEraseBrush(w)
     IVIEW_WINDOW w;
{
  char *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  int mode, type;
  
  if (iview != nil && iview->brush.showing) {
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    StGWFrameRect(gwinfo, iview->brush.left, iview->brush.top,
		                  iview->brush.width, iview->brush.height);
    iview->brush.showing = FALSE;
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, type);
  }
}

IViewDrawBrush(w)
     IVIEW_WINDOW w;
{
  char *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  int mode, type;

  if (iview != nil && ! iview->brush.showing) {
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    StGWFrameRect(gwinfo, iview->brush.left, iview->brush.top,
		                  iview->brush.width, iview->brush.height);
    iview->brush.showing = TRUE;
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, type);
  }
}

IViewMoveBrush(w, x, y)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) return;

  IViewEraseBrush(w);
  iview->brush.left = x - iview->brush.width;
  iview->brush.top = y - iview->brush.height;
  IViewDrawBrush(w);
}

/**************************************************************************/
/**                                                                      **/
/**                      Mouse Action Functions                          **/
/**                                                                      **/
/**************************************************************************/

static struct {
  int x, y, left, top, width, height;
} dragRect;

static drag(w, x, y)
     IVIEW_WINDOW w;
     int x, y;
{
  char *gwinfo = IViewWindowWinInfo(w);
  
  if (dragRect.width != 0 && dragRect.height != 0) 
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
  dragRect.width = abs(dragRect.x - x); 
  dragRect.height = abs(dragRect.y - y);
  dragRect.left = (x < dragRect.x) ? x : dragRect.x; 
  dragRect.top = (y < dragRect.y) ? y : dragRect.y; 
  if (dragRect.width != 0 && dragRect.height != 0) 
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
}

IViewStdSelectingMouseAction(w, x, y, type, mods)
     IVIEW_WINDOW w;
     int x, y;
     MouseEventType type;
     MouseClickModifier mods;
{
  int mode, line_type;
  int clickwidth, clickheight;
  char *gwinfo = IViewWindowWinInfo(w);

  if (type == MouseClick) {
    if (mods != ExtendModifier) IViewUnselectAllPoints(w);
    StGrGetClickRange(gwinfo, &clickwidth, &clickheight);
    IViewAdjustPointsInRect(w, x - clickwidth / 2, y - clickheight / 2,
                               clickwidth, clickheight, pointSelected);
    
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    line_type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    dragRect.x = x; dragRect.y = y;
    dragRect.left = x, dragRect.top = y;
    dragRect.width = 0; dragRect.height = 0;
    StGWWhileButtonDown(gwinfo, drag, TRUE);
    if (dragRect.width != 0 && dragRect.height != 0)
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, line_type);

    IViewAdjustPointsInRect(w, dragRect.left, dragRect.top,
				               dragRect.width, dragRect.height, pointSelected);
  }
}

static dragbrush(w, x, y)
     IVIEW_WINDOW w;
     int x, y;
{
  IView iview = IViewGetIView(w);
  
  IViewMoveBrush(w, x, y);
  IViewAdjustPointsInRect(w, iview->brush.left, iview->brush.top,
                             iview->brush.width, iview->brush.height,
                             pointSelected);
}

IViewStdBrushingMouseAction(w, x, y, type, mods)
     IVIEW_WINDOW w;
     int x, y;
     MouseEventType type;
     MouseClickModifier mods;
{
  IView iview = IViewGetIView(w);
  char *gwinfo = IViewWindowWinInfo(w);

  IViewMoveBrush(w, x, y);
  if (type == MouseClick) {
    if (mods != ExtendModifier) IViewUnselectAllPoints(w);
    StGWWhileButtonDown(gwinfo, dragbrush, TRUE);
  }
  else if (type == MouseMove) {
    IViewMoveBrush(w, x, y);
    IViewAdjustPointsInRect(w, iview->brush.left, iview->brush.top,
                               iview->brush.width, iview->brush.height, pointHilited);
  }
}

IViewStdMouseAction(w, x, y, type, mods)
     IVIEW_WINDOW w;
     int x, y;
     MouseEventType type;
     MouseClickModifier mods;
{
  switch (IViewMouseMode(w)) {
  case selecting: IViewStdSelectingMouseAction(w, x, y, type, mods); break;
  case brushing:  IViewStdBrushingMouseAction(w, x, y, type, mods); break;
  }
}

IViewStdUnselectAllPoints(w)
     IVIEW_WINDOW w;
{
  int i, n = IViewNumPoints(w);
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++)
    if ((int) IViewPointState(w, i) > (int) pointNormal 
        && ! IViewPointMasked(w, i)) 
      IViewSetPointState(w, i, pointNormal);
  IViewAdjustScreens(w);
}

IViewEraseSelection(w)
	IVIEW_WINDOW w;
{
  int n = IViewNumPoints(w), i;
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) 
    if (IViewPointState(w, i) == pointSelected)
      IViewSetPointState(w, i, pointInvisible);
  IViewAdjustScreens(w);
}

IViewMaskSelection(w)
	IVIEW_WINDOW w;
{
  int n = IViewNumPoints(w), i;
  
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) 
    if (IViewPointState(w, i) == pointSelected)
      IViewSetPointMask(w, i, TRUE);
  IViewRedrawContent(w);
}

IViewUnmaskAllPoints(w)
	IVIEW_WINDOW w;
{
  int n = IViewNumPoints(w), i;
  
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) IViewSetPointMask(w, i, FALSE);
  IViewRedrawContent(w);
}

IViewShowAllPoints(w)
	IVIEW_WINDOW w;
{
  int n = IViewNumPoints(w), i;
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) IViewSetPointState(w, i, pointNormal);
  IViewAdjustScreens(w);
}

IViewAllPointsShowing(w)
	IVIEW_WINDOW w;
{
  int result = TRUE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && result; i++)
    if (IViewPointState(w, i) == pointInvisible) result = FALSE;
  return(result);
}

IViewAllPointsUnmasked(w)
	IVIEW_WINDOW w;
{
  int result = TRUE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && result; i++)
    if (IViewPointMasked(w, i)) result = FALSE;
  return(result);
}

IViewAnyPointsSelected(w)
	IVIEW_WINDOW w;
{
  int result = FALSE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && ! result; i++)
    if (IViewPointState(w, i) == pointSelected) result = TRUE;
  return(result);
}

/*************************************************************************/
/**                                                                     **/
/**                      IView Linking Functions                        **/
/**                                                                     **/
/*************************************************************************/
#ifdef OLDLINKS
typedef struct IViewEntry{
  IVIEW_WINDOW w;
  struct IViewEntry *next;
} IViewEntry;

static IViewEntry *LinkedList = nil;

IViewInternalIsLinked(w)
     IVIEW_WINDOW w;
{
  register IViewEntry *entry;
  
  for (entry = LinkedList; entry != nil; entry = entry->next) {
    if (entry->w == w)
      return (TRUE);
  }
  return (FALSE);
}

IViewLinkWindow(w)
     IVIEW_WINDOW w;
{
  IViewEntry *entry;
  int i, n = IViewNumPoints(w);
  
#ifdef CHECK_ACTIVE_IVIEW
  if (w == nil || ! IViewWindowIsActive(w) || IViewInternalIsLinked(w)) return;
#else
  if (w == nil || IViewInternalIsLinked(w)) return;
#endif CHECK_ACTIVE_IVIEW  
  entry = (IViewEntry *) StCalloc(1, sizeof(IViewEntry));
  
  entry->w = w;
  entry->next = LinkedList;
  LinkedList = entry;
  IViewSetIsLinked(w, TRUE);
  
  for (i = 0; i < n; i++) IViewMatchPointState(w, i);
  IViewAdjustScreens(w);
}

IViewUnlinkWindow(w)
     IVIEW_WINDOW w;
{
  register IViewEntry	*entry, *entry2;
  
  IViewSetIsLinked(w, FALSE);

  if (LinkedList != nil) {		       /* if list empty, ignore */
    if ((*LinkedList).w == w) {	       /* is it the first element? */
      entry2 = LinkedList;
      LinkedList = LinkedList->next;
    }
    else {
      for (entry = LinkedList; entry != nil; entry = entry2) {
        entry2 = entry->next;
        if (entry2 == nil)
          return;                      /* w not in list! */
        if (entry2->w == w)	{          /* found it */
           entry->next = entry2->next;
           break;
        }
      }
    }
    StFree(entry2);                 /* get rid of entry record */
  }
}

IViewUnlinkAllWindows()
{
  IViewEntry *view_list;
  IVIEW_WINDOW w;
  
  for (view_list = LinkedList; view_list != nil;) {
    w = view_list->w;
    view_list = view_list->next;
    IViewUnlinkWindow(w);
  }
}

IViewMatchPointState(w, p)
     IVIEW_WINDOW w;
     unsigned p;
{
  IViewEntry *list;

  for (list = LinkedList; list != nil; list = list->next) {
    if (w != list->w && IViewPointState(w, p) != IViewPointState(list->w, p)) {
	  IViewSetPointScreenState(list->w, p, IViewPointState(list->w, p));
      IViewDataSetPointState(IViewData(list->w), p, IViewPointState(w, p));
	  IViewAdjustOwnScreenPoint(list->w, p);
	  IViewSetPointScreenState(list->w, p, IViewPointState(list->w, p));
	}
  }
}

IViewAdjustScreens(w)
	IVIEW_WINDOW w;
{
  IViewEntry *list;

  IViewAdjustOwnScreen(w);
  if (IViewIsLinked(w)) {
    for (list = LinkedList; list != nil; list = list->next)
      if (w != list->w) IViewAdjustOwnScreen(list->w);
  }
}

static IViewSetIsLinked(w, linked)
     IVIEW_WINDOW w;
     int linked;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  iview->links = linked;
}

IViewIsLinked(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  return(iview->links != nil);
}

IViewCheckLinks(w) IVIEW_WINDOW w; {}

#else
IViewSetLinks(w, links)
     IVIEW_WINDOW w;
     long links;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  iview->links = links;
}

long IViewGetLinks(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  return(iview->links);
}

IViewIsLinked(w)
     IVIEW_WINDOW w;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) return(FALSE);
  else return(iview->links != nil);
}

static IViewSetIsLinked(w, linked)
     IVIEW_WINDOW w;
     int linked;
{
  IView iview = IViewGetIView(w);
  if (iview == nil) StPerror("No IView installed in this window");
  iview->links = linked;
}
#endif OLDLINKS

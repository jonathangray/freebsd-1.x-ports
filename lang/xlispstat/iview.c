/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xmath.h"
#include "iview.h"

#define LABEL_OFFSET 5

#ifndef TRUE
#define TRUE 1
#endif TRUE
#ifndef FALSE
#define FALSE 0
#endif FALSE

#ifdef USESTRINGS
extern char *IViewDataStringString();
extern double IViewDataStringValue(), IViewDataStringTransformedValue()
#endif /* USESTRINGS */
extern char *IViewDataVariableLabel(), *IViewDataPointLabel();
extern PointState IViewDataPointState(), IViewDataPointScreenState();
extern double IViewDataPointValue(), IViewDataLineValue();
extern double IViewDataPointTransformedValue(), IViewDataLineTransformedValue();
extern double **IViewDataTransformation();
extern double IViewScale(), IViewShift();
extern MouseMode IViewMouseMode();

extern char *IViewData(), *StGrData();
extern char *IViewDataPoints();

extern PointState IViewPointState();

/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/
StGrNumVariables(gwinfo)
     char *gwinfo;
{
  return(IViewDataNumVariables(StGrData(gwinfo)));
}

IViewNumVariables(w)
     IVIEW_WINDOW w;
{
  return(IViewDataNumVariables(IViewData(w)));
}

IViewSetVariableLabel(w, var, s)
     IVIEW_WINDOW w;
     int var;
     char *s;
{
  IViewDataSetVariableLabel(IViewData(w), var, s);
}

char *IViewVariableLabel(w, var)
     IVIEW_WINDOW w;
     int var;
{
  return(IViewDataVariableLabel(IViewData(w), var));
}

IViewSetRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var;
     double low, high;
{
  double scale, shift;

  scale = IViewScale(w, var);
  shift = IViewShift(w, var);
  low = scale * low + shift;
  high = scale * high + shift;
  IViewSetScaledRange(w, var, low, high);
}

IViewGetRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var;
     double *low, *high;
{
  double scale, shift;

  scale = IViewScale(w, var);
  shift = IViewShift(w, var);
  IViewGetScaledRange(w, var, low, high);
  if (scale > 0.0) {
    if (low != nil) *low = (*low - shift) / scale;
    if (high != nil) *high = (*high - shift) / scale;
  }
}

IViewSetScaledRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var;
     double low, high;
{
  IViewDataSetRange(IViewData(w), var, low, high);
}

IViewGetScaledRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var;
     double *low, *high;
{
  IViewDataGetRange(IViewData(w), var, low, high);
}

IViewSetScreenRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var, low, high;
{
  IViewDataSetScreenRange(IViewData(w), var, low, high);
}

IViewGetScreenRange(w, var, low, high)
     IVIEW_WINDOW w;
     int var, *low, *high;
{
  IViewDataGetScreenRange(IViewData(w), var, low, high);
}

IViewSetIdentityTransformation(w)
	IVIEW_WINDOW w;
{
  IViewDataSetIdentityTransformation(IViewData(w));
}

IViewSetTransformation(w, a)
	IVIEW_WINDOW w;
	double **a;
{
  IViewDataSetTransformation(IViewData(w), a);
}

double **IViewTransformation(w)
	IVIEW_WINDOW w;
{
  return(IViewDataTransformation(IViewData(w)));
}

IViewIsTransformed(w)
	IVIEW_WINDOW w;
{
  return(IViewDataIsTransformed(IViewData(w)));
}

IViewApplyTransformation(w, a, inbasis)
	IVIEW_WINDOW w;
	double **a;
	int *inbasis;
{
  IViewDataApplyTransformation(IViewData(w), a, inbasis);
}

double IViewEncodeValue(w, value, var)
	IVIEW_WINDOW w;
	double value;
	int var;
{
  double scale = IViewScale(w, var), shift = IViewShift(w, var);
  if (scale == 1.0 && shift == 0.0) return(value);
  else return(scale * value + shift);
}

double IViewDecodeValue(w, value, var)
	IVIEW_WINDOW w;
	double value;
	int var;
{
  double scale = IViewScale(w, var), shift = IViewShift(w, var);
  return((scale > 0.0) ? (value - shift) / scale : 0.0);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Point Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewAddPoints(w, n)
     IVIEW_WINDOW w;
     int n;
{
  IViewDataAddPoints(IViewData(w), n);
}

IViewClearPoints(w)
     IVIEW_WINDOW w;
{
  IViewDataClearPoints(IViewData(w));
}

IViewNumPoints(w)
     IVIEW_WINDOW w;
{
  return(IViewDataNumPoints(IViewData(w)));
}

IViewSetPointValue(w, var, point, value)
     IVIEW_WINDOW w;
     int var, point;
     double value;
{
  IViewDataSetPointValue(IViewData(w), var, point, IViewEncodeValue(w, value, var));
}

double IViewPointValue(w, var, point)
     IVIEW_WINDOW w;
     int var, point;
{
  return(IViewDecodeValue(w, IViewDataPointValue(IViewData(w), var, point), var));
}

IViewSetPointScaledValue(w, var, point, value)
     IVIEW_WINDOW w;
     int var, point;
     double value;
{
  IViewDataSetPointValue(IViewData(w), var, point, value);
}

double IViewPointScaledValue(w, var, point)
     IVIEW_WINDOW w;
     int var, point;
{
  return(IViewDataPointValue(IViewData(w), var, point));
}

double IViewPointTransformedValue(w, var, point)
     IVIEW_WINDOW w;
     int var, point;
{
  return(IViewDataPointTransformedValue(IViewData(w), var, point));
}

IViewPointScreenValue(w, var, point)
     IVIEW_WINDOW w;
     int var, point;
{
  return(IViewDataPointScreenValue(IViewData(w), var, point));
}

IViewGetScreenPointValues(w, point, x)
	IVIEW_WINDOW w;
	int point, *x;
{
  IViewDataGetScreenPointValues(IViewData(w), point, x);
}

IViewSetPointMask(w, point, masked)
     IVIEW_WINDOW w;
     int point;
     int masked;
{
  IViewDataSetPointMask(IViewData(w), point, masked);
}

IViewPointMasked(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointMasked(IViewData(w),  point));
}

IViewSetPointColor(w, point, color)
     IVIEW_WINDOW w;
     int point;
     int color;
{
  IViewDataSetPointColor(IViewData(w), point, color);
}

IViewPointColor(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointColor(IViewData(w),  point));
}

IViewSetPointState(w, point, state)
     IVIEW_WINDOW w;
     int point;
     PointState state;
{
  if (IViewPointState(w, point) != state) {
    IViewSetPointScreenState(w, point, IViewPointState(w, point));
    IViewDataSetPointState(IViewData(w), point, state);
    IViewAdjustOwnScreenPoint(w, point);
    IViewSetPointScreenState(w, point, IViewPointState(w, point));
    if (IViewIsLinked(w)) IViewMatchPointState(w, point);
  }
}

PointState IViewPointState(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointState(IViewData(w),  point));
}

IViewSetPointScreenState(w, point, state)
     IVIEW_WINDOW w;
     int point;
     PointState state;
{
  IViewDataSetPointScreenState(IViewData(w), point, state);
}

PointState IViewPointScreenState(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointScreenState(IViewData(w),  point));
}

IViewResetScreenStates(w)
     IVIEW_WINDOW w;
{
  IViewDataResetScreenStates(IViewData(w));
}

IViewSetPointMark(w, point, marked)
     IVIEW_WINDOW w;
     int point;
     int marked;
{
  IViewDataSetPointMark(IViewData(w), point, marked);
}

IViewPointMarked(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointMarked(IViewData(w),  point));
}

IViewClearPointMarks(w)
     IVIEW_WINDOW w;
{
  IViewDataClearPointMarks(IViewData(w));
}

IViewSetPointLabel(w, point, s)
     IVIEW_WINDOW w;
     int point;
     char *s;
{
  IViewDataSetPointLabel(IViewData(w), point, s);
}

char *IViewPointLabel(w, point)
     IVIEW_WINDOW w;
     int point;
{
  return(IViewDataPointLabel(IViewData(w), point));
}

IViewSetPointSymbol(w, point, sym, hsym)
     IVIEW_WINDOW w;
     int point, sym, hsym;
{
  IViewDataSetPointSymbol(IViewData(w),  point, sym, hsym);
}

IViewGetPointSymbol(w, point, sym, hsym)
     IVIEW_WINDOW w;
     int point, *sym, *hsym;
{
  IViewDataGetPointSymbol(IViewData(w),  point, sym, hsym);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

IViewNumLines(w)
     IVIEW_WINDOW w;
{
  return(IViewDataNumLines(IViewData(w)));
}

IViewAddLines(w, n)
     IVIEW_WINDOW w;
     int n;
{
  IViewDataAddLines(IViewData(w), n);
}

IViewClearLines(w)
     IVIEW_WINDOW w;
{
  IViewDataClearLines(IViewData(w));
}

IViewSetLineValue(w, var, line, value)
	IVIEW_WINDOW w;
	int var, line;
	double value;
{
  IViewDataSetLineValue(IViewData(w), var, line, IViewEncodeValue(w, value, var));
}

double IViewLineValue(w, var, line)
	IVIEW_WINDOW w;
	int var, line;
{
  return(IViewDecodeValue(w, IViewDataLineValue(IViewData(w), var, line), var));
}

IViewSetLineScaledValue(w, var, line, value)
	IVIEW_WINDOW w;
	int var, line;
	double value;
{
  IViewDataSetLineValue(IViewData(w), var, line, value);
}

double IViewLineScaledValue(w, var, line)
	IVIEW_WINDOW w;
	int var, line;
{
  return(IViewDataLineValue(IViewData(w), var, line));
}

double IViewLineTransformedValue(w, var, line)
	IVIEW_WINDOW w;
	int var, line;
{
  return(IViewDataLineTransformedValue(IViewData(w), var, line));
}

IViewLineScreenValue(w, var, line)
	IVIEW_WINDOW w;
	int var, line;
{
  return(IViewDataLineScreenValue(IViewData(w), var, line));
}

IViewSetLineMask(w, line, masked)
     IVIEW_WINDOW w;
     int line;
     int masked;
{
  IViewDataSetLineMask(IViewData(w), line, masked);
}

IViewLineMasked(w, line)
     IVIEW_WINDOW w;
     int line;
{
  return(IViewDataLineMasked(IViewData(w),  line));
}

IViewSetLineColor(w, line, color)
     IVIEW_WINDOW w;
     int line;
     int color;
{
  IViewDataSetLineColor(IViewData(w), line, color);
}

IViewLineColor(w, line)
     IVIEW_WINDOW w;
     int line;
{
  return(IViewDataLineColor(IViewData(w),  line));
}

IViewSetNextLine(w, line, next)
     IVIEW_WINDOW w;
     int line;
     int next;
{
  IViewDataSetNextLine(IViewData(w), line, next);
}

IViewNextLine(w, line)
	IVIEW_WINDOW w;
	int line;
{
  return(IViewDataNextLine(IViewData(w), line));
}

IViewSetLineType(w, line, type)
     IVIEW_WINDOW w;
     int line;
     int type;
{
  IViewDataSetLineType(IViewData(w), line, type);
}

IViewLineType(w, line)
	IVIEW_WINDOW w;
	int line;
{
  return(IViewDataLineType(IViewData(w), line));
}

IViewSetLineWidth(w, line, width)
     IVIEW_WINDOW w;
     int line, width;
{
  IViewDataSetLineWidth(IViewData(w), line, width);
}

IViewGetLineWidth(w, line, width)
	IVIEW_WINDOW w;
	int line, *width;
{
  IViewDataGetLineWidth(IViewData(w), line, width);
}

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                     IView String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewNumStrings(w)
     IVIEW_WINDOW w;
{
  return(IViewDataNumStrings(IViewData(w)));
}

IViewAddStrings(w, n)
     IVIEW_WINDOW w;
     int n;
{
  IViewDataAddStrings(IViewData(w), n);
}

IViewClearStrings(w)
     IVIEW_WINDOW w;
{
  IViewDataClearStrings(IViewData(w));
}

IViewSetStringValue(w, var, string, value)
	IVIEW_WINDOW w;
	int var, string;
	double value;
{
  IViewDataSetStringValue(IViewData(w), var, string, IViewEncodeValue(w, value, var));
}

double IViewStringValue(w, var, string)
	IVIEW_WINDOW w;
	int var, string;
{
  return(IViewDecodeValue(w, IViewDataStringValue(IViewData(w), var, string), var));
}

IViewSetStringScaledValue(w, var, string, value)
	IVIEW_WINDOW w;
	int var, string;
	double value;
{
  IViewDataSetStringValue(IViewData(w), var, string, value);
}

double IViewStringScaledValue(w, var, string)
	IVIEW_WINDOW w;
	int var, string;
{
  return(IViewDataStringValue(IViewData(w), var, string));
}

double IViewStringTransformedValue(w, var, string)
	IVIEW_WINDOW w;
	int var, string;
{
  return(IViewDataStringTransformedValue(IViewData(w), var, string));
}

IViewStringScreenValue(w, var, string)
	IVIEW_WINDOW w;
	int var, string;
{
  return(IViewDataStringScreenValue(IViewData(w), var, string));
}

IViewSetStringMask(w, string, masked)
     IVIEW_WINDOW w;
     int string;
     int masked;
{
  IViewDataSetStringMask(IViewData(w), string, masked);
}

IViewStringMasked(w, string)
     IVIEW_WINDOW w;
     int string;
{
  return(IViewDataStringMasked(IViewData(w),  string));
}

IViewSetStringColor(w, string, color)
     IVIEW_WINDOW w;
     int string;
     int color;
{
  IViewDataSetStringColor(IViewData(w), string, color);
}

IViewStringColor(w, string)
     IVIEW_WINDOW w;
     int string;
{
  return(IViewDataStringColor(IViewData(w),  string));
}

IViewSetStringString(w, string, str)
	IVIEW_WINDOW w;
	int string;
	char *str;
{
  IViewDataSetStringString(IViewData(w), string, str);
}

char *IViewStringString(w, string)
	IVIEW_WINDOW w;
	int string;
{
  return(IViewDataStringString(IViewData(w), string));
}

IViewSetStringModifiers(w, string, up, h, v)
	IVIEW_WINDOW w;
	int string, up, h, v;
{
  IViewDataSetStringModifiers(IViewData(w), string, up, h, v);
}

IViewGetStringModifiers(w, string, up, h, v)
	IVIEW_WINDOW w;
	int string, *up, *h, *v;
{
  IViewDataGetStringModifiers(IViewData(w), string, up, h, v);
}
#endif /* USESTRINGS */
	
/**************************************************************************/
/**                                                                      **/
/**                     IView Data Drawing Functions                     **/
/**                                                                      **/
/**************************************************************************/

IViewDrawDataPoints(w, var1, var2, m, n)
     IVIEW_WINDOW w;
     unsigned var1, var2, m, n;
{
  IViewDataDrawPoints(IViewData(w), w, var1, var2, m, n, LABEL_OFFSET);
}

static IViewDrawDataLine(w, var1, var2, line,
                         left, top, width, height, orig_x, orig_y,
                         use_color, draw_color)
     IVIEW_WINDOW w;
     unsigned var1, var2, line;
     int left, top, width, height, orig_x, orig_y, use_color, draw_color;
{
  int n = IViewNumLines(w);
  int x, y, nx, ny;
  int next;
/*  int right = left + width, bottom = top + height;*/
  int type, color, linewidth;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (line >= n || IViewLineMasked(w, line)) return;
  x = orig_x + IViewLineScreenValue(w, var1, line);
  y = orig_y - IViewLineScreenValue(w, var2, line);
/*  if (x < left || x > right) return;
  if (y < top || y > bottom) return;*/

  next = IViewNextLine(w, line);
  if (next >= n || next < 0 || IViewLineMasked(w, next)) return;
  nx = orig_x + IViewLineScreenValue(w, var1, next);
  ny = orig_y - IViewLineScreenValue(w, var2, next);
/*  if (nx < left || nx > right) return;
  if (ny < top || ny > bottom) return;*/
  
  IViewGetLineWidth(w, line, &linewidth);
  StGWSetLineWidth(gwinfo, linewidth);
  type = IViewLineType(w, line);
  if (use_color) {
    color = IViewLineColor(w, line);
    if (color >= 0) StGWSetDrawColor(gwinfo, color);
    else StGWSetDrawColor(gwinfo, draw_color);
  }
  StGWSetLineType(gwinfo, type);
  StGWDrawLine(gwinfo, x, y, nx, ny);
}

IViewDrawDataLines(w, var1, var2, m, n)
	IVIEW_WINDOW w;
	unsigned var1, var2, m, n;
{
  int vars = IViewNumVariables(w);
  int i, left, top, width, height, x, y, use_color;
  int line_type, line_width, draw_color;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (var1 >= vars || var2 >= vars) return;
  if (n > IViewNumLines(w)) return;
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  use_color = StGWUseColor(gwinfo);
  line_type = StGWLineType(gwinfo);
  StGWGetLineWidth(gwinfo, &line_width);
  if (use_color) draw_color = StGWDrawColor(gwinfo);

  for (i = m; i < n; i++)
    IViewDrawDataLine(w, var1, var2, i, left, top, width, height, x, y,
                      use_color, draw_color);

  StGWSetLineType(gwinfo, line_type);
  StGWSetLineWidth(gwinfo, line_width);
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}

#ifdef USESTRINGS
static IViewDrawDataString(w, var1, var2,  string,
                         left, top, width, height, orig_x, orig_y,
                         use_color, draw_color)
     IVIEW_WINDOW w;
     unsigned var1, var2, string;
     int left, top, width, height, orig_x, orig_y;
     int use_color, draw_color;
{
  int n = IViewNumStrings(w);
  int x, y;
/*  int right = left + width, bottom = top + height; not needed JKL */
  int up, h, v;
  int color;
  char *s;
  char *gwinfo = IViewWindowWinInfo(w);

  if (string >= n || IViewStringMasked(w, string)) return;
  x = orig_x + IViewStringScreenValue(w, var1, string);
  y = orig_y - IViewStringScreenValue(w, var2, string);
/*  if (x < left || x > right) return;
  if (y < top || y > bottom) return;*/
  
  if (use_color) {
    color = IViewStringColor(w, string);
    if (color >= 0) StGWSetDrawColor(gwinfo, color);
    else StGWSetDrawColor(gwinfo, draw_color);
  }
  IViewGetStringModifiers(w, string, &up, &h, &v);
  s = IViewStringString(w, string);
  if (s != nil) {
    if (up) StGWDrawTextUp(gwinfo, s, x, y, h, v);
    else StGWDrawText(gwinfo, s, x, y, h, v);
  }
}

IViewDrawDataStrings(w, var1, var2, m, n)
	IVIEW_WINDOW w;
	unsigned var1, var2, m, n;
{
  int vars = IViewNumVariables(w);
  int i, left, top, width, height, x, y, use_color, draw_color;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (var1 >= vars || var2 >= vars) return;
  if (n > IViewNumStrings(w)) return;
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  use_color = StGWUseColor(gwinfo);
  if (use_color) draw_color = StGWDrawColor(gwinfo);
  for (i = m; i < n; i++)
    IViewDrawDataString(w, var1, var2, i, left, top,
                        width, height, x, y, use_color, draw_color);
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}
#endif /* USESTRINGS */

IViewDepthCuePoints(w, var, cut1, cut2, cut3, m, n)
     IVIEW_WINDOW w;
     unsigned var, cut1, cut2, cut3, m, n;
{
  IViewDataCuePoints(IViewData(w), var, cut1, cut2, cut3, m, n);
}

/**************************************************************************/
/**                                                                      **/
/**                     Standard Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewStdResize(w)
	IVIEW_WINDOW w;
{
  int left, top, width, height, x, y, size;
  int m_left, m_top, m_right, m_bottom;
  int i , vars = IViewNumVariables(w);
  char *gwinfo = IViewWindowWinInfo(w);
  
  width = StGWCanvasWidth(gwinfo);
  height = StGWCanvasHeight(gwinfo);
  StGrGetMargin(gwinfo, &m_left, &m_top, &m_right, &m_bottom);
  left = m_left;
  top = m_top;
  width -= m_left + m_right;
  height -= m_top + m_bottom;
  IViewGetAxisMargin(w, &m_left, &m_top, &m_right, &m_bottom);
  left += m_left;
  top += m_top;
  width -= m_left + m_right;
  height -= m_top + m_bottom;
  if (IViewFixedAspect(w)) {
    size = (width > height) ? height : width;
    left += (width - size) / 2;
    top += (height - size) / 2;
    StGrSetContentRect(gwinfo, left, top, size, size);
    StGrSetContentOrigin(gwinfo, left, top + size);
    for (i = 0; i < vars; i++) IViewSetScreenRange(w, i, 0, size);
  }
  else {
    StGrSetContentRect(gwinfo, left, top, width, height);
    StGrSetContentOrigin(gwinfo, left, top + height);
    StGrGetContentVariables(gwinfo, &x, &y);
    IViewSetScreenRange(w, x, 0, width);
    IViewSetScreenRange(w, y, 0, height);
    for (i = 0; i < vars; i++) 
      if (i != x && i != y)
	IViewSetScreenRange(w, i, 0, width);
  }
  IViewResizeOverlays(w);
}

IViewStdRedraw(w)
	IVIEW_WINDOW w;
{
  int left, top, height, width;
  char *gwinfo = IViewWindowWinInfo(w);
  
  StGWStartBuffering(gwinfo);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);  
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);
  IViewRedrawBackground(w);
  IViewRedrawOverlays(w);
  IViewRedrawContent(w);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  StGrSetDirty(gwinfo, FALSE);
}

IViewStdRedrawBackground(w)
	IVIEW_WINDOW w;
{
  int left, top, height, width;
  char *gwinfo = IViewWindowWinInfo(w);
  
  StGWStartBuffering(gwinfo);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);  
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);
  StGWEraseRect(gwinfo, left, top, width, height);
  IViewDrawAxes(w);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
}

IViewGetContentMarginRect(w, left, top, width, height)
	IVIEW_WINDOW w;
	int *left, *top, *width, *height;
{
  int cleft, ctop, cwidth, cheight, mleft, mtop, mright, mbottom;
  int x_showing, y_showing;
  char *gwinfo = IViewWindowWinInfo(w);
  
  StGrGetContentRect(gwinfo, &cleft, &ctop, &cwidth, &cheight);
#ifdef DODO
  StGrGetMargin(gwinfo, &mleft, &mtop, &mright, &mbottom);
#endif /* DODO */
  IViewGetAxisMargin(w, &mleft, &mtop, &mright, &mbottom);
  IViewGetXaxis(w, &x_showing, nil, nil);
  IViewGetYaxis(w, &y_showing, nil, nil);
  if (y_showing || x_showing) {
    cwidth += mright;
    ctop -= mtop; cheight += mtop;
    if (! y_showing) { cleft -= mleft; cwidth += mleft; }
    if (! x_showing) cheight += mbottom;
  }
  if (left != nil) *left = cleft;
  if (top != nil) *top = ctop;
  if (width != nil) *width = cwidth;
  if (height != nil) *height = cheight;
}

IViewStdRedrawContent(w)
	IVIEW_WINDOW w;
{
  int left, top, width, height, vleft, vtop, vwidth, vheight;
  int x, y;
  char *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);

  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  IViewGetContentMarginRect(w, &left, &top, &width, &height);
  StGrGetContentVariables(gwinfo, &x, &y);
  
  StGWStartBuffering(gwinfo);
  StGWSetClipRect(gwinfo, TRUE, left, top, width + 1, height + 1);
  StGWEraseRect(gwinfo, left, top, width + 1, height + 1);
  IViewDrawDataPoints(w, x, y, 0, IViewNumPoints(w));
  IViewDrawDataLines(w, x, y, 0, IViewNumLines(w));
#ifdef USESTRINGS
  IViewDrawDataStrings(w, x, y, 0, IViewNumStrings(w));
#endif /* USESTRINGS */
  StGWBufferToScreen(gwinfo, left, top, width + 1, height + 1);
  StGWGetViewRect(gwinfo, &vleft, &vtop, &vwidth, &vheight);
  StGWSetClipRect(gwinfo, TRUE, vleft, vtop, vwidth, vheight);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  IViewResetScreenStates(w);
}

IViewStdMarkPointsInRect(w, left, top, width, height)
     IVIEW_WINDOW w;
     int left, top, width, height;
{
  int c_left, c_top, c_width, c_height, x, y, center_x, center_y;
  int i, n = IViewNumPoints(w), vars = IViewNumVariables(w);
  unsigned var1, var2;
  char *gwinfo = IViewWindowWinInfo(w);
  
  StGrGetContentRect(gwinfo, &c_left, &c_top, &c_width, &c_height);
  StGrGetContentOrigin(gwinfo, &center_x, &center_y);
  StGrGetContentVariables(gwinfo, &var1, &var2);
  
  if (var1 >= vars || var2 >= vars) return;
  
  for (i = 0; i < n; i++) {
    x = center_x + IViewPointScreenValue(w, var1, i);
    y = center_y - IViewPointScreenValue(w, var2, i);
    if ((x >= left && x <= left + width && y >= top && y <= top + height)
        && (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible))
      IViewSetPointMark(w, i, TRUE);
    else IViewSetPointMark(w, i, FALSE);
  }
}

static DrawPoint(w, i, state, var1, var2, left, top, width, height, 
                 center_x, center_y, use_color)
	IVIEW_WINDOW w;
	int i;
	PointState state;
	int var1, var2, left, top, width, height, center_x, center_y;
	int use_color;
{
  int x, y, sym, hsym;
  int color, oldcolor;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (use_color) {
    oldcolor = StGWDrawColor(gwinfo);
    color = IViewPointColor(w, i);
    if (color >= 0) StGWSetDrawColor(gwinfo, color);
  }
  x = center_x + IViewPointScreenValue(w, var1, i);
  y = center_y - IViewPointScreenValue(w, var2, i);
  IViewGetPointSymbol(w, i, &sym, &hsym);
  if (state == pointNormal) StGWReplaceSymbol(gwinfo, hsym, sym, x, y);
  else StGWReplaceSymbol(gwinfo, sym, hsym, x, y);
  if (use_color && color >= 0) StGWSetDrawColor(gwinfo, oldcolor);
}

static DrawLabel(w, i, var1, var2, left, top, width, height, 
                 center_x, center_y)
	IVIEW_WINDOW w;
	int i;
	int var1, var2, left, top, width, height, center_x, center_y;
{
  char *gwinfo = IViewWindowWinInfo(w);
  int x, y, mode = StGWDrawMode(gwinfo);

  StGWSetDrawMode(gwinfo, 1);
  x = center_x + IViewPointScreenValue(w, var1, i);
  y = center_y - IViewPointScreenValue(w, var2, i);
  StGWDrawString(gwinfo, IViewPointLabel(w, i), 
                           x + LABEL_OFFSET, y - LABEL_OFFSET);
  StGWSetDrawMode(gwinfo, mode);
}

IViewStdAdjustScreen(w)
	IVIEW_WINDOW w;
{
  char *gwinfo = IViewWindowWinInfo(w);
  if (StGrDirty(gwinfo)) IViewRedrawContent(w);
  StGrSetDirty(gwinfo, FALSE);
  IViewResetScreenStates(w);
}

IViewStdAdjustPointsInRect(w, left, top, width, height, state)
	IVIEW_WINDOW w;
	int left, top, width, height;
	PointState state;
{
  int i, n = IViewNumPoints(w);
  PointState point_state;
  int masked, in_rect;
  
  IViewCheckLinks(w);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  
  IViewMarkPointsInRect(w, left, top, width, height);
  for (i = 0; i < n; i++) {
    masked = IViewPointMasked(w, i);
    point_state = IViewPointState(w, i);
    if (! masked && point_state != pointInvisible) {
      in_rect = IViewPointMarked(w, i);
      if (in_rect && (int) point_state < (int) state) {
        IViewSetPointState(w, i, state);
      }
      else if (! in_rect 
	       && state == pointHilited && point_state == pointHilited) {
        IViewSetPointState(w, i, pointNormal);
      }
    }
  }
  IViewAdjustScreens(w);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
}


IViewStdAdjustScreenPoint(w, i)
	IVIEW_WINDOW w;
	int i;
{
  unsigned var1, var2;
  int left, top, width, height, x, y;
  PointState point_state, screen_state;
  int masked, showingLabels = IViewShowingLabels(w);
  char *gwinfo = IViewWindowWinInfo(w);
  int use_color = StGWUseColor(gwinfo);
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  StGrGetContentVariables(gwinfo, &var1, &var2);
  
  masked = IViewPointMasked(w, i);
  point_state = IViewPointState(w, i);
  screen_state = IViewPointScreenState(w, i);
  if (! masked && point_state != screen_state) {
    IViewSetPointScreenState(w, i, point_state);
	if (point_state == pointInvisible || screen_state == pointInvisible) {
	  StGrSetDirty(gwinfo, TRUE);
	}
    else if ((int) point_state > (int) screen_state) { 
      if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
      DrawPoint(w, i, point_state, var1, var2, left, top, width, height, x, y, use_color);
      if (showingLabels)
        DrawLabel(w, i, var1, var2, left, top, width, height, x, y); /* to draw */
      if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
    }
    else {
      if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
      if (showingLabels)
        DrawLabel(w, i, var1, var2, left, top, width, height, x, y); /* to erase */
      DrawPoint(w, i, point_state, var1, var2, left, top, width, height, x, y, use_color);
      if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       IView Rotation Functions                       **/
/**                                                                      **/
/**************************************************************************/

IViewRotate2(w, var1, var2, newalpha)
	IVIEW_WINDOW w;
	unsigned var1, var2;
	double newalpha;
{
  static int *inbasis;
  static double **a;
  static int maxvars = 0;
  static double alpha = 0.0, s = 0.0, c = 1.0;
  int i, j, vars = IViewNumVariables(w);
  
  if (var1 >= vars || var2 >= vars) return;
  
  if (vars > maxvars) {
    maxvars = vars;
    if (a != nil) StFree(a[0]);
    StFree(a);
    StFree(inbasis);
    a = (double **) StCalloc(sizeof(double *), maxvars);
    a[0] = (double *) StCalloc(sizeof(double), maxvars * maxvars);
    for (i = 1; i < vars; i++) a[i] = a[0] + i * maxvars;
    inbasis = (int *) StCalloc(sizeof(int), maxvars);
  }
  
  if (alpha != newalpha) {
    alpha = newalpha;
    s = sin(alpha);
    c = cos(alpha);
  }
  
  for (i = 0; i < vars; i++) {
    inbasis[i] = FALSE;
    for (j = 0; j < vars; j++) a[i][j] = (i == j) ? 1.0 : 0.0;
  }
  a[var1][var1] =  c; a[var1][var2] = -s;
  a[var2][var1] =  s; a[var2][var2] =  c;
  inbasis[var1] = TRUE; inbasis[var2] = TRUE;

  IViewApplyTransformation(w, a, inbasis);
}
  
/**************************************************************************/
/**                                                                      **/
/**                        Miscellaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

static double NiceValue(x)
     double x;
{
  long ilx;
  double lx, v1, v2, v3, v4;
  
  if (x <= 0.0) return (0.0);
  else {
    lx = log(x) / log(10.0);
    ilx = floor(lx);
    v1 = pow(10.0, (float) ilx);
    v2 = pow(10.0, (float) ilx) * 2.0;
    v3 = pow(10.0, (float) ilx) * 5.0;
    v4 = pow(10.0, (float) ilx + 1);
    
    if ((fabs(x - v1) < fabs(x - v2))
	&& (fabs(x - v1) < fabs(x - v3))
	&& (fabs(x - v1) < fabs(x - v4)))
      return(v1);
    else if ((fabs(x - v2) < fabs(x - v3))
	     && (fabs(x - v2) < fabs(x - v4)))
      return(v2);
    else if (fabs(x - v3) < fabs(x - v4))
      return(v3);
    else
      return(v4);
  }
}

GetNiceRange(low, high, ticks)
     double *low, *high;
     int *ticks;
{
  double delta;
  
  if (fabs(*high) >= HUGE || fabs(*low) >= HUGE) return;
  if ((*high <= *low) || (*ticks < 2)) return;
  
  delta = NiceValue((*high - *low) / (*ticks - 1));
  if (delta <= 0.0) return;
  
  *low = floor(*low / delta + .01) * delta;   /* adjust by .01 for rounding */
  *high = ceil(*high / delta - .01) * delta;
  
  *ticks = 1 + (.01 + (*high - *low) / delta); /* add .01 for rounding */
}


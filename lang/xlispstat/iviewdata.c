/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "StGWWindow.h"
#include "stmem.h"

typedef char *IVIEW_WINDOW;

typedef char *IViewReallocData;

/* Forward declarations */
double IViewBasicPointValue();

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                      Fixed Point Arithmetic Package                  **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

#define SCALE 65536

#ifdef MACINTOSH

#include "xmath.h"

typedef long Fixed;
typedef struct { short h; unsigned short v; } fixed_struct;
static FixRound(x) fixed_struct x; { return(x.h); }

#ifdef MPWC
#ifdef _MC68881_
#undef _MC68881_
#endif _MC68881_
#endif MPWC

#ifdef DODO_MC68881_
static Fixed FixInnerProduct(vars, x, y, inbasis)
	int vars, *inbasis;
	Fixed *x, *y;
{
  double result = 0.0;
  
  for (; vars-- > 0; x++, y++) 
    if (*inbasis++) result += ((double) *x) * ((double) *y);
    
  return((Fixed) (result / SCALE));
}
#else /* _MC68881_ */
#define INTPART(x)  (((fixed_struct *) (x))->h)
#define FRACPART(x) (((fixed_struct *) (x))->v)

static Fixed FixInnerProduct(vars, x, y, inbasis)
	int vars, *inbasis;
	Fixed *x, *y;
{
  register Fixed result = 0, hx, lx, hy, ly;
  
  for (; vars-- > 0; x++, y++) 
    if (*inbasis++) {
      hx = INTPART(x); lx = FRACPART(x);
      hy = INTPART(y); ly = FRACPART(y);
      if (hx == 0) result += *x * hy + ((*x * (ly >> 2)) >> 14);
      else result += *x * hy + hx * ly + ((lx * (ly >> 2)) >> 14);
    }
    
  return(result);
}
#endif /* _MC68881_ */

#else
typedef long Fixed;

#define FixRound(x) ((int) ((x) / SCALE))

static Fixed FixInnerProduct(vars, x, y, inbasis)
	int vars, *inbasis;
	Fixed *x, *y;
{
  double result = 0.0;
  
  for (; vars-- > 0; x++, y++) 
    if (*inbasis++) result += ((double) *x) * ((double) *y);
    
  return((Fixed) (result / SCALE));
}
#endif MACINTOSH

#define Int2Fixed(x) ((Fixed)((x) * SCALE))
#define Double2Fixed(x) ((Fixed) ((x) * SCALE))
#define Fixed2Double(x) (((double) (x)) / SCALE)

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                         IViewBasicPoints Package                     **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

typedef struct basic_points {
  int num_points, num_variables;
  double *scale, *location, **transformation;
  IViewReallocData *data, *screen_data;
  int recalculateScreenPoints, fixed;
} *IViewBasicPoints;

/**************************************************************************/
/**                                                                      **/
/**            Basic Points Creation and Destruction Functions           **/
/**                                                                      **/
/**************************************************************************/

static IViewBasicPoints IViewNewBasicPoints(m, scale, location)
	int m;
	double *scale, *location;
{
  IViewBasicPoints p;
  
  p = (IViewBasicPoints) StCalloc(sizeof(struct basic_points), 1);
  
  p->num_points = 0;
  p->num_variables = m;
  p->scale = scale;
  p->location = location;
  p->recalculateScreenPoints = FALSE;
  p->transformation = (double **) nil;

  if (m > 0) {
    p->data = (IViewReallocData *) StCalloc(sizeof(IViewReallocData), m);
    p->screen_data = (IViewReallocData *) StCalloc(sizeof(IViewReallocData), m);    
  }

  return(p);
}

static IViewFreeBasicPoints(p)
	IViewBasicPoints p;
{
  int i;
  
  for (i = 0; i < p->num_variables; i++) {
    if (p->data != nil) StRFree(p->data[i]);
    if (p->screen_data != nil) StRFree(p->screen_data[i]);
  }
  
  StFree(p->data);
  StFree(p->screen_data);
  StFree(p);
}

static IViewAddBasicPoints(p, n)
	IViewBasicPoints p;
	int n;
{
  int size, i;

  size = p->num_points + n;
  if (p->num_variables > 0) {
    if (p->data == nil || p->screen_data == nil) return;

    for (i = 0; i < p->num_variables; i++) {
      p->data[i] = StRRealloc(p->data[i], sizeof(double), size);
      p->screen_data[i] = StRRealloc(p->screen_data[i], sizeof(Fixed), size);
    }
  }
  p->num_points += n;
}

static IViewClearBasicPoints(p)
	IViewBasicPoints p;
{
  int i;
  p->num_points = 0;
  if (p->data == nil || p->screen_data == nil) return;

  for (i = 0; i < p->num_variables; i++) {
    StRFree(p->data[i]);
    p->data[i] = nil;
    StRFree(p->screen_data[i]);
    p->screen_data[i] = nil;
  }
}

/**************************************************************************/
/**                                                                      **/
/**       Basic Points Data Set Access and Modificaiton Functions        **/
/**                                                                      **/
/**************************************************************************/

static IViewBasicPointsNumPoints(p)
	IViewBasicPoints p;
{
  return(p->num_points);
}

static IViewBasicPointsNumVariables(p)
	IViewBasicPoints p;
{
  return(p->num_variables);
}

static IViewBasicPointsSetRecalculate(p, fixed)
	IViewBasicPoints p;
	int fixed;
{
  p->recalculateScreenPoints = TRUE;
  p->fixed = fixed;
}

static double TransformedValue(p, var, point)
	IViewBasicPoints p;
	unsigned var, point;
{
  double x, a;
  int i;
  
  if (p->transformation == nil) 
    return(IViewBasicPointValue(p, var, point));
  else {
    for (i = 0, x = 0.0; i < p->num_variables; i++)
      if ((a = p->transformation[var][i]) != 0.0)
        x += IViewBasicPointValue(p, i, point) * a;
    return(x);
  }
}

static SetScreenValue(p, var, point)
	IViewBasicPoints p;
	unsigned var, point;
{
  double x = TransformedValue(p, var, point);
  Fixed screen_x, *screen_data;
  
  if (p->screen_data == nil) return;
  screen_data = (Fixed *) StRPtr(p->screen_data[var]);
  if (screen_data == nil) return;

  if (p->location == nil || p->scale == nil) x = 0.0;
  else x = x * p->scale[var] + p->location[var];
  
  screen_x = Double2Fixed(x);
  screen_data[point] = screen_x;
}

static RecalculateScreenPoints(p)
	IViewBasicPoints p;
{
  int var, point;
  
  for (var = 0; var < p->num_variables; var++)
    for (point = 0; point < p->num_points; point++) 
      SetScreenValue(p, var, point);
  p->recalculateScreenPoints = FALSE;
}

static IViewBasicPointsSetTransformation(p, a)
	IViewBasicPoints p;
	double **a;
{
  p->transformation = a;
  p->recalculateScreenPoints = TRUE;
}  

static IViewBasicPointsApplyTransformation(p, a, inbasis)
	IViewBasicPoints p;
	double **a;
	int *inbasis;
{
  static maxvars = 0;
  static Fixed **b, *x, **screen_data, *screen_location;
  int vars, n, i, j;
  double *scale = p->scale, *location = p->location;
  if (p->screen_data == nil) return;

  vars = p->num_variables;
  n = p->num_points;

  /* allocate local working storage */
  if (maxvars < vars) {
    maxvars = vars;
    if (b != nil) StFree(b[0]);
    StFree(b);
    StFree(x);
    StFree(screen_data);
    StFree(screen_location);
    b = (Fixed **) StCalloc(sizeof(Fixed *), maxvars);
    b[0] = (Fixed *) StCalloc(sizeof(Fixed), maxvars * maxvars);
    for (i = 1; i < maxvars; i++) b[i] = b[0] + i * maxvars;
    x = (Fixed *) StCalloc(sizeof(Fixed), maxvars);
    screen_data = (Fixed **) StCalloc(sizeof(Fixed *), maxvars);
    screen_location = (Fixed *) StCalloc(sizeof(Fixed), maxvars);
  }
  
  /* convert transformation to fixed point format*/ 
  if (p->fixed) {
    for (i = 0; i < vars; i++)
      if (inbasis[i])
        for (j = 0; j < vars; j++)
          if (inbasis[j]) b[i][j] = Double2Fixed(a[i][j]);
  }
  else {
    for (i = 0; i < vars; i++)
      if (inbasis[i]) {
        for (j = 0; j < vars; j++)
          if (inbasis[j]) 
            b[i][j] = Double2Fixed(scale[i] * a[i][j] / scale[j]);
        screen_location[i] = Double2Fixed(location[i]);
      }
  }
  
  /* set up array with screen coordinates */
  for (i = 0; i < vars; i++)
    screen_data[i] = (Fixed *) StRPtr(p->screen_data[i]);
  if (! p->fixed)
    for (i = 0; i < vars; i++) 
      if (inbasis[i]) 
        for (j = 0; j < n; j++) screen_data[i][j] -= screen_location[i];
    
  /* apply the transformation */
  for (i = 0; i < n; i++) {
    for (j = 0; j < vars; j++) 
      if (inbasis[j]) x[j] = screen_data[j][i];    
    for (j = 0; j < vars; j++) 
      if (inbasis[j]) {
        screen_data[j][i] = FixInnerProduct(vars, b[j], x, inbasis);
      }
  }

  /* adjust origins if scaling is not fixed */
  if (! p->fixed)
    for (i = 0; i < vars; i++) 
      if (inbasis[i]) 
        for (j = 0; j < n; j++) screen_data[i][j] += screen_location[i];
}

/**************************************************************************/
/**                                                                      **/
/**         Basic Points Point Access and Modification Functions         **/
/**                                                                      **/
/**************************************************************************/

static IViewBasicPointSetValue(p, var, point, value)
	IViewBasicPoints p;
	unsigned var, point;
	double value;
{
  double *x;
  
  if (var < p->num_variables && point < p->num_points && p->data != nil) {
    x = (double *) StRPtr(p->data[var]);
    if (x != nil) x[point] = value;
  }
  if (p->transformation != nil) p->recalculateScreenPoints = TRUE;
  if (! p->recalculateScreenPoints) SetScreenValue(p, var, point);
}

static double IViewBasicPointValue(p, var, point)
	IViewBasicPoints p;
	unsigned var, point;
{
  double *x, value;
  
  if (var < p->num_variables  && point < p->num_points && p->data != nil) {
    x = (double *) StRPtr(p->data[var]);
    value = (x != nil) ? x[point] : 0.0;
    return(value);
  }
  else StPerror("index out of range");
}

static IViewBasicPointScreenValue(p, var, point)
	IViewBasicPoints p;
	unsigned var, point;
{
  Fixed *screen_data;
  
  if (p->recalculateScreenPoints) RecalculateScreenPoints(p);
  
  if (var < p->num_variables && point < p->num_points 
      && p->screen_data != nil) {
    screen_data = (Fixed *) StRPtr(p->screen_data[var]);
#ifdef PERSPECTIVE
    return(PerspScreenValue(p, var, point));
#else
    return(FixRound(screen_data[point]));
#endif PERSPECTIVE
  }
  else StPerror("index out of range");
}

static IViewBasicPointsGetScreenValues(p, point, x)
	IViewBasicPoints p;
	unsigned point;
	int *x;
{
  int i, n, vars;
  Fixed *screen_data;
  
  n = p->num_points;
  vars = p->num_variables;

  if (p->recalculateScreenPoints) RecalculateScreenPoints(p);
  if (p->screen_data == nil || point >= n) return;
  
  for (i = 0; i < vars; i++){
    screen_data = (Fixed *) StRPtr(p->screen_data[i]);
    if (screen_data != nil) x[i] = FixRound(screen_data[point]);
  }
}

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                            IViewData Package                         **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

typedef char color_index;

typedef struct point_symbol {
  int normal, highlighted;
} PointSymbol;

typedef struct point_info {
  PointState state, screen_state;
  char marked, masked;
  color_index color;
  PointSymbol symbol;
  char *label;
} PointInfo;

typedef struct line_info {
  int next, type;
  char masked, width;
  color_index color;
} LineInfo;

#ifdef USESTRINGS
typedef struct string_info {
  char *string;
  char masked, up, h, v;
  color_index color;
} StringInfo;
#endif /* USESTRINGS */

typedef struct iview_data {
#ifdef USESTRINGS
  IViewBasicPoints points, lines, strings;
  IViewReallocData pointInfo, lineInfo, stringInfo;
#else
  IViewBasicPoints points, lines;
  IViewReallocData pointInfo, lineInfo;
#endif /* USESTRINGS */
  double *mins, *maxes, *scale, *location;
  int *screen_mins, *screen_maxes;
  char **variableLabels;
  int recalculateScreenPoints, transformed;
  double **transformation;
} *IViewData;

/**************************************************************************/
/**                                                                      **/
/**          IView Data Construction and Destruction Functions           **/
/**                                                                      **/
/**************************************************************************/

char *IViewDataNew(vars)
	int vars;
{
  IViewData p;
  
  p = (IViewData) StCalloc(sizeof(struct iview_data), 1);
  if (vars > 0) {
    p->mins = (double *) StCalloc(sizeof(double), vars);
    p->maxes = (double *) StCalloc(sizeof(double), vars);
    p->scale = (double *) StCalloc(sizeof(double), vars);
    p->location = (double *) StCalloc(sizeof(double), vars);
    p->screen_mins = (int *) StCalloc(sizeof(int), vars);
    p->screen_maxes = (int *) StCalloc(sizeof(int), vars);
  }
  
  p->points = IViewNewBasicPoints(vars, p->scale, p->location);
  p->lines = IViewNewBasicPoints(vars, p->scale, p->location);
#ifdef USESTRINGS
  p->strings = IViewNewBasicPoints(vars, p->scale, p->location);
#endif /* USESTRINGS */
  p->variableLabels = (char **) StCalloc(sizeof(char *), vars);
  p->recalculateScreenPoints = FALSE;
  p->transformed = FALSE;
  
  return((char *) p);
}

IViewDataFree(p)
	IViewData p;
{
  int i;
  PointInfo *pointInfo;
#ifdef USESTRINGS
  StringInfo *stringInfo;
#endif /* USESTRINGS */
  
  StFree(p->mins);
  StFree(p->maxes);
  StFree(p->scale);
  StFree(p->location);
  StFree(p->screen_mins);
  StFree(p->screen_maxes);

  for (i = 0; i < p->points->num_points; i++) {
    pointInfo = (PointInfo *) StRPtr(p->pointInfo);
    if (pointInfo[i].label != nil) StFree(pointInfo[i].label);
  }
  StRFree(p->pointInfo);
  
  StRFree(p->lineInfo);
  
#ifdef USESTRINGS
  for (i = 0; i < p->strings->num_points; i++) {
    stringInfo = (StringInfo *) StRPtr(p->stringInfo);
    if (stringInfo[i].string != nil) StFree(stringInfo[i].string);
  }
  StRFree(p->stringInfo);
#endif /* USESTRINGS */

  for (i = 0; i < p->points->num_variables; i++) {
    if (p->variableLabels[i] != nil) StFree(p->variableLabels[i]);
  }
  StFree(p->variableLabels);
    
  IViewFreeBasicPoints(p->points);
  IViewFreeBasicPoints(p->lines);
#ifdef USESTRINGS
  IViewFreeBasicPoints(p->strings);
#endif /* USESTRINGS */
  
  if (p->transformation != nil) {
    StFree(p->transformation[0]);
    StFree(p->transformation);
  }
  StFree(p);
}

IViewBasicPoints IViewDataPoints(data)
	IViewData data;
{
  if (data == nil) StPerror("nil data pointer");
  if (data->points == nil) StPerror("nil point data pointer");
  return(data->points);
}

IViewBasicPoints IViewDataLines(data)
	IViewData data;
{
  if (data == nil) StPerror("nil data pointer");
  if (data->lines == nil) StPerror("nil line data pointer");
  return(data->lines);
}

#ifdef USESTRINGS
IViewBasicPoints IViewDataStrings(data)
	IViewData data;
{
  if (data == nil) StPerror("nil data pointer");
  if (data->strings == nil) StPerror("nil string data pointer");
  return(data->strings);
}
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                        General Data Functions                        **/
/**                                                                      **/
/**************************************************************************/

IViewDataNumVariables(p)
	IViewData p;
{
  return(IViewBasicPointsNumVariables(IViewDataPoints(p)));
}

IViewDataSetVariableLabel(p, var, s)
	IViewData p;
	unsigned var;
	char *s;
{
  if (p == nil || var >= IViewDataNumVariables(p)) return;
  
  StFree(p->variableLabels[var]);
  p->variableLabels[var] = nil;
  if (s != 0 && strlen(s) > 0) {
    p->variableLabels[var] = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(p->variableLabels[var], s);
  }
}

char *IViewDataVariableLabel(p, var)
	IViewData p;
	unsigned var;
{
  if (p == nil || var >= IViewDataNumVariables(p)) return(nil);
  return(p->variableLabels[var]);
}

static CalculateLocationScale(p)
	IViewData p;
{
  int i, fixed, vars = IViewDataNumVariables(p);
  double range, screen_range, scale;
  
  for (i = 0; i < vars; i++) {
    range = p->maxes[i] - p->mins[i];
    screen_range = p->screen_maxes[i] - p->screen_mins[i];
    p->scale[i] = (range > 0) ? screen_range / range : 0.0;
    p->location[i] = p->screen_mins[i] - p->mins[i] * p->scale[i];
  }
  if (vars > 0) {
    fixed = (p->location[0] == 0.0) ? TRUE : FALSE;
    scale = p->scale[0];
    for (i = 1; i < vars; i++)
      fixed = (fixed && scale == p->scale[i] && p->location[i] == 0.0);
  }
  return(fixed);
}

IViewDataSetRange(p, var, low, high)
	IViewData p;
	unsigned var;
	double low, high;
{
  int fixed;
  
  if (p != nil && var < IViewDataNumVariables(p) && low < high) {
    p->mins[var] = low;
    p->maxes[var] = high;
    fixed = CalculateLocationScale(p);
    IViewBasicPointsSetRecalculate(p->points, fixed);
    IViewBasicPointsSetRecalculate(p->lines, fixed);
#ifdef USESTRINGS
    IViewBasicPointsSetRecalculate(p->strings, fixed);
#endif /* USESTRINGS */
  }
}

IViewDataGetRange(p, var, low, high)
	IViewData p;
	unsigned var;
	double *low, *high;
{
  if (p != nil && var < IViewDataNumVariables(p)) {
    if (low != nil) *low = p->mins[var];
    if (high != nil) *high = p->maxes[var];
  }
}

IViewDataSetScreenRange(p, var, low, high)
	IViewData p;
	unsigned var;
	int low, high;
{
  int fixed;
  
  if (p != nil && var < IViewDataNumVariables(p) && low < high) {
    p->screen_mins[var] = low;
    p->screen_maxes[var] = high;
    fixed = CalculateLocationScale(p);
    IViewBasicPointsSetRecalculate(IViewDataPoints(p), fixed);
    IViewBasicPointsSetRecalculate(IViewDataLines(p), fixed);
#ifdef USESTRINGS
    IViewBasicPointsSetRecalculate(IViewDataStrings(p), fixed);
#endif /* USESTRINGS */
  }
}

IViewDataGetScreenRange(p, var, low, high)
	IViewData p;
	unsigned var;
	int *low, *high;
{
  if (p != nil && var < IViewDataNumVariables(p)) {
    if (low != nil) *low = p->screen_mins[var];
    if (high != nil) *high = p->screen_maxes[var];
  }
}

IViewDataSetIdentityTransformation(data)
     IViewData data;
{
  int i, j, vars = IViewDataNumVariables(data);
  double *p;
  
  if (data == nil) return;
  
  data->transformed = FALSE;
  if (data->transformation == nil) {
    data->transformation = (double **) StCalloc(sizeof(double *), vars);
    p = (double *) StCalloc(sizeof(double), vars * vars);
    for (i = 0; i < vars; i++)
      data->transformation[i] = p + i * vars;
  }
  
  for (i = 0; i < vars; i++)
    for (j = 0; j < vars; j++)
      data->transformation[i][j] = (i == j) ? 1.0 : 0.0;

  IViewBasicPointsSetTransformation(IViewDataPoints(data), data->transformation);
  IViewBasicPointsSetTransformation(IViewDataLines(data), data->transformation);
#ifdef USESTRINGS
  IViewBasicPointsSetTransformation(IViewDataStrings(data), data->transformation);
#endif /* USESTRINGS */
}

IViewDataSetTransformation(data, a)
     IViewData data;
     double **a;
{
  int i, j, vars = IViewDataNumVariables(data);
  double *p;
  
  if (data == nil) return;
  data->transformed = TRUE;
  if (a != nil) {
    if (data->transformation == nil) {
      data->transformation = (double **) StCalloc(sizeof(double *), vars);
      p = (double *) StCalloc(sizeof(double), vars * vars);
      for (i = 0; i < vars; i++)
        data->transformation[i] = p + i * vars;
    }
  
    for (i = 0; i < vars; i++)
      for (j = 0; j < vars; j++)
        data->transformation[i][j] = a[i][j];
  }
  else if (data->transformation != nil) {
    StFree(data->transformation[0]);
    StFree(data->transformation);
  }

  IViewBasicPointsSetTransformation(IViewDataPoints(data), data->transformation);
  IViewBasicPointsSetTransformation(IViewDataLines(data), data->transformation);
#ifdef USESTRINGS
  IViewBasicPointsSetTransformation(IViewDataStrings(data), data->transformation);
#endif /* USESTRINGS */
}

double **IViewDataTransformation(data)
     IViewData data;
{
  if (data == nil) return(nil);
  else return(data->transformation);
}

IViewDataApplyTransformation(data, a, inbasis)
     IViewData data;
     double **a;
     int *inbasis;
{
  static int temp_size;
  static double *temp;
  double **b;
  int i, j, k, vars = IViewDataNumVariables(data);

  if (data == nil) return;
  
  if (data->transformation == nil) IViewDataSetIdentityTransformation(data);
  data->transformed = TRUE;
  b = data->transformation;
  
  if (temp_size < vars) {
    StFree(temp);
    temp = nil;
  }
  if (temp == nil) {
    temp_size = vars;
    temp = (double *) StCalloc(sizeof(double), temp_size);
  }

  for (j = 0; j < vars; j++) {
    for (i = 0; i < vars; i++) 
      if (inbasis[i]){
        temp[i] = 0.0;
        for (k = 0; k < vars; k++)
          if (inbasis[k] && a[i][k] != 0.0 && b[k][j] != 0) 
            temp[i] += a[i][k] * b[k][j];
      }
    for (i = 0; i < vars; i++)
      if (inbasis[i]) b[i][j] = temp[i];
  }

  IViewBasicPointsApplyTransformation(IViewDataPoints(data), a, inbasis);
  IViewBasicPointsApplyTransformation(IViewDataLines(data), a, inbasis);
#ifdef USESTRINGS
  IViewBasicPointsApplyTransformation(IViewDataStrings(data), a, inbasis);
#endif /* USESTRINGS */
}

IViewDataIsTransformed(data)
     IViewData data;
{
  return (data != nil && data->transformed);
}

/**************************************************************************/
/**                                                                      **/
/**                          Point Data Functions                        **/
/**                                                                      **/
/**************************************************************************/

IViewDataAddPoints(p, n)
     IViewData p;
     int n;
{
  int i, fixed, old_n = IViewDataNumPoints(p);
    
  IViewAddBasicPoints(IViewDataPoints(p), n);
  n += old_n;
  p->pointInfo = StRRealloc(p->pointInfo, sizeof(struct point_info), n);
  
  for (i = old_n; i < n; i++) {
    IViewDataSetPointSymbol(p, i, 4, 5);
    IViewDataSetPointState(p, i, pointNormal);
    IViewDataSetPointColor(p, i, -1);
  }
  fixed = CalculateLocationScale(p);
  IViewBasicPointsSetRecalculate(IViewDataPoints(p), fixed);
}

IViewDataClearPoints(p)
	IViewData p;
{
  PointInfo *pointInfo;
  int i, n = IViewDataNumPoints(p);
   
  for (i = 0; i < n; i++) {
    pointInfo = (PointInfo *) StRPtr(p->pointInfo);
    if (pointInfo[i].label != nil) StFree(pointInfo[i].label);
  }
  StRFree(p->pointInfo);
  p->pointInfo = nil;
  IViewClearBasicPoints(IViewDataPoints(p));
}

IViewDataNumPoints(p)
	IViewData p;
{
  return(IViewBasicPointsNumPoints(IViewDataPoints(p)));
}

IViewDataSetPointValue(p, var, point, value)
	IViewData p;
	int var, point;
	double value;
{
  IViewBasicPointSetValue(IViewDataPoints(p), var, point, value);
}

double IViewDataPointValue(p, var, point)
	IViewData p;
	int var, point;
{
  return(IViewBasicPointValue(IViewDataPoints(p), var, point));
}

double IViewDataPointTransformedValue(p, var, point)
	IViewData p;
	int var, point;
{
  return(TransformedValue(IViewDataPoints(p), var, point));
}

IViewDataPointScreenValue(p, var, point)
	IViewData p;
	int var, point;
{
  return(IViewBasicPointScreenValue(IViewDataPoints(p), var, point));
}

IViewDataGetScreenPointValues(d, point, x)
	IViewData d;
	int point, *x;
{
  IViewBasicPointsGetScreenValues(IViewDataPoints(d), point, x);
}

IViewDataSetPointMask(p, point, masked)
     IViewData p;
     unsigned point;
     int masked;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].masked = masked;
}

IViewDataPointMasked(p, point)
     IViewData p;
     unsigned point;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].masked);
}

IViewDataSetPointColor(p, point, color)
     IViewData p;
     unsigned point;
     int color;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].color = color;
}

IViewDataPointColor(p, point)
     IViewData p;
     unsigned point;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].color);
}

IViewDataSetPointState(p, point, state)
     IViewData p;
     unsigned point;
     PointState state;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].state = state;
}

PointState IViewDataPointState(p, point)
     IViewData p;
     unsigned point;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return((PointState) 0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return(info[point].state);
}

IViewDataSetPointScreenState(p, point, state)
     IViewData p;
     unsigned point;
     PointState state;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].screen_state = state;
}

PointState IViewDataPointScreenState(p, point)
     IViewData p;
     unsigned point;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return((PointState) 0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return(info[point].screen_state);
}

IViewDataResetScreenStates(p)
     IViewData p;
{
  int i, n = IViewDataNumPoints(p);
  PointInfo *info;
  
  if (p == nil || p->pointInfo == nil) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  for (i = 0; i < n; i++) info[i].screen_state = info[i].state;
}

IViewDataSetPointMark(p, point, marked)
     IViewData p;
     unsigned point;
     int marked;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].marked = marked;
}

IViewDataPointMarked(p, point)
     IViewData p;
     unsigned point;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].marked);
}

IViewDataClearPointMarks(p)
     IViewData p;
{
  int i, n = IViewDataNumPoints(p);
  PointInfo *info;
  
  if (p == nil || p->pointInfo == nil) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  for (i = 0; i < n; i++) info[i].marked = FALSE;
}

IViewDataSetPointLabel(p, point, s)
	IViewData p;
	unsigned point;
	char *s;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  StRLock(p->pointInfo);
  info = (PointInfo *) StRPtr(p->pointInfo);
  
  StFree(info[point].label);
  info[point].label = nil;
  if (s != 0 && strlen(s) > 0) {
    info[point].label = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(info[point].label, s);
  }
  StRUnlock(p->pointInfo);
}

char *IViewDataPointLabel(p, point)
	IViewData p;
	unsigned point;
{
  PointInfo *info;
  char label[100];
  
  if (point >= IViewDataNumPoints(p)) return(nil);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  
  /* only allocate label on demand to save time and space */
  if (info[point].label == nil) {
    sprintf(label, "%d", point);
    IViewDataSetPointLabel(p, point, label);
  }
  return(info[point].label);
}

IViewDataSetPointSymbol(p, point, sym, hsym)
     IViewData p;
     unsigned point;
     int sym, hsym;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].symbol.normal = sym;
  info[point].symbol.highlighted = hsym;
}

IViewDataGetPointSymbol(p, point, sym, hsym)
     IViewData p;
     unsigned point;
     int *sym, *hsym;
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  if (sym != nil) *sym = info[point].symbol.normal;
  if (hsym != nil) *hsym = info[point].symbol.highlighted;
}
  
/**************************************************************************/
/**                                                                      **/
/**                            Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

IViewDataNumLines(p)
	IViewData p;
{
  return(IViewBasicPointsNumPoints(IViewDataLines(p)));
}

IViewDataAddLines(p, n)
     IViewData p;
     int n;
{
  int i, old_n = IViewDataNumLines(p);
  if (p == nil) return;
  
  IViewAddBasicPoints(IViewDataLines(p), n);
  n += old_n;
  p->lineInfo = StRRealloc(p->lineInfo, sizeof(struct line_info), n);
  
  for (i = old_n; i < n - 1; i++) {
    IViewDataSetNextLine(p, i, i + 1);
    IViewDataSetLineType(p, i, 0);
    IViewDataSetLineColor(p, i, -1);
    IViewDataSetLineWidth(p, i, 1);
  }
  IViewDataSetNextLine(p, n - 1, -1);
  IViewDataSetLineType(p, n - 1, 0);  
  IViewDataSetLineColor(p, n - 1, -1);
  IViewDataSetLineWidth(p, n - 1, 1);
}

IViewDataClearLines(p)
	IViewData p;
{
  StRFree(p->lineInfo);
  p->lineInfo = nil;
  IViewClearBasicPoints(IViewDataLines(p));
}

IViewDataSetLineValue(p, var, line, value)
	IViewData p;
	int var, line;
	double value;
{
  IViewBasicPointSetValue(IViewDataLines(p), var, line, value);
}

double IViewDataLineValue(p, var, line)
	IViewData p;
	int var, line;
{
  return(IViewBasicPointValue(IViewDataLines(p), var, line));
}

double IViewDataLineTransformedValue(p, var, line)
	IViewData p;
	int var, line;
{
  return(TransformedValue(IViewDataLines(p), var, line));
}

IViewDataLineScreenValue(p, var, line)
	IViewData p;
	int var, line;
{
  return(IViewBasicPointScreenValue(IViewDataLines(p), var, line));
}

IViewDataSetLineMask(p, line, masked)
     IViewData p;
     unsigned line;
     int masked;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].masked = masked;
}

IViewDataLineMasked(p, line)
     IViewData p;
     unsigned line;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return((int) info[line].masked);
}

IViewDataSetLineColor(p, line, color)
     IViewData p;
     unsigned line;
     int color;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].color = color;
}

IViewDataLineColor(p, line)
     IViewData p;
     unsigned line;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return((int) info[line].color);
}

IViewDataSetLineWidth(p, line, width)
     IViewData p;
     unsigned line, width;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].width = width;
}

IViewDataGetLineWidth(p, line, width)
     IViewData p;
     unsigned line, *width;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  if (width != 0) *width = (int) info[line].width;
}

IViewDataSetNextLine(p, line, next)
     IViewData p;
     unsigned line;
     int next;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p) || next >= IViewDataNumLines(p)) return;

  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].next = next;
}

IViewDataNextLine(p, line)
     IViewData p;
     unsigned line;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return(info[line].next);
}

IViewDataSetLineType(p, line, type)
     IViewData p;
     unsigned line;
     int type;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].type = type;
}

IViewDataLineType(p, line)
     IViewData p;
     unsigned line;
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return(info[line].type);
}

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                           String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewDataNumStrings(p)
	IViewData p;
{
  return(IViewBasicPointsNumPoints(IViewDataStrings(p)));
}

IViewDataAddStrings(p, n)
     IViewData p;
     int n;
{
  int i, old_n = IViewDataNumStrings(p);
  if (p == nil) return;
  
  IViewAddBasicPoints(IViewDataStrings(p), n);
  n += old_n;
  p->stringInfo = StRRealloc(p->stringInfo, sizeof(struct string_info), n);
  
  for (i = old_n; i < n; i++) IViewDataSetStringColor(p, i, -1);
}

IViewDataClearStrings(p)
	IViewData p;
{
  StringInfo *stringInfo;
  int i, n = IViewDataNumStrings(p);
   
  for (i = 0; i < n; i++) {
    stringInfo = (StringInfo *) StRPtr(p->stringInfo);
    if (stringInfo[i].string != nil) StFree(stringInfo[i].string);
  }
  StRFree(p->stringInfo);
  p->stringInfo = nil;
  IViewClearBasicPoints(IViewDataStrings(p));
}

IViewDataSetStringValue(p, var, string, value)
	IViewData p;
	int var, string;
	double value;
{
  IViewBasicPointSetValue(IViewDataStrings(p), var, string, value);
}

double IViewDataStringValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(IViewBasicPointValue(IViewDataStrings(p), var, string));
}

double IViewDataStringTransformedValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(TransformedValue(IViewDataStrings(p), var, string));
}

IViewDataStringScreenValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(IViewBasicPointScreenValue(IViewDataStrings(p), var, string));
}

IViewDataSetStringMask(p, string, masked)
     IViewData p;
     unsigned string;
     int masked;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].masked = masked;
}

IViewDataStringMasked(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(0);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return((int) info[string].masked);
}

IViewDataSetStringColor(p, string, color)
     IViewData p;
     unsigned string;
     int color;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].color = color;
}

IViewDataStringColor(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(0);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return((int) info[string].color);
}

IViewDataSetStringString(p, string, s)
     IViewData p;
     unsigned string;
     char *s;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  StRLock(p->stringInfo);
  StFree(info[string].string);
  info[string].string = nil;
  if (s != 0 && strlen(s) > 0) {
    info[string].string = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(info[string].string, s);
  }
  StRUnlock(p->stringInfo);
}

char *IViewDataStringString(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(nil);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return(info[string].string);
}

IViewDataSetStringModifiers(p, string, up, h, v)
     IViewData p;
     unsigned string;
     int up, h, v;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].up = up;
  info[string].h = h;
  info[string].v = v;
}

IViewDataGetStringModifiers(p, string, up, h, v)
     IViewData p;
     unsigned string;
     int *up, *h, *v;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  if (up != nil) *up = info[string].up;
  if (h != nil) *h = info[string].h;
  if (v != nil) *v = info[string].v;
}
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                          Data Drawing Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewDataDrawPoints(data, w, var1, var2, m, n, offset)
	IViewData data;
    IVIEW_WINDOW w;
    unsigned var1, var2, m, n;
    int offset;
{
  int vars = IViewNumVariables(w);
  int right, bottom;
  int point, left, top, width, height, orig_x, orig_y;
  int showingLabels = IViewShowingLabels(w);
  int x, y, sym, hsym;
  PointState state;
  PointInfo *info;
  Fixed *screen1, *screen2;
  char *gwinfo = IViewWindowWinInfo(w);
  int mode = StGWDrawMode(gwinfo);
  int draw_color, use_color = StGWUseColor(gwinfo);
  
  if (n > IViewNumPoints(w)) return;
  if (var1 >= vars || var2 >= vars) return;
  if (data == nil || data->pointInfo == nil || data->points == nil) return;
  if (data->points->screen_data[var1] == nil 
      || data->points->screen_data[var2] == nil) return;
  
  if (data->points->recalculateScreenPoints)
    RecalculateScreenPoints(data->points);
  info = (PointInfo *) StRPtr(data->pointInfo);
  screen1 = (Fixed *) StRPtr(data->points->screen_data[var1]);
  screen2 = (Fixed *) StRPtr(data->points->screen_data[var2]);

  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &orig_x, &orig_y);
  right = left + width;
  bottom = top + height;

  if (use_color) draw_color = StGWDrawColor(gwinfo);
  for (point = m; point < n; point++, info++) {
    state = info->state;
    sym = info->symbol.normal;
    hsym = info->symbol.highlighted;
    if (! info->masked && state != pointInvisible) {
#ifdef PERSPECTIVE
      x = orig_x + PerspScreenValue(data->points, var1, point);
      y = orig_y - PerspScreenValue(data->points, var2, point);
#else
      x = orig_x + FixRound(screen1[point]);
      y = orig_y - FixRound(screen2[point]);
#endif /* PERSPECTIVE */
      if (TRUE /* x >= left && x <= right && y >= top && y <= bottom */) {
        if (use_color) {
          if (info->color >= 0) StGWSetDrawColor(gwinfo, info->color);
          else StGWSetDrawColor(gwinfo, draw_color);
        }
        if (state == pointNormal) StGWDrawSymbol(gwinfo, sym, x, y);
        else {
          StGWDrawSymbol(gwinfo, hsym, x, y);
          if (showingLabels) {
            StGWSetDrawMode(gwinfo, 1);
            StGWDrawString(gwinfo, info->label, x + offset, y - offset);
            StGWSetDrawMode(gwinfo, mode);
          }
        }
      }
    }
  }
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}

IViewDataCuePoints(data, var, cut1, cut2, cut3, m, n)
	IViewData data;
    int cut1, cut2, cut3;
    unsigned var, m, n;
{
  int vars;
  int point;
  PointInfo *info;
  Fixed fcut1, fcut2, fcut3;
  Fixed *screen_z, z;
  
  if (data == nil || data->pointInfo == nil || data->points == nil) return;
  if (data->points->screen_data[var] == nil) return;
  if (n > IViewDataNumPoints(data)) return;
  vars = IViewDataNumVariables(data);
  if (var >= vars) return;
  
  if (data->points->recalculateScreenPoints)
    RecalculateScreenPoints(data->points);
  info = (PointInfo *) StRPtr(data->pointInfo);
  screen_z = (Fixed *) StRPtr(data->points->screen_data[var]);

  fcut1 = Int2Fixed(cut1);
  fcut2 = Int2Fixed(cut2);
  fcut3 = Int2Fixed(cut3);
  for (point = m; point < n; point++, info++) {
    z = screen_z[point];
    if (z < fcut1) info->symbol.normal = 0;
    else if (z < fcut2) info->symbol.normal = 1;
    else if (z < fcut3) info->symbol.normal = 2;
    else info->symbol.normal = 3;
    info->symbol.highlighted = 5;
  }
}

#ifdef PERSPECTIVE
static PerspScreenValue(p, var, point)
	IViewBasicPoints p;
	unsigned var, point;
{
  Fixed screen_x, screen_z, d = 800;

  if (p->num_variables > 2 && p->num_variables > var) {
    screen_z = FixRound(((Fixed *) StRPtr(p->screen_data[2]))[point]);
    /* could create problems if StRPtr return value is zero JKL */
    screen_x = FixRound(((Fixed *) StRPtr(p->screen_data[var]))[point]);
    if (screen_z != d) {
      screen_x = (screen_x * d) / (d - screen_z);
      return((int) screen_x);
    }
    else return(FixRound(screen_x));
  }
  else return(0);
}
#endif /* PERSPECTIVE */


/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "iview.h"

#ifndef TRUE
#define TRUE 1
#endif TRUE
#ifndef FALSE
#define FALSE 0
#endif FALSE

/* external functions */
extern double IViewPointValue(), IViewLineValue(),
 IViewPointScaledValue(), IViewLineScaledValue(),
 IViewShift(), IViewScale(), IViewDecodeValue();
#ifdef USESTRINGS
extern double IViewStringValue(), IViewStringScaledValue();
#endif /* USESTRINGS */
extern PointState IViewPointState();

static double dmin(x, y) double x, y; { return((x > y) ? y : x); }
static double dmax(x, y) double x, y; { return((x > y) ? x : y); }

static adjust_range_step(value, low, high, inited)
	double value, *low, *high;
	int *inited;
{
  if (*inited) { 
    *low = dmin(*low, value); 
    *high = dmax(*high, value);
  }
  else {
    *low = value;
    *high = value;
    *inited = TRUE;
  }
}

IViewGetVisibleRange(w, var, plow, phigh)
	IVIEW_WINDOW w;
	unsigned var;
	double *plow, *phigh;
{
  int inited, i, n;
  double value, low, high;
  
  inited = FALSE;
  
  n = IViewNumPoints(w);
  for (i = 0; i < n; i++) {
    if (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible) {
      value = IViewPointScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }
  }
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) {
    if (! IViewLineMasked(w, i)) {
      value = IViewLineScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }      
  }
#ifdef USESTRINGS
  n = IViewNumStrings(w);
  for (i = 0; i < n; i++) {
    if (! IViewStringMasked(w, i)) {
    value = IViewStringScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }
  }
#endif /* USESTRINGS */
  if (plow != nil) *plow = IViewDecodeValue(w, low, var);
  if (phigh != nil) *phigh = IViewDecodeValue(w, high, var);
}

static apply_scale_shift_data(w, var, scale, shift)
	IVIEW_WINDOW w;
	unsigned var;
	double scale, shift;
{
  double value;
  int i, n;

  if (var >= IViewNumVariables(w)) return;
  IViewSetScale(w, var, scale * IViewScale(w, var));
  IViewSetShift(w, var, scale * IViewShift(w, var) + shift);
  
  n = IViewNumPoints(w);
  for (i = 0; i < n; i++) {
    value = IViewPointScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetPointScaledValue(w, var, i, value);
  }
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) {
    value = IViewLineScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetLineScaledValue(w, var, i, value);
  }
#ifdef USESTRINGS
  n = IViewNumStrings(w);
  for (i = 0; i < n; i++) {
    value = IViewStringScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetStringScaledValue(w, var, i, value);
  }
#endif /* USESTRINGS */
}

static scale_to_range(w, var, low, high)
	IVIEW_WINDOW w;
	unsigned var;
	double low, high;
{
  double old_low, old_high, scale, shift, old_scale, old_shift;

  if (var >= IViewNumVariables(w)) return;
  IViewGetVisibleRange(w, var, &old_low, &old_high);
  if (old_high <= old_low) return;
  scale = (high - low) / (old_high - old_low);
  shift = - scale * old_low + low;

  old_scale = IViewScale(w, var);
  old_shift = IViewShift(w, var);
  if (old_scale > 0.0) {
    scale = scale / old_scale;
    shift = shift - scale * old_shift;
  }
  
  apply_scale_shift_data(w, var, scale, shift);
}

IViewScaleToRange(w, var, low, high)
	IVIEW_WINDOW w;
	unsigned var;
	double low, high;
{
  scale_to_range(w, var, low, high);
}

IViewApplyScaleShift(w, var, scale, shift)
	IVIEW_WINDOW w;
	unsigned var;
	double scale, shift;
{
  apply_scale_shift_data(w, var, scale, shift);
}

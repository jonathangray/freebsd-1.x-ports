/* xsiview3 - XLISP interface to IVIEW dynamic graphics package.       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "iview.h"

# define numberp(x) (floatp(x) || fixp(x))
# define sequencep(x) (listp(x) || simplevectorp(x))
# define seqlen(x) ((listp(x)) ? llength(x) : getsize(x))

/* external functions */
extern IVIEW_WINDOW GETIVIEWADDRESS();
extern LVAL list2(), getnextelement();
extern LVAL recursive_subr_map_elements(), arraydata(), newarray();
extern LVAL xsgetmatrix(), integer_list_2(), double_list_2();
extern IVIEW_WINDOW get_iview_address();
extern double makedouble();
extern char *IViewVariableLabel();
extern double **IViewTransformation();

/* external variables */
extern char buf[];
extern LVAL s_true;
extern LVAL s_invisible, s_normal, s_hilited, s_selected;
extern LVAL s_solid, s_dashed;
extern LVAL sk_point_labels;
extern LVAL s_left, s_center, s_right, s_top, s_bottom;
extern LVAL sk_draw, sk_redraw, sk_redraw_content,sk_scale, sk_basis;

/* static global variables */
static int maxvars = 0;
static double **transform, *transformdata;
static int *inbasis;
static IVIEW_WINDOW wind;
static int range_type;

/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

static LVAL base_variable_label()
{
  int var, set = FALSE;
  char *label;
  LVAL result;
  
  var = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    label = (char *) getstring(xlgastring());
  }
  xllastarg();
  
  if (set) IViewSetVariableLabel(wind, var, label);
  
  label = IViewVariableLabel(wind, var);
  if (label == nil) result = newstring(1);
  else {
    result = newstring(strlen(label) + 1);
    strcpy(getstring(result), label);
  }
  
  return(result);
}

static LVAL variable_label() 
{
  return(recursive_subr_map_elements(base_variable_label, variable_label));
} 

LVAL iview_variable_label()
{
  wind = get_iview_address(xlgaobject());
  return(variable_label());
}

static LVAL base_range()
{
  int var, set = FALSE;
  double low, high;
  
  var = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    low = makedouble(xlgetarg());
    high = makedouble(xlgetarg());
  }
  
  if (set) {
    if (range_type != 'S') IViewSetRange(wind, var, low, high);
    else IViewSetScaledRange(wind, var, low, high);
  }
  if (range_type != 'S') IViewGetRange(wind, var, &low, &high);
  else IViewGetScaledRange(wind, var, &low, &high);

  return(double_list_2(low, high));
}

static LVAL range()
{
  return(recursive_subr_map_elements(base_range, range));
}

LVAL iview_range()
{
  LVAL object = xlgaobject(), result;
  int set = (xlargc  > 1) ? TRUE : FALSE, draw;

  wind = get_iview_address(object);
  draw = draw_key_arg(TRUE);
  range_type = 'N';
  result = range();
  if (set) check_redraw(object, draw, FALSE);
  return(result);
}

LVAL iview_scaled_range()
{
  LVAL object = xlgaobject(), result;
  int set = (xlargc  > 1) ? TRUE : FALSE, draw;

  wind = get_iview_address(object);
  draw = draw_key_arg(TRUE);
  range_type = 'S';
  result = range();
  if (set) check_redraw(object, draw, FALSE);
  return(result);
}

static LVAL base_screen_range()
{
  int var, set = FALSE;
  int low, high;
  
  var = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    low = getfixnum(xlgafixnum());
    high = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  if (set) IViewSetScreenRange(wind, var, low, high);
  IViewGetScreenRange(wind, var, &low, &high);
  
  return(integer_list_2(low, high));
}

static LVAL screen_range()
{
  return(recursive_subr_map_elements(base_screen_range, screen_range));
}

LVAL iview_screen_range()
{
  wind = get_iview_address(xlgaobject());
  return(screen_range());
}

static set_internal_transformation(vars, m, b)
	int vars;
	LVAL m, b;
{
  int i, j, k, rows, cols;
  LVAL data;
  
  if (vars <= 0) return;
  if (vars > maxvars) {
    maxvars = 0;
    StFree(transformdata);
    StFree(transform);
    StFree(inbasis);
    transformdata = (double *) StCalloc(vars * vars, sizeof(double));
    transform = (double **) StCalloc(vars, sizeof(double *));
    for (i = 0; i < vars; i++) transform[i] = transformdata + vars * i;
    inbasis = (int *) StCalloc(vars, sizeof(double));
    maxvars = vars;
  }
  
  if (! matrixp(m)) xlerror("not a matrix", m);
  rows = numrows(m);
  cols = numcols(m);
  if (rows > vars) rows = vars;
  if (cols > vars) cols = vars;
  if (rows != cols) xlerror("bad transformation matrix", m);

  /* fill in upper left corner of transform from m; rest is identity */
  data = arraydata(m);
  for (i = 0, k = 0; i < rows; i++) {
    for (j = 0; j < cols; j++, k++)
      transform[i][j] = makedouble(getelement(data, k));
    for (j = cols; j < vars; j++)
      transform[i][j] = (i == j) ? 1.0 : 0.0;
  }
  for (i = rows; i < vars; i++)
    for (j = 0; j < vars; j++)
      transform[i][j] = (i == j) ? 1.0 : 0.0;
    
  /* figure out basis elements using b and size of m */
  if (b != NIL) {
    if (! sequencep(b)) xlerror("not a sequence", b);
    if (seqlen(b) != rows) xlerror("wrong length for basis", b);
    for (i = 0; i < rows; i++)
      inbasis[i] = (getnextelement(&b, i) != NIL) ? TRUE : FALSE;
  }
  else for (i = 0; i < rows; i++) inbasis[i] = TRUE;
  for (i = rows; i < vars; i++) inbasis[i] = FALSE;
}

static LVAL newmatrix(r, c)
	unsigned r, c;
{
  LVAL rows, cols, dim, result;
  
  
  xlstkcheck(3);
  xlsave(rows);
  xlsave(cols);
  xlsave(dim);
  
  rows = cvfixnum((FIXTYPE) r);
  cols = cvfixnum((FIXTYPE) c);
  dim = list2(rows, cols);
  result = newarray(dim, NIL, NIL);
  xlpopn(3);
  
  return(result);
}

static LVAL make_transformation(a, vars)
	double **a;
	int vars;
{
  LVAL result, data;
  int i, j, k;
  
  if (a == nil) return(NIL);
  
  xlsave1(result);
  result = newmatrix(vars, vars);
  data = arraydata(result);
  for (i = 0, k = 0; i < vars; i++)
    for (j = 0; j < vars; j++, k++)
      setelement(data, k, cvflonum((FLOTYPE) a[i][j]));
  xlpop();
  return(result);
}

LVAL iview_transformation()
{
  IVIEW_WINDOW w;
  LVAL m, object;
  int set = FALSE;
  int vars;
  
  object = xlgaobject();
  w = get_iview_address(object);
  if (moreargs()) {
    set = TRUE;
    m = xlgetarg();
  }
  
  vars = IViewNumVariables(w);
  if (set) {
    if (m == NIL) IViewSetIdentityTransformation(w);
    else {
      set_internal_transformation(vars, m, NIL);
      IViewSetTransformation(w, transform);
    }
    check_redraw(object, TRUE, TRUE);
  }
  else m = (IViewIsTransformed(w))
         ? make_transformation(IViewTransformation(w), vars) : NIL;
  
  return(m);
}

LVAL iview_apply_transformation()
{
  IVIEW_WINDOW w;
  LVAL m, b, object;
  int vars;

  object = xlgaobject();
  w = get_iview_address(object);
  m = xsgetmatrix();
  if (! xlgetkeyarg(sk_basis, &b)) b = NIL;

  vars = IViewNumVariables(w);
  set_internal_transformation(vars, m, b);
  IViewApplyTransformation(w, transform, inbasis);
  check_redraw(object, TRUE, TRUE);
  
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                     IView Data Drawing Functions                     **/
/**                                                                      **/
/**************************************************************************/

static LVAL draw_data(which)
	int which;
{
  IVIEW_WINDOW w;
  int var1, var2, m, n;
  
  w = get_iview_address(xlgaobject());
  var1 = getfixnum(xlgafixnum());
  var2 = getfixnum(xlgafixnum());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  xllastarg();
  
  switch(which) {
  case 'P': IViewDrawDataPoints(w, var1, var2, m, n); break;
  case 'L': IViewDrawDataLines(w, var1, var2, m, n); break;
#ifdef USESTRINGS
  case 'S': IViewDrawDataStrings(w, var1, var2, m, n); break;
#endif /* USESTRINGS */
  }
  return(NIL);
}

LVAL iview_draw_data_points()  { return(draw_data('P')); }
LVAL iview_draw_data_lines()   { return(draw_data('L')); }
#ifdef USESTRINGS
LVAL iview_draw_data_strings() { return(draw_data('S')); }
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                     Standard Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_std_mark_points_in_rect()
{
  IVIEW_WINDOW w;
  int left, top, width, height;

  w = get_iview_address(xlgaobject());
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  xllastarg();
  
  IViewStdMarkPointsInRect(w, left, top, width, height);
  return(NIL);
}

LVAL iview_std_adjust_screen()
{
  IVIEW_WINDOW w;
  
  w = get_iview_address(xlgaobject());
  IViewStdAdjustScreen(w);
  return(NIL);
}

PointState decode_point_state(state)
	LVAL state;
{
  if (state == s_invisible) return(pointInvisible);
  else if (state == s_normal) return(pointNormal);
  else if (state == s_hilited) return(pointHilited);
  else if (state == s_selected) return(pointSelected);
  else xlerror("unknown point state", state);
}
  
LVAL iview_std_adjust_points_in_rect()
{
  IVIEW_WINDOW w;
  int left, top, width, height;
  PointState state;
  
  w = get_iview_address(xlgaobject());
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  state = decode_point_state(xlgetarg());
  xllastarg();
  
  IViewStdAdjustPointsInRect(w, left, top, width, height, state);
  return(NIL);
}

LVAL iview_std_adjust_screen_point()
{
  LVAL object;
  int point;

  object = xlgaobject();
  point = getfixnum(xlgafixnum());
  xllastarg();

  IViewStdAdjustScreenPoint(get_iview_address(object), point);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                       IView Rotation Functions                       **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_rotate_2()
{
  IVIEW_WINDOW w;
  int var1, var2;
  double alpha;
  LVAL object;
  
  object = xlgaobject();
  w = get_iview_address(object);
  var1 = getfixnum(xlgafixnum());
  var2 = getfixnum(xlgafixnum());
  alpha = makedouble(xlgetarg());
  
  IViewRotate2(w, var1, var2, alpha);
  check_redraw(object, TRUE, TRUE);
  
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                        Miscellaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_get_nice_range()
{
  double low, high;
  int ticks;
  LVAL temp, result;
  
  low = makedouble(xlgetarg());
  high = makedouble(xlgetarg());
  ticks = getfixnum(xlgafixnum());
  xllastarg();
  
  GetNiceRange(&low, &high, &ticks);
  xlstkcheck(2);
  xlsave(result);
  xlsave(temp);
  temp = cvfixnum((FIXTYPE) ticks); result = consa(temp);
  temp = cvflonum((FLOTYPE) high); result = cons(temp, result);
  temp = cvflonum((FLOTYPE) low); result = cons(temp, result);  
  xlpopn(2);
  
  return(result);
}

LVAL iview_adjust_depth_cuing()
{
  LVAL object;
  int vz;
  IVIEW_WINDOW w;
  int i, low, high, cut1, cut2, cut3, z, nz;
  int next, n;
  
  object = xlgaobject();
  vz = getfixnum(xlgafixnum());
  xllastarg();
  
  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);

  IViewGetScreenRange(w, vz, &low, &high);
  cut1 = (low + high) / 2 - (high - low) / 8;
  cut2 = (low + high) / 2;
  cut3 = (low + high) / 2 + (high - low) / 8;
  n  = IViewNumPoints(w);
  IViewDepthCuePoints(w, vz, cut1, cut2, cut3, 0, n);
  cut1 = (low + high) / 2 - (high - low) / 8;
  cut3 = (low + high) / 2 + (high - low) / 8;
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) {
    z = IViewLineScreenValue(w, vz, i);
    next = IViewNextLine(w, i);
    nz = (next >= 0)
       ? IViewLineScreenValue(w, vz, next) : z;
    z = (z + nz) / 2;
    if (z < cut1) IViewSetLineWidth(w, i, 1);
    else if (z < cut3) IViewSetLineWidth(w, i, 2);
    else IViewSetLineWidth(w, i, 3);
  }
  return(NIL);
}

/* xsscatmat - XLISP interface to IVIEW dynamic graphics package.      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xmath.h"
#include "xlisp.h"
#include "iview.h"

# define numberp(x) (floatp(x) || fixp(x))
# define sequencep(x) (listp(x) || simplevectorp(x))
# define seqlen(x) ((listp(x)) ? llength(x) : getsize(x))

#define LABEL_OFFSET 3
#define AXIS_FRACTION 0.5

#define SPIN_CONTROL_HEIGHT 20
#define SPIN_CONTROL_SIZE 10
#define SPIN_CONTROL_LEFT 5
#define SPIN_CONTROL_TOP 5
#define SPIN_CONTROL_GAP 5
#define SPIN_PITCH_STRING "Pitch"
#define SPIN_ROLL_STRING  "Roll"
#define SPIN_YAW_STRING   "Yaw"
#define ALPHA 3.14159 / 72.0

#define Pitching    0
#define Rolling     1
#define Yawing      2
#define ApplyMatrix 3

typedef struct {
  int left, top, width, height;
} Rect;

/* external functions */
extern IVIEW_WINDOW IViewNew(), GETIVIEWADDRESS();
extern char *IViewVariableLabel(), *IViewPointLabel();
extern MouseMode IViewMouseMode();
extern PointState IViewPointState();
extern double IViewPointValue();
extern double IViewLineValue();
extern double makedouble();
extern LVAL slot_value(), integer_list_3(), integer_list_4(),
  recursive_subr_map_elements(), peekarg();
extern double **IViewTransformation(), IViewScale();

/* external variables */
extern LVAL s_depth_cuing, s_showing_labels, s_showing_axes, 
  s_variable_labels, s_content_variables, s_true, s_rotation_type,
  s_rotation_angle, s_rotation_controls, sk_scale, sk_draw, sk_show,
  sk_resize, sk_redraw, sk_adjust_depth_cuing, sk_draw_axes, sk_show_window,
  sk_apply_transformation, s_pitching, s_rolling, s_yawing;
  
/* forward declarations */
int IViewSpinRotate();

/**************************************************************************/
/**                                                                      **/
/**                      Spinner Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_allocate()
{
  LVAL object;
  int vars, i, show, ascent, height;
  IVIEW_WINDOW w;
  char *gwinfo;

  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);

  gwinfo = StGWObWinInfo(object);
  get_iview_ivars(object, &vars);
  
  if (vars < 3) xlfail("too few variables");
  w = IViewNew(object);
  initialize_iview(w, object);
  
  for (i = 0; i < vars; i++)
    IViewSetScaledRange(w, i, -sqrt((double) vars), sqrt((double) vars));
  set_content_variables(object, 0, 1, 2);
  
  IViewSetIdentityTransformation(w);
  set_rotation_type(object, Rolling);
  set_angle(object, ALPHA);
  ascent = StGWTextAscent(gwinfo);
  height = (ascent > SPIN_CONTROL_SIZE) ? 2 * ascent : SPIN_CONTROL_HEIGHT;
  StGrSetMargin(gwinfo, 0, 0, 0, height);
  
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);

  return(object);
}

/**************************************************************************/
/**                                                                      **/
/**                     State Accessors and Mutators                     **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_content_variables()
{
  int x, y, z, set = FALSE;
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
    z = getfixnum(xlgafixnum());
  } 
  xllastarg();
  
  if (set) set_content_variables(object, x, y, z);

  return(slot_value(object, s_content_variables));
}

static set_content_variables(object, x, y, z)
	LVAL object;
	unsigned x, y, z;
{    
  char *gwinfo = StGWObWinInfo(object);
  StGrSetContentVariables(gwinfo, x, y);
  set_slot_value(object, s_content_variables, integer_list_3(x, y, z));
}

static get_content_variables(object, x, y, z)
	LVAL object;
	int *x, *y, *z;
{
  LVAL val = slot_value(object, s_content_variables);
  if (consp(val) && fixp(car(val))) { 
    if (x != nil) *x = getfixnum(car(val)); 
    val = cdr(val);
  }    
  if (consp(val) && fixp(car(val))) {
    if (y != nil) *y = getfixnum(car(val));
    val = cdr(val);
  }    
  if (consp(val) && fixp(car(val))) {
    if (z != nil) *z = getfixnum(car(val));
    val = cdr(val);
  }    
}

LVAL iview_spin_depth_cuing()
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) 
    set_cuing(object, (xlgetarg() != NIL) ? TRUE : FALSE);
  xllastarg();
  
  return((is_cuing(object)) ? s_true : NIL);
}

static is_cuing(object)
	LVAL object;
{
  return((slot_value(object, s_depth_cuing) != NIL) ? TRUE : FALSE);
}

static set_cuing(object, cuing)
	LVAL object;
	int cuing;
{
  set_slot_value(object, s_depth_cuing, (cuing) ? s_true : NIL);    
  if (cuing) adjust_cuing(object);
  else cuing_off(object);
}

LVAL iview_spin_showing_axes()
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs())
    set_showing_axes(object, (xlgetarg() != NIL) ? TRUE : FALSE);
  xllastarg();
  
  return((is_showing_axes(object)) ? s_true : NIL);
}

static is_showing_axes(object)
	LVAL object;
{
  return((slot_value(object, s_showing_axes) != NIL) ? TRUE : FALSE);
}

static set_showing_axes(object, showing)
	LVAL object;
	int showing;
{
  set_slot_value(object, s_showing_axes, (showing) ? s_true : NIL);    
}

static rotation_type(object)
	LVAL object;
{
  LVAL value = slot_value(object, s_rotation_type);

  if (symbolp(value)) {
    if (value == s_pitching) return(Pitching);
    else if (value == s_yawing) return(Yawing);
    else return(Rolling);
  }
  else if (matrixp(value)) return(ApplyMatrix);
  else return(Pitching);
}

static set_rotation_type(object, type)
	LVAL object;
	int type;
{
  LVAL value;

  switch (type) {
  case Pitching: value = s_pitching; break;
  case Rolling:  value = s_rolling;  break;
  case Yawing:   value = s_yawing;   break;
  default:       value = s_pitching; break;
  }
  set_slot_value(object, s_rotation_type, value);
}

static set_angle(object, alpha)
	LVAL object;
	double alpha;
{
  set_slot_value(object, s_rotation_angle, cvflonum((FLOTYPE) alpha));
}

static double spin_angle(object)
	LVAL object;
{
  LVAL value = slot_value(object, s_rotation_angle);
  
  if (floatp(value)) return(getflonum(value));
  else return(0.0);
}

LVAL iview_spin_angle()
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) set_angle(object, makedouble(xlgetarg()));
  xllastarg();
  
  return(slot_value(object, s_rotation_angle));
}

#ifdef DODO
static get_spin_controls(w, p1, p2, r1, r2, y1, y2)
	IVIEW_WINDOW w;
	Rect *p1, *p2, *r1, *r2, *y1, *y2;
{
  int height, top, string_space;
  char *gwinfo = IViewWindowWinInfo(w);
  
  height = StGWCanvasHeight(gwinfo);
  string_space = StGWTextWidth(gwinfo, SPIN_PITCH_STRING)
               + SPIN_CONTROL_GAP + 2 * SPIN_CONTROL_SIZE;
               
  top = height - SPIN_CONTROL_HEIGHT + SPIN_CONTROL_GAP;
  setcontrols(p1, p2, SPIN_CONTROL_SIZE, top);
  setcontrols(r1, r2, p2->left + string_space, top);
  setcontrols(y1, y2, r2->left + string_space, top);
}

static setcontrols(r1, r2, x, y)
	Rect *r1, *r2;
	int x, y;
{
  r1->left = x; r1->top = y;
  r1->width = SPIN_CONTROL_SIZE; r1->height = SPIN_CONTROL_SIZE;
  *r2 = *r1;
  r2->left += SPIN_CONTROL_SIZE + SPIN_CONTROL_GAP;
}
#endif DODO

/**************************************************************************/
/**                                                                      **/
/**                            Data Functions                            **/
/**                                                                      **/
/**************************************************************************/

/**************************************************************************/
/**                                                                      **/
/**                    Drawing and Resizing Functions                    **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_resize()
{
  IVIEW_WINDOW w;
  LVAL object;
  int vars, i, top, left, width, height, size;
  char *gwinfo;
  object = xlgaobject();
  xllastarg();
  
  gwinfo = StGWObWinInfo(object);
  w = GETIVIEWADDRESS(object);
  if (w == nil || gwinfo == nil) return(NIL);
  
  vars = IViewNumVariables(w);
  IViewSetFixedAspect(w, TRUE);
  IViewStdResize(w);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  size = width;
  StGrSetContentOrigin(gwinfo, left + size / 2, top + size / 2);
    
  for (i = 0; i < vars; i++)
    IViewSetScreenRange(w, i, - size / 2, size / 2);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);

  return(NIL);
}

static redraw_content(w, object)
	IVIEW_WINDOW w;
	LVAL object;
{
  int left, top, width, height;
  char *gwinfo = StGWObWinInfo(object);
  
  if (is_cuing(object)) adjust_cuing(object);
  StGWStartBuffering(gwinfo);
  IViewStdRedrawContent(w);
  if (is_showing_axes(object)) send_message(object, sk_draw_axes);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGWBufferToScreen(gwinfo, left, top, width, height);
}

LVAL iview_spin_draw_axes()
{
  IVIEW_WINDOW w;
  int x, y, xorig, yorig, size, xend, yend, vars, i, unit;
  char *s;
  double **a;
  char *gwinfo;
  LVAL object;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  if (w != nil) {
    StGrGetContentVariables(gwinfo, &x, &y);
    StGrGetContentOrigin(gwinfo, &xorig, &yorig);
    StGrGetContentRect(gwinfo, nil, nil, &size, nil);
    vars = IViewNumVariables(w);
    a = IViewTransformation(w);

    unit = size / 2;
    for (i = 0; i < vars; i++) {
      xend = xorig + a[x][i] * AXIS_FRACTION * unit;
      yend = yorig - a[y][i] * AXIS_FRACTION * unit;
      StGWDrawLine(gwinfo, xorig, yorig, xend, yend);
      s = IViewVariableLabel(w, i);
      xend += LABEL_OFFSET;
      yend -= LABEL_OFFSET;
      StGWDrawString(gwinfo, s, xend, yend);
    }
  }
  return(NIL);
}

LVAL iview_spin_redraw_content()
{
  IVIEW_WINDOW w;
  LVAL object;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  xllastarg();
  
  if (w != nil) redraw_content(w, object);
  return(NIL);
}

#ifdef DODO
static drawcontrols(gwinfo, r1, r2, s)
	char *gwinfo;
	Rect r1, r2;
	char *s;
{
  int x, y;
  
  x = r2.left + SPIN_CONTROL_GAP + SPIN_CONTROL_SIZE;
  y = r2.top + SPIN_CONTROL_SIZE;
  StGWFrameRect(gwinfo, r1.left, r1.top, r1.width, r1.height);
  StGWFrameRect(gwinfo, r2.left, r2.top, r2.width, r2.height);
  StGWDrawString(gwinfo, s, x, y);
}
#endif DODO

/**************************************************************************/
/**                                                                      **/
/**                           Mouse Functions                            **/
/**                                                                      **/
/**************************************************************************/

static adjust_cuing(object)
	LVAL object;
{
  int vx, vy, vz;
  
  get_content_variables(object, &vx, &vy, &vz);
  send_message1(object, sk_adjust_depth_cuing, vz);
}

static cuing_off(object)
	LVAL object;
{
  IVIEW_WINDOW w;
  int n, i;

  w = GETIVIEWADDRESS(object);
  if (w == nil) return;

  n  = IViewNumPoints(w);
  for (i = 0; i < n; i++) IViewSetPointSymbol(w, i, 0, 5);
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) IViewSetLineWidth(w, i, 1);
} 

/**************************************************************************/
/**                                                                      **/
/**                         Rotation Functions                           **/
/**                                                                      **/
/**************************************************************************/

static IViewSpinRotate(w)
	IVIEW_WINDOW w;
{
  int x, y, z;
  LVAL object = (LVAL) IViewWindowGetObject(w);
  double alpha = spin_angle(object);
  
  get_content_variables(object, &x, &y, &z);
  
  switch (rotation_type(object)) {
  case Pitching:    IViewRotate2(w, y, z, alpha); break;
  case Rolling:     IViewRotate2(w, x, y, alpha); break;
  case Yawing:      IViewRotate2(w, x, z, alpha); break;
  case ApplyMatrix: send_message_1L(object, 
                                    sk_apply_transformation,
                                    slot_value(object, s_rotation_type));
  }
  IViewRedrawContent(w);
}

LVAL iview_spin_rotate()
{
  IVIEW_WINDOW w;
  
  w = GETIVIEWADDRESS(xlgaobject());
  xllastarg();
  
  if (w != nil) IViewSpinRotate(w);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                        General Definitions                           **/
/**                                                                      **/
/**************************************************************************/

#define InitialTop (20 + MBarHeight)
#define InitialLeft 10
#define MOUSE_TOLERANCE 0

typedef struct {
  long Object;                                /* elements of window_data */
  int idleOn, frontOnly;                      /* elements of window_data */
  int mouse_x, mouse_y;                       /* elements of window_data */
  WindowPtr window;
  int (*FreeMem)();
  ColorCode backColor, drawColor;
  int canvasWidth, canvasHeight;
  int lineType, drawMode, lineWidth;
  long RefCon;
  int use_color;
  int hasHscroll, hasVscroll, view_h, view_v;
  int v_scroll_inc[2], h_scroll_inc[2];
  ControlHandle hscroll, vscroll;
  int initialized;
  int symbolMode;
  int cursor;
  Rect clip_rect;
  int clipped;
} StGWWinInfo;

extern Rect scroll_bar_bounds();

#define GetWRefCon(w) (((WindowPeek)(w))->refCon)

#define BAG_RES    1800
#define GLASS_RES  1801
#define CAN_RES    1802
#define BRUSH_RES  1803
#define HAND_RES   1804
#define FINGER_RES 1805

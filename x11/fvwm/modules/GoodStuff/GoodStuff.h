/*************************************************************************
 *
 * Subroutine Prototypes
 * 
 *************************************************************************/
extern void   CreateWindow(void);
extern Pixel  GetColor(char *name);
extern Pixel  GetHilite(Pixel background);
extern Pixel  GetShadow(Pixel background);
extern void   nocolor(char *a, char *b);
extern void   RedrawWindow(int);
extern void   match_string(char *tline);
extern void   Loop(void);
extern void   ParseOptions(char *);
extern char   *safemalloc(int length);
extern void   change_window_name(char *str);
extern int    My_XNextEvent(Display *dpy, XEvent *event);
extern void   CopyString(char **dest, char *source);
extern FVWM_INLINE void RelieveWindow(Window win,int x,int y,int w,int h,GC rGC,GC sGC);
extern void   SendInfo(int *,int);
extern void   SendText(int *,char *text, unsigned long window);
extern void   DeadPipe(int nonsense);
extern void   LoadIconFile(int button);
extern void   CreateIconWindow(int button);
extern void   ConfigureIconWindow(int button,int row, int column);
extern void   DrawIconWindow(int button);
extern char   *findIconFile(char *icon, char *pathlist);
extern void   GetBitmapFile(int button);
extern void   GetXPMFile(int button);
void process_message(unsigned long type,unsigned long *body);
void send_clientmessage (Window w, Atom a, Time timestamp);
void swallow(unsigned long *body);

extern Display *dpy;			/* which display are we talking to */
extern Window Root;
extern Window main_win;
extern int screen;
extern int d_depth;
extern Pixel back_pix, fore_pix;
extern GC  NormalGC;
extern GC  ReliefGC;
extern int ButtonWidth,ButtonHeight;
extern XFontStruct *font;
#define MAX_BUTTONS 100

struct button_info
{
  char *action;
  char *title;
  char *icon_file;
  int icon_w;
  int icon_h;
  Pixmap iconPixmap;		/* pixmap for the icon */
  Pixmap icon_maskPixmap;	/* pixmap for the icon mask */
  Window IconWin;
  int icon_depth;
  char *hangon;
  char up;
  char swallow;
};

extern struct button_info Buttons[MAX_BUTTONS];

extern char *iconPath;
extern char *pixmapPath;

#ifdef BROKEN_SUN_HEADERS
#include "sun_headers.h"
#endif

#ifdef NEEDS_ALPHA_HEADER
#include "alpha_header.h"
#endif /* NEEDS_ALPHA_HEADER */


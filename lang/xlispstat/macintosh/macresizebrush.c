#define _IVIEWWINDOW_
#include "iview.h"

#ifdef MPWC
#include <OSUtils.h>
#include <TextEdit.h>
#include <OSEvents.h>
#define TRUE 1
#define FALSE 0
#define gray qd.gray
#endif MPWC

#define ResizeBoxHeight 10
#define ResizeBoxLeft 95
#define ResizeBoxTop 50
    
static Rect ResizeMessageRect;
static Rect ResizeBox = { ResizeBoxTop, ResizeBoxLeft,  
          				  ResizeBoxTop + ResizeBoxHeight,
          				  ResizeBoxLeft + ResizeBoxHeight},
          	windowRect = { 50, 150, 200, 350},
          	OKRect = { 120, 10, 140, 90 },
          	CancelRect = { 120, 110, 140, 190 };

static char ResizeMessage[] = "To resize brush click in this window and drag";

static Point mouseLoc;
static Rect oldBrush, brush;

static ControlHandle OKButton, CancelButton;
static WindowPtr wind;
static int cancelled;

static Boolean DoContent();

IViewGetNewBrushSize(w, new_width, new_height)
	IVIEW_WINDOW w;
	int *new_width, *new_height;
{
  int left, top, width, height;
  
  IViewGetBrush(w, &left, &top, &width, &height);
  left -= width; top -= height;
  
  SetRect(&oldBrush, left, top, left + width, top + height);
  brush = oldBrush;

  setup_window();
  SetPt(&mouseLoc, brush.right, brush.bottom);
  DrawBrush();
  event_loop();
  DisposeWindow(wind);
  if (new_width != nil) *new_width = brush.right - brush.left;
  if (new_height != nil) *new_height = brush.bottom - brush.top;
  return(! cancelled);
}

static setup_window()
{
  wind = NewWindow(nil,&windowRect,"\p", true, 
                  dBoxProc, (WindowPtr) -1, true, 0L);
						  
  OKButton = NewControl(wind,&OKRect,"\pOK", true, 0, 0, 0, 0, 0L);

  CancelButton = NewControl(wind,&CancelRect,"\pCancel", true, 0, 0, 0, 0, 0L);

  SetPort(wind);

  ResizeMessageRect = ResizeBox;
  InsetRect(&ResizeMessageRect, -50, -20);
  OffsetRect(&ResizeMessageRect, 0, -30);

  TextBox(ResizeMessage, (long) strlen(ResizeMessage), 
          &ResizeMessageRect, 1);
/*  FrameRect(&ResizeBox);*/
}

static event_loop()
{
  EventRecord myEvent;
  WindowPtr whichWindow;
  Boolean done = false;
  
  cancelled = FALSE;
  FlushEvents( everyEvent, 0 );
  do {
    if (GetNextEvent(everyEvent, &myEvent)) {
      switch (myEvent.what) {
      case mouseDown:
        switch (FindWindow( myEvent.where, &whichWindow )) {
        case inContent:
          if (whichWindow == wind) done = DoContent(&myEvent);
          else SysBeep(10);
          break;
        default:
          SysBeep(10);
          break;
        } /* end switch FindWindow */
        break;
      default:; 
      } /* end switch myEvent.what) */
    }
    MoveBrush();
  } while (! done);
  FlushEvents( everyEvent, 0 );
}

static Boolean DoContent(theEvent)
	EventRecord	*theEvent;
{
  int cntlCode;
  ControlHandle theControl;

  GlobalToLocal( &(*theEvent).where );
  switch (cntlCode = FindControl((*theEvent).where, wind, &theControl)) {
  case inButton:
    DrawBrush(); /* to erase */
    if (theControl==OKButton) 
      if (TrackControl(theControl, (*theEvent).where, nil)) return(true);
    if (theControl==CancelButton) 
      if (TrackControl(theControl, (*theEvent).where, nil)) {
        brush = oldBrush;
        cancelled = TRUE;
        return(true);
      }
    DrawBrush(); /* to redraw */
    break;
  default:
/*    if (PtInRect((*theEvent).where, &ResizeBox)) */
      ResizeBrush((*theEvent).where);
  }
  return(false);
}

static ResizeBrush(p)
	Point p;
{
	Point BottomLeft, lastBottomLeft;

    BottomLeft = p;
	DrawBrush();  				/* to erase */
	Pt2Rect(p, BottomLeft, &brush);
	mouseLoc = BottomLeft;
	DrawBrush();  				/* to redraw */
	while (Button()) {
      		lastBottomLeft = BottomLeft;
		GetMouse(&BottomLeft);
		if (! EqualPt(lastBottomLeft, BottomLeft)) {
        	DrawBrush();  		/* to erase */
        	Pt2Rect(p, BottomLeft, &brush);
			mouseLoc = BottomLeft;
			DrawBrush();  		/* to redraw */
		}
	}
}

static DrawBrush()
{
  PenMode(patXor);
  PenPat(gray);
  FrameRect(&brush);  
  PenNormal();
}

static MoveBrush()
{
  Point oldMouseLoc;

  oldMouseLoc = mouseLoc;
  GetMouse(&mouseLoc);
  if ((oldMouseLoc.h != mouseLoc.h) || (oldMouseLoc.v != mouseLoc.v)) {
    DrawBrush(); 						/* to erase */
    OffsetRect(&brush, mouseLoc.h - oldMouseLoc.h,
               mouseLoc.v - oldMouseLoc.v);
    DrawBrush(); 						/* to redraw */
  }
}

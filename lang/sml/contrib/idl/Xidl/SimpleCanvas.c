/*

	Simple canvas widget

	@(#)SimpleCanvas.c	2.1 93/03/07 00:58:08
*/

#include <X11/copyright.h>
#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/Xaw/XawInit.h>
#include <X11/StringDefs.h>
#include "SimpleCanvasP.h"

#define CHECKCLASS(w,s)\
	if (w!=NULL && XtClass(w)!=simplecanvasWidgetClass)\
	{  fprintf(stderr, "[SimpleCanvas.%s called on widget %s(0x%x) of class %s]\n",\
			   s, w->core.name, w, XtClass(w)->core_class.class_name);\
	   exit(200);\
	}

#define PUBLIC
#define PRIVATE static
PRIVATE XtResource resources[] = {
#define offset(field) XtOffset(SimpleCanvasWidget, simplecanvas.field)
#define FONT(n)\
	{"font#n", XtCFont, XtRFontStruct, sizeof(XFontStruct *),\
		  offset(fontinfo[n]), XtRString, XtDefaultFont},
   /* {name, class, type, size, offset, default_type, default_addr}, */
   {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
		   offset(foreground), XtRString, XtDefaultForeground},
   {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	     offset(fontinfo[0]), XtRString, XtDefaultFont},
   FONT(0)
   FONT(1)
   FONT(2)
   FONT(3)
   FONT(4)
   FONT(5)
   FONT(6)
   FONT(7)
   FONT(8)
   FONT(9)
   {XtNexposeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		       offset(expose_callback), XtRCallback, NULL},
   {XtNoffsetCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		       offset(offset_callback), XtRCallback, NULL},
   {XtNextentCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		       offset(extent_callback), XtRCallback, NULL},
#undef offset
};

       void SimpleCanvasRedraw  ( /* Widget */ );
       void SimpleCanvasClear   ( /* Widget */ );
       void SimpleCanvasDestroy ( /* Widget */ );
       void SimpleCanvasInit    ( /* Widget , Widget */ );
       void SimpleCanvasSetOffset    ( /* Widget , Position, Position */ );

PRIVATE
void    SimpleCanvasStartDrag(w, event, params, paramc)
	SimpleCanvasWidget       w;
	XButtonEvent *event;
	String       *params;
	Cardinal     *paramc;
{       if (!XtIsRealized(w)) return;
	CHECKCLASS(w, "StartDrag");
	{
	   w->simplecanvas.dragging = True;
	   w->simplecanvas.dragx = event->x;
	   w->simplecanvas.dragy = event->y;
	}
}

/*
	This (expensive) operation implements CONTINUOUS dragging of
	the canvas. On fast machines it works well; on slow machines it
	should be dispensed with -- StartDrag and EndDrag provide
	``jump'' dragging when used without this.
*/
PRIVATE
void    SimpleCanvasDrag(w, event, params, paramc)
	SimpleCanvasWidget       w;
	XMotionEvent *event;
	String       *params;
	Cardinal     *paramc;
{       if (!XtIsRealized(w)) return;
	CHECKCLASS(w, "Drag");
	if (w->simplecanvas.dragging)
	{
	  SimpleCanvasSetOffset(w, w->simplecanvas.offsetx - event->x +
				   w->simplecanvas.dragx,
				   w->simplecanvas.offsety - event->y +
				   w->simplecanvas.dragy);
	}
	w->simplecanvas.dragging = True;
	w->simplecanvas.dragx    = event->x;
	w->simplecanvas.dragy    = event->y;
}

PRIVATE
void    SimpleCanvasEndDrag(w, event, params, paramc)
	SimpleCanvasWidget       w;
	XButtonEvent *event;
	String       *params;
	Cardinal     *paramc;
{       if (!XtIsRealized(w)) return;
	CHECKCLASS(w, "EndDrag");
	if (w->simplecanvas.dragging)
	{
	  SimpleCanvasSetOffset(w, w->simplecanvas.offsetx - event->x +
				   w->simplecanvas.dragx,
				   w->simplecanvas.offsety - event->y +
				   w->simplecanvas.dragy);

	}
	w->simplecanvas.dragging = False;
}


PRIVATE XtActionsRec actions[] =
{
   {"SimpleCanvasRedraw",       SimpleCanvasRedraw},
   {"SimpleCanvasClear",        SimpleCanvasClear},
   {"SimpleCanvasStartDrag",    SimpleCanvasStartDrag},
   {"SimpleCanvasDrag",         SimpleCanvasDrag},
   {"SimpleCanvasEndDrag",      SimpleCanvasEndDrag},
};

PRIVATE char translations[] =
	"<BtnDown>:     SimpleCanvasStartDrag() \n\
	 <BtnMotion>:   SimpleCanvasDrag()      \n\
	 <BtnUp>:       SimpleCanvasEndDrag()   \n\
	";

PRIVATE void
simplecanvasExpose(w, event, region)
   Widget  w;
   XEvent *event;
   Region  region;
{  if (!XtIsRealized(w)) return;
   CHECKCLASS(w, "Expose");
   XtCallCallbacks(w, XtNexposeCallback, (caddr_t) region);
   SimpleCanvasRedraw((SimpleCanvasWidget)w);
}

SimpleCanvasClassRec simplecanvasClassRec = {
   {   /* core fields            */
       /* superclass             */ (WidgetClass) & widgetClassRec,
       /* class_name             */ "SimpleCanvas",
       /* widget_size            */ sizeof(SimpleCanvasRec),
       /* class_initialize       */ XawInitializeWidgetSet,
       /* class_part_initialize  */ NULL,
       /* class_inited           */ FALSE,
       /* initialize             */ SimpleCanvasInit,
       /* initialize_hook        */ NULL,
       /* realize                */ XtInheritRealize,
       /* actions                */ actions,
       /* num_actions            */ XtNumber(actions),
       /* resources              */ resources,
       /* num_resources          */ XtNumber(resources),
       /* xrm_class              */ NULLQUARK,
       /* compress_motion        */ TRUE,
       /* compress_exposure      */ TRUE,
       /* compress_interleave    */ TRUE,
       /* visible_interest       */ FALSE,
       /* destroy                */ SimpleCanvasDestroy,
       /* resize                 */ XtInheritResize,
       /* expose                 */ simplecanvasExpose,
       /* set_values             */ NULL,
       /* set_values_hook        */ NULL,
       /* set_values_almost      */ XtInheritSetValuesAlmost,
       /* get_values_hook        */ NULL,
       /* accept_focus           */ NULL,
       /* version                */ XtVersion,
       /* callback_private       */ NULL,
       /* tm_table               */ translations,
       /* query_geometry         */ XtInheritQueryGeometry,
       /* display_accelerator    */ XtInheritDisplayAccelerator,
       /* extension              */ NULL
   },
   {   /* simplecanvas fields    */
       /* empty                  */ 0
   }
};


WidgetClass simplecanvasWidgetClass = (WidgetClass) & simplecanvasClassRec;


/*
	Drawing on the Canvas

*/
PRIVATE void BoundingBox(r, xa,ya, wa,ha)
	Position  *xa,*ya;
	Dimension *wa,*ha;
	Thing     *r;
{       Position x, y;
	int      w, h;
	switch (r->type)
	{
	    case LineType:
		  x=r->obj.line.startx;
		  y=r->obj.line.starty;
		  w=r->obj.line.endx-x;
		  h=r->obj.line.endy-y;
	    break;
	    case BoxType:
		 x=r->obj.box.startx;
		 y=r->obj.box.starty;
		 w=r->obj.box.width;
		 h=r->obj.box.height;
	    break;
	    case StringType:
		 x=r->obj.string.startx;
		 y=r->obj.string.starty;
		 w=r->obj.string.width;
		 h=r->obj.string.ascent+r->obj.string.descent;
	    break;

	    default: x = y = w = h = 0; break;
	}
	if (w>=0) { *xa=x; *wa=w; } else { *xa=x+w; *wa= -w; }
	if (h>=0) { *ya=y; *ha=h; } else { *ya=y+h; *ha= -h; }
}

PRIVATE
Thing*  newLine(w, class, startx, starty, endx, endy)
	SimpleCanvasWidget  w;
	byte                class;
	Position            startx, starty, endx, endy;
{       Thing *r=XtNew(Thing);
	r->type = LineType;
	r->class= class;
	r->obj.line.startx=startx;
	r->obj.line.starty=starty;
	r->obj.line.endx=endx;
	r->obj.line.endy=endy;
	r->next = w->simplecanvas.everything;
	w->simplecanvas.everything = r;
	return r;
}

PRIVATE
Thing   *newBox(w, class, startx, starty, width, height)
	SimpleCanvasWidget  w;
	byte                class;
	Position            startx, starty;
	Dimension           width,  height;
{       Thing *r=XtNew(Thing);
	r->type = BoxType;
	r->class= class;
	r->obj.box.startx=startx;
	r->obj.box.starty=starty;
	r->obj.box.width=width;
	r->obj.box.height=height;
	r->next = w->simplecanvas.everything;
	w->simplecanvas.everything = r;
	return r;
}

PRIVATE
Thing*  newString(w, class, fontnumber, string, length, startx, starty,
		     width, ascent, descent)
	SimpleCanvasWidget  w;
	byte                class;
	int                 fontnumber, length;
	Position            startx, starty;
	Dimension           width, ascent, descent;
	String              string;
{       Thing *r=XtNew(Thing);
	r->type = StringType;
	r->class= class;
	r->obj.string.string=XtNewString(string);
	r->obj.string.fontnumber=fontnumber&0xFF;
	r->obj.string.startx=startx;
	r->obj.string.starty=starty;
	r->obj.string.width=width;
	r->obj.string.length=length;
	r->obj.string.ascent=ascent;
	r->obj.string.descent=descent;
	r->next = w->simplecanvas.everything;
	w->simplecanvas.everything = r;
	return r;
}

#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)

PRIVATE
void    DrawThing(w, thing)
	SimpleCanvasWidget w;
	Thing              *thing;
{       Dimension offsetx = w->simplecanvas.offsetx,
		  offsety = w->simplecanvas.offsety;
	GC             gc = w->simplecanvas.gc[thing->class];
	Position  sx, sy, ex, ey;
	if (gc==NULL)
	{
	   fprintf(stderr, "[Warning: DrawThing(canvas=0x%x, type=%s) GC == NULL]\n",
			   w,
			   thing->type==LineType?"Line":
			   thing->type==BoxType?"Box"  :
			   thing->type==StringType?"String":"Unknown type");
	   return;
	}
	if (!XtIsRealized(w))
	{  fprintf(stderr, "[Warning: DrawThing(canvas=0x%x, type=%s) unrealized canvas]\n",
			   w,
			   thing->type==LineType?"Line":
			   thing->type==BoxType?"Box"  :
			   thing->type==StringType?"String":"Unknown type");
	   return;
	}
	switch (thing->type)
	{
	   case LineType:
	       sx=thing->obj.line.startx-offsetx;
	       sy=thing->obj.line.starty-offsety;
	       ex=thing->obj.line.endx-offsetx;
	       ey=thing->obj.line.endy-offsety;
	       XDrawLine(XtDisplay(w), XtWindow(w), gc, sx, sy, ex, ey);
	   break;

	   case BoxType:
	       sx=thing->obj.box.startx-offsetx;
	       sy=thing->obj.box.starty-offsety;
	       ex=sx+thing->obj.box.width-offsetx;
	       ey=sy+thing->obj.box.height-offsety;
	       XDrawRectangle(XtDisplay(w), XtWindow(w), gc,
			       sx,
			       sy,
			       thing->obj.box.width,
			       thing->obj.box.height);

	   break;
	   case StringType:
	   {    XTextItem text;
		text.chars =thing->obj.string.string;
		text.nchars=thing->obj.string.length;
		text.delta = 0;
		text.font  =
		 w->simplecanvas.fontinfo[thing->obj.string.fontnumber]->fid;

		sx=thing->obj.string.startx-offsetx;
		sy=thing->obj.string.starty-offsety;
		ex=sx+thing->obj.string.width;
		ey=sy+thing->obj.string.ascent+thing->obj.string.descent;
		XDrawText
		(   XtDisplay(w), XtWindow(w), gc,
		    sx,
		    thing->obj.string.starty+thing->obj.string.ascent-offsety,
		    &text, 1
		);
	   }
	   break;
	 }
}

PRIVATE
void   Extent(w, thing)
       SimpleCanvasWidget w;
       Thing              *thing;
/*     recalculate the extent of the diagram
       call the extent change callback if necessary
*/
{      Position  x,y, xx, yy;
       Position  tlx=w->simplecanvas.topleftx,
		 tly=w->simplecanvas.toplefty,
		 brx=w->simplecanvas.botrightx,
		 bry=w->simplecanvas.botrighty;
       Dimension ww,  hh;
       BoundingBox(thing, &x, &y, &ww, &hh);
       xx = x + ww;
       yy = y + hh;
       w->simplecanvas.topleftx   = min(tlx, x);
       w->simplecanvas.toplefty   = min(tly, y);
       w->simplecanvas.botrightx  = max(brx, xx);
       w->simplecanvas.botrighty  = max(bry, yy);
       if (!(  tlx==w->simplecanvas.topleftx
	   &&  tly==w->simplecanvas.toplefty
	   &&  brx==w->simplecanvas.botrightx
	   &&  bry==w->simplecanvas.botrighty))
       {  XtCallCallbacks(w, XtNextentCallback, NULL);
       }
}

PRIVATE
Thing*  DrawNewThing(w, thing)
	SimpleCanvasWidget w;
	Thing              *thing;
{
	DrawThing(w, thing);
	Extent(w, thing);
	return thing;
}

PUBLIC  Thing*  SimpleCanvasDrawLine(c, class, x, y, w, h)
	SimpleCanvasWidget c;
	byte               class;
	Position           x,y;
	Dimension          w,h;
{
	CHECKCLASS(c, "DrawLine");
	return DrawNewThing(c, newLine(c, class, x, y, x+w, y+h));
}

PUBLIC  Thing*  SimpleCanvasDrawBox(c, class, x, y, w, h)
	SimpleCanvasWidget c;
	byte               class;
	Position           x,y;
	Dimension          w,h;
{
	CHECKCLASS(c, "DrawBox");
	return DrawNewThing(c, newBox(c, class, x, y, w, h));
}


PUBLIC  Thing*  SimpleCanvasDrawString(c, class, font, string, x, y, w)
	SimpleCanvasWidget c;
	byte               class;
	byte               font;
	String             string;
	Position           x,y;
	Dimension          *w;
{
	int             len = strlen(string);
	XCharStruct     box;
	int             dir, asc, desc;

	font &= 0xFF;
	CHECKCLASS(c, "DrawString");
	XTextExtents(c->simplecanvas.fontinfo[font],
		     string, len, &dir, &asc, &desc, &box);
	{ Thing* thing = DrawNewThing
			 (c, newString(c, class, font, string, len, x, y,
			     box.width, asc, desc));
	  if (w!=NULL) *w=box.width;
	  return thing;
	}
}

PRIVATE
void    emboxthing(w, s) SimpleCanvasWidget w; Thing *s;
{  Position  x, y;
   Dimension wd, ht;
   GC gc = w->simplecanvas.gc[0];
   if (!XtIsRealized(w)) return;
   BoundingBox(s, &x, &y, &wd, &ht);
   XDrawRectangle(XtDisplay(w), XtWindow(w), gc,
		  x-w->simplecanvas.offsetx-1,
		  y-w->simplecanvas.offsety-1,
		  wd+1,
		  ht+1);
}

PRIVATE
void    unboxthing(w, s) SimpleCanvasWidget w; Thing *s;
{  Position  x, y;
   Dimension wd, ht;
   GC gc = w->simplecanvas.gc[255];
   if (s==NULL) return;
   if (!XtIsRealized(w)) return;
   BoundingBox(s, &x, &y, &wd, &ht);
   XDrawRectangle(XtDisplay(w), XtWindow(w), gc,
		  x-w->simplecanvas.offsetx-1,
		  y-w->simplecanvas.offsety-1,
		  wd+1,
		  ht+1);
}

PRIVATE
Bool    withinselectable(x, y, r)
	Position x, y;
	Thing *r;
{       if (r->class==0) return False;
	switch (r->type)
	{
		case StringType:
		{       Position xx, yy; Dimension ww, hh;
			BoundingBox(r, &xx, &yy, &ww, &hh);
			return xx <= x && x <= xx+ww &&
			       yy <= y && y <= yy+hh;
		}
		break;
	}
	return False;
}

PUBLIC  Thing*  SimpleCanvasFindSelectable(w, x, y)
	SimpleCanvasWidget w;
	Position x, y;
{       Thing * r;
	for (r=w->simplecanvas.everything;
	     r!=NULL && !withinselectable(x, y, r);
	     r=r->next
	    ) {};
	return r;
}

PUBLIC  Thing*  SimpleCanvasFindAndSelect(w, x, y)
	SimpleCanvasWidget w;
	Position x,y;
  {     CHECKCLASS(w, "FindAndSelect");
     {
	Thing *s = SimpleCanvasFindSelectable(w, x+w->simplecanvas.offsetx,
						 y+w->simplecanvas.offsety);
	if (s!=NULL)
	{  unboxthing(w, w->simplecanvas.selected[s->class]);
	   emboxthing(w, s);
	   w->simplecanvas.selected[s->class] = s;
	}
	return s;
     }
  }

PUBLIC  void    SimpleCanvasChangeClass(w, thing, c)
	SimpleCanvasWidget w;
	Thing              *thing;
	byte               c;
{       CHECKCLASS(w, "ChangeClass");
	thing->class=c;
	DrawThing(w, thing);
}

PUBLIC  byte    SimpleCanvasGetClass(w, thing)
	SimpleCanvasWidget w;
	Thing              *thing;
{       CHECKCLASS(w, "GetClass");
	return thing->class;
}

PUBLIC  Thing*  SimpleCanvasGetSelection(w, class)
	SimpleCanvasWidget w;
	byte class;
{       CHECKCLASS(w, "GetSelection");
	return w->simplecanvas.selected[class];
}

PUBLIC  void    SimpleCanvasRedraw(w) SimpleCanvasWidget w;
{       Thing *thing;
	CHECKCLASS(w, "Redraw");
	if (!XtIsRealized(w)) return;
	XClearWindow(XtDisplay(w), XtWindow(w));
	for (thing=w->simplecanvas.everything; thing!=NULL; thing=thing->next)
	    DrawThing(w, thing);
	/*
	    Embox the selected items
	*/
	{ int i;
	  for (i=0; i<256; i++)
	  {   Thing *s = w->simplecanvas.selected[i];
	      if (s!=NULL) emboxthing(w, s);
	  }
	}
}

PUBLIC  void    SimpleCanvasClear(w)
	SimpleCanvasWidget w;
{       Thing   *next, *thing;
	CHECKCLASS(w, "Clear");
	for(thing=w->simplecanvas.everything; thing!=NULL; thing=next)
	{  next=thing->next;
	   switch (thing->type)
	   {
	      case StringType: XtFree(thing->obj.string.string);
	   }
	   XtFree(thing);
	}
	/*
	   Clear the display list
	*/
	w->simplecanvas.everything=NULL;
	w->simplecanvas.offsetx=0;
	w->simplecanvas.offsety=0;
	w->simplecanvas.topleftx   =
	w->simplecanvas.toplefty   =
	w->simplecanvas.botrightx  =
	w->simplecanvas.botrighty  = 0;
	XClearWindow(XtDisplay(w), XtWindow(w));
	/*
	   Clear the selections
	*/
	{ int i; for (i=0; i<256; i++) w->simplecanvas.selected[i]=NULL; }
}

PRIVATE
void    SimpleCanvasInit(req, new) SimpleCanvasWidget req, new;
{       XtGCMask  mask =
		  GCForeground | GCBackground | GCLineStyle |
		  GCLineWidth  ;

	Screen    *ws     = XtScreen(new);

	Pixel     BLACK   = req->simplecanvas.foreground?
			    req->simplecanvas.foreground:
			    BlackPixelOfScreen(ws);
	Pixel     WHITE   = req->core.background_pixel?
			    req->core.background_pixel:
			    WhitePixelOfScreen(ws);

	Pixmap    STIPPLE = XmuCreateStippledPixmap(ws, BLACK, WHITE, 1);

	XGCValues values;

	Dimension width =req->core.width,
		  height=req->core.height;

	new->core.width  = width  == 0?20:width;
	new->core.height = height == 0?20:height;

	/*
		First zilch all the private properties
	*/


	{ int i;
	  for (i=0; i<256; i++)
	      { if (new->simplecanvas.fontinfo[i]==NULL)
		   new->simplecanvas.fontinfo[i]=
		   new->simplecanvas.fontinfo[0];
		   new->simplecanvas.gc[i]=0;
	      }
	  new->simplecanvas.everything = NULL;
	  new->simplecanvas.offsetx   =
	  new->simplecanvas.offsety   = 0;
	  new->simplecanvas.topleftx   =
	  new->simplecanvas.toplefty   =
	  new->simplecanvas.botrightx  =
	  new->simplecanvas.botrighty  = 0;
	}
	/* Selections */
	{ int i; for (i=0; i<256; i++) new->simplecanvas.selected[i]=NULL; }
	/*
		Make the initial set of drawing clases
		(this is really a bit arbitrary)

			0 -- positive unstippled -- used for boxing
			1 -- ditto
		      128 -- positive stippled   -- used for greying
		      255 -- inverse of 1        -- used for unboxing
	*/

	values.line_width = 0;
	values.line_style = LineSolid;
	values.background = WHITE;
	values.foreground = BLACK;
	new->simplecanvas.gc[1] =
	new->simplecanvas.gc[0] = XtGetGC(new, mask, values);

	values.background = BLACK;
	values.foreground = WHITE;
	new->simplecanvas.gc[255]  = XtGetGC(new, mask, values);

	values.background = WHITE;
	values.foreground = BLACK;
	values.stipple    = STIPPLE;
	values.fill_style = FillStippled;  /* For drawing greyed text */
	mask             |= GCStipple | GCFillStyle;
	new->simplecanvas.gc[128]  = XtGetGC(new, mask, values);
}

PRIVATE
void    SimpleCanvasDestroy(w) SimpleCanvasWidget w;
{       int i;
	CHECKCLASS(w, "Destroy");
	for (i=0; i<256; i++)
	    if (w->simplecanvas.gc[i]) XtReleaseGC(w, w->simplecanvas.gc[i]);
	SimpleCanvasClear(w);
}

PUBLIC  void    SimpleCanvasSetFont(w, fn, fs)
	SimpleCanvasWidget w;
	byte  fn;
	XFontStruct *fs;
{       CHECKCLASS(w, "SetFont");
	w->simplecanvas.fontinfo[fn]=fs;
}

PUBLIC  void    SimpleCanvasSetGC(w, gn, g)
	SimpleCanvasWidget w;
	byte gn;
	GC   g;
{       CHECKCLASS(w, "SetGC");
	w->simplecanvas.gc[gn]=g;
}

PUBLIC  XFontStruct *SimpleCanvasGetFont(w, font)
	SimpleCanvasWidget w;
	byte               font;
{       CHECKCLASS(w, "GetFont");
	return(w->simplecanvas.fontinfo[font]);
}

PUBLIC  GC      SimpleCanvasGetGC(w, gcid)
	SimpleCanvasWidget w;
	byte               gcid;
{       CHECKCLASS(w, "GetGC");
	return(w->simplecanvas.gc[gcid]);
}


PUBLIC  void    SimpleCanvasSetX(w, x)
	SimpleCanvasWidget w;
	Position x;
{       CHECKCLASS(w, "SetX");
	w->simplecanvas.offsetx=x;
	SimpleCanvasRedraw(w);
	XtCallCallbacks(w, XtNoffsetCallback, NULL);
}

PUBLIC  void    SimpleCanvasSetY(w, y)
	SimpleCanvasWidget w;
	Position y;
{       CHECKCLASS(w, "SetY");
	w->simplecanvas.offsety=y;
	SimpleCanvasRedraw(w);
	XtCallCallbacks(w, XtNoffsetCallback, NULL);
}

PUBLIC  void    SimpleCanvasSetOffset(w, x, y)
	SimpleCanvasWidget w;
	Position x, y;
{       CHECKCLASS(w, "SetOffset");
	w->simplecanvas.offsetx=x;
	w->simplecanvas.offsety=y;
	SimpleCanvasRedraw(w);
	XtCallCallbacks(w, XtNoffsetCallback, NULL);

}

PUBLIC  void    SimpleCanvasGetOffset(w, x, y)
	SimpleCanvasWidget w;
	Position *x, *y;
{       CHECKCLASS(w, "GetOffset");
	*x=w->simplecanvas.offsetx;
	*y=w->simplecanvas.offsety;
}

PUBLIC  void    SimpleCanvasGetExtent(c, x, y, w, h)
	SimpleCanvasWidget c;
	Position *x, *y;
	Dimension *w, *h;
{       CHECKCLASS(c, "GetExtent");
	*x=c->simplecanvas.topleftx;
	*y=c->simplecanvas.toplefty;
	*w=c->simplecanvas.botrightx-c->simplecanvas.topleftx;
	*h=c->simplecanvas.botrighty-c->simplecanvas.toplefty;
}



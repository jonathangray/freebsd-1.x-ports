/*==================================================================*/
/*                                                                  */
/* HyperWidget                                                      */
/*                                                                  */
/* B.Raoult (mab@ecmwf.co.uk)                              Oct.91   */
/*                                                                  */
/*                                                                  */
/* Hyper text like widget.                                          */
/*                                                                  */
/* (c)  B.Raoult 91                                                 */
/*==================================================================*/

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include <X11/cursorfont.h>
#include "Hyper.h"
#include "HyperP.h"


#define ABS(a)           ((a)>=0?(a):-(a))
#define MIN(a,b)         ((a)>(b)?(b):(a))


#define NORMAL           0
#define HIGHLIGHT        1
#define NEWLINE          2

#define MAX_LINE_SIZE    1024

/* 
  Private functions 
*/

static void    free_text();
static void    create_gcs();
static void    create_new_text();
/* static void    select(); */
static void    Select();    /* type clash from <sys/select.h> under SunOS 5 */
static void    cursor();
static void    activate();
static void    add_to_text ();
static void    calc_new_size ();
static void    zoom_open ();
static void    show_selection();

/*
  Widget class methods
*/

static void    Initialize();
static void    Redisplay();
static void    Resize();
static void    Destroy();
static Boolean SetValues();

static char defaultTranslations[] = 
"<Btn1Down>:select()\n<Btn1Up>: activate()\n<Motion>:cursor()";

static XtActionsRec actionsList[] = {
    { "select",   (XtActionProc) Select},
    { "activate", (XtActionProc) activate},
    { "cursor",   (XtActionProc) cursor},
};

static XtResource resources[] = {

    {XtNhighlightFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    XtOffset(HyperWidget, hyper.highlight_font), XtRString, "Fixed"},

    {XtNnormalFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    XtOffset(HyperWidget, hyper.normal_font), XtRString, "Fixed"},

    {XtNhighlightColor, XtCColor, XtRPixel, sizeof (Pixel),
    XtOffset(HyperWidget, hyper.highlight_color),XtRString, "Red"},

    {XtNselectColor, XtCColor, XtRPixel, sizeof (Pixel),
    XtOffset(HyperWidget, hyper.select_color),XtRString, "Blue"},

    {XtNnormalColor, XtCColor, XtRPixel, sizeof (Pixel),
    XtOffset(HyperWidget, hyper.normal_color),XtRString,"Black"},

    {XtNactivateCallback,XtCCallback,XtRCallback,sizeof(caddr_t),
    XtOffset (HyperWidget, hyper.activate),XtRCallback,NULL},

    {XtNhyperCallback,XtCCallback,XtRCallback,sizeof(caddr_t),
    XtOffset (HyperWidget, hyper.hyper_callbacks),XtRCallback,NULL},

    {XtNzoomEffect,XtCZoom,XtRBoolean,sizeof(Boolean),
    XtOffset (HyperWidget, hyper.zoom),XtRImmediate,(XtPointer)TRUE},

#ifndef XDESIGNER
    {XtNstartHighlight,XtCTagChar,XtRUnsignedChar,sizeof(unsigned char),
    XtOffset(HyperWidget,hyper.start_of_highlight),XtRImmediate,
    (XtPointer)'{'},

    {XtNendHighlight,XtCTagChar,XtRUnsignedChar,sizeof(unsigned char),
    XtOffset (HyperWidget, hyper.end_of_highlight),XtRImmediate,
    (XtPointer)'}'},
#endif

    {XtNzoomSpeed,XtCZoomSpeed,XtRInt,sizeof(int),
    XtOffset (HyperWidget, hyper.speed),XtRImmediate,(XtPointer)4},

    {XtNmargin,XtCMargin,XtRInt,sizeof(int),
    XtOffset (HyperWidget, hyper.margin),XtRImmediate,(XtPointer)10},
};

/*---------------------------------------------------------------*/
/* Static initialisation of the class record                     */
/*---------------------------------------------------------------*/

HyperClassRec  hyperClassRec = {
    {
#ifdef MOTIF
    (WidgetClass) &xmPrimitiveClassRec,  /* superclass            */
#else
    (WidgetClass) &widgetClassRec,       /* superclass            */
#endif
    "Hyper",                             /* class_name            */
    sizeof(HyperRec),                    /* widget_size           */
    NULL,                                /* class_initialize      */
    NULL,                                /* class_part_initialize */
    FALSE,                               /* class_inited          */
    Initialize,                          /* initialize            */
    NULL,                                /* initialize_hook       */
    XtInheritRealize,                    /* realize               */
    actionsList,                         /* actions               */
    XtNumber(actionsList),               /* num_actions           */
    resources,                           /* resources             */
    XtNumber(resources),                 /* num_resources         */
    NULLQUARK,                           /* xrm_class             */
    TRUE,                                /* compress_motion       */
    XtExposeCompressMultiple,            /* compress_exposure     */
    TRUE,                                /* compress_enterleave   */
    TRUE,                                /* visible_interest      */
    Destroy,                             /* destroy               */
    Resize,                              /* resize                */
    Redisplay,                           /* expose                */
    SetValues,                           /* set_values            */
    NULL,                                /* set_values_hook       */
    XtInheritSetValuesAlmost,            /* set_values_almost     */
    NULL,                                /* get_values_hook       */
    NULL,                                /* accept_focus          */
    XtVersion,                           /* version               */
    NULL,                                /* callback private      */
    defaultTranslations,                 /* tm_table              */
    NULL,                                /* query_geometry        */
    NULL,                                /* display_accelerator   */
    NULL,                                /* extension             */
    },
#ifdef MOTIF
    {
    (XtWidgetProc)_XtInherit,             /* border_highlight      */
    (XtWidgetProc)_XtInherit,             /* border_unhighligh     */
    XtInheritTranslations,                /* translations          */
    (XtActionProc)_XtInherit,             /* arm_and_activate      */
    NULL,                                 /* syn_resources         */
    0,                                    /* num_syn_resources     */
    NULL,                                 /* extension             */
    },
#endif
    {
    0,                                    /* ignore                */
    }
};









WidgetClass hyperWidgetClass = (WidgetClass) &hyperClassRec;

/*---------------------------------------------------------------*/
/* Create the two GCs needed                                     */
/*---------------------------------------------------------------*/

static void create_gcs(w)
Widget w;
{
    HyperWidget hw = (HyperWidget) w;
    XGCValues values;
    XtGCMask  valueMask;

    valueMask = GCForeground | GCBackground | GCFont;

    values.background = hw->core.background_pixel;

    values.foreground = hw->hyper.highlight_color;
    values.font       = hw->hyper.highlight_font->fid;
    hw->hyper.highlight_gc = XtGetGC (w, valueMask, &values);

    values.foreground = hw->hyper.select_color;
    hw->hyper.select_gc = XtGetGC (w, valueMask, &values);

    values.foreground = hw->hyper.normal_color;
    values.font       = hw->hyper.normal_font->fid;
    hw->hyper.normal_gc = XtGetGC (w, valueMask, &values);



    valueMask = GCBackground | GCForeground | GCFunction;

    values.background = hw->core.background_pixel;
    values.foreground = hw->hyper.normal_color;

    values.function   = GXxor;
    hw->hyper.xor_gc = XtGetGC (w, valueMask, &values);


}

/*--------------------------------------------------------------*/
/* Initialize: Create the GCs                                   */
/*--------------------------------------------------------------*/

static void Initialize (request, new)
HyperWidget request, new;
{
    /* Check the size of the widget */

    if (request->core.width == 0)
        new->core.width = 100;
    if (request->core.height == 0)
        new->core.height = 100;


    /* Create the GCs */

    create_gcs(new);

    /* No text yet */

    new->hyper.first_seg     = new->hyper.last_selected 
        = new->hyper.last_cursor =  NULL;
    new->hyper.hand     = XCreateFontCursor(XtDisplay(new),XC_hand2);

    /* Nothing found */

    new->hyper.grep_seg = NULL;
    new->hyper.grep_txt = NULL;
    new->hyper.grep_len = 0;
    new->hyper.grep_off = 0;

}

/*--------------------------------------------------------------*/
/* Free all memory allocated for the text segments              */
/*--------------------------------------------------------------*/

static void free_text(s)
text_segment *s;
{

    while(s)
    {
        text_segment *p=s->next;
        if(s->text) XtFree(s->text);
        XtFree((char *) s);
        s = p;
    }

}

/*--------------------------------------------------------------*/
/* Destroy the widget: release all memory alocated              */
/*--------------------------------------------------------------*/

static void Destroy (w)
Widget w;
{
    HyperWidget hw = (HyperWidget) w;

    free_text(hw->hyper.first_seg);
    XtReleaseGC (w, hw->hyper.normal_gc);
    XtReleaseGC (w, hw->hyper.highlight_gc);
    XtReleaseGC (w, hw->hyper.xor_gc);
    XtReleaseGC (w, hw->hyper.select_gc);
    XtRemoveAllCallbacks (w,XtNactivateCallback);
}

/*--------------------------------------------------------------*/
/* Resize : not implemented                                     */
/*--------------------------------------------------------------*/


static void Resize (w)
HyperWidget w;
{
    /* 
       For futur implementation
       May be for text warp ...
    */
}

/*--------------------------------------------------------------*/
/* Redisplay : redraw the text                                  */
/*--------------------------------------------------------------*/


static void Redisplay (w, event, region)
HyperWidget  w;
XEvent       *event;
Region        region;
{

    if(w->core.visible)
    {
        text_segment *s = w->hyper.first_seg;
        int x = w->hyper.margin;
        int y = 0;
        Boolean newline = TRUE;

        while(s)
        {

            /* change line on new lines */

            if(newline)
            {
                x = w->hyper.margin;
                y += s->height;
            }

            /* redraw only what is needed */

            if(XRectInRegion(region,x,y-s->height+s->desc,s->width,s->height)
                != RectangleOut)
            {

                XDrawImageString(XtDisplay (w), XtWindow (w), 
                    s->gc,
                    x,
                    y,
                    s->text,
                    s->length);
            }

            x += s->width;

            newline = (s->type == NEWLINE);

            s = s->next;
        }


        if(w->hyper.grep_seg)
        {
            if(XRectInRegion(region,
                w->hyper.grep_x,
                w->hyper.grep_y,
                w->hyper.grep_width,
                w->hyper.grep_height) != RectangleOut)

                XFillRectangle(XtDisplay(w),XtWindow(w),
                    w->hyper.xor_gc,
                    w->hyper.grep_x,
                    w->hyper.grep_y,
                    w->hyper.grep_width,
                    w->hyper.grep_height);

        }
    }
}

/*------------------------------------------------------------------*/
/* SetValues : redraw only for font or color changes                */
/*------------------------------------------------------------------*/

static Boolean SetValues (current, request, new)
Widget current, request, new;
{
    HyperWidget hcurrent = (HyperWidget) current;
    HyperWidget hnew = (HyperWidget) new;

    Boolean    redraw = FALSE;

#define HAS_CHANGED(a)    (hnew->a != hcurrent->a)

    if(
        HAS_CHANGED(core.background_pixel) ||
        HAS_CHANGED(hyper.select_color) ||
        HAS_CHANGED(hyper.highlight_color) ||
        HAS_CHANGED(hyper.highlight_font)  ||
        HAS_CHANGED(hyper.normal_color) ||
        HAS_CHANGED(hyper.normal_font)
        )
    {

        XtReleaseGC (new, hnew->hyper.normal_gc);
        XtReleaseGC (new, hnew->hyper.highlight_gc);
        XtReleaseGC (new, hnew->hyper.xor_gc);
        XtReleaseGC (new, hnew->hyper.select_gc);
        create_gcs(new);

        /* rebuild text */
/*
        if(HAS_CHANGED(hyper.normal_font) || 
            HAS_CHANGED(hyper.highlight_font))
			*/
            create_new_text(hnew);

        redraw = TRUE;
    }

    return (redraw);

#undef HAS_CHANGED

}

/*------------------------------------------------------------------*/
/* Calculate the size of the widget                                 */
/*------------------------------------------------------------------*/

static void calc_new_size (w)
Widget w;
{
    HyperWidget        hw = (HyperWidget) w;
    text_segment       *s = hw->hyper.first_seg;
    int                 x = hw->hyper.margin;
    int                 y = 0;
    int                 last_height = 0;
    Boolean             newline = TRUE;
    Dimension           maxWidth = hw->hyper.margin;
    Dimension           maxHeight = hw->hyper.margin;
    XtGeometryResult    result;
    Dimension           replyWidth = 0, replyHeight = 0;

    /* Get the size of the widget */

    while(s)
    {
        if(newline)
        {
            if(x>maxWidth) maxWidth=x;
            x = hw->hyper.margin;
            y += s->height;
            if(y>maxHeight) maxHeight=y;

        }

        s->x = x;
        s->y = y - s->height;

        x += s->width;

        newline = (s->type == NEWLINE);
        last_height = s->height;

        s = s->next;
    }

    x+= hw->hyper.margin;
    y+= last_height;

    if(x>maxWidth ) maxWidth=x;
    if(y>maxHeight) maxHeight=y;

    /* 
    Tell our parent we want a new size 
    */

    if(hw->core.width != maxWidth || hw->core.height != maxHeight)
    {
        result = XtMakeResizeRequest(w,maxWidth,maxHeight, 
            &replyWidth, &replyHeight) ;

        if (result == XtGeometryAlmost)
            XtMakeResizeRequest (w, replyWidth, replyHeight,NULL, NULL);
    }
}

/*-----------------------------------------------------------------------*/
/* Find the "visible" part of a widget as the intersection of all the    */
/* windows of it's parents' windows                                      */
/*-----------------------------------------------------------------------*/

static void find_visible_part(w,x,y,width,height)
Widget    w;
Position  *x;
Position  *y;
Dimension *width;
Dimension *height;
{
    Position root_x,root_y;
    Widget   p = w;

    *width  = w->core.width;
    *height = w->core.height;
    XtTranslateCoords(w,0,0,&root_x,&root_y);

    *x = 0;
    *y = 0;

    while(p = XtParent(p))
    {
        Position  rx,ry;
        Dimension w,h;

        /* 
           make all computations in the root's
           coordinate system
        */

        XtTranslateCoords(p,0,0,&rx,&ry);

        w = p->core.width;
        h = p->core.height;

        /* 
            use the smallest rectangle
        */

        if(w < *width)  *width  = w;
        if(h < *height) *height = h;

        if(rx>root_x) root_x = rx;
        if(ry>root_y) root_y = ry;

        /* stop when reach a shell,
          don't go to top level shell */
        if(XtIsShell(p)) break;
    }

    /* Back to the widget's coordinate system */

    XtTranslateCoords(w,0,0,x,y);
    *x = root_x - *x;
    *y = root_y - *y;


}

/*-----------------------------------------------------------------------*/
/* Do an "zoom" effect animation, from the selected text segment to the  */
/* visible part of the widget                                            */
/*-----------------------------------------------------------------------*/

static void zoom_open(w,s)
HyperWidget   w;
text_segment *s;
{
    int dx1,dx2,dy1,dy2;

    Position x ;
    Position y ;
    Dimension width  ;
    Dimension height ;

    /* selected rectangle */

    Position  xs = s->x;
    Position  ys = s->y;
    Dimension ws = s->width;
    Dimension hs = s->height;


    /* get the rectangle we want to zoom to */

    find_visible_part(w,&x,&y,&width,&height);

    /* make sure selected rectangle in visible */

    if(xs<x) xs = x;
    if(ys<y) ys = y;
    if(xs+ws > x+width)  ws = x+width-xs;
    if(ys+hs > y+height) hs = y+height-ys;

    /* get the offsets in each directions */

    dx1 = x-xs;
    dy1 = y-ys;
    dx2 = ((x+width)-(xs+ws));
    dy2 = ((y+height)-(ys+hs));

    /* in the rectangles are differents */

    if(dx1 || dy1 || dx2 || dy2)
    {
        int min = 32000; /* <-- Can be buggy */

        /* 
          work in "left,top,bottom,right" rectangles (Mac)
          rather than "x,y,width,height" (X)
          It's easier for the animation 
        */

        int xws = xs+ws;
        int yhs = ys+hs;
        int xw  = x + width;
        int yh  = y + height;


        /* Get smallest non-null offset */

        if(dx1) min = MIN(min,ABS(dx1));
        if(dx2) min = MIN(min,ABS(dx2));
        if(dy1) min = MIN(min,ABS(dy1));
        if(dy2) min = MIN(min,ABS(dy2));

        /* Scale offsets so minimun offset is 1 pixel */

        dx1 /= min;
        dx2 /= min;
        dy1 /= min;
        dy2 /= min;

        /* Use speed .. */

        dx1 *= w->hyper.speed;
        dx2 *= w->hyper.speed;
        dy1 *= w->hyper.speed;
        dy2 *= w->hyper.speed;

        /* Animate */

        while(min--)
        {
            XDrawRectangle(XtDisplay(w),XtWindow(w),
                w->hyper.xor_gc,xs,ys,xws-xs,yhs-ys);

            /* Needed, otherwise X calls are buffered */
            XSync(XtDisplay(w),False);

            XDrawRectangle(XtDisplay(w),XtWindow(w),
                w->hyper.xor_gc,xs,ys,xws-xs,yhs-ys);

            xs += dx1;
            ys += dy1;

            xws += dx2;
            yhs += dy2;

        }
    }

}

/*----------------------------------------------------------------------*/
/* Find the text segment at point (x,y)                                 */
/*----------------------------------------------------------------------*/
text_segment *find_segment(w,x,y)
HyperWidget w;
int x,y;
{
    text_segment *s = w->hyper.first_seg;

    while(s)
    {
        if( s->type == HIGHLIGHT &&
            x >= s->x &&
            y >= s->y &&
            x <= s->x + s->width &&
            y <= s->y + s->height 
            )
            return s;
        s = s->next;
    }

	return NULL;
}

/*----------------------------------------------------------------------*/
/* highlight text under cursor                                          */
/*----------------------------------------------------------------------*/
static void hilite(w,on)
HyperWidget   w;
Boolean on;
{

    text_segment *s = w->hyper.last_selected;

    if(s)
        XDrawImageString(XtDisplay (w), XtWindow (w),
            on?w->hyper.select_gc:s->gc,
            s->x,
            s->y+s->height,
            s->text, s->length);

}

/*-----------------------------------------------------------------------*/
/* Check for mouse down                                                  */
/*-----------------------------------------------------------------------*/

static void Select (w, event, args, n_args)
HyperWidget   w;
XEvent        *event;
char          *args[];
int            n_args;
{
    text_segment *s;

    /* 
       Find if the used clicked in an 
       highlighted text 
    */

    if(s = w->hyper.last_selected = find_segment(w,event->xbutton.x,event->xbutton.y))
        hilite(w,TRUE);
}

/*-----------------------------------------------------------------------*/
/* Check for mouse up                                                    */
/*-----------------------------------------------------------------------*/

static void activate (w, event, args, n_args)
Widget   w;
XEvent        *event;
char          *args[];
int            n_args;
{
    HyperWidget hw = (HyperWidget) w;
    hyperCallbackStruct cb;
    text_segment *s;

    /* 
       Find if the used clicked in an 
       highlighted text 
    */

    if((s = find_segment(hw,event->xbutton.x,event->xbutton.y))
        && (s == hw->hyper.last_selected))
    {
        hilite(hw,FALSE);

        /* zoom if required */

        if(hw->hyper.zoom) zoom_open(hw,s);

        /* Fill callback struct */

        cb.text     = s->text;
        cb.length   = s->length;
        cb.reason   = HYPER_REASON;
        cb.event    = event;

        /* call callbacks */

        XtCallCallbacks (w, XtNactivateCallback, &cb);
    }
    hw->hyper.last_selected = NULL;
}

/*-----------------------------------------------------------------------*/
/* Check for mouse moves                                                 */
/*-----------------------------------------------------------------------*/

static void cursor (w, event, args, n_args)
HyperWidget   w;
XEvent        *event;
char          *args[];
int            n_args;
{

    text_segment *s;
    hyperCallbackStruct cb;

    s = find_segment(w,event->xbutton.x,event->xbutton.y);

    if(s != w->hyper.last_cursor)
    {
        if(s)
	{
            XDefineCursor(XtDisplay(w),XtWindow(w),w->hyper.hand);

	    cb.text     = s->text;
	    cb.length   = s->length;
	    cb.reason   = HYPER_REASON;
	    cb.event    = event;
	    XtCallCallbackList((Widget) w, w->hyper.hyper_callbacks, &cb);
	}
        else
	{
            XUndefineCursor(XtDisplay(w),XtWindow(w));
	    XtCallCallbackList((Widget) w, w->hyper.hyper_callbacks,
			                       (hyperCallbackStruct *) 0);
	    
	}
        hilite(w,s == w->hyper.last_selected);
        w->hyper.last_cursor = s;
    }

}


/*-----------------------------------------------------------------------*/
/* Add a new text segment to the text                                    */
/*-----------------------------------------------------------------------*/
static void add_to_text(w,word,type)
HyperWidget w;
char *word;
int  type;
{
    text_segment *s = XtNew(text_segment);
    XCharStruct   char_info;
    int dir,ascent,desc;
    text_segment *p,*q;

    s->next = NULL;
    s->text = (word?XtNewString(word):NULL);
    s->type = type;
    s->gc   = (type == HIGHLIGHT ? w->hyper.highlight_gc : w->hyper.normal_gc);
    s->x    = s->y = s->width = s->height = 0;
    s->length = (word?strlen(word):0);

    XTextExtents(
        (type == HIGHLIGHT ? w->hyper.highlight_font : w->hyper.normal_font),
        word,
        s->length,
        &dir,&ascent,&desc,&char_info);

    s->height = ascent + desc;
    s->desc   = desc;
    s->width  = char_info.width;

    if(p = w->hyper.first_seg)
    {
        while(p)
        {
            q=p;
            p=p->next;
        }
        q->next = s;
    }
    else w->hyper.first_seg = s;
}

/*-----------------------------------------------------------------------*/
/* Rebuild the text structure. Called when the font changes              */
/*-----------------------------------------------------------------------*/

static void create_new_text(w)
HyperWidget   w;
{
    text_segment *s = w->hyper.first_seg;

    w->hyper.first_seg = w->hyper.last_selected = w->hyper.last_cursor = NULL;

    while(s)
    {
        add_to_text(w,s->text,s->type);
        s = s->next;
    }
    free_text(s);
    calc_new_size(w);
}

/*-----------------------------------------------------------------------*/
/* Build the text. Gets the chars from the funtion "get_next_char"       */
/* using "data" as a parameter                                           */
/*-----------------------------------------------------------------------*/

static void set_text(w,get_next_char,data)
Widget   w;
char (*get_next_char)();
XtPointer data;
{
    HyperWidget hw = (HyperWidget) w;
    char word[MAX_LINE_SIZE];
    int  i = 0;
    char soh = hw->hyper.start_of_highlight;
    char eoh = hw->hyper.end_of_highlight;
    char c;
    int  mode = NORMAL;

    free_text(hw->hyper.first_seg);
    hw->hyper.first_seg = hw->hyper.last_selected = hw->hyper.last_cursor = NULL;
    hw->hyper.grep_seg = NULL;
    hw->hyper.grep_txt = NULL;
    hw->hyper.grep_len = 0;
    hw->hyper.grep_off = 0;

    while(c = (get_next_char)(&data))
    {

        /* New line */

        if(c == '\n')
        {
            word[i]=0;
            if(i) add_to_text(hw,word,mode);
            add_to_text(hw,NULL,NEWLINE);
            i = 0;
        }

        /* Start of highlight */

        else if(c == soh)
        {
            word[i]=0;
            if(i) add_to_text(hw,word,mode);
            mode = HIGHLIGHT;
            i = 0;
        }

        /* End of highlight */

        else if(c == eoh)
        {
            word[i]=0;
            if(i) add_to_text(hw,word,mode);
            mode = NORMAL;
            i = 0;
        }
        else 
        {
            if(c=='\t') c = ' ';
            word[i++] = c;
            if(i==MAX_LINE_SIZE)
            {
                word[--i]=0;
                add_to_text(hw,word,mode);
                i=0;
                word[i++] = c;
            }
        }
    }

    /* flush .. */

    if(i)
    {
        word[i]=0;
        add_to_text(hw,word,mode);
    }

    calc_new_size(hw);

    if (XtIsRealized(w))
    {
	XClearArea(XtDisplay(w), XtWindow(w),
		                0, 0, w->core.width, w->core.height, True);
    }
}

/*-----------------------------------------------------------------------*/
/* Create a new HyperWidget                                              */
/*-----------------------------------------------------------------------*/

Widget CreateHyper(parent,name,al,ac)
Widget parent;
char   *name;
ArgList al;
int     ac;
{
    return XtCreateWidget(name,hyperWidgetClass,parent,al,ac);
}


/*-----------------------------------------------------------------------*/
/* Load the text from a file                                             */
/*-----------------------------------------------------------------------*/

/* provides chars to "set_text" routine */

static char get_from_file(f)
FILE **f;
{
    int n =  getc(*f);
    return (n==EOF?0:(char)n);
}

/* Public routine */

void HyperLoadFile(hyper,fname)
HyperWidget hyper;
char   *fname;
{
    Widget widget = (Widget) hyper;
    extern char *sys_errlist[];

    FILE *f = fopen(fname,"r");
    if(f)
    {
        set_text(hyper,get_from_file,(XtPointer)f);
        fclose(f);
    }
    else
    {
        char msg[1024];
        sprintf(msg,"%s: %s",fname,sys_errlist[errno]);
        XtWarning(msg);
    }

}

/*-----------------------------------------------------------------------*/
/* Load text from memory buffer                                          */
/*-----------------------------------------------------------------------*/

/* provides chars to "set_text" routine */

static char get_from_buffer(buffer)
char **buffer;
{
    char c = **buffer;
    (*buffer)++;
    return c;
}

/* Public routine */

void HyperSetText(widget,text)
HyperWidget  widget;
char *text;
{
    set_text(widget,get_from_buffer,(XtPointer)text);
}

/*-----------------------------------------------------------------------*/
/* Specifies start and end of highlignt chars                            */
/*-----------------------------------------------------------------------*/

#ifdef _NO_PROTO

void HyperSetTags(widget,start_highlight,end_highlight)
HyperWidget   widget;
unsigned char start_highlight;
unsigned char end_highlight;

#else

void HyperSetTags(HyperWidget widget,
				  unsigned char start_highlight,
				  unsigned char end_highlight)

#endif

{
    widget->hyper.start_of_highlight = start_highlight;
    widget->hyper.end_of_highlight = end_highlight;
}


/*-----------------------------------------------------------------------*/
/* convert a string to lower case                                        */
/*-----------------------------------------------------------------------*/

static void lowcase(p)
register char *p;
{
    while(*p)
    {
        if(isupper(*p)) *p += 32;
        p++;
    }
}

/*-----------------------------------------------------------------------*/
/* Returns the text of the widget                                        */
/* the memory is allocated. It must be freed by the application          */
/* If include_tags if FALSE, the special characters are not returned     */
/*-----------------------------------------------------------------------*/

#ifdef _NO_PROTO

char *HyperGetText(widget,include_tags)
HyperWidget widget;
Boolean include_tags;

#else

char *HyperGetText(HyperWidget widget,Boolean include_tags)

#endif
{

    char         *p ;
    text_segment *s = widget->hyper.first_seg;
    int          len = 1;
    char         soh[2];
    char         eoh[2];

    soh[0] = widget->hyper.start_of_highlight;
    eoh[0] = widget->hyper.end_of_highlight;

    soh[1] = eoh[1] = 0;

    /* Get size of text */

    while(s)
    {
        len += s->length?s->length:1;
        if(include_tags && s->type == HIGHLIGHT)
            len += 2;
        s = s->next;
    }

    p = XtMalloc(len);
    *p = 0;

    s = widget->hyper.first_seg;
    while(s)
    {
        if(s->length)
        {
            if(include_tags && s->type == HIGHLIGHT)
                strcat(p,soh);
            strcat(p,s->text);
            if(include_tags && s->type == HIGHLIGHT)
                strcat(p,eoh);
        }
        else
            strcat(p,"\n");
        s=s->next;
    }

    return p;

}

/*-----------------------------------------------------------------------*/
/* Only for Motif                                                        */
/* If the widget is in a XmScrolledWindow, scroll it so the selection is */
/* visible                                                               */
/*-----------------------------------------------------------------------*/

static void show_selection(h)
HyperWidget h;
{
#ifdef MOTIF
#define SetArg(a,b)  XtSetArg(al[ac],a,b);ac++
#define GetValues(w) XtGetValues(w,al,ac);ac=0
#define SetValues(w) XtSetValues(w,al,ac);ac=0

    Widget clip = XtParent(h);
    Widget swin;

    Widget h_scroll;
    Widget v_scroll;

    int ac = 0;

    Position    x_parent,y_parent;
    Position    x_grep,y_grep;
    Dimension   h_grep,w_grep;
    Position    x_clip,y_clip;
    Dimension   h_clip,w_clip;
    Position    dv=0,dh=0;
    int min,max;
    int v_val,v_size,v_inc,v_page;
    int h_val,h_size,h_inc,h_page;
    Position x,y;

    Arg al[5];



    /* check if selection exists */

    if(!h->hyper.grep_seg) return;

    /* check if the widget is in a scrolled window */
    /* the XnScrolledWindow creates a clip window  */
    /* The widget's parent is the clip window      */


    if(!clip) return;
    swin = XtParent(clip);

    if(!swin || !XmIsScrolledWindow(swin)) return;


    /* Get window scroll bars */

    SetArg(XmNhorizontalScrollBar, &h_scroll);
    SetArg(XmNverticalScrollBar  , &v_scroll);
    GetValues(swin);

    /* Get size of clip window and selection rect */

    w_clip = clip->core.width;
    h_clip = clip->core.height;

    w_grep = h->hyper.grep_width;
    h_grep = h->hyper.grep_height;

    /* Get global coordinates of clip and selection rect */

    XtTranslateCoords(clip,0,0,&x_clip,&y_clip);
    XtTranslateCoords(h,h->hyper.grep_x,h->hyper.grep_y,&x_grep,&y_grep);

    /* offset of selection within clip window */

    x = x_grep - x_clip;
    y = y_grep - y_clip;


    /* selection y coordinate is not visible */

    if( y < 0 || y + h_grep > h_clip)
    {
        /* the widget must be moved verticaly by dv pixels */

        dv = (y + h_grep / 2)  - h_clip / 2;

        SetArg(XmNminimum,&min);
        SetArg(XmNmaximum,&max);

        GetValues(v_scroll);

        XmScrollBarGetValues(v_scroll,&v_val,&v_size,&v_inc,&v_page);

        max -= v_size;

        if( dv + v_val > max ) dv = max - v_val;
        if( dv + v_val < min ) dv = min - v_val;


    }

    /* selection x coordinate is not visible */

    if( x < 0 || x + w_grep > w_clip)
    {
        /* the widget must be moved horizontaly by dh pixels */

        dh = (x + w_grep / 2)  - w_clip / 2;

        SetArg(XmNminimum,&min);
        SetArg(XmNmaximum,&max);
        GetValues(h_scroll);

        XmScrollBarGetValues(h_scroll,&h_val,&h_size,&h_inc,&h_page);

        max -= h_size;

        if( dh + h_val > max ) dh = max - h_val;
        if( dh + h_val < min ) dh = min - h_val;

    }

    /* if the widget must be moved */

    if(dv || dh)
    {
        Position x = h->core.x-dh;
        Position y = h->core.y-dv;

        /* move it */

        SetArg(XmNx,x);
        SetArg(XmNy,y);
        SetValues(h);

        /* update scroll bars */

        if(dv) XmScrollBarSetValues(v_scroll,v_val+dv,v_size,v_inc,
            v_page,TRUE);
        if(dh) XmScrollBarSetValues(h_scroll,h_val+dh,h_size,h_inc,
            h_page,TRUE);


    }


#endif /* MOTIF */
}

/*-----------------------------------------------------------------------*/
/* Clear previous selection                                              */
/*-----------------------------------------------------------------------*/

static void clear_selection(hw)
HyperWidget hw;
{
    Widget w = (Widget) hw;

    if(hw->hyper.grep_seg)
    {
        if(XtIsRealized(w))

            /* force a redraw */

            XClearArea(XtDisplay(w),XtWindow(w),
                hw->hyper.grep_x,
                hw->hyper.grep_y,
                hw->hyper.grep_width,
                hw->hyper.grep_height,
                TRUE);

    }
    hw->hyper.grep_seg = NULL;
}

/*-----------------------------------------------------------------------*/
/* Set the new selection                                                 */
/*-----------------------------------------------------------------------*/

static void set_selection(w)
HyperWidget w;
{
    Widget widget = (Widget) w;

    if(w->hyper.grep_seg)
    {
        text_segment *s = w->hyper.grep_seg;
        char *p = s->text;
        XCharStruct   char_info;
        int dir,ascent,desc;

        /* get size of the begining of
           the segment, up to the found string */

        XTextExtents(
            (s->type == HIGHLIGHT ? 
            w->hyper.highlight_font : 
            w->hyper.normal_font),
            s->text,
            w->hyper.grep_off,
            &dir,&ascent,&desc,&char_info);

        w->hyper.grep_x      = s->x + char_info.width;
        w->hyper.grep_y      = s->y + desc;
        w->hyper.grep_height = s->height;

        /* Get size of the selection */

        XTextExtents(
            (s->type == HIGHLIGHT ? 
            w->hyper.highlight_font : 
            w->hyper.normal_font),
            w->hyper.grep_txt,
            w->hyper.grep_len,
            &dir,&ascent,&desc,&char_info);


        w->hyper.grep_width  = char_info.width;

        /* force update */

        if(XtIsRealized(widget))
            XClearArea(XtDisplay(widget),XtWindow(widget),
                w->hyper.grep_x,
                w->hyper.grep_y,
                w->hyper.grep_width,
                w->hyper.grep_height,
                TRUE);
    }
}

/* size of regexp buffer */

#define ESIZE 1024

/*-----------------------------------------------------------------------*/
/* if you have regexp, define USE_REGEXP                                 */
/* NOTE : since regexp variables and functions are not static            */
/* you can have some problems if you use the same names or include       */
/* regexp somewhere else                                                 */
/*-----------------------------------------------------------------------*/
#ifdef USE_REGEXP

/* regexp macros ... see "man regexp" */

#define INIT        register char *sp = instring;
#define GETC()      (*sp++)
#define PEEKC()     (*sp)
#define UNGETC(c)   (--sp)
#define RETURN(c)   return;
#define ERROR(c)    printf(stderr,"Warning regexp error %d\n",c)


#include <regexp.h>


#else 

/*-----------------------------------------------------------------------*/
/* If we don't have regexp mimic it.                                     */
/* Just find plain text using strncmp. no regular expression matching    */
/*-----------------------------------------------------------------------*/

static char *loc1,*loc2;
static int len;

static compile(w,buf,end)
char *w,*buf;
int end;
{
    strcpy(buf,w);
    len = strlen(w);
}

static step(w,buf)
char *w;
char *buf;
{
    loc1 = w;
    while(*loc1)
    {
        if(strncmp(loc1,buf,len) == 0)
        {
            loc2 = loc1+len;
            return TRUE;
        }
        loc1++;
    }
    return FALSE;
}


#endif

/*-----------------------------------------------------------------------*/
/* Select a word in the hyper widget                                     */
/* word : word to find ( or regular expression if USE_REGEX is defined)  */
/* ignore_case : if TRUE ignore case in comparaison                      */
/* from_start : if TRUE search from start of text, else search from      */
/* current selection                                                     */
/* wrap: if TRUE, continue search from the begining of text if the end   */
/* is reached                                                            */
/*-----------------------------------------------------------------------*/

#ifdef _NO_PROTO

Boolean HyperGrep(widget,word,ignore_case,from_start,wrap)
HyperWidget   widget;
char     *word;
Boolean  ignore_case;
Boolean  from_start;
Boolean  wrap;

#else

Boolean HyperGrep(HyperWidget widget,
				  char *word,
				  Boolean ignore_case,
				  Boolean from_start,
				  Boolean wrap)

#endif

{
    char         *w = word;
    char         *p;
    int          offset,from,to;
    text_segment *s;
    char          expbuf[ESIZE];

    if(!widget->hyper.first_seg) return;

    if(ignore_case)
    {
        /* if ignore case, change word to lower case */
        w = XtNewString(word);
        lowcase(w);
    }

    /* compile the regular expression */
    compile(w,expbuf,&expbuf[ESIZE],'\0');


    if(ignore_case) XtFree(w);

    /* if from_start or no previous selection, 
       start from first segment */

    if(from_start || widget->hyper.grep_seg == NULL)
    {
        offset=0;
        wrap = FALSE;
        s = widget->hyper.first_seg;
    }
    else 
    {
        /* start from last selection */

        offset = widget->hyper.grep_off + widget->hyper.grep_len;
        s = widget->hyper.grep_seg;
    }

    for(;;)
    {
        if(s->text)
        {
            if(ignore_case)
            {
                /* if ignore case, change segment to lower case */
                p = XtNewString(s->text);
                lowcase(p);
            }

            /* search the string */

            if(step(p+offset,expbuf))
            {
                /* if found ...*/

                /* clear previous selection */
                clear_selection(widget);

                widget->hyper.grep_seg = s;
                widget->hyper.grep_off = offset + (loc1-(p+offset));
                widget->hyper.grep_txt = s->text + widget->hyper.grep_off;
                widget->hyper.grep_len = loc2-loc1;

                /* set new selection */

                set_selection(widget);

                /* make it visible */

                show_selection(widget);

                if(ignore_case) XtFree(p);

                return TRUE;
            }

            if(ignore_case) XtFree(p);
        }

        offset = 0;
        s = s->next;

        /* if end of text and wrap mode, go to start of text */
        if(!s)
            if(wrap)
            {
                wrap = FALSE;
                s = widget->hyper.first_seg;
            }
            else break;

    }


    return FALSE;

}

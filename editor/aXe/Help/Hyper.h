/*==================================================================*/
/*                                                                  */
/* HyperWidget                                                      */
/*                                                                  */
/* B.Raoult (mab@ecmwf.co.uk)                              Oct.91   */
/*                                                                  */
/* Hyper text like widget.                                          */
/*                                                                  */
/*==================================================================*/

#ifndef  HYPER_H
#define  HYPER_H

/* 
    If you define MOTIF, the widget will inherit proprieties 
   from the XmPrimitive class : Help Callback, user data, ...
*/

/*
#define MOTIF
*/

/*
   If your machine got regexp.h
*/
/*#define USE_REGEXP*/

extern WidgetClass hyperWidgetClass;
typedef struct _HyperClassRec * HyperWidgetClass;
typedef struct _HyperRec      * HyperWidget;

/*
 * Define resource strings for the Hyper widget.
 */

#define XtNhighlightFont     "highlightFont"
#define XtNnormalFont        "normalFont"
#define XtNhighlightColor    "highlightColor"
#define XtNselectColor       "selectColor"
#define XtNnormalColor       "normalColor"
#define XtNactivateCallback  "activateCallback"
#define XtNhyperCallback     "hyperCallback"
#define XtNzoomEffect        "zoomEffect"
#define XtCZoom              "Zoom"
#define XtNstartHighlight    "startHighlight"
#define XtNendHighlight      "endHighlight"
#define XtCTagChar           "TagChar"
#define XtNzoomSpeed         "zoomSpeed"
#define XtCZoomSpeed         "ZoomSpeed"
/* Same as in StringDfs.h, so expect to get it form there (JKW)
#define XtCMargin            "Margin"
*/
#define XtNmargin            "margin"

/*
  Callback structure
*/

#define HYPER_REASON 1

typedef struct {
    int     reason;   /* always = HYPER_REASON                            */
    XEvent *event;    /* event                                            */
    char     *text;     /* pointer on highlighted text selected (read only) */
    int  length;    /* length of selected text                          */
}  hyperCallbackStruct;

#ifdef _NO_PROTO

extern Widget CreateHyper();
extern void HyperLoadFile();
extern void HyperSetText();
extern void HyperSetTags();
extern Boolean HyperFind();
extern char    *HyperGetText();

#else

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

    extern Widget CreateHyper(Widget parent,
        char *name,
        ArgList al,
        int ac);

    extern void HyperLoadFile(HyperWidget widget,
        char *fname);

    extern void HyperSetText(HyperWidget widget,
        char *text);

    extern void HyperSetTags (HyperWidget widget,
        unsigned char start_highlight,
        unsigned char end_highlight);

    Boolean HyperGrep(HyperWidget  widget,
        char    *word,
        Boolean ignore_case,
        Boolean from_start,
        Boolean wrap);

    char *HyperGetText(HyperWidget widget,
	Boolean include_tags);


#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* _NO_PROTO */

#define XtIsHyper(w)     XtIsSubclass(w,hyperWidgetClass)

#endif HYPER_H

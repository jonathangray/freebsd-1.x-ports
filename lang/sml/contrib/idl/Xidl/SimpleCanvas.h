/*
	Simple X canvas widget (for use with idl)

	@(#)SimpleCanvas.h	2.1 93/03/07 00:58:10



*/
#include <X11/copyright.h>

#ifndef _SimpleCanvas_h
#define _SimpleCanvas_h

/****************************************************************
 *
 * SimpleCanvas widget
 *
 ****************************************************************/

/* Resources:

 Name                Class              RepType         Default Value
 ----                -----              -------         -------------

 background          Background         Pixel           XtDefaultBackground
 border              BorderColor        Pixel           XtDefaultForeground
 borderWidth         BorderWidth        Dimension       1
 exposeCallback      Callback           Pointer         NULL
 dragCallback        Callback           Pointer         NULL
 foreground          Foreground         Pixel           XtDefaultForeground
 height              Height             Dimension       0
 mappedWhenManaged   MappedWhenManaged  Boolean         True
 sensitive           Sensitive          Boolean         True
 width               Width              Dimension       0
 x                   Position           Position        0
 y                   Position           Position        0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNdrawingColor     "drawingColor"
#define XtNexposeCallback   "exposeCallback"
#define XtNoffsetCallback   "offsetCallback"
#define XtNextentCallback   "extentCallback"

/*
#define XtNcanvasResource       "canvasResource"
#define XtCSimpleCanvasResource "SimpleCanvasResource"
*/

/* declare specific SimpleCanvasWidget class and instance datatypes */

typedef struct _SimpleCanvasClassRec* SimpleCanvasWidgetClass;
typedef struct _SimpleCanvasRec*      SimpleCanvasWidget;

/* declare the class constant */

extern WidgetClass simplecanvasWidgetClass;


#endif /* _SimpleCanvas_h */

/*
 *  Copyright (C) 1991 By DeepCore Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *      DeepCore Technologies
 *	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#include "dgl.h"

/*
 * local variables
 */
static DGLWidget *ButtomWidget = (DGLWidget*)0; 
static DGLWidget *TopWidget = (DGLWidget*)0; 


DGLWidget *DGLWidgetCreate(Xdim, Ydim)
int Xdim, Ydim;
{
DGLWidget *W;

  if ((W=(DGLWidget*)malloc(sizeof(DGLWidget)))==(DGLWidget*)0)
    return (DGLWidget*)0;

  W->Status = W->Xpos = W->Ypos = W->Xhot = W->Yhot = 0;
  W->CallBackFunction = 0;
  W->Over = W->Under = (DGLWidget*)0;

  if ((W->Object=(DGLBitmap*)DGLBitmapCreate(Xdim, Ydim))==(DGLBitmap*)0) {
    free(W);
    return (DGLWidget*)0;
  }

  if ((W->Mask=(DGLBitmap*)DGLBitmapCreate(Xdim, Ydim))==(DGLBitmap*)0) {
    DGLBitmapDestroy(W->Object);
    free(W);
    return (DGLWidget*)0;
  }

  if ((W->SaveUnder=(DGLBitmap*)DGLBitmapCreate(Xdim, Ydim))==(DGLBitmap*)0) {
    DGLBitmapDestroy(W->Object);
    DGLBitmapDestroy(W->Mask);
    free(W);
    return (DGLWidget*)0;
  }
  return W;
}

DGLWidgetDestroy(Widget)
DGLWidget *Widget;
{
  DGLBitmapDestroy(Widget->Object);
  DGLBitmapDestroy(Widget->Mask);
  DGLBitmapDestroy(Widget->SaveUnder);
  free(Widget);
}

DGLWidget *DGLWidgetDup(Widget)
DGLWidget *Widget;
{
DGLWidget *W;

  if ((W=DGLWidgetCreate(XSIZ(Widget), YSIZ(Widget))) == (DGLWidget*)0) 
    return (DGLWidget*)0;
  DGLBitmapCopy(W->Object, 0, 0, Widget->Object, 0, 0, 
		XSIZ(Widget), YSIZ(Widget), 0);
  DGLBitmapCopy(W->SaveUnder, 0, 0, Widget->SaveUnder, 0, 0, 
		XSIZ(Widget), YSIZ(Widget), 0);
  DGLBitmapCopy(W->Mask, 0, 0, Widget->Mask, 0, 0, 
		XSIZ(Widget), YSIZ(Widget), 0);
  W->Status = Widget->Status;
  W->Xpos = Widget->Xpos;
  W->Ypos = Widget->Ypos;
  W->Xhot = Widget->Xhot;
  W->Yhot = Widget->Yhot;
  return W;
}

DGLWidgetInsert(Position, Widget)
DGLWidget *Position, *Widget;
{
  if (Position->Over)
    Position->Over->Under = Widget;
  else
    TopWidget = Widget;

  Widget->Over = Position->Over;
  Widget->Under = Position;
  Position->Over = Widget;

  if (Widget->SaveUnder->Bitmap && (Widget->SaveUnder->Type==MEMBUF))
    free (Widget->SaveUnder->Bitmap);

  Widget->SaveUnder->Bitmap = (char*)malloc(XSIZ(Widget)*YSIZ(Widget));

  if (Widget->SaveUnder->Bitmap == (char*)0) 
    return 1;

  return 0;
}

DGLWidgetAdd(Widget)
DGLWidget *Widget;
{
  if (ButtomWidget) {
    ButtomWidget->Under = Widget;
  }
  else {
    TopWidget = Widget;
  }
  Widget->Over = ButtomWidget;
  Widget->Under = (DGLWidget*)0;
  ButtomWidget = Widget;

  if (Widget->SaveUnder->Bitmap && (Widget->SaveUnder->Type==MEMBUF))
    free (Widget->SaveUnder->Bitmap);

  Widget->SaveUnder->Bitmap = 
    (char*)malloc(XSIZ(Widget)*YSIZ(Widget));

  if (Widget->SaveUnder->Bitmap == (char*)0)
    return 1;

  return 0;
}

DGLWidgetDelete(Widget)
DGLWidget *Widget;
{
  if (Widget->Over)
    if (Widget->Under) {
      Widget->Under->Over = Widget->Over;
      Widget->Over->Under = Widget->Under;
    }
    else {
      Widget->Over->Under = Widget->Under;
      ButtomWidget = Widget->Over;
    }
  else
    if (Widget->Under) {
      Widget->Under->Over = Widget->Over;
      TopWidget = Widget->Under;
    }
    else {
      TopWidget = Widget->Under;
      ButtomWidget = Widget->Over;
    }
}

DGLWidgetCheckOverlapUp(Original, Xsize, Ysize, Xpos, Ypos, Widget, Map)
DGLBitmap *Original;
int Xsize, Ysize, Xpos, Ypos;
DGLWidget *Widget;
DGLBitmap *Map;
{
DGLWidget *CheckWidget;
int xo, yo, xosize, yosize;

  CheckWidget=Widget;
  while (CheckWidget=CheckWidget->Over) {

    /* Not ACTIVE widgets have no interest */
    if ((CheckWidget->Status & ACTIVE) == 0) 
      continue;

    /* The following cyrillics tests for overlapping widgets - YUCK */
    if (XPOS(CheckWidget) > Xpos) {
      xo = XPOS(CheckWidget);
      xosize = (Xpos + Xsize) - XPOS(CheckWidget);
    } else {
      xo = Xpos;
      xosize = (XPOS(CheckWidget)+XSIZ(CheckWidget)) - Xpos;
    }
        
    if (xosize < 0) 
      continue;
        
    if (YPOS(CheckWidget) > Ypos) {
      yo = YPOS(CheckWidget);
      yosize = (Ypos + Ysize) - YPOS(CheckWidget);
    } 
    else {
      yo = Ypos;
      yosize = (YPOS(CheckWidget)+YSIZ(CheckWidget)) - Ypos;
    }
        
    if (yosize < 0) 
      continue;

    /*
     * We have an overlap ! 
     * xo, yo, xosize, yosize in DISPLAY coordinates
     */
    /*
     * Copy Widget SaveUnder into overlapping Widgets SaveUnder 
     */
    DGLBitmapCopy(CheckWidget->SaveUnder, 
	          xo-XPOS(CheckWidget), yo-YPOS(CheckWidget), 
		  Original,
		  xo-Xpos, yo-Ypos,
		  xosize, yosize, Map);
    /*
     * Null out the overlapped area in the WorkMap
     */
    DGLBitmapClear(Map, 
	           xo-Xpos, yo-Ypos, 
		   CheckWidget->Mask,
		   xo-XPOS(CheckWidget), yo-YPOS(CheckWidget), 
	    	   xosize, yosize, CheckWidget->Mask, 0);

  }
}

DGLWidgetCheckOverlapDown(Original, Xsize, Ysize, Xpos, Ypos, Widget)
DGLBitmap *Original;
int Xsize, Ysize, Xpos, Ypos;
DGLWidget *Widget;
{
DGLWidget *CheckWidget;
int xo, yo, xosize, yosize;

  for (CheckWidget = TopWidget; 
  CheckWidget != Widget; 
  CheckWidget = CheckWidget->Under) {

    /* Not ACTIVE widgets have no interest */
    if ((CheckWidget->Status & ACTIVE) == 0) 
      continue;

    /* The following cyrillics tests for overlapping widgets - YUCK */
    if (XPOS(CheckWidget) > Xpos) {
      xo = XPOS(CheckWidget);
      xosize = (Xpos + Xsize) - XPOS(CheckWidget);
    }
    else {
      xo = Xpos;
      xosize = (XPOS(CheckWidget)+XSIZ(CheckWidget)) - Xpos;
    }
        
    if (xosize < 0) 
      continue;
        
    if (YPOS(CheckWidget) > Ypos) {
      yo = YPOS(CheckWidget);
      yosize = (Ypos + Ysize) - YPOS(CheckWidget);
    }
    else {
      yo = Ypos;
      yosize = (YPOS(CheckWidget)+YSIZ(CheckWidget)) - Ypos;
    }
        
    if (yosize < 0) 
      continue;
    
    /*
     * We have an overlap ! 
     * xo, yo, xosize, yosize in DISPLAY coordinates
     */
	    
    /*
     * Copy overlapping Widgets SaveUnder into Widgets SaveUnder
     */
    DGLBitmapCopy(Original,
		  xo-Xpos, yo-Ypos,
		  CheckWidget->SaveUnder,
		  xo-XPOS(CheckWidget), yo-YPOS(CheckWidget), 
		  xosize, yosize, CheckWidget->Mask);

  }
}

DGLWidgetHide(Widget)
DGLWidget *Widget;
{
DGLBitmap *WorkMap;

  if (Widget->Status & ACTIVE) {

    WorkMap = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (WorkMap == (DGLBitmap*)0) 
      return 1;
    memcpy(WorkMap->Bitmap, Widget->Mask->Bitmap, (XSIZ(Widget)*YSIZ(Widget)));

    DGLWidgetCheckOverlapUp(Widget->SaveUnder, 
			    XSIZ(Widget), YSIZ(Widget), 
			    XPOS(Widget), YPOS(Widget),
			    Widget, WorkMap);

    /*
     * Erase what remains (uncovered) of widgets object 
     */
    DGLBitmapCopy(DGLDisplay, XPOS(Widget), YPOS(Widget), 
                  Widget->SaveUnder, 0, 0, 
		  XSIZ(Widget), YSIZ(Widget),
		  WorkMap);
    
    DGLBitmapDestroy(WorkMap);
    Widget->Status &= ~ACTIVE;
    }
  return 0;
}

DGLWidgetShow(Widget)
DGLWidget *Widget;
{
DGLWidget *CheckWidget;
DGLBitmap *WorkMap;

  if ((Widget->Status & ACTIVE) == 0) {

    WorkMap = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (WorkMap == (DGLBitmap*)0) 
      return 1;
    memcpy(WorkMap->Bitmap, Widget->Mask->Bitmap, (XSIZ(Widget)*YSIZ(Widget)));
    
    /*
     * Store SaveUnder now, may be overwritten later
     */
    DGLBitmapCopy(Widget->SaveUnder, 0, 0,
                  DGLDisplay, XPOS(Widget), YPOS(Widget), 
	          XSIZ(Widget), YSIZ(Widget),
	          0);
  
    DGLWidgetCheckOverlapDown(Widget->SaveUnder, 
			      XSIZ(Widget), YSIZ(Widget), 
			      XPOS(Widget), YPOS(Widget),
			      Widget);
  
    DGLWidgetCheckOverlapUp(Widget->Object, 
			    XSIZ(Widget), YSIZ(Widget), 
			    XPOS(Widget), YPOS(Widget),
			    Widget, WorkMap);
          
    DGLBitmapCopy(DGLDisplay, XPOS(Widget), YPOS(Widget), 
	          Widget->Object, 0, 0, 
		  XSIZ(Widget), YSIZ(Widget),
		  WorkMap);
      
    DGLBitmapDestroy(WorkMap);
    Widget->Status |= ACTIVE;
    }
  return 0;
}

DGLWidgetUpdate(Widget)
DGLWidget *Widget;
{
DGLWidget *CheckWidget;
DGLBitmap *WorkMap;

  if (Widget->Status & ACTIVE) {

    WorkMap = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (WorkMap == (DGLBitmap*)0) 
      return 1;
    memcpy(WorkMap->Bitmap, Widget->Mask->Bitmap, (XSIZ(Widget)*YSIZ(Widget)));
    
    DGLWidgetCheckOverlapUp(Widget->SaveUnder, 
			    XSIZ(Widget), YSIZ(Widget), 
			    XPOS(Widget), YPOS(Widget),
			    Widget, WorkMap);
        
    DGLBitmapCopy(DGLDisplay, XPOS(Widget), YPOS(Widget), 
	          Widget->Object, 0, 0, 
		  XSIZ(Widget), YSIZ(Widget),
		  WorkMap);
    
    DGLBitmapDestroy(WorkMap);
  }
  return 0;
}

DGLWidgetMoveTo(Widget, x, y)
DGLWidget *Widget;
int x, y;
{
DGLWidget *CheckWidget;
DGLBitmap *WorkMap, *SaveMap, *SaveMask;
int xs, ys;
int xo, yo, xosize, yosize;

  if (Widget->Xpos == x && Widget->Ypos == y) return 0;

  if (Widget->Status & ACTIVE) { 

    /*
     * Save the 'old' SaveUnder for later restoration 
     */
    SaveMap = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (SaveMap == (DGLBitmap*)0) 
      return 1;
    memcpy(SaveMap->Bitmap, Widget->SaveUnder->Bitmap,
           (XSIZ(Widget)*YSIZ(Widget)));

    SaveMask = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (SaveMask == (DGLBitmap*)0) 
      return 1;
    memcpy(SaveMask->Bitmap, Widget->Mask->Bitmap, (XSIZ(Widget)*YSIZ(Widget)));

    DGLWidgetCheckOverlapUp(SaveMap, 
			    XSIZ(Widget), YSIZ(Widget), 
			    XPOS(Widget), YPOS(Widget),
			    Widget, SaveMask);
    xs = XPOS(Widget);
    ys = YPOS(Widget);

    /*
     * Now move Widget
     */

    Widget->Xpos = x;
    Widget->Ypos = y;

    /*
     * Store new SaveUnder now, may be overwritten later
     */
    DGLBitmapCopy(Widget->SaveUnder, 0, 0,
                  DGLDisplay, XPOS(Widget), YPOS(Widget), 
	          XSIZ(Widget), YSIZ(Widget),
	          0);
    /* 
     * Put in saveunder from old position if overlap
     */

    if (XPOS(Widget) > xs) {
      xo = XPOS(Widget);
      xosize = (xs + SaveMap->Xsize) - XPOS(Widget);
    }
    else {
      xo = xs;
      xosize = (XPOS(Widget)+XSIZ(Widget)) - xs;
    }
    if (YPOS(Widget) > ys) {
      yo = YPOS(Widget);
      yosize = (ys + SaveMap->Ysize) - YPOS(Widget);
    }
    else {
      yo = ys;
      yosize = (YPOS(Widget)+YSIZ(Widget)) - ys;
    }
    if (xosize > 0 && yosize > 0) {
      /*
       * Copy SaveMap into Widgets SaveUnder
       */
      DGLBitmapCopy(Widget->SaveUnder, 
		    xo-XPOS(Widget), yo-YPOS(Widget), 
		    SaveMap,
		    xo-xs, yo-ys, 
		    xosize, yosize, Widget->Mask);
      /*
       * Null out the overlapped area in the SaveMask
       */
      DGLBitmapClear(SaveMask, 
		     xo-xs, yo-ys,
		     Widget->Mask,
		     xo-XPOS(Widget), yo-YPOS(Widget), 
 	             xosize, yosize, Widget->Mask, 0);
    }

    WorkMap = (DGLBitmap*)DGLBitmapCreate(XSIZ(Widget), YSIZ(Widget));
    if (WorkMap == (DGLBitmap*)0) 
      return 1;
    memcpy(WorkMap->Bitmap, Widget->Mask->Bitmap, (XSIZ(Widget)*YSIZ(Widget)));

    DGLWidgetCheckOverlapDown(Widget->SaveUnder, 
			      XSIZ(Widget), YSIZ(Widget), 
			      XPOS(Widget), YPOS(Widget),
			      Widget);

    DGLWidgetCheckOverlapUp(Widget->Object, 
			    XSIZ(Widget), YSIZ(Widget), 
			    XPOS(Widget), YPOS(Widget),
     			    Widget, WorkMap);

    DGLBitmapCopy(DGLDisplay, XPOS(Widget), YPOS(Widget), 
                  Widget->Object, 0, 0, 
	          XSIZ(Widget), YSIZ(Widget),
	          WorkMap);

    /*
     * Erase what remains (uncovered) of widgets old object 
     */
    DGLBitmapCopy(DGLDisplay, xs, ys,
                  SaveMap, 0, 0, 
	          SaveMap->Xsize, SaveMap->Ysize,
	          SaveMask);

    DGLBitmapDestroy(WorkMap);
    DGLBitmapDestroy(SaveMap);
    DGLBitmapDestroy(SaveMask);
  }
  else {
    Widget->Xpos = x;
    Widget->Ypos = y;
  }
  return 0;
}

DGLWidgetMove(Widget, x, y)
DGLWidget *Widget;
int x, y;
{
  DGLWidgetMoveTo(Widget, Widget->Xpos + x, Widget->Ypos + y);
}

DGLWidget *DGLWidgetFind(x, y) 
int x, y;
{
DGLWidget *Pointer = ButtomWidget;

  while (Pointer) {
    if (x >= XPOS(Pointer) && x < XPOS(Pointer)+XSIZ(Pointer)
      && 
       y >= YPOS(Pointer) && y < YPOS(Pointer)+YSIZ(Pointer)) break;
    Pointer = Pointer->Over;
  }
  return Pointer;
}

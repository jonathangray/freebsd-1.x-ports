/*****************************************************************************/
/**               Copyright 1990 by Solbourne Computer Inc.                 **/
/**                          Longmont, Colorado                             **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    name of Solbourne not be used in advertising                         **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    SOLBOURNE COMPUTER INC. DISCLAIMS ALL WARRANTIES WITH REGARD         **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL SOLBOURNE                **/
/**    BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-           **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/

/**********************************************************************
 *
 * $XConsortium: vdt.c,v 1.140 90/03/23 11:42:33 jim Exp $
 *
 * Virtual Desktop includes
 *
 * 22-Aug-90 Tom LaStrange        Initial Version.
 *
 **********************************************************************/

#ifndef _VDT_
#define _VDT_

#define DEFAULT_PANNER_SCALE 20
#define DEFAULT_PANNER_GEOMETRY "-0-0"

extern void InitVirtualDesktop();
extern void Set__SWM_ROOT();
extern void Set__SWM_VERSION();
extern void Remove__SWM_VERSION();
extern void MakePanner();
extern void MakeVirtualDesktop();
extern void MapFrame();
extern void UnmapFrame();
extern void RaiseFrame();
extern void LowerFrame();
extern void MapIcon();
extern void UnmapIcon();
extern void RaiseIcon();
extern void LowerIcon();
extern void MoveIcon();
extern void ResizeVirtual();
extern Window MakeVirtual();
extern void HandlePannerExpose();
extern void HandlePannerButtonPress();
extern void HandlePannerButtonRelease();
extern void HandlePannerMotionNotify();
extern void MoveDesktop();
extern void ScrollDesktop();
extern void ResizeDesktop();
extern void ScrollTo();
extern void PaintVirtualWindow();

#endif /* _VDT_ */


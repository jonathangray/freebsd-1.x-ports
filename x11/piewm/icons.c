/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/**********************************************************************
 *
 * $XConsortium: icons.c,v 1.20 90/03/27 13:51:34 jim Exp $
 *
 * Icon releated routines
 *
 * $Log: icons.c,v $
 * Revision 1.1  1994/05/18 18:13:01  asami
 * Initial revision
 *
 * Revision 1.5  1991/10/03  00:13:31  cross
 * Fixed some bogus uses of the NULL symbol
 *
 * Revision 1.4  1991/10/01  20:13:10  cross
 * Fixed things so that the shape_mask returned from an Xpm with
 * shaping in it works.  (Yay!)
 *
 * Revision 1.3  1991/09/26  00:32:30  cross
 * changed GetBitmap() calls to GetPixmap()
 *
 * Revision 1.2  1991/09/26  00:09:45  cross
 * Changed FindBitmap calls to FindPixmap.  And, did a little in the way of
 * correctly using the returned shape_mask, but I doubt it'll work right.
 *
 * Revision 1.1  1991/09/25  23:48:20  cross
 * Initial revision
 *
 * Revision 10.0  91/06/12  09:05:35  toml
 * Revision bump
 * 
 * Revision 9.0  91/04/23  07:40:37  toml
 * Revision bump
 * 
 * Revision 8.0  90/11/15  20:02:40  toml
 * Revision bump
 * 
 * Revision 7.3  90/11/12  21:34:53  toml
 * Implemented Scr->StickyAbove
 * 
 * Revision 7.2  90/11/12  19:57:23  toml
 * Patches to allow sticky windows to lower
 * 
 * Revision 1.2  90/11/04  18:30:24  brossard
 * When non-sticky client provide their own icons, don't forget to
 * reparent the root.
 * Removed the test above for stickyness since all windows are now child
 * of the virtual root.
 * 
 *
 * 10-Apr-89 Tom LaStrange        Initial Version.
 *
 **********************************************************************/

#include <stdio.h>
#include "twm.h"
#include "screen.h"
#include "icons.h"
#include "gram.h"
#include "parse.h"
#include "util.h"

#define iconWidth(w)	(Scr->IconBorderWidth * 2 + w->icon_w_width)
#define iconHeight(w)	(Scr->IconBorderWidth * 2 + w->icon_w_height)

#ifdef SHAPE
Pixmap SetIconMask();
Pixmap SetIconClip();
#endif /* SHAPE */

static
splitEntry (ie, grav1, grav2, w, h)
    IconEntry	*ie;
    int		grav1, grav2;
    int		w, h;
{
    IconEntry	*new;

    switch (grav1) {
    case D_NORTH:
    case D_SOUTH:
	if (w != ie->w)
	    splitEntry (ie, grav2, grav1, w, ie->h);
	if (h != ie->h) {
	    new = (IconEntry *)malloc (sizeof (IconEntry));
	    new->twm_win = 0;
	    new->used = 0;
	    new->next = ie->next;
	    ie->next = new;
	    new->x = ie->x;
	    new->h = (ie->h - h);
	    new->w = ie->w;
	    ie->h = h;
	    if (grav1 == D_SOUTH) {
		new->y = ie->y;
		ie->y = new->y + new->h;
	    } else
		new->y = ie->y + ie->h;
	}
	break;
    case D_EAST:
    case D_WEST:
	if (h != ie->h)
	    splitEntry (ie, grav2, grav1, ie->w, h);
	if (w != ie->w) {
	    new = (IconEntry *)malloc (sizeof (IconEntry));
	    new->twm_win = 0;
	    new->used = 0;
	    new->next = ie->next;
	    ie->next = new;
	    new->y = ie->y;
	    new->w = (ie->w - w);
	    new->h = ie->h;
	    ie->w = w;
	    if (grav1 == D_EAST) {
		new->x = ie->x;
		ie->x = new->x + new->w;
	    } else
		new->x = ie->x + ie->w;
	}
	break;
    }
}

roundUp (v, multiple)
{
    return ((v + multiple - 1) / multiple) * multiple;
}

PlaceIcon(tmp_win, def_x, def_y, final_x, final_y)
TwmWindow *tmp_win;
int def_x, def_y;
int *final_x, *final_y;
{
    IconRegion	*ir;
    IconEntry	*ie;
    int		w = 0, h = 0;

    ie = 0;
    for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	w = roundUp (iconWidth (tmp_win), ir->stepx);
	h = roundUp (iconHeight (tmp_win), ir->stepy);
	for (ie = ir->entries; ie; ie=ie->next) {
	    if (ie->used)
		continue;
	    if (ie->w >= w && ie->h >= h)
		break;
	}
	if (ie)
	    break;
    }
    if (ie) {
	splitEntry (ie, ir->grav1, ir->grav2, w, h);
	ie->used = 1;
	ie->twm_win = tmp_win;
	*final_x = ie->x + (ie->w - iconWidth (tmp_win)) / 2;
	*final_y = ie->y + (ie->h - iconHeight (tmp_win)) / 2;
    } else {
	if (tmp_win->root == Scr->VirtualDesktop) {
	    def_x += Scr->vdtPositionX;
	    def_y += Scr->vdtPositionY;
	}
	*final_x = def_x;
	*final_y = def_y;
    }
    return;
}

static IconEntry *
FindIconEntry (tmp_win, irp)
    TwmWindow   *tmp_win;
    IconRegion	**irp;
{
    IconRegion	*ir;
    IconEntry	*ie;

    for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	for (ie = ir->entries; ie; ie=ie->next)
	    if (ie->twm_win == tmp_win) {
		if (irp)
		    *irp = ir;
		return ie;
	    }
    }
    return 0;
}

IconUp (tmp_win)
    TwmWindow   *tmp_win;
{
    int		x, y;
    int		defx, defy;
    struct IconRegion *ir;

    /*
     * If the client specified a particular location, let's use it (this might
     * want to be an option at some point).  Otherwise, try to fit within the
     * icon region.
     */
    if (tmp_win->wmhints && (tmp_win->wmhints->flags & IconPositionHint))
      return;

    if (tmp_win->icon_moved) {
	if (!XGetGeometry (dpy, tmp_win->icon_w, &JunkRoot, &defx, &defy,
			   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth))
	  return;

	x = defx + ((int) JunkWidth) / 2;
	y = defy + ((int) JunkHeight) / 2;

	for (ir = Scr->FirstRegion; ir; ir = ir->next) {
	    if (x >= ir->x && x < (ir->x + ir->w) &&
		y >= ir->y && y < (ir->y + ir->h))
	      break;
	}
	if (!ir) return;		/* outside icon regions, leave alone */
    }

    defx = -100;
    defy = -100;
    PlaceIcon(tmp_win, defx, defy, &x, &y);
    if (tmp_win->root == Scr->VirtualDesktop) {
	defx += Scr->vdtPositionX;
	defy += Scr->vdtPositionY;
    }
    if (x != defx || y != defy) {
	MoveIcon(tmp_win, x, y);
	tmp_win->icon_moved = FALSE;	/* since we've restored it */
    }
}

static IconEntry *
prevIconEntry (ie, ir)
    IconEntry	*ie;
    IconRegion	*ir;
{
    IconEntry	*ip;

    if (ie == ir->entries)
	return 0;
    for (ip = ir->entries; ip->next != ie; ip=ip->next)
	;
    return ip;
}

/* old is being freed; and is adjacent to ie.  Merge
 * regions together
 */

static
mergeEntries (old, ie)
    IconEntry	*old, *ie;
{
    if (old->y == ie->y) {
	ie->w = old->w + ie->w;
	if (old->x < ie->x)
	    ie->x = old->x;
    } else {
	ie->h = old->h + ie->h;
	if (old->y < ie->y)
	    ie->y = old->y;
    }
}

IconDown (tmp_win)
    TwmWindow   *tmp_win;
{
    IconEntry	*ie, *ip, *in;
    IconRegion	*ir;

    ie = FindIconEntry (tmp_win, &ir);
    if (ie) {
	ie->twm_win = 0;
	ie->used = 0;
	ip = prevIconEntry (ie, ir);
	in = ie->next;
	for (;;) {
	    if (ip && ip->used == 0 &&
		((ip->x == ie->x && ip->w == ie->w) ||
		 (ip->y == ie->y && ip->h == ie->h))) {
	    	ip->next = ie->next;
	    	mergeEntries (ie, ip);
	    	free ((char *) ie);
		ie = ip;
	    	ip = prevIconEntry (ip, ir);
	    } else if (in && in->used == 0 &&
		       ((in->x == ie->x && in->w == ie->w) ||
			(in->y == ie->y && in->h == ie->h))) {
	    	ie->next = in->next;
	    	mergeEntries (in, ie);
	    	free ((char *) in);
	    	in = ie->next;
	    } else
		break;
	}
    }
}

AddIconRegion(geom, grav1, grav2, stepx, stepy)
char *geom;
int grav1, grav2;
{
    IconRegion *ir;
    int mask;

    ir = (IconRegion *)malloc(sizeof(IconRegion));
    ir->next = NULL;
    if (Scr->LastRegion)
	Scr->LastRegion->next = ir;
    Scr->LastRegion = ir;
    if (!Scr->FirstRegion)
	Scr->FirstRegion = ir;

    ir->entries = NULL;
    ir->grav1 = grav1;
    ir->grav2 = grav2;
    if (stepx <= 0)
	stepx = 1;
    if (stepy <= 0)
	stepy = 1;
    ir->stepx = stepx;
    ir->stepy = stepy;
    ir->x = ir->y = ir->w = ir->h = 0;

    mask = XParseGeometry(geom, &ir->x, &ir->y,
			  (unsigned int *)&ir->w, (unsigned int *)&ir->h);

    if (mask & XNegative)
	ir->x += Scr->MyDisplayWidth - ir->w;

    if (mask & YNegative)
	ir->y += Scr->MyDisplayHeight - ir->h;
    ir->entries = (IconEntry *)malloc(sizeof(IconEntry));
    ir->entries->next = 0;
    ir->entries->x = ir->x;
    ir->entries->y = ir->y;
    ir->entries->w = ir->w;
    ir->entries->h = ir->h;
    ir->entries->twm_win = 0;
    ir->entries->used = 0;
}

#ifdef comment
FreeIconEntries (ir)
    IconRegion	*ir;
{
    IconEntry	*ie, *tmp;

    for (ie = ir->entries; ie; ie=tmp) {
	tmp = ie->next;
	free ((char *) ie);
    }
}

FreeIconRegions()
{
    IconRegion *ir, *tmp;

    for (ir = Scr->FirstRegion; ir != NULL;) {
	tmp = ir;
	FreeIconEntries (ir);
	ir = ir->next;
	free((char *) tmp);
    }
    Scr->FirstRegion = NULL;
    Scr->LastRegion = NULL;
}
#endif /* comment */

CreateIconWindow(tmp_win, def_x, def_y)
TwmWindow *tmp_win;
int def_x, def_y;
{
    unsigned long event_mask;
    unsigned long valuemask;		/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    Pixmap pm = None;			/* tmp pixmap variable */
#ifdef SHAPE 
    Pixmap bmmask = None;
    Pixmap bmclip = None;
#endif /* SHAPE */
    int final_x, final_y;
    int x;
    static Bool isXpm[1] = { False };
    Pixmap shape_mask = None;

    FB(tmp_win->iconc.fore, tmp_win->iconc.back);

    tmp_win->forced = FALSE;
    tmp_win->icon_not_ours = FALSE;

    /* now go through the steps to get an icon window,  if ForceIcon is 
     * set, then no matter what else is defined, the bitmap from the
     * .twmrc file is used
     */
    if (Scr->ForceIcon) {
	char *icon_name;
	Pixmap bm;

	icon_name = LookInNameList(Scr->IconNames, tmp_win->full_name);
        if (icon_name == NULL)
	    icon_name = LookInList(Scr->IconNames, tmp_win->full_name,
				   &tmp_win->class);

	bm = None;
	if (icon_name != NULL) {
	    if ((bm = (Pixmap)LookInNameList(Scr->Icons, icon_name)) == None) {
		if ((bm = FindPixmap (icon_name, &JunkWidth, &JunkHeight,
				      isXpm, NULL, &shape_mask)) != None) {
		    AddToList(&Scr->Icons, icon_name, (char *)bm);

#ifdef XPM
		    if (*isXpm == True)
			AddToList(&Scr->IconsPixmapType, icon_name, "p");
		    else
			AddToList(&Scr->IconsPixmapType, icon_name, "b");
#endif  /* XPM */

#ifdef SHAPE 
		    if (HasShape) {
			if (shape_mask == None) {
		    	    bmmask = SetIconMask(icon_name);
			    bmclip = SetIconClip(icon_name);
		    	    bmmask =  (Pixmap)LookInNameList(Scr->Iconsmask, 
				                         icon_name);
			} else {
			    bmmask = shape_mask;
			    bmclip = shape_mask;
			    AddToList(&Scr->Iconsmask, icon_name,
					(char *)bmmask);
			    AddToList(&Scr->Iconsclip, icon_name,
					(char *)bmclip);
			}
		    }
#endif /* SHAPE */
		}
	    }
#ifdef SHAPE
	    else if (HasShape) {
		bmmask =  (Pixmap)LookInNameList(Scr->Iconsmask, icon_name);
		bmclip =  (Pixmap)LookInNameList(Scr->Iconsclip, icon_name);
	    }
#endif /* SHAPE */
	}

	if (bm != None) {
	  Status s;
	    pm = XCreatePixmap(dpy, Scr->Root, tmp_win->icon_width,
				tmp_win->icon_height, Scr->d_depth);

#ifdef XPM
            if (*(LookInNameList(Scr->IconsPixmapType, icon_name)) == 'b') {
#endif /* XPM */
		/* the copy plane works on color ! */
		XCopyPlane(dpy, bm, pm, Scr->NormalGC,
			   0,0, tmp_win->icon_width, tmp_win->icon_height,
			   0, 0, 1 );
#ifdef XPM
            } else {
                XCopyArea(dpy, bm, pm, Scr->NormalGC, 0, 0,
			  tmp_win->icon_width, tmp_win->icon_height,
			  0, 0);
            }
#endif  /* XPM */
	    tmp_win->forced = TRUE;
	}
    }

    /* if the pixmap is still NULL, we didn't get one from the above code,
     * that could mean that ForceIcon was not set, or that the window
     * was not in the Icons list, now check the WM hints for an icon
     */
    if (pm == None && tmp_win->wmhints &&
	tmp_win->wmhints->flags & IconPixmapHint) {
    
	XGetGeometry(dpy,   tmp_win->wmhints->icon_pixmap,
		     &JunkRoot, &JunkX, &JunkY,
		     (unsigned int *)&tmp_win->icon_width,
		     (unsigned int *)&tmp_win->icon_height,
		     &JunkBW, &JunkDepth);

	pm = XCreatePixmap(dpy, Scr->Root,
			   tmp_win->icon_width, tmp_win->icon_height,
			   Scr->d_depth);

	XCopyPlane(dpy, tmp_win->wmhints->icon_pixmap, pm, Scr->NormalGC,
	    0,0, tmp_win->icon_width, tmp_win->icon_height, 0, 0, 1 );
    }

    /* if we still haven't got an icon, let's look in the Icon list 
     * if ForceIcon is not set
     */
    if (pm == None && !Scr->ForceIcon) {
	char *icon_name;
	Pixmap bm;

	icon_name = LookInNameList(Scr->IconNames, tmp_win->full_name);
        if (icon_name == NULL)
	    icon_name = LookInList(Scr->IconNames, tmp_win->full_name,
				   &tmp_win->class);

	bm = None;
	if (icon_name != NULL) {
	    if ((bm = (Pixmap)LookInNameList(Scr->Icons, icon_name)) == None) {
		if ((bm = FindPixmap (icon_name, &JunkWidth, &JunkHeight,
				      isXpm, NULL, &shape_mask)) != None) {
		    AddToList(&Scr->Icons, icon_name, (char *)bm);

#ifdef XPM
		    if (*isXpm == True)
			AddToList(&Scr->IconsPixmapType, icon_name, "p");
		    else
			AddToList(&Scr->IconsPixmapType, icon_name, "b");
#endif  /* XPM */

#ifdef SHAPE 
		    if (HasShape) {
			if (shape_mask == None) {
		    	    bmmask = SetIconMask(icon_name);
			    bmclip = SetIconClip(icon_name);
		    	    bmmask =  (Pixmap)LookInNameList(Scr->Iconsmask, 
				                         icon_name);
			} else {
			    bmmask = shape_mask;
			    bmclip = shape_mask;
			    AddToList(&Scr->Iconsmask, icon_name,
					(char *)bmmask);
			    AddToList(&Scr->Iconsclip, icon_name,
					(char *)bmclip);
			}
		    }
#endif /* SHAPE */
		}
	    }
#ifdef SHAPE
	    else if (HasShape) {
		bmmask =  (Pixmap)LookInNameList(Scr->Iconsmask, icon_name);
		bmclip =  (Pixmap)LookInNameList(Scr->Iconsclip, icon_name);
	    }
#endif /* SHAPE */
	}
	if (bm != None) {
	    XGetGeometry(dpy, bm, &JunkRoot, &JunkX, &JunkY,
			 (unsigned int *)&tmp_win->icon_width,
			 (unsigned int *)&tmp_win->icon_height,
			 &JunkBW, &JunkDepth);
	    pm = XCreatePixmap(dpy, Scr->Root,
			       tmp_win->icon_width, tmp_win->icon_height,
			       Scr->d_depth);

#ifdef XPM
            if (*(LookInNameList(Scr->IconsPixmapType, icon_name)) == 'b') {
#endif /* XPM */
                /* the copy plane works on color ! */
                XCopyPlane(dpy, bm, pm, Scr->NormalGC, 0,0,
			   tmp_win->icon_width, tmp_win->icon_height,
			   0, 0, 1 );
#ifdef XPM
            } else {
                XCopyArea(dpy, bm, pm, Scr->NormalGC, 0, 0,
			  tmp_win->icon_width, tmp_win->icon_height,
			  0, 0);
            }
#endif /* XPM */
	}
    }

    /* if we still don't have an icon, assign the UnknownIcon */

    if (pm == None && Scr->UnknownPm != None) {
	tmp_win->icon_width = Scr->UnknownWidth;
	tmp_win->icon_height = Scr->UnknownHeight;

	pm = XCreatePixmap(dpy, Scr->Root, tmp_win->icon_width,
	    tmp_win->icon_height, Scr->d_depth);

	/* the copy plane works on color ! */
	XCopyPlane(dpy, Scr->UnknownPm, pm, Scr->NormalGC,
	    0,0, tmp_win->icon_width, tmp_win->icon_height, 0, 0, 1 );
#ifdef SHAPE
	bmmask = Scr->UnknownPmmsk;
	bmclip = Scr->UnknownPmclp;
#endif /* SHAPE */
    }

    if (pm == None) {
	tmp_win->icon_height = 0;
	tmp_win->icon_width = 0;
	tmp_win->icon_title = True;
	valuemask = 0;
    } else {
	valuemask = CWBackPixmap;
	attributes.background_pixmap = pm;
    }

    if (tmp_win->icon_title) {
	tmp_win->icon_w_width = XTextWidth(Scr->IconFont.font,
	    tmp_win->icon_name, strlen(tmp_win->icon_name));

	tmp_win->icon_w_width += 6;
    }
    if (tmp_win->icon_w_width < tmp_win->icon_width) {
	tmp_win->icon_x = (tmp_win->icon_width - tmp_win->icon_w_width)/2;
	tmp_win->icon_x += 3;
	tmp_win->icon_w_width = tmp_win->icon_width;
    } else {
	tmp_win->icon_x = 3;
    }
    tmp_win->icon_y = tmp_win->icon_height + Scr->IconFont.height;
    tmp_win->icon_w_height = tmp_win->icon_height;
    if (tmp_win->icon_title)
	tmp_win->icon_w_height += Scr->IconFont.height + 4;

    event_mask = 0;
    if (tmp_win->wmhints && tmp_win->wmhints->flags & IconWindowHint) {
	tmp_win->icon_w = tmp_win->wmhints->icon_window;
	if (tmp_win->forced ||
	    XGetGeometry(dpy, tmp_win->icon_w, &JunkRoot, &JunkX, &JunkY,
			 (unsigned int *)&tmp_win->icon_w_width,
			 (unsigned int *)&tmp_win->icon_w_height,
			 &JunkBW, &JunkDepth) == 0) {
	    tmp_win->icon_w = None;
	    tmp_win->wmhints->flags &= ~IconWindowHint;
	} else {
	    tmp_win->icon_not_ours = TRUE;
	    /*
	     * We need to reparent to take into account the virtual root
	     */
	    if (Scr->VirtualDesktop && (!tmp_win->sticky || !Scr->StickyAbove))
		XReparentWindow (dpy, tmp_win->wmhints->icon_window,
				 tmp_win->root,
				 tmp_win->icon_x, tmp_win->icon_y);
	  }
    } else {
	tmp_win->icon_w = None;
    }

    if (tmp_win->icon_w == None) {
	tmp_win->icon_w =
	  XCreateSimpleWindow(dpy, tmp_win->root, 0,0,
			      tmp_win->icon_w_width, tmp_win->icon_w_height,
			      Scr->IconBorderWidth, tmp_win->icon_border,
			      tmp_win->iconc.back);
	event_mask = ExposureMask;
    }

    if (Scr->VirtualDesktop) {
	tmp_win->virtualIcon =
	  MakeVirtual(tmp_win, 0, 0,
		      tmp_win->icon_w_width, tmp_win->icon_w_height,
		      tmp_win->virtual.back, tmp_win->icon_border);
    }
    XSelectInput (dpy, tmp_win->icon_w,
		  KeyPressMask | ButtonPressMask | ButtonReleaseMask |
		  event_mask);

    tmp_win->icon_bm_w = None;
    if (pm != None &&
	(! (tmp_win->wmhints && tmp_win->wmhints->flags & IconWindowHint))) {
	int y;

	y = 0;
	if (tmp_win->icon_w_width == tmp_win->icon_width)
	    x = 0;
	else
	    x = (tmp_win->icon_w_width - tmp_win->icon_width)/2;

	tmp_win->icon_bm_w = XCreateWindow (dpy, tmp_win->icon_w, x, y,
					    (unsigned int)tmp_win->icon_width,
					    (unsigned int)tmp_win->icon_height,
					    (unsigned int) 0, Scr->d_depth,
					    (unsigned int) CopyFromParent,
					    Scr->d_visual, valuemask,
					    &attributes);
    }

    /* I need to figure out where to put the icon window now, because 
     * getting here means that I am going to make the icon visible
     */
    if (tmp_win->wmhints &&
	tmp_win->wmhints->flags & IconPositionHint) {
	final_x = tmp_win->wmhints->icon_x;
	final_y = tmp_win->wmhints->icon_y;
    } else {
	PlaceIcon(tmp_win, def_x, def_y, &final_x, &final_y);
    }

    if (tmp_win->root == Scr->Root) {
	if (final_x > Scr->MyDisplayWidth)
	    final_x = Scr->MyDisplayWidth - tmp_win->icon_w_width -
		(2 * Scr->IconBorderWidth);

	if (final_y > Scr->MyDisplayHeight)
	    final_y = Scr->MyDisplayHeight - tmp_win->icon_height -
		Scr->IconFont.height - 4 - (2 * Scr->IconBorderWidth);
    }


    MoveIcon(tmp_win, final_x, final_y);
    tmp_win->iconified = TRUE;
#ifdef SHAPE
    if (bmmask != None && HasShape) {
	XRectangle titlebox[1];
	
	XShapeCombineMask(dpy, tmp_win->icon_w, ShapeBounding, 
			 x, 0, bmmask, ShapeSet);
 	titlebox[0].x = -(Scr->IconBorderWidth);
	titlebox[0].y = -(Scr->IconBorderWidth);
	titlebox[0].height = Scr->IconFont.height + 2 * Scr->IconBorderWidth;
	titlebox[0].width = tmp_win->icon_w_width + 2 * Scr->IconBorderWidth;
	XShapeCombineRectangles(dpy, tmp_win->icon_w, ShapeBounding,
			 0, tmp_win->icon_height+4,  
			 titlebox, 1, ShapeUnion, YXBanded);
	if (bmclip == None) bmclip = bmmask; 
    }
    if (bmclip != None && HasShape) {
	XRectangle titlebox[1];
	XShapeCombineMask(dpy, tmp_win->icon_w, ShapeClip, 
			 x, 0, bmclip, ShapeSet);
 	titlebox[0].x = 0;
	titlebox[0].y = 0;
	titlebox[0].height = Scr->IconFont.height + 0;
	titlebox[0].width = tmp_win->icon_w_width;
	XShapeCombineRectangles(dpy, tmp_win->icon_w, ShapeClip,
			 0, tmp_win->icon_height+4,  
			 titlebox, 1, ShapeUnion, YXBanded);
    }
#endif /* SHAPE */

    XMapSubwindows(dpy, tmp_win->icon_w);

    XSaveContext(dpy, tmp_win->icon_w, TwmContext, (caddr_t)tmp_win);
    XSaveContext(dpy, tmp_win->icon_w, ScreenContext, (caddr_t)Scr);
    XDefineCursor(dpy, tmp_win->icon_w, Scr->IconCursor);
    if (pm) XFreePixmap (dpy, pm);
    return;
}

#ifdef SHAPE

#define MAX_PATH 1024 /* Yes I should use the system symbol */

Pixmap SetIconMask(icon_name)
char *icon_name;
{
    char icon_mask[MAX_PATH+1];
    Pixmap bm;

    strcpy(icon_mask, icon_name);
    strcat(icon_mask, "msk");
    if ((bm = GetPixmap (icon_mask)) != None) {
    	AddToList(&Scr->Iconsmask, icon_name, (char *)bm);
	return (bm);
    }
    return (None);
}

#define ERR_RETURN 0

static char *Format_image(image, resultsize) /* Stolen from lib/X/XWrBitF.c */
XImage *image;
int *resultsize;
{
    register int x, c, b;
    register char *ptr;
    int y;
    char *data;
    int width, height;
    int bytes_per_line;
    
    width = image->width;
    height = image->height;
    
    bytes_per_line = (width+7)/8;
    *resultsize = bytes_per_line * height;    /* Calculate size of data */

    data = (char *) malloc( *resultsize );     /* Get space for data */
    if (!data)
      return(ERR_RETURN);

    /*
     * The slow but robust brute force method of converting the image:
     */
    ptr = data;
    c = 0; b=1;
    for (y=0; y<height; y++) {
	for (x=0; x<width;) {
	    if (XGetPixel(image, x, y))
	      c |= b;
	    b <<= 1;
	    if (!(++x & 7)) {
		*(ptr++)=c;
		c=0; b=1;
	    }
	}
	if (x & 7) {
	    *(ptr++)=c;
	    c=0; b=1;
	}
    }

    return(data);
}

#define tst_bit(i, j, w, b) (b[(i+j*(w*8))/8] &  (1 << ((i+j*(w*8)) % 8)))
#define set_bit(i, j, w, b) (b[(i+j*(w*8))/8] |= (1 << ((i+j*(w*8)) % 8)))

Pixmap SetIconClip(icon_name)
char *icon_name;
{
    char icon_clip[MAX_PATH+1];
    Pixmap bm, bmmask;
    unsigned int height, width;
    int size;
    XImage *image;
    char *dataold, *datanew;
    int i, j, ii, jj, w, border;

    border = Scr->IconBorderWidth;
    strcpy(icon_clip, icon_name);
    strcat(icon_clip, "clp");
    if ((bm = GetPixmap (icon_clip)) != None) {
    	AddToList(&Scr->Iconsclip, icon_name, (char *)bm);
	if ((Pixmap)LookInNameList(Scr->Iconsmask, icon_name) == None) {
	    /*
	     * Create border mask from clip.
	     */
	    XGetGeometry(dpy, bm, &JunkRoot, &JunkX, &JunkY,
			 (unsigned int *)&width, (unsigned int *)&height,
			 &JunkBW, &JunkDepth);
	    image = XGetImage(dpy, bm, 0,0, width, height, 1L, XYPixmap);
	    dataold = Format_image(image, &size);
	    datanew = (char *) malloc(size);
	    for (i=0; i<size; i++) datanew[i] = 0;
	    XDestroyImage(image);
	    w = (width+7)/8;
	    for ( j = 0; j < height; j++) {
		for ( i = 0; i < 8*w; i++) {
		    if (!tst_bit(i, j, w, dataold)) continue;
		    for ( jj = j-border; jj <= j+border; jj++) {
			for ( ii = i-border; ii <= i+border; ii++) {
			    if (jj < 0         || ii < 0        ||
				jj >= height   || ii >= w*8) continue;
			    set_bit(ii, jj, w, datanew);
			}
		    }
		}
	    }
	    bmmask = XCreateBitmapFromData(dpy, Scr->Root, 
					   (char *)datanew, width, height);
	    AddToList(&Scr->Iconsmask, icon_name, (char *)bmmask);
	    free(dataold);
	    free(datanew);
	}
	return (bm);
    }
    return (None);
}

#endif /* SHAPE */

/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevmswn.c */
/*
 * Microsoft Windows 3.n driver for Ghostscript.
 * Original version by Russell Lang and Maurice Castro with help from
 * Programming Windows, 2nd Ed., Charles Petzold, Microsoft Press;
 * created from gdevbgi.c and gnuplot/term/win.trm 5th June 1992.
 * Extensively modified by L. Peter Deutsch, Aladdin Enterprises.
 */
#include "gdevmswn.h"
#include "gp.h"
#include "gpcheck.h"
#include "gsprops.h"
#include "gdevpccm.h"

/* Forward references */
private void near win_makeimg(P1(gx_device_win *));
LRESULT CALLBACK _export WndImgProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK _export WndImgChildProc(HWND, UINT, WPARAM, LPARAM);

/* Define the repaint interval in milliseconds. */
/* Suggested values: 10000 for 286, 5000 for 386, 3000 for 486. */
#define repaint_interval 5000
#define TIMER_ID 1

/* Open the win driver */
int
win_open(gx_device *dev)
{	HDC hdc;
	int depth;
	WNDCLASS wndclass;
	static BOOL registered = FALSE;

	/* Initialize the scrolling information. */
	
	wdev->cxClient = wdev->cyClient = 0;
	wdev->nVscrollPos = wdev->nVscrollMax = 0;
	wdev->nHscrollPos = wdev->nHscrollMax = 0;

	wdev->update = wdev->timer = FALSE;

	if (dev->width == INITIAL_WIDTH)
	  dev->width  = (int)(8.5  * dev->x_pixels_per_inch);
	if (dev->height == INITIAL_HEIGHT)
	  dev->height = (int)(11.0 * dev->y_pixels_per_inch);

	/* If this is the first instance, register the window classes. */
	if (!registered) {
	    /* register the window class for graphics */
	    wndclass.style = CS_HREDRAW | CS_VREDRAW;
	    wndclass.lpfnWndProc = WndImgChildProc;
	    wndclass.cbClsExtra = 0;
	    wndclass.cbWndExtra = sizeof(LONG);
	    wndclass.hInstance = phInstance;
	    wndclass.hIcon = LoadIcon(phInstance,"grpicon");
	    wndclass.hCursor = LoadCursor((HINSTANCE)NULL, IDC_ARROW);
	    wndclass.hbrBackground = GetStockObject(WHITE_BRUSH);
	    wndclass.lpszMenuName = NULL;
	    wndclass.lpszClassName = szAppName;
	    RegisterClass(&wndclass);

	    wndclass.style = CS_HREDRAW | CS_VREDRAW;
	    wndclass.lpfnWndProc = WndImgProc;
	    wndclass.cbClsExtra = 0;
	    wndclass.cbWndExtra = sizeof(LONG);
	    wndclass.hInstance = phInstance;
	    wndclass.hIcon = LoadIcon(phInstance,"grpicon");
	    wndclass.hCursor = LoadCursor((HINSTANCE)NULL, IDC_ARROW);
	    wndclass.hbrBackground = GetStockObject(WHITE_BRUSH);
	    wndclass.lpszMenuName = NULL;
	    wndclass.lpszClassName = szImgName;
	    RegisterClass(&wndclass);
	
	    registered = TRUE;
	}

	win_makeimg(wdev);
	wdev->hdctext = NULL;

	/* Set parameters that were unknown before opening device */
	/* Find out if the device supports color */
	/* We recognize 2, 16 or 256 color devices */
	hdc = GetDC(wdev->hwndimgchild);
	depth = GetDeviceCaps(hdc,PLANES) * GetDeviceCaps(hdc,BITSPIXEL);
	ReleaseDC(wdev->hwndimgchild,hdc);
	if ( depth >= 8 ) { /* use 64 static colors and 166 dynamic colors from 8 planes */
		static const gx_device_color_info win_256color = dci_color(8,31,4);
		dev->color_info = win_256color;
		wdev->nColors = 64;
	}
	else if ( depth >= 4 ) {
		static const gx_device_color_info win_16ega_color = dci_ega;
		static const gx_device_color_info win_16vga_color = dci_vga;
		hdc = GetDC(NULL);
		if (GetDeviceCaps(hdc, VERTRES) <= 350)
		    dev->color_info = win_16ega_color;
		else
		    dev->color_info = win_16vga_color;
		ReleaseDC(NULL,hdc);
		wdev->nColors = 16;
	} 
	else {   /* default is black_and_white */
		wdev->nColors = 2;
	}

	/* create palette for display */
	if ((wdev->limgpalette = win_makepalette(wdev))
		== (LPLOGPALETTE)NULL)
		return win_nomemory();
	wdev->himgpalette = CreatePalette(wdev->limgpalette);

	return 0;
}

/* Make the output appear on the screen. */
int
win_sync_output(gx_device *dev)
{
    if (wdev->timer)
        KillTimer(wdev->hwndimgchild, TIMER_ID);
    wdev->timer = FALSE;
    wdev->update = FALSE;
    if (gsview) {
	SendMessage(gsview_hwnd, WM_GSVIEW, SYNC_OUTPUT, (LPARAM)NULL);
    }
    else {
	if ( !IsWindow(wdev->hwndimg) ) {  /* some clod closed the window */
		win_makeimg(wdev);
	}
	if ( !IsIconic(wdev->hwndimg) ) {  /* redraw window */
		InvalidateRect(wdev->hwndimg, NULL, 1);
		UpdateWindow(wdev->hwndimg);
	}
    }
    return(0);
}

/* Make the window visible, and display the output. */
int
win_output_page(gx_device *dev, int copies, int flush)
{
	if (wdev->timer)
	    KillTimer(wdev->hwndimgchild, TIMER_ID);
	wdev->timer = FALSE;
	wdev->update = FALSE;

	if (gsview) {
	    SendMessage(gsview_hwnd, WM_GSVIEW, OUTPUT_PAGE, (LPARAM)NULL);
	    gsview_next = FALSE;
	    /* wait for NEXT_PAGE message */
	    while (!gsview_next)
		gp_check_interrupts();
	    return(0);
	}
	else {
	    if (IsIconic(wdev->hwndimg))    /* useless as an Icon so fix it */
		ShowWindow(wdev->hwndimg, SW_SHOWNORMAL);
	    BringWindowToTop(wdev->hwndimg);
	}
	return( win_sync_output(dev) );
}

/* Close the win driver */
int
win_close(gx_device *dev)
{
	/* Free resources */
	if (wdev->timer)
	    KillTimer(wdev->hwndimgchild, TIMER_ID);
	wdev->timer = FALSE;
	wdev->update = FALSE;

	DeleteObject(wdev->himgpalette);
	gs_free((char *)(wdev->limgpalette), 1, sizeof(LOGPALETTE) + 
		(1<<(wdev->color_info.depth)) * sizeof(PALETTEENTRY),
		"win_close");
	if (gsview)
		DestroyWindow(wdev->hwndimgchild);
	else
		DestroyWindow(wdev->hwndimg);
	gp_check_interrupts();	/* process WIN_DESTROY message */

	return(0);
}

/* Map a r-g-b color to the colors available under Windows */
gx_color_index
win_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{
	switch(dev->color_info.depth) {
	  case 8: {
		int i;
		LPLOGPALETTE lpal = wdev->limgpalette;
		PALETTEENTRY *pep;
		byte cr, cg, cb;

		/* map colors to 0->255 in 32 steps */
		cr = win_color_value(r);
		cg = win_color_value(g);
		cb = win_color_value(b);

		/* search in palette */
		for ( i = 0, pep = &lpal->palPalEntry[i];
		      i < wdev->nColors; i++, pep++
		    )
		{	if ( !((cr ^ pep->peRed) & 0xf8) &&
			     !((cg ^ pep->peGreen) & 0xf8) &&
			     !((cb ^ pep->peBlue) & 0xf8)
			   )
				return((gx_color_index)i);	/* found it */
		}
		
		/* next try adding it to palette */
		if (i < 220) { /* allow 36 for windows and other apps */
			LPLOGPALETTE lipal = wdev->limgpalette;
			wdev->nColors = i+1;

			DeleteObject(wdev->himgpalette);
	 		lipal->palPalEntry[i].peFlags = 0;
			lipal->palPalEntry[i].peRed   =  cr;
			lipal->palPalEntry[i].peGreen =  cg;
			lipal->palPalEntry[i].peBlue  =  cb;
			lipal->palNumEntries = wdev->nColors;
			wdev->himgpalette = CreatePalette(lipal);

			return((gx_color_index)i);	/* return new palette index */
		}

		return(gx_no_color_index);  /* not found - dither instead */
		}
	  case 4:
		if ((r == g) && (g == b) && (r >= gx_max_color_value / 3 * 2 - 1)
		   && (r < gx_max_color_value / 4 * 3))
			return ((gx_color_index)8);	/* light gray */
		return pc_4bit_map_rgb_color(dev, r, g, b);
	}
	return (gx_default_map_rgb_color(dev,r,g,b));
}

/* Map a color code to r-g-b. */
int
win_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{
gx_color_value one;
	switch(dev->color_info.depth) {
	  case 8:
		one = (gx_color_value) (gx_max_color_value / 255);
		prgb[0] = wdev->limgpalette->palPalEntry[(int)color].peRed * one;
		prgb[1] = wdev->limgpalette->palPalEntry[(int)color].peGreen * one;
		prgb[2] = wdev->limgpalette->palPalEntry[(int)color].peBlue * one;
		break;
	  case 4:
		if (color == 8)	/* VGA light gray */
		    prgb[0] = prgb[1] = prgb[2] = (gx_max_color_value / 4 * 3);
		else
		    pc_4bit_map_color_rgb(dev, color, prgb);
		break;
	  default:
		prgb[0] = prgb[1] = prgb[2] = 
			(int)color ? gx_max_color_value : 0;
	}
	return 0;
}

/* Standard properties for windows. */
private const gs_prop_item win_props_std[] = {
	prop_def("HWResolution", prt_float_array),
	prop_def("HWSize", prt_int_array),
		/* Slots for arrays */
	prop_float, prop_float,
	prop_int, prop_int
};

/* Set window properties -- size and resolution. */
/* We implement this ourselves so that we can do it without */
/* closing and opening the device. */
int
win_put_props(gx_device *dev, gs_prop_item *plist, int count)
{	gs_prop_item *known[2];
	int code = 0;
	gx_device temp_dev;
	props_extract(plist, count, win_props_std, 2, known, 1);
	temp_dev = *dev;
	if ( known[1] != 0 )
	   {	if ( known[1]->value.a.size != 2 )
			known[1]->status = pv_typecheck,
			code = gs_error_typecheck;
		else
		   {	gs_prop_item *ap = known[1]->value.a.p.v;
			if ( ap[0].value.i <= 0 || ap[0].value.i > 0x7fff ||
			     ap[1].value.i <= 0 || ap[1].value.i > 0x7fff
			   )
				known[1]->status = pv_rangecheck,
				code = gs_error_rangecheck;
			else
			   {	temp_dev.width = ap[0].value.i;
				temp_dev.height = ap[1].value.i;
			   }
			if ( code == 0 ) code = 1;
		   }
	   }
	if ( known[0] != 0 )
	   {	if ( known[0]->value.a.size != 2 )
			known[0]->status = pv_typecheck,
			code = gs_error_typecheck;
		else
		   {	gs_prop_item *ap = known[0]->value.a.p.v;
			if ( ap[0].value.f <= 0 || ap[1].value.f <= 0 )
				known[0]->status = pv_rangecheck,
				code = gs_error_rangecheck;
			else
			   {	temp_dev.x_pixels_per_inch = ap[0].value.f;
				temp_dev.y_pixels_per_inch = ap[1].value.f;
			   }
			if ( code == 0 ) code = 1;
		   }
	   }
	if ( code < 0 )
		return_error(code);
	/* Hand off the change to the implementation. */
	if ( dev->is_open && code )
	{	int ccode;
		(*wdev->free_bitmap)(wdev);
		ccode = (*wdev->alloc_bitmap)(wdev, &temp_dev);
		if ( ccode < 0 )
		{	(*wdev->alloc_bitmap)(wdev, dev);
			return ccode;
		}
	}
	dev->x_pixels_per_inch = temp_dev.x_pixels_per_inch;
	dev->y_pixels_per_inch = temp_dev.y_pixels_per_inch;
	dev->width = temp_dev.width;
	dev->height = temp_dev.height;
	if (IsWindow(wdev->hwndimg) && IsWindow(wdev->hwndimgchild)) {
	    RECT rect;
	    /* erase bitmap - before window gets redrawn */
	    (*dev->procs->fill_rectangle)(dev, 0, 0, dev->width, dev->height,
		win_map_rgb_color(dev, gx_max_color_value, 
		    gx_max_color_value, gx_max_color_value));
	    /* cause scroll bars to be redrawn */
	    GetClientRect(wdev->hwndimgchild,&rect);
	    SendMessage(wdev->hwndimgchild, WM_SIZE, SIZE_RESTORED, 
		MAKELPARAM(rect.right-rect.left, rect.bottom-rect.top));
	}
	return 0;
}


/* ------ Internal routines ------ */

#undef wdev

/* make image window */
private void near 
win_makeimg(gx_device_win *wdev)
{	HMENU sysmenu;
	RECT rect;
	/* parent window */
	if (gsview) {
	    wdev->hwndimg = gsview_hwnd;
	    if (IsIconic(wdev->hwndimg))
	        ShowWindow(wdev->hwndimg, SW_SHOWNORMAL);
	}
	else {
	    wdev->hwndimg = CreateWindow(szImgName, (LPSTR)szImgName,
		  WS_OVERLAPPEDWINDOW,
		  CW_USEDEFAULT, CW_USEDEFAULT, 
		  CW_USEDEFAULT, CW_USEDEFAULT, 
		  NULL, NULL, phInstance, (void FAR *)wdev);
	    ShowWindow(wdev->hwndimg, SW_SHOWMINNOACTIVE);
	}
	/* child for graphics */
	GetClientRect(wdev->hwndimg, &rect);
	CreateWindow(szAppName, (LPSTR)szImgName,
		  WS_CHILD | WS_VSCROLL | WS_HSCROLL,
		  rect.left, rect.top,
		  rect.right-rect.left, rect.bottom-rect.top, 
		  wdev->hwndimg, NULL, phInstance, (void FAR *)wdev);
	ShowWindow(wdev->hwndimgchild, SW_SHOWNORMAL);

	if (!gsview) {
	    /* modify the menu to have the new items we want */
	    sysmenu = GetSystemMenu(wdev->hwndimg,0);	/* get the sysmenu */
	    AppendMenu(sysmenu, MF_SEPARATOR, 0, NULL);
	    AppendMenu(sysmenu, MF_STRING, M_COPY_CLIP, "Copy to Clip&board");
	}
}


/* out of memory error message box */
int
win_nomemory(void)
{
       	MessageBox(hwndtext,(LPSTR)"Not enough memory",(LPSTR) szAppName, MB_ICONSTOP);
	return gs_error_limitcheck;
}


LPLOGPALETTE
win_makepalette(gx_device_win *wdev)
{	int i, val;
	LPLOGPALETTE logpalette;
	logpalette = (LPLOGPALETTE)gs_malloc(1, sizeof(LOGPALETTE) + 
		(1<<(wdev->color_info.depth)) * sizeof(PALETTEENTRY),
		"win_makepalette");
	if (logpalette == (LPLOGPALETTE)NULL)
		return(0);
	logpalette->palVersion = 0x300;
	logpalette->palNumEntries = wdev->nColors;
	for (i=0; i<wdev->nColors; i++) {
	  logpalette->palPalEntry[i].peFlags = 0;
	  switch (wdev->nColors) {
		case 64:
		  /* colors are rrggbb */
		  logpalette->palPalEntry[i].peRed   = ((i & 0x30)>>4)*85;
		  logpalette->palPalEntry[i].peGreen = ((i & 0xC)>>2)*85;
		  logpalette->palPalEntry[i].peBlue  = (i & 3)*85;
		  break;
		case 16:
		  /* colors are irgb */
		  val = (i & 8 ? 255 : 128);
		  logpalette->palPalEntry[i].peRed   = i & 4 ? val : 0;
		  logpalette->palPalEntry[i].peGreen = i & 2 ? val : 0;
		  logpalette->palPalEntry[i].peBlue  = i & 1 ? val : 0;
		  if (i == 8) { /* light gray */
		      logpalette->palPalEntry[i].peRed   = 
		      logpalette->palPalEntry[i].peGreen = 
		      logpalette->palPalEntry[i].peBlue  = 192;
		  }
		  break;
		case 2:
		  logpalette->palPalEntry[i].peRed =
		    logpalette->palPalEntry[i].peGreen =
		    logpalette->palPalEntry[i].peBlue = (i ? 255 : 0);
		  break;
	  }
	}
	return(logpalette);
}


void
win_update(gx_device_win *wdev)
{
	wdev->update = TRUE;
	if (!wdev->timer) {
		SetTimer(wdev->hwndimgchild, TIMER_ID, repaint_interval, NULL);
		wdev->timer = TRUE;
  	}
}


/* child graphics window */
LRESULT CALLBACK _export
WndImgChildProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{	gx_device_win *wdev;
	HDC hdc;
	PAINTSTRUCT ps;
	RECT rect;
	HPALETTE oldpalette;
	int nVscrollInc, nHscrollInc;

	wdev = (gx_device_win *)GetWindowLong(hwnd, 0);

	switch(message) {
		case WM_CREATE:
			wdev =  (gx_device_win *)(((CREATESTRUCT *)lParam)->lpCreateParams);
			SetWindowLong(hwnd, 0, (LONG)wdev);
			wdev->hwndimgchild = hwnd;
			if (gsview)
			    SendMessage(gsview_hwnd, WM_GSVIEW, HWND_IMGCHILD, (LPARAM)wdev->hwndimgchild);
#if WINVER >= 0x030a
			/* Enable Drag Drop */
			if (is_win31)
				DragAcceptFiles(hwnd, TRUE);
#endif
			break;
		case WM_GSVIEW:
			switch(wParam) {
				case NEXT_PAGE:
					gsview_next = TRUE;
					return 0;
				case COPY_CLIPBOARD:
					(*wdev->copy_to_clipboard)(wdev);
					return 0;
			}
			break;
		case WM_TIMER:
			if ((wParam == TIMER_ID) && wdev->update) {
				InvalidateRect(wdev->hwndimgchild, (LPRECT)NULL, FALSE);
				UpdateWindow(wdev->hwndimgchild);
				wdev->update = FALSE;
			}
			break;
		case WM_SIZE:
			if (wParam == SIZE_MINIMIZED)
				return(0);
			wdev->cyClient = HIWORD(lParam);
			wdev->cxClient = LOWORD(lParam);

			wdev->cyAdjust = min(wdev->height, wdev->cyClient) - wdev->cyClient;
			wdev->cyClient += wdev->cyAdjust;

			wdev->nVscrollMax = max(0, wdev->height - wdev->cyClient);
			wdev->nVscrollPos = min(wdev->nVscrollPos, wdev->nVscrollMax);

			SetScrollRange(hwnd, SB_VERT, 0, wdev->nVscrollMax, FALSE);
			SetScrollPos(hwnd, SB_VERT, wdev->nVscrollPos, TRUE);

			wdev->cxAdjust = min(wdev->width,  wdev->cxClient) - wdev->cxClient;
			wdev->cxClient += wdev->cxAdjust;

			wdev->nHscrollMax = max(0, wdev->width - wdev->cxClient);
			wdev->nHscrollPos = min(wdev->nHscrollPos, wdev->nHscrollMax);

			SetScrollRange(hwnd, SB_HORZ, 0, wdev->nHscrollMax, FALSE);
			SetScrollPos(hwnd, SB_HORZ, wdev->nHscrollPos, TRUE);

			if (gsview && wdev->cxAdjust > 0)	/* don't make gsview window grow */
				wdev->cxAdjust = 0;
			if (gsview && wdev->cyAdjust > 0)
				wdev->cyAdjust = 0;

			if ((wParam==SIZENORMAL) && (wdev->cxAdjust!=0 || wdev->cyAdjust!=0)) {
			    GetWindowRect(GetParent(hwnd),&rect);
			    MoveWindow(GetParent(hwnd),rect.left,rect.top,
				rect.right-rect.left+wdev->cxAdjust,
				rect.bottom-rect.top+wdev->cyAdjust, TRUE);
			    wdev->cxAdjust = wdev->cyAdjust = 0;
			}
			if (gsview)
				SendMessage(gsview_hwnd, WM_GSVIEW, SCROLL_POSITION, 
					MAKELPARAM(wdev->nHscrollPos, wdev->nVscrollPos));
			return(0);
		case WM_VSCROLL:
			switch(LOWORD(wParam)) {
				case SB_TOP:
					nVscrollInc = -wdev->nVscrollPos;
					break;
				case SB_BOTTOM:
					nVscrollInc = wdev->nVscrollMax - wdev->nVscrollPos;
					break;
				case SB_LINEUP:
					nVscrollInc = -wdev->cyClient/16;
					break;
				case SB_LINEDOWN:
					nVscrollInc = wdev->cyClient/16;
					break;
				case SB_PAGEUP:
					nVscrollInc = min(-1,-wdev->cyClient);
					break;
				case SB_PAGEDOWN:
					nVscrollInc = max(1,wdev->cyClient);
					break;
				case SB_THUMBPOSITION:
					nVscrollInc = LOWORD(lParam) - wdev->nVscrollPos;
					break;
				default:
					nVscrollInc = 0;
				}
			if ((nVscrollInc = max(-wdev->nVscrollPos, 
				min(nVscrollInc, wdev->nVscrollMax - wdev->nVscrollPos)))!=0) {
				wdev->nVscrollPos += nVscrollInc;
				ScrollWindow(hwnd,0,-nVscrollInc,NULL,NULL);
				SetScrollPos(hwnd,SB_VERT,wdev->nVscrollPos,TRUE);
				UpdateWindow(hwnd);
			}
			if (gsview)
				SendMessage(gsview_hwnd, WM_GSVIEW, SCROLL_POSITION, 
					MAKELPARAM(wdev->nHscrollPos, wdev->nVscrollPos));
			return(0);
		case WM_HSCROLL:
			switch(LOWORD(wParam)) {
				case SB_LINEUP:
					nHscrollInc = -wdev->cxClient/16;
					break;
				case SB_LINEDOWN:
					nHscrollInc = wdev->cyClient/16;
					break;
				case SB_PAGEUP:
					nHscrollInc = min(-1,-wdev->cxClient);
					break;
				case SB_PAGEDOWN:
					nHscrollInc = max(1,wdev->cxClient);
					break;
				case SB_THUMBPOSITION:
					nHscrollInc = LOWORD(lParam) - wdev->nHscrollPos;
					break;
				default:
					nHscrollInc = 0;
				}
			if ((nHscrollInc = max(-wdev->nHscrollPos, 
				min(nHscrollInc, wdev->nHscrollMax - wdev->nHscrollPos)))!=0) {
				wdev->nHscrollPos += nHscrollInc;
				ScrollWindow(hwnd,-nHscrollInc,0,NULL,NULL);
				SetScrollPos(hwnd,SB_HORZ,wdev->nHscrollPos,TRUE);
				UpdateWindow(hwnd);
			}
			if (gsview)
				SendMessage(gsview_hwnd, WM_GSVIEW, SCROLL_POSITION, 
					MAKELPARAM(wdev->nHscrollPos, wdev->nVscrollPos));
			return(0);
		case WM_KEYDOWN:
			switch(LOWORD(wParam)) {
				case VK_HOME:
					SendMessage(hwnd,WM_VSCROLL,SB_TOP,0L);
					break;
				case VK_END:
					SendMessage(hwnd,WM_VSCROLL,SB_BOTTOM,0L);
					break;
				case VK_PRIOR:
					SendMessage(hwnd,WM_VSCROLL,SB_PAGEUP,0L);
					break;
				case VK_NEXT:
					SendMessage(hwnd,WM_VSCROLL,SB_PAGEDOWN,0L);
					break;
				case VK_UP:
					SendMessage(hwnd,WM_VSCROLL,SB_LINEUP,0L);
					break;
				case VK_DOWN:
					SendMessage(hwnd,WM_VSCROLL,SB_LINEDOWN,0L);
					break;
				case VK_LEFT:
					SendMessage(hwnd,WM_HSCROLL,SB_PAGEUP,0L);
					break;
				case VK_RIGHT:
					SendMessage(hwnd,WM_HSCROLL,SB_PAGEDOWN,0L);
					break;
			}
			return(0);
		case WM_PAINT:
			{
			int sx,sy,wx,wy,dx,dy;
			hdc = BeginPaint(hwnd, &ps);
			oldpalette = SelectPalette(hdc,wdev->himgpalette,NULL);
			RealizePalette(hdc);
			SetMapMode(hdc, MM_TEXT);
			SetBkMode(hdc,OPAQUE);
			GetClientRect(hwnd, &rect);
			SetViewportExt(hdc, rect.right, rect.bottom);
			dx = rect.left;	/* destination */
			dy = rect.top;
			wx = rect.right-rect.left; /* width */
			wy = rect.bottom-rect.top;
			sx = rect.left;	/* source */
			sy = rect.top;
			sx += wdev->nHscrollPos; /* scrollbars */
			sy += wdev->nVscrollPos;	
			if (sx+wx > wdev->width)
				wx = wdev->width - sx;
			if (sy+wy > wdev->height)
				wy = wdev->height - sy;
			(*wdev->repaint)(wdev,hdc,dx,dy,wx,wy,sx,sy);
			SelectPalette(hdc,oldpalette,NULL);
			EndPaint(hwnd, &ps);
			return 0;
			}
#if WINVER >= 0x030a
		case WM_DROPFILES:
			if (is_win31)
				SendMessage(hwndtext, message, wParam, lParam);
		case WM_DESTROY:
			/* disable Drag Drop */
			if (is_win31)
				DragAcceptFiles(hwnd, FALSE);
			break;
#endif
	}

not_ours:
	return DefWindowProc((HWND)hwnd, (WORD)message, (WORD)wParam, (DWORD)lParam);
}

/* parent overlapped window */
LRESULT CALLBACK _export
WndImgProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	gx_device_win *wdev;

	wdev = (gx_device_win *)GetWindowLong(hwnd, 0);

	switch(message) {
		case WM_CREATE:
			wdev =  (gx_device_win *)(((CREATESTRUCT *)lParam)->lpCreateParams);
			wdev->hwndimg = hwnd;
			SetWindowLong(hwnd, 0, (LONG)wdev);
#if WINVER >= 0x030a
			/* Enable Drag Drop */
			if (is_win31)
				DragAcceptFiles(hwnd, TRUE);
#endif
			break;
		case WM_SYSCOMMAND:
			switch(LOWORD(wParam)) {
				case M_COPY_CLIP:
					(*wdev->copy_to_clipboard)(wdev);
					return 0;
			}
			break;
		case WM_KEYDOWN:
		case WM_KEYUP:
			if (wdev->hwndimgchild) {
				SendMessage(wdev->hwndimgchild, message, wParam, lParam);
				return 0;
			}
			break;
		case WM_SIZE:
			if (wParam != SIZE_MINIMIZED  && wdev->hwndimgchild!=(HWND)NULL)
			    SetWindowPos(wdev->hwndimgchild, (HWND)NULL, 0, 0,
				LOWORD(lParam), HIWORD(lParam), 
				SWP_NOZORDER | SWP_NOACTIVATE);
			return(0);
#if WINVER >= 0x030a
		case WM_DROPFILES:
			if (is_win31)
				SendMessage(hwndtext, message, wParam, lParam);
			break;
		case WM_DESTROY:
			if (is_win31)
				DragAcceptFiles(hwnd, FALSE);
#endif
			break;
	}
	return DefWindowProc(hwnd, message, wParam, lParam);
}


/* $Header: /a/cvs/386BSD/ports/editor/point/repaint.c,v 1.1 1994/02/15 22:12:39 jkh Exp $ */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "pt.h"
#include <X11/StringDefs.h>

void
SetTextColor( w, setWindowBackground, foreground, colorName, gc )
	struct window *w;
	int setWindowBackground, foreground;
	char * colorName;
	GC gc;
{
	extern Display *MainDisplay;
	extern Tcl_Interp * pointsMainInterp;

	XColor * color = Tk_GetColor( pointsMainInterp, w->tk_toplevel,
				(Colormap)NULL, Tk_GetUid(colorName) );
	if( color == NULL )
		return;

	if( foreground ) {
		XSetForeground( MainDisplay, gc, color->pixel );
	} else {
		XSetBackground( MainDisplay, gc, color->pixel);
	}

	if( setWindowBackground ) {
		Tk_SetWindowBackground( w->tk_text, color->pixel );
	}
}

void InitRedisplay( w )
	struct window *w;
{
	extern Display *MainDisplay;
	extern char * textBackground;
	extern char * textForeground;
	extern char * deSelectedTextBackground;
	extern char * deSelectedTextForeground;
	extern char * selectedTextBackground;
	extern char * selectedTextForeground;
	extern Tcl_Interp * pointsMainInterp;
	extern char msgBuffer[];

	XFontStruct *fontInfo;
	struct fontDataStruct *fontData = &(w->font);

	if( w->x_window_id == (Window)NULL ) {
		sprintf( msgBuffer, "%s.vScrollAndText.text", w->tk_pathname );
		w->tk_text = Tk_NameToWindow(pointsMainInterp, msgBuffer,
							w->tk_toplevel);
		w->x_window_id = Tk_WindowId( w->tk_text );
	}
	if( w->ln_window_id == (Window)NULL ) {
		Tk_Window ln_w;
		sprintf( msgBuffer, "%s.vScrollAndText.lineNumbers",
							w->tk_pathname );
		ln_w = Tk_NameToWindow(pointsMainInterp, msgBuffer,
							w->tk_toplevel);
		w->ln_window_id = Tk_WindowId( ln_w );
	}

	if( fontData->height == 0 && w->x_window_id != (Window)NULL ) {
	retry_font:
		fontInfo = Tk_GetFontStruct( pointsMainInterp, w->tk_toplevel,
						Tk_GetUid(fontData->name) );
		if( fontInfo == NULL ) {
			printf("Cannot load font %s, using fixed\n",
				fontData->name );
			fontData->name = "fixed";
			goto retry_font;
		}
		fontData->font = fontInfo->fid;
		fontData->height = fontInfo->ascent + fontInfo->descent;
		fontData->width = fontInfo->max_bounds.width;
		fontData->ascent = fontInfo->ascent;

		/* set up the gc for normal text */
		XSetFont(MainDisplay, fontData->gc_normal, fontData->font);

		/* set up the gc for selected text */
		XSetFont(MainDisplay, fontData->gc_selected, fontData->font);
		XSetFont(MainDisplay, fontData->gc_deselected, fontData->font);
		
		/* set the foreground and background colors for all the GCs */
		SetTextColor( w, 0, 1, textForeground,
						fontData->gc_normal );
		SetTextColor( w, 1, 0, textBackground,
						fontData->gc_normal );
		SetTextColor( w, 0, 1, selectedTextForeground,
						fontData->gc_selected );
		SetTextColor( w, 0, 0, selectedTextBackground,
						fontData->gc_selected );
		SetTextColor( w, 0, 1, deSelectedTextForeground,
						fontData->gc_deselected );
		SetTextColor( w, 0, 0, deSelectedTextBackground,
						fontData->gc_deselected );
		
		Tk_FreeFontStruct( fontInfo );
	}
}

void
WorkspaceResized( w )
	struct window * w;
{
	extern int showPartialLines;

	int font_height, font_width;
	int high, wide;

	InitRedisplay( w );
	wide = Tk_Width( w->tk_text );
	high = Tk_Height( w->tk_text );

	/* These calculations do not allow partially hidden rows and columns */
	if( (font_height = (w->font).height) == 0 )
		font_height = 15;
	w->nRows = (high - w->topMargin) / font_height;
	if( (font_width = (w->font).width) == 0 )
		font_width = 8;
	w->nCols = (wide - w->leftMargin) / font_width;
	if( showPartialLines ) {
		w->nRows = (high - w->topMargin + font_height - 1)
						/ font_height;
		w->nCols = (wide - w->leftMargin + font_width - 1)
						/ font_width;
	}
#ifdef FIX_DISPLAY_LATER
	/* reallocate the screen image and set to nuls */
	PtFree( w->screen_image );
	high = w->nRows * w->nCols;
	w->screen_image = (char *)PtMalloc( high, "screen image" );
	memset( w->screen_image, '\0', high );
#endif
}

void
repaint( w, left, top, right, bottom )
	struct window * w;
	int left;
	int top;
	int right;
	int bottom;
{
	int firstRow, lastRow;
	int firstCol, lastCol;

	InitRedisplay( w );
	if( w->nRows == 0 )
		WorkspaceResized( w );

	/* this is necessary when the whole window is exposed */
	if( top < w->topMargin )
		top = w->topMargin;
	if( bottom < w->topMargin )
		bottom = w->topMargin;
	if( left < w->leftMargin )
		left = w->leftMargin;
	if( right < w->topMargin )
		right = w->leftMargin;
	firstRow =    (top - w->topMargin) / (w->font).height;
	lastRow  = (bottom - w->topMargin) / (w->font).height;
	/* never write a partial line at the bottom of the screen */
	if( lastRow >= w->nRows )
		lastRow = w->nRows - 1;
	firstCol =  (left - w->leftMargin) / (w->font).width;
	lastCol  = (right - w->leftMargin) / (w->font).width;
	drawWindowFast( w, firstRow, lastRow, firstCol, lastCol, 1 );
}


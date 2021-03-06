/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#ifndef iv_xcursor_h
#define iv_xcursor_h

#include <IV-X11/Xlib.h>

#include <InterViews/_enter.h>

class Color;
class Display;
class WindowVisual;

class CursorRep {
public:
    const Color* fg_;
    const Color* bg_;
    Display* display_;
    XCursor xcursor_;

    CursorRep(const Color* fg, const Color* bg);
    virtual ~CursorRep();

    XCursor xid(Display*, WindowVisual*);

    virtual void make_xcursor(Display*, WindowVisual*) = 0;
    const Color* make_color(
	Display*, Style*,
	const char* str1, const char* str2, const char* str3,
	const char* default_value
    );
};

class CursorRepData : public CursorRep {
public:
    short x_, y_;
    const int* pat_;
    const int* mask_;

    CursorRepData(
	short x_hot, short y_hot, const int* pat, const int* mask,
	const Color* fg, const Color* bg
    );
    virtual ~CursorRepData();

    virtual void make_xcursor(Display*, WindowVisual*);
    Pixmap make_cursor_pixmap(XDisplay*, XWindow, const int* scanline);
};

class CursorRepBitmap : public CursorRep {
public:
    const Bitmap* pat_;
    const Bitmap* mask_;

    CursorRepBitmap(
	const Bitmap* pat, const Bitmap* mask,
	const Color* fg, const Color* bg
    );
    virtual ~CursorRepBitmap();

    virtual void make_xcursor(Display*, WindowVisual*);
};

class CursorRepFont : public CursorRep {
public:
    const Font* font_;
    int pat_;
    int mask_;

    CursorRepFont(
	const Font*, int pat, int mask, const Color* fg, const Color* bg
    );
    virtual ~CursorRepFont();

    virtual void make_xcursor(Display*, WindowVisual*);
};

class CursorRepXFont : public CursorRep {
public:
    int code_;

    CursorRepXFont(int code, const Color* fg, const Color* bg);
    virtual ~CursorRepXFont();

    virtual void make_xcursor(Display*, WindowVisual*);
};

#include <InterViews/_leave.h>

#endif

/*
 * Copyright 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#ifndef _AxeiiSinkP_h
#define _AxeiiSinkP_h

#include <X11/IntrinsicP.h>
#include <X11/Xaw/AsciiSinkP.h> 
#include <AxeiiSink.h>

typedef struct _AxeiiSinkClassPart {
  int foo;
} AxeiiSinkClassPart;

typedef struct _AxeiiSinkClassRec {
    ObjectClassPart     object_class;
    TextSinkClassPart	text_sink_class;
    AsciiSinkClassPart	ascii_sink_class;
    AxeiiSinkClassPart  axeii_sink_class;
} AxeiiSinkClassRec;

extern AxeiiSinkClassRec axeiiSinkClassRec;

typedef struct {
    /* resources     */
     Boolean         block_cursor;
     String          cursor_bitmap;

    /* private state */
     unsigned int    cursor_width;
     unsigned int    cursor_height;
} AxeiiSinkPart;

typedef struct _AxeiiSinkRec {
    ObjectPart          object;
    TextSinkPart	text_sink;
    AsciiSinkPart	ascii_sink;
    AxeiiSinkPart       axeii_sink;
} AxeiiSinkRec;

#endif /* _AxeiiSinkP_h */


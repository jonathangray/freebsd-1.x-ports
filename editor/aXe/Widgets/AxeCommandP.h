/*
 * Copyright 1992 The University of Newcastle upon Tyne
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

#ifndef _AxeCommandP_h
#define _AxeCommandP_h

#include <X11/Xaw/CommandP.h>
#include <AxeCommand.h>

typedef struct _AxeCommandClass 
{
    XtPointer      extension;
    XtTranslations translations;
} AxeCommandClassPart;

typedef struct _AxeCommandClassRec {
    CoreClassPart	    core_class;
    SimpleClassPart	    simple_class;
#ifdef XAW3D
    ThreeDClassPart         threeD_class;
#endif    
    LabelClassPart	    label_class;
    CommandClassPart	    command_class;
    AxeCommandClassPart     axe_command_class;
} AxeCommandClassRec;

extern AxeCommandClassRec axeCommandClassRec;

typedef struct {
    String     help;
} AxeCommandPart;

typedef struct _AxeCommandRec {
    CorePart            core;
    SimplePart	        simple;
#ifdef XAW3D
    ThreeDPart          threeD;
#endif
    LabelPart	        label;
    CommandPart	        command;
    AxeCommandPart      axeCommand;
} AxeCommandRec;

#endif /* _AxeCommandP_h */



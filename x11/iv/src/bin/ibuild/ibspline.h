/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * ISplineComp declarations
 */

#ifndef ibspline_h
#define ibspline_h

#include "ibcode.h"
#include "ibcomp.h"
#include <Unidraw/Components/spline.h>

class ISplineComp : public IComp {
public:
    ISplineComp(SFH_OpenBSpline* = nil);

    virtual ClassId GetSubstId(const char*& delim);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SplineCode : public GraphicCodeView {
public:
    SplineCode(ISplineComp* = nil);

    ISplineComp* GetISplineComp();
    virtual boolean Definition(ostream&);

    virtual void Update();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean GCoreConstDecls(ostream&);
    virtual boolean GCoreConstInits(ostream&);
    virtual boolean GConstDecls(ostream&);
    virtual boolean GConstInits(ostream&);

    virtual const char* GetGHeader();
    virtual const char* GetCVHeader();
};

class IClosedSplineComp : public ISplineComp {
public:
    IClosedSplineComp(SFH_ClosedBSpline* = nil);

    virtual ClassId GetSubstId(const char*& delim);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ClosedSplineCode : public SplineCode {
public:
    ClosedSplineCode(IClosedSplineComp* = nil);

    IClosedSplineComp* GetIClosedSplineComp();
    virtual boolean Definition(ostream&);

    virtual void Update();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean GCoreConstDecls(ostream&);
    virtual boolean GCoreConstInits(ostream&);
    virtual boolean GConstDecls(ostream&);
    virtual boolean GConstInits(ostream&);
};

#endif

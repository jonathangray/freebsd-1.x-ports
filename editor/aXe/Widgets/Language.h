#ifndef Interp_h
#define Interp_h

#include <X11/Intrinsic.h>
#include <tcl.h>

typedef struct
{
    void       (* Initialize)();
    Tcl_Interp *interp;
    void       (* SetBuffer)();
    Widget     buffer;
} InterpRec;

extern InterpRec interpreter;

#endif

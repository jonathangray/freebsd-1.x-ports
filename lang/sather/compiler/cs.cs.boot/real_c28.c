/* real_c28.c : Sather class: REAL_CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr REA107_create_(ptr self__, ptr v__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int REA28_print_indent_ = 2;
ptr REA28_create_(ptr self__, ptr v__);
void REA28_out_of_line_(ptr self__, ptr fn__);
ptr REA28_dup_(ptr self__);
void REA28_put_kwdname_(ptr self__, int nm__);
ptr REA28_sather_code_(ptr self__);
ptr REA28_initialize_(ptr self__, ptr initarg__);
ptr REA28_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_REA28[];

ptr REA28_create_(ptr self__, ptr v__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(28,0);
   PATT_(res__,12) = (ptr)v__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void REA28_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr REA28_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void REA28_put_kwdname_(ptr self__, int nm__)
{
   ptr gl558_;
   static int gl559_;
   static union dtype_ gl560_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl558_ = x__;
   cache_dispatch_(gl558_,796,gl559_,INTVAL_(gl560_));
   IATT_(gl558_,INTVAL_(gl560_)) = (int)nm__;

   ret0__:
   return;
}

ptr REA28_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr REA28_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr REA28_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)REA107_create_(res__,PATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


/* char_c26.c : Sather class: CHAR_CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr CHA105_create_(ptr self__, ptr v__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int CHA26_print_indent_ = 2;
ptr CHA26_create_(ptr self__, ptr v__);
void CHA26_out_of_line_(ptr self__, ptr fn__);
ptr CHA26_dup_(ptr self__);
void CHA26_put_kwdname_(ptr self__, int nm__);
ptr CHA26_sather_code_(ptr self__);
ptr CHA26_initialize_(ptr self__, ptr initarg__);
ptr CHA26_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_CHA26[];

ptr CHA26_create_(ptr self__, ptr v__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(26,0);
   PATT_(res__,12) = (ptr)v__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void CHA26_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CHA26_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void CHA26_put_kwdname_(ptr self__, int nm__)
{
   ptr gl552_;
   static int gl553_;
   static union dtype_ gl554_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl552_ = x__;
   cache_dispatch_(gl552_,796,gl553_,INTVAL_(gl554_));
   IATT_(gl552_,INTVAL_(gl554_)) = (int)nm__;

   ret0__:
   return;
}

ptr CHA26_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CHA26_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr CHA26_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)CHA105_create_(res__,PATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


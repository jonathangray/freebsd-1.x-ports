/* return53.c : Sather class: RETURN_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr RET144_create_(ptr self__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int RET53_print_indent_ = 2;
ptr RET53_create_(ptr self__);
void RET53_out_of_line_(ptr self__, ptr fn__);
ptr RET53_dup_(ptr self__);
void RET53_put_kwdname_(ptr self__, int nm__);
ptr RET53_sather_code_(ptr self__);
ptr RET53_initialize_(ptr self__, ptr initarg__);
ptr RET53_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_RET53[];

ptr RET53_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(53,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void RET53_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr RET53_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void RET53_put_kwdname_(ptr self__, int nm__)
{
   ptr gl801_;
   static int gl802_;
   static union dtype_ gl803_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl801_ = x__;
   cache_dispatch_(gl801_,796,gl802_,INTVAL_(gl803_));
   IATT_(gl801_,INTVAL_(gl803_)) = (int)nm__;

   ret0__:
   return;
}

ptr RET53_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr RET53_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr RET53_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)RET144_create_(res__,IATT_(self__,4));

   ret0__:
   return (res__);
}


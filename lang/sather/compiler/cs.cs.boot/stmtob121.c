/* stmtob121.c : Sather class: STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STM199_create_(ptr self__, int ln__);
#include "macros_.h"



/*constant*/ int STM121_print_indent_ = 2;
ptr STM121_create_(ptr self__);
void STM121_out_of_line_(ptr self__, ptr fn__);
ptr STM121_dup_(ptr self__);
void STM121_put_kwdname_(ptr self__, int nm__);
ptr STM121_sather_code_(ptr self__);
ptr STM121_initialize_(ptr self__, ptr initarg__);
ptr STM121_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_STM121[];

ptr STM121_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(121,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void STM121_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr STM121_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void STM121_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3010_;
   static int gl3011_;
   static union dtype_ gl3012_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3010_ = x__;
   cache_dispatch_(gl3010_,796,gl3011_,INTVAL_(gl3012_));
   IATT_(gl3010_,INTVAL_(gl3012_)) = (int)nm__;

   ret0__:
   return;
}

ptr STM121_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr STM121_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STM121_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)STM199_create_(0,IATT_(self__,4));

   ret0__:
   return (res__);
}


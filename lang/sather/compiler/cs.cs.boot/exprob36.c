/* exprob36.c : Sather class: EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr EXP117_create_(ptr self__, int ln__);
#include "macros_.h"



/*constant*/ int EXP36_print_indent_ = 2;
ptr EXP36_create_(ptr self__);
void EXP36_out_of_line_(ptr self__, ptr fn__);
ptr EXP36_dup_(ptr self__);
void EXP36_put_kwdname_(ptr self__, int nm__);
ptr EXP36_sather_code_(ptr self__);
ptr EXP36_initialize_(ptr self__, ptr initarg__);
ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_EXP36[];

ptr EXP36_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(36,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void EXP36_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr EXP36_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void EXP36_put_kwdname_(ptr self__, int nm__)
{
   ptr gl606_;
   static int gl607_;
   static union dtype_ gl608_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl606_ = x__;
   cache_dispatch_(gl606_,796,gl607_,INTVAL_(gl608_));
   IATT_(gl606_,INTVAL_(gl608_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXP36_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXP36_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)EXP117_create_(0,IATT_(self__,4));

   ret0__:
   return (res__);
}


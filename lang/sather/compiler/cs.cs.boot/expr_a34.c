/* expr_a34.c : Sather class: EXPR_ARGS_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr EXP113_create_(ptr self__, ptr o__, int f__, ptr alst__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST37_pcopy_(ptr self__, ptr pl__, ptr pi__);
#include "macros_.h"



/*constant*/ int EXP34_print_indent_ = 2;
ptr EXP34_create_(ptr self__, ptr o__, int f__, ptr alst__);
void EXP34_out_of_line_(ptr self__, ptr fn__);
ptr EXP34_dup_(ptr self__);
void EXP34_put_kwdname_(ptr self__, int nm__);
ptr EXP34_sather_code_(ptr self__);
ptr EXP34_initialize_(ptr self__, ptr initarg__);
ptr EXP34_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_EXP34[];

ptr EXP34_create_(ptr self__, ptr o__, int f__, ptr alst__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(34,0);
   PATT_(res__,12) = (ptr)o__;
   IATT_(res__,16) = (int)f__;
   PATT_(res__,20) = (ptr)alst__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void EXP34_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr EXP34_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void EXP34_put_kwdname_(ptr self__, int nm__)
{
   ptr gl579_;
   static int gl580_;
   static union dtype_ gl581_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl579_ = x__;
   cache_dispatch_(gl579_,796,gl580_,INTVAL_(gl581_));
   IATT_(gl579_,INTVAL_(gl581_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXP34_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXP34_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr EXP34_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl582_;
   static int gl583_;
   static union dtype_ gl584_;
   ptr gl8_;
   ptr gl585_;
   static int gl586_;
   static union dtype_ gl587_;
   ptr gl9_;

   if ((PATT_(self__,20) != 0)) {
      gl582_ = PATT_(self__,12);
      cache_dispatch_(gl582_,407,gl583_,INTVAL_(gl584_));
      gl8_ = PFN_(gl584_)(gl582_,pl__,pi__);
      res__ = (ptr)EXP113_create_(res__,gl8_,IATT_(self__,16),LST37_pcopy_(PATT_(self__,20),pl__,pi__),IATT_(self__,4));
   }
   else {
      gl585_ = PATT_(self__,12);
      cache_dispatch_(gl585_,407,gl586_,INTVAL_(gl587_));
      gl9_ = PFN_(gl587_)(gl585_,pl__,pi__);
      res__ = (ptr)EXP113_create_(res__,gl9_,IATT_(self__,16),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


/* cond_s49.c : Sather class: COND_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr CON140_create_(ptr self__, ptr t__, ptr tp__, ptr eip__, ptr ep__, int ln__);
#include "macros_.h"



/*constant*/ int CON49_print_indent_ = 2;
ptr CON49_create_(ptr self__, ptr t__, ptr tp__, ptr eip__, ptr ep__, int ln__);
void CON49_out_of_line_(ptr self__, ptr fn__);
ptr CON49_dup_(ptr self__);
void CON49_put_kwdname_(ptr self__, int nm__);
ptr CON49_sather_code_(ptr self__);
ptr CON49_initialize_(ptr self__, ptr initarg__);
ptr CON49_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_CON49[];

ptr CON49_create_(ptr self__, ptr t__, ptr tp__, ptr eip__, ptr ep__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(49,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)tp__;
   PATT_(res__,20) = (ptr)eip__;
   PATT_(res__,24) = (ptr)ep__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void CON49_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CON49_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void CON49_put_kwdname_(ptr self__, int nm__)
{
   ptr gl771_;
   static int gl772_;
   static union dtype_ gl773_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl771_ = x__;
   cache_dispatch_(gl771_,796,gl772_,INTVAL_(gl773_));
   IATT_(gl771_,INTVAL_(gl773_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON49_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON49_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr CON49_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl774_;
   static int gl775_;
   static union dtype_ gl776_;
   ptr gl32_;
   ptr gl777_;
   static int gl778_;
   static union dtype_ gl779_;
   ptr gl33_;

   if ((PATT_(self__,24) != 0)) {
      gl774_ = PATT_(self__,12);
      cache_dispatch_(gl774_,407,gl775_,INTVAL_(gl776_));
      gl32_ = PFN_(gl776_)(gl774_,pl__,pi__);
      res__ = (ptr)CON140_create_(res__,gl32_,LST38_pcopy_(PATT_(self__,16),pl__,pi__),LST38_pcopy_(PATT_(self__,20),pl__,pi__),LST38_pcopy_(PATT_(self__,24),pl__,pi__),IATT_(self__,4));
   }
   else {
      gl777_ = PATT_(self__,12);
      cache_dispatch_(gl777_,407,gl778_,INTVAL_(gl779_));
      gl33_ = PFN_(gl779_)(gl777_,pl__,pi__);
      res__ = (ptr)CON140_create_(res__,gl33_,LST38_pcopy_(PATT_(self__,16),pl__,pi__),LST38_pcopy_(PATT_(self__,20),pl__,pi__),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


/* assign50.c : Sather class: ASSIGN_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr ASS141_create_(ptr self__, ptr lv__, ptr exp__, int ln__);
#include "macros_.h"



/*constant*/ int ASS50_print_indent_ = 2;
ptr ASS50_create_(ptr self__, ptr lv__, ptr exp__, int ln__);
void ASS50_out_of_line_(ptr self__, ptr fn__);
ptr ASS50_dup_(ptr self__);
void ASS50_put_kwdname_(ptr self__, int nm__);
ptr ASS50_sather_code_(ptr self__);
ptr ASS50_initialize_(ptr self__, ptr initarg__);
ptr ASS50_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_ASS50[];

ptr ASS50_create_(ptr self__, ptr lv__, ptr exp__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(50,0);
   PATT_(res__,12) = (ptr)lv__;
   PATT_(res__,16) = (ptr)exp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void ASS50_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ASS50_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ASS50_put_kwdname_(ptr self__, int nm__)
{
   ptr gl780_;
   static int gl781_;
   static union dtype_ gl782_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl780_ = x__;
   cache_dispatch_(gl780_,796,gl781_,INTVAL_(gl782_));
   IATT_(gl780_,INTVAL_(gl782_)) = (int)nm__;

   ret0__:
   return;
}

ptr ASS50_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ASS50_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ASS50_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl783_;
   static int gl784_;
   static union dtype_ gl785_;
   ptr gl786_;
   static int gl787_;
   static union dtype_ gl788_;
   ptr gl34_;
   ptr gl35_;

   gl783_ = PATT_(self__,12);
   cache_dispatch_(gl783_,407,gl784_,INTVAL_(gl785_));
   gl34_ = PFN_(gl785_)(gl783_,pl__,pi__);
   gl786_ = PATT_(self__,16);
   cache_dispatch_(gl786_,407,gl787_,INTVAL_(gl788_));
   gl35_ = PFN_(gl788_)(gl786_,pl__,pi__);
   res__ = (ptr)ASS141_create_(res__,gl34_,gl35_,IATT_(self__,4));

   ret0__:
   return (res__);
}


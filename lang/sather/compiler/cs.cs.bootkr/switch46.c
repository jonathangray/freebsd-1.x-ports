/* switch46.c : Sather class: SWITCH_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr EXP36_pcopy_();
extern ptr LST38_pcopy_();
extern ptr LST40_pcopy_();
extern ptr SWI137_create_();
#include "macros_.h"



/*constant*/ int SWI46_print_indent_ = 2;
ptr SWI46_create_();
SWI46_out_of_line_();
ptr SWI46_dup_();
SWI46_put_kwdname_();
ptr SWI46_sather_code_();
ptr SWI46_initialize_();
ptr SWI46_pcopy_();
extern int attr_ent_SWI46[];

ptr SWI46_create_(self__,t__,wp__,ep__,ln__)
ptr self__;
ptr t__;
ptr wp__;
ptr ep__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(46,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)wp__;
   PATT_(res__,20) = (ptr)ep__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

SWI46_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr SWI46_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

SWI46_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl750_;
   static int gl751_;
   static union dtype_ gl752_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl750_ = x__;
   cache_dispatch_(gl750_,796,gl751_,INTVAL_(gl752_));
   IATT_(gl750_,INTVAL_(gl752_)) = (int)nm__;

   ret0__:
   return;
}

ptr SWI46_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SWI46_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr SWI46_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl753_;
   static int gl754_;
   static union dtype_ gl755_;
   ptr gl28_;
   ptr gl756_;
   static int gl757_;
   static union dtype_ gl758_;
   ptr gl29_;

   if ((PATT_(self__,20) != 0)) {
      gl753_ = PATT_(self__,12);
      cache_dispatch_(gl753_,407,gl754_,INTVAL_(gl755_));
      gl28_ = PFN_(gl755_)(gl753_,pl__,pi__);
      res__ = (ptr)SWI137_create_(res__,gl28_,LST40_pcopy_(PATT_(self__,16),pl__,pi__),LST38_pcopy_(PATT_(self__,20),pl__,pi__),IATT_(self__,4));
   }
   else {
      gl756_ = PATT_(self__,12);
      cache_dispatch_(gl756_,407,gl757_,INTVAL_(gl758_));
      gl29_ = PFN_(gl758_)(gl756_,pl__,pi__);
      res__ = (ptr)SWI137_create_(res__,gl29_,LST40_pcopy_(PATT_(self__,16),pl__,pi__),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


/* elsif_48.c : Sather class: ELSIF_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr ELS139_create_();
extern ptr EXP36_pcopy_();
extern ptr LST38_pcopy_();
#include "macros_.h"



/*constant*/ int ELS48_print_indent_ = 2;
ptr ELS48_create_();
ELS48_out_of_line_();
ptr ELS48_dup_();
ELS48_put_kwdname_();
ptr ELS48_sather_code_();
ptr ELS48_initialize_();
ptr ELS48_pcopy_();
extern int attr_ent_ELS48[];

ptr ELS48_create_(self__,t__,tp__,ln__)
ptr self__;
ptr t__;
ptr tp__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(48,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)tp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

ELS48_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ELS48_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

ELS48_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl765_;
   static int gl766_;
   static union dtype_ gl767_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl765_ = x__;
   cache_dispatch_(gl765_,796,gl766_,INTVAL_(gl767_));
   IATT_(gl765_,INTVAL_(gl767_)) = (int)nm__;

   ret0__:
   return;
}

ptr ELS48_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ELS48_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ELS48_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl768_;
   static int gl769_;
   static union dtype_ gl770_;
   ptr gl31_;

   gl768_ = PATT_(self__,12);
   cache_dispatch_(gl768_,407,gl769_,INTVAL_(gl770_));
   gl31_ = PFN_(gl770_)(gl768_,pl__,pi__);
   res__ = (ptr)ELS139_create_(res__,gl31_,LST38_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));

   ret0__:
   return (res__);
}


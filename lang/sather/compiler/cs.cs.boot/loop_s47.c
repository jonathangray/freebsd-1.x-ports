/* loop_s47.c : Sather class: LOOP_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LOO138_create_(ptr self__, ptr t__, ptr s__, int ln__);
#include "macros_.h"



/*constant*/ int LOO47_print_indent_ = 2;
ptr LOO47_create_(ptr self__, ptr t__, ptr s__, int ln__);
void LOO47_out_of_line_(ptr self__, ptr fn__);
ptr LOO47_dup_(ptr self__);
void LOO47_put_kwdname_(ptr self__, int nm__);
ptr LOO47_sather_code_(ptr self__);
ptr LOO47_initialize_(ptr self__, ptr initarg__);
ptr LOO47_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_LOO47[];

ptr LOO47_create_(ptr self__, ptr t__, ptr s__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(47,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)s__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void LOO47_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LOO47_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void LOO47_put_kwdname_(ptr self__, int nm__)
{
   ptr gl759_;
   static int gl760_;
   static union dtype_ gl761_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl759_ = x__;
   cache_dispatch_(gl759_,796,gl760_,INTVAL_(gl761_));
   IATT_(gl759_,INTVAL_(gl761_)) = (int)nm__;

   ret0__:
   return;
}

ptr LOO47_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LOO47_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LOO47_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl762_;
   static int gl763_;
   static union dtype_ gl764_;
   ptr gl30_;

   if ((PATT_(self__,12) != 0)) {
      gl762_ = PATT_(self__,12);
      cache_dispatch_(gl762_,407,gl763_,INTVAL_(gl764_));
      gl30_ = PFN_(gl764_)(gl762_,pl__,pi__);
      res__ = (ptr)LOO138_create_(res__,gl30_,LST38_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));
   }
   else {
      res__ = (ptr)LOO138_create_(res__,0,LST38_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


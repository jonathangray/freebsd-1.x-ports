/* aref_e32.c : Sather class: AREF_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST37_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr ARE111_create_(ptr self__, ptr arr__, ptr inds__, int ln__);
#include "macros_.h"



/*constant*/ int ARE32_print_indent_ = 2;
ptr ARE32_create_(ptr self__, ptr arr__, ptr inds__);
void ARE32_out_of_line_(ptr self__, ptr fn__);
ptr ARE32_dup_(ptr self__);
void ARE32_put_kwdname_(ptr self__, int nm__);
ptr ARE32_sather_code_(ptr self__);
ptr ARE32_initialize_(ptr self__, ptr initarg__);
ptr ARE32_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_ARE32[];

ptr ARE32_create_(ptr self__, ptr arr__, ptr inds__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(32,0);
   PATT_(res__,12) = (ptr)arr__;
   PATT_(res__,16) = (ptr)inds__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void ARE32_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ARE32_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ARE32_put_kwdname_(ptr self__, int nm__)
{
   ptr gl570_;
   static int gl571_;
   static union dtype_ gl572_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl570_ = x__;
   cache_dispatch_(gl570_,796,gl571_,INTVAL_(gl572_));
   IATT_(gl570_,INTVAL_(gl572_)) = (int)nm__;

   ret0__:
   return;
}

ptr ARE32_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ARE32_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ARE32_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl573_;
   static int gl574_;
   static union dtype_ gl575_;
   ptr gl7_;

   if ((PATT_(self__,12) != 0)) {
      gl573_ = PATT_(self__,12);
      cache_dispatch_(gl573_,407,gl574_,INTVAL_(gl575_));
      gl7_ = PFN_(gl575_)(gl573_,pl__,pi__);
      res__ = (ptr)ARE111_create_(res__,gl7_,LST37_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));
   }
   else {
      res__ = (ptr)ARE111_create_(res__,0,LST37_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


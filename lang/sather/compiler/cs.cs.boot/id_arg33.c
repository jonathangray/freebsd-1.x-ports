/* id_arg33.c : Sather class: ID_ARGS_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr ID_112_create_(ptr self__, int id__, ptr alst__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr LST37_pcopy_(ptr self__, ptr pl__, ptr pi__);
#include "macros_.h"



/*constant*/ int ID_33_print_indent_ = 2;
ptr ID_33_create_(ptr self__, int f__, ptr alst__);
void ID_33_out_of_line_(ptr self__, ptr fn__);
ptr ID_33_dup_(ptr self__);
void ID_33_put_kwdname_(ptr self__, int nm__);
ptr ID_33_sather_code_(ptr self__);
ptr ID_33_initialize_(ptr self__, ptr initarg__);
ptr ID_33_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_ID_33[];

ptr ID_33_create_(ptr self__, int f__, ptr alst__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(33,0);
   IATT_(res__,12) = (int)f__;
   PATT_(res__,16) = (ptr)alst__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void ID_33_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ID_33_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ID_33_put_kwdname_(ptr self__, int nm__)
{
   ptr gl576_;
   static int gl577_;
   static union dtype_ gl578_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl576_ = x__;
   cache_dispatch_(gl576_,796,gl577_,INTVAL_(gl578_));
   IATT_(gl576_,INTVAL_(gl578_)) = (int)nm__;

   ret0__:
   return;
}

ptr ID_33_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ID_33_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ID_33_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   if ((PATT_(self__,16) != 0)) {
      res__ = (ptr)ID_112_create_(res__,IATT_(self__,12),LST37_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));
   }
   else {
      res__ = (ptr)ID_112_create_(res__,IATT_(self__,12),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


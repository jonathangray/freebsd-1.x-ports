/* except39.c : Sather class: EXCEPT_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr LST38_pcopy_();
extern ptr LOC54_create_();
extern ptr LOC54_create_lst_();
extern ptr EXC124_create_();
#include "macros_.h"



/*constant*/ int EXC39_print_indent_ = 2;
ptr EXC39_create_();
EXC39_out_of_line_();
ptr EXC39_dup_();
EXC39_put_kwdname_();
ptr EXC39_sather_code_();
ptr EXC39_initialize_();
ptr EXC39_pcopy_();
extern int attr_ent_EXC39[];

ptr EXC39_create_(self__,norm__,hdlr__,err__,ln__)
ptr self__;
ptr norm__;
ptr hdlr__;
ptr err__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(39,0);
   PATT_(res__,12) = (ptr)norm__;
   if ((hdlr__ != 0)) {
      PATT_(res__,20) = (ptr)LOC54_create_lst_(0,LOC54_create_(0,err__,0));
   }
   else {
   }
   PATT_(res__,16) = (ptr)hdlr__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

EXC39_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr EXC39_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

EXC39_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl633_;
   static int gl634_;
   static union dtype_ gl635_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl633_ = x__;
   cache_dispatch_(gl633_,796,gl634_,INTVAL_(gl635_));
   IATT_(gl633_,INTVAL_(gl635_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXC39_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXC39_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr EXC39_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   if ((PATT_(self__,16) == 0)) {
      res__ = (ptr)EXC124_create_(res__,LST38_pcopy_(PATT_(self__,12),pl__,pi__),0,0,IATT_(self__,4));
   }
   else {
      res__ = (ptr)EXC124_create_(res__,LST38_pcopy_(PATT_(self__,12),pl__,pi__),LST38_pcopy_(PATT_(self__,16),pl__,pi__),LST38_pcopy_(PATT_(self__,20),pl__,pi__),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


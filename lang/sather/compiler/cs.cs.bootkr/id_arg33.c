/* id_arg33.c : Sather class: ID_ARGS_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr ID_112_create_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr LST37_pcopy_();
#include "macros_.h"



/*constant*/ int ID_33_print_indent_ = 2;
ptr ID_33_create_();
ID_33_out_of_line_();
ptr ID_33_dup_();
ID_33_put_kwdname_();
ptr ID_33_sather_code_();
ptr ID_33_initialize_();
ptr ID_33_pcopy_();
extern int attr_ent_ID_33[];

ptr ID_33_create_(self__,f__,alst__)
ptr self__;
int f__;
ptr alst__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(33,0);
   IATT_(res__,12) = (int)f__;
   PATT_(res__,16) = (ptr)alst__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

ID_33_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ID_33_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

ID_33_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
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

ptr ID_33_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ID_33_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ID_33_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
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


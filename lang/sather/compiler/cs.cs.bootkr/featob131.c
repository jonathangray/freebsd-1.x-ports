/* featob131.c : Sather class: FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr FEA162_create_();
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



ptr FEA131_dup_();
FEA131_put_kwdname_();
ptr FEA131_sather_code_();
ptr FEA131_initialize_();
ptr FEA131_pcopy_();
/*constant*/ int FEA131_print_indent_ = 2;
ptr FEA131_create_();
FEA131_out_of_line_();
FEA131_mark_private_();
FEA131_mark_abstract_();
FEA131_mark_spec_();
FEA131_mark_shared_();
FEA131_mark_readonly_();
extern int attr_ent_FEA131[];

ptr FEA131_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

FEA131_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3152_;
   static int gl3153_;
   static union dtype_ gl3154_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3152_ = x__;
   cache_dispatch_(gl3152_,796,gl3153_,INTVAL_(gl3154_));
   IATT_(gl3152_,INTVAL_(gl3154_)) = (int)nm__;

   ret0__:
   return;
}

ptr FEA131_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr FEA131_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr FEA131_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)FEA162_create_(0,IATT_(self__,20),PATT_(self__,12));

   ret0__:
   return (res__);
}

ptr FEA131_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(131,0);
   IATT_(res__,20) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

FEA131_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

FEA131_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

FEA131_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

FEA131_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

FEA131_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

FEA131_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


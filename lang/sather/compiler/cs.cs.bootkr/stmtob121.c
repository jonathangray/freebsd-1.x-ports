/* stmtob121.c : Sather class: STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STM199_create_();
#include "macros_.h"



/*constant*/ int STM121_print_indent_ = 2;
ptr STM121_create_();
STM121_out_of_line_();
ptr STM121_dup_();
STM121_put_kwdname_();
ptr STM121_sather_code_();
ptr STM121_initialize_();
ptr STM121_pcopy_();
extern int attr_ent_STM121[];

ptr STM121_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(121,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

STM121_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr STM121_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

STM121_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3010_;
   static int gl3011_;
   static union dtype_ gl3012_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3010_ = x__;
   cache_dispatch_(gl3010_,796,gl3011_,INTVAL_(gl3012_));
   IATT_(gl3010_,INTVAL_(gl3012_)) = (int)nm__;

   ret0__:
   return;
}

ptr STM121_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr STM121_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STM121_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)STM199_create_(0,IATT_(self__,4));

   ret0__:
   return (res__);
}


/* str_co30.c : Sather class: STR_CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STR109_create_();
#include "macros_.h"



/*constant*/ int STR30_print_indent_ = 2;
ptr STR30_create_();
STR30_out_of_line_();
ptr STR30_dup_();
STR30_put_kwdname_();
ptr STR30_sather_code_();
ptr STR30_initialize_();
ptr STR30_pcopy_();
extern int attr_ent_STR30[];

ptr STR30_create_(self__,v__)
ptr self__;
int v__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(30,1);
   IATT_(res__,12) = (int)v__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

STR30_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr STR30_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

STR30_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl564_;
   static int gl565_;
   static union dtype_ gl566_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl564_ = x__;
   cache_dispatch_(gl564_,796,gl565_,INTVAL_(gl566_));
   IATT_(gl564_,INTVAL_(gl566_)) = (int)nm__;

   ret0__:
   return;
}

ptr STR30_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr STR30_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR30_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)STR109_create_(res__,IATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


/* bool_c29.c : Sather class: BOOL_CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr BOO108_create_();
#include "macros_.h"



/*constant*/ int BOO29_print_indent_ = 2;
ptr BOO29_create_();
BOO29_out_of_line_();
ptr BOO29_dup_();
BOO29_put_kwdname_();
ptr BOO29_sather_code_();
ptr BOO29_initialize_();
ptr BOO29_pcopy_();
extern int attr_ent_BOO29[];

ptr BOO29_create_(self__,v__)
ptr self__;
int v__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(29,1);
   IATT_(res__,12) = (int)v__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

BOO29_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr BOO29_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

BOO29_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl561_;
   static int gl562_;
   static union dtype_ gl563_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl561_ = x__;
   cache_dispatch_(gl561_,796,gl562_,INTVAL_(gl563_));
   IATT_(gl561_,INTVAL_(gl563_)) = (int)nm__;

   ret0__:
   return;
}

ptr BOO29_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr BOO29_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr BOO29_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)BOO108_create_(res__,IATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


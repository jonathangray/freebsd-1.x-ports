/* parser116.c : Sather class: PARSEROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr SEM188_create_();
#include "macros_.h"



/*constant*/ int PAR116_print_indent_ = 2;
ptr PAR116_create_();
PAR116_out_of_line_();
ptr PAR116_dup_();
PAR116_put_kwdname_();
ptr PAR116_sather_code_();
ptr PAR116_initialize_();
ptr PAR116_pcopy_();
extern int attr_ent_PAR116[];

ptr PAR116_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(116,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

PAR116_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr PAR116_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

PAR116_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl2922_;
   static int gl2923_;
   static union dtype_ gl2924_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2922_ = x__;
   cache_dispatch_(gl2922_,796,gl2923_,INTVAL_(gl2924_));
   IATT_(gl2922_,INTVAL_(gl2924_)) = (int)nm__;

   ret0__:
   return;
}

ptr PAR116_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr PAR116_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr PAR116_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)SEM188_create_(0,IATT_(self__,4),IATT_(self__,8));

   ret0__:
   return (res__);
}


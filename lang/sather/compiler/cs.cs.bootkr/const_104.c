/* const_104.c : Sather class: CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr CON194_create_();
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int CON104_print_indent_ = 2;
ptr CON104_create_();
CON104_out_of_line_();
ptr CON104_dup_();
CON104_put_kwdname_();
ptr CON104_sather_code_();
ptr CON104_initialize_();
ptr CON104_pcopy_();
extern int attr_ent_CON104[];

ptr CON104_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(104,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

CON104_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CON104_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

CON104_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1336_;
   static int gl1337_;
   static union dtype_ gl1338_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1336_ = x__;
   cache_dispatch_(gl1336_,796,gl1337_,INTVAL_(gl1338_));
   IATT_(gl1336_,INTVAL_(gl1338_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON104_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON104_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr CON104_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)CON194_create_(0,IATT_(self__,4));

   ret0__:
   return (res__);
}


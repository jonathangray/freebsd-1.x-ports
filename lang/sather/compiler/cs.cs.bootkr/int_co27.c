/* int_co27.c : Sather class: INT_CONST_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr INT106_create_();
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int INT27_print_indent_ = 2;
ptr INT27_create_();
INT27_out_of_line_();
ptr INT27_dup_();
INT27_put_kwdname_();
ptr INT27_sather_code_();
ptr INT27_initialize_();
ptr INT27_pcopy_();
extern int attr_ent_INT27[];

ptr INT27_create_(self__,v__)
ptr self__;
ptr v__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(27,0);
   PATT_(res__,12) = (ptr)v__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

INT27_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr INT27_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

INT27_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl555_;
   static int gl556_;
   static union dtype_ gl557_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl555_ = x__;
   cache_dispatch_(gl555_,796,gl556_,INTVAL_(gl557_));
   IATT_(gl555_,INTVAL_(gl557_)) = (int)nm__;

   ret0__:
   return;
}

ptr INT27_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr INT27_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr INT27_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;

   res__ = (ptr)INT106_create_(res__,PATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


/* break_52.c : Sather class: BREAK_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr EXP36_pcopy_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr BRE143_create_();
#include "macros_.h"



/*constant*/ int BRE52_print_indent_ = 2;
ptr BRE52_create_();
BRE52_out_of_line_();
ptr BRE52_dup_();
BRE52_put_kwdname_();
ptr BRE52_sather_code_();
ptr BRE52_initialize_();
ptr BRE52_pcopy_();
ptr BRE52_inhcreate_();
extern int attr_ent_BRE52[];

ptr BRE52_create_(self__,expr__)
ptr self__;
ptr expr__;
{
   ptr res__ = 0;

   res__ = (ptr)BRE52_inhcreate_(self__);
   PATT_(res__,12) = (ptr)expr__;

   ret0__:
   return (res__);
}

BRE52_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr BRE52_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

BRE52_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl795_;
   static int gl796_;
   static union dtype_ gl797_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl795_ = x__;
   cache_dispatch_(gl795_,796,gl796_,INTVAL_(gl797_));
   IATT_(gl795_,INTVAL_(gl797_)) = (int)nm__;

   ret0__:
   return;
}

ptr BRE52_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr BRE52_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr BRE52_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl798_;
   static int gl799_;
   static union dtype_ gl800_;

   res__ = (ptr)BRE143_create_(res__,IATT_(self__,4));
   if ((PATT_(self__,12) != 0)) {
      gl798_ = PATT_(self__,12);
      cache_dispatch_(gl798_,407,gl799_,INTVAL_(gl800_));
      PATT_(res__,16) = (ptr)PFN_(gl800_)(gl798_,pl__,pi__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr BRE52_inhcreate_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(52,0);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}


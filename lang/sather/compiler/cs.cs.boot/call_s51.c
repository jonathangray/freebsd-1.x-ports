/* call_s51.c : Sather class: CALL_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr CAL142_create_(ptr self__, ptr exp__, int ln__);
#include "macros_.h"



/*constant*/ int CAL51_print_indent_ = 2;
ptr CAL51_create_(ptr self__, ptr exp__);
void CAL51_out_of_line_(ptr self__, ptr fn__);
ptr CAL51_dup_(ptr self__);
void CAL51_put_kwdname_(ptr self__, int nm__);
ptr CAL51_sather_code_(ptr self__);
ptr CAL51_initialize_(ptr self__, ptr initarg__);
ptr CAL51_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_CAL51[];

ptr CAL51_create_(ptr self__, ptr exp__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(51,0);
   PATT_(res__,12) = (ptr)exp__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void CAL51_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CAL51_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void CAL51_put_kwdname_(ptr self__, int nm__)
{
   ptr gl789_;
   static int gl790_;
   static union dtype_ gl791_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl789_ = x__;
   cache_dispatch_(gl789_,796,gl790_,INTVAL_(gl791_));
   IATT_(gl789_,INTVAL_(gl791_)) = (int)nm__;

   ret0__:
   return;
}

ptr CAL51_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CAL51_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr CAL51_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl792_;
   static int gl793_;
   static union dtype_ gl794_;
   ptr gl36_;

   gl792_ = PATT_(self__,12);
   cache_dispatch_(gl792_,407,gl793_,INTVAL_(gl794_));
   gl36_ = PFN_(gl794_)(gl792_,pl__,pi__);
   res__ = (ptr)CAL142_create_(res__,gl36_,IATT_(self__,4));

   ret0__:
   return (res__);
}


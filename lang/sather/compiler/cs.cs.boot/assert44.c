/* assert44.c : Sather class: ASSERT_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr ASS135_create_(ptr self__, ptr a__, int ln__);
#include "macros_.h"



/*constant*/ int ASS44_print_indent_ = 2;
ptr ASS44_create_(ptr self__, ptr a__, int ln__);
void ASS44_out_of_line_(ptr self__, ptr fn__);
ptr ASS44_dup_(ptr self__);
void ASS44_put_kwdname_(ptr self__, int nm__);
ptr ASS44_sather_code_(ptr self__);
ptr ASS44_initialize_(ptr self__, ptr initarg__);
ptr ASS44_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_ASS44[];

ptr ASS44_create_(ptr self__, ptr a__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(44,0);
   PATT_(res__,12) = (ptr)a__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void ASS44_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ASS44_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ASS44_put_kwdname_(ptr self__, int nm__)
{
   ptr gl741_;
   static int gl742_;
   static union dtype_ gl743_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl741_ = x__;
   cache_dispatch_(gl741_,796,gl742_,INTVAL_(gl743_));
   IATT_(gl741_,INTVAL_(gl743_)) = (int)nm__;

   ret0__:
   return;
}

ptr ASS44_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ASS44_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ASS44_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl744_;
   static int gl745_;
   static union dtype_ gl746_;
   ptr gl27_;

   gl744_ = PATT_(self__,12);
   cache_dispatch_(gl744_,407,gl745_,INTVAL_(gl746_));
   gl27_ = PFN_(gl746_)(gl744_,pl__,pi__);
   res__ = (ptr)ASS135_create_(res__,gl27_,IATT_(self__,4));

   ret0__:
   return (res__);
}


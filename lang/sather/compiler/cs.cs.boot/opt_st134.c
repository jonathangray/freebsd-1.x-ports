/* opt_st134.c : Sather class: OPT_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr OPT211_create_(ptr self__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int OPT134_print_indent_ = 2;
ptr OPT134_create_(ptr self__);
void OPT134_out_of_line_(ptr self__, ptr fn__);
ptr OPT134_dup_(ptr self__);
void OPT134_put_kwdname_(ptr self__, int nm__);
ptr OPT134_sather_code_(ptr self__);
ptr OPT134_initialize_(ptr self__, ptr initarg__);
ptr OPT134_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_OPT134[];

ptr OPT134_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(134,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void OPT134_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr OPT134_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void OPT134_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3208_;
   static int gl3209_;
   static union dtype_ gl3210_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3208_ = x__;
   cache_dispatch_(gl3208_,796,gl3209_,INTVAL_(gl3210_));
   IATT_(gl3208_,INTVAL_(gl3210_)) = (int)nm__;

   ret0__:
   return;
}

ptr OPT134_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr OPT134_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OPT134_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)OPT211_create_(res__,IATT_(self__,4));

   ret0__:
   return (res__);
}


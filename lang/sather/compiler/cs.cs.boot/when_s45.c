/* when_s45.c : Sather class: WHEN_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr LST37_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr WHE136_create_(ptr self__, ptr e__, ptr t__, int ln__);
#include "macros_.h"



/*constant*/ int WHE45_print_indent_ = 2;
ptr WHE45_create_(ptr self__, ptr e__, ptr t__);
void WHE45_out_of_line_(ptr self__, ptr fn__);
ptr WHE45_dup_(ptr self__);
void WHE45_put_kwdname_(ptr self__, int nm__);
ptr WHE45_sather_code_(ptr self__);
ptr WHE45_initialize_(ptr self__, ptr initarg__);
ptr WHE45_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_WHE45[];

ptr WHE45_create_(ptr self__, ptr e__, ptr t__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(45,0);
   PATT_(res__,12) = (ptr)e__;
   PATT_(res__,16) = (ptr)t__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void WHE45_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr WHE45_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void WHE45_put_kwdname_(ptr self__, int nm__)
{
   ptr gl747_;
   static int gl748_;
   static union dtype_ gl749_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl747_ = x__;
   cache_dispatch_(gl747_,796,gl748_,INTVAL_(gl749_));
   IATT_(gl747_,INTVAL_(gl749_)) = (int)nm__;

   ret0__:
   return;
}

ptr WHE45_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr WHE45_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr WHE45_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)WHE136_create_(res__,LST37_pcopy_(PATT_(self__,12),pl__,pi__),LST38_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));

   ret0__:
   return (res__);
}


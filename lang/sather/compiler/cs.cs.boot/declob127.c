/* declob127.c : Sather class: DECLOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr DEC205_create_(ptr self__, ptr t__, int ln__);
#include "macros_.h"



/*constant*/ int DEC127_print_indent_ = 2;
ptr DEC127_create_(ptr self__, ptr t__);
void DEC127_out_of_line_(ptr self__, ptr fn__);
ptr DEC127_dup_(ptr self__);
void DEC127_put_kwdname_(ptr self__, int nm__);
ptr DEC127_sather_code_(ptr self__);
ptr DEC127_initialize_(ptr self__, ptr initarg__);
ptr DEC127_pcopy_(ptr self__, ptr pl__, ptr pi__);
void DEC127_mark_private_(ptr self__);
extern int attr_ent_DEC127[];

ptr DEC127_create_(ptr self__, ptr t__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(127,0);
   PATT_(res__,12) = (ptr)t__;
   IATT_(res__,16) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void DEC127_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr DEC127_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void DEC127_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3097_;
   static int gl3098_;
   static union dtype_ gl3099_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3097_ = x__;
   cache_dispatch_(gl3097_,796,gl3098_,INTVAL_(gl3099_));
   IATT_(gl3097_,INTVAL_(gl3099_)) = (int)nm__;

   ret0__:
   return;
}

ptr DEC127_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr DEC127_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr DEC127_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl3100_;
   static int gl3101_;
   static union dtype_ gl3102_;
   ptr gl302_;

   gl3100_ = PATT_(self__,12);
   cache_dispatch_(gl3100_,407,gl3101_,INTVAL_(gl3102_));
   gl302_ = PFN_(gl3102_)(gl3100_,pl__,pi__);
   res__ = (ptr)DEC205_create_(res__,gl302_,IATT_(self__,16));

   ret0__:
   return (res__);
}

void DEC127_mark_private_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}


/* featob131.c : Sather class: FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr FEA162_create_(ptr self__, int ln__, ptr c_def__);
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



ptr FEA131_dup_(ptr self__);
void FEA131_put_kwdname_(ptr self__, int nm__);
ptr FEA131_sather_code_(ptr self__);
ptr FEA131_initialize_(ptr self__, ptr initarg__);
ptr FEA131_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int FEA131_print_indent_ = 2;
ptr FEA131_create_(ptr self__);
void FEA131_out_of_line_(ptr self__, ptr fn__);
void FEA131_mark_private_(ptr self__);
void FEA131_mark_abstract_(ptr self__);
void FEA131_mark_spec_(ptr self__);
void FEA131_mark_shared_(ptr self__);
void FEA131_mark_readonly_(ptr self__);
extern int attr_ent_FEA131[];

ptr FEA131_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void FEA131_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3152_;
   static int gl3153_;
   static union dtype_ gl3154_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3152_ = x__;
   cache_dispatch_(gl3152_,796,gl3153_,INTVAL_(gl3154_));
   IATT_(gl3152_,INTVAL_(gl3154_)) = (int)nm__;

   ret0__:
   return;
}

ptr FEA131_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr FEA131_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr FEA131_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)FEA162_create_(0,IATT_(self__,20),PATT_(self__,12));

   ret0__:
   return (res__);
}

ptr FEA131_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(131,0);
   IATT_(res__,20) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void FEA131_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void FEA131_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void FEA131_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void FEA131_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void FEA131_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void FEA131_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


/* id_exp25.c : Sather class: ID_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr ID_103_create_(ptr self__, int nm__, int ln__);
#include "macros_.h"



/*constant*/ int ID_25_print_indent_ = 2;
ptr ID_25_create_(ptr self__, int nm__);
void ID_25_out_of_line_(ptr self__, ptr fn__);
ptr ID_25_dup_(ptr self__);
void ID_25_put_kwdname_(ptr self__, int nm__);
ptr ID_25_sather_code_(ptr self__);
ptr ID_25_initialize_(ptr self__, ptr initarg__);
ptr ID_25_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_ID_25[];

ptr ID_25_create_(ptr self__, int nm__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(25,1);
   IATT_(res__,12) = (int)nm__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void ID_25_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ID_25_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void ID_25_put_kwdname_(ptr self__, int nm__)
{
   ptr gl549_;
   static int gl550_;
   static union dtype_ gl551_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl549_ = x__;
   cache_dispatch_(gl549_,796,gl550_,INTVAL_(gl551_));
   IATT_(gl549_,INTVAL_(gl551_)) = (int)nm__;

   ret0__:
   return;
}

ptr ID_25_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ID_25_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ID_25_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)ID_103_create_(res__,IATT_(self__,12),IATT_(self__,4));

   ret0__:
   return (res__);
}


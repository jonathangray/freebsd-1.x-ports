/* rout_s24.c : Sather class: ROUT_SPECOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr ROU24_create_(ptr self__, ptr a__, ptr o__, ptr b__);
ptr ROU24_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_ROU24[];

ptr ROU24_create_(ptr self__, ptr a__, ptr o__, ptr b__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(24,0);
   PATT_(res__,4) = (ptr)a__;
   PATT_(res__,8) = (ptr)b__;
   PATT_(res__,12) = (ptr)o__;

   ret0__:
   return (res__);
}

ptr ROU24_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


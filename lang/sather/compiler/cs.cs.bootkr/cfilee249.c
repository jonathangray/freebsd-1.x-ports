/* cfilee249.c : Sather class: CFILEENT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr CFI249_create_();
ptr CFI249_initialize_();
extern int attr_ent_CFI249[];

ptr CFI249_create_(self__,s__,i__)
ptr self__;
ptr s__;
int i__;
{
   ptr res__ = 0;
   ptr gl4861_;

   gl4861_ = res__;
   res__ = (ptr)new_(249,0);
   PATT_(res__,4) = (ptr)s__;
   IATT_(res__,8) = (int)i__;

   ret0__:
   return (res__);
}

ptr CFI249_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


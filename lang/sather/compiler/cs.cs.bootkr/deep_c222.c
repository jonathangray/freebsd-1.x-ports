/* deep_c222.c : Sather class: DEEP_COPY, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern ptr deep_copy_();

#include "macros_.h"



ptr DEE222_deep_copy_();
ptr DEE222_initialize_();
extern int attr_ent_DEE222[];

ptr DEE222_deep_copy_(self__,ob__)
ptr self__;
ptr ob__;
{
   ptr res__ = 0;

   res__ = (ptr)deep_copy_(ob__);

   ret0__:
   return (res__);
}

ptr DEE222_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


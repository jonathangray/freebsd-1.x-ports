/* persis220.c : Sather class: PERSIST_OB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern deep_save_();
extern deep_restore_();
extern deep_save_vn_();
extern deep_restore_vn_();
extern ptr address_of();
#define address_of(x) ((ptr)&(x))

#include "macros_.h"



PER220_deep_save_();
ptr PER220_deep_restore_();
PER220_deep_save_vn_();
ptr PER220_deep_restore_vn_();
ptr PER220_initialize_();
extern int attr_ent_PER220[];

PER220_deep_save_(self__,p__,f__)
ptr self__;
ptr p__;
ptr f__;
{

   deep_save_(p__,IATT_(f__,8));

   ret0__:
   return;
}

ptr PER220_deep_restore_(self__,f__)
ptr self__;
ptr f__;
{
   ptr res__ = 0;

   deep_restore_(address_of(res__),IATT_(f__,8));

   ret0__:
   return (res__);
}

PER220_deep_save_vn_(self__,p__,f__,version__)
ptr self__;
ptr p__;
ptr f__;
int version__;
{

   deep_save_vn_(p__,IATT_(f__,8),version__);

   ret0__:
   return;
}

ptr PER220_deep_restore_vn_(self__,f__,version__)
ptr self__;
ptr f__;
int version__;
{
   ptr res__ = 0;

   deep_restore_vn_(address_of(res__),IATT_(f__,8),version__);

   ret0__:
   return (res__);
}

ptr PER220_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


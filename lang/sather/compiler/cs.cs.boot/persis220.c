/* persis220.c : Sather class: PERSIST_OB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void deep_save_(ptr p__, int fd__);
extern void deep_restore_(ptr p__, int fd__);
extern void deep_save_vn_(ptr p__, int fd__, int v__);
extern void deep_restore_vn_(ptr p__, int fd__, int v__);
extern ptr address_of(ptr l__);
#define address_of(x) ((ptr)&(x))

#include "macros_.h"



void PER220_deep_save_(ptr self__, ptr p__, ptr f__);
ptr PER220_deep_restore_(ptr self__, ptr f__);
void PER220_deep_save_vn_(ptr self__, ptr p__, ptr f__, int version__);
ptr PER220_deep_restore_vn_(ptr self__, ptr f__, int version__);
ptr PER220_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_PER220[];

void PER220_deep_save_(ptr self__, ptr p__, ptr f__)
{

   deep_save_(p__,IATT_(f__,8));

   ret0__:
   return;
}

ptr PER220_deep_restore_(ptr self__, ptr f__)
{
   ptr res__ = 0;

   deep_restore_(address_of(res__),IATT_(f__,8));

   ret0__:
   return (res__);
}

void PER220_deep_save_vn_(ptr self__, ptr p__, ptr f__, int version__)
{

   deep_save_vn_(p__,IATT_(f__,8),version__);

   ret0__:
   return;
}

ptr PER220_deep_restore_vn_(ptr self__, ptr f__, int version__)
{
   ptr res__ = 0;

   deep_restore_vn_(address_of(res__),IATT_(f__,8),version__);

   ret0__:
   return (res__);
}

ptr PER220_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


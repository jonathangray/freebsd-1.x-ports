/* file_s91.c : Sather class: FILE_STAT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern int stat(ptr fname__, ptr addr__);
extern ptr str_ptr_(ptr s__);

extern ptr C_F74_create_(ptr self__);
#include "macros_.h"



ptr FIL91_create_(ptr self__, ptr f__);
ptr FIL91_create_from_fn_(ptr self__, ptr s__);
ptr FIL91_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_FIL91[];

ptr FIL91_create_(ptr self__, ptr f__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(91,0);
   PATT_(res__,8) = (ptr)C_F74_create_(0);
   IATT_(res__,4) = (int)stat(str_ptr_(PATT_(f__,8)),PATT_(res__,8));

   ret0__:
   return (res__);
}

ptr FIL91_create_from_fn_(ptr self__, ptr s__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(91,0);
   PATT_(res__,8) = (ptr)C_F74_create_(0);
   IATT_(res__,4) = (int)stat(str_ptr_(s__),PATT_(res__,8));

   ret0__:
   return (res__);
}

ptr FIL91_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


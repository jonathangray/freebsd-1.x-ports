/* class_191.c : Sather class: CLASS_STAT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr CLA191_create_();
CLA191_update_();
ptr CLA191_initialize_();
extern int attr_ent_CLA191[];

ptr CLA191_create_(self__,co__)
ptr self__;
ptr co__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(191,0);
   IATT_(res__,20) = (int)IATT_(co__,20);
   IATT_(res__,12) = (int)IATT_(co__,128);
   IATT_(res__,16) = (int)IATT_(co__,132);
   IATT_(res__,8) = (int)IATT_(co__,104);
   PATT_(res__,24) = (ptr)PATT_(co__,92);
   PATT_(res__,28) = (ptr)PATT_(co__,136);

   ret0__:
   return (res__);
}

CLA191_update_(self__,co__)
ptr self__;
ptr co__;
{

   IATT_(self__,20) = (int)IATT_(co__,20);
   IATT_(self__,12) = (int)IATT_(co__,128);
   IATT_(self__,16) = (int)IATT_(co__,132);
   IATT_(self__,8) = (int)IATT_(co__,104);
   PATT_(self__,24) = (ptr)PATT_(co__,92);
   PATT_(self__,28) = (ptr)PATT_(co__,136);

   ret0__:
   return;
}

ptr CLA191_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


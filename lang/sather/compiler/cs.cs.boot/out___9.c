/* out___9.c : Sather class: OUT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr FIL11_r_(ptr self__, float re__);
extern ptr FIL11_d_(ptr self__, double do__);
extern ptr FIL11_nl_(ptr self__);
extern ptr FIL11_out_(ptr self__);
extern ptr FIL11_b_(ptr self__, char bo__);
extern ptr FIL11_c_(ptr self__, char ch__);
extern ptr FIL11_i_(ptr self__, int in__);
extern ptr FIL11_s_(ptr self__, ptr st__);
#include "macros_.h"



/*shared*/ ptr OUT9_file_;
ptr OUT9_b_(ptr self__, char bo__);
ptr OUT9_c_(ptr self__, char ch__);
ptr OUT9_i_(ptr self__, int in__);
ptr OUT9_s_(ptr self__, ptr st__);
ptr OUT9_r_(ptr self__, float re__);
ptr OUT9_d_(ptr self__, double do__);
ptr OUT9_nl_(ptr self__);
extern int attr_ent_OUT9[];

ptr OUT9_b_(ptr self__, char bo__)
{
   ptr res__ = 0;

   (void)FIL11_b_(OUT9_file_,bo__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_c_(ptr self__, char ch__)
{
   ptr res__ = 0;

   (void)FIL11_c_(OUT9_file_,ch__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_i_(ptr self__, int in__)
{
   ptr res__ = 0;

   (void)FIL11_i_(OUT9_file_,in__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_s_(ptr self__, ptr st__)
{
   ptr res__ = 0;

   (void)FIL11_s_(OUT9_file_,st__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_r_(ptr self__, float re__)
{
   ptr res__ = 0;

   (void)FIL11_r_(OUT9_file_,re__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_d_(ptr self__, double do__)
{
   ptr res__ = 0;

   (void)FIL11_d_(OUT9_file_,do__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_nl_(ptr self__)
{
   ptr res__ = 0;

   (void)FIL11_nl_(OUT9_file_);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


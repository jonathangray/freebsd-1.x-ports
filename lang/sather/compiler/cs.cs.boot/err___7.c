/* err___7.c : Sather class: ERR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr FIL11_r_(ptr self__, float re__);
extern ptr FIL11_d_(ptr self__, double do__);
extern ptr FIL11_nl_(ptr self__);
extern ptr FIL11_err_(ptr self__);
extern ptr FIL11_b_(ptr self__, char bo__);
extern ptr FIL11_c_(ptr self__, char ch__);
extern ptr FIL11_i_(ptr self__, int in__);
extern ptr FIL11_s_(ptr self__, ptr st__);
#include "macros_.h"



/*shared*/ ptr ERR7_file_;
ptr ERR7_b_(ptr self__, char bo__);
ptr ERR7_c_(ptr self__, char ch__);
ptr ERR7_i_(ptr self__, int in__);
ptr ERR7_s_(ptr self__, ptr st__);
ptr ERR7_r_(ptr self__, float re__);
ptr ERR7_d_(ptr self__, double do__);
ptr ERR7_nl_(ptr self__);
extern int attr_ent_ERR7[];

ptr ERR7_b_(ptr self__, char bo__)
{
   ptr res__ = 0;

   (void)FIL11_b_(ERR7_file_,bo__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_c_(ptr self__, char ch__)
{
   ptr res__ = 0;

   (void)FIL11_c_(ERR7_file_,ch__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_i_(ptr self__, int in__)
{
   ptr res__ = 0;

   (void)FIL11_i_(ERR7_file_,in__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_s_(ptr self__, ptr st__)
{
   ptr res__ = 0;

   (void)FIL11_s_(ERR7_file_,st__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_r_(ptr self__, float re__)
{
   ptr res__ = 0;

   (void)FIL11_r_(ERR7_file_,re__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_d_(ptr self__, double do__)
{
   ptr res__ = 0;

   (void)FIL11_d_(ERR7_file_,do__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ERR7_nl_(ptr self__)
{
   ptr res__ = 0;

   (void)FIL11_nl_(ERR7_file_);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* out___9.c : Sather class: OUT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr FIL11_r_();
extern ptr FIL11_d_();
extern ptr FIL11_nl_();
extern ptr FIL11_out_();
extern ptr FIL11_b_();
extern ptr FIL11_c_();
extern ptr FIL11_i_();
extern ptr FIL11_s_();
#include "macros_.h"



/*shared*/ ptr OUT9_file_;
ptr OUT9_b_();
ptr OUT9_c_();
ptr OUT9_i_();
ptr OUT9_s_();
ptr OUT9_r_();
ptr OUT9_d_();
ptr OUT9_nl_();
extern int attr_ent_OUT9[];

ptr OUT9_b_(self__,bo__)
ptr self__;
char bo__;
{
   ptr res__ = 0;

   (void)FIL11_b_(OUT9_file_,bo__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_c_(self__,ch__)
ptr self__;
char ch__;
{
   ptr res__ = 0;

   (void)FIL11_c_(OUT9_file_,ch__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_i_(self__,in__)
ptr self__;
int in__;
{
   ptr res__ = 0;

   (void)FIL11_i_(OUT9_file_,in__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_s_(self__,st__)
ptr self__;
ptr st__;
{
   ptr res__ = 0;

   (void)FIL11_s_(OUT9_file_,st__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_r_(self__,re__)
ptr self__;
float re__;
{
   ptr res__ = 0;

   (void)FIL11_r_(OUT9_file_,re__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_d_(self__,do__)
ptr self__;
double do__;
{
   ptr res__ = 0;

   (void)FIL11_d_(OUT9_file_,do__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OUT9_nl_(self__)
ptr self__;
{
   ptr res__ = 0;

   (void)FIL11_nl_(OUT9_file_);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* str_se245.c : Sather class: STR_SET_CURS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern struct { int tp_; int sz_; char st_; } gs2878_;
#include "macros_.h"



/*constant*/ ptr STR245_del_key_ = (ptr)(&gs2878_);
ptr STR245_create_(ptr self__, ptr t__);
void STR245_first_(ptr self__);
ptr STR245_item_(ptr self__);
void STR245_next_(ptr self__);
ptr STR245_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR245[];

ptr STR245_create_(ptr self__, ptr t__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(245,0);
   PATT_(res__,12) = (ptr)t__;
   STR245_first_(res__);

   ret0__:
   return (res__);
}

void STR245_first_(ptr self__)
{
   ptr    val__ = 0;

   CATT_(self__,4) = (char)0;
   IATT_(self__,8) = (int)0;
   while (1) {
      val__ = (ptr)PATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2));
      if (((val__ != 0) & (val__ != (ptr)(&gs2878_)))) {
         goto ret0__;
      }
      else {
         if ((IATT_(self__,8) == (IATT_(PATT_(self__,12),12) - 1))) {
            CATT_(self__,4) = (char)1;
            goto ret0__;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }

   ret0__:
   return;
}

ptr STR245_item_(ptr self__)
{
   ptr res__ = 0;

   if ((! CATT_(self__,4))) {
      res__ = (ptr)PATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

void STR245_next_(ptr self__)
{

   if (CATT_(self__,4)) {
      goto ret0__;
   }
   else {
   }
   IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
   while (1) {
      if ((IATT_(self__,8) == (IATT_(PATT_(self__,12),12) - 1))) {
         CATT_(self__,4) = (char)1;
         goto ret0__;
      }
      else {
         if (((PATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2)) != 0) & (PATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2)) != (ptr)(&gs2878_)))) {
            goto ret0__;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }

   ret0__:
   return;
}

ptr STR245_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


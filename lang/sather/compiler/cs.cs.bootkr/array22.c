/* array22.c : Sather class: ARRAY2{$OB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



int ARR2_asize_();
ARR2_clear_();
extern int attr_ent_ARR2[];

int ARR2_asize_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)(IATT_(self__,4) * IATT_(self__,8));

   ret0__:
   return (res__);
}

ARR2_clear_(self__)
ptr self__;
{
   ptr gl490_;
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_491_;
      }
      else {
      }
      j__ = S_int_VOID_;
      while (1) {
         if ((j__ == IATT_(self__,8))) {
            goto goto_tag_492_;
         }
         else {
         }
         gl490_ = self__;
         PATT_(gl490_, IATT_(gl490_, 12 + ((i__) << 2)) + ((j__) << 2)) = (ptr)0;
         j__ = (int)(j__ + 1);
      }
   goto_tag_492_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_491_: ;

   ret0__:
   return;
}


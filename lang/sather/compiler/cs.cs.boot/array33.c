/* array33.c : Sather class: ARRAY3{$OB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



int ARR3_asize_(ptr self__);
void ARR3_clear_(ptr self__);
extern int attr_ent_ARR3[];

int ARR3_asize_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)((IATT_(self__,4) * IATT_(self__,8)) * IATT_(self__,12));

   ret0__:
   return (res__);
}

void ARR3_clear_(ptr self__)
{
   ptr gl493_;
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    k__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_494_;
      }
      else {
      }
      j__ = S_int_VOID_;
      while (1) {
         if ((j__ == IATT_(self__,8))) {
            goto goto_tag_495_;
         }
         else {
         }
         k__ = S_int_VOID_;
         while (1) {
            if ((k__ == IATT_(self__,12))) {
               goto goto_tag_496_;
            }
            else {
            }
            gl493_ = self__;
            PATT_(gl493_, IATT_(gl493_, IATT_(gl493_, 16 + ((i__) << 2)) + ((j__) << 2)) + ((k__) << 2)) = (ptr)0;
            k__ = (int)(k__ + 1);
         }
      goto_tag_496_: ;
         j__ = (int)(j__ + 1);
      }
   goto_tag_495_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_494_: ;

   ret0__:
   return;
}


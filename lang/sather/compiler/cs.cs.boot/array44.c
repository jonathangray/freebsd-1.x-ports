/* array44.c : Sather class: ARRAY4{$OB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



int ARR4_asize_(ptr self__);
void ARR4_clear_(ptr self__);
extern int attr_ent_ARR4[];

int ARR4_asize_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)(((IATT_(self__,4) * IATT_(self__,8)) * IATT_(self__,12)) * IATT_(self__,16));

   ret0__:
   return (res__);
}

void ARR4_clear_(ptr self__)
{
   ptr gl497_;
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    k__ = S_int_VOID_;
   int    l__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_498_;
      }
      else {
      }
      j__ = S_int_VOID_;
      while (1) {
         if ((j__ == IATT_(self__,8))) {
            goto goto_tag_499_;
         }
         else {
         }
         k__ = S_int_VOID_;
         while (1) {
            if ((k__ == IATT_(self__,12))) {
               goto goto_tag_500_;
            }
            else {
            }
            l__ = S_int_VOID_;
            while (1) {
               if ((l__ == IATT_(self__,16))) {
                  goto goto_tag_501_;
               }
               else {
               }
               gl497_ = self__;
               PATT_(gl497_, IATT_(gl497_, IATT_(gl497_, IATT_(gl497_, 20 + ((i__) << 2)) + ((j__) << 2)) + ((k__) << 2)) + ((l__) << 2)) = (ptr)0;
               l__ = (int)(l__ + 1);
            }
         goto_tag_501_: ;
            k__ = (int)(k__ + 1);
         }
      goto_tag_500_: ;
         j__ = (int)(j__ + 1);
      }
   goto_tag_499_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_498_: ;

   ret0__:
   return;
}


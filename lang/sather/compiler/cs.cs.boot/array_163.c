/* array_163.c : Sather class: ARRAY{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



void ARR163_clear_(ptr self__);
extern int attr_ent_ARR163[];

void ARR163_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_4485_;
      }
      else {
      }
      IATT_(self__, 8 + ((i__) << 2)) = (int)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4485_: ;

   ret0__:
   return;
}


/* array_221.c : Sather class: ARRAY{BOOL}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



void ARR221_clear_(ptr self__);
extern int attr_ent_ARR221[];

void ARR221_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_4812_;
      }
      else {
      }
      CATT_(self__, 8 + ((i__))) = (char)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4812_: ;

   ret0__:
   return;
}


/* array_79.c : Sather class: ARRAY{CHAR}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



void ARR79_clear_(ptr self__);
extern int attr_ent_ARR79[];

void ARR79_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_982_;
      }
      else {
      }
      CATT_(self__, 8 + ((i__))) = (char)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_982_: ;

   ret0__:
   return;
}


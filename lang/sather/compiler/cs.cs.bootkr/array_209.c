/* array_209.c : Sather class: ARRAY{$FEATOB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ARR209_clear_();
extern int attr_ent_ARR209[];

ARR209_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_4769_;
      }
      else {
      }
      PATT_(self__, 8 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4769_: ;

   ret0__:
   return;
}


/* array_197.c : Sather class: ARRAY{$EXPROB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ARR197_clear_();
extern int attr_ent_ARR197[];

ARR197_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_4723_;
      }
      else {
      }
      PATT_(self__, 8 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4723_: ;

   ret0__:
   return;
}


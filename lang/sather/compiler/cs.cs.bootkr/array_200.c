/* array_200.c : Sather class: ARRAY{$STMTOB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ARR200_clear_();
extern int attr_ent_ARR200[];

ARR200_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_4732_;
      }
      else {
      }
      PATT_(self__, 8 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4732_: ;

   ret0__:
   return;
}


/* block_202.c : Sather class: BLOCK_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int GLO94_global_key_(ptr self__);
#include "macros_.h"



int BLO202_get_goto_tag_(ptr self__);
ptr BLO202_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_BLO202[];

int BLO202_get_goto_tag_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((! (IATT_(self__,4) > 0))) {
      IATT_(self__,4) = (int)GLO94_global_key_(0);
   }
   else {
   }
   res__ = (int)IATT_(self__,4);

   ret0__:
   return (res__);
}

ptr BLO202_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


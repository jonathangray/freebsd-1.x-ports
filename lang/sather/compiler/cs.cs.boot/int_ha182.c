/* int_ha182.c : Sather class: INT_HASH_ELT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr INT182_create_(ptr self__, int i__, ptr iset__);
char INT182_is_equal_(ptr self__, ptr e__);
ptr INT182_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_INT182[];

ptr INT182_create_(ptr self__, int i__, ptr iset__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(182,0);
   IATT_(res__,4) = (int)i__;
   PATT_(res__,12) = (ptr)iset__;
   IATT_(res__,8) = (int)i__;

   ret0__:
   return (res__);
}

char INT182_is_equal_(ptr self__, ptr e__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(e__,8) == IATT_(self__,8));

   ret0__:
   return (res__);
}

ptr INT182_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


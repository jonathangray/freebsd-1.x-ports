/* str_ha173.c : Sather class: STR_HASH_MAP_ELT{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int STR20_hash_(ptr self__);
extern char STR20_is_equal_(ptr self__, ptr st__);
#include "macros_.h"



ptr STR173_create_(ptr self__, ptr str__, int val__);
void STR173_set_hash_(ptr self__);
char STR173_is_equal_(ptr self__, ptr e__);
ptr STR173_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR173[];

ptr STR173_create_(ptr self__, ptr str__, int val__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(173,0);
   PATT_(res__,4) = (ptr)str__;
   IATT_(res__,12) = (int)val__;
   STR173_set_hash_(res__);

   ret0__:
   return (res__);
}

void STR173_set_hash_(ptr self__)
{

   IATT_(self__,8) = (int)STR20_hash_(PATT_(self__,4));

   ret0__:
   return;
}

char STR173_is_equal_(ptr self__, ptr e__)
{
   char res__ = S_char_VOID_;

   if ((IATT_(e__,8) == IATT_(self__,8))) {
      res__ = (char)STR20_is_equal_(PATT_(self__,4),PATT_(e__,4));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR173_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


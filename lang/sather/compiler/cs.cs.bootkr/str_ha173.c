/* str_ha173.c : Sather class: STR_HASH_MAP_ELT{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int STR20_hash_();
extern char STR20_is_equal_();
#include "macros_.h"



ptr STR173_create_();
STR173_set_hash_();
char STR173_is_equal_();
ptr STR173_initialize_();
extern int attr_ent_STR173[];

ptr STR173_create_(self__,str__,val__)
ptr self__;
ptr str__;
int val__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(173,0);
   PATT_(res__,4) = (ptr)str__;
   IATT_(res__,12) = (int)val__;
   STR173_set_hash_(res__);

   ret0__:
   return (res__);
}

STR173_set_hash_(self__)
ptr self__;
{

   IATT_(self__,8) = (int)STR20_hash_(PATT_(self__,4));

   ret0__:
   return;
}

char STR173_is_equal_(self__,e__)
ptr self__;
ptr e__;
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

ptr STR173_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


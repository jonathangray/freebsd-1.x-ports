/* strint234.c : Sather class: STRINT_TO_INT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern GEN252_clear_();
extern ptr STR251_create_();
extern ptr GEN252_create_();
extern ptr GEN252_get_();
extern GEN252_insert_();
extern GEN252_delete_();
#include "macros_.h"



ptr STR234_create_();
int STR234_get_();
STR234_insert_();
STR234_delete_();
STR234_clear_();
ptr STR234_initialize_();
extern int attr_ent_STR234[];

ptr STR234_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(234,0);
   PATT_(res__,4) = (ptr)GEN252_create_(PATT_(res__,4));

   ret0__:
   return (res__);
}

int STR234_get_(self__,s__,i__)
ptr self__;
ptr s__;
int i__;
{
   int res__ = S_int_VOID_;
   ptr    te__ = 0;

   te__ = (ptr)GEN252_get_(PATT_(self__,4),STR251_create_(0,s__,i__,0));
   if ((te__ == 0)) {
      res__ = (int)(- 1);
   }
   else {
      res__ = (int)IATT_(te__,16);
   }

   ret0__:
   return (res__);
}

STR234_insert_(self__,s__,i__,v__)
ptr self__;
ptr s__;
int i__;
int v__;
{

   GEN252_insert_(PATT_(self__,4),STR251_create_(0,s__,i__,v__));

   ret0__:
   return;
}

STR234_delete_(self__,s__,i__)
ptr self__;
ptr s__;
int i__;
{

   GEN252_delete_(PATT_(self__,4),STR251_create_(0,s__,i__,0));

   ret0__:
   return;
}

STR234_clear_(self__)
ptr self__;
{

   GEN252_clear_(PATT_(self__,4));

   ret0__:
   return;
}

ptr STR234_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


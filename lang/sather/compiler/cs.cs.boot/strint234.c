/* strint234.c : Sather class: STRINT_TO_INT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void GEN252_clear_(ptr self__);
extern ptr STR251_create_(ptr self__, ptr ns__, int i__, int nval__);
extern ptr GEN252_create_(ptr self__);
extern ptr GEN252_get_(ptr self__, ptr e__);
extern void GEN252_insert_(ptr self__, ptr e__);
extern void GEN252_delete_(ptr self__, ptr e__);
#include "macros_.h"



ptr STR234_create_(ptr self__);
int STR234_get_(ptr self__, ptr s__, int i__);
void STR234_insert_(ptr self__, ptr s__, int i__, int v__);
void STR234_delete_(ptr self__, ptr s__, int i__);
void STR234_clear_(ptr self__);
ptr STR234_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR234[];

ptr STR234_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(234,0);
   PATT_(res__,4) = (ptr)GEN252_create_(PATT_(res__,4));

   ret0__:
   return (res__);
}

int STR234_get_(ptr self__, ptr s__, int i__)
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

void STR234_insert_(ptr self__, ptr s__, int i__, int v__)
{

   GEN252_insert_(PATT_(self__,4),STR251_create_(0,s__,i__,v__));

   ret0__:
   return;
}

void STR234_delete_(ptr self__, ptr s__, int i__)
{

   GEN252_delete_(PATT_(self__,4),STR251_create_(0,s__,i__,0));

   ret0__:
   return;
}

void STR234_clear_(ptr self__)
{

   GEN252_clear_(PATT_(self__,4));

   ret0__:
   return;
}

ptr STR234_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* int_to86.c : Sather class: INT_TO_INTSET, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr INT164_insert_();
extern ptr INT164_create_();
extern ptr INT182_create_();
extern ptr GEN183_get_();
extern GEN183_insert_();
extern ptr GEN183_create_();
extern GEN183_delete_();
extern GEN183_clear_();
#include "macros_.h"



ptr INT86_create_();
ptr INT86_get_();
INT86_insert_();
INT86_ins_ent_();
INT86_delete_();
INT86_clear_();
ptr INT86_initialize_();
extern int attr_ent_INT86[];

ptr INT86_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(86,0);
   PATT_(res__,4) = (ptr)GEN183_create_(PATT_(res__,4));

   ret0__:
   return (res__);
}

ptr INT86_get_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;
   ptr    ie__ = 0;

   ie__ = (ptr)GEN183_get_(PATT_(self__,4),INT182_create_(0,i__,0));
   if ((ie__ != 0)) {
      res__ = (ptr)PATT_(ie__,12);
   }
   else {
   }

   ret0__:
   return (res__);
}

INT86_insert_(self__,i__,iset__)
ptr self__;
int i__;
ptr iset__;
{

   GEN183_insert_(PATT_(self__,4),INT182_create_(0,i__,iset__));

   ret0__:
   return;
}

INT86_ins_ent_(self__,i__,j__)
ptr self__;
int i__;
int j__;
{
   ptr    ie__ = 0;

   ie__ = (ptr)GEN183_get_(PATT_(self__,4),INT182_create_(0,i__,0));
   if ((ie__ != 0)) {
      PATT_(ie__,12) = (ptr)INT164_insert_(PATT_(ie__,12),j__);
   }
   else {
      INT86_insert_(self__,i__,INT164_insert_(INT164_create_(0),j__));
   }

   ret0__:
   return;
}

INT86_delete_(self__,i__)
ptr self__;
int i__;
{

   GEN183_delete_(PATT_(self__,4),INT182_create_(0,i__,0));

   ret0__:
   return;
}

INT86_clear_(self__)
ptr self__;
{

   GEN183_clear_(PATT_(self__,4));

   ret0__:
   return;
}

ptr INT86_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


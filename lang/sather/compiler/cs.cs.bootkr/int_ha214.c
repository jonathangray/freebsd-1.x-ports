/* int_ha214.c : Sather class: INT_HASH_SET_CURSOR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr INT214_create_();
int INT214_first_();
int INT214_item_();
int INT214_next_();
ptr INT214_initialize_();
extern int attr_ent_INT214[];

ptr INT214_create_(self__,t__)
ptr self__;
ptr t__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(214,0);
   PATT_(res__,12) = (ptr)t__;
   (void)INT214_first_(res__);

   ret0__:
   return (res__);
}

int INT214_first_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   int    val__ = S_int_VOID_;

   CATT_(self__,4) = (char)0;
   IATT_(self__,8) = (int)0;
   while (1) {
      val__ = (int)IATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2));
      if ((val__ >= 0)) {
         goto goto_tag_4787_;
      }
      else {
         if ((IATT_(self__,8) == (IATT_(PATT_(self__,12),12) - 1))) {
            CATT_(self__,4) = (char)1;
            goto goto_tag_4787_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4787_: ;
   res__ = (int)INT214_item_(self__);

   ret0__:
   return (res__);
}

int INT214_item_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((! CATT_(self__,4))) {
      res__ = (int)IATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

int INT214_next_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if (CATT_(self__,4)) {
      goto ret0__;
   }
   else {
   }
   IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
   while (1) {
      if ((IATT_(self__,8) == (IATT_(PATT_(self__,12),12) - 1))) {
         CATT_(self__,4) = (char)1;
         goto goto_tag_4788_;
      }
      else {
         if ((IATT_(PATT_(self__,12), 16 + ((IATT_(self__,8)) << 2)) >= 0)) {
            goto goto_tag_4788_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4788_: ;
   res__ = (int)INT214_item_(self__);

   ret0__:
   return (res__);
}

ptr INT214_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


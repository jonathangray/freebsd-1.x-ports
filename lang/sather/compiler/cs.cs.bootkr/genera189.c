/* genera189.c : Sather class: GENERAL_HASH_CURSOR{STR_HASH_MAP_ELT{INT}}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr GEN189_create_();
ptr GEN189_first_();
ptr GEN189_item_();
ptr GEN189_next_();
ptr GEN189_initialize_();
extern int attr_ent_GEN189[];

ptr GEN189_create_(self__,nt__)
ptr self__;
ptr nt__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(189,0);
   PATT_(res__,12) = (ptr)nt__;
   (void)GEN189_first_(res__);

   ret0__:
   return (res__);
}

ptr GEN189_first_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    val__ = 0;

   CATT_(self__,4) = (char)0;
   IATT_(self__,8) = (int)0;
   while (1) {
      val__ = (ptr)PATT_(PATT_(PATT_(self__,12),8), 8 + ((IATT_(self__,8)) << 2));
      if ((val__ != 0)) {
         goto goto_tag_4675_;
      }
      else {
         if ((IATT_(self__,8) >= (IATT_(PATT_(PATT_(self__,12),8),4) - 1))) {
            CATT_(self__,4) = (char)1;
            goto goto_tag_4675_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4675_: ;
   res__ = (ptr)GEN189_item_(self__);

   ret0__:
   return (res__);
}

ptr GEN189_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((! CATT_(self__,4))) {
      res__ = (ptr)PATT_(PATT_(PATT_(self__,12),8), 8 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr GEN189_next_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    val__ = 0;

   if (CATT_(self__,4)) {
      goto ret0__;
   }
   else {
   }
   IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
   while (1) {
      if ((IATT_(self__,8) >= (IATT_(PATT_(PATT_(self__,12),8),4) - 1))) {
         CATT_(self__,4) = (char)1;
         goto goto_tag_4676_;
      }
      else {
         val__ = (ptr)PATT_(PATT_(PATT_(self__,12),8), 8 + ((IATT_(self__,8)) << 2));
         if ((val__ != 0)) {
            goto goto_tag_4676_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4676_: ;
   res__ = (ptr)GEN189_item_(self__);

   ret0__:
   return (res__);
}

ptr GEN189_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


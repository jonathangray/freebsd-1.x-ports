/* genera255.c : Sather class: GENERAL_HASH_CURSOR{STRINT_HASH_ELT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



ptr GEN255_create_(ptr self__, ptr nt__);
ptr GEN255_first_(ptr self__);
ptr GEN255_item_(ptr self__);
ptr GEN255_next_(ptr self__);
ptr GEN255_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_GEN255[];

ptr GEN255_create_(ptr self__, ptr nt__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(255,0);
   PATT_(res__,12) = (ptr)nt__;
   (void)GEN255_first_(res__);

   ret0__:
   return (res__);
}

ptr GEN255_first_(ptr self__)
{
   ptr res__ = 0;
   ptr    val__ = 0;

   CATT_(self__,4) = (char)0;
   IATT_(self__,8) = (int)0;
   while (1) {
      val__ = (ptr)PATT_(PATT_(PATT_(self__,12),8), 8 + ((IATT_(self__,8)) << 2));
      if ((val__ != 0)) {
         goto goto_tag_4879_;
      }
      else {
         if ((IATT_(self__,8) >= (IATT_(PATT_(PATT_(self__,12),8),4) - 1))) {
            CATT_(self__,4) = (char)1;
            goto goto_tag_4879_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4879_: ;
   res__ = (ptr)GEN255_item_(self__);

   ret0__:
   return (res__);
}

ptr GEN255_item_(ptr self__)
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

ptr GEN255_next_(ptr self__)
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
         goto goto_tag_4880_;
      }
      else {
         val__ = (ptr)PATT_(PATT_(PATT_(self__,12),8), 8 + ((IATT_(self__,8)) << 2));
         if ((val__ != 0)) {
            goto goto_tag_4880_;
         }
         else {
            IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
         }
      }
   }
goto_tag_4880_: ;
   res__ = (ptr)GEN255_item_(self__);

   ret0__:
   return (res__);
}

ptr GEN255_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


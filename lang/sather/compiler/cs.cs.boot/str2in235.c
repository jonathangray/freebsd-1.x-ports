/* str2in235.c : Sather class: STR2INT2STR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr GEN174_get_(ptr self__, ptr e__);
extern void ARR77_clear_(ptr self__);
extern ptr STR173_create_(ptr self__, ptr str__, int val__);
extern void GEN174_insert_(ptr self__, ptr e__);
extern ptr GEN174_create_(ptr self__);
#include "macros_.h"



ptr STR235_create_(ptr self__);
int STR235_geti_(ptr self__, ptr s__);
ptr STR235_gets_(ptr self__, int i__);
char STR235_insert_(ptr self__, ptr s__, int v__);
ptr STR235_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR235[];

ptr STR235_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(235,0);
   PATT_(res__,4) = (ptr)GEN174_create_(PATT_(res__,4));
   PATT_(res__,12) = (ptr)new1_(77,100,0);

   ret0__:
   return (res__);
}

int STR235_geti_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   ptr    te__ = 0;

   te__ = (ptr)GEN174_get_(PATT_(self__,4),STR173_create_(0,s__,0));
   if ((te__ != 0)) {
      res__ = (int)IATT_(te__,12);
      goto ret0__;
   }
   else {
   }
   res__ = (int)(- 1);

   ret0__:
   return (res__);
}

ptr STR235_gets_(ptr self__, int i__)
{
   ptr res__ = 0;
   SATHER_STR_(20,1,ls1016_,"");

   if (((i__ >= IATT_(PATT_(self__,12),4)) | (i__ < 0))) {
      res__ = (ptr)(ptr)(&ls1016_);
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)PATT_(PATT_(self__,12), 8 + ((i__) << 2));
   if ((res__ == 0)) {
      res__ = (ptr)(ptr)(&ls1016_);
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR235_insert_(ptr self__, ptr s__, int v__)
{
   char res__ = S_char_VOID_;
   ptr    a__ = 0;

   res__ = (char)0;
   if ((STR235_geti_(self__,s__) == (- 1))) {
      GEN174_insert_(PATT_(self__,4),STR173_create_(0,s__,v__));
      while (1) {
         if ((v__ < IATT_(PATT_(self__,12),4))) {
            goto goto_tag_4842_;
         }
         else {
         }
         a__ = (ptr)PATT_(self__,12);
         PATT_(self__,12) = (ptr)extend1_(a__,(2 * IATT_(a__,4)),0);
         ARR77_clear_(a__);
      }
   goto_tag_4842_: ;
      PATT_(PATT_(self__,12), 8 + ((v__) << 2)) = (ptr)s__;
      res__ = (char)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR235_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


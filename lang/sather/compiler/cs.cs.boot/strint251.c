/* strint251.c : Sather class: STRINT_HASH_ELT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_bit_xor_(int self__, int i__);
extern int INT15_bit_and_(int self__, int i__);
extern char STR20_is_equal_(ptr self__, ptr st__);
extern int CHA14_to_i_(char self__);
extern int INT15_lshift_(int self__, int i__);
#include "macros_.h"



ptr STR251_create_(ptr self__, ptr ns__, int i__, int nval__);
int STR251_str_hash_(ptr self__, ptr st__, int i__);
char STR251_is_equal_(ptr self__, ptr e__);
ptr STR251_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR251[];

ptr STR251_create_(ptr self__, ptr ns__, int i__, int nval__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(251,0);
   PATT_(res__,4) = (ptr)ns__;
   IATT_(res__,16) = (int)nval__;
   IATT_(res__,8) = (int)i__;
   IATT_(res__,12) = (int)STR251_str_hash_(self__,ns__,i__);

   ret0__:
   return (res__);
}

int STR251_str_hash_(ptr self__, ptr st__, int i__)
{
   int res__ = S_int_VOID_;
   int    i_488_ = S_int_VOID_;

   i_488_ = S_int_VOID_;
   while (1) {
      if ((CATT_(st__, 8 + ((i_488_))) == '\00')) {
         goto ret0__;
      }
      else {
      }
      res__ = (int)INT15_bit_xor_(res__,INT15_lshift_(CHA14_to_i_(CATT_(st__, 8 + ((i_488_)))),INT15_bit_and_(i_488_,15)));
      i_488_ = (int)(i_488_ + 1);
   }
   res__ = (int)INT15_bit_xor_(res__,i_488_);

   ret0__:
   return (res__);
}

char STR251_is_equal_(ptr self__, ptr e__)
{
   char res__ = S_char_VOID_;

   res__ = (char)0;
   if ((IATT_(e__,12) == IATT_(self__,12))) {
      if ((IATT_(e__,8) == IATT_(self__,8))) {
         res__ = (char)STR20_is_equal_(PATT_(self__,4),PATT_(e__,4));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR251_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


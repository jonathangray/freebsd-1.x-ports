/* class_166.c : Sather class: CLASS_KEY_SET, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern ptr OUT9_s_();
extern ptr OUT9_i_();
extern ptr OUT9_c_();
#include "macros_.h"



ptr CLA166_create_();
CLA166_insert_();
CLA166_display_();
char CLA166_exists_();
CLA166_clear_();
ptr CLA166_initialize_();
extern int attr_ent_CLA166[];

ptr CLA166_create_(self__,sz__)
ptr self__;
int sz__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(166,0);
   PATT_(res__,4) = (ptr)new1_(98,sz__,1);
   PATT_(res__,8) = (ptr)new1_(98,sz__,1);
   CLA166_clear_(res__);

   ret0__:
   return (res__);
}

CLA166_insert_(self__,ci__,k__)
ptr self__;
int ci__;
int k__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,8),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4506_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,8), 16 + ((i__) << 2)) == k__)) {
         if (((IATT_(PATT_(self__,4), 16 + ((i__) << 2)) == 0) | (IATT_(PATT_(self__,4), 16 + ((i__) << 2)) == ci__))) {
            goto ret0__;
         }
         else {
         }
      }
      else {
         if ((IATT_(PATT_(self__,8), 16 + ((i__) << 2)) == (- 1))) {
            IATT_(PATT_(self__,8), 16 + ((i__) << 2)) = (int)k__;
            IATT_(PATT_(self__,4), 16 + ((i__) << 2)) = (int)ci__;
            goto ret0__;
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4506_: ;

   ret0__:
   return;
}

CLA166_display_(self__)
ptr self__;
{
   SATHER_STR_(20,15,ls2873_,"Class index = ");
   SATHER_STR_(20,16,ls2874_,"; identifier = ");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,8),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4507_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,8), 16 + ((i__) << 2)) != (- 1))) {
         (void)OUT9_c_(OUT9_s_(OUT9_s_(OUT9_i_(OUT9_s_(0,(ptr)(&ls2873_)),IATT_(PATT_(self__,4), 16 + ((i__) << 2))),(ptr)(&ls2874_)),STR69_at_index_(GLO68_str_table_,IATT_(PATT_(self__,8), 16 + ((i__) << 2)))),'\n');
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4507_: ;

   ret0__:
   return;
}

char CLA166_exists_(self__,ci__,k__)
ptr self__;
int ci__;
int k__;
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,8),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4508_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,8), 16 + ((i__) << 2)) == k__)) {
         if (((IATT_(PATT_(self__,4), 16 + ((i__) << 2)) == 0) | (IATT_(PATT_(self__,4), 16 + ((i__) << 2)) == ci__))) {
            res__ = (char)1;
            goto ret0__;
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4508_: ;

   ret0__:
   return (res__);
}

CLA166_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ >= IATT_(PATT_(self__,8),12))) {
         goto goto_tag_4509_;
      }
      else {
      }
      IATT_(PATT_(self__,8), 16 + ((i__) << 2)) = (int)(- 1);
      i__ = (int)(i__ + 1);
   }
goto_tag_4509_: ;

   ret0__:
   return;
}

ptr CLA166_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


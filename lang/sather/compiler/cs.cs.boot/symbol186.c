/* symbol186.c : Sather class: SYMBOL_TABLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);

extern ptr LIS98_create_(ptr self__, int init_size__);
extern char LIS98_is_empty_(ptr self__);
extern ptr LIS98_push_(ptr self__, int e__);
extern int LIS98_pop_(ptr self__);
extern ptr HAS230_create_(ptr self__, int size__);
extern ptr LIS231_push_(ptr self__, ptr e__);
extern ptr LIS231_create_(ptr self__, int init_size__);
extern char HAS230_remove_obj_(ptr self__, int nm__);
extern ptr HAS230_get_obj_(ptr self__, int nm__);
extern void HAS230_add_unique_obj_(ptr self__, int nm__, ptr ob__);
extern char HAS230_add_obj_(ptr self__, int nm__, ptr ob__);
#include "macros_.h"



void SYM186_error_msg_(ptr self__, ptr s__);
void SYM186_error_exit_(ptr self__, ptr s__);
ptr SYM186_create_(ptr self__, int sz__, ptr cls__);
void SYM186_add_feature_(ptr self__, int nm__, ptr new_fo__);
void SYM186_del_feature_(ptr self__, int nm__);
ptr SYM186_get_feature_(ptr self__, int nm__);
char SYM186_defined_in_curr_scope_p_(ptr self__, int nm__);
char SYM186_defined_in_all_scopes_p_(ptr self__, int nm__);
char SYM186_defined_in_non_outer_scopes_p_(ptr self__, int nm__);
void SYM186_enter_unique_sym_(ptr self__, int nm__, ptr item__);
void SYM186_enter_sym_(ptr self__, int nm__, ptr item__);
ptr SYM186_get_sym_curr_scope_(ptr self__, int nm__);
ptr SYM186_get_sym_(ptr self__, int nm__);
void SYM186_enter_new_scope_(ptr self__);
void SYM186_leave_new_scope_(ptr self__);
ptr SYM186_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_SYM186[];

void SYM186_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void SYM186_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

ptr SYM186_create_(ptr self__, int sz__, ptr cls__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(186,0);
   PATT_(res__,4) = (ptr)cls__;
   PATT_(res__,8) = (ptr)LIS231_push_(LIS231_create_(0,4),HAS230_create_(0,20));
   IATT_(res__,16) = (int)0;
   PATT_(res__,12) = (ptr)LIS98_create_(PATT_(res__,12),20);

   ret0__:
   return (res__);
}

void SYM186_add_feature_(ptr self__, int nm__, ptr new_fo__)
{

   if ((! LIS98_is_empty_(PATT_(self__,12)))) {
      goto ret0__;
   }
   else {
   }
   SYM186_enter_unique_sym_(self__,nm__,new_fo__);

   ret0__:
   return;
}

void SYM186_del_feature_(ptr self__, int nm__)
{
   char    b__ = S_char_VOID_;

   if ((! LIS98_is_empty_(PATT_(self__,12)))) {
      goto ret0__;
   }
   else {
   }
   b__ = (char)HAS230_remove_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__);

   ret0__:
   return;
}

ptr SYM186_get_feature_(ptr self__, int nm__)
{
   ptr res__ = 0;

   res__ = (ptr)HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((0) << 2)),nm__);

   ret0__:
   return (res__);
}

char SYM186_defined_in_curr_scope_p_(ptr self__, int nm__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__) != 0);

   ret0__:
   return (res__);
}

char SYM186_defined_in_all_scopes_p_(ptr self__, int nm__)
{
   char res__ = S_char_VOID_;
   int    limit__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   limit__ = S_int_VOID_;
   if (((IATT_(PATT_(self__,12),4) - 1) <= 0)) {
      limit__ = (int)IATT_(self__,16);
   }
   else {
      limit__ = (int)IATT_(PATT_(self__,12), 16 + ((1) << 2));
   }
   i__ = (int)(IATT_(PATT_(self__,8),4) - 1);
   while (1) {
      if ((i__ < limit__)) {
         goto goto_tag_4582_;
      }
      else {
      }
      res__ = (char)(HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((i__) << 2)),nm__) != 0);
      if (res__) {
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ - 1);
   }
goto_tag_4582_: ;
   res__ = (char)(HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(PATT_(self__,12), 16 + ((0) << 2))) << 2)),nm__) != 0);

   ret0__:
   return (res__);
}

char SYM186_defined_in_non_outer_scopes_p_(ptr self__, int nm__)
{
   char res__ = S_char_VOID_;
   int    limit__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if (LIS98_is_empty_(PATT_(self__,12))) {
      goto ret0__;
   }
   else {
   }
   limit__ = S_int_VOID_;
   if (((IATT_(PATT_(self__,12),4) - 1) <= 0)) {
      limit__ = (int)IATT_(self__,16);
   }
   else {
      limit__ = (int)IATT_(PATT_(self__,12), 16 + ((1) << 2));
   }
   i__ = (int)(IATT_(PATT_(self__,8),4) - 1);
   while (1) {
      if ((i__ < limit__)) {
         goto goto_tag_4583_;
      }
      else {
      }
      res__ = (char)(HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((i__) << 2)),nm__) != 0);
      if (res__) {
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ - 1);
   }
goto_tag_4583_: ;

   ret0__:
   return (res__);
}

void SYM186_enter_unique_sym_(ptr self__, int nm__, ptr item__)
{

   HAS230_add_unique_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__,item__);

   ret0__:
   return;
}

void SYM186_enter_sym_(ptr self__, int nm__, ptr item__)
{
   char    r__ = S_char_VOID_;

   r__ = (char)HAS230_add_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__,item__);

   ret0__:
   return;
}

ptr SYM186_get_sym_curr_scope_(ptr self__, int nm__)
{
   ptr res__ = 0;

   res__ = (ptr)HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__);

   ret0__:
   return (res__);
}

ptr SYM186_get_sym_(ptr self__, int nm__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    index__ = S_int_VOID_;

   res__ = (ptr)HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((IATT_(self__,16)) << 2)),nm__);
   if ((res__ == 0)) {
      i__ = (int)(IATT_(PATT_(self__,12),4) - 1);
      while (1) {
         if ((i__ < 0)) {
            goto goto_tag_4584_;
         }
         else {
         }
         index__ = (int)IATT_(PATT_(self__,12), 16 + ((i__) << 2));
         res__ = (ptr)HAS230_get_obj_(PATT_(PATT_(self__,8), 16 + ((index__) << 2)),nm__);
         if ((res__ != 0)) {
            goto goto_tag_4584_;
         }
         else {
         }
         i__ = (int)(i__ - 1);
      }
   goto_tag_4584_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

void SYM186_enter_new_scope_(ptr self__)
{

   PATT_(self__,12) = (ptr)LIS98_push_(PATT_(self__,12),IATT_(self__,16));
   IATT_(self__,16) = (int)IATT_(PATT_(self__,8),4);
   PATT_(self__,8) = (ptr)LIS231_push_(PATT_(self__,8),HAS230_create_(0,10));

   ret0__:
   return;
}

void SYM186_leave_new_scope_(ptr self__)
{

   if (LIS98_is_empty_(PATT_(self__,12))) {
      goto ret0__;
   }
   else {
   }
   IATT_(self__,16) = (int)LIS98_pop_(PATT_(self__,12));

   ret0__:
   return;
}

ptr SYM186_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* str_bu70.c : Sather class: STR_BUFFER, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int STR20_length_();
#include "macros_.h"



STR70_clear_();
/*constant*/ int STR70_def_init_size_ = 5;
ptr STR70_create_();
ptr STR70_push_();
int STR70_size_();
char STR70_is_empty_();
char STR70_pop_();
char STR70_top_();
STR70_init_iterate_();
char STR70_curr_item_();
char STR70_next_item_();
char STR70_prev_item_();
ptr STR70_push_unique_();
ptr STR70_append_();
ptr STR70_union_();
char STR70_not_in_();
int STR70_contains_();
ptr STR70_initialize_();
/*constant*/ int STR70_def_str_len_ = 20;
ptr STR70_strval_();
char STR70_is_equal_();
ptr STR70_terminate_();
int STR70_length_();
STR70_init_();
extern int attr_ent_STR70[];

STR70_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_968_;
      }
      else {
      }
      CATT_(self__, 16 + ((i__))) = (char)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_968_: ;

   ret0__:
   return;
}

ptr STR70_create_(self__,init_str_len__)
ptr self__;
int init_str_len__;
{
   ptr res__ = 0;

   if ((init_str_len__ <= 0)) {
      res__ = (ptr)new1_(70,20,1);
   }
   else {
      res__ = (ptr)new1_(70,init_str_len__,1);
   }

   ret0__:
   return (res__);
}

ptr STR70_push_(self__,e__)
ptr self__;
char e__;
{
   ptr res__ = 0;

   if ((IATT_(self__,4) < IATT_(self__,12))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,12)),1);
   }
   CATT_(res__, 16 + ((IATT_(self__,4)))) = (char)e__;
   IATT_(res__,4) = (int)(IATT_(res__,4) + 1);

   ret0__:
   return (res__);
}

int STR70_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,4);
   }

   ret0__:
   return (res__);
}

char STR70_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

char STR70_pop_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (STR70_is_empty_(self__)) {
      res__ = (char)0;
   }
   else {
      IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
      res__ = (char)CATT_(self__, 16 + ((IATT_(self__,4))));
      CATT_(self__, 16 + ((IATT_(self__,4)))) = (char)0;
   }

   ret0__:
   return (res__);
}

char STR70_top_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (STR70_is_empty_(self__)) {
      res__ = (char)0;
   }
   else {
      res__ = (char)CATT_(self__, 16 + (((IATT_(self__,4) - 1))));
   }

   ret0__:
   return (res__);
}

STR70_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,8) = (int)0;

   ret0__:
   return;
}

char STR70_curr_item_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if ((IATT_(self__,8) < IATT_(self__,4))) {
      res__ = (char)CATT_(self__, 16 + ((IATT_(self__,8))));
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR70_next_item_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (((IATT_(self__,8) + 1) < IATT_(self__,4))) {
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      res__ = (char)CATT_(self__, 16 + ((IATT_(self__,8))));
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR70_prev_item_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (((IATT_(self__,8) - 1) >= 0)) {
      IATT_(self__,8) = (int)(IATT_(self__,8) - 1);
      res__ = (char)CATT_(self__, 16 + ((IATT_(self__,8))));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR70_push_unique_(self__,e__)
ptr self__;
char e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)STR70_contains_(self__,e__);
   if ((k__ >= 0)) {
      CATT_(self__, 16 + ((k__))) = (char)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)STR70_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr STR70_append_(self__,list__)
ptr self__;
ptr list__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if ((list__ == 0)) {
      res__ = (ptr)self__;
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)self__;
   i__ = (int)0;
   sz__ = (int)IATT_(list__,4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_969_;
      }
      else {
      }
      res__ = (ptr)STR70_push_(res__,CATT_(list__, 16 + ((i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_969_: ;

   ret0__:
   return (res__);
}

ptr STR70_union_(self__,list__)
ptr self__;
ptr list__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,4);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_970_;
      }
      else {
      }
      if (STR70_not_in_(self__,CATT_(list__, 16 + ((i__))))) {
         res__ = (ptr)STR70_push_(res__,CATT_(list__, 16 + ((i__))));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_970_: ;

   ret0__:
   return (res__);
}

char STR70_not_in_(self__,e__)
ptr self__;
char e__;
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (char)1;
   while (1) {
      if ((i__ >= IATT_(self__,4))) {
         goto goto_tag_971_;
      }
      else {
      }
      if ((CATT_(self__, 16 + ((i__))) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_971_: ;

   ret0__:
   return (res__);
}

int STR70_contains_(self__,e__)
ptr self__;
char e__;
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (int)(- 1);
   while (1) {
      if ((i__ >= IATT_(self__,4))) {
         goto goto_tag_972_;
      }
      else {
      }
      if ((CATT_(self__, 16 + ((i__))) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_972_: ;

   ret0__:
   return (res__);
}

ptr STR70_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR70_strval_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl973_;
   int    i__ = S_int_VOID_;

   gl973_ = res__;
   res__ = (ptr)new1_(20,(IATT_(self__,4) + 1),1);
   i__ = (int)0;
   while (1) {
      if (((CATT_(self__, 16 + ((i__))) == '\00') | (i__ >= IATT_(self__,4)))) {
         goto goto_tag_974_;
      }
      else {
      }
      CATT_(res__, 8 + ((i__))) = (char)CATT_(self__, 16 + ((i__)));
      i__ = (int)(i__ + 1);
   }
goto_tag_974_: ;
   CATT_(res__, 8 + ((i__))) = (char)'\00';

   ret0__:
   return (res__);
}

char STR70_is_equal_(self__,strv__)
ptr self__;
ptr strv__;
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   int    l1__ = S_int_VOID_;
   int    l2__ = S_int_VOID_;

   i__ = (int)0;
   l1__ = (int)STR20_length_(strv__);
   l2__ = (int)IATT_(self__,4);
   while (1) {
      if (((i__ >= l1__) | (i__ >= l2__))) {
         goto goto_tag_975_;
      }
      else {
      }
      if ((CATT_(self__, 16 + ((i__))) != CATT_(strv__, 8 + ((i__))))) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_975_: ;
   if (((CATT_(self__, 16 + ((i__))) == '\00') & (CATT_(strv__, 8 + ((i__))) == '\00'))) {
      res__ = (char)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR70_terminate_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,4) < IATT_(self__,12))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,12)),1);
   }
   CATT_(res__, 16 + ((IATT_(self__,4)))) = (char)'\00';

   ret0__:
   return (res__);
}

int STR70_length_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((CATT_(self__, 16 + ((IATT_(self__,4)))) == '\00')) {
      res__ = (int)(IATT_(self__,4) + 1);
   }
   else {
      res__ = (int)IATT_(self__,4);
   }

   ret0__:
   return (res__);
}

STR70_init_(self__)
ptr self__;
{

   IATT_(self__,4) = (int)0;

   ret0__:
   return;
}


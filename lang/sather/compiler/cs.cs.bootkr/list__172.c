/* list__172.c : Sather class: LIST{CHAR}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



LIS172_clear_();
/*constant*/ int LIS172_def_init_size_ = 5;
ptr LIS172_create_();
ptr LIS172_push_();
int LIS172_size_();
char LIS172_is_empty_();
char LIS172_pop_();
char LIS172_top_();
LIS172_init_iterate_();
char LIS172_curr_item_();
char LIS172_next_item_();
char LIS172_prev_item_();
ptr LIS172_push_unique_();
ptr LIS172_append_();
ptr LIS172_union_();
char LIS172_not_in_();
int LIS172_contains_();
ptr LIS172_initialize_();
extern int attr_ent_LIS172[];

LIS172_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_4523_;
      }
      else {
      }
      CATT_(self__, 16 + ((i__))) = (char)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4523_: ;

   ret0__:
   return;
}

ptr LIS172_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(172,5,1);
   }
   else {
      res__ = (ptr)new1_(172,init_size__,1);
   }

   ret0__:
   return (res__);
}

ptr LIS172_push_(self__,e__)
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

int LIS172_size_(self__)
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

char LIS172_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

char LIS172_pop_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (LIS172_is_empty_(self__)) {
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

char LIS172_top_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (LIS172_is_empty_(self__)) {
      res__ = (char)0;
   }
   else {
      res__ = (char)CATT_(self__, 16 + (((IATT_(self__,4) - 1))));
   }

   ret0__:
   return (res__);
}

LIS172_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,8) = (int)0;

   ret0__:
   return;
}

char LIS172_curr_item_(self__)
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

char LIS172_next_item_(self__)
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

char LIS172_prev_item_(self__)
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

ptr LIS172_push_unique_(self__,e__)
ptr self__;
char e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LIS172_contains_(self__,e__);
   if ((k__ >= 0)) {
      CATT_(self__, 16 + ((k__))) = (char)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LIS172_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LIS172_append_(self__,list__)
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
         goto goto_tag_4524_;
      }
      else {
      }
      res__ = (ptr)LIS172_push_(res__,CATT_(list__, 16 + ((i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_4524_: ;

   ret0__:
   return (res__);
}

ptr LIS172_union_(self__,list__)
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
         goto goto_tag_4525_;
      }
      else {
      }
      if (LIS172_not_in_(self__,CATT_(list__, 16 + ((i__))))) {
         res__ = (ptr)LIS172_push_(res__,CATT_(list__, 16 + ((i__))));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4525_: ;

   ret0__:
   return (res__);
}

char LIS172_not_in_(self__,e__)
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
         goto goto_tag_4526_;
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
goto_tag_4526_: ;

   ret0__:
   return (res__);
}

int LIS172_contains_(self__,e__)
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
         goto goto_tag_4527_;
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
goto_tag_4527_: ;

   ret0__:
   return (res__);
}

ptr LIS172_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


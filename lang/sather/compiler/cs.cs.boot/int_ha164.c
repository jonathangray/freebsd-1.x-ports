/* int_ha164.c : Sather class: INT_HASH_SET, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_bit_and_(int self__, int i__);
extern int INT214_item_(ptr self__);
extern ptr INT214_create_(ptr self__, ptr t__);
extern int INT214_next_(ptr self__);
extern int INT15_lshift_(int self__, int i__);
#include "macros_.h"



void INT164_clear_(ptr self__);
ptr INT164_create_(ptr self__);
char INT164_get_(ptr self__, int k__);
char INT164_test_(ptr self__, int k__);
ptr INT164_double_size_(ptr self__);
ptr INT164_insert_(ptr self__, int k__);
void INT164_delete_(ptr self__, int k__);
int INT164_size_(ptr self__);
ptr INT164_cursor_(ptr self__);
ptr INT164_union_(ptr self__, ptr s__);
ptr INT164_intersection_(ptr self__, ptr s__);
ptr INT164_difference_(ptr self__, ptr s__);
ptr INT164_sym_difference_(ptr self__, ptr s__);
char INT164_is_a_subset_of_(ptr self__, ptr s__);
ptr INT164_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_INT164[];

void INT164_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)0;
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_4486_;
      }
      else {
      }
      IATT_(self__, 16 + ((i__) << 2)) = (int)(- 1);
      i__ = (int)(i__ + 1);
   }
goto_tag_4486_: ;

   ret0__:
   return;
}

ptr INT164_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new1_(164,9,1);
   IATT_(res__,8) = (int)7;
   INT164_clear_(res__);

   ret0__:
   return (res__);
}

char INT164_get_(ptr self__, int k__)
{
   char res__ = S_char_VOID_;
   int    hsh__ = S_int_VOID_;
   int    kt__ = S_int_VOID_;

   hsh__ = (int)INT15_bit_and_(k__,IATT_(self__,8));
   while (1) {
      while (1) {
         kt__ = (int)IATT_(self__, 16 + ((hsh__) << 2));
         if ((kt__ == k__)) {
            res__ = (char)1;
            goto ret0__;
         }
         else {
            if ((kt__ == (- 1))) {
               goto goto_tag_4487_;
            }
            else {
            }
         }
         hsh__ = (int)(hsh__ + 1);
      }
   goto_tag_4487_: ;
      if ((hsh__ != (IATT_(self__,12) - 1))) {
         goto goto_tag_4488_;
      }
      else {
         hsh__ = (int)0;
      }
   }
goto_tag_4488_: ;

   ret0__:
   return (res__);
}

char INT164_test_(ptr self__, int k__)
{
   char res__ = S_char_VOID_;

   res__ = (char)INT164_get_(self__,k__);

   ret0__:
   return (res__);
}

ptr INT164_double_size_(ptr self__)
{
   ptr res__ = 0;
   int    ns__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    val__ = S_int_VOID_;
   int    hsh__ = S_int_VOID_;

   ns__ = (int)(((IATT_(self__,12) - 1) * 2) + 1);
   res__ = (ptr)new1_(164,ns__,1);
   IATT_(res__,8) = (int)(INT15_lshift_(IATT_(self__,8),1) + 1);
   INT164_clear_(res__);
   i__ = (int)0;
   while (1) {
      if ((i__ == (IATT_(self__,12) - 1))) {
         goto goto_tag_4489_;
      }
      else {
      }
      val__ = (int)IATT_(self__, 16 + ((i__) << 2));
      if ((val__ >= 0)) {
         hsh__ = (int)INT15_bit_and_(val__,IATT_(res__,8));
         while (1) {
            if ((IATT_(res__, 16 + ((hsh__) << 2)) == (- 1))) {
               IATT_(res__, 16 + ((hsh__) << 2)) = (int)val__;
               IATT_(res__,4) = (int)(IATT_(res__,4) + 1);
               goto goto_tag_4490_;
            }
            else {
               if ((hsh__ == (ns__ - 2))) {
                  hsh__ = (int)0;
               }
               else {
                  hsh__ = (int)(hsh__ + 1);
               }
            }
         }
      goto_tag_4490_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4489_: ;
   INT164_clear_(self__);

   ret0__:
   return (res__);
}

ptr INT164_insert_(ptr self__, int k__)
{
   ptr res__ = 0;
   int    hsh__ = S_int_VOID_;

   if ((INT15_lshift_((IATT_(self__,4) + 1),1) > IATT_(self__,12))) {
      res__ = (ptr)INT164_double_size_(self__);
   }
   else {
      res__ = (ptr)self__;
   }
   hsh__ = (int)INT15_bit_and_(k__,IATT_(res__,8));
   while (1) {
      if ((IATT_(res__, 16 + ((hsh__) << 2)) <= (- 1))) {
         IATT_(res__, 16 + ((hsh__) << 2)) = (int)k__;
         IATT_(res__,4) = (int)(IATT_(res__,4) + 1);
         goto goto_tag_4491_;
      }
      else {
         if ((IATT_(res__, 16 + ((hsh__) << 2)) == k__)) {
            goto goto_tag_4491_;
         }
         else {
            if ((hsh__ == (IATT_(res__,12) - 2))) {
               hsh__ = (int)0;
            }
            else {
               hsh__ = (int)(hsh__ + 1);
            }
         }
      }
   }
goto_tag_4491_: ;

   ret0__:
   return (res__);
}

void INT164_delete_(ptr self__, int k__)
{
   int    hsh__ = S_int_VOID_;

   hsh__ = (int)INT15_bit_and_(k__,IATT_(self__,8));
   while (1) {
      if ((IATT_(self__, 16 + ((hsh__) << 2)) == (- 1))) {
         goto goto_tag_4492_;
      }
      else {
         if ((IATT_(self__, 16 + ((hsh__) << 2)) == k__)) {
            IATT_(self__, 16 + ((hsh__) << 2)) = (int)(- 2);
            goto goto_tag_4492_;
         }
         else {
            if ((hsh__ == (IATT_(self__,12) - 2))) {
               hsh__ = (int)0;
            }
            else {
               hsh__ = (int)(hsh__ + 1);
            }
         }
      }
   }
goto_tag_4492_: ;

   ret0__:
   return;
}

int INT164_size_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_4493_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) >= 0)) {
         res__ = (int)(res__ + 1);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4493_: ;

   ret0__:
   return (res__);
}

ptr INT164_cursor_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)INT214_create_(0,self__);

   ret0__:
   return (res__);
}

ptr INT164_union_(ptr self__, ptr s__)
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)INT164_create_(self__);
   c__ = (ptr)INT164_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4494_;
      }
      else {
      }
      res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      (void)INT214_next_(c__);
   }
goto_tag_4494_: ;
   c__ = (ptr)INT164_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4495_;
      }
      else {
      }
      res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      (void)INT214_next_(c__);
   }
goto_tag_4495_: ;

   ret0__:
   return (res__);
}

ptr INT164_intersection_(ptr self__, ptr s__)
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)INT164_create_(self__);
   c__ = (ptr)INT164_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4496_;
      }
      else {
      }
      if (INT164_get_(s__,INT214_item_(c__))) {
         res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      }
      else {
      }
      (void)INT214_next_(c__);
   }
goto_tag_4496_: ;

   ret0__:
   return (res__);
}

ptr INT164_difference_(ptr self__, ptr s__)
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)INT164_create_(self__);
   c__ = (ptr)INT164_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4497_;
      }
      else {
      }
      if ((! INT164_get_(s__,INT214_item_(c__)))) {
         res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      }
      else {
      }
      (void)INT214_next_(c__);
   }
goto_tag_4497_: ;

   ret0__:
   return (res__);
}

ptr INT164_sym_difference_(ptr self__, ptr s__)
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)INT164_create_(self__);
   c__ = (ptr)INT164_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4498_;
      }
      else {
      }
      if ((! INT164_get_(s__,INT214_item_(c__)))) {
         res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      }
      else {
      }
      (void)INT214_next_(c__);
   }
goto_tag_4498_: ;
   c__ = (ptr)INT164_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4499_;
      }
      else {
      }
      if ((! INT164_get_(self__,INT214_item_(c__)))) {
         res__ = (ptr)INT164_insert_(res__,INT214_item_(c__));
      }
      else {
      }
      (void)INT214_next_(c__);
   }
goto_tag_4499_: ;

   ret0__:
   return (res__);
}

char INT164_is_a_subset_of_(ptr self__, ptr s__)
{
   char res__ = S_char_VOID_;
   ptr    c__ = 0;

   c__ = (ptr)INT164_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4500_;
      }
      else {
      }
      if ((! INT164_get_(s__,INT214_item_(c__)))) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      (void)INT214_next_(c__);
   }
goto_tag_4500_: ;
   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr INT164_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


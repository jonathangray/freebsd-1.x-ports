/* list__98.c : Sather class: LIST{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



void LIS98_clear_(ptr self__);
/*constant*/ int LIS98_def_init_size_ = 5;
ptr LIS98_create_(ptr self__, int init_size__);
ptr LIS98_push_(ptr self__, int e__);
int LIS98_size_(ptr self__);
char LIS98_is_empty_(ptr self__);
int LIS98_pop_(ptr self__);
int LIS98_top_(ptr self__);
void LIS98_init_iterate_(ptr self__);
int LIS98_curr_item_(ptr self__);
int LIS98_next_item_(ptr self__);
int LIS98_prev_item_(ptr self__);
ptr LIS98_push_unique_(ptr self__, int e__);
ptr LIS98_append_(ptr self__, ptr list__);
ptr LIS98_union_(ptr self__, ptr list__);
char LIS98_not_in_(ptr self__, int e__);
int LIS98_contains_(ptr self__, int e__);
ptr LIS98_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_LIS98[];

void LIS98_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_1180_;
      }
      else {
      }
      IATT_(self__, 16 + ((i__) << 2)) = (int)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_1180_: ;

   ret0__:
   return;
}

ptr LIS98_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(98,5,1);
   }
   else {
      res__ = (ptr)new1_(98,init_size__,1);
   }

   ret0__:
   return (res__);
}

ptr LIS98_push_(ptr self__, int e__)
{
   ptr res__ = 0;

   if ((IATT_(self__,4) < IATT_(self__,12))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,12)),1);
   }
   IATT_(res__, 16 + ((IATT_(self__,4)) << 2)) = (int)e__;
   IATT_(res__,4) = (int)(IATT_(res__,4) + 1);

   ret0__:
   return (res__);
}

int LIS98_size_(ptr self__)
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

char LIS98_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

int LIS98_pop_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (LIS98_is_empty_(self__)) {
      res__ = (int)0;
   }
   else {
      IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,4)) << 2));
      IATT_(self__, 16 + ((IATT_(self__,4)) << 2)) = (int)0;
   }

   ret0__:
   return (res__);
}

int LIS98_top_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (LIS98_is_empty_(self__)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__, 16 + (((IATT_(self__,4) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LIS98_init_iterate_(ptr self__)
{

   IATT_(self__,8) = (int)0;

   ret0__:
   return;
}

int LIS98_curr_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((IATT_(self__,8) < IATT_(self__,4))) {
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

int LIS98_next_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (((IATT_(self__,8) + 1) < IATT_(self__,4))) {
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

int LIS98_prev_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (((IATT_(self__,8) - 1) >= 0)) {
      IATT_(self__,8) = (int)(IATT_(self__,8) - 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LIS98_push_unique_(ptr self__, int e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LIS98_contains_(self__,e__);
   if ((k__ >= 0)) {
      IATT_(self__, 16 + ((k__) << 2)) = (int)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LIS98_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LIS98_append_(ptr self__, ptr list__)
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
         goto goto_tag_1181_;
      }
      else {
      }
      res__ = (ptr)LIS98_push_(res__,IATT_(list__, 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_1181_: ;

   ret0__:
   return (res__);
}

ptr LIS98_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,4);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1182_;
      }
      else {
      }
      if (LIS98_not_in_(self__,IATT_(list__, 16 + ((i__) << 2)))) {
         res__ = (ptr)LIS98_push_(res__,IATT_(list__, 16 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1182_: ;

   ret0__:
   return (res__);
}

char LIS98_not_in_(ptr self__, int e__)
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
         goto goto_tag_1183_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1183_: ;

   ret0__:
   return (res__);
}

int LIS98_contains_(ptr self__, int e__)
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
         goto goto_tag_1184_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1184_: ;

   ret0__:
   return (res__);
}

ptr LIS98_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


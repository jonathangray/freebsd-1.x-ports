/* list__87.c : Sather class: LIST{STR}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



LIS87_clear_();
/*constant*/ int LIS87_def_init_size_ = 5;
ptr LIS87_create_();
ptr LIS87_push_();
int LIS87_size_();
char LIS87_is_empty_();
ptr LIS87_pop_();
ptr LIS87_top_();
LIS87_init_iterate_();
ptr LIS87_curr_item_();
ptr LIS87_next_item_();
ptr LIS87_prev_item_();
ptr LIS87_push_unique_();
ptr LIS87_append_();
ptr LIS87_union_();
char LIS87_not_in_();
int LIS87_contains_();
ptr LIS87_initialize_();
extern int attr_ent_LIS87[];

LIS87_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_1016_;
      }
      else {
      }
      PATT_(self__, 16 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_1016_: ;

   ret0__:
   return;
}

ptr LIS87_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(87,5,0);
   }
   else {
      res__ = (ptr)new1_(87,init_size__,0);
   }

   ret0__:
   return (res__);
}

ptr LIS87_push_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;

   if ((IATT_(self__,4) < IATT_(self__,12))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,12)),0);
   }
   PATT_(res__, 16 + ((IATT_(self__,4)) << 2)) = (ptr)e__;
   IATT_(res__,4) = (int)(IATT_(res__,4) + 1);

   ret0__:
   return (res__);
}

int LIS87_size_(self__)
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

char LIS87_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

ptr LIS87_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LIS87_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
      res__ = (ptr)PATT_(self__, 16 + ((IATT_(self__,4)) << 2));
      PATT_(self__, 16 + ((IATT_(self__,4)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LIS87_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LIS87_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 16 + (((IATT_(self__,4) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LIS87_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,8) = (int)0;

   ret0__:
   return;
}

ptr LIS87_curr_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,8) < IATT_(self__,4))) {
      res__ = (ptr)PATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LIS87_next_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,8) + 1) < IATT_(self__,4))) {
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      res__ = (ptr)PATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LIS87_prev_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,8) - 1) >= 0)) {
      IATT_(self__,8) = (int)(IATT_(self__,8) - 1);
      res__ = (ptr)PATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LIS87_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LIS87_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 16 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LIS87_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LIS87_append_(self__,list__)
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
         goto goto_tag_1017_;
      }
      else {
      }
      res__ = (ptr)LIS87_push_(res__,PATT_(list__, 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_1017_: ;

   ret0__:
   return (res__);
}

ptr LIS87_union_(self__,list__)
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
         goto goto_tag_1018_;
      }
      else {
      }
      if (LIS87_not_in_(self__,PATT_(list__, 16 + ((i__) << 2)))) {
         res__ = (ptr)LIS87_push_(res__,PATT_(list__, 16 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1018_: ;

   ret0__:
   return (res__);
}

char LIS87_not_in_(self__,e__)
ptr self__;
ptr e__;
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
         goto goto_tag_1019_;
      }
      else {
      }
      if ((PATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1019_: ;

   ret0__:
   return (res__);
}

int LIS87_contains_(self__,e__)
ptr self__;
ptr e__;
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
         goto goto_tag_1020_;
      }
      else {
      }
      if ((PATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1020_: ;

   ret0__:
   return (res__);
}

ptr LIS87_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


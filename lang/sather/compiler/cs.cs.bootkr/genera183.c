/* genera183.c : Sather class: GENERAL_HASH{INT_HASH_ELT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_bit_and_();
extern ARR228_clear_();
extern ptr GEN229_create_();
extern char INT182_is_equal_();
extern ptr GEN229_item_();
extern ptr GEN229_next_();
extern int INT15_lshift_();
#include "macros_.h"



ptr GEN183_create_();
ptr GEN183_get_();
char GEN183_test_();
GEN183_double_size_();
GEN183_insert_();
GEN183_delete_();
GEN183_clear_();
ptr GEN183_cursor_();
char GEN183_is_empty_();
ptr GEN183_union_();
ptr GEN183_intersection_();
ptr GEN183_difference_();
ptr GEN183_sym_difference_();
char GEN183_is_a_subset_of_();
ptr GEN183_initialize_();
extern int attr_ent_GEN183[];

ptr GEN183_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(183,0);
   PATT_(res__,8) = (ptr)new1_(228,9,0);
   IATT_(res__,12) = (int)7;

   ret0__:
   return (res__);
}

ptr GEN183_get_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    hsh__ = S_int_VOID_;
   ptr    te__ = 0;

   hsh__ = (int)INT15_bit_and_(IATT_(e__,8),IATT_(self__,12));
   while (1) {
      while (1) {
         te__ = (ptr)PATT_(PATT_(self__,8), 8 + ((hsh__) << 2));
         if ((te__ == 0)) {
            goto goto_tag_4566_;
         }
         else {
            if (INT182_is_equal_(te__,e__)) {
               res__ = (ptr)te__;
               goto ret0__;
            }
            else {
            }
         }
         hsh__ = (int)(hsh__ + 1);
      }
   goto_tag_4566_: ;
      if ((hsh__ != (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4567_;
      }
      else {
         hsh__ = (int)0;
      }
   }
goto_tag_4567_: ;

   ret0__:
   return (res__);
}

char GEN183_test_(self__,e__)
ptr self__;
ptr e__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(GEN183_get_(self__,e__) != 0);

   ret0__:
   return (res__);
}

GEN183_double_size_(self__)
ptr self__;
{
   int    ns__ = S_int_VOID_;
   ptr    ntbl__ = 0;
   int    i__ = S_int_VOID_;
   ptr    val__ = 0;
   int    hsh__ = S_int_VOID_;

   ns__ = (int)(((IATT_(PATT_(self__,8),4) - 1) * 2) + 1);
   IATT_(self__,12) = (int)(INT15_lshift_(IATT_(self__,12),1) + 1);
   ntbl__ = (ptr)new1_(228,ns__,0);
   IATT_(self__,4) = (int)0;
   i__ = (int)0;
   while (1) {
      if ((i__ == (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4568_;
      }
      else {
      }
      val__ = (ptr)PATT_(PATT_(self__,8), 8 + ((i__) << 2));
      if ((val__ != 0)) {
         hsh__ = (int)INT15_bit_and_(IATT_(val__,8),IATT_(self__,12));
         while (1) {
            if ((PATT_(ntbl__, 8 + ((hsh__) << 2)) == 0)) {
               PATT_(ntbl__, 8 + ((hsh__) << 2)) = (ptr)val__;
               IATT_(self__,4) = (int)(IATT_(self__,4) + 1);
               goto goto_tag_4569_;
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
      goto_tag_4569_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4568_: ;
   ARR228_clear_(PATT_(self__,8));
   PATT_(self__,8) = (ptr)ntbl__;

   ret0__:
   return;
}

GEN183_insert_(self__,e__)
ptr self__;
ptr e__;
{
   int    hsh__ = S_int_VOID_;

   if ((INT15_lshift_((IATT_(self__,4) + 1),1) > IATT_(PATT_(self__,8),4))) {
      GEN183_double_size_(self__);
   }
   else {
   }
   hsh__ = (int)INT15_bit_and_(IATT_(e__,8),IATT_(self__,12));
   while (1) {
      if ((PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) == 0)) {
         PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
         IATT_(self__,4) = (int)(IATT_(self__,4) + 1);
         goto goto_tag_4570_;
      }
      else {
         if (INT182_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
            goto goto_tag_4570_;
         }
         else {
         }
      }
      if ((hsh__ == (IATT_(PATT_(self__,8),4) - 2))) {
         hsh__ = (int)0;
      }
      else {
         hsh__ = (int)(hsh__ + 1);
      }
   }
goto_tag_4570_: ;

   ret0__:
   return;
}

GEN183_delete_(self__,e__)
ptr self__;
ptr e__;
{
   int    hsh__ = S_int_VOID_;
   int    hole__ = S_int_VOID_;
   int    index__ = S_int_VOID_;

   hsh__ = (int)INT15_bit_and_(IATT_(e__,8),IATT_(self__,12));
   hole__ = S_int_VOID_;
   while (1) {
      if ((PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if (INT182_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            hole__ = (int)hsh__;
            PATT_(PATT_(self__,8), 8 + ((hole__) << 2)) = (ptr)0;
            IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
            goto goto_tag_4571_;
         }
         else {
            if ((hsh__ == (IATT_(PATT_(self__,8),4) - 2))) {
               hsh__ = (int)0;
            }
            else {
               hsh__ = (int)(hsh__ + 1);
            }
         }
      }
   }
goto_tag_4571_: ;
   index__ = (int)hole__;
   while (1) {
      if ((index__ == (IATT_(PATT_(self__,8),4) - 2))) {
         index__ = (int)0;
      }
      else {
         index__ = (int)(index__ + 1);
      }
      if ((PATT_(PATT_(self__,8), 8 + ((index__) << 2)) == 0)) {
         goto goto_tag_4572_;
      }
      else {
      }
      hsh__ = (int)INT15_bit_and_(IATT_(PATT_(PATT_(self__,8), 8 + ((index__) << 2)),8),IATT_(self__,12));
      if ((hsh__ <= index__)) {
         if (((hole__ < index__) & (hole__ >= hsh__))) {
            PATT_(PATT_(self__,8), 8 + ((hole__) << 2)) = (ptr)PATT_(PATT_(self__,8), 8 + ((index__) << 2));
            hole__ = (int)index__;
            PATT_(PATT_(self__,8), 8 + ((index__) << 2)) = (ptr)0;
         }
         else {
         }
      }
      else {
         if (((hole__ >= hsh__) | (hole__ < index__))) {
            PATT_(PATT_(self__,8), 8 + ((hole__) << 2)) = (ptr)PATT_(PATT_(self__,8), 8 + ((index__) << 2));
            hole__ = (int)index__;
            PATT_(PATT_(self__,8), 8 + ((index__) << 2)) = (ptr)0;
         }
         else {
         }
      }
   }
goto_tag_4572_: ;

   ret0__:
   return;
}

GEN183_clear_(self__)
ptr self__;
{

   IATT_(self__,4) = (int)0;
   ARR228_clear_(PATT_(self__,8));

   ret0__:
   return;
}

ptr GEN183_cursor_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)GEN229_create_(0,self__);

   ret0__:
   return (res__);
}

char GEN183_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

ptr GEN183_union_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN183_create_(self__);
   c__ = (ptr)GEN183_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4573_;
      }
      else {
      }
      GEN183_insert_(res__,GEN229_item_(c__));
      (void)GEN229_next_(c__);
   }
goto_tag_4573_: ;
   c__ = (ptr)GEN183_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4574_;
      }
      else {
      }
      GEN183_insert_(res__,GEN229_item_(c__));
      (void)GEN229_next_(c__);
   }
goto_tag_4574_: ;

   ret0__:
   return (res__);
}

ptr GEN183_intersection_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN183_create_(self__);
   c__ = (ptr)GEN183_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4575_;
      }
      else {
      }
      if ((GEN183_get_(s__,GEN229_item_(c__)) != 0)) {
         GEN183_insert_(res__,GEN229_item_(c__));
      }
      else {
      }
      (void)GEN229_next_(c__);
   }
goto_tag_4575_: ;

   ret0__:
   return (res__);
}

ptr GEN183_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN183_create_(self__);
   c__ = (ptr)GEN183_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4576_;
      }
      else {
      }
      if ((GEN183_get_(s__,GEN229_item_(c__)) == 0)) {
         GEN183_insert_(res__,GEN229_item_(c__));
      }
      else {
      }
      (void)GEN229_next_(c__);
   }
goto_tag_4576_: ;

   ret0__:
   return (res__);
}

ptr GEN183_sym_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN183_create_(self__);
   c__ = (ptr)GEN183_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4577_;
      }
      else {
      }
      if ((GEN183_get_(s__,GEN229_item_(c__)) == 0)) {
         GEN183_insert_(res__,GEN229_item_(c__));
      }
      else {
      }
      (void)GEN229_next_(c__);
   }
goto_tag_4577_: ;
   c__ = (ptr)GEN183_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4578_;
      }
      else {
      }
      if ((GEN183_get_(self__,GEN229_item_(c__)) == 0)) {
         GEN183_insert_(res__,GEN229_item_(c__));
      }
      else {
      }
      (void)GEN229_next_(c__);
   }
goto_tag_4578_: ;

   ret0__:
   return (res__);
}

char GEN183_is_a_subset_of_(self__,s__)
ptr self__;
ptr s__;
{
   char res__ = S_char_VOID_;
   ptr    c__ = 0;

   c__ = (ptr)GEN183_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4579_;
      }
      else {
      }
      if ((GEN183_get_(s__,GEN229_item_(c__)) == 0)) {
         goto ret0__;
      }
      else {
      }
      (void)GEN229_next_(c__);
   }
goto_tag_4579_: ;
   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr GEN183_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


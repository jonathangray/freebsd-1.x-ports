/* genera174.c : Sather class: GENERAL_HASH{STR_HASH_MAP_ELT{INT}}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ARR224_clear_();
extern int INT15_bit_and_();
extern ptr GEN189_item_();
extern ptr GEN189_next_();
extern char STR173_is_equal_();
extern ptr GEN189_create_();
extern int INT15_lshift_();
#include "macros_.h"



ptr GEN174_create_();
ptr GEN174_get_();
char GEN174_test_();
GEN174_double_size_();
GEN174_insert_();
GEN174_delete_();
GEN174_clear_();
ptr GEN174_cursor_();
char GEN174_is_empty_();
ptr GEN174_union_();
ptr GEN174_intersection_();
ptr GEN174_difference_();
ptr GEN174_sym_difference_();
char GEN174_is_a_subset_of_();
ptr GEN174_initialize_();
extern int attr_ent_GEN174[];

ptr GEN174_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(174,0);
   PATT_(res__,8) = (ptr)new1_(224,9,0);
   IATT_(res__,12) = (int)7;

   ret0__:
   return (res__);
}

ptr GEN174_get_(self__,e__)
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
            goto goto_tag_4528_;
         }
         else {
            if (STR173_is_equal_(te__,e__)) {
               res__ = (ptr)te__;
               goto ret0__;
            }
            else {
            }
         }
         hsh__ = (int)(hsh__ + 1);
      }
   goto_tag_4528_: ;
      if ((hsh__ != (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4529_;
      }
      else {
         hsh__ = (int)0;
      }
   }
goto_tag_4529_: ;

   ret0__:
   return (res__);
}

char GEN174_test_(self__,e__)
ptr self__;
ptr e__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(GEN174_get_(self__,e__) != 0);

   ret0__:
   return (res__);
}

GEN174_double_size_(self__)
ptr self__;
{
   int    ns__ = S_int_VOID_;
   ptr    ntbl__ = 0;
   int    i__ = S_int_VOID_;
   ptr    val__ = 0;
   int    hsh__ = S_int_VOID_;

   ns__ = (int)(((IATT_(PATT_(self__,8),4) - 1) * 2) + 1);
   IATT_(self__,12) = (int)(INT15_lshift_(IATT_(self__,12),1) + 1);
   ntbl__ = (ptr)new1_(224,ns__,0);
   IATT_(self__,4) = (int)0;
   i__ = (int)0;
   while (1) {
      if ((i__ == (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4530_;
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
               goto goto_tag_4531_;
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
      goto_tag_4531_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4530_: ;
   ARR224_clear_(PATT_(self__,8));
   PATT_(self__,8) = (ptr)ntbl__;

   ret0__:
   return;
}

GEN174_insert_(self__,e__)
ptr self__;
ptr e__;
{
   int    hsh__ = S_int_VOID_;

   if ((INT15_lshift_((IATT_(self__,4) + 1),1) > IATT_(PATT_(self__,8),4))) {
      GEN174_double_size_(self__);
   }
   else {
   }
   hsh__ = (int)INT15_bit_and_(IATT_(e__,8),IATT_(self__,12));
   while (1) {
      if ((PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) == 0)) {
         PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
         IATT_(self__,4) = (int)(IATT_(self__,4) + 1);
         goto goto_tag_4532_;
      }
      else {
         if (STR173_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
            goto goto_tag_4532_;
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
goto_tag_4532_: ;

   ret0__:
   return;
}

GEN174_delete_(self__,e__)
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
         if (STR173_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            hole__ = (int)hsh__;
            PATT_(PATT_(self__,8), 8 + ((hole__) << 2)) = (ptr)0;
            IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
            goto goto_tag_4533_;
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
goto_tag_4533_: ;
   index__ = (int)hole__;
   while (1) {
      if ((index__ == (IATT_(PATT_(self__,8),4) - 2))) {
         index__ = (int)0;
      }
      else {
         index__ = (int)(index__ + 1);
      }
      if ((PATT_(PATT_(self__,8), 8 + ((index__) << 2)) == 0)) {
         goto goto_tag_4534_;
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
goto_tag_4534_: ;

   ret0__:
   return;
}

GEN174_clear_(self__)
ptr self__;
{

   IATT_(self__,4) = (int)0;
   ARR224_clear_(PATT_(self__,8));

   ret0__:
   return;
}

ptr GEN174_cursor_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)GEN189_create_(0,self__);

   ret0__:
   return (res__);
}

char GEN174_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

ptr GEN174_union_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN174_create_(self__);
   c__ = (ptr)GEN174_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4535_;
      }
      else {
      }
      GEN174_insert_(res__,GEN189_item_(c__));
      (void)GEN189_next_(c__);
   }
goto_tag_4535_: ;
   c__ = (ptr)GEN174_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4536_;
      }
      else {
      }
      GEN174_insert_(res__,GEN189_item_(c__));
      (void)GEN189_next_(c__);
   }
goto_tag_4536_: ;

   ret0__:
   return (res__);
}

ptr GEN174_intersection_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN174_create_(self__);
   c__ = (ptr)GEN174_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4537_;
      }
      else {
      }
      if ((GEN174_get_(s__,GEN189_item_(c__)) != 0)) {
         GEN174_insert_(res__,GEN189_item_(c__));
      }
      else {
      }
      (void)GEN189_next_(c__);
   }
goto_tag_4537_: ;

   ret0__:
   return (res__);
}

ptr GEN174_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN174_create_(self__);
   c__ = (ptr)GEN174_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4538_;
      }
      else {
      }
      if ((GEN174_get_(s__,GEN189_item_(c__)) == 0)) {
         GEN174_insert_(res__,GEN189_item_(c__));
      }
      else {
      }
      (void)GEN189_next_(c__);
   }
goto_tag_4538_: ;

   ret0__:
   return (res__);
}

ptr GEN174_sym_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN174_create_(self__);
   c__ = (ptr)GEN174_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4539_;
      }
      else {
      }
      if ((GEN174_get_(s__,GEN189_item_(c__)) == 0)) {
         GEN174_insert_(res__,GEN189_item_(c__));
      }
      else {
      }
      (void)GEN189_next_(c__);
   }
goto_tag_4539_: ;
   c__ = (ptr)GEN174_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4540_;
      }
      else {
      }
      if ((GEN174_get_(self__,GEN189_item_(c__)) == 0)) {
         GEN174_insert_(res__,GEN189_item_(c__));
      }
      else {
      }
      (void)GEN189_next_(c__);
   }
goto_tag_4540_: ;

   ret0__:
   return (res__);
}

char GEN174_is_a_subset_of_(self__,s__)
ptr self__;
ptr s__;
{
   char res__ = S_char_VOID_;
   ptr    c__ = 0;

   c__ = (ptr)GEN174_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4541_;
      }
      else {
      }
      if ((GEN174_get_(s__,GEN189_item_(c__)) == 0)) {
         goto ret0__;
      }
      else {
      }
      (void)GEN189_next_(c__);
   }
goto_tag_4541_: ;
   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr GEN174_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


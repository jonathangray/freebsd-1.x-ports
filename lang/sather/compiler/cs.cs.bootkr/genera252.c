/* genera252.c : Sather class: GENERAL_HASH{STRINT_HASH_ELT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_bit_and_();
extern ptr GEN255_create_();
extern ptr GEN255_item_();
extern ptr GEN255_next_();
extern char STR251_is_equal_();
extern ARR254_clear_();
extern int INT15_lshift_();
#include "macros_.h"



ptr GEN252_create_();
ptr GEN252_get_();
char GEN252_test_();
GEN252_double_size_();
GEN252_insert_();
GEN252_delete_();
GEN252_clear_();
ptr GEN252_cursor_();
char GEN252_is_empty_();
ptr GEN252_union_();
ptr GEN252_intersection_();
ptr GEN252_difference_();
ptr GEN252_sym_difference_();
char GEN252_is_a_subset_of_();
ptr GEN252_initialize_();
extern int attr_ent_GEN252[];

ptr GEN252_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(252,0);
   PATT_(res__,8) = (ptr)new1_(254,9,0);
   IATT_(res__,12) = (int)7;

   ret0__:
   return (res__);
}

ptr GEN252_get_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    hsh__ = S_int_VOID_;
   ptr    te__ = 0;

   hsh__ = (int)INT15_bit_and_(IATT_(e__,12),IATT_(self__,12));
   while (1) {
      while (1) {
         te__ = (ptr)PATT_(PATT_(self__,8), 8 + ((hsh__) << 2));
         if ((te__ == 0)) {
            goto goto_tag_4863_;
         }
         else {
            if (STR251_is_equal_(te__,e__)) {
               res__ = (ptr)te__;
               goto ret0__;
            }
            else {
            }
         }
         hsh__ = (int)(hsh__ + 1);
      }
   goto_tag_4863_: ;
      if ((hsh__ != (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4864_;
      }
      else {
         hsh__ = (int)0;
      }
   }
goto_tag_4864_: ;

   ret0__:
   return (res__);
}

char GEN252_test_(self__,e__)
ptr self__;
ptr e__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(GEN252_get_(self__,e__) != 0);

   ret0__:
   return (res__);
}

GEN252_double_size_(self__)
ptr self__;
{
   int    ns__ = S_int_VOID_;
   ptr    ntbl__ = 0;
   int    i__ = S_int_VOID_;
   ptr    val__ = 0;
   int    hsh__ = S_int_VOID_;

   ns__ = (int)(((IATT_(PATT_(self__,8),4) - 1) * 2) + 1);
   IATT_(self__,12) = (int)(INT15_lshift_(IATT_(self__,12),1) + 1);
   ntbl__ = (ptr)new1_(254,ns__,0);
   IATT_(self__,4) = (int)0;
   i__ = (int)0;
   while (1) {
      if ((i__ == (IATT_(PATT_(self__,8),4) - 1))) {
         goto goto_tag_4865_;
      }
      else {
      }
      val__ = (ptr)PATT_(PATT_(self__,8), 8 + ((i__) << 2));
      if ((val__ != 0)) {
         hsh__ = (int)INT15_bit_and_(IATT_(val__,12),IATT_(self__,12));
         while (1) {
            if ((PATT_(ntbl__, 8 + ((hsh__) << 2)) == 0)) {
               PATT_(ntbl__, 8 + ((hsh__) << 2)) = (ptr)val__;
               IATT_(self__,4) = (int)(IATT_(self__,4) + 1);
               goto goto_tag_4866_;
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
      goto_tag_4866_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4865_: ;
   ARR254_clear_(PATT_(self__,8));
   PATT_(self__,8) = (ptr)ntbl__;

   ret0__:
   return;
}

GEN252_insert_(self__,e__)
ptr self__;
ptr e__;
{
   int    hsh__ = S_int_VOID_;

   if ((INT15_lshift_((IATT_(self__,4) + 1),1) > IATT_(PATT_(self__,8),4))) {
      GEN252_double_size_(self__);
   }
   else {
   }
   hsh__ = (int)INT15_bit_and_(IATT_(e__,12),IATT_(self__,12));
   while (1) {
      if ((PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) == 0)) {
         PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
         IATT_(self__,4) = (int)(IATT_(self__,4) + 1);
         goto goto_tag_4867_;
      }
      else {
         if (STR251_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) = (ptr)e__;
            goto goto_tag_4867_;
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
goto_tag_4867_: ;

   ret0__:
   return;
}

GEN252_delete_(self__,e__)
ptr self__;
ptr e__;
{
   int    hsh__ = S_int_VOID_;
   int    hole__ = S_int_VOID_;
   int    index__ = S_int_VOID_;

   hsh__ = (int)INT15_bit_and_(IATT_(e__,12),IATT_(self__,12));
   hole__ = S_int_VOID_;
   while (1) {
      if ((PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if (STR251_is_equal_(PATT_(PATT_(self__,8), 8 + ((hsh__) << 2)),e__)) {
            hole__ = (int)hsh__;
            PATT_(PATT_(self__,8), 8 + ((hole__) << 2)) = (ptr)0;
            IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
            goto goto_tag_4868_;
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
goto_tag_4868_: ;
   index__ = (int)hole__;
   while (1) {
      if ((index__ == (IATT_(PATT_(self__,8),4) - 2))) {
         index__ = (int)0;
      }
      else {
         index__ = (int)(index__ + 1);
      }
      if ((PATT_(PATT_(self__,8), 8 + ((index__) << 2)) == 0)) {
         goto goto_tag_4869_;
      }
      else {
      }
      hsh__ = (int)INT15_bit_and_(IATT_(PATT_(PATT_(self__,8), 8 + ((index__) << 2)),12),IATT_(self__,12));
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
goto_tag_4869_: ;

   ret0__:
   return;
}

GEN252_clear_(self__)
ptr self__;
{

   IATT_(self__,4) = (int)0;
   ARR254_clear_(PATT_(self__,8));

   ret0__:
   return;
}

ptr GEN252_cursor_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)GEN255_create_(0,self__);

   ret0__:
   return (res__);
}

char GEN252_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

ptr GEN252_union_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN252_create_(self__);
   c__ = (ptr)GEN252_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4870_;
      }
      else {
      }
      GEN252_insert_(res__,GEN255_item_(c__));
      (void)GEN255_next_(c__);
   }
goto_tag_4870_: ;
   c__ = (ptr)GEN252_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4871_;
      }
      else {
      }
      GEN252_insert_(res__,GEN255_item_(c__));
      (void)GEN255_next_(c__);
   }
goto_tag_4871_: ;

   ret0__:
   return (res__);
}

ptr GEN252_intersection_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN252_create_(self__);
   c__ = (ptr)GEN252_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4872_;
      }
      else {
      }
      if ((GEN252_get_(s__,GEN255_item_(c__)) != 0)) {
         GEN252_insert_(res__,GEN255_item_(c__));
      }
      else {
      }
      (void)GEN255_next_(c__);
   }
goto_tag_4872_: ;

   ret0__:
   return (res__);
}

ptr GEN252_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN252_create_(self__);
   c__ = (ptr)GEN252_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4873_;
      }
      else {
      }
      if ((GEN252_get_(s__,GEN255_item_(c__)) == 0)) {
         GEN252_insert_(res__,GEN255_item_(c__));
      }
      else {
      }
      (void)GEN255_next_(c__);
   }
goto_tag_4873_: ;

   ret0__:
   return (res__);
}

ptr GEN252_sym_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)GEN252_create_(self__);
   c__ = (ptr)GEN252_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4874_;
      }
      else {
      }
      if ((GEN252_get_(s__,GEN255_item_(c__)) == 0)) {
         GEN252_insert_(res__,GEN255_item_(c__));
      }
      else {
      }
      (void)GEN255_next_(c__);
   }
goto_tag_4874_: ;
   c__ = (ptr)GEN252_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4875_;
      }
      else {
      }
      if ((GEN252_get_(self__,GEN255_item_(c__)) == 0)) {
         GEN252_insert_(res__,GEN255_item_(c__));
      }
      else {
      }
      (void)GEN255_next_(c__);
   }
goto_tag_4875_: ;

   ret0__:
   return (res__);
}

char GEN252_is_a_subset_of_(self__,s__)
ptr self__;
ptr s__;
{
   char res__ = S_char_VOID_;
   ptr    c__ = 0;

   c__ = (ptr)GEN252_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4876_;
      }
      else {
      }
      if ((GEN252_get_(s__,GEN255_item_(c__)) == 0)) {
         goto ret0__;
      }
      else {
      }
      (void)GEN255_next_(c__);
   }
goto_tag_4876_: ;
   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr GEN252_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* str_se218.c : Sather class: STR_SET, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int CHA14_to_i_();
extern int INT15_bit_xor_();
extern int INT15_bit_and_();
extern int INT15_rshift_();
extern int INT15_lshift_();
extern char STR20_is_equal_();
extern ptr STR245_create_();
extern ptr STR245_item_();
extern STR245_next_();
extern struct { int tp_; int sz_; char st_; } gs2878_;
#include "macros_.h"



STR218_clear_();
/*shared*/ ptr STR218_del_key_;
ptr STR218_create_();
int STR218_hash_();
char STR218_get_();
ptr STR218_double_size_();
ptr STR218_insert_();
STR218_delete_();
int STR218_size_();
ptr STR218_cursor_();
ptr STR218_union_();
ptr STR218_intersection_();
ptr STR218_difference_();
ptr STR218_sym_difference_();
ptr STR218_initialize_();
extern int attr_ent_STR218[];

STR218_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)0;
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_4803_;
      }
      else {
      }
      PATT_(self__, 16 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4803_: ;

   ret0__:
   return;
}

ptr STR218_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new1_(218,9,0);
   IATT_(res__,8) = (int)7;

   ret0__:
   return (res__);
}

int STR218_hash_(self__,s__)
ptr self__;
ptr s__;
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((CATT_(s__, 8 + ((i__))) == '\00')) {
         goto goto_tag_4804_;
      }
      else {
      }
      res__ = (int)INT15_bit_xor_(res__,INT15_lshift_(CHA14_to_i_(CATT_(s__, 8 + ((i__)))),INT15_bit_and_(i__,15)));
      i__ = (int)(i__ + 1);
   }
goto_tag_4804_: ;

   ret0__:
   return (res__);
}

char STR218_get_(self__,o__)
ptr self__;
ptr o__;
{
   char res__ = S_char_VOID_;
   int    hsh__ = S_int_VOID_;
   ptr    kt__ = 0;

   hsh__ = (int)INT15_bit_and_(INT15_rshift_(STR218_hash_(self__,o__),2),IATT_(self__,8));
   while (1) {
      while (1) {
         kt__ = (ptr)PATT_(self__, 16 + ((hsh__) << 2));
         if ((kt__ == o__)) {
            res__ = (char)1;
            goto ret0__;
         }
         else {
            if ((kt__ == 0)) {
               goto goto_tag_4805_;
            }
            else {
               if (STR20_is_equal_(kt__,o__)) {
                  res__ = (char)1;
                  goto ret0__;
               }
               else {
               }
            }
         }
         hsh__ = (int)(hsh__ + 1);
      }
   goto_tag_4805_: ;
      if ((hsh__ != (IATT_(self__,12) - 1))) {
         goto ret0__;
      }
      else {
         hsh__ = (int)0;
      }
   }

   ret0__:
   return (res__);
}

ptr STR218_double_size_(self__)
ptr self__;
{
   ptr res__ = 0;
   int    ns__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    val__ = 0;
   int    hsh__ = S_int_VOID_;

   ns__ = (int)(((IATT_(self__,12) - 1) * 2) + 1);
   res__ = (ptr)new1_(218,ns__,0);
   IATT_(res__,8) = (int)(INT15_lshift_(IATT_(self__,8),1) + 1);
   i__ = (int)0;
   while (1) {
      if ((i__ == (IATT_(self__,12) - 1))) {
         goto goto_tag_4806_;
      }
      else {
      }
      val__ = (ptr)PATT_(self__, 16 + ((i__) << 2));
      if (((val__ != 0) & (val__ != STR218_del_key_))) {
         hsh__ = (int)INT15_bit_and_(INT15_rshift_(STR218_hash_(self__,val__),2),IATT_(res__,8));
         while (1) {
            if ((PATT_(res__, 16 + ((hsh__) << 2)) == 0)) {
               PATT_(res__, 16 + ((hsh__) << 2)) = (ptr)val__;
               IATT_(res__,4) = (int)(IATT_(res__,4) + 1);
               goto goto_tag_4807_;
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
      goto_tag_4807_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4806_: ;
   STR218_clear_(self__);

   ret0__:
   return (res__);
}

ptr STR218_insert_(self__,o__)
ptr self__;
ptr o__;
{
   ptr res__ = 0;
   int    hsh__ = S_int_VOID_;

   if ((INT15_lshift_((IATT_(self__,4) + 1),1) > IATT_(self__,12))) {
      res__ = (ptr)STR218_double_size_(self__);
   }
   else {
      res__ = (ptr)self__;
   }
   hsh__ = (int)INT15_bit_and_(INT15_rshift_(STR218_hash_(self__,o__),2),IATT_(res__,8));
   while (1) {
      if ((PATT_(res__, 16 + ((hsh__) << 2)) == 0)) {
         PATT_(res__, 16 + ((hsh__) << 2)) = (ptr)o__;
         IATT_(res__,4) = (int)(IATT_(res__,4) + 1);
         goto ret0__;
      }
      else {
         if (STR20_is_equal_(PATT_(res__, 16 + ((hsh__) << 2)),o__)) {
            goto ret0__;
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

   ret0__:
   return (res__);
}

STR218_delete_(self__,o__)
ptr self__;
ptr o__;
{
   int    hsh__ = S_int_VOID_;

   hsh__ = (int)INT15_bit_and_(INT15_rshift_(STR218_hash_(self__,o__),2),IATT_(self__,8));
   while (1) {
      if ((PATT_(self__, 16 + ((hsh__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if (STR20_is_equal_(PATT_(self__, 16 + ((hsh__) << 2)),o__)) {
            PATT_(self__, 16 + ((hsh__) << 2)) = (ptr)STR218_del_key_;
            goto ret0__;
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

   ret0__:
   return;
}

int STR218_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_4808_;
      }
      else {
      }
      if (((PATT_(self__, 16 + ((i__) << 2)) != 0) & (PATT_(self__, 16 + ((i__) << 2)) != STR218_del_key_))) {
         res__ = (int)(res__ + 1);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4808_: ;

   ret0__:
   return (res__);
}

ptr STR218_cursor_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR245_create_(0,self__);

   ret0__:
   return (res__);
}

ptr STR218_union_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   res__ = (ptr)self__;
   c__ = (ptr)STR218_cursor_(s__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4809_;
      }
      else {
      }
      res__ = (ptr)STR218_insert_(res__,STR245_item_(c__));
      STR245_next_(c__);
   }
goto_tag_4809_: ;

   ret0__:
   return (res__);
}

ptr STR218_intersection_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   c__ = (ptr)STR218_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4810_;
      }
      else {
      }
      if ((! STR218_get_(s__,STR245_item_(c__)))) {
         STR218_delete_(self__,STR245_item_(c__));
      }
      else {
      }
      STR245_next_(c__);
   }
goto_tag_4810_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR218_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    c__ = 0;

   c__ = (ptr)STR218_cursor_(self__);
   while (1) {
      if (CATT_(c__,4)) {
         goto goto_tag_4811_;
      }
      else {
      }
      if (STR218_get_(s__,STR245_item_(c__))) {
         STR218_delete_(self__,STR245_item_(c__));
      }
      else {
      }
      STR245_next_(c__);
   }
goto_tag_4811_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR218_sym_difference_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   ptr    tmp__ = 0;

   tmp__ = (ptr)copy_(s__,0);
   tmp__ = (ptr)STR218_difference_(tmp__,self__);
   res__ = (ptr)STR218_difference_(self__,s__);
   res__ = (ptr)STR218_union_(res__,tmp__);

   ret0__:
   return (res__);
}

ptr STR218_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


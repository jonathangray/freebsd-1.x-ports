/* classo93.c : Sather class: CLASSOB_S_TABLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern error_exit();

extern ERR96_compiler_error_msg_();
extern ptr ERR7_s_();
extern ptr ERR7_i_();
extern ptr ERR7_c_();
extern ptr OUT9_c_();
extern ptr OUT9_s_();
extern ptr OUT9_i_();
extern int INT15_u_mod_();
extern int LST147_hash_();
extern char LST147_is_equal_();
extern CLA148_update_ind_();
extern ptr CLA148_full_name_();
extern struct { int tp_; int sz_; char st_; } gs1_;
extern struct { int tp_; int sz_; char st_; } gs2_;
extern struct { int tp_; int sz_; char st_; } gs3_;
extern struct { int tp_; int sz_; char st_; } gs4_;
extern struct { int tp_; int sz_; char st_; } gs5_;
extern struct { int tp_; int sz_; char st_; } gs6_;
extern struct { int tp_; int sz_; char st_; } gs7_;
extern struct { int tp_; int sz_; char st_; } gs8_;
extern struct { int tp_; int sz_; char st_; } gs9_;
extern struct { int tp_; int sz_; char st_; } gs10_;
extern struct { int tp_; int sz_; char st_; } gs11_;
extern struct { int tp_; int sz_; char st_; } gs12_;
extern struct { int tp_; int sz_; char st_; } gs13_;
extern struct { int tp_; int sz_; char st_; } gs14_;
extern struct { int tp_; int sz_; char st_; } gs15_;
extern struct { int tp_; int sz_; char st_; } gs16_;
extern struct { int tp_; int sz_; char st_; } gs17_;
extern struct { int tp_; int sz_; char st_; } gs18_;
extern struct { int tp_; int sz_; char st_; } gs19_;
extern struct { int tp_; int sz_; char st_; } gs20_;
extern struct { int tp_; int sz_; char st_; } gs21_;
extern struct { int tp_; int sz_; char st_; } gs22_;
extern struct { int tp_; int sz_; char st_; } gs23_;
extern struct { int tp_; int sz_; char st_; } gs24_;
extern struct { int tp_; int sz_; char st_; } gs25_;
extern struct { int tp_; int sz_; char st_; } gs26_;
extern struct { int tp_; int sz_; char st_; } gs27_;
extern struct { int tp_; int sz_; char st_; } gs28_;
extern struct { int tp_; int sz_; char st_; } gs29_;
extern struct { int tp_; int sz_; char st_; } gs30_;
extern struct { int tp_; int sz_; char st_; } gs31_;
extern struct { int tp_; int sz_; char st_; } gs32_;
extern struct { int tp_; int sz_; char st_; } gs33_;
extern struct { int tp_; int sz_; char st_; } gs34_;
extern struct { int tp_; int sz_; char st_; } gs35_;
extern struct { int tp_; int sz_; char st_; } gs36_;
extern struct { int tp_; int sz_; char st_; } gs37_;
extern struct { int tp_; int sz_; char st_; } gs38_;
extern struct { int tp_; int sz_; char st_; } gs39_;
extern struct { int tp_; int sz_; char st_; } gs40_;
extern struct { int tp_; int sz_; char st_; } gs41_;
extern struct { int tp_; int sz_; char st_; } gs42_;
extern struct { int tp_; int sz_; char st_; } gs43_;
extern struct { int tp_; int sz_; char st_; } gs44_;
extern struct { int tp_; int sz_; char st_; } gs45_;
extern struct { int tp_; int sz_; char st_; } gs46_;
extern struct { int tp_; int sz_; char st_; } gs47_;
extern struct { int tp_; int sz_; char st_; } gs48_;
extern struct { int tp_; int sz_; char st_; } gs49_;
extern struct { int tp_; int sz_; char st_; } gs50_;
extern struct { int tp_; int sz_; char st_; } gs51_;
extern struct { int tp_; int sz_; char st_; } gs52_;
extern struct { int tp_; int sz_; char st_; } gs53_;
extern struct { int tp_; int sz_; char st_; } gs54_;
extern struct { int tp_; int sz_; char st_; } gs55_;
extern struct { int tp_; int sz_; char st_; } gs56_;
extern struct { int tp_; int sz_; char st_; } gs57_;
extern struct { int tp_; int sz_; char st_; } gs58_;
extern struct { int tp_; int sz_; char st_; } gs59_;
extern struct { int tp_; int sz_; char st_; } gs60_;
extern struct { int tp_; int sz_; char st_; } gs61_;
extern struct { int tp_; int sz_; char st_; } gs62_;
extern struct { int tp_; int sz_; char st_; } gs63_;
extern struct { int tp_; int sz_; char st_; } gs64_;
extern struct { int tp_; int sz_; char st_; } gs65_;
extern struct { int tp_; int sz_; char st_; } gs66_;
extern struct { int tp_; int sz_; char st_; } gs67_;
extern struct { int tp_; int sz_; char st_; } gs68_;
extern struct { int tp_; int sz_; char st_; } gs69_;
extern struct { int tp_; int sz_; char st_; } gs70_;
extern struct { int tp_; int sz_; char st_; } gs71_;
extern struct { int tp_; int sz_; char st_; } gs72_;
extern struct { int tp_; int sz_; char st_; } gs73_;
extern struct { int tp_; int sz_; char st_; } gs74_;
extern struct { int tp_; int sz_; char st_; } gs75_;
extern struct { int tp_; int sz_; char st_; } gs1165_;
#include "macros_.h"



/*constant*/ ptr CLA93_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr CLA93_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr CLA93_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr CLA93_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr CLA93_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr CLA93_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr CLA93_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr CLA93_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr CLA93_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr CLA93_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr CLA93_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr CLA93_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr CLA93_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr CLA93_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr CLA93_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr CLA93_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr CLA93_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr CLA93_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr CLA93_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr CLA93_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr CLA93_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr CLA93_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr CLA93_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr CLA93_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr CLA93_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr CLA93_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr CLA93_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr CLA93_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr CLA93_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr CLA93_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr CLA93_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr CLA93_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr CLA93_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr CLA93_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr CLA93_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr CLA93_while_kw_name_ = (ptr)(&gs74_);
ptr CLA93_initialize_();
/*constant*/ ptr CLA93_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr CLA93_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr CLA93_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr CLA93_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr CLA93_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr CLA93_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr CLA93_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr CLA93_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr CLA93_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr CLA93_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr CLA93_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr CLA93_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr CLA93_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr CLA93_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr CLA93_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr CLA93_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr CLA93_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr CLA93_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr CLA93_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr CLA93_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr CLA93_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr CLA93_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr CLA93_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr CLA93_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr CLA93_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr CLA93_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr CLA93_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr CLA93_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr CLA93_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr CLA93_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr CLA93_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr CLA93_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr CLA93_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr CLA93_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr CLA93_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr CLA93_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr CLA93_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr CLA93_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr CLA93_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr CLA93_out_classname_ = (ptr)(&gs58_);
/*constant*/ int CLA93_and_kw_ind_ = 1;
/*constant*/ int CLA93_assert_kw_ind_ = 2;
/*constant*/ int CLA93_break_kw_ind_ = 3;
/*constant*/ int CLA93_class_kw_ind_ = 4;
/*constant*/ int CLA93_constant_kw_ind_ = 5;
/*constant*/ int CLA93_debug_kw_ind_ = 6;
/*constant*/ int CLA93_else_kw_ind_ = 7;
/*constant*/ int CLA93_elsif_kw_ind_ = 8;
/*constant*/ int CLA93_end_kw_ind_ = 9;
/*constant*/ int CLA93_if_kw_ind_ = 10;
/*constant*/ int CLA93_inline_kw_ind_ = 11;
/*constant*/ int CLA93_is_kw_ind_ = 12;
/*constant*/ int CLA93_loop_kw_ind_ = 13;
/*constant*/ int CLA93_not_kw_ind_ = 14;
/*constant*/ int CLA93_or_kw_ind_ = 15;
/*constant*/ int CLA93_private_kw_ind_ = 16;
/*constant*/ int CLA93_return_kw_ind_ = 17;
/*constant*/ int CLA93_shared_kw_ind_ = 18;
/*constant*/ int CLA93_switch_kw_ind_ = 19;
/*constant*/ int CLA93_then_kw_ind_ = 20;
/*constant*/ int CLA93_until_kw_ind_ = 21;
/*constant*/ int CLA93_when_kw_ind_ = 22;
/*constant*/ int CLA93_asize_ind_ = 23;
/*constant*/ int CLA93_asize1_ind_ = 24;
/*constant*/ int CLA93_asize2_ind_ = 25;
/*constant*/ int CLA93_asize3_ind_ = 26;
/*constant*/ int CLA93_asize4_ind_ = 27;
/*constant*/ int CLA93_copy_ind_ = 28;
/*constant*/ int CLA93_deep_copy_ind_ = 29;
/*constant*/ int CLA93_extend_ind_ = 30;
/*constant*/ int CLA93_new_ind_ = 31;
/*constant*/ int CLA93_type_ind_ = 32;
/*constant*/ int CLA93_res_ind_ = 33;
/*constant*/ int CLA93_self_ind_ = 34;
/*constant*/ int CLA93_false_ind_ = 35;
/*constant*/ int CLA93_true_ind_ = 36;
/*constant*/ int CLA93_void_ind_ = 37;
/*constant*/ int CLA93_first_base_class_ind_ = 38;
/*constant*/ int CLA93_array_ind_ = 38;
/*constant*/ int CLA93_array2_ind_ = 39;
/*constant*/ int CLA93_array3_ind_ = 40;
/*constant*/ int CLA93_array4_ind_ = 41;
/*constant*/ int CLA93_bool_ind_ = 42;
/*constant*/ int CLA93_c_ind_ = 43;
/*constant*/ int CLA93_char_ind_ = 44;
/*constant*/ int CLA93_double_ind_ = 45;
/*constant*/ int CLA93_file_ind_ = 46;
/*constant*/ int CLA93_int_ind_ = 47;
/*constant*/ int CLA93_real_ind_ = 48;
/*constant*/ int CLA93_self_type_ind_ = 49;
/*constant*/ int CLA93_str_ind_ = 50;
/*constant*/ int CLA93_str_cursor_ind_ = 51;
/*constant*/ int CLA93_ob_ind_ = 52;
/*constant*/ int CLA93_sys_ind_ = 53;
/*constant*/ int CLA93_fob_ind_ = 54;
/*constant*/ int CLA93_undefine_ind_ = 55;
/*constant*/ int CLA93_err_ind_ = 56;
/*constant*/ int CLA93_in_ind_ = 57;
/*constant*/ int CLA93_out_ind_ = 58;
/*constant*/ int CLA93_sux_ind_ = 59;
/*constant*/ int CLA93_last_base_class_ind_ = 59;
/*constant*/ int CLA93_exception_ind_ = 60;
/*constant*/ int CLA93_init_ind_ = 61;
/*constant*/ int CLA93_alias_kw_ind_ = 62;
/*constant*/ int CLA93_undefine_kw_ind_ = 63;
/*constant*/ int CLA93_require_kw_ind_ = 64;
/*constant*/ int CLA93_ensure_kw_ind_ = 65;
/*constant*/ int CLA93_abstract_kw_ind_ = 66;
/*constant*/ int CLA93_invariant_kw_ind_ = 67;
/*constant*/ int CLA93_raise_kw_ind_ = 68;
/*constant*/ int CLA93_protect_kw_ind_ = 69;
/*constant*/ int CLA93_against_kw_ind_ = 70;
/*constant*/ int CLA93_typecase_kw_ind_ = 71;
/*constant*/ int CLA93_attr_kw_ind_ = 72;
/*constant*/ int CLA93_readonly_kw_ind_ = 73;
/*constant*/ int CLA93_while_kw_ind_ = 74;
/*constant*/ int CLA93_include_kw_ind_ = 75;
/*constant*/ int CLA93_arg_ind_ = 76;
/*constant*/ int CLA93_last_reserved_word_ind_ = 76;
char CLA93_base_classname_p_();
/*constant*/ int CLA93_def_table_size_ = 50;
CLA93_error_msg_();
CLA93_error_exit_();
CLA93_expand_();
ptr CLA93_create_();
int CLA93_hash_();
ptr CLA93_get_obj_();
CLA93_add_unique_obj_();
CLA93_add_unique_obj_at_();
CLA93_double_tables_();
char CLA93_remove_obj_();
ptr CLA93_at_index_();
CLA93_print_table_();
extern int attr_ent_CLA93[];

ptr CLA93_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char CLA93_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

CLA93_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

CLA93_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

CLA93_expand_(self__,size__)
ptr self__;
int size__;
{

   while (1) {
      if ((IATT_(self__,16) > size__)) {
         goto goto_tag_1023_;
      }
      else {
      }
      CLA93_double_tables_(self__);
   }
goto_tag_1023_: ;
   IATT_(self__,24) = (int)size__;

   ret0__:
   return;
}

ptr CLA93_create_(self__,size__)
ptr self__;
int size__;
{
   ptr res__ = 0;
   ptr gl1024_;
   ptr gl1025_;
   ptr gl1026_;

   res__ = (ptr)new_(93,0);
   if ((size__ <= 0)) {
      size__ = (int)50;
   }
   else {
   }
   IATT_(res__,16) = (int)size__;
   IATT_(res__,20) = (int)(size__ * 2);
   gl1024_ = PATT_(res__,4);
   PATT_(res__,4) = (ptr)new1_(184,(2 * size__),0);
   gl1025_ = PATT_(res__,8);
   PATT_(res__,8) = (ptr)new1_(163,(2 * size__),1);
   gl1026_ = PATT_(res__,12);
   PATT_(res__,12) = (ptr)new1_(185,size__,0);
   IATT_(res__,24) = (int)0;

   ret0__:
   return (res__);
}

int CLA93_hash_(self__,k__)
ptr self__;
ptr k__;
{
   int res__ = S_int_VOID_;

   res__ = (int)LST147_hash_(k__,IATT_(self__,20));

   ret0__:
   return (res__);
}

ptr CLA93_get_obj_(self__,k__)
ptr self__;
ptr k__;
{
   ptr res__ = 0;
   int    insert__ = S_int_VOID_;

   insert__ = (int)CLA93_hash_(self__,k__);
   while (1) {
      if ((PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if (LST147_is_equal_(PATT_(PATT_(self__,4), 8 + ((insert__) << 2)),k__)) {
            res__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2));
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return (res__);
}

CLA93_add_unique_obj_(self__,k__,inst__)
ptr self__;
ptr k__;
ptr inst__;
{
   int    insert__ = S_int_VOID_;
   ptr    tmp_inst__ = 0;

   if ((IATT_(self__,24) == IATT_(self__,16))) {
      CLA93_double_tables_(self__);
   }
   else {
   }
   insert__ = (int)CLA93_hash_(self__,k__);
   while (1) {
      if ((PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         tmp_inst__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         while (1) {
            if ((tmp_inst__ == 0)) {
               goto goto_tag_1027_;
            }
            else {
            }
            IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
            if ((IATT_(self__,24) == IATT_(self__,16))) {
               CLA93_double_tables_(self__);
               CLA93_add_unique_obj_(self__,k__,inst__);
               goto ret0__;
            }
            else {
            }
            tmp_inst__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         }
      goto_tag_1027_: ;
         PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)inst__;
         CLA148_update_ind_(inst__,IATT_(self__,24));
         PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (ptr)k__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
         IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
         goto ret0__;
      }
      else {
         if (LST147_is_equal_(PATT_(PATT_(self__,4), 8 + ((insert__) << 2)),k__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)inst__;
            CLA148_update_ind_(inst__,IATT_(PATT_(self__,8), 8 + ((insert__) << 2)));
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return;
}

CLA93_add_unique_obj_at_(self__,k__,inst__,loc__)
ptr self__;
ptr k__;
ptr inst__;
int loc__;
{
   SATHER_STR_(20,34,ls736_,"ERROR: Collision in class index (");
   SATHER_STR_(20,11,ls737_,") between ");
   SATHER_STR_(20,6,ls738_," and ");
   SATHER_STR_(20,16,ls725_,"CLASSOB_S_TABLE");
   SATHER_STR_(20,79,ls739_,"Compiler error (code 3) in incremental compilation information; Please report\n");
   int    insert__ = S_int_VOID_;
   ptr    tmp_inst__ = 0;

   if ((IATT_(self__,24) == IATT_(self__,16))) {
      CLA93_double_tables_(self__);
   }
   else {
   }
   while (1) {
      if ((loc__ < IATT_(self__,16))) {
         goto goto_tag_1028_;
      }
      else {
      }
      CLA93_double_tables_(self__);
   }
goto_tag_1028_: ;
   insert__ = (int)CLA93_hash_(self__,k__);
   while (1) {
      if ((PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         tmp_inst__ = (ptr)PATT_(PATT_(self__,12), 8 + ((loc__) << 2));
         if ((tmp_inst__ != 0)) {
            (void)ERR7_c_(ERR7_s_(ERR7_s_(ERR7_s_(ERR7_s_(ERR7_i_(ERR7_s_(0,(ptr)(&ls736_)),loc__),(ptr)(&ls737_)),CLA148_full_name_(tmp_inst__)),(ptr)(&ls738_)),CLA148_full_name_(inst__)),'\n');
            ERR96_compiler_error_msg_(0,(ptr)(&ls725_),(ptr)(&ls739_));
            (void)CLA93_remove_obj_(self__,PATT_(tmp_inst__,44));
         }
         else {
         }
         PATT_(PATT_(self__,12), 8 + ((loc__) << 2)) = (ptr)inst__;
         CLA148_update_ind_(inst__,loc__);
         PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (ptr)k__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)loc__;
         if ((tmp_inst__ != 0)) {
            CLA93_add_unique_obj_(self__,PATT_(tmp_inst__,44),tmp_inst__);
         }
         else {
         }
         goto ret0__;
      }
      else {
         if (LST147_is_equal_(PATT_(PATT_(self__,4), 8 + ((insert__) << 2)),k__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)inst__;
            CLA148_update_ind_(inst__,IATT_(PATT_(self__,8), 8 + ((insert__) << 2)));
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return;
}

CLA93_double_tables_(self__)
ptr self__;
{
   ptr gl1029_;
   ptr gl1030_;
   int    old_keys_size__ = S_int_VOID_;
   ptr    nkeys__ = 0;
   ptr    nindices__ = 0;
   int    i__ = S_int_VOID_;
   ptr    k__ = 0;
   int    insert__ = S_int_VOID_;

   old_keys_size__ = (int)IATT_(self__,20);
   IATT_(self__,16) = (int)(2 * IATT_(self__,16));
   IATT_(self__,20) = (int)(2 * IATT_(self__,20));
   gl1029_ = PATT_(self__,4);
   nkeys__ = (ptr)new1_(184,IATT_(self__,20),0);
   gl1030_ = PATT_(self__,8);
   nindices__ = (ptr)new1_(163,IATT_(self__,20),1);
   PATT_(self__,12) = (ptr)extend1_(PATT_(self__,12),IATT_(self__,16),0);
   i__ = (int)0;
   while (1) {
      if ((i__ >= old_keys_size__)) {
         goto goto_tag_1031_;
      }
      else {
      }
      if ((PATT_(PATT_(self__,4), 8 + ((i__) << 2)) != 0)) {
         k__ = (ptr)PATT_(PATT_(self__,4), 8 + ((i__) << 2));
         insert__ = (int)CLA93_hash_(self__,k__);
         while (1) {
            if ((PATT_(nkeys__, 8 + ((insert__) << 2)) == 0)) {
               PATT_(nkeys__, 8 + ((insert__) << 2)) = (ptr)k__;
               IATT_(nindices__, 8 + ((insert__) << 2)) = (int)IATT_(PATT_(self__,8), 8 + ((i__) << 2));
               goto goto_tag_1032_;
            }
            else {
               insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
            }
         }
      goto_tag_1032_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1031_: ;
   PATT_(self__,4) = (ptr)nkeys__;
   PATT_(self__,8) = (ptr)nindices__;

   ret0__:
   return;
}

char CLA93_remove_obj_(self__,k__)
ptr self__;
ptr k__;
{
   char res__ = S_char_VOID_;
   int    insert__ = S_int_VOID_;
   ptr    inst__ = 0;

   insert__ = (int)CLA93_hash_(self__,k__);
   while (1) {
      if ((PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if (LST147_is_equal_(PATT_(PATT_(self__,4), 8 + ((insert__) << 2)),k__)) {
            inst__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2));
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)0;
            CLA148_update_ind_(inst__,0);
            PATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (ptr)0;
            IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)0;
            res__ = (char)1;
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return (res__);
}

ptr CLA93_at_index_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;
   SATHER_STR_(20,40,ls742_,"(CLASSOB_S_TABLE) : Index out of range\n");

   if (((i__ < 0) | (i__ >= IATT_(self__,16)))) {
      CLA93_error_msg_(self__,(ptr)(&ls742_));
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)PATT_(PATT_(self__,12), 8 + ((i__) << 2));

   ret0__:
   return (res__);
}

CLA93_print_table_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,20))) {
         goto goto_tag_1033_;
      }
      else {
      }
      if ((PATT_(PATT_(self__,4), 8 + ((i__) << 2)) != 0)) {
         (void)OUT9_c_(OUT9_c_(OUT9_i_(OUT9_c_(OUT9_s_(OUT9_c_(0,'<'),CLA148_full_name_(PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((i__) << 2))) << 2)))),','),IATT_(PATT_(self__,8), 8 + ((i__) << 2))),'>'),'\n');
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1033_: ;

   ret0__:
   return;
}


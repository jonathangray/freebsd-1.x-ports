/* classo66.c : Sather class: CLASSOB_TABLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_u_mod_();
extern char STR20_is_upper_case_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_to_upper_case_();
extern ptr STR20_i_();
extern ptr STR20_c_();
extern /*shared*/ char LST43_duplicate_defs_ok_;
extern ptr CLA67_name_str_();
extern CLA67_change_name_();
extern int CLA67_unique_key_();
extern ptr CLA67_features_();
extern /*constant*/ int RES71_c_ind_;
extern CLA67_add_features_();
extern /*shared*/ ptr GLO68_str_table_;
extern int STR69_find_str_();
extern ptr OUT80_s_();
extern /*shared*/ char COM82_warnings_only_;
extern ptr LIS87_push_();
extern ERR96_format_error_msg_file_();
extern /*constant*/ int RES97_ARRAY_ici_;
extern /*constant*/ int RES97_ARRAY2_ici_;
extern /*constant*/ int RES97_ARRAY3_ici_;
extern /*constant*/ int RES97_ARRAY4_ici_;
extern /*constant*/ int RES97_BOOL_ici_;
extern /*constant*/ int RES97_C_ici_;
extern /*constant*/ int RES97_CHAR_ici_;
extern /*constant*/ int RES97_DOUBLE_ici_;
extern /*constant*/ int RES97_ERR_ici_;
extern /*constant*/ int RES97_FILE_ici_;
extern /*constant*/ int RES97_IN_ici_;
extern /*constant*/ int RES97_INT_ici_;
extern /*constant*/ int RES97_OUT_ici_;
extern /*constant*/ int RES97_REAL_ici_;
extern /*constant*/ int RES97_SELF_TYPE_ici_;
extern /*constant*/ int RES97_STR_ici_;
extern /*constant*/ int RES97_STR_CURSOR_ici_;
extern /*constant*/ int RES97_OB_ici_;
extern /*constant*/ int RES97_SYS_ici_;
extern /*constant*/ int RES97_FOB_ici_;
extern /*constant*/ int RES97_SUX_ici_;
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern /*constant*/ int RES97_LAST_PREDEF_ici_;
extern ERR96_format_warning_msg_file_();
extern ERR96_compiler_error_msg_();
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



/*constant*/ int CLA66_def_table_size_ = 50;
ptr CLA66_create_();
int CLA66_hash_();
ptr CLA66_get_obj_();
char CLA66_add_obj_();
CLA66_add_unique_obj_();
CLA66_double_tables_();
char CLA66_remove_obj_();
ptr CLA66_initialize_();
/*constant*/ ptr CLA66_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr CLA66_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr CLA66_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr CLA66_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr CLA66_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr CLA66_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr CLA66_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr CLA66_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr CLA66_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr CLA66_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr CLA66_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr CLA66_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr CLA66_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr CLA66_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr CLA66_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr CLA66_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr CLA66_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr CLA66_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr CLA66_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr CLA66_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr CLA66_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr CLA66_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr CLA66_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr CLA66_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr CLA66_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr CLA66_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr CLA66_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr CLA66_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr CLA66_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr CLA66_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr CLA66_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr CLA66_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr CLA66_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr CLA66_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr CLA66_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr CLA66_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr CLA66_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr CLA66_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr CLA66_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr CLA66_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr CLA66_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr CLA66_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr CLA66_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr CLA66_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr CLA66_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr CLA66_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr CLA66_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr CLA66_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr CLA66_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr CLA66_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr CLA66_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr CLA66_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr CLA66_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr CLA66_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr CLA66_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr CLA66_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr CLA66_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr CLA66_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr CLA66_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr CLA66_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr CLA66_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr CLA66_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr CLA66_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr CLA66_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr CLA66_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr CLA66_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr CLA66_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr CLA66_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr CLA66_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr CLA66_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr CLA66_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr CLA66_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr CLA66_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr CLA66_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr CLA66_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr CLA66_out_classname_ = (ptr)(&gs58_);
/*constant*/ int CLA66_and_kw_ind_ = 1;
/*constant*/ int CLA66_assert_kw_ind_ = 2;
/*constant*/ int CLA66_break_kw_ind_ = 3;
/*constant*/ int CLA66_class_kw_ind_ = 4;
/*constant*/ int CLA66_constant_kw_ind_ = 5;
/*constant*/ int CLA66_debug_kw_ind_ = 6;
/*constant*/ int CLA66_else_kw_ind_ = 7;
/*constant*/ int CLA66_elsif_kw_ind_ = 8;
/*constant*/ int CLA66_end_kw_ind_ = 9;
/*constant*/ int CLA66_if_kw_ind_ = 10;
/*constant*/ int CLA66_inline_kw_ind_ = 11;
/*constant*/ int CLA66_is_kw_ind_ = 12;
/*constant*/ int CLA66_loop_kw_ind_ = 13;
/*constant*/ int CLA66_not_kw_ind_ = 14;
/*constant*/ int CLA66_or_kw_ind_ = 15;
/*constant*/ int CLA66_private_kw_ind_ = 16;
/*constant*/ int CLA66_return_kw_ind_ = 17;
/*constant*/ int CLA66_shared_kw_ind_ = 18;
/*constant*/ int CLA66_switch_kw_ind_ = 19;
/*constant*/ int CLA66_then_kw_ind_ = 20;
/*constant*/ int CLA66_until_kw_ind_ = 21;
/*constant*/ int CLA66_when_kw_ind_ = 22;
/*constant*/ int CLA66_asize_ind_ = 23;
/*constant*/ int CLA66_asize1_ind_ = 24;
/*constant*/ int CLA66_asize2_ind_ = 25;
/*constant*/ int CLA66_asize3_ind_ = 26;
/*constant*/ int CLA66_asize4_ind_ = 27;
/*constant*/ int CLA66_copy_ind_ = 28;
/*constant*/ int CLA66_deep_copy_ind_ = 29;
/*constant*/ int CLA66_extend_ind_ = 30;
/*constant*/ int CLA66_new_ind_ = 31;
/*constant*/ int CLA66_type_ind_ = 32;
/*constant*/ int CLA66_res_ind_ = 33;
/*constant*/ int CLA66_self_ind_ = 34;
/*constant*/ int CLA66_false_ind_ = 35;
/*constant*/ int CLA66_true_ind_ = 36;
/*constant*/ int CLA66_void_ind_ = 37;
/*constant*/ int CLA66_first_base_class_ind_ = 38;
/*constant*/ int CLA66_array_ind_ = 38;
/*constant*/ int CLA66_array2_ind_ = 39;
/*constant*/ int CLA66_array3_ind_ = 40;
/*constant*/ int CLA66_array4_ind_ = 41;
/*constant*/ int CLA66_bool_ind_ = 42;
/*constant*/ int CLA66_c_ind_ = 43;
/*constant*/ int CLA66_char_ind_ = 44;
/*constant*/ int CLA66_double_ind_ = 45;
/*constant*/ int CLA66_file_ind_ = 46;
/*constant*/ int CLA66_int_ind_ = 47;
/*constant*/ int CLA66_real_ind_ = 48;
/*constant*/ int CLA66_self_type_ind_ = 49;
/*constant*/ int CLA66_str_ind_ = 50;
/*constant*/ int CLA66_str_cursor_ind_ = 51;
/*constant*/ int CLA66_ob_ind_ = 52;
/*constant*/ int CLA66_sys_ind_ = 53;
/*constant*/ int CLA66_fob_ind_ = 54;
/*constant*/ int CLA66_undefine_ind_ = 55;
/*constant*/ int CLA66_err_ind_ = 56;
/*constant*/ int CLA66_in_ind_ = 57;
/*constant*/ int CLA66_out_ind_ = 58;
/*constant*/ int CLA66_sux_ind_ = 59;
/*constant*/ int CLA66_last_base_class_ind_ = 59;
/*constant*/ int CLA66_exception_ind_ = 60;
/*constant*/ int CLA66_init_ind_ = 61;
/*constant*/ int CLA66_alias_kw_ind_ = 62;
/*constant*/ int CLA66_undefine_kw_ind_ = 63;
/*constant*/ int CLA66_require_kw_ind_ = 64;
/*constant*/ int CLA66_ensure_kw_ind_ = 65;
/*constant*/ int CLA66_abstract_kw_ind_ = 66;
/*constant*/ int CLA66_invariant_kw_ind_ = 67;
/*constant*/ int CLA66_raise_kw_ind_ = 68;
/*constant*/ int CLA66_protect_kw_ind_ = 69;
/*constant*/ int CLA66_against_kw_ind_ = 70;
/*constant*/ int CLA66_typecase_kw_ind_ = 71;
/*constant*/ int CLA66_attr_kw_ind_ = 72;
/*constant*/ int CLA66_readonly_kw_ind_ = 73;
/*constant*/ int CLA66_while_kw_ind_ = 74;
/*constant*/ int CLA66_include_kw_ind_ = 75;
/*constant*/ int CLA66_arg_ind_ = 76;
/*constant*/ int CLA66_last_reserved_word_ind_ = 76;
char CLA66_base_classname_p_();
CLA66_uppercase_name_p_();
CLA66_install_();
ptr CLA66_at_index_();
CLA66_print_table_();
CLA66_print_def_();
extern int attr_ent_CLA66[];

ptr CLA66_create_(self__,size__)
ptr self__;
int size__;
{
   ptr res__ = 0;
   ptr gl936_;
   ptr gl937_;
   ptr gl938_;

   res__ = (ptr)new_(66,0);
   if ((size__ <= 25)) {
      size__ = (int)50;
   }
   else {
   }
   IATT_(res__,16) = (int)size__;
   IATT_(res__,20) = (int)(size__ * 2);
   gl936_ = PATT_(res__,4);
   PATT_(res__,4) = (ptr)new1_(163,(2 * size__),1);
   gl937_ = PATT_(res__,8);
   PATT_(res__,8) = (ptr)new1_(163,(2 * size__),1);
   gl938_ = PATT_(res__,12);
   PATT_(res__,12) = (ptr)new1_(216,size__,0);
   IATT_(res__,24) = (int)25;

   ret0__:
   return (res__);
}

int CLA66_hash_(self__,nm__)
ptr self__;
int nm__;
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_u_mod_(nm__,IATT_(self__,20));

   ret0__:
   return (res__);
}

ptr CLA66_get_obj_(self__,nm__)
ptr self__;
int nm__;
{
   ptr res__ = 0;
   int    insert__ = S_int_VOID_;

   insert__ = (int)CLA66_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
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

char CLA66_add_obj_(self__,nm__,ob__)
ptr self__;
int nm__;
ptr ob__;
{
   char res__ = S_char_VOID_;
   int    insert__ = S_int_VOID_;

   if ((IATT_(self__,24) >= IATT_(self__,16))) {
      CLA66_double_tables_(self__);
   }
   else {
   }
   insert__ = (int)CLA66_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
         IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (int)nm__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
         IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
            IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
            IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
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

CLA66_add_unique_obj_(self__,nm__,ob__)
ptr self__;
int nm__;
ptr ob__;
{
   int    insert__ = S_int_VOID_;
   ptr    tmp_ob__ = 0;

   if ((IATT_(self__,24) >= IATT_(self__,16))) {
      CLA66_double_tables_(self__);
   }
   else {
   }
   insert__ = (int)CLA66_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         tmp_ob__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         while (1) {
            if ((tmp_ob__ == 0)) {
               goto goto_tag_939_;
            }
            else {
            }
            IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
            if ((IATT_(self__,24) == IATT_(self__,16))) {
               CLA66_double_tables_(self__);
               CLA66_add_unique_obj_(self__,nm__,ob__);
               goto ret0__;
            }
            else {
            }
            tmp_ob__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         }
      goto_tag_939_: ;
         PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
         IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (int)nm__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
         IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)ob__;
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

CLA66_double_tables_(self__)
ptr self__;
{
   ptr gl940_;
   ptr gl941_;
   int    old_names_size__ = S_int_VOID_;
   ptr    nnames__ = 0;
   ptr    nindices__ = 0;
   int    i__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   old_names_size__ = (int)IATT_(self__,20);
   IATT_(self__,16) = (int)(2 * IATT_(self__,16));
   IATT_(self__,20) = (int)(2 * IATT_(self__,20));
   gl940_ = PATT_(self__,4);
   nnames__ = (ptr)new1_(163,IATT_(self__,20),1);
   gl941_ = PATT_(self__,8);
   nindices__ = (ptr)new1_(163,IATT_(self__,20),1);
   PATT_(self__,12) = (ptr)extend1_(PATT_(self__,12),IATT_(self__,16),0);
   i__ = (int)0;
   while (1) {
      if ((i__ >= old_names_size__)) {
         goto goto_tag_942_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,4), 8 + ((i__) << 2)) > 0)) {
         nm__ = (int)IATT_(PATT_(self__,4), 8 + ((i__) << 2));
         insert__ = (int)CLA66_hash_(self__,nm__);
         while (1) {
            if ((IATT_(nnames__, 8 + ((insert__) << 2)) == 0)) {
               IATT_(nnames__, 8 + ((insert__) << 2)) = (int)nm__;
               IATT_(nindices__, 8 + ((insert__) << 2)) = (int)IATT_(PATT_(self__,8), 8 + ((i__) << 2));
               goto goto_tag_943_;
            }
            else {
               insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
            }
         }
      goto_tag_943_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_942_: ;
   PATT_(self__,4) = (ptr)nnames__;
   PATT_(self__,8) = (ptr)nindices__;

   ret0__:
   return;
}

char CLA66_remove_obj_(self__,nm__)
ptr self__;
int nm__;
{
   char res__ = S_char_VOID_;
   int    insert__ = S_int_VOID_;

   insert__ = (int)CLA66_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)0;
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

ptr CLA66_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char CLA66_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

CLA66_uppercase_name_p_(self__,def__)
ptr self__;
ptr def__;
{
   SATHER_STR_(20,14,ls745_,"CLASSOB_TABLE");
   SATHER_STR_(20,13,ls749_,"Class name \"");
   SATHER_STR_(20,27,ls750_,"\" converted to upper-case\n");
   ptr    nm__ = 0;
   ptr    new_nm__ = 0;

   nm__ = (ptr)CLA67_name_str_(def__);
   if ((! STR20_is_upper_case_(nm__))) {
      ERR96_format_error_msg_file_(0,PATT_(def__,16),IATT_(def__,8),(ptr)(&ls745_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls749_)),nm__),(ptr)(&ls750_)));
      new_nm__ = (ptr)STR20_to_upper_case_(copy_(nm__,1));
      CLA67_change_name_(def__,new_nm__);
   }
   else {
   }

   ret0__:
   return;
}

CLA66_install_(self__,new_def__)
ptr self__;
ptr new_def__;
{
   SATHER_STR_(20,14,ls745_,"CLASSOB_TABLE");
   SATHER_STR_(20,27,ls777_,"Parametrization of class \"");
   SATHER_STR_(20,29,ls778_,"\" differs from that in file ");
   SATHER_STR_(20,8,ls779_,", line ");
   SATHER_STR_(20,2,ls780_,"\n");
   SATHER_STR_(20,32,ls781_,"Number of parameters of class \"");
   SATHER_STR_(20,32,ls784_,"Extending definition of class \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,5,ls786_," in ");
   SATHER_STR_(20,11,ls787_,",\n        ");
   int    key__ = S_int_VOID_;
   ptr    prev_def__ = 0;
   int    insert__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    err_str__ = 0;
   int    i_56_ = S_int_VOID_;

   key__ = (int)CLA67_unique_key_(new_def__);
   CLA66_uppercase_name_p_(self__,new_def__);
   prev_def__ = (ptr)CLA66_get_obj_(self__,key__);
   if ((prev_def__ == 0)) {
      insert__ = (int)(IATT_(self__,24) + 1);
      switch (IATT_(new_def__,12)) {
         case (38) :
            IATT_(self__,24) = (int)RES97_ARRAY_ici_;
            break;
         case (39) :
            IATT_(self__,24) = (int)RES97_ARRAY2_ici_;
            break;
         case (40) :
            IATT_(self__,24) = (int)RES97_ARRAY3_ici_;
            break;
         case (41) :
            IATT_(self__,24) = (int)RES97_ARRAY4_ici_;
            break;
         case (42) :
            IATT_(self__,24) = (int)RES97_BOOL_ici_;
            break;
         case (43) :
            IATT_(self__,24) = (int)RES97_C_ici_;
            break;
         case (44) :
            IATT_(self__,24) = (int)RES97_CHAR_ici_;
            break;
         case (45) :
            IATT_(self__,24) = (int)RES97_DOUBLE_ici_;
            break;
         case (56) :
            IATT_(self__,24) = (int)RES97_ERR_ici_;
            break;
         case (46) :
            IATT_(self__,24) = (int)RES97_FILE_ici_;
            break;
         case (57) :
            IATT_(self__,24) = (int)RES97_IN_ici_;
            break;
         case (47) :
            IATT_(self__,24) = (int)RES97_INT_ici_;
            break;
         case (58) :
            IATT_(self__,24) = (int)RES97_OUT_ici_;
            break;
         case (48) :
            IATT_(self__,24) = (int)RES97_REAL_ici_;
            break;
         case (49) :
            IATT_(self__,24) = (int)RES97_SELF_TYPE_ici_;
            break;
         case (50) :
            IATT_(self__,24) = (int)RES97_STR_ici_;
            break;
         case (51) :
            IATT_(self__,24) = (int)RES97_STR_CURSOR_ici_;
            break;
         case (52) :
            IATT_(self__,24) = (int)RES97_OB_ici_;
            break;
         case (53) :
            IATT_(self__,24) = (int)RES97_SYS_ici_;
            break;
         case (54) :
            IATT_(self__,24) = (int)RES97_FOB_ici_;
            break;
         case (59) :
            IATT_(self__,24) = (int)RES97_SUX_ici_;
            break;
         case (55) :
            IATT_(self__,24) = (int)RES97_UNDEFINE_ici_;
            break;
         default:
            CLA66_add_unique_obj_(self__,key__,new_def__);
            goto ret0__;
            ;
      }
      CLA66_add_unique_obj_(self__,key__,new_def__);
      if ((insert__ <= RES97_LAST_PREDEF_ici_)) {
         IATT_(self__,24) = (int)(RES97_LAST_PREDEF_ici_ + 1);
      }
      else {
         IATT_(self__,24) = (int)insert__;
      }
   }
   else {
      if ((PATT_(prev_def__,24) == 0)) {
         if ((PATT_(new_def__,24) == 0)) {
         }
         else {
            ERR96_format_error_msg_file_(0,PATT_(new_def__,16),IATT_(new_def__,40),(ptr)(&ls745_),STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls777_)),CLA67_name_str_(prev_def__)),(ptr)(&ls778_)),PATT_(prev_def__,16)),(ptr)(&ls779_)),IATT_(prev_def__,40)),(ptr)(&ls780_)));
         }
      }
      else {
         if ((PATT_(new_def__,24) == 0)) {
            ERR96_format_error_msg_file_(0,PATT_(new_def__,16),IATT_(new_def__,40),(ptr)(&ls745_),STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls777_)),CLA67_name_str_(prev_def__)),(ptr)(&ls778_)),PATT_(prev_def__,16)),(ptr)(&ls779_)),IATT_(prev_def__,40)),(ptr)(&ls780_)));
         }
         else {
            psz__ = (int)IATT_(PATT_(new_def__,24),4);
            if ((psz__ != IATT_(PATT_(prev_def__,24),4))) {
               ERR96_format_error_msg_file_(0,PATT_(new_def__,16),IATT_(new_def__,40),(ptr)(&ls745_),STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls781_)),CLA67_name_str_(prev_def__)),(ptr)(&ls778_)),PATT_(prev_def__,16)),(ptr)(&ls779_)),IATT_(prev_def__,40)),(ptr)(&ls780_)));
            }
            else {
               i__ = (int)0;
               while (1) {
                  if ((i__ >= psz__)) {
                     goto goto_tag_944_;
                  }
                  else {
                  }
                  if ((IATT_(PATT_(prev_def__,24), 16 + ((i__) << 2)) != IATT_(PATT_(new_def__,24), 16 + ((i__) << 2)))) {
                     ERR96_format_error_msg_file_(0,PATT_(new_def__,16),IATT_(new_def__,40),(ptr)(&ls745_),STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls781_)),CLA67_name_str_(prev_def__)),(ptr)(&ls778_)),PATT_(prev_def__,16)),(ptr)(&ls779_)),IATT_(prev_def__,40)),(ptr)(&ls780_)));
                  }
                  else {
                  }
                  i__ = (int)(i__ + 1);
               }
            goto_tag_944_: ;
            }
         }
      }
      if ((IATT_(prev_def__,12) != 43)) {
         if ((! COM82_warnings_only_)) {
            err_str__ = (ptr)STR20_c_(STR20_s_(STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls784_)),CLA67_name_str_(prev_def__)),(ptr)(&ls785_)),(ptr)(&ls786_)),'"'),PATT_(prev_def__,16)),'"');
            i_56_ = (int)0;
            if ((IATT_(PATT_(prev_def__,32),4) > 0)) {
               while (1) {
                  if ((i_56_ == IATT_(PATT_(prev_def__,32),4))) {
                     goto goto_tag_945_;
                  }
                  else {
                  }
                  err_str__ = (ptr)STR20_s_(STR20_s_(STR20_c_(STR20_s_(err_str__,(ptr)(&ls787_)),'"'),PATT_(PATT_(prev_def__,32), 16 + ((i_56_) << 2))),(ptr)(&ls785_));
                  i_56_ = (int)(i_56_ + 1);
               }
            goto_tag_945_: ;
            }
            else {
            }
            ERR96_format_warning_msg_file_(0,PATT_(new_def__,16),IATT_(new_def__,40),(ptr)(&ls745_),err_str__);
         }
         else {
         }
         LST43_duplicate_defs_ok_ = (char)0;
      }
      else {
      }
      PATT_(prev_def__,32) = (ptr)LIS87_push_(PATT_(prev_def__,32),PATT_(new_def__,16));
      CLA67_add_features_(prev_def__,CLA67_features_(new_def__));
      if ((IATT_(prev_def__,12) != 43)) {
         LST43_duplicate_defs_ok_ = (char)1;
      }
      else {
      }
   }

   ret0__:
   return;
}

ptr CLA66_at_index_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;
   SATHER_STR_(20,14,ls745_,"CLASSOB_TABLE");
   SATHER_STR_(20,15,ls790_,"Invalid index ");

   if (((i__ < 0) | (i__ >= IATT_(self__,16)))) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls745_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls790_)),i__));
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)PATT_(PATT_(self__,12), 8 + ((i__) << 2));

   ret0__:
   return (res__);
}

CLA66_print_table_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,20))) {
         goto goto_tag_946_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,4), 8 + ((i__) << 2)) > 0)) {
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_946_: ;

   ret0__:
   return;
}

CLA66_print_def_(self__,outfile__,cnm__)
ptr self__;
ptr outfile__;
ptr cnm__;
{
   SATHER_STR_(20,14,ls794_," not found!!\n");
   int    index__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   index__ = (int)STR69_find_str_(GLO68_str_table_,cnm__);
   if ((index__ > 0)) {
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(self__,20))) {
            goto goto_tag_947_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,4), 8 + ((i__) << 2)) == index__)) {
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_947_: ;
   }
   else {
      (void)OUT80_s_(OUT80_s_(outfile__,cnm__),(ptr)(&ls794_));
   }

   ret0__:
   return;
}


/* classo67.c : Sather class: CLASSOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_();
extern CS_160_repeated_type_parameters_();
extern /*shared*/ int GLO68_curr_lineno_;
extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_curr_file_stat_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern int STR69_insert_str_();
extern int C_F74_st_mtime_();
extern ptr LST43_add_unique_feat_();
extern ptr STR20_s_();
extern ptr LIS87_create_();
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



/*constant*/ int CLA67_print_indent_ = 2;
/*constant*/ ptr CLA67_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr CLA67_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr CLA67_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr CLA67_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr CLA67_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr CLA67_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr CLA67_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr CLA67_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr CLA67_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr CLA67_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr CLA67_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr CLA67_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr CLA67_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr CLA67_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr CLA67_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr CLA67_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr CLA67_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr CLA67_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr CLA67_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr CLA67_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr CLA67_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr CLA67_out_classname_ = (ptr)(&gs58_);
ptr CLA67_initialize_();
/*constant*/ ptr CLA67_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr CLA67_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr CLA67_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr CLA67_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr CLA67_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr CLA67_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr CLA67_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr CLA67_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr CLA67_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr CLA67_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr CLA67_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr CLA67_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr CLA67_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr CLA67_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr CLA67_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr CLA67_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr CLA67_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr CLA67_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr CLA67_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr CLA67_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr CLA67_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr CLA67_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr CLA67_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr CLA67_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr CLA67_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr CLA67_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr CLA67_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr CLA67_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr CLA67_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr CLA67_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr CLA67_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr CLA67_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr CLA67_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr CLA67_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr CLA67_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr CLA67_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr CLA67_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr CLA67_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr CLA67_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr CLA67_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr CLA67_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr CLA67_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr CLA67_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr CLA67_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr CLA67_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr CLA67_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr CLA67_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr CLA67_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr CLA67_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr CLA67_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr CLA67_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr CLA67_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr CLA67_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr CLA67_void_name_ = (ptr)(&gs37_);
/*constant*/ int CLA67_and_kw_ind_ = 1;
/*constant*/ int CLA67_assert_kw_ind_ = 2;
/*constant*/ int CLA67_break_kw_ind_ = 3;
/*constant*/ int CLA67_class_kw_ind_ = 4;
/*constant*/ int CLA67_constant_kw_ind_ = 5;
/*constant*/ int CLA67_debug_kw_ind_ = 6;
/*constant*/ int CLA67_else_kw_ind_ = 7;
/*constant*/ int CLA67_elsif_kw_ind_ = 8;
/*constant*/ int CLA67_end_kw_ind_ = 9;
/*constant*/ int CLA67_if_kw_ind_ = 10;
/*constant*/ int CLA67_inline_kw_ind_ = 11;
/*constant*/ int CLA67_is_kw_ind_ = 12;
/*constant*/ int CLA67_loop_kw_ind_ = 13;
/*constant*/ int CLA67_not_kw_ind_ = 14;
/*constant*/ int CLA67_or_kw_ind_ = 15;
/*constant*/ int CLA67_private_kw_ind_ = 16;
/*constant*/ int CLA67_return_kw_ind_ = 17;
/*constant*/ int CLA67_shared_kw_ind_ = 18;
/*constant*/ int CLA67_switch_kw_ind_ = 19;
/*constant*/ int CLA67_then_kw_ind_ = 20;
/*constant*/ int CLA67_until_kw_ind_ = 21;
/*constant*/ int CLA67_when_kw_ind_ = 22;
/*constant*/ int CLA67_asize_ind_ = 23;
/*constant*/ int CLA67_asize1_ind_ = 24;
/*constant*/ int CLA67_asize2_ind_ = 25;
/*constant*/ int CLA67_asize3_ind_ = 26;
/*constant*/ int CLA67_asize4_ind_ = 27;
/*constant*/ int CLA67_copy_ind_ = 28;
/*constant*/ int CLA67_deep_copy_ind_ = 29;
/*constant*/ int CLA67_extend_ind_ = 30;
/*constant*/ int CLA67_new_ind_ = 31;
/*constant*/ int CLA67_type_ind_ = 32;
/*constant*/ int CLA67_res_ind_ = 33;
/*constant*/ int CLA67_self_ind_ = 34;
/*constant*/ int CLA67_false_ind_ = 35;
/*constant*/ int CLA67_true_ind_ = 36;
/*constant*/ int CLA67_void_ind_ = 37;
/*constant*/ int CLA67_first_base_class_ind_ = 38;
/*constant*/ int CLA67_array_ind_ = 38;
/*constant*/ int CLA67_array2_ind_ = 39;
/*constant*/ int CLA67_array3_ind_ = 40;
/*constant*/ int CLA67_array4_ind_ = 41;
/*constant*/ int CLA67_bool_ind_ = 42;
/*constant*/ int CLA67_c_ind_ = 43;
/*constant*/ int CLA67_char_ind_ = 44;
/*constant*/ int CLA67_double_ind_ = 45;
/*constant*/ int CLA67_file_ind_ = 46;
/*constant*/ int CLA67_int_ind_ = 47;
/*constant*/ int CLA67_real_ind_ = 48;
/*constant*/ int CLA67_self_type_ind_ = 49;
/*constant*/ int CLA67_str_ind_ = 50;
/*constant*/ int CLA67_str_cursor_ind_ = 51;
/*constant*/ int CLA67_ob_ind_ = 52;
/*constant*/ int CLA67_sys_ind_ = 53;
/*constant*/ int CLA67_fob_ind_ = 54;
/*constant*/ int CLA67_undefine_ind_ = 55;
/*constant*/ int CLA67_err_ind_ = 56;
/*constant*/ int CLA67_in_ind_ = 57;
/*constant*/ int CLA67_out_ind_ = 58;
/*constant*/ int CLA67_sux_ind_ = 59;
/*constant*/ int CLA67_last_base_class_ind_ = 59;
/*constant*/ int CLA67_exception_ind_ = 60;
/*constant*/ int CLA67_init_ind_ = 61;
/*constant*/ int CLA67_alias_kw_ind_ = 62;
/*constant*/ int CLA67_undefine_kw_ind_ = 63;
/*constant*/ int CLA67_require_kw_ind_ = 64;
/*constant*/ int CLA67_ensure_kw_ind_ = 65;
/*constant*/ int CLA67_abstract_kw_ind_ = 66;
/*constant*/ int CLA67_invariant_kw_ind_ = 67;
/*constant*/ int CLA67_raise_kw_ind_ = 68;
/*constant*/ int CLA67_protect_kw_ind_ = 69;
/*constant*/ int CLA67_against_kw_ind_ = 70;
/*constant*/ int CLA67_typecase_kw_ind_ = 71;
/*constant*/ int CLA67_attr_kw_ind_ = 72;
/*constant*/ int CLA67_readonly_kw_ind_ = 73;
/*constant*/ int CLA67_while_kw_ind_ = 74;
/*constant*/ int CLA67_include_kw_ind_ = 75;
/*constant*/ int CLA67_arg_ind_ = 76;
/*constant*/ int CLA67_last_reserved_word_ind_ = 76;
char CLA67_base_classname_p_();
ptr CLA67_create_();
int CLA67_num_params_();
char CLA67_non_param_p_();
ptr CLA67_name_str_();
ptr CLA67_filename_();
CLA67_change_name_();
CLA67_add_features_();
ptr CLA67_features_();
int CLA67_unique_key_();
extern int attr_ent_CLA67[];

ptr CLA67_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   CATT_(self__,4) = (char)0;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char CLA67_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr CLA67_create_(self__,index__,lno__,pl__,fl__,ab__)
ptr self__;
int index__;
int lno__;
ptr pl__;
ptr fl__;
char ab__;
{
   ptr res__ = 0;
   ptr gl948_;
   static int gl949_;
   static union dtype_ gl950_;
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;
   int    k__ = S_int_VOID_;

   res__ = (ptr)new_(67,0);
   IATT_(res__,12) = (int)index__;
   PATT_(res__,24) = (ptr)pl__;
   PATT_(res__,20) = (ptr)fl__;
   IATT_(res__,40) = (int)lno__;
   IATT_(res__,8) = (int)GLO68_curr_lineno_;
   PATT_(res__,16) = (ptr)GLO68_curr_filename_;
   if ((GLO68_curr_file_stat_ != 0)) {
      IATT_(res__,36) = (int)C_F74_st_mtime_(PATT_(GLO68_curr_file_stat_,8));
   }
   else {
   }
   PATT_(res__,32) = (ptr)LIS87_create_(0,2);
   CATT_(res__,4) = (char)ab__;
   if ((pl__ != 0)) {
      i__ = (int)0;
      j__ = S_int_VOID_;
      sz__ = (int)IATT_(pl__,4);
      while (1) {
         if ((i__ >= (sz__ - 1))) {
            goto goto_tag_951_;
         }
         else {
         }
         j__ = (int)(i__ + 1);
         while (1) {
            if ((j__ >= sz__)) {
               goto goto_tag_952_;
            }
            else {
            }
            if ((IATT_(pl__, 16 + ((i__) << 2)) == IATT_(pl__, 16 + ((j__) << 2)))) {
               CS_160_repeated_type_parameters_(0,ERR96_filename_(0),lno__,IATT_(pl__, 16 + ((i__) << 2)),IATT_(pl__, 16 + ((j__) << 2)));
            }
            else {
            }
            j__ = (int)(j__ + 1);
         }
      goto_tag_952_: ;
         i__ = (int)(i__ + 1);
      }
   goto_tag_951_: ;
   }
   else {
   }
   fsz__ = (int)IATT_(fl__,12);
   k__ = (int)0;
   while (1) {
      if ((k__ >= fsz__)) {
         goto goto_tag_953_;
      }
      else {
      }
      if ((PATT_(fl__, 24 + ((k__) << 2)) != 0)) {
         gl948_ = PATT_(fl__, 24 + ((k__) << 2));
         cache_dispatch_(gl948_,329,gl949_,INTVAL_(gl950_));
         PATT_(gl948_,INTVAL_(gl950_)) = (ptr)res__;
      }
      else {
      }
      k__ = (int)(k__ + 1);
   }
goto_tag_953_: ;

   ret0__:
   return (res__);
}

int CLA67_num_params_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((PATT_(self__,24) == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(PATT_(self__,24),4);
   }

   ret0__:
   return (res__);
}

char CLA67_non_param_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if ((PATT_(self__,24) == 0)) {
      res__ = (char)1;
   }
   else {
      res__ = (char)(IATT_(PATT_(self__,24),4) == 0);
   }

   ret0__:
   return (res__);
}

ptr CLA67_name_str_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR69_at_index_(GLO68_str_table_,IATT_(self__,12));

   ret0__:
   return (res__);
}

ptr CLA67_filename_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls336_," ; ");
   int    i__ = S_int_VOID_;
   int    msz__ = S_int_VOID_;

   res__ = (ptr)PATT_(self__,16);
   i__ = (int)0;
   msz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      msz__ = (int)IATT_(PATT_(self__,32),4);
   }
   else {
   }
   while (1) {
      if ((i__ >= msz__)) {
         goto goto_tag_954_;
      }
      else {
      }
      res__ = (ptr)STR20_s_(STR20_s_(res__,(ptr)(&ls336_)),PATT_(PATT_(self__,32), 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_954_: ;

   ret0__:
   return (res__);
}

CLA67_change_name_(self__,new_nm__)
ptr self__;
ptr new_nm__;
{

   IATT_(self__,12) = (int)STR69_insert_str_(GLO68_str_table_,new_nm__);

   ret0__:
   return;
}

CLA67_add_features_(self__,nflst__)
ptr self__;
ptr nflst__;
{
   int    i__ = S_int_VOID_;
   int    num_feats__ = S_int_VOID_;

   i__ = (int)0;
   num_feats__ = S_int_VOID_;
   if ((nflst__ != 0)) {
      num_feats__ = (int)IATT_(nflst__,12);
   }
   else {
   }
   while (1) {
      if ((i__ >= num_feats__)) {
         goto goto_tag_955_;
      }
      else {
      }
      PATT_(self__,20) = (ptr)LST43_add_unique_feat_(PATT_(self__,20),PATT_(nflst__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_955_: ;

   ret0__:
   return;
}

ptr CLA67_features_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,20);

   ret0__:
   return (res__);
}

int CLA67_unique_key_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}


/* str_ta69.c : Sather class: STR_TABLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR70_strval_(ptr self__);
extern char STR70_is_equal_(ptr self__, ptr strv__);
extern int CHA14_to_i_(char self__);
extern int INT15_u_mod_(int self__, int i__);
extern /*shared*/ ptr COM82_target_dir_;
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char STR20_is_equal_(ptr self__, ptr st__);
extern /*constant*/ ptr INS88_name_ind_file_;
extern ptr INT164_create_(ptr self__);
extern ptr INT164_insert_(ptr self__, int k__);
extern char INT164_get_(ptr self__, int k__);
extern void STR171_open_for_read_(ptr self__, ptr nm__);
extern int STR171_error_(ptr self__);
extern int STR171_get_i_(ptr self__);
extern int STR171_get_ci_(ptr self__);
extern ptr STR171_get_s_up_to_(ptr self__, char c0__);
extern char STR171_check_eof_(ptr self__);
extern void STR171_close_(ptr self__);
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
extern struct { int tp_; int sz_; char st_; } gs2284_;
#include "macros_.h"



/*shared*/ ptr STR69_reserved_names_;
/*constant*/ ptr STR69_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr STR69_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr STR69_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr STR69_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr STR69_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr STR69_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr STR69_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr STR69_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr STR69_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr STR69_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr STR69_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr STR69_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr STR69_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr STR69_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr STR69_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr STR69_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr STR69_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr STR69_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr STR69_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr STR69_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr STR69_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr STR69_out_classname_ = (ptr)(&gs58_);
ptr STR69_initialize_(ptr self__, ptr initarg__);
/*constant*/ ptr STR69_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr STR69_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr STR69_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr STR69_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr STR69_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr STR69_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr STR69_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr STR69_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr STR69_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr STR69_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr STR69_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr STR69_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr STR69_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr STR69_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr STR69_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr STR69_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr STR69_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr STR69_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr STR69_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr STR69_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr STR69_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr STR69_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr STR69_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr STR69_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr STR69_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr STR69_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr STR69_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr STR69_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr STR69_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr STR69_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr STR69_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr STR69_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr STR69_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr STR69_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr STR69_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr STR69_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr STR69_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr STR69_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr STR69_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr STR69_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr STR69_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr STR69_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr STR69_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr STR69_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr STR69_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr STR69_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr STR69_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr STR69_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr STR69_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr STR69_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr STR69_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr STR69_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr STR69_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr STR69_void_name_ = (ptr)(&gs37_);
/*constant*/ int STR69_and_kw_ind_ = 1;
/*constant*/ int STR69_assert_kw_ind_ = 2;
/*constant*/ int STR69_break_kw_ind_ = 3;
/*constant*/ int STR69_class_kw_ind_ = 4;
/*constant*/ int STR69_constant_kw_ind_ = 5;
/*constant*/ int STR69_debug_kw_ind_ = 6;
/*constant*/ int STR69_else_kw_ind_ = 7;
/*constant*/ int STR69_elsif_kw_ind_ = 8;
/*constant*/ int STR69_end_kw_ind_ = 9;
/*constant*/ int STR69_if_kw_ind_ = 10;
/*constant*/ int STR69_inline_kw_ind_ = 11;
/*constant*/ int STR69_is_kw_ind_ = 12;
/*constant*/ int STR69_loop_kw_ind_ = 13;
/*constant*/ int STR69_not_kw_ind_ = 14;
/*constant*/ int STR69_or_kw_ind_ = 15;
/*constant*/ int STR69_private_kw_ind_ = 16;
/*constant*/ int STR69_return_kw_ind_ = 17;
/*constant*/ int STR69_shared_kw_ind_ = 18;
/*constant*/ int STR69_switch_kw_ind_ = 19;
/*constant*/ int STR69_then_kw_ind_ = 20;
/*constant*/ int STR69_until_kw_ind_ = 21;
/*constant*/ int STR69_when_kw_ind_ = 22;
/*constant*/ int STR69_asize_ind_ = 23;
/*constant*/ int STR69_asize1_ind_ = 24;
/*constant*/ int STR69_asize2_ind_ = 25;
/*constant*/ int STR69_asize3_ind_ = 26;
/*constant*/ int STR69_asize4_ind_ = 27;
/*constant*/ int STR69_copy_ind_ = 28;
/*constant*/ int STR69_deep_copy_ind_ = 29;
/*constant*/ int STR69_extend_ind_ = 30;
/*constant*/ int STR69_new_ind_ = 31;
/*constant*/ int STR69_type_ind_ = 32;
/*constant*/ int STR69_res_ind_ = 33;
/*constant*/ int STR69_self_ind_ = 34;
/*constant*/ int STR69_false_ind_ = 35;
/*constant*/ int STR69_true_ind_ = 36;
/*constant*/ int STR69_void_ind_ = 37;
/*constant*/ int STR69_first_base_class_ind_ = 38;
/*constant*/ int STR69_array_ind_ = 38;
/*constant*/ int STR69_array2_ind_ = 39;
/*constant*/ int STR69_array3_ind_ = 40;
/*constant*/ int STR69_array4_ind_ = 41;
/*constant*/ int STR69_bool_ind_ = 42;
/*constant*/ int STR69_c_ind_ = 43;
/*constant*/ int STR69_char_ind_ = 44;
/*constant*/ int STR69_double_ind_ = 45;
/*constant*/ int STR69_file_ind_ = 46;
/*constant*/ int STR69_int_ind_ = 47;
/*constant*/ int STR69_real_ind_ = 48;
/*constant*/ int STR69_self_type_ind_ = 49;
/*constant*/ int STR69_str_ind_ = 50;
/*constant*/ int STR69_str_cursor_ind_ = 51;
/*constant*/ int STR69_ob_ind_ = 52;
/*constant*/ int STR69_sys_ind_ = 53;
/*constant*/ int STR69_fob_ind_ = 54;
/*constant*/ int STR69_undefine_ind_ = 55;
/*constant*/ int STR69_err_ind_ = 56;
/*constant*/ int STR69_in_ind_ = 57;
/*constant*/ int STR69_out_ind_ = 58;
/*constant*/ int STR69_sux_ind_ = 59;
/*constant*/ int STR69_last_base_class_ind_ = 59;
/*constant*/ int STR69_exception_ind_ = 60;
/*constant*/ int STR69_init_ind_ = 61;
/*constant*/ int STR69_alias_kw_ind_ = 62;
/*constant*/ int STR69_undefine_kw_ind_ = 63;
/*constant*/ int STR69_require_kw_ind_ = 64;
/*constant*/ int STR69_ensure_kw_ind_ = 65;
/*constant*/ int STR69_abstract_kw_ind_ = 66;
/*constant*/ int STR69_invariant_kw_ind_ = 67;
/*constant*/ int STR69_raise_kw_ind_ = 68;
/*constant*/ int STR69_protect_kw_ind_ = 69;
/*constant*/ int STR69_against_kw_ind_ = 70;
/*constant*/ int STR69_typecase_kw_ind_ = 71;
/*constant*/ int STR69_attr_kw_ind_ = 72;
/*constant*/ int STR69_readonly_kw_ind_ = 73;
/*constant*/ int STR69_while_kw_ind_ = 74;
/*constant*/ int STR69_include_kw_ind_ = 75;
/*constant*/ int STR69_arg_ind_ = 76;
/*constant*/ int STR69_last_reserved_word_ind_ = 76;
char STR69_base_classname_p_(ptr self__, int i__);
/*constant*/ int STR69_init_size_ = 2000;
ptr STR69_create_(ptr self__);
void STR69_augment_(ptr self__);
char STR69_reserved_name_p_(ptr self__, int nm__);
int STR69_hash_string_(ptr self__, ptr s__);
int STR69_hash_string_buf_(ptr self__, ptr s__);
int STR69_index_of_str_(ptr self__, ptr s__);
int STR69_insert_str_(ptr self__, ptr s__);
int STR69_find_str_(ptr self__, ptr s__);
void STR69_double_tables_(ptr self__);
ptr STR69_at_index_(ptr self__, int i__);
extern int attr_ent_STR69[];

ptr STR69_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char STR69_base_classname_p_(ptr self__, int i__)
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr STR69_create_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,1,ls1016_,"");
   ptr gl956_;
   ptr gl957_;
   int    i__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   STR69_reserved_names_ = (ptr)INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(INT164_insert_(STR69_reserved_names_,24),25),26),27),28),30),61),31),33),34),32),37);
   res__ = (ptr)new_(69,0);
   IATT_(res__,16) = (int)2000;
   IATT_(res__,20) = (int)(2 * IATT_(res__,16));
   gl956_ = PATT_(res__,4);
   PATT_(res__,4) = (ptr)new1_(77,IATT_(res__,16),0);
   gl957_ = PATT_(res__,8);
   PATT_(res__,8) = (ptr)new1_(163,IATT_(res__,20),1);
   PATT_(PATT_(res__,4), 8 + ((0) << 2)) = (ptr)(ptr)(&ls1016_);
   PATT_(PATT_(res__,4), 8 + ((62) << 2)) = (ptr)(ptr)(&gs62_);
   PATT_(PATT_(res__,4), 8 + ((72) << 2)) = (ptr)(ptr)(&gs72_);
   PATT_(PATT_(res__,4), 8 + ((63) << 2)) = (ptr)(ptr)(&gs63_);
   PATT_(PATT_(res__,4), 8 + ((64) << 2)) = (ptr)(ptr)(&gs64_);
   PATT_(PATT_(res__,4), 8 + ((65) << 2)) = (ptr)(ptr)(&gs65_);
   PATT_(PATT_(res__,4), 8 + ((67) << 2)) = (ptr)(ptr)(&gs67_);
   PATT_(PATT_(res__,4), 8 + ((71) << 2)) = (ptr)(ptr)(&gs71_);
   PATT_(PATT_(res__,4), 8 + ((66) << 2)) = (ptr)(ptr)(&gs66_);
   PATT_(PATT_(res__,4), 8 + ((75) << 2)) = (ptr)(ptr)(&gs75_);
   PATT_(PATT_(res__,4), 8 + ((1) << 2)) = (ptr)(ptr)(&gs1_);
   PATT_(PATT_(res__,4), 8 + ((2) << 2)) = (ptr)(ptr)(&gs2_);
   PATT_(PATT_(res__,4), 8 + ((3) << 2)) = (ptr)(ptr)(&gs3_);
   PATT_(PATT_(res__,4), 8 + ((4) << 2)) = (ptr)(ptr)(&gs4_);
   PATT_(PATT_(res__,4), 8 + ((5) << 2)) = (ptr)(ptr)(&gs5_);
   PATT_(PATT_(res__,4), 8 + ((6) << 2)) = (ptr)(ptr)(&gs6_);
   PATT_(PATT_(res__,4), 8 + ((7) << 2)) = (ptr)(ptr)(&gs7_);
   PATT_(PATT_(res__,4), 8 + ((8) << 2)) = (ptr)(ptr)(&gs8_);
   PATT_(PATT_(res__,4), 8 + ((9) << 2)) = (ptr)(ptr)(&gs9_);
   PATT_(PATT_(res__,4), 8 + ((10) << 2)) = (ptr)(ptr)(&gs10_);
   PATT_(PATT_(res__,4), 8 + ((11) << 2)) = (ptr)(ptr)(&gs11_);
   PATT_(PATT_(res__,4), 8 + ((12) << 2)) = (ptr)(ptr)(&gs12_);
   PATT_(PATT_(res__,4), 8 + ((13) << 2)) = (ptr)(ptr)(&gs13_);
   PATT_(PATT_(res__,4), 8 + ((14) << 2)) = (ptr)(ptr)(&gs14_);
   PATT_(PATT_(res__,4), 8 + ((15) << 2)) = (ptr)(ptr)(&gs15_);
   PATT_(PATT_(res__,4), 8 + ((16) << 2)) = (ptr)(ptr)(&gs16_);
   PATT_(PATT_(res__,4), 8 + ((69) << 2)) = (ptr)(ptr)(&gs69_);
   PATT_(PATT_(res__,4), 8 + ((68) << 2)) = (ptr)(ptr)(&gs68_);
   PATT_(PATT_(res__,4), 8 + ((70) << 2)) = (ptr)(ptr)(&gs70_);
   PATT_(PATT_(res__,4), 8 + ((17) << 2)) = (ptr)(ptr)(&gs17_);
   PATT_(PATT_(res__,4), 8 + ((73) << 2)) = (ptr)(ptr)(&gs73_);
   PATT_(PATT_(res__,4), 8 + ((18) << 2)) = (ptr)(ptr)(&gs18_);
   PATT_(PATT_(res__,4), 8 + ((19) << 2)) = (ptr)(ptr)(&gs19_);
   PATT_(PATT_(res__,4), 8 + ((20) << 2)) = (ptr)(ptr)(&gs20_);
   PATT_(PATT_(res__,4), 8 + ((21) << 2)) = (ptr)(ptr)(&gs21_);
   PATT_(PATT_(res__,4), 8 + ((22) << 2)) = (ptr)(ptr)(&gs22_);
   PATT_(PATT_(res__,4), 8 + ((74) << 2)) = (ptr)(ptr)(&gs74_);
   PATT_(PATT_(res__,4), 8 + ((23) << 2)) = (ptr)(ptr)(&gs23_);
   PATT_(PATT_(res__,4), 8 + ((24) << 2)) = (ptr)(ptr)(&gs24_);
   PATT_(PATT_(res__,4), 8 + ((25) << 2)) = (ptr)(ptr)(&gs25_);
   PATT_(PATT_(res__,4), 8 + ((26) << 2)) = (ptr)(ptr)(&gs26_);
   PATT_(PATT_(res__,4), 8 + ((27) << 2)) = (ptr)(ptr)(&gs27_);
   PATT_(PATT_(res__,4), 8 + ((28) << 2)) = (ptr)(ptr)(&gs28_);
   PATT_(PATT_(res__,4), 8 + ((29) << 2)) = (ptr)(ptr)(&gs29_);
   PATT_(PATT_(res__,4), 8 + ((30) << 2)) = (ptr)(ptr)(&gs30_);
   PATT_(PATT_(res__,4), 8 + ((61) << 2)) = (ptr)(ptr)(&gs61_);
   PATT_(PATT_(res__,4), 8 + ((31) << 2)) = (ptr)(ptr)(&gs31_);
   PATT_(PATT_(res__,4), 8 + ((32) << 2)) = (ptr)(ptr)(&gs32_);
   PATT_(PATT_(res__,4), 8 + ((33) << 2)) = (ptr)(ptr)(&gs33_);
   PATT_(PATT_(res__,4), 8 + ((34) << 2)) = (ptr)(ptr)(&gs34_);
   PATT_(PATT_(res__,4), 8 + ((60) << 2)) = (ptr)(ptr)(&gs60_);
   PATT_(PATT_(res__,4), 8 + ((76) << 2)) = (ptr)(ptr)(&gs1165_);
   PATT_(PATT_(res__,4), 8 + ((35) << 2)) = (ptr)(ptr)(&gs35_);
   PATT_(PATT_(res__,4), 8 + ((36) << 2)) = (ptr)(ptr)(&gs36_);
   PATT_(PATT_(res__,4), 8 + ((37) << 2)) = (ptr)(ptr)(&gs37_);
   PATT_(PATT_(res__,4), 8 + ((38) << 2)) = (ptr)(ptr)(&gs38_);
   PATT_(PATT_(res__,4), 8 + ((39) << 2)) = (ptr)(ptr)(&gs39_);
   PATT_(PATT_(res__,4), 8 + ((40) << 2)) = (ptr)(ptr)(&gs40_);
   PATT_(PATT_(res__,4), 8 + ((41) << 2)) = (ptr)(ptr)(&gs41_);
   PATT_(PATT_(res__,4), 8 + ((42) << 2)) = (ptr)(ptr)(&gs42_);
   PATT_(PATT_(res__,4), 8 + ((43) << 2)) = (ptr)(ptr)(&gs43_);
   PATT_(PATT_(res__,4), 8 + ((44) << 2)) = (ptr)(ptr)(&gs44_);
   PATT_(PATT_(res__,4), 8 + ((45) << 2)) = (ptr)(ptr)(&gs45_);
   PATT_(PATT_(res__,4), 8 + ((46) << 2)) = (ptr)(ptr)(&gs46_);
   PATT_(PATT_(res__,4), 8 + ((47) << 2)) = (ptr)(ptr)(&gs47_);
   PATT_(PATT_(res__,4), 8 + ((48) << 2)) = (ptr)(ptr)(&gs48_);
   PATT_(PATT_(res__,4), 8 + ((49) << 2)) = (ptr)(ptr)(&gs49_);
   PATT_(PATT_(res__,4), 8 + ((50) << 2)) = (ptr)(ptr)(&gs50_);
   PATT_(PATT_(res__,4), 8 + ((51) << 2)) = (ptr)(ptr)(&gs51_);
   PATT_(PATT_(res__,4), 8 + ((52) << 2)) = (ptr)(ptr)(&gs52_);
   PATT_(PATT_(res__,4), 8 + ((53) << 2)) = (ptr)(ptr)(&gs53_);
   PATT_(PATT_(res__,4), 8 + ((54) << 2)) = (ptr)(ptr)(&gs54_);
   PATT_(PATT_(res__,4), 8 + ((59) << 2)) = (ptr)(ptr)(&gs59_);
   PATT_(PATT_(res__,4), 8 + ((55) << 2)) = (ptr)(ptr)(&gs55_);
   PATT_(PATT_(res__,4), 8 + ((56) << 2)) = (ptr)(ptr)(&gs56_);
   PATT_(PATT_(res__,4), 8 + ((57) << 2)) = (ptr)(ptr)(&gs57_);
   PATT_(PATT_(res__,4), 8 + ((58) << 2)) = (ptr)(ptr)(&gs58_);
   IATT_(res__,12) = (int)(76 + 1);
   i__ = (int)1;
   while (1) {
      if ((i__ > 76)) {
         goto goto_tag_958_;
      }
      else {
      }
      insert__ = (int)STR69_hash_string_(res__,PATT_(PATT_(res__,4), 8 + ((i__) << 2)));
      while (1) {
         if ((IATT_(PATT_(res__,8), 8 + ((insert__) << 2)) != 0)) {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(res__,20));
         }
         else {
            goto goto_tag_959_;
         }
      }
   goto_tag_959_: ;
      IATT_(PATT_(res__,8), 8 + ((insert__) << 2)) = (int)i__;
      i__ = (int)(i__ + 1);
   }
goto_tag_958_: ;

   ret0__:
   return (res__);
}

void STR69_augment_(ptr self__)
{
   ptr    fname__ = 0;
   ptr    f__ = 0;
   int    i__ = S_int_VOID_;
   int    c__ = S_int_VOID_;
   ptr    s__ = 0;
   int    first__ = S_int_VOID_;
   int    prev__ = S_int_VOID_;
   int    last__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   fname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2284_));
   f__ = (ptr)new_(171,0);
   STR171_open_for_read_(f__,fname__);
   if ((STR171_error_(f__) != 0)) {
      goto ret0__;
   }
   else {
      i__ = (int)STR171_get_i_(f__);
      c__ = (int)STR171_get_ci_(f__);
      s__ = (ptr)STR171_get_s_up_to_(f__,'\10');
      first__ = (int)i__;
      prev__ = S_int_VOID_;
      while (1) {
         if (STR171_check_eof_(f__)) {
            goto goto_tag_960_;
         }
         else {
         }
         if ((i__ == IATT_(self__,16))) {
            STR69_double_tables_(self__);
         }
         else {
         }
         if ((PATT_(PATT_(self__,4), 8 + ((i__) << 2)) == 0)) {
            PATT_(PATT_(self__,4), 8 + ((i__) << 2)) = (ptr)s__;
         }
         else {
         }
         prev__ = (int)i__;
         i__ = (int)STR171_get_i_(f__);
         c__ = (int)STR171_get_ci_(f__);
         s__ = (ptr)STR171_get_s_up_to_(f__,'\10');
      }
   goto_tag_960_: ;
      IATT_(self__,12) = (int)(prev__ + 1);
      last__ = (int)prev__;
      i__ = (int)first__;
      while (1) {
         if ((i__ > last__)) {
            goto goto_tag_961_;
         }
         else {
         }
         insert__ = (int)STR69_hash_string_(self__,PATT_(PATT_(self__,4), 8 + ((i__) << 2)));
         while (1) {
            if ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) != 0)) {
               insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
            }
            else {
               goto goto_tag_962_;
            }
         }
      goto_tag_962_: ;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)i__;
         i__ = (int)(i__ + 1);
      }
   goto_tag_961_: ;
      STR171_close_(f__);
   }

   ret0__:
   return;
}

char STR69_reserved_name_p_(ptr self__, int nm__)
{
   char res__ = S_char_VOID_;

   res__ = (char)INT164_get_(STR69_reserved_names_,nm__);

   ret0__:
   return (res__);
}

int STR69_hash_string_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((CATT_(s__, 8 + ((i__))) == '\00')) {
         goto goto_tag_963_;
      }
      else {
      }
      res__ = (int)(CHA14_to_i_(CATT_(s__, 8 + ((i__)))) + (31 * res__));
      i__ = (int)(i__ + 1);
   }
goto_tag_963_: ;
   res__ = (int)INT15_u_mod_(res__,IATT_(self__,20));

   ret0__:
   return (res__);
}

int STR69_hash_string_buf_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((CATT_(s__, 16 + ((i__))) == '\00')) {
         goto goto_tag_964_;
      }
      else {
      }
      res__ = (int)(CHA14_to_i_(CATT_(s__, 16 + ((i__)))) + (31 * res__));
      i__ = (int)(i__ + 1);
   }
goto_tag_964_: ;
   res__ = (int)INT15_u_mod_(res__,IATT_(self__,20));

   ret0__:
   return (res__);
}

int STR69_index_of_str_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   if ((s__ == 0)) {
      res__ = (int)(- 1);
      goto ret0__;
   }
   else {
   }
   insert__ = (int)STR69_hash_string_buf_(self__,s__);
   while (1) {
      if ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) == 0)) {
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,12);
         PATT_(PATT_(self__,4), 8 + ((IATT_(self__,12)) << 2)) = (ptr)STR70_strval_(s__);
         res__ = (int)IATT_(self__,12);
         IATT_(self__,12) = (int)(IATT_(self__,12) + 1);
         if ((IATT_(self__,12) == IATT_(self__,16))) {
            STR69_double_tables_(self__);
         }
         else {
         }
         goto ret0__;
      }
      else {
         if (STR70_is_equal_(s__,PATT_(PATT_(self__,4), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)))) {
            res__ = (int)IATT_(PATT_(self__,8), 8 + ((insert__) << 2));
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

int STR69_insert_str_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   if ((s__ == 0)) {
      res__ = (int)(- 1);
      goto ret0__;
   }
   else {
   }
   insert__ = (int)STR69_hash_string_(self__,s__);
   while (1) {
      if ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) == 0)) {
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,12);
         PATT_(PATT_(self__,4), 8 + ((IATT_(self__,12)) << 2)) = (ptr)s__;
         res__ = (int)IATT_(self__,12);
         IATT_(self__,12) = (int)(IATT_(self__,12) + 1);
         if ((IATT_(self__,12) == IATT_(self__,16))) {
            STR69_double_tables_(self__);
         }
         else {
         }
         goto ret0__;
      }
      else {
         if (STR20_is_equal_(s__,PATT_(PATT_(self__,4), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)))) {
            res__ = (int)IATT_(PATT_(self__,8), 8 + ((insert__) << 2));
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

int STR69_find_str_(ptr self__, ptr s__)
{
   int res__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   if ((s__ == 0)) {
      res__ = (int)(- 1);
      goto ret0__;
   }
   else {
   }
   insert__ = (int)STR69_hash_string_(self__,s__);
   while (1) {
      if ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) == 0)) {
         res__ = (int)(- 1);
         goto ret0__;
      }
      else {
         if (STR20_is_equal_(s__,PATT_(PATT_(self__,4), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)))) {
            res__ = (int)IATT_(PATT_(self__,8), 8 + ((insert__) << 2));
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

void STR69_double_tables_(ptr self__)
{
   ptr gl965_;
   int    old_ssize__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   old_ssize__ = (int)IATT_(self__,16);
   IATT_(self__,16) = (int)(IATT_(self__,16) * 2);
   IATT_(self__,20) = (int)(2 * IATT_(self__,16));
   gl965_ = PATT_(self__,8);
   PATT_(self__,8) = (ptr)new1_(163,IATT_(self__,20),1);
   PATT_(self__,4) = (ptr)extend1_(PATT_(self__,4),IATT_(self__,16),0);
   i__ = (int)1;
   insert__ = S_int_VOID_;
   while (1) {
      if ((i__ >= old_ssize__)) {
         goto goto_tag_966_;
      }
      else {
      }
      if ((PATT_(PATT_(self__,4), 8 + ((i__) << 2)) != 0)) {
         insert__ = (int)STR69_hash_string_(self__,PATT_(PATT_(self__,4), 8 + ((i__) << 2)));
         while (1) {
            if ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) != 0)) {
               insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
            }
            else {
               goto goto_tag_967_;
            }
         }
      goto_tag_967_: ;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)i__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_966_: ;

   ret0__:
   return;
}

ptr STR69_at_index_(ptr self__, int i__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(PATT_(self__,4), 8 + ((i__) << 2));

   ret0__:
   return (res__);
}


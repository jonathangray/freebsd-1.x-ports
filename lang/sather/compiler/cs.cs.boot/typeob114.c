/* typeob114.c : Sather class: TYPEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr INS150_create_(ptr self__, int index__, int ln__);
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



/*constant*/ int TYP114_print_indent_ = 2;
ptr TYP114_create_(ptr self__);
void TYP114_out_of_line_(ptr self__, ptr fn__);
ptr TYP114_dup_(ptr self__);
void TYP114_put_kwdname_(ptr self__, int nm__);
ptr TYP114_sather_code_(ptr self__);
ptr TYP114_initialize_(ptr self__, ptr initarg__);
ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ ptr TYP114_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr TYP114_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr TYP114_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr TYP114_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr TYP114_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr TYP114_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr TYP114_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr TYP114_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr TYP114_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr TYP114_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr TYP114_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr TYP114_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr TYP114_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr TYP114_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr TYP114_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr TYP114_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr TYP114_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr TYP114_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr TYP114_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr TYP114_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr TYP114_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr TYP114_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr TYP114_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr TYP114_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr TYP114_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr TYP114_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr TYP114_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr TYP114_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr TYP114_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr TYP114_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr TYP114_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr TYP114_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr TYP114_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr TYP114_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr TYP114_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr TYP114_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr TYP114_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr TYP114_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr TYP114_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr TYP114_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr TYP114_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr TYP114_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr TYP114_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr TYP114_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr TYP114_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr TYP114_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr TYP114_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr TYP114_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr TYP114_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr TYP114_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr TYP114_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr TYP114_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr TYP114_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr TYP114_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr TYP114_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr TYP114_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr TYP114_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr TYP114_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr TYP114_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr TYP114_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr TYP114_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr TYP114_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr TYP114_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr TYP114_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr TYP114_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr TYP114_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr TYP114_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr TYP114_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr TYP114_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr TYP114_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr TYP114_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr TYP114_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr TYP114_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr TYP114_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr TYP114_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr TYP114_out_classname_ = (ptr)(&gs58_);
/*constant*/ int TYP114_and_kw_ind_ = 1;
/*constant*/ int TYP114_assert_kw_ind_ = 2;
/*constant*/ int TYP114_break_kw_ind_ = 3;
/*constant*/ int TYP114_class_kw_ind_ = 4;
/*constant*/ int TYP114_constant_kw_ind_ = 5;
/*constant*/ int TYP114_debug_kw_ind_ = 6;
/*constant*/ int TYP114_else_kw_ind_ = 7;
/*constant*/ int TYP114_elsif_kw_ind_ = 8;
/*constant*/ int TYP114_end_kw_ind_ = 9;
/*constant*/ int TYP114_if_kw_ind_ = 10;
/*constant*/ int TYP114_inline_kw_ind_ = 11;
/*constant*/ int TYP114_is_kw_ind_ = 12;
/*constant*/ int TYP114_loop_kw_ind_ = 13;
/*constant*/ int TYP114_not_kw_ind_ = 14;
/*constant*/ int TYP114_or_kw_ind_ = 15;
/*constant*/ int TYP114_private_kw_ind_ = 16;
/*constant*/ int TYP114_return_kw_ind_ = 17;
/*constant*/ int TYP114_shared_kw_ind_ = 18;
/*constant*/ int TYP114_switch_kw_ind_ = 19;
/*constant*/ int TYP114_then_kw_ind_ = 20;
/*constant*/ int TYP114_until_kw_ind_ = 21;
/*constant*/ int TYP114_when_kw_ind_ = 22;
/*constant*/ int TYP114_asize_ind_ = 23;
/*constant*/ int TYP114_asize1_ind_ = 24;
/*constant*/ int TYP114_asize2_ind_ = 25;
/*constant*/ int TYP114_asize3_ind_ = 26;
/*constant*/ int TYP114_asize4_ind_ = 27;
/*constant*/ int TYP114_copy_ind_ = 28;
/*constant*/ int TYP114_deep_copy_ind_ = 29;
/*constant*/ int TYP114_extend_ind_ = 30;
/*constant*/ int TYP114_new_ind_ = 31;
/*constant*/ int TYP114_type_ind_ = 32;
/*constant*/ int TYP114_res_ind_ = 33;
/*constant*/ int TYP114_self_ind_ = 34;
/*constant*/ int TYP114_false_ind_ = 35;
/*constant*/ int TYP114_true_ind_ = 36;
/*constant*/ int TYP114_void_ind_ = 37;
/*constant*/ int TYP114_first_base_class_ind_ = 38;
/*constant*/ int TYP114_array_ind_ = 38;
/*constant*/ int TYP114_array2_ind_ = 39;
/*constant*/ int TYP114_array3_ind_ = 40;
/*constant*/ int TYP114_array4_ind_ = 41;
/*constant*/ int TYP114_bool_ind_ = 42;
/*constant*/ int TYP114_c_ind_ = 43;
/*constant*/ int TYP114_char_ind_ = 44;
/*constant*/ int TYP114_double_ind_ = 45;
/*constant*/ int TYP114_file_ind_ = 46;
/*constant*/ int TYP114_int_ind_ = 47;
/*constant*/ int TYP114_real_ind_ = 48;
/*constant*/ int TYP114_self_type_ind_ = 49;
/*constant*/ int TYP114_str_ind_ = 50;
/*constant*/ int TYP114_str_cursor_ind_ = 51;
/*constant*/ int TYP114_ob_ind_ = 52;
/*constant*/ int TYP114_sys_ind_ = 53;
/*constant*/ int TYP114_fob_ind_ = 54;
/*constant*/ int TYP114_undefine_ind_ = 55;
/*constant*/ int TYP114_err_ind_ = 56;
/*constant*/ int TYP114_in_ind_ = 57;
/*constant*/ int TYP114_out_ind_ = 58;
/*constant*/ int TYP114_sux_ind_ = 59;
/*constant*/ int TYP114_last_base_class_ind_ = 59;
/*constant*/ int TYP114_exception_ind_ = 60;
/*constant*/ int TYP114_init_ind_ = 61;
/*constant*/ int TYP114_alias_kw_ind_ = 62;
/*constant*/ int TYP114_undefine_kw_ind_ = 63;
/*constant*/ int TYP114_require_kw_ind_ = 64;
/*constant*/ int TYP114_ensure_kw_ind_ = 65;
/*constant*/ int TYP114_abstract_kw_ind_ = 66;
/*constant*/ int TYP114_invariant_kw_ind_ = 67;
/*constant*/ int TYP114_raise_kw_ind_ = 68;
/*constant*/ int TYP114_protect_kw_ind_ = 69;
/*constant*/ int TYP114_against_kw_ind_ = 70;
/*constant*/ int TYP114_typecase_kw_ind_ = 71;
/*constant*/ int TYP114_attr_kw_ind_ = 72;
/*constant*/ int TYP114_readonly_kw_ind_ = 73;
/*constant*/ int TYP114_while_kw_ind_ = 74;
/*constant*/ int TYP114_include_kw_ind_ = 75;
/*constant*/ int TYP114_arg_ind_ = 76;
/*constant*/ int TYP114_last_reserved_word_ind_ = 76;
char TYP114_base_classname_p_(ptr self__, int i__);
ptr TYP114_disp_type_(ptr self__);
ptr TYP114_name_str_(ptr self__);
ptr TYP114_get_key_(ptr self__, ptr pl__, ptr pi__);
extern int attr_ent_TYP114[];

ptr TYP114_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(114,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void TYP114_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr TYP114_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void TYP114_put_kwdname_(ptr self__, int nm__)
{
   ptr gl2676_;
   static int gl2677_;
   static union dtype_ gl2678_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2676_ = x__;
   cache_dispatch_(gl2676_,796,gl2677_,INTVAL_(gl2678_));
   IATT_(gl2676_,INTVAL_(gl2678_)) = (int)nm__;

   ret0__:
   return;
}

ptr TYP114_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP114_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)INS150_create_(0,0,IATT_(self__,4));

   ret0__:
   return (res__);
}

char TYP114_base_classname_p_(ptr self__, int i__)
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr TYP114_disp_type_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP114_name_str_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP114_get_key_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}


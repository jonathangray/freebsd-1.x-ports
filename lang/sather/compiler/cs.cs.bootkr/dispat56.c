/* dispat56.c : Sather class: DISPATCH_TYPEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_();
extern ERR96_error_msg_();
extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr TYP114_name_str_();
extern ptr TYP114_disp_type_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern ptr TYP114_pcopy_();
extern ptr TYP149_dispatched_();
extern ptr TYP114_get_key_();
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



/*constant*/ int DIS56_print_indent_ = 2;
ptr DIS56_create_();
DIS56_out_of_line_();
ptr DIS56_dup_();
DIS56_put_kwdname_();
ptr DIS56_sather_code_();
ptr DIS56_initialize_();
ptr DIS56_pcopy_();
/*constant*/ ptr DIS56_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr DIS56_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr DIS56_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr DIS56_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr DIS56_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr DIS56_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr DIS56_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr DIS56_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr DIS56_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr DIS56_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr DIS56_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr DIS56_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr DIS56_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr DIS56_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr DIS56_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr DIS56_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr DIS56_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr DIS56_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr DIS56_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr DIS56_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr DIS56_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr DIS56_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr DIS56_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr DIS56_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr DIS56_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr DIS56_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr DIS56_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr DIS56_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr DIS56_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr DIS56_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr DIS56_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr DIS56_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr DIS56_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr DIS56_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr DIS56_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr DIS56_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr DIS56_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr DIS56_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr DIS56_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr DIS56_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr DIS56_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr DIS56_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr DIS56_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr DIS56_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr DIS56_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr DIS56_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr DIS56_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr DIS56_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr DIS56_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr DIS56_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr DIS56_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr DIS56_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr DIS56_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr DIS56_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr DIS56_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr DIS56_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr DIS56_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr DIS56_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr DIS56_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr DIS56_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr DIS56_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr DIS56_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr DIS56_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr DIS56_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr DIS56_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr DIS56_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr DIS56_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr DIS56_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr DIS56_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr DIS56_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr DIS56_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr DIS56_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr DIS56_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr DIS56_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr DIS56_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr DIS56_out_classname_ = (ptr)(&gs58_);
/*constant*/ int DIS56_and_kw_ind_ = 1;
/*constant*/ int DIS56_assert_kw_ind_ = 2;
/*constant*/ int DIS56_break_kw_ind_ = 3;
/*constant*/ int DIS56_class_kw_ind_ = 4;
/*constant*/ int DIS56_constant_kw_ind_ = 5;
/*constant*/ int DIS56_debug_kw_ind_ = 6;
/*constant*/ int DIS56_else_kw_ind_ = 7;
/*constant*/ int DIS56_elsif_kw_ind_ = 8;
/*constant*/ int DIS56_end_kw_ind_ = 9;
/*constant*/ int DIS56_if_kw_ind_ = 10;
/*constant*/ int DIS56_inline_kw_ind_ = 11;
/*constant*/ int DIS56_is_kw_ind_ = 12;
/*constant*/ int DIS56_loop_kw_ind_ = 13;
/*constant*/ int DIS56_not_kw_ind_ = 14;
/*constant*/ int DIS56_or_kw_ind_ = 15;
/*constant*/ int DIS56_private_kw_ind_ = 16;
/*constant*/ int DIS56_return_kw_ind_ = 17;
/*constant*/ int DIS56_shared_kw_ind_ = 18;
/*constant*/ int DIS56_switch_kw_ind_ = 19;
/*constant*/ int DIS56_then_kw_ind_ = 20;
/*constant*/ int DIS56_until_kw_ind_ = 21;
/*constant*/ int DIS56_when_kw_ind_ = 22;
/*constant*/ int DIS56_asize_ind_ = 23;
/*constant*/ int DIS56_asize1_ind_ = 24;
/*constant*/ int DIS56_asize2_ind_ = 25;
/*constant*/ int DIS56_asize3_ind_ = 26;
/*constant*/ int DIS56_asize4_ind_ = 27;
/*constant*/ int DIS56_copy_ind_ = 28;
/*constant*/ int DIS56_deep_copy_ind_ = 29;
/*constant*/ int DIS56_extend_ind_ = 30;
/*constant*/ int DIS56_new_ind_ = 31;
/*constant*/ int DIS56_type_ind_ = 32;
/*constant*/ int DIS56_res_ind_ = 33;
/*constant*/ int DIS56_self_ind_ = 34;
/*constant*/ int DIS56_false_ind_ = 35;
/*constant*/ int DIS56_true_ind_ = 36;
/*constant*/ int DIS56_void_ind_ = 37;
/*constant*/ int DIS56_first_base_class_ind_ = 38;
/*constant*/ int DIS56_array_ind_ = 38;
/*constant*/ int DIS56_array2_ind_ = 39;
/*constant*/ int DIS56_array3_ind_ = 40;
/*constant*/ int DIS56_array4_ind_ = 41;
/*constant*/ int DIS56_bool_ind_ = 42;
/*constant*/ int DIS56_c_ind_ = 43;
/*constant*/ int DIS56_char_ind_ = 44;
/*constant*/ int DIS56_double_ind_ = 45;
/*constant*/ int DIS56_file_ind_ = 46;
/*constant*/ int DIS56_int_ind_ = 47;
/*constant*/ int DIS56_real_ind_ = 48;
/*constant*/ int DIS56_self_type_ind_ = 49;
/*constant*/ int DIS56_str_ind_ = 50;
/*constant*/ int DIS56_str_cursor_ind_ = 51;
/*constant*/ int DIS56_ob_ind_ = 52;
/*constant*/ int DIS56_sys_ind_ = 53;
/*constant*/ int DIS56_fob_ind_ = 54;
/*constant*/ int DIS56_undefine_ind_ = 55;
/*constant*/ int DIS56_err_ind_ = 56;
/*constant*/ int DIS56_in_ind_ = 57;
/*constant*/ int DIS56_out_ind_ = 58;
/*constant*/ int DIS56_sux_ind_ = 59;
/*constant*/ int DIS56_last_base_class_ind_ = 59;
/*constant*/ int DIS56_exception_ind_ = 60;
/*constant*/ int DIS56_init_ind_ = 61;
/*constant*/ int DIS56_alias_kw_ind_ = 62;
/*constant*/ int DIS56_undefine_kw_ind_ = 63;
/*constant*/ int DIS56_require_kw_ind_ = 64;
/*constant*/ int DIS56_ensure_kw_ind_ = 65;
/*constant*/ int DIS56_abstract_kw_ind_ = 66;
/*constant*/ int DIS56_invariant_kw_ind_ = 67;
/*constant*/ int DIS56_raise_kw_ind_ = 68;
/*constant*/ int DIS56_protect_kw_ind_ = 69;
/*constant*/ int DIS56_against_kw_ind_ = 70;
/*constant*/ int DIS56_typecase_kw_ind_ = 71;
/*constant*/ int DIS56_attr_kw_ind_ = 72;
/*constant*/ int DIS56_readonly_kw_ind_ = 73;
/*constant*/ int DIS56_while_kw_ind_ = 74;
/*constant*/ int DIS56_include_kw_ind_ = 75;
/*constant*/ int DIS56_arg_ind_ = 76;
/*constant*/ int DIS56_last_reserved_word_ind_ = 76;
char DIS56_base_classname_p_();
ptr DIS56_disp_type_();
ptr DIS56_name_str_();
ptr DIS56_get_key_();
extern int attr_ent_DIS56[];

ptr DIS56_create_(self__,t__)
ptr self__;
ptr t__;
{
   ptr res__ = 0;
   SATHER_STR_(20,45,ls2691_," (DISPATCH_TYPE) : Erroneous dispatch type \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   ptr gl827_;
   static int gl828_;
   static union dtype_ gl829_;
   int gl40_;
   ptr gl830_;
   static int gl831_;
   static union dtype_ gl832_;
   ptr gl41_;
   ptr gl833_;
   static int gl834_;
   static union dtype_ gl835_;
   ptr gl42_;

   gl827_ = t__;
   gl40_ = TYPE_(gl827_);
   if ((gl40_ == 56)) {
      gl830_ = t__;
      cache_dispatch_(gl830_,332,gl831_,INTVAL_(gl832_));
      gl41_ = PFN_(gl832_)(gl830_);
      ERR96_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_i_(STR20_c_(STR20_s_(STR20_create_(0),ERR96_filename_(0)),'('),GLO68_curr_lineno_),')'),(ptr)(&ls2691_)),gl41_),(ptr)(&ls632_)));
      gl833_ = t__;
      cache_dispatch_(gl833_,1672,gl834_,INTVAL_(gl835_));
      gl42_ = PFN_(gl835_)(gl833_);
      res__ = (ptr)DIS56_create_(res__,gl42_);
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)new_(56,0);
   PATT_(res__,12) = (ptr)t__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

DIS56_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr DIS56_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

DIS56_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl836_;
   static int gl837_;
   static union dtype_ gl838_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl836_ = x__;
   cache_dispatch_(gl836_,796,gl837_,INTVAL_(gl838_));
   IATT_(gl836_,INTVAL_(gl838_)) = (int)nm__;

   ret0__:
   return;
}

ptr DIS56_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr DIS56_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr DIS56_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl839_;
   static int gl840_;
   static union dtype_ gl841_;
   ptr gl842_;
   static int gl843_;
   static union dtype_ gl844_;
   ptr gl845_;
   static int gl846_;
   static union dtype_ gl847_;

   gl842_ = PATT_(self__,12);
   cache_dispatch_(gl842_,407,gl843_,INTVAL_(gl844_));
   gl839_ = PFN_(gl844_)(gl842_,pl__,pi__);
   cache_dispatch_(gl839_,889,gl840_,INTVAL_(gl841_));
   res__ = (ptr)PFN_(gl841_)(gl839_);
   gl845_ = res__;
   cache_dispatch_(gl845_,298,gl846_,INTVAL_(gl847_));
   IATT_(gl845_,INTVAL_(gl847_)) = (int)IATT_(self__,4);

   ret0__:
   return (res__);
}

char DIS56_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr DIS56_disp_type_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl848_;
   static int gl849_;
   static union dtype_ gl850_;

   gl848_ = PATT_(self__,12);
   cache_dispatch_(gl848_,1672,gl849_,INTVAL_(gl850_));
   res__ = (ptr)PFN_(gl850_)(gl848_);

   ret0__:
   return (res__);
}

ptr DIS56_name_str_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl851_;
   static int gl852_;
   static union dtype_ gl853_;
   ptr gl43_;

   gl851_ = PATT_(self__,12);
   cache_dispatch_(gl851_,332,gl852_,INTVAL_(gl853_));
   gl43_ = PFN_(gl853_)(gl851_);
   res__ = (ptr)STR20_s_(STR20_c_(STR20_create_(0),'$'),gl43_);

   ret0__:
   return (res__);
}

ptr DIS56_get_key_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl854_;
   static int gl855_;
   static union dtype_ gl856_;

   gl854_ = PATT_(self__,12);
   cache_dispatch_(gl854_,2669,gl855_,INTVAL_(gl856_));
   res__ = (ptr)PFN_(gl856_)(gl854_,pl__,pi__);

   ret0__:
   return (res__);
}


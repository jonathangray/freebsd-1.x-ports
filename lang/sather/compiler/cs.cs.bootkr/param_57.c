/* param_57.c : Sather class: PARAM_TYPEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr CLA66_get_obj_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ int GLO68_curr_lineno_;
extern int STR69_insert_str_();
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_class_defs_;
extern ptr LST147_create_();
extern char STR20_is_upper_case_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_to_upper_case_();
extern ptr INS150_create_();
extern ptr INS150_dispatched_();
extern ptr LST147_push_();
extern ptr CLA148_create_();
extern ptr CLA93_get_obj_();
extern ptr ERR96_filename_();
extern ERR96_format_error_msg_file_();
extern int ERR96_out_of_line_err_info_();
extern /*constant*/ int RES97_OB_ici_;
extern ERR96_compiler_error_msg_();
extern OLD101_install_new_classob_s_();
extern ptr LST102_create_();
extern ptr LST102_push_();
extern ptr TYP114_name_str_();
extern ptr TYP114_pcopy_();
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



/*constant*/ int PAR57_print_indent_ = 2;
ptr PAR57_create_();
PAR57_out_of_line_();
ptr PAR57_dup_();
PAR57_put_kwdname_();
ptr PAR57_sather_code_();
ptr PAR57_initialize_();
ptr PAR57_pcopy_();
/*constant*/ ptr PAR57_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr PAR57_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr PAR57_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr PAR57_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr PAR57_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr PAR57_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr PAR57_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr PAR57_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr PAR57_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr PAR57_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr PAR57_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr PAR57_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr PAR57_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr PAR57_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr PAR57_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr PAR57_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr PAR57_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr PAR57_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr PAR57_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr PAR57_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr PAR57_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr PAR57_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr PAR57_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr PAR57_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr PAR57_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr PAR57_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr PAR57_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr PAR57_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr PAR57_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr PAR57_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr PAR57_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr PAR57_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr PAR57_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr PAR57_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr PAR57_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr PAR57_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr PAR57_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr PAR57_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr PAR57_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr PAR57_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr PAR57_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr PAR57_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr PAR57_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr PAR57_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr PAR57_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr PAR57_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr PAR57_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr PAR57_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr PAR57_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr PAR57_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr PAR57_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr PAR57_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr PAR57_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr PAR57_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr PAR57_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr PAR57_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr PAR57_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr PAR57_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr PAR57_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr PAR57_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr PAR57_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr PAR57_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr PAR57_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr PAR57_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr PAR57_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr PAR57_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr PAR57_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr PAR57_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr PAR57_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr PAR57_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr PAR57_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr PAR57_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr PAR57_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr PAR57_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr PAR57_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr PAR57_out_classname_ = (ptr)(&gs58_);
/*constant*/ int PAR57_and_kw_ind_ = 1;
/*constant*/ int PAR57_assert_kw_ind_ = 2;
/*constant*/ int PAR57_break_kw_ind_ = 3;
/*constant*/ int PAR57_class_kw_ind_ = 4;
/*constant*/ int PAR57_constant_kw_ind_ = 5;
/*constant*/ int PAR57_debug_kw_ind_ = 6;
/*constant*/ int PAR57_else_kw_ind_ = 7;
/*constant*/ int PAR57_elsif_kw_ind_ = 8;
/*constant*/ int PAR57_end_kw_ind_ = 9;
/*constant*/ int PAR57_if_kw_ind_ = 10;
/*constant*/ int PAR57_inline_kw_ind_ = 11;
/*constant*/ int PAR57_is_kw_ind_ = 12;
/*constant*/ int PAR57_loop_kw_ind_ = 13;
/*constant*/ int PAR57_not_kw_ind_ = 14;
/*constant*/ int PAR57_or_kw_ind_ = 15;
/*constant*/ int PAR57_private_kw_ind_ = 16;
/*constant*/ int PAR57_return_kw_ind_ = 17;
/*constant*/ int PAR57_shared_kw_ind_ = 18;
/*constant*/ int PAR57_switch_kw_ind_ = 19;
/*constant*/ int PAR57_then_kw_ind_ = 20;
/*constant*/ int PAR57_until_kw_ind_ = 21;
/*constant*/ int PAR57_when_kw_ind_ = 22;
/*constant*/ int PAR57_asize_ind_ = 23;
/*constant*/ int PAR57_asize1_ind_ = 24;
/*constant*/ int PAR57_asize2_ind_ = 25;
/*constant*/ int PAR57_asize3_ind_ = 26;
/*constant*/ int PAR57_asize4_ind_ = 27;
/*constant*/ int PAR57_copy_ind_ = 28;
/*constant*/ int PAR57_deep_copy_ind_ = 29;
/*constant*/ int PAR57_extend_ind_ = 30;
/*constant*/ int PAR57_new_ind_ = 31;
/*constant*/ int PAR57_type_ind_ = 32;
/*constant*/ int PAR57_res_ind_ = 33;
/*constant*/ int PAR57_self_ind_ = 34;
/*constant*/ int PAR57_false_ind_ = 35;
/*constant*/ int PAR57_true_ind_ = 36;
/*constant*/ int PAR57_void_ind_ = 37;
/*constant*/ int PAR57_first_base_class_ind_ = 38;
/*constant*/ int PAR57_array_ind_ = 38;
/*constant*/ int PAR57_array2_ind_ = 39;
/*constant*/ int PAR57_array3_ind_ = 40;
/*constant*/ int PAR57_array4_ind_ = 41;
/*constant*/ int PAR57_bool_ind_ = 42;
/*constant*/ int PAR57_c_ind_ = 43;
/*constant*/ int PAR57_char_ind_ = 44;
/*constant*/ int PAR57_double_ind_ = 45;
/*constant*/ int PAR57_file_ind_ = 46;
/*constant*/ int PAR57_int_ind_ = 47;
/*constant*/ int PAR57_real_ind_ = 48;
/*constant*/ int PAR57_self_type_ind_ = 49;
/*constant*/ int PAR57_str_ind_ = 50;
/*constant*/ int PAR57_str_cursor_ind_ = 51;
/*constant*/ int PAR57_ob_ind_ = 52;
/*constant*/ int PAR57_sys_ind_ = 53;
/*constant*/ int PAR57_fob_ind_ = 54;
/*constant*/ int PAR57_undefine_ind_ = 55;
/*constant*/ int PAR57_err_ind_ = 56;
/*constant*/ int PAR57_in_ind_ = 57;
/*constant*/ int PAR57_out_ind_ = 58;
/*constant*/ int PAR57_sux_ind_ = 59;
/*constant*/ int PAR57_last_base_class_ind_ = 59;
/*constant*/ int PAR57_exception_ind_ = 60;
/*constant*/ int PAR57_init_ind_ = 61;
/*constant*/ int PAR57_alias_kw_ind_ = 62;
/*constant*/ int PAR57_undefine_kw_ind_ = 63;
/*constant*/ int PAR57_require_kw_ind_ = 64;
/*constant*/ int PAR57_ensure_kw_ind_ = 65;
/*constant*/ int PAR57_abstract_kw_ind_ = 66;
/*constant*/ int PAR57_invariant_kw_ind_ = 67;
/*constant*/ int PAR57_raise_kw_ind_ = 68;
/*constant*/ int PAR57_protect_kw_ind_ = 69;
/*constant*/ int PAR57_against_kw_ind_ = 70;
/*constant*/ int PAR57_typecase_kw_ind_ = 71;
/*constant*/ int PAR57_attr_kw_ind_ = 72;
/*constant*/ int PAR57_readonly_kw_ind_ = 73;
/*constant*/ int PAR57_while_kw_ind_ = 74;
/*constant*/ int PAR57_include_kw_ind_ = 75;
/*constant*/ int PAR57_arg_ind_ = 76;
/*constant*/ int PAR57_last_reserved_word_ind_ = 76;
char PAR57_base_classname_p_();
ptr PAR57_disp_type_();
ptr PAR57_name_str_();
ptr PAR57_get_key_();
char PAR57_install_param_inst_();
extern int attr_ent_PAR57[];

ptr PAR57_create_(self__,nm__,p__)
ptr self__;
int nm__;
ptr p__;
{
   ptr res__ = 0;
   SATHER_STR_(20,13,ls2680_,"PARAM_TYPEOB");
   SATHER_STR_(20,26,ls2670_,"Uncapitalized type name \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr    new_str__ = 0;

   res__ = (ptr)new_(57,0);
   if ((! STR20_is_upper_case_(STR69_at_index_(GLO68_str_table_,nm__)))) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls2680_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2670_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
      new_str__ = (ptr)STR20_to_upper_case_(copy_(STR69_at_index_(GLO68_str_table_,nm__),1));
      nm__ = (int)STR69_insert_str_(GLO68_str_table_,new_str__);
   }
   else {
   }
   IATT_(res__,12) = (int)nm__;
   PATT_(res__,16) = (ptr)p__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

PAR57_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr PAR57_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

PAR57_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl857_;
   static int gl858_;
   static union dtype_ gl859_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl857_ = x__;
   cache_dispatch_(gl857_,796,gl858_,INTVAL_(gl859_));
   IATT_(gl857_,INTVAL_(gl859_)) = (int)nm__;

   ret0__:
   return;
}

ptr PAR57_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr PAR57_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr PAR57_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr    k__ = 0;
   ptr    inst__ = 0;

   k__ = (ptr)PAR57_get_key_(self__,pl__,pi__);
   if ((k__ == 0)) {
      res__ = (ptr)INS150_dispatched_(INS150_create_(0,RES97_OB_ici_,IATT_(self__,4)));
   }
   else {
      inst__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,k__);
      res__ = (ptr)INS150_create_(0,IATT_(inst__,20),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}

char PAR57_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr PAR57_disp_type_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr PAR57_name_str_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls2683_,"{");
   SATHER_STR_(20,2,ls659_,",");
   SATHER_STR_(20,2,ls2684_,"}");
   ptr gl860_;
   static int gl861_;
   static union dtype_ gl862_;
   ptr gl44_;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;

   res__ = (ptr)STR20_s_(copy_(STR69_at_index_(GLO68_str_table_,IATT_(self__,12)),1),(ptr)(&ls2683_));
   i__ = (int)0;
   psz__ = S_int_VOID_;
   if ((PATT_(self__,16) != 0)) {
      psz__ = (int)IATT_(PATT_(self__,16),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_863_;
      }
      else {
      }
      gl860_ = PATT_(PATT_(self__,16), 24 + ((i__) << 2));
      cache_dispatch_(gl860_,332,gl861_,INTVAL_(gl862_));
      gl44_ = PFN_(gl862_)(gl860_);
      res__ = (ptr)STR20_s_(res__,gl44_);
      if ((! (i__ >= psz__))) {
         res__ = (ptr)STR20_s_(res__,(ptr)(&ls659_));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_863_: ;
   res__ = (ptr)STR20_s_(res__,(ptr)(&ls2684_));

   ret0__:
   return (res__);
}

ptr PAR57_get_key_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   SATHER_STR_(20,13,ls2680_,"PARAM_TYPEOB");
   SATHER_STR_(20,22,ls2685_,"Missing instantiation");
   SATHER_STR_(20,13,ls2687_,"\"pcopy\" fail");
   ptr gl864_;
   static int gl865_;
   static union dtype_ gl866_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    localpi__ = 0;
   ptr    par__ = 0;
   ptr    inst__ = 0;

   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,16) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,16),12);
   }
   else {
   }
   if ((sz__ <= 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls2680_),(ptr)(&ls2685_));
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)LST147_push_(LST147_create_(res__,(1 + sz__)),IATT_(self__,12));
   localpi__ = (ptr)LST102_create_(localpi__,(1 + sz__));
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_867_;
      }
      else {
      }
      gl864_ = PATT_(PATT_(self__,16), 24 + ((i__) << 2));
      cache_dispatch_(gl864_,407,gl865_,INTVAL_(gl866_));
      par__ = (ptr)PFN_(gl866_)(gl864_,pl__,pi__);
      if ((par__ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls2680_),(ptr)(&ls2687_));
      }
      else {
         localpi__ = (ptr)LST102_push_(localpi__,par__);
      }
      res__ = (ptr)LST147_push_(res__,IATT_(par__,12));
      i__ = (int)(i__ + 1);
   }
goto_tag_867_: ;
   inst__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,res__);
   if ((inst__ != 0)) {
   }
   else {
      if (PAR57_install_param_inst_(self__,res__,localpi__)) {
      }
      else {
         res__ = (ptr)0;
      }
   }

   ret0__:
   return (res__);
}

char PAR57_install_param_inst_(self__,k__,pinst__)
ptr self__;
ptr k__;
ptr pinst__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,14,ls2689_,"PARAM_CLASSOB");
   SATHER_STR_(20,17,ls2677_,"Undefined type \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr    def__ = 0;

   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,IATT_(self__,12));
   if ((def__ == 0)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls2689_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2677_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls785_)));
      goto ret0__;
   }
   else {
   }
   OLD101_install_new_classob_s_(0,(- 1),k__,CLA148_create_(0,def__,k__,pinst__));
   res__ = (char)1;

   ret0__:
   return (res__);
}


/* simple55.c : Sather class: SIMPLE_TYPEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr LST147_create_();
extern char STR20_is_upper_case_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_to_upper_case_();
extern ptr INS150_create_();
extern ptr INS150_dispatched_();
extern ptr LST147_push_();
extern ptr CLA148_create_();
extern ptr CLA66_get_obj_();
extern char CLA67_non_param_p_();
extern /*shared*/ int GLO68_curr_lineno_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern int STR69_insert_str_();
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_class_defs_;
extern ptr CLA93_get_obj_();
extern ptr ERR96_filename_();
extern ERR96_format_error_msg_file_();
extern int ERR96_out_of_line_err_info_();
extern /*constant*/ int RES97_OB_ici_;
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
extern /*constant*/ int RES97_SYS_ici_;
extern /*constant*/ int RES97_FOB_ici_;
extern /*constant*/ int RES97_SUX_ici_;
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern OLD101_install_new_classob_s_();
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



/*constant*/ int SIM55_print_indent_ = 2;
ptr SIM55_create_();
SIM55_out_of_line_();
ptr SIM55_dup_();
SIM55_put_kwdname_();
ptr SIM55_sather_code_();
ptr SIM55_initialize_();
ptr SIM55_pcopy_();
/*constant*/ ptr SIM55_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr SIM55_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr SIM55_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr SIM55_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr SIM55_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr SIM55_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr SIM55_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr SIM55_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr SIM55_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr SIM55_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr SIM55_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr SIM55_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr SIM55_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr SIM55_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr SIM55_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr SIM55_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr SIM55_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr SIM55_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr SIM55_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr SIM55_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr SIM55_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr SIM55_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr SIM55_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr SIM55_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr SIM55_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr SIM55_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr SIM55_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr SIM55_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr SIM55_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr SIM55_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr SIM55_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr SIM55_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr SIM55_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr SIM55_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr SIM55_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr SIM55_while_kw_name_ = (ptr)(&gs74_);
/*constant*/ ptr SIM55_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr SIM55_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr SIM55_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr SIM55_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr SIM55_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr SIM55_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr SIM55_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr SIM55_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr SIM55_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr SIM55_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr SIM55_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr SIM55_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr SIM55_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr SIM55_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr SIM55_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr SIM55_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr SIM55_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr SIM55_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr SIM55_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr SIM55_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr SIM55_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr SIM55_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr SIM55_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr SIM55_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr SIM55_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr SIM55_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr SIM55_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr SIM55_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr SIM55_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr SIM55_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr SIM55_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr SIM55_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr SIM55_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr SIM55_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr SIM55_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr SIM55_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr SIM55_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr SIM55_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr SIM55_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr SIM55_out_classname_ = (ptr)(&gs58_);
/*constant*/ int SIM55_and_kw_ind_ = 1;
/*constant*/ int SIM55_assert_kw_ind_ = 2;
/*constant*/ int SIM55_break_kw_ind_ = 3;
/*constant*/ int SIM55_class_kw_ind_ = 4;
/*constant*/ int SIM55_constant_kw_ind_ = 5;
/*constant*/ int SIM55_debug_kw_ind_ = 6;
/*constant*/ int SIM55_else_kw_ind_ = 7;
/*constant*/ int SIM55_elsif_kw_ind_ = 8;
/*constant*/ int SIM55_end_kw_ind_ = 9;
/*constant*/ int SIM55_if_kw_ind_ = 10;
/*constant*/ int SIM55_inline_kw_ind_ = 11;
/*constant*/ int SIM55_is_kw_ind_ = 12;
/*constant*/ int SIM55_loop_kw_ind_ = 13;
/*constant*/ int SIM55_not_kw_ind_ = 14;
/*constant*/ int SIM55_or_kw_ind_ = 15;
/*constant*/ int SIM55_private_kw_ind_ = 16;
/*constant*/ int SIM55_return_kw_ind_ = 17;
/*constant*/ int SIM55_shared_kw_ind_ = 18;
/*constant*/ int SIM55_switch_kw_ind_ = 19;
/*constant*/ int SIM55_then_kw_ind_ = 20;
/*constant*/ int SIM55_until_kw_ind_ = 21;
/*constant*/ int SIM55_when_kw_ind_ = 22;
/*constant*/ int SIM55_asize_ind_ = 23;
/*constant*/ int SIM55_asize1_ind_ = 24;
/*constant*/ int SIM55_asize2_ind_ = 25;
/*constant*/ int SIM55_asize3_ind_ = 26;
/*constant*/ int SIM55_asize4_ind_ = 27;
/*constant*/ int SIM55_copy_ind_ = 28;
/*constant*/ int SIM55_deep_copy_ind_ = 29;
/*constant*/ int SIM55_extend_ind_ = 30;
/*constant*/ int SIM55_new_ind_ = 31;
/*constant*/ int SIM55_type_ind_ = 32;
/*constant*/ int SIM55_res_ind_ = 33;
/*constant*/ int SIM55_self_ind_ = 34;
/*constant*/ int SIM55_false_ind_ = 35;
/*constant*/ int SIM55_true_ind_ = 36;
/*constant*/ int SIM55_void_ind_ = 37;
/*constant*/ int SIM55_first_base_class_ind_ = 38;
/*constant*/ int SIM55_array_ind_ = 38;
/*constant*/ int SIM55_array2_ind_ = 39;
/*constant*/ int SIM55_array3_ind_ = 40;
/*constant*/ int SIM55_array4_ind_ = 41;
/*constant*/ int SIM55_bool_ind_ = 42;
/*constant*/ int SIM55_c_ind_ = 43;
/*constant*/ int SIM55_char_ind_ = 44;
/*constant*/ int SIM55_double_ind_ = 45;
/*constant*/ int SIM55_file_ind_ = 46;
/*constant*/ int SIM55_int_ind_ = 47;
/*constant*/ int SIM55_real_ind_ = 48;
/*constant*/ int SIM55_self_type_ind_ = 49;
/*constant*/ int SIM55_str_ind_ = 50;
/*constant*/ int SIM55_str_cursor_ind_ = 51;
/*constant*/ int SIM55_ob_ind_ = 52;
/*constant*/ int SIM55_sys_ind_ = 53;
/*constant*/ int SIM55_fob_ind_ = 54;
/*constant*/ int SIM55_undefine_ind_ = 55;
/*constant*/ int SIM55_err_ind_ = 56;
/*constant*/ int SIM55_in_ind_ = 57;
/*constant*/ int SIM55_out_ind_ = 58;
/*constant*/ int SIM55_sux_ind_ = 59;
/*constant*/ int SIM55_last_base_class_ind_ = 59;
/*constant*/ int SIM55_exception_ind_ = 60;
/*constant*/ int SIM55_init_ind_ = 61;
/*constant*/ int SIM55_alias_kw_ind_ = 62;
/*constant*/ int SIM55_undefine_kw_ind_ = 63;
/*constant*/ int SIM55_require_kw_ind_ = 64;
/*constant*/ int SIM55_ensure_kw_ind_ = 65;
/*constant*/ int SIM55_abstract_kw_ind_ = 66;
/*constant*/ int SIM55_invariant_kw_ind_ = 67;
/*constant*/ int SIM55_raise_kw_ind_ = 68;
/*constant*/ int SIM55_protect_kw_ind_ = 69;
/*constant*/ int SIM55_against_kw_ind_ = 70;
/*constant*/ int SIM55_typecase_kw_ind_ = 71;
/*constant*/ int SIM55_attr_kw_ind_ = 72;
/*constant*/ int SIM55_readonly_kw_ind_ = 73;
/*constant*/ int SIM55_while_kw_ind_ = 74;
/*constant*/ int SIM55_include_kw_ind_ = 75;
/*constant*/ int SIM55_arg_ind_ = 76;
/*constant*/ int SIM55_last_reserved_word_ind_ = 76;
char SIM55_base_classname_p_();
ptr SIM55_disp_type_();
ptr SIM55_name_str_();
ptr SIM55_get_key_();
char SIM55_install_simple_inst_();
extern int attr_ent_SIM55[];

ptr SIM55_create_(self__,nm__)
ptr self__;
int nm__;
{
   ptr res__ = 0;
   SATHER_STR_(20,14,ls502_,"SIMPLE_TYPEOB");
   SATHER_STR_(20,26,ls2670_,"Uncapitalized type name \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr    new_str__ = 0;

   res__ = (ptr)new_(55,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;
   if ((! STR20_is_upper_case_(STR69_at_index_(GLO68_str_table_,nm__)))) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls502_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2670_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
      new_str__ = (ptr)STR20_to_upper_case_(copy_(STR69_at_index_(GLO68_str_table_,nm__),1));
      nm__ = (int)STR69_insert_str_(GLO68_str_table_,new_str__);
   }
   else {
   }
   IATT_(res__,12) = (int)nm__;

   ret0__:
   return (res__);
}

SIM55_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr SIM55_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

SIM55_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl817_;
   static int gl818_;
   static union dtype_ gl819_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl817_ = x__;
   cache_dispatch_(gl817_,796,gl818_,INTVAL_(gl819_));
   IATT_(gl817_,INTVAL_(gl819_)) = (int)nm__;

   ret0__:
   return;
}

ptr SIM55_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SIM55_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr SIM55_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   SATHER_STR_(20,14,ls502_,"SIMPLE_TYPEOB");
   SATHER_STR_(20,28,ls2673_,"Unparametrized use of ARRAY");
   SATHER_STR_(20,29,ls2674_,"Unparametrized use of ARRAY2");
   SATHER_STR_(20,29,ls2675_,"Unparametrized use of ARRAY3");
   SATHER_STR_(20,29,ls2676_,"Unparametrized use of ARRAY4");
   ptr gl820_;
   static int gl821_;
   static union dtype_ gl822_;
   ptr gl823_;
   static int gl824_;
   static union dtype_ gl825_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    key__ = 0;
   ptr    inst__ = 0;

   i__ = (int)0;
   if ((pl__ != 0)) {
      sz__ = (int)IATT_(pl__,4);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_826_;
         }
         else {
         }
         if ((IATT_(self__,12) == IATT_(pl__, 16 + ((i__) << 2)))) {
            gl820_ = PATT_(pi__, 28 + ((i__) << 2));
            res__ = (ptr)copy_(gl820_,atomic_p_(gl820_));
            gl823_ = res__;
            cache_dispatch_(gl823_,298,gl824_,INTVAL_(gl825_));
            IATT_(gl823_,INTVAL_(gl825_)) = (int)IATT_(self__,4);
            goto ret0__;
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_826_: ;
   }
   else {
   }
   switch (IATT_(self__,12)) {
      case (52) :
         res__ = (ptr)INS150_dispatched_(INS150_create_(0,RES97_OB_ici_,IATT_(self__,4)));
         break;
      case (38) :
         ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_create_(0),(ptr)(&ls2673_)));
         res__ = (ptr)INS150_create_(0,RES97_ARRAY_ici_,IATT_(self__,4));
         break;
      case (39) :
         ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_create_(0),(ptr)(&ls2674_)));
         res__ = (ptr)INS150_create_(0,RES97_ARRAY2_ici_,IATT_(self__,4));
         break;
      case (40) :
         ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_create_(0),(ptr)(&ls2675_)));
         res__ = (ptr)INS150_create_(0,RES97_ARRAY3_ici_,IATT_(self__,4));
         break;
      case (41) :
         ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_create_(0),(ptr)(&ls2676_)));
         res__ = (ptr)INS150_create_(0,RES97_ARRAY4_ici_,IATT_(self__,4));
         break;
      case (42) :
         res__ = (ptr)INS150_create_(0,RES97_BOOL_ici_,IATT_(self__,4));
         break;
      case (43) :
         res__ = (ptr)INS150_create_(0,RES97_C_ici_,IATT_(self__,4));
         break;
      case (44) :
         res__ = (ptr)INS150_create_(0,RES97_CHAR_ici_,IATT_(self__,4));
         break;
      case (45) :
         res__ = (ptr)INS150_create_(0,RES97_DOUBLE_ici_,IATT_(self__,4));
         break;
      case (56) :
         res__ = (ptr)INS150_create_(0,RES97_ERR_ici_,IATT_(self__,4));
         break;
      case (46) :
         res__ = (ptr)INS150_create_(0,RES97_FILE_ici_,IATT_(self__,4));
         break;
      case (57) :
         res__ = (ptr)INS150_create_(0,RES97_IN_ici_,IATT_(self__,4));
         break;
      case (47) :
         res__ = (ptr)INS150_create_(0,RES97_INT_ici_,IATT_(self__,4));
         break;
      case (58) :
         res__ = (ptr)INS150_create_(0,RES97_OUT_ici_,IATT_(self__,4));
         break;
      case (48) :
         res__ = (ptr)INS150_create_(0,RES97_REAL_ici_,IATT_(self__,4));
         break;
      case (49) :
         res__ = (ptr)INS150_create_(0,RES97_SELF_TYPE_ici_,IATT_(self__,4));
         break;
      case (50) :
         res__ = (ptr)INS150_create_(0,RES97_STR_ici_,IATT_(self__,4));
         break;
      case (51) :
         res__ = (ptr)INS150_create_(0,RES97_STR_CURSOR_ici_,IATT_(self__,4));
         break;
      case (53) :
         res__ = (ptr)INS150_create_(0,RES97_SYS_ici_,IATT_(self__,4));
         break;
      case (54) :
         res__ = (ptr)INS150_create_(0,RES97_FOB_ici_,IATT_(self__,4));
         break;
      case (59) :
         res__ = (ptr)INS150_create_(0,RES97_SUX_ici_,IATT_(self__,4));
         break;
      case (55) :
         res__ = (ptr)INS150_create_(0,RES97_UNDEFINE_ici_,IATT_(self__,4));
         break;
      default:
         key__ = (ptr)SIM55_get_key_(self__,pl__,pi__);
         if ((key__ == 0)) {
            res__ = (ptr)INS150_dispatched_(INS150_create_(0,RES97_OB_ici_,IATT_(self__,4)));
         }
         else {
            inst__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,key__);
            res__ = (ptr)INS150_create_(0,IATT_(inst__,20),IATT_(self__,4));
         }
         ;
   }

   ret0__:
   return (res__);
}

char SIM55_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

ptr SIM55_disp_type_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr SIM55_name_str_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR69_at_index_(GLO68_str_table_,IATT_(self__,12));

   ret0__:
   return (res__);
}

ptr SIM55_get_key_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr    inst__ = 0;

   res__ = (ptr)LST147_push_(LST147_create_(res__,1),IATT_(self__,12));
   inst__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,res__);
   if ((inst__ != 0)) {
   }
   else {
      if (SIM55_install_simple_inst_(self__,res__)) {
      }
      else {
         res__ = (ptr)0;
      }
   }

   ret0__:
   return (res__);
}

char SIM55_install_simple_inst_(self__,k__)
ptr self__;
ptr k__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,14,ls502_,"SIMPLE_TYPEOB");
   SATHER_STR_(20,17,ls2677_,"Undefined type \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,21,ls2678_,"Type specification \"");
   SATHER_STR_(20,25,ls2679_,"\" expects instantiations");
   ptr    def__ = 0;

   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,IATT_(self__,12));
   if ((def__ == 0)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2677_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls785_)));
      goto ret0__;
   }
   else {
   }
   if (CLA67_non_param_p_(def__)) {
      OLD101_install_new_classob_s_(0,(- 1),k__,CLA148_create_(0,def__,k__,0));
      res__ = (char)1;
   }
   else {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,4),(ptr)(&ls502_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2678_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls2679_)));
      goto ret0__;
   }

   ret0__:
   return (res__);
}


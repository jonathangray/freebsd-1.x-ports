/* sather95.c : Sather class: SATHER_SEMANT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ int omerrs;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);

extern ptr ERR7_s_(ptr self__, ptr st__);
extern ptr OUT9_s_(ptr self__, ptr st__);
extern ptr OUT9_nl_(ptr self__);
extern ptr OUT9_i_(ptr self__, int in__);
extern ptr OUT9_c_(ptr self__, char ch__);
extern int INT15_bit_and_(int self__, int i__);
extern int INT15_rshift_(int self__, int i__);
extern ptr BOO16_to_s_(char self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern /*shared*/ ptr CS_23_outfile_;
extern ptr LST43_create_(ptr self__, int init_size__);
extern ptr CLA66_get_obj_(ptr self__, int nm__);
extern ptr CLA67_create_(ptr self__, int index__, int lno__, ptr pl__, ptr fl__, char ab__);
extern /*shared*/ ptr GLO68_class_defs_;
extern void CLA66_install_(ptr self__, ptr new_def__);
extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ ptr GLO68_class_stat_tbl_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_creator_classob_;
extern /*shared*/ ptr GLO68_creator_classob_s_;
extern /*shared*/ char GLO68_print_feat_info_;
extern /*shared*/ ptr GLO68_dot_sather_reader_;
extern int STR69_insert_str_(ptr self__, ptr s__);
extern int STR78_get_(ptr self__, ptr s__);
extern /*shared*/ ptr GLO68_features_called_from_c_;
extern /*shared*/ ptr GLO68_name_mappings_;
extern /*constant*/ int COM84_c_name_ind_;
extern void STR78_insert_(ptr self__, ptr s__, int v__);
extern /*shared*/ int GLO68_creator_class_name_;
extern /*shared*/ ptr GLO68_main_rout_;
extern /*constant*/ ptr INS88_main_rout_name_;
extern /*shared*/ char COM82_new_compilation_;
extern /*shared*/ ptr COM82_target_dir_;
extern /*constant*/ ptr INS88_test_command_;
extern int UNI90_system_(ptr self__, ptr com__);
extern void CLA93_expand_(ptr self__, int size__);
extern ptr CLA93_get_obj_(ptr self__, ptr k__);
extern ptr CLA93_at_index_(ptr self__, int i__);
extern void ERR96_warning_msg_(ptr self__, ptr s__);
extern /*constant*/ int RES97_OB_ici_;
extern ptr LIS98_create_(ptr self__, int init_size__);
extern ptr LIS98_push_(ptr self__, int e__);
extern /*constant*/ int RES97_ARRAY_ici_;
extern void OLD101_install_new_classob_s_(ptr self__, int loc__, ptr k__, ptr co__);
extern ptr LST102_create_(ptr self__, int init_size__);
extern ptr LST102_push_(ptr self__, ptr e__);
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
extern /*constant*/ int RES97_LAST_PREDEF_ici_;
extern void ERR96_error_exit_(ptr self__, ptr s__);
extern int LIS98_contains_(ptr self__, int e__);
extern char GLO94_handle_class_p_(ptr self__, ptr co__);
extern ptr GLO94_extract_poss_names_(ptr self__, ptr s__);
extern ptr GLO94_analyze_class_name_(ptr self__, ptr str__, int start_index__);
extern int GLO94_key_of_class_feat_(ptr self__, int cnm__, int feat__);
extern int GLO94_classind_from_key_(ptr self__, int k__);
extern char OLD101_updated_p_(ptr self__, ptr co__);
extern int UNI90_rename_(ptr self__, ptr from__, ptr to__);
extern void SAT99_open_for_write_(ptr self__, ptr nm__);
extern int SAT99_error_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern void SAT99_close_(ptr self__);
extern /*constant*/ ptr INS88_cmp_command_;
extern int UNI90_unlink_(ptr self__, ptr name__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern /*shared*/ char COM82_dbg_mode_;
extern /*shared*/ char GLO68_self_exists_;
extern /*shared*/ char COM82_gen_base_;
extern /*shared*/ char COM82_rt_code_check_;
extern /*shared*/ ptr GLO68_precomp_dir_;
extern ptr LST147_create_(ptr self__, int init_size__);
extern ptr LST147_push_(ptr self__, int e__);
extern ptr CLA148_create_(ptr self__, ptr d__, ptr k__, ptr pi__);
extern ptr INS150_create_(ptr self__, int index__, int ln__);
extern ptr INS150_dispatched_(ptr self__);
extern void CLA148_mark_is_used_(ptr self__);
extern ptr CLA148_full_name_(ptr self__);
extern void CLA148_copy_features_(ptr self__);
extern char CLA148_expand_cinh_(ptr self__);
extern void CLA148_resolve_predef_types_and_compute_num_attrs_(ptr self__);
extern int CLA148_compute_pdepth_(ptr self__);
extern void CLA148_compute_anc_(ptr self__);
extern void CLA148_compute_des_(ptr self__);
extern void CLA148_compute_pdes_(ptr self__);
extern void CLA148_adjust_parent_alldes_(ptr self__);
extern void CLA148_compute_time_stamp_(ptr self__);
extern void CLA148_compute_attr_offsets_(ptr self__);
extern char INT164_get_(ptr self__, int k__);
extern ptr CLA148_get_feature_(ptr self__, int index__);
extern ptr INT164_insert_(ptr self__, int k__);
extern void CLA148_pre_semant_(ptr self__);
extern void CLA148_semant_(ptr self__);
extern void CLA148_mark_callees_and_callers_(ptr self__);
extern void CLA148_misc_info_(ptr self__);
extern void CLA148_cprint_header_and_macros_(ptr self__, ptr outfile__);
extern void CLA148_cprint_externs_(ptr self__, ptr outfile__);
extern void CLA148_cprint_ext_strs_(ptr self__, ptr outfile__);
extern void CLA148_cprint_declns_(ptr self__, ptr outfile__);
extern void CLA148_cprint_routines_(ptr self__, ptr outfile__);
extern void CLA161_mark_relevant_(ptr self__, ptr co__);
extern void GLO94_copy_user_cfile_(ptr self__, ptr fn__, ptr outfile__);
extern ptr CLA161_irrelevant_classes_(ptr self__);
extern /*constant*/ ptr INS88_rm_command_;
extern int C_N180_info_size_(ptr self__);
extern ptr C_N180_ith_info_(ptr self__, int i__);
extern /*shared*/ char COM82_do_timing_;
extern /*shared*/ ptr TIM92_rmfiles_com_time_;
extern ptr TIM73_time_syscall_(ptr self__, ptr com__);
extern void GLO94_system_(ptr self__, ptr com__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr INT164_create_(ptr self__);
extern /*shared*/ ptr GLO68_other_cnames_;
extern ptr GEN189_create_(ptr self__, ptr nt__);
extern ptr GEN189_first_(ptr self__);
extern ptr GEN189_item_(ptr self__);
extern ptr GEN189_next_(ptr self__);
extern void DBT190_init_(ptr self__);
extern void DBT190_print_(ptr self__);
extern /*shared*/ ptr GLO68_c_macros_;
extern ptr INT86_get_(ptr self__, int i__);
extern ptr INT164_union_(ptr self__, ptr s__);
extern /*shared*/ char COM82_has_gc_;
extern /*shared*/ char COM82_verbose_code_;
extern /*constant*/ ptr INS88_offset_file_;
extern void CLA148_cprint_attr_tbl_(ptr self__, ptr outfile__);
extern void CLA148_cprint_feat_tbl_(ptr self__, ptr outfile__);
extern /*shared*/ char GLO68_print_des_info_;
extern void CLA148_cprint_des_tbl_(ptr self__, ptr outfile__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_ind_dec_(ptr self__);
extern /*shared*/ int GLO68_dispatch_table_size_;
extern /*shared*/ ptr GLO68_dispatch_flags_;
extern /*shared*/ ptr GLO68_tmpct_;
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern void GLO94_cprint_ctype_name_(ptr self__, int ctype__, ptr outfile__);
extern ptr SAT99_i_(ptr self__, int in__);
extern void INS150_cprint_void_(ptr self__, ptr outfile__);
extern /*constant*/ ptr INS88_c_names_file_;
extern int CLA148_cprint_extern_feats_(ptr self__, ptr outfile__);
extern /*shared*/ ptr GLO68_str_consts_;
extern void STR109_cprint_mach_indep_global_(ptr self__, ptr outfile__);
extern void CLA148_cprint_init_shareds_and_consts_(ptr self__, ptr outfile__);
extern void CLA148_cprint_insert_feats_(ptr self__, ptr outfile__);
extern /*shared*/ ptr GLO68_final_prog_name_;
extern void FEA162_cprint_offset_(ptr self__, ptr outfile__);
extern int FEA162_get_offset_(ptr self__);
extern int GLO94_featname_from_key_(ptr self__, int k__);
extern void ROU156_cprint_cname_(ptr self__, ptr outfile__);
extern void CON151_cprint_cname_(ptr self__, ptr outfile__);
extern void SHA152_cprint_cname_(ptr self__, ptr outfile__);
extern void OUT80_open_for_write_(ptr self__, ptr nm__);
extern int OUT80_error_(ptr self__);
extern ptr OUT80_i_(ptr self__, int in__);
extern ptr OUT80_s_(ptr self__, ptr st__);
extern ptr OUT80_c_(ptr self__, char ch__);
extern void OUT80_close_(ptr self__);
extern int COM178_info_size_(ptr self__);
extern ptr COM178_ith_info_(ptr self__, int i__);
extern /*constant*/ ptr INS88_env_v_sat_home_;
extern /*constant*/ ptr INS88_env_v_environment_;
extern /*shared*/ ptr COM82_sather_home_;
extern ptr SAT99_nl_(ptr self__);
extern /*shared*/ ptr GLO68_cc_flags_;
extern char GLO94_is_clib_option_(ptr self__, ptr s__);
extern /*shared*/ ptr GLO68_clib_options_;
extern /*shared*/ ptr COM82_target_environment_;
extern /*shared*/ ptr COM82_object_files_;
extern /*shared*/ ptr GLO68_c_compiler_;
extern /*constant*/ ptr INS88_c_compiler_;
extern ptr ROU156_typeof_(ptr self__);
extern ptr TYP149_paramstype_(ptr self__);
extern char LST102_conforms_to_(ptr self__, ptr tpp__);
extern ptr GLO94_extract_simple_classnames_(ptr self__, ptr str__, int start_index__);
extern ptr GLO94_union_classnames_(ptr self__, ptr l1__, ptr l2__);
extern char RES71_base_classname_p_(ptr self__, int i__);
extern /*constant*/ int RES71_array_ind_;
extern struct { int tp_; int sz_; char st_; } gs2304_;
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
extern struct { int tp_; int sz_; char st_; } gs2306_;
extern struct { int tp_; int sz_; char st_; } gs2308_;
extern struct { int tp_; int sz_; char st_; } gs1165_;
extern struct { int tp_; int sz_; char st_; } gs1465_;
extern struct { int tp_; int sz_; char st_; } gs2279_;
extern struct { int tp_; int sz_; char st_; } gs2280_;
extern struct { int tp_; int sz_; char st_; } gs2290_;
extern struct { int tp_; int sz_; char st_; } gs2291_;
extern struct { int tp_; int sz_; char st_; } gs2301_;
#include "macros_.h"



/*constant*/ ptr SAT95_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr SAT95_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr SAT95_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr SAT95_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr SAT95_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr SAT95_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr SAT95_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr SAT95_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr SAT95_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr SAT95_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr SAT95_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr SAT95_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr SAT95_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ ptr SAT95_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr SAT95_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr SAT95_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr SAT95_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr SAT95_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr SAT95_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr SAT95_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr SAT95_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr SAT95_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr SAT95_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr SAT95_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr SAT95_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr SAT95_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr SAT95_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr SAT95_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr SAT95_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr SAT95_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr SAT95_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr SAT95_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr SAT95_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr SAT95_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr SAT95_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr SAT95_while_kw_name_ = (ptr)(&gs74_);
ptr SAT95_initialize_(ptr self__, ptr initarg__);
/*constant*/ ptr SAT95_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr SAT95_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr SAT95_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr SAT95_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr SAT95_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr SAT95_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr SAT95_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr SAT95_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr SAT95_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr SAT95_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr SAT95_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr SAT95_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr SAT95_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr SAT95_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr SAT95_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr SAT95_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr SAT95_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr SAT95_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr SAT95_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr SAT95_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr SAT95_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr SAT95_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr SAT95_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr SAT95_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr SAT95_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr SAT95_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr SAT95_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr SAT95_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr SAT95_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr SAT95_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr SAT95_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr SAT95_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr SAT95_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr SAT95_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr SAT95_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr SAT95_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr SAT95_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr SAT95_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr SAT95_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr SAT95_out_classname_ = (ptr)(&gs58_);
/*constant*/ int SAT95_and_kw_ind_ = 1;
/*constant*/ int SAT95_assert_kw_ind_ = 2;
/*constant*/ int SAT95_break_kw_ind_ = 3;
/*constant*/ int SAT95_class_kw_ind_ = 4;
/*constant*/ int SAT95_constant_kw_ind_ = 5;
/*constant*/ int SAT95_debug_kw_ind_ = 6;
/*constant*/ int SAT95_else_kw_ind_ = 7;
/*constant*/ int SAT95_elsif_kw_ind_ = 8;
/*constant*/ int SAT95_end_kw_ind_ = 9;
/*constant*/ int SAT95_if_kw_ind_ = 10;
/*constant*/ int SAT95_inline_kw_ind_ = 11;
/*constant*/ int SAT95_is_kw_ind_ = 12;
/*constant*/ int SAT95_loop_kw_ind_ = 13;
/*constant*/ int SAT95_not_kw_ind_ = 14;
/*constant*/ int SAT95_or_kw_ind_ = 15;
/*constant*/ int SAT95_private_kw_ind_ = 16;
/*constant*/ int SAT95_return_kw_ind_ = 17;
/*constant*/ int SAT95_shared_kw_ind_ = 18;
/*constant*/ int SAT95_switch_kw_ind_ = 19;
/*constant*/ int SAT95_then_kw_ind_ = 20;
/*constant*/ int SAT95_until_kw_ind_ = 21;
/*constant*/ int SAT95_when_kw_ind_ = 22;
/*constant*/ int SAT95_asize_ind_ = 23;
/*constant*/ int SAT95_asize1_ind_ = 24;
/*constant*/ int SAT95_asize2_ind_ = 25;
/*constant*/ int SAT95_asize3_ind_ = 26;
/*constant*/ int SAT95_asize4_ind_ = 27;
/*constant*/ int SAT95_copy_ind_ = 28;
/*constant*/ int SAT95_deep_copy_ind_ = 29;
/*constant*/ int SAT95_extend_ind_ = 30;
/*constant*/ int SAT95_new_ind_ = 31;
/*constant*/ int SAT95_type_ind_ = 32;
/*constant*/ int SAT95_res_ind_ = 33;
/*constant*/ int SAT95_self_ind_ = 34;
/*constant*/ int SAT95_false_ind_ = 35;
/*constant*/ int SAT95_true_ind_ = 36;
/*constant*/ int SAT95_void_ind_ = 37;
/*constant*/ int SAT95_first_base_class_ind_ = 38;
/*constant*/ int SAT95_array_ind_ = 38;
/*constant*/ int SAT95_array2_ind_ = 39;
/*constant*/ int SAT95_array3_ind_ = 40;
/*constant*/ int SAT95_array4_ind_ = 41;
/*constant*/ int SAT95_bool_ind_ = 42;
/*constant*/ int SAT95_c_ind_ = 43;
/*constant*/ int SAT95_char_ind_ = 44;
/*constant*/ int SAT95_double_ind_ = 45;
/*constant*/ int SAT95_file_ind_ = 46;
/*constant*/ int SAT95_int_ind_ = 47;
/*constant*/ int SAT95_real_ind_ = 48;
/*constant*/ int SAT95_self_type_ind_ = 49;
/*constant*/ int SAT95_str_ind_ = 50;
/*constant*/ int SAT95_str_cursor_ind_ = 51;
/*constant*/ int SAT95_ob_ind_ = 52;
/*constant*/ int SAT95_sys_ind_ = 53;
/*constant*/ int SAT95_fob_ind_ = 54;
/*constant*/ int SAT95_undefine_ind_ = 55;
/*constant*/ int SAT95_err_ind_ = 56;
/*constant*/ int SAT95_in_ind_ = 57;
/*constant*/ int SAT95_out_ind_ = 58;
/*constant*/ int SAT95_sux_ind_ = 59;
/*constant*/ int SAT95_last_base_class_ind_ = 59;
/*constant*/ int SAT95_exception_ind_ = 60;
/*constant*/ int SAT95_init_ind_ = 61;
/*constant*/ int SAT95_alias_kw_ind_ = 62;
/*constant*/ int SAT95_undefine_kw_ind_ = 63;
/*constant*/ int SAT95_require_kw_ind_ = 64;
/*constant*/ int SAT95_ensure_kw_ind_ = 65;
/*constant*/ int SAT95_abstract_kw_ind_ = 66;
/*constant*/ int SAT95_invariant_kw_ind_ = 67;
/*constant*/ int SAT95_raise_kw_ind_ = 68;
/*constant*/ int SAT95_protect_kw_ind_ = 69;
/*constant*/ int SAT95_against_kw_ind_ = 70;
/*constant*/ int SAT95_typecase_kw_ind_ = 71;
/*constant*/ int SAT95_attr_kw_ind_ = 72;
/*constant*/ int SAT95_readonly_kw_ind_ = 73;
/*constant*/ int SAT95_while_kw_ind_ = 74;
/*constant*/ int SAT95_include_kw_ind_ = 75;
/*constant*/ int SAT95_arg_ind_ = 76;
/*constant*/ int SAT95_last_reserved_word_ind_ = 76;
char SAT95_base_classname_p_(ptr self__, int i__);
/*shared*/ int SAT95_array_str_ind_;
void SAT95_error_msg_(ptr self__, ptr s__);
void SAT95_error_exit_(ptr self__, ptr s__);
char SAT95_install_predefined_classes_(ptr self__);
int SAT95_install_root_classes_(ptr self__, ptr names__);
void SAT95_all_create_inst_(ptr self__);
void SAT95_all_expand_cinh_(ptr self__);
void SAT95_all_resolve_predef_types_and_compute_num_attrs_(ptr self__, ptr used_classes__);
void SAT95_all_compute_anc_and_des_and_time_stamp_(ptr self__);
void SAT95_print_descendant_information_(ptr self__);
void SAT95_all_compute_attr_offsets_(ptr self__);
void SAT95_find_called_features_(ptr self__);
void SAT95_all_semant_(ptr self__);
void SAT95_final_pass_(ptr self__, ptr co__);
void SAT95_all_class_inst_cprint_(ptr self__);
void SAT95_cprint_macros_(ptr self__, ptr outfile__);
void SAT95_cs_options_cprint_(ptr self__, ptr outfile__);
void SAT95_cprint_class_info_(ptr self__);
void SAT95_cprint_init_(ptr self__);
/*shared*/ int SAT95_total_externs_;
/*shared*/ char SAT95_externs_done_;
void SAT95_cprint_init_externs_(ptr self__);
void SAT95_cprint_init_shareds_and_consts_(ptr self__);
void SAT95_cprint_init_feats_(ptr self__, ptr outfile__);
void SAT95_cprint_rt_static_(ptr self__, ptr outfile__);
void SAT95_print_defines_(ptr self__, ptr outfile__);
void SAT95_print_sather_to_c_macros_(ptr self__);
void SAT95_print_browser_info_(ptr self__);
void SAT95_old_version_print_sather_to_c_macros_(ptr self__);
void SAT95_print_makefile_(ptr self__);
char SAT95_argv_needed_(ptr self__);
char SAT95_old_version_argv_needed_(ptr self__);
ptr SAT95_get_simple_classnames_(ptr self__);
int SAT95_get_array_str_ind_(ptr self__);
void SAT95_print_gen_rt_table_(ptr self__, ptr outfile__, ptr table_prefix__, char cond__);
extern int attr_ent_SAT95[];

ptr SAT95_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char SAT95_base_classname_p_(ptr self__, int i__)
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

void SAT95_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void SAT95_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

char SAT95_install_predefined_classes_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,14,ls2341_,"<NOT IN FILE>");
   SATHER_STR_(20,31,ls2343_,"(WARNING) : Predefined class \"");
   SATHER_STR_(20,13,ls2344_,"\" not found\n");
   ptr    def__ = 0;
   ptr    k__ = 0;
   ptr    pi__ = 0;
   int    max__ = S_int_VOID_;

   def__ = S_ptr_VOID_;
   k__ = S_ptr_VOID_;
   pi__ = S_ptr_VOID_;
   res__ = (char)1;
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,52);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,52,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),52);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_OB_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),52);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_OB_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   pi__ = (ptr)LST102_push_(LST102_create_(pi__,1),INS150_dispatched_(INS150_create_(0,RES97_OB_ici_,(- 1))));
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,38);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,38)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,38,0,LIS98_push_(LIS98_create_(0,1),52),LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),38),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_ARRAY_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),38),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_ARRAY_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,39);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,39)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,39,0,LIS98_push_(LIS98_create_(0,1),52),LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),39),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_ARRAY2_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),39),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_ARRAY2_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,40);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,40)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,40,0,LIS98_push_(LIS98_create_(0,1),52),LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),40),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_ARRAY3_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),40),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_ARRAY3_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,41);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,41)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,41,0,LIS98_push_(LIS98_create_(0,1),52),LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),41),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_ARRAY4_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_push_(LST147_create_(k__,2),41),(- RES97_OB_ici_));
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_ARRAY4_ici_,k__,CLA148_create_(0,def__,k__,pi__));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,42);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,42)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,42,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),42);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_BOOL_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),42);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_BOOL_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,43);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,43)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,43,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),43);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_C_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),43);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_C_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,44);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,44)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,44,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),44);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_CHAR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),44);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_CHAR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,45);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,45)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,45,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),45);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_DOUBLE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),45);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_DOUBLE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,56);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,56)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,56,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),56);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_ERR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),56);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_ERR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,46);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,46)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,46,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),46);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_FILE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),46);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_FILE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,57);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,57)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,57,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),57);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_IN_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),57);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_IN_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,47);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,47)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,47,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),47);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_INT_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),47);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_INT_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,58);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,58)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,58,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),58);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_OUT_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),58);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_OUT_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,48);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,48)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,48,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),48);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_REAL_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),48);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_REAL_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,49);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,49,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),49);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_SELF_TYPE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),49);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_SELF_TYPE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,50);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,50)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,50,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),50);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_STR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),50);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_STR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,51);
   if ((def__ == 0)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2343_)),STR69_at_index_(GLO68_str_table_,51)),(ptr)(&ls2344_)));
      def__ = (ptr)CLA67_create_(0,51,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),51);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_STR_CURSOR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),51);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_STR_CURSOR_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,53);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,53,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),53);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_SYS_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),53);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_SYS_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,54);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,54,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),54);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_FOB_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),54);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_FOB_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,59);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,59,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),59);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_SUX_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),59);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_SUX_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,55);
   if ((def__ == 0)) {
      def__ = (ptr)CLA67_create_(0,55,0,0,LST43_create_(0,1),0);
      CLA66_install_(GLO68_class_defs_,def__);
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),55);
      GLO68_curr_filename_ = (ptr)(ptr)(&ls2341_);
      OLD101_install_new_classob_s_(0,RES97_UNDEFINE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   else {
      k__ = (ptr)LST147_push_(LST147_create_(k__,1),55);
      GLO68_curr_filename_ = (ptr)PATT_(def__,16);
      OLD101_install_new_classob_s_(0,RES97_UNDEFINE_ici_,k__,CLA148_create_(0,def__,k__,0));
   }
   max__ = (int)RES97_LAST_PREDEF_ici_;
   if ((max__ < IATT_(GLO68_class_stat_tbl_,56))) {
      max__ = (int)IATT_(GLO68_class_stat_tbl_,56);
   }
   else {
   }
   CLA93_expand_(GLO68_class_inst_,(max__ + 1));

   ret0__:
   return (res__);
}

int SAT95_install_root_classes_(ptr self__, ptr names__)
{
   int res__ = S_int_VOID_;
   SATHER_STR_(20,13,ls2347_,"Main class \"");
   SATHER_STR_(20,13,ls2344_,"\" not found\n");
   SATHER_STR_(20,34,ls2348_,"(SATHER_SEMANT): Abstract class \"");
   SATHER_STR_(20,22,ls2349_,"\" used as main class\n");
   int    i__ = S_int_VOID_;
   int    nsz__ = S_int_VOID_;
   ptr    def__ = 0;
   ptr    k__ = 0;
   ptr    inst__ = 0;

   i__ = (int)0;
   nsz__ = S_int_VOID_;
   if ((names__ != 0)) {
      nsz__ = (int)IATT_(names__,4);
   }
   else {
   }
   while (1) {
      if ((i__ >= nsz__)) {
         goto goto_tag_1071_;
      }
      else {
      }
      def__ = (ptr)CLA66_get_obj_(GLO68_class_defs_,IATT_(names__, 16 + ((i__) << 2)));
      if ((def__ == 0)) {
         SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2347_)),STR69_at_index_(GLO68_str_table_,IATT_(names__, 16 + ((i__) << 2)))),(ptr)(&ls2344_)));
      }
      else {
         k__ = (ptr)LST147_push_(LST147_create_(k__,1),IATT_(names__, 16 + ((i__) << 2)));
         inst__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,k__);
         if ((inst__ == 0)) {
            inst__ = (ptr)CLA148_create_(0,def__,k__,0);
            GLO68_curr_filename_ = (ptr)PATT_(def__,16);
            GLO68_curr_class_inst_ = (ptr)inst__;
            OLD101_install_new_classob_s_(0,(- 1),k__,inst__);
            CLA148_mark_is_used_(inst__);
            if (CATT_(inst__,4)) {
               ERR96_error_exit_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2348_)),CLA148_full_name_(inst__)),(ptr)(&ls2349_)));
            }
            else {
            }
            res__ = (int)(res__ + 1);
         }
         else {
         }
         if ((def__ == GLO68_creator_classob_)) {
            GLO68_creator_classob_s_ = (ptr)inst__;
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1071_: ;

   ret0__:
   return (res__);
}

void SAT95_all_create_inst_(ptr self__)
{
   SATHER_STR_(20,106,ls2351_,"ERROR: The compiler cannot handle more than 16383 classes.  One solution might be use the \"-ncs\" option.\n");
   char    fixed_pt__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   fixed_pt__ = (char)0;
   while (1) {
      if (fixed_pt__) {
         goto goto_tag_1072_;
      }
      else {
      }
      fixed_pt__ = (char)1;
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1073_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co__ != 0)) {
            GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
            GLO68_curr_class_inst_ = (ptr)co__;
            if ((! CATT_(co__,11))) {
               CLA148_copy_features_(co__);
               CATT_(co__,11) = (char)1;
               fixed_pt__ = (char)0;
            }
            else {
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1073_: ;
   }
goto_tag_1072_: ;
   if ((IATT_(GLO68_class_inst_,24) > 16383)) {
      ERR96_error_exit_(0,(ptr)(&ls2351_));
   }
   else {
   }

   ret0__:
   return;
}

void SAT95_all_expand_cinh_(ptr self__)
{
   SATHER_STR_(20,42,ls2352_," (CLASSOB_S) : Error in inheritance for \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1074_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         if ((! CLA148_expand_cinh_(co__))) {
            SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(PATT_(PATT_(co__,28),16),(ptr)(&ls2352_)),CLA148_full_name_(co__)),(ptr)(&ls632_)));
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1074_: ;

   ret0__:
   return;
}

void SAT95_all_resolve_predef_types_and_compute_num_attrs_(ptr self__, ptr used_classes__)
{
   SATHER_STR_(20,72,ls2355_,"REMINDER: Full type info access via used SYS class requires \"cs -info\"\n");
   char    fixed_pt__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;
   int    i_69_ = S_int_VOID_;
   ptr    co_70_ = 0;
   ptr    sys_class__ = 0;

   fixed_pt__ = (char)0;
   while (1) {
      if (fixed_pt__) {
         goto goto_tag_1075_;
      }
      else {
      }
      fixed_pt__ = (char)1;
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1076_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co__ != 0)) {
            if (((LIS98_contains_(used_classes__,IATT_(PATT_(co__,28),12)) >= 0) | GLO94_handle_class_p_(0,co__))) {
               CLA148_mark_is_used_(co__);
               if ((! CATT_(co__,10))) {
                  CATT_(co__,10) = (char)1;
                  fixed_pt__ = (char)0;
               }
               else {
               }
            }
            else {
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1076_: ;
   }
goto_tag_1075_: ;
   i_69_ = (int)0;
   while (1) {
      if ((i_69_ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1077_;
      }
      else {
      }
      co_70_ = (ptr)CLA93_at_index_(GLO68_class_inst_,i_69_);
      if ((co_70_ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co_70_,28),16);
         GLO68_curr_class_inst_ = (ptr)co_70_;
         CLA148_resolve_predef_types_and_compute_num_attrs_(co_70_);
      }
      else {
      }
      i_69_ = (int)(i_69_ + 1);
   }
goto_tag_1077_: ;
   sys_class__ = (ptr)CLA93_at_index_(GLO68_class_inst_,RES97_SYS_ici_);
   if ((sys_class__ != 0)) {
      if ((CATT_(sys_class__,9) & (! GLO68_print_feat_info_))) {
         (void)ERR7_s_(0,(ptr)(&ls2355_));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

void SAT95_all_compute_anc_and_des_and_time_stamp_(ptr self__)
{
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;
   int    maxd__ = S_int_VOID_;
   int    d__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    j_71_ = S_int_VOID_;

   i__ = (int)0;
   co__ = S_ptr_VOID_;
   maxd__ = S_int_VOID_;
   d__ = S_int_VOID_;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1078_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         d__ = (int)CLA148_compute_pdepth_(co__);
         if ((d__ > maxd__)) {
            maxd__ = (int)d__;
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1078_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1079_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         CLA148_compute_anc_(co__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1079_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1080_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         CLA148_compute_des_(co__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1080_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1081_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         if ((PATT_(co__,76) == 0)) {
            PATT_(co__,76) = (ptr)copy_(PATT_(co__,68),1);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1081_: ;
   i__ = (int)1;
   while (1) {
      if ((i__ > maxd__)) {
         goto goto_tag_1082_;
      }
      else {
      }
      j__ = (int)0;
      while (1) {
         if ((j__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1083_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,j__);
         if ((co__ != 0)) {
            if ((IATT_(co__,80) == i__)) {
               GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
               GLO68_curr_class_inst_ = (ptr)co__;
               CLA148_compute_pdes_(co__);
            }
            else {
            }
         }
         else {
         }
         j__ = (int)(j__ + 1);
      }
   goto_tag_1083_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_1082_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1084_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         CLA148_adjust_parent_alldes_(co__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1084_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ > maxd__)) {
         goto goto_tag_1085_;
      }
      else {
      }
      j_71_ = (int)0;
      while (1) {
         if ((j_71_ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1086_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,j_71_);
         if ((co__ != 0)) {
            if ((IATT_(co__,80) == i__)) {
               GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
               GLO68_curr_class_inst_ = (ptr)co__;
               CLA148_compute_time_stamp_(co__);
            }
            else {
            }
         }
         else {
         }
         j_71_ = (int)(j_71_ + 1);
      }
   goto_tag_1086_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_1085_: ;

   ret0__:
   return;
}

void SAT95_print_descendant_information_(ptr self__)
{
   SATHER_STR_(20,18,ls2358_,"----------------\n");
   SATHER_STR_(20,4,ls2360_,"   ");
   int    i__ = S_int_VOID_;
   ptr    curClassOb__ = 0;
   int    j__ = S_int_VOID_;
   ptr    desClassOb__ = 0;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1087_;
      }
      else {
      }
      curClassOb__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((curClassOb__ != 0)) {
         (void)OUT9_nl_(OUT9_s_(OUT9_s_(0,(ptr)(&ls2358_)),CLA148_full_name_(curClassOb__)));
         if ((PATT_(curClassOb__,76) != 0)) {
            j__ = (int)0;
            while (1) {
               if ((j__ >= IATT_(GLO68_class_inst_,24))) {
                  goto goto_tag_1088_;
               }
               else {
               }
               if (INT164_get_(PATT_(curClassOb__,76),j__)) {
                  desClassOb__ = (ptr)CLA93_at_index_(GLO68_class_inst_,j__);
                  (void)OUT9_nl_(OUT9_s_(OUT9_s_(0,(ptr)(&ls2360_)),CLA148_full_name_(desClassOb__)));
               }
               else {
               }
               j__ = (int)(j__ + 1);
            }
         goto_tag_1088_: ;
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1087_: ;
   (void)OUT9_s_(0,(ptr)(&ls2358_));

   ret0__:
   return;
}

void SAT95_all_compute_attr_offsets_(ptr self__)
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;

   i__ = (int)0;
   sz__ = (int)IATT_(GLO68_class_inst_,24);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1089_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         CLA148_compute_attr_offsets_(co__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1089_: ;

   ret0__:
   return;
}

void SAT95_find_called_features_(ptr self__)
{
   SATHER_STR_(20,29,ls2361_,"Error in handling (c_name) <");
   SATHER_STR_(20,8,ls2362_,"> and <");
   SATHER_STR_(20,3,ls644_,">\n");
   SATHER_STR_(20,40,ls2364_,"Error (c_name) : Cannot analyze class \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,39,ls2366_,"Error (c_name) : Cannot find feature \"");
   SATHER_STR_(20,14,ls2371_,"Class (index=");
   SATHER_STR_(20,46,ls2372_,") from previous compilation no longer exists\n");
   SATHER_STR_(20,41,ls2373_,"Error (c_name) : Invalid ref from C to \"");
   SATHER_STR_(20,29,ls2374_,"\" which is an attribute in \"");
   SATHER_STR_(20,9,ls2375_,"\" class\n");
   SATHER_STR_(20,37,ls2376_,"Error (c_name) : Cannot find class \"");
   SATHER_STR_(20,33,ls2378_,"Error (c_name) : Creator class \"");
   SATHER_STR_(20,13,ls2344_,"\" not found\n");
   SATHER_STR_(20,10,ls2380_,"Error : \"");
   SATHER_STR_(20,28,ls2381_,"\" not defined as a routine\n");
   ptr gl1090_;
   static int gl1091_;
   static union dtype_ gl1092_;
   ptr gl1093_;
   static int gl1094_;
   static union dtype_ gl1095_;
   int gl72_;
   ptr    macros__ = 0;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    info__ = 0;
   ptr    cnm__ = 0;
   ptr    cnm_info__ = 0;
   ptr    co__ = 0;
   int    feat_ind__ = S_int_VOID_;
   ptr    feat__ = 0;
   int    k__ = S_int_VOID_;
   int    old_k__ = S_int_VOID_;
   int    old_ci__ = S_int_VOID_;
   ptr    old_co__ = 0;
   ptr    rout_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    shared_feat__ = 0;
   int    main_ind__ = S_int_VOID_;
   ptr    main_feat__ = 0;
   ptr    tbl__ = 0;
   ptr    tbl_curs__ = 0;
   ptr    elt__ = 0;
   int    index__ = S_int_VOID_;
   int    key__ = S_int_VOID_;
   int    ci__ = S_int_VOID_;
   ptr    co_73_ = 0;
   int    new_ci__ = S_int_VOID_;
   int    ci_74_ = S_int_VOID_;
   ptr    co_75_ = 0;
   ptr    new_co__ = 0;

   macros__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((4) << 2));
   sz__ = (int)C_N180_info_size_(macros__);
   i__ = (int)0;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1096_;
      }
      else {
      }
      info__ = (ptr)C_N180_ith_info_(macros__,i__);
      cnm__ = (ptr)GLO94_extract_poss_names_(0,PATT_(info__, 16 + ((1) << 2)));
      if ((cnm__ == 0)) {
         SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2361_)),PATT_(info__, 16 + ((0) << 2))),(ptr)(&ls2362_)),PATT_(info__, 16 + ((1) << 2))),(ptr)(&ls644_)));
      }
      else {
         cnm_info__ = (ptr)GLO94_analyze_class_name_(0,PATT_(cnm__, 8 + ((0) << 2)),0);
         if ((cnm_info__ == 0)) {
            SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2364_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls632_)));
         }
         else {
            co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(cnm_info__, 16 + ((0) << 2)));
            if ((co__ != 0)) {
               GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
               GLO68_curr_class_inst_ = (ptr)co__;
               feat_ind__ = (int)STR69_insert_str_(GLO68_str_table_,PATT_(cnm__, 8 + ((1) << 2)));
               feat__ = (ptr)CLA148_get_feature_(co__,feat_ind__);
               if ((feat__ == 0)) {
                  SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2366_)),PATT_(cnm__, 8 + ((1) << 2))),(ptr)(&ls632_)));
               }
               else {
                  k__ = (int)GLO94_key_of_class_feat_(0,IATT_(co__,20),feat_ind__);
                  old_k__ = (int)STR78_get_(PATT_(GLO68_class_stat_tbl_,20),PATT_(info__, 16 + ((0) << 2)));
                  if ((old_k__ == 0)) {
                     CATT_(co__,12) = (char)1;
                  }
                  else {
                     if ((old_k__ != k__)) {
                        old_ci__ = (int)GLO94_classind_from_key_(0,old_k__);
                        CATT_(co__,12) = (char)1;
                        if ((old_ci__ != IATT_(co__,20))) {
                           old_co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,old_ci__);
                           if ((old_co__ == 0)) {
                              (void)OUT9_s_(OUT9_i_(OUT9_s_(0,(ptr)(&ls2371_)),old_ci__),(ptr)(&ls2372_));
                           }
                           else {
                              CATT_(old_co__,12) = (char)1;
                           }
                        }
                        else {
                        }
                     }
                     else {
                     }
                  }
                  gl1090_ = feat__;
                  switch (TYPE_(gl1090_)) {
                     case (156) :
                        rout_feat__ = (ptr)feat__;
                        CATT_(rout_feat__,11) = (char)1;
                        GLO68_features_called_from_c_ = (ptr)INT164_insert_(GLO68_features_called_from_c_,k__);
                        STR78_insert_(GLO68_name_mappings_,PATT_(info__, 16 + ((0) << 2)),k__);
                        break;
                     case (151) :
                        const_feat__ = (ptr)feat__;
                        CATT_(const_feat__,11) = (char)1;
                        GLO68_features_called_from_c_ = (ptr)INT164_insert_(GLO68_features_called_from_c_,k__);
                        STR78_insert_(GLO68_name_mappings_,PATT_(info__, 16 + ((0) << 2)),k__);
                        break;
                     case (152) :
                        shared_feat__ = (ptr)feat__;
                        CATT_(shared_feat__,11) = (char)1;
                        GLO68_features_called_from_c_ = (ptr)INT164_insert_(GLO68_features_called_from_c_,k__);
                        STR78_insert_(GLO68_name_mappings_,PATT_(info__, 16 + ((0) << 2)),k__);
                        break;
                     case (153) :
                        SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2373_)),PATT_(cnm__, 8 + ((1) << 2))),(ptr)(&ls2374_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls2375_)));
                        break;
                     default:
                        ;
                        ;
                  }
               }
            }
            else {
               SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2376_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls632_)));
            }
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1096_: ;
   main_ind__ = (int)STR69_insert_str_(GLO68_str_table_,(ptr)(&gs1465_));
   if ((GLO68_creator_classob_s_ == 0)) {
      SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2378_)),STR69_at_index_(GLO68_str_table_,GLO68_creator_class_name_)),(ptr)(&ls2344_)));
   }
   else {
   }
   main_feat__ = (ptr)CLA148_get_feature_(GLO68_creator_classob_s_,main_ind__);
   if ((main_feat__ == 0)) {
      SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2380_)),STR69_at_index_(GLO68_str_table_,main_ind__)),(ptr)(&ls2344_)));
   }
   else {
      gl1093_ = main_feat__;
      gl72_ = TYPE_(gl1093_);
      if ((gl72_ != 156)) {
         SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2380_)),STR69_at_index_(GLO68_str_table_,main_ind__)),(ptr)(&ls2381_)));
      }
      else {
         GLO68_main_rout_ = (ptr)main_feat__;
         CATT_(GLO68_main_rout_,11) = (char)1;
      }
   }
   tbl__ = (ptr)PATT_(PATT_(GLO68_class_stat_tbl_,20),4);
   tbl_curs__ = (ptr)GEN189_create_(tbl_curs__,tbl__);
   (void)GEN189_first_(tbl_curs__);
   while (1) {
      if (CATT_(tbl_curs__,4)) {
         goto goto_tag_1097_;
      }
      else {
      }
      elt__ = (ptr)GEN189_item_(tbl_curs__);
      index__ = (int)STR78_get_(GLO68_name_mappings_,PATT_(elt__,4));
      if ((index__ == 0)) {
         key__ = (int)IATT_(elt__,12);
         ci__ = (int)GLO94_classind_from_key_(0,key__);
         co_73_ = (ptr)CLA93_at_index_(GLO68_class_inst_,ci__);
         CATT_(co_73_,12) = (char)1;
      }
      else {
         if ((index__ != IATT_(elt__,12))) {
            new_ci__ = (int)GLO94_classind_from_key_(0,index__);
            ci_74_ = (int)GLO94_classind_from_key_(0,IATT_(elt__,12));
            co_75_ = (ptr)CLA93_at_index_(GLO68_class_inst_,ci_74_);
            if ((ci_74_ != new_ci__)) {
               new_co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,new_ci__);
               CATT_(new_co__,12) = (char)1;
            }
            else {
            }
            if ((co_75_ == 0)) {
               (void)OUT9_s_(OUT9_i_(OUT9_s_(0,(ptr)(&ls2371_)),ci_74_),(ptr)(&ls2372_));
            }
            else {
               CATT_(co_75_,12) = (char)1;
            }
         }
         else {
         }
      }
      (void)GEN189_next_(tbl_curs__);
   }
goto_tag_1097_: ;

   ret0__:
   return;
}

void SAT95_all_semant_(ptr self__)
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;
   char    fixed_pt__ = S_char_VOID_;
   int    i_76_ = S_int_VOID_;
   int    sz_77_ = S_int_VOID_;
   ptr    co_78_ = 0;
   int    i_79_ = S_int_VOID_;
   int    sz_80_ = S_int_VOID_;
   ptr    co_81_ = 0;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(GLO68_class_inst_,24);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1098_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
         GLO68_curr_class_inst_ = (ptr)co__;
         CLA148_pre_semant_(co__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1098_: ;
   fixed_pt__ = (char)0;
   while (1) {
      if (fixed_pt__) {
         goto goto_tag_1099_;
      }
      else {
      }
      fixed_pt__ = (char)1;
      i_76_ = (int)0;
      sz_77_ = (int)IATT_(GLO68_class_inst_,24);
      while (1) {
         if ((i_76_ >= sz_77_)) {
            goto goto_tag_1100_;
         }
         else {
         }
         co_78_ = (ptr)CLA93_at_index_(GLO68_class_inst_,i_76_);
         if ((co_78_ != 0)) {
            if (GLO94_handle_class_p_(0,co_78_)) {
               if ((! CATT_(co_78_,16))) {
                  GLO68_curr_filename_ = (ptr)PATT_(PATT_(co_78_,28),16);
                  GLO68_curr_class_inst_ = (ptr)co_78_;
                  CLA148_semant_(co_78_);
                  CATT_(co_78_,16) = (char)1;
                  fixed_pt__ = (char)0;
               }
               else {
               }
            }
            else {
            }
         }
         else {
         }
         i_76_ = (int)(i_76_ + 1);
      }
   goto_tag_1100_: ;
   }
goto_tag_1099_: ;
   i_79_ = S_int_VOID_;
   sz_80_ = (int)IATT_(GLO68_class_inst_,24);
   while (1) {
      if ((i_79_ >= sz_80_)) {
         goto goto_tag_1101_;
      }
      else {
      }
      co_81_ = (ptr)CLA93_at_index_(GLO68_class_inst_,i_79_);
      if ((co_81_ != 0)) {
         if (GLO94_handle_class_p_(0,co_81_)) {
            GLO68_curr_filename_ = (ptr)PATT_(PATT_(co_81_,28),16);
            GLO68_curr_class_inst_ = (ptr)co_81_;
            CLA148_mark_callees_and_callers_(co_81_);
         }
         else {
         }
      }
      else {
      }
      i_79_ = (int)(i_79_ + 1);
   }
goto_tag_1101_: ;

   ret0__:
   return;
}

void SAT95_final_pass_(ptr self__, ptr co__)
{
   SATHER_STR_(20,3,ls917_,".c");
   SATHER_STR_(20,3,ls2391_,".C");
   SATHER_STR_(20,3,ls919_,".o");
   SATHER_STR_(20,5,ls2392_," -f ");
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,23,ls2394_,"#include \"macros_.h\"\n\n");
   SATHER_STR_(20,21,ls2395_,"extern int attr_ent_");
   SATHER_STR_(20,6,ls2396_,"[];\n\n");
   SATHER_STR_(20,5,ls2398_," -s ");
   SATHER_STR_(20,27,ls2399_,"** Regenerated C file for ");
   ptr    outfname__ = 0;
   ptr    outf_old__ = 0;
   ptr    outf_obj__ = 0;
   ptr    outfile__ = 0;
   char    changed__ = S_char_VOID_;

   GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
   GLO68_curr_class_inst_ = (ptr)co__;
   CLA148_misc_info_(co__);
   if ((omerrs > 0)) {
      goto ret0__;
   }
   else {
   }
   if ((OLD101_updated_p_(0,co__) | COM82_new_compilation_)) {
      outfname__ = S_ptr_VOID_;
      outf_old__ = S_ptr_VOID_;
      outf_obj__ = S_ptr_VOID_;
      outfname__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),PATT_(co__,92)),(ptr)(&ls917_));
      outf_old__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),PATT_(co__,92)),(ptr)(&ls2391_));
      outf_obj__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),PATT_(co__,92)),(ptr)(&ls919_));
      if ((0 == UNI90_system_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&gs2306_)),(ptr)(&ls2392_)),outfname__)))) {
         (void)UNI90_rename_(0,outfname__,outf_old__);
      }
      else {
      }
      outfile__ = (ptr)new_(99,0);
      SAT99_open_for_write_(outfile__,outfname__);
      if ((SAT99_error_(outfile__) != 0)) {
         SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),outfname__),(ptr)(&ls632_)));
      }
      else {
         IATT_(outfile__,16) = (int)1;
         CLA148_cprint_header_and_macros_(co__,outfile__);
         CLA148_cprint_externs_(co__,outfile__);
         CLA148_cprint_ext_strs_(co__,outfile__);
         if (((((IATT_(co__,20) != RES97_INT_ici_) & (IATT_(co__,20) != RES97_CHAR_ici_)) & (IATT_(co__,20) != RES97_REAL_ici_)) & (IATT_(co__,20) != RES97_DOUBLE_ici_))) {
            outfile__ = (ptr)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2394_)),2);
         }
         else {
         }
         CLA148_cprint_declns_(co__,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2395_)),PATT_(co__,84)),(ptr)(&ls2396_)),2);
         CLA148_cprint_routines_(co__,outfile__);
      }
      SAT99_close_(outfile__);
      changed__ = (char)(INT15_rshift_(INT15_bit_and_(UNI90_system_(0,STR20_s_(STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&gs2308_)),(ptr)(&ls2398_)),outf_old__),' '),outfname__)),0xFF00),8) != 0);
      if (changed__) {
         if ((! COM82_new_compilation_)) {
            (void)OUT9_c_(OUT9_s_(OUT9_s_(0,(ptr)(&ls2399_)),CLA148_full_name_(co__)),'\n');
         }
         else {
         }
         (void)UNI90_unlink_(0,outf_old__);
      }
      else {
         (void)UNI90_rename_(0,outf_old__,outfname__);
      }
   }
   else {
   }

   ret0__:
   return;
}

void SAT95_all_class_inst_cprint_(ptr self__)
{
   SATHER_STR_(20,14,ls625_,"SATHER_SEMANT");
   SATHER_STR_(20,25,ls2400_,"Target directory unknown");
   SATHER_STR_(20,3,ls919_,".o");
   SATHER_STR_(20,5,ls2405_,"(cd ");
   SATHER_STR_(20,12,ls2406_,";\nfor f in ");
   SATHER_STR_(20,21,ls2407_,"Removing old files \"");
   SATHER_STR_(20,10,ls2408_,".o\" and \"");
   SATHER_STR_(20,5,ls2409_,".c\"\n");
   SATHER_STR_(20,4,ls2410_,".o ");
   SATHER_STR_(20,4,ls2411_,".c ");
   SATHER_STR_(20,6,ls2412_,"\ndo\n ");
   SATHER_STR_(20,13,ls2413_," -f $f\ndone)");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;
   ptr    fname__ = 0;
   ptr    delete_classes__ = 0;
   int    len__ = S_int_VOID_;
   ptr    rmfiles_com__ = 0;
   int    i_82_ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(GLO68_class_inst_,24);
   if ((COM82_target_dir_ == 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls625_),(ptr)(&ls2400_));
   }
   else {
   }
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_init_(0);
   }
   else {
   }
   GLO68_self_exists_ = (char)1;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1102_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         if (GLO94_handle_class_p_(0,co__)) {
            CLA161_mark_relevant_(GLO68_class_stat_tbl_,co__);
            if ((((((IATT_(co__,20) == RES97_OB_ici_) | (IATT_(co__,20) == RES97_C_ici_)) | (IATT_(co__,20) == RES97_SELF_TYPE_ici_)) | (IATT_(co__,20) == RES97_FOB_ici_)) | (IATT_(co__,20) == RES97_UNDEFINE_ici_))) {
            }
            else {
               if (((IATT_(co__,20) <= RES97_LAST_PREDEF_ici_) & (IATT_(co__,20) != RES97_SYS_ici_))) {
                  if ((COM82_gen_base_ | COM82_rt_code_check_)) {
                     SAT95_final_pass_(self__,co__);
                  }
                  else {
                     fname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),PATT_(co__,92)),(ptr)(&ls919_));
                     GLO94_copy_user_cfile_(0,STR20_s_(STR20_s_(STR20_create_(0),GLO68_precomp_dir_),fname__),CS_23_outfile_);
                     CLA148_misc_info_(co__);
                  }
               }
               else {
                  SAT95_final_pass_(self__,co__);
               }
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1102_: ;
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_print_(0);
   }
   else {
   }
   delete_classes__ = (ptr)CLA161_irrelevant_classes_(GLO68_class_stat_tbl_);
   len__ = (int)IATT_(delete_classes__,4);
   if ((len__ > 0)) {
      rmfiles_com__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2405_)),COM82_target_dir_),(ptr)(&ls2406_));
      i_82_ = (int)0;
      while (1) {
         if ((i_82_ >= len__)) {
            goto goto_tag_1103_;
         }
         else {
         }
         (void)OUT9_s_(OUT9_s_(OUT9_s_(OUT9_s_(OUT9_s_(0,(ptr)(&ls2407_)),PATT_(delete_classes__, 16 + ((i_82_) << 2))),(ptr)(&ls2408_)),PATT_(delete_classes__, 16 + ((i_82_) << 2))),(ptr)(&ls2409_));
         rmfiles_com__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_s_(rmfiles_com__,PATT_(delete_classes__, 16 + ((i_82_) << 2))),(ptr)(&ls2410_)),PATT_(delete_classes__, 16 + ((i_82_) << 2))),(ptr)(&ls2411_));
         i_82_ = (int)(i_82_ + 1);
      }
   goto_tag_1103_: ;
      rmfiles_com__ = (ptr)STR20_s_(STR20_s_(STR20_s_(rmfiles_com__,(ptr)(&ls2412_)),(ptr)(&gs2304_)),(ptr)(&ls2413_));
      if (COM82_do_timing_) {
         TIM92_rmfiles_com_time_ = (ptr)TIM73_time_syscall_(0,rmfiles_com__);
      }
      else {
         GLO94_system_(0,rmfiles_com__);
      }
   }
   else {
   }

   ret0__:
   return;
}

void SAT95_cprint_macros_(ptr self__, ptr outfile__)
{
   ptr    allmacros__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    ith_cname__ = S_int_VOID_;
   ptr    macros__ = 0;
   int    i_83_ = S_int_VOID_;
   int    sz_84_ = S_int_VOID_;
   int    ith_macro__ = S_int_VOID_;

   (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
   allmacros__ = (ptr)INT164_create_(0);
   i__ = S_int_VOID_;
   sz__ = (int)IATT_(GLO68_other_cnames_,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1104_;
      }
      else {
      }
      ith_cname__ = (int)IATT_(GLO68_other_cnames_, 16 + ((i__) << 2));
      if ((ith_cname__ > 0)) {
         macros__ = (ptr)INT86_get_(GLO68_c_macros_,ith_cname__);
         if ((macros__ != 0)) {
            allmacros__ = (ptr)INT164_union_(allmacros__,macros__);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1104_: ;
   i_83_ = S_int_VOID_;
   sz_84_ = (int)IATT_(allmacros__,12);
   while (1) {
      if ((i_83_ >= sz_84_)) {
         goto goto_tag_1105_;
      }
      else {
      }
      ith_macro__ = (int)IATT_(allmacros__, 16 + ((i_83_) << 2));
      if ((ith_macro__ > 0)) {
         (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,ith_macro__)),'\n'),1);
      }
      else {
      }
      i_83_ = (int)(i_83_ + 1);
   }
goto_tag_1105_: ;
   (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);

   ret0__:
   return;
}

void SAT95_cs_options_cprint_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,5,ls2414_,"dbg=");
   SATHER_STR_(20,6,ls2415_,", gc=");
   SATHER_STR_(20,7,ls2416_,", chk=");

   (void)SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2414_)),BOO16_to_s_(COM82_dbg_mode_)),(ptr)(&ls2415_)),BOO16_to_s_(COM82_has_gc_)),(ptr)(&ls2416_)),BOO16_to_s_(COM82_rt_code_check_));

   ret0__:
   return;
}

void SAT95_cprint_class_info_(ptr self__)
{
   SATHER_STR_(20,12,ls2417_,"CLASINFO_.c");
   SATHER_STR_(20,32,ls2418_,"Error in opening \"CLASINFO_.c\"\n");
   SATHER_STR_(20,11,ls629_,"#include \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,4,ls622_,"/* ");
   SATHER_STR_(20,5,ls2419_," */\n");
   SATHER_STR_(20,24,ls2420_,"int* attr_table_[] = {\n");
   SATHER_STR_(20,10,ls2421_,"attr_ent_");
   SATHER_STR_(20,4,ls693_,"};\n");
   SATHER_STR_(20,5,ls617_,"feat");
   SATHER_STR_(20,4,ls364_,"des");
   SATHER_STR_(20,28,ls2423_,"\n\nvoid main_init_info_()\n{\n");
   SATHER_STR_(20,4,ls2424_,"\n}\n");
   ptr    outfname__ = 0;
   ptr    outfile__ = 0;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;
   ptr    co_85_ = 0;

   outfname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2417_));
   outfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(outfile__,outfname__);
   IATT_(outfile__,16) = (int)1;
   if ((SAT99_error_(outfile__) != 0)) {
      SAT95_error_msg_(self__,(ptr)(&ls2418_));
   }
   else {
      if (COM82_verbose_code_) {
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls629_)),(ptr)(&gs2279_)),(ptr)(&ls632_)),1);
      }
      else {
      }
      (void)SAT99_s_(outfile__,(ptr)(&ls622_));
      SAT95_cs_options_cprint_(self__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2419_)),1);
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1106_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co__ != 0)) {
            if (GLO94_handle_class_p_(0,co__)) {
               if ((((((IATT_(co__,20) != RES97_OB_ici_) & (IATT_(co__,20) != RES97_C_ici_)) & (IATT_(co__,20) != RES97_UNDEFINE_ici_)) & (IATT_(co__,20) != RES97_FOB_ici_)) & (IATT_(co__,20) != RES97_SELF_TYPE_ici_))) {
                  CLA148_cprint_attr_tbl_(co__,outfile__);
                  if (GLO68_print_feat_info_) {
                     CLA148_cprint_feat_tbl_(co__,outfile__);
                  }
                  else {
                  }
                  if (GLO68_print_des_info_) {
                     CLA148_cprint_des_tbl_(co__,outfile__);
                  }
                  else {
                  }
               }
               else {
               }
            }
            else {
               if ((i__ == RES97_ARRAY_ici_)) {
                  CLA148_cprint_attr_tbl_(co__,outfile__);
               }
               else {
               }
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1106_: ;
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2420_)),1);
      (void)SAT99_ind_inc_(outfile__);
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1107_;
         }
         else {
         }
         (void)SAT99_indent_(outfile__);
         co_85_ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co_85_ != 0)) {
            if (GLO94_handle_class_p_(0,co_85_)) {
               if ((((((IATT_(co_85_,20) != RES97_C_ici_) & (IATT_(co_85_,20) != RES97_OB_ici_)) & (IATT_(co_85_,20) != RES97_FOB_ici_)) & (IATT_(co_85_,20) != RES97_UNDEFINE_ici_)) & (IATT_(co_85_,20) != RES97_SELF_TYPE_ici_))) {
                  (void)SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2421_)),PATT_(co_85_,84));
               }
               else {
                  (void)SAT99_c_(outfile__,'0');
               }
            }
            else {
               if ((i__ == RES97_ARRAY_ici_)) {
                  (void)SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2421_)),PATT_(co_85_,84));
               }
               else {
                  (void)SAT99_c_(outfile__,'0');
               }
            }
         }
         else {
            (void)SAT99_c_(outfile__,'0');
         }
         i__ = (int)(i__ + 1);
         if ((i__ < IATT_(GLO68_class_inst_,24))) {
            (void)SAT99_c_(outfile__,',');
         }
         else {
         }
         (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
      }
   goto_tag_1107_: ;
      (void)SAT99_ind_dec_(outfile__);
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls693_)),1);
      SAT95_print_gen_rt_table_(self__,outfile__,(ptr)(&ls617_),GLO68_print_feat_info_);
      SAT95_print_gen_rt_table_(self__,outfile__,(ptr)(&ls364_),GLO68_print_des_info_);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2423_)),4);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2424_)),2);
      SAT99_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_cprint_init_(ptr self__)
{
   SATHER_STR_(20,8,ls2427_,"MAIN_.c");
   SATHER_STR_(20,28,ls2428_,"Error in opening \"MAIN_.c\"\n");
   SATHER_STR_(20,23,ls2429_,"#include \"EXTERNS_.h\"\n");
   SATHER_STR_(20,48,ls2431_,"\n\nextern void main_init_shareds_and_consts_();\n");
   SATHER_STR_(20,31,ls2432_,"extern void main_init_info_();");
   SATHER_STR_(20,50,ls2434_,"\n\nint main_(argc_,argv_)\nint argc_;\nptr argv_;\n{\n");
   SATHER_STR_(20,9,ls2435_,"int i_;\n");
   SATHER_STR_(20,17,ls2436_,"ptr new_argv_;\n\n");
   SATHER_STR_(20,12,ls2165_,"static int ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,21,ls2166_,"static union dtype_ ");
   SATHER_STR_(20,21,ls2437_,"main_init_feats_();\n");
   SATHER_STR_(20,20,ls2438_,"main_init_info_();\n");
   SATHER_STR_(20,34,ls2439_,"main_init_shareds_and_consts_();\n");
   SATHER_STR_(20,42,ls2441_,"new_argv_ = (ptr)calloc((argc_ + 2), 4);\n");
   SATHER_STR_(20,33,ls2443_,"/* Fail to create ARRAY{STR} */\n");
   SATHER_STR_(20,23,ls2444_,"IATT_(new_argv_, 0) = ");
   SATHER_STR_(20,30,ls2445_,"IATT_(new_argv_, 4) = argc_;\n");
   SATHER_STR_(20,34,ls2446_,"for (i_ = 0; i_ < argc_; i_++) {\n");
   SATHER_STR_(20,66,ls2447_,"PATT_(new_argv_, (i_ + 2) * 4) = makestr_(PATT_(argv_, i_ * 4));\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   SATHER_STR_(20,24,ls2449_,"Error : Creator class \"");
   SATHER_STR_(20,13,ls2344_,"\" not found\n");
   SATHER_STR_(20,8,ls2451_,"_main_(");
   SATHER_STR_(20,15,ls2452_,", new_argv_);\n");
   SATHER_STR_(20,11,ls2453_,"return 0;\n");
   SATHER_STR_(20,14,ls2454_,"return ((int)");
   SATHER_STR_(20,16,ls2455_,", new_argv_));\n");
   SATHER_STR_(20,12,ls2456_,"_main_(0);\n");
   SATHER_STR_(20,13,ls2457_,"return (int)");
   int    total__ = S_int_VOID_;
   ptr    outfname__ = 0;
   ptr    outfile__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    tmp_ind__ = S_int_VOID_;
   ptr    creator_type__ = 0;

   SAT95_cprint_init_externs_(self__);
   total__ = (int)SAT95_total_externs_;
   outfname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2427_));
   outfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(outfile__,outfname__);
   IATT_(outfile__,16) = (int)1;
   if ((SAT99_error_(outfile__) != 0)) {
      SAT95_error_msg_(self__,(ptr)(&ls2428_));
   }
   else {
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2429_)),1);
      GLO68_dispatch_table_size_ = (int)(total__ * 4);
      GLO68_dispatch_flags_ = (ptr)new1_(163,GLO68_dispatch_table_size_,1);
      SAT95_cprint_rt_static_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2431_));
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2432_)),3);
      SAT95_cprint_init_feats_(self__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_s_(SAT99_indent_(SAT99_ind_inc_(SAT99_s_(outfile__,(ptr)(&ls2434_)))),(ptr)(&ls2435_))),(ptr)(&ls2436_)),9);
      i__ = S_int_VOID_;
      sz__ = (int)IATT_(GLO68_tmpct_,4);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1108_;
         }
         else {
         }
         (void)SAT99_indent_(outfile__);
         if ((IATT_(GLO68_tmpct_, 16 + ((i__) << 2)) < 0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2165_));
            GLO94_cprint_ctemp_name_(0,IATT_(GLO68_tmpct_, 16 + ((i__) << 2)),outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
            (void)SAT99_indent_(outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls2166_));
            GLO94_cprint_ctemp_name_(0,IATT_(GLO68_tmpct_, 16 + (((i__ + 2)) << 2)),outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
            i__ = (int)(i__ + 4);
         }
         else {
            GLO94_cprint_ctype_name_(0,IATT_(GLO68_tmpct_, 16 + (((i__ + 1)) << 2)),outfile__);
            (void)SAT99_c_(outfile__,' ');
            GLO94_cprint_ctemp_name_(0,IATT_(GLO68_tmpct_, 16 + ((i__) << 2)),outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
            i__ = (int)(i__ + 2);
         }
      }
   goto_tag_1108_: ;
      (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2437_)),1);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2438_)),1);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2439_)),1);
      if (SAT95_argv_needed_(self__)) {
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2441_)),1);
         (void)SAT99_indent_(outfile__);
         tmp_ind__ = (int)SAT95_get_array_str_ind_(self__);
         if ((tmp_ind__ == 0)) {
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2443_)),1);
         }
         else {
            (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2444_)),tmp_ind__),(ptr)(&ls650_)),1);
         }
         (void)SAT99_indent_(outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls2445_));
         (void)SAT99_indent_(outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2446_)),2);
         (void)SAT99_ind_inc_(outfile__);
         (void)SAT99_indent_(outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls2447_));
         (void)SAT99_ind_dec_(outfile__);
         (void)SAT99_indent_(outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2448_)),2);
         (void)SAT99_indent_(outfile__);
         if ((GLO68_creator_classob_s_ == 0)) {
            SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2449_)),STR69_at_index_(GLO68_str_table_,GLO68_creator_class_name_)),(ptr)(&ls2344_)));
         }
         else {
         }
         if ((GLO68_main_rout_ != 0)) {
            creator_type__ = (ptr)INS150_create_(0,IATT_(GLO68_creator_classob_s_,20),(- 1));
            if ((PATT_(GLO68_main_rout_,60) == 0)) {
               (void)SAT99_s_(SAT99_s_(outfile__,PATT_(GLO68_creator_classob_s_,84)),(ptr)(&ls2451_));
               INS150_cprint_void_(creator_type__,outfile__);
               (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2452_)),(ptr)(&ls2453_)),2);
            }
            else {
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2454_)),PATT_(GLO68_creator_classob_s_,84)),(ptr)(&ls2451_));
               INS150_cprint_void_(creator_type__,outfile__);
               (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2455_)),1);
            }
         }
         else {
         }
      }
      else {
         if ((GLO68_creator_classob_s_ == 0)) {
            SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2449_)),STR69_at_index_(GLO68_str_table_,GLO68_creator_class_name_)),(ptr)(&ls2344_)));
         }
         else {
         }
         if ((GLO68_main_rout_ != 0)) {
            if ((PATT_(GLO68_main_rout_,60) == 0)) {
               (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,PATT_(GLO68_creator_classob_s_,84)),(ptr)(&ls2456_)),(ptr)(&ls2453_)),2);
            }
            else {
               (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2457_)),PATT_(GLO68_creator_classob_s_,84)),(ptr)(&ls2456_)),1);
            }
         }
         else {
         }
      }
      (void)SAT99_ind_dec_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2448_)),1);
      SAT99_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_cprint_init_externs_(ptr self__)
{
   SATHER_STR_(20,11,ls2459_,"EXTERNS_.h");
   SATHER_STR_(20,31,ls2460_,"Error in opening \"EXTERNS_.h\"\n");
   SATHER_STR_(20,19,ls628_,"#include \"all_.h\"\n");
   SATHER_STR_(20,11,ls629_,"#include \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,4,ls622_,"/* ");
   SATHER_STR_(20,5,ls2419_," */\n");
   ptr    outfname__ = 0;
   ptr    outfile__ = 0;
   int    total__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;
   ptr    co_86_ = 0;

   if ((! SAT95_externs_done_)) {
      SAT95_externs_done_ = (char)1;
      outfname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2459_));
      outfile__ = (ptr)new_(99,0);
      SAT99_open_for_write_(outfile__,outfname__);
      IATT_(outfile__,16) = (int)1;
      if ((SAT99_error_(outfile__) != 0)) {
         SAT95_error_msg_(self__,(ptr)(&ls2460_));
      }
      else {
         total__ = S_int_VOID_;
         i__ = (int)0;
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls628_)),(ptr)(&ls629_)),(ptr)(&gs2280_)),(ptr)(&ls632_)),2);
         (void)SAT99_s_(outfile__,(ptr)(&ls622_));
         SAT95_cs_options_cprint_(self__,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2419_)),1);
         co__ = S_ptr_VOID_;
         while (1) {
            if ((i__ >= IATT_(GLO68_class_inst_,24))) {
               goto goto_tag_1109_;
            }
            else {
            }
            co_86_ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
            if ((co_86_ != 0)) {
               if (GLO94_handle_class_p_(0,co_86_)) {
                  GLO68_curr_filename_ = (ptr)PATT_(PATT_(co_86_,28),16);
                  GLO68_curr_class_inst_ = (ptr)co_86_;
                  total__ = (int)(total__ + CLA148_cprint_extern_feats_(co_86_,outfile__));
                  (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
               }
               else {
               }
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_1109_: ;
         SAT95_cprint_macros_(self__,outfile__);
         if (COM82_verbose_code_) {
            (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls629_)),(ptr)(&gs2279_)),(ptr)(&ls632_)),1);
         }
         else {
         }
         SAT95_total_externs_ = (int)total__;
      }
   }
   else {
   }

   ret0__:
   return;
}

void SAT95_cprint_init_shareds_and_consts_(ptr self__)
{
   SATHER_STR_(20,9,ls2461_,"SINIT_.c");
   SATHER_STR_(20,29,ls2462_,"Error in opening \"SINIT_.c\"\n");
   SATHER_STR_(20,23,ls2429_,"#include \"EXTERNS_.h\"\n");
   SATHER_STR_(20,42,ls2463_,"\n\nvoid main_init_shareds_and_consts_()\n{\n");
   SATHER_STR_(20,4,ls2424_,"\n}\n");
   ptr    outfname__ = 0;
   ptr    outfile__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    i_87_ = S_int_VOID_;
   ptr    co__ = 0;

   outfname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2461_));
   outfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(outfile__,outfname__);
   IATT_(outfile__,16) = (int)1;
   if ((SAT99_error_(outfile__) != 0)) {
      SAT95_error_msg_(self__,(ptr)(&ls2462_));
   }
   else {
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2429_)),1);
      i__ = S_int_VOID_;
      sz__ = (int)IATT_(GLO68_str_consts_,4);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1110_;
         }
         else {
         }
         STR109_cprint_mach_indep_global_(PATT_(GLO68_str_consts_, 16 + ((i__) << 2)),outfile__);
         i__ = (int)(i__ + 1);
      }
   goto_tag_1110_: ;
      (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
      (void)SAT99_s_(outfile__,(ptr)(&ls2463_));
      i_87_ = (int)0;
      GLO68_self_exists_ = (char)0;
      while (1) {
         if ((i_87_ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1111_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i_87_);
         if ((co__ != 0)) {
            if (GLO94_handle_class_p_(0,co__)) {
               if ((IATT_(co__,20) != RES97_C_ici_)) {
                  GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
                  GLO68_curr_class_inst_ = (ptr)co__;
                  CLA148_cprint_init_shareds_and_consts_(co__,outfile__);
               }
               else {
               }
            }
            else {
            }
         }
         else {
         }
         i_87_ = (int)(i_87_ + 1);
      }
   goto_tag_1111_: ;
      (void)SAT99_ind_dec_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2424_)),2);
      SAT99_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_cprint_init_feats_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,29,ls2464_,"\n\nvoid main_init_feats_()\n{\n");
   SATHER_STR_(20,4,ls2424_,"\n}\n");
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2464_)),4);
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1112_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         if (GLO94_handle_class_p_(0,co__)) {
            GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
            GLO68_curr_class_inst_ = (ptr)co__;
            if ((IATT_(co__,20) != RES97_C_ici_)) {
               CLA148_cprint_insert_feats_(co__,outfile__);
            }
            else {
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1112_: ;
   (void)SAT99_ind_dec_(outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2424_)),2);

   ret0__:
   return;
}

void SAT95_cprint_rt_static_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,19,ls2465_,"ptr prog_name_ = \"");
   SATHER_STR_(20,4,ls667_,"\";\n");
   SATHER_STR_(20,18,ls2466_,"ptr prog_dir_ = \"");
   SATHER_STR_(20,23,ls2467_,"int max_name_index_ = ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,20,ls2468_,"int num_classes_ = ");
   SATHER_STR_(20,28,ls2469_,"int dispatch_table_size_ = ");
   SATHER_STR_(20,21,ls2470_,"int dispatch_table_[");
   SATHER_STR_(20,5,ls2471_,"];\n\n");

   (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2465_)),GLO68_final_prog_name_),(ptr)(&ls667_)),1);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2466_)),COM82_target_dir_),(ptr)(&ls667_)),1);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2467_)),IATT_(GLO68_str_table_,12)),(ptr)(&ls650_)),1);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2468_)),IATT_(GLO68_class_inst_,24)),(ptr)(&ls650_)),1);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2469_)),GLO68_dispatch_table_size_),(ptr)(&ls650_)),1);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2470_)),GLO68_dispatch_table_size_),(ptr)(&ls2471_)),2);

   ret0__:
   return;
}

void SAT95_print_defines_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,9,ls2473_,"#define ");
   ptr gl1113_;
   static int gl1114_;
   static union dtype_ gl1115_;
   int gl88_;
   ptr gl1116_;
   static int gl1117_;
   static union dtype_ gl1118_;
   ptr gl1119_;
   static int gl1120_;
   static union dtype_ gl1121_;
   int gl89_;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;
   ptr    feats__ = 0;
   int    fsz__ = S_int_VOID_;
   int    j__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_1122_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         if (GLO94_handle_class_p_(0,co__)) {
            GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
            GLO68_curr_class_inst_ = (ptr)co__;
            feats__ = (ptr)PATT_(co__,32);
            if ((feats__ != 0)) {
               fsz__ = (int)IATT_(feats__,12);
               j__ = (int)0;
               while (1) {
                  if ((j__ >= fsz__)) {
                     goto goto_tag_1123_;
                  }
                  else {
                  }
                  gl1113_ = PATT_(feats__, 24 + ((j__) << 2));
                  gl88_ = TYPE_(gl1113_);
                  if ((gl88_ == 153)) {
                     (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                     gl1116_ = PATT_(feats__, 24 + ((j__) << 2));
                     cache_dispatch_(gl1116_,662,gl1117_,INTVAL_(gl1118_));
                     VFN_(gl1118_)(gl1116_,outfile__);
                     gl1119_ = PATT_(feats__, 24 + ((j__) << 2));
                     cache_dispatch_(gl1119_,803,gl1120_,INTVAL_(gl1121_));
                     gl89_ = IFN_(gl1121_)(gl1119_);
                     (void)SAT99_inc_ln_(SAT99_c_(SAT99_i_(SAT99_c_(outfile__,' '),gl89_),'\n'),1);
                  }
                  else {
                  }
                  j__ = (int)(j__ + 1);
               }
            goto_tag_1123_: ;
            }
            else {
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1122_: ;

   ret0__:
   return;
}

void SAT95_print_sather_to_c_macros_(ptr self__)
{
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,9,ls2473_,"#define ");
   ptr gl1124_;
   static int gl1125_;
   static union dtype_ gl1126_;
   ptr    map_fname__ = 0;
   ptr    outfile__ = 0;
   ptr    macros__ = 0;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    info__ = 0;
   int    k__ = S_int_VOID_;
   int    feat_ind__ = S_int_VOID_;
   int    class_ind__ = S_int_VOID_;
   ptr    co__ = 0;
   ptr    feat__ = 0;
   ptr    rout_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    shared_feat__ = 0;

   map_fname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2280_));
   outfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(outfile__,map_fname__);
   if ((SAT99_error_(outfile__) != 0)) {
      SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),map_fname__),(ptr)(&ls632_)));
   }
   else {
      macros__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((4) << 2));
      sz__ = (int)C_N180_info_size_(macros__);
      i__ = (int)0;
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1127_;
         }
         else {
         }
         info__ = (ptr)C_N180_ith_info_(macros__,i__);
         k__ = (int)STR78_get_(GLO68_name_mappings_,PATT_(info__, 16 + ((0) << 2)));
         if ((k__ != 0)) {
            feat_ind__ = (int)GLO94_featname_from_key_(0,k__);
            class_ind__ = (int)GLO94_classind_from_key_(0,k__);
            co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,class_ind__);
            feat__ = (ptr)CLA148_get_feature_(co__,feat_ind__);
            gl1124_ = feat__;
            switch (TYPE_(gl1124_)) {
               case (156) :
                  rout_feat__ = (ptr)feat__;
                  (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                  ROU156_cprint_cname_(rout_feat__,outfile__);
                  (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                  break;
               case (151) :
                  const_feat__ = (ptr)feat__;
                  (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                  CON151_cprint_cname_(const_feat__,outfile__);
                  (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                  break;
               case (152) :
                  shared_feat__ = (ptr)feat__;
                  (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                  SHA152_cprint_cname_(shared_feat__,outfile__);
                  (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                  break;
               default:
                  ;
                  ;
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1127_: ;
      SAT99_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_print_browser_info_(ptr self__)
{
   SATHER_STR_(20,5,ls2477_,".sa_");
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,2,ls1512_,"$");
   ptr    info_fname__ = 0;
   ptr    outfile__ = 0;
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   info_fname__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),GLO68_final_prog_name_),(ptr)(&ls2477_));
   outfile__ = (ptr)new_(80,0);
   OUT80_open_for_write_(outfile__,info_fname__);
   if ((OUT80_error_(outfile__) != 0)) {
      SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),info_fname__),(ptr)(&ls632_)));
   }
   else {
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1128_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co__ != 0)) {
            if ((i__ == RES97_OB_ici_)) {
               (void)OUT80_c_(OUT80_s_(OUT80_s_(OUT80_i_(outfile__,i__),(ptr)(&ls1512_)),CLA148_full_name_(co__)),'\n');
            }
            else {
               (void)OUT80_c_(OUT80_s_(OUT80_i_(outfile__,i__),CLA148_full_name_(co__)),'\n');
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1128_: ;
      OUT80_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_old_version_print_sather_to_c_macros_(ptr self__)
{
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,23,ls2479_,"/* Error in handling <");
   SATHER_STR_(20,8,ls2362_,"> and <");
   SATHER_STR_(20,6,ls2480_,"> */\n");
   SATHER_STR_(20,31,ls2481_,"Error : Cannot analyze class \"");
   SATHER_STR_(20,30,ls2482_,"Error : Cannot find feature \"");
   SATHER_STR_(20,9,ls2473_,"#define ");
   SATHER_STR_(20,28,ls2483_,"Error : Cannot find class \"");
   ptr gl1129_;
   static int gl1130_;
   static union dtype_ gl1131_;
   ptr gl1132_;
   static int gl1133_;
   static union dtype_ gl1134_;
   ptr gl1135_;
   static int gl1136_;
   static union dtype_ gl1137_;
   int gl90_;
   ptr gl1138_;
   static int gl1139_;
   static union dtype_ gl1140_;
   int gl91_;
   ptr gl1141_;
   static int gl1142_;
   static union dtype_ gl1143_;
   int gl92_;
   ptr    map_fname__ = 0;
   ptr    outfile__ = 0;
   ptr    macros__ = 0;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    info__ = 0;
   ptr    cnm__ = 0;
   ptr    cnm_info__ = 0;
   ptr    co__ = 0;
   int    feat_ind__ = S_int_VOID_;
   ptr    feat__ = 0;
   ptr    rout_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    shared_feat__ = 0;

   map_fname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2280_));
   outfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(outfile__,map_fname__);
   if ((SAT99_error_(outfile__) != 0)) {
      SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),(ptr)(&gs2280_)),(ptr)(&ls632_)));
   }
   else {
      macros__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((4) << 2));
      gl1129_ = macros__;
      cache_dispatch_(gl1129_,1035,gl1130_,INTVAL_(gl1131_));
      sz__ = (int)IFN_(gl1131_)(gl1129_);
      i__ = (int)0;
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1144_;
         }
         else {
         }
         gl1132_ = macros__;
         cache_dispatch_(gl1132_,1037,gl1133_,INTVAL_(gl1134_));
         info__ = (ptr)PFN_(gl1134_)(gl1132_,i__);
         cnm__ = (ptr)GLO94_extract_poss_names_(0,PATT_(info__, 16 + ((1) << 2)));
         if ((cnm__ == 0)) {
            (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2479_)),PATT_(info__, 16 + ((0) << 2))),(ptr)(&ls2362_)),PATT_(info__, 16 + ((1) << 2))),(ptr)(&ls2480_)),1);
         }
         else {
            cnm_info__ = (ptr)GLO94_analyze_class_name_(0,PATT_(cnm__, 8 + ((0) << 2)),0);
            if ((cnm_info__ == 0)) {
               SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2481_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls632_)));
            }
            else {
               co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(cnm_info__, 16 + ((0) << 2)));
               if ((co__ != 0)) {
                  GLO68_curr_filename_ = (ptr)PATT_(PATT_(co__,28),16);
                  GLO68_curr_class_inst_ = (ptr)co__;
                  feat_ind__ = (int)STR69_insert_str_(GLO68_str_table_,PATT_(cnm__, 8 + ((1) << 2)));
                  feat__ = (ptr)CLA148_get_feature_(co__,feat_ind__);
                  if ((feat__ == 0)) {
                     SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2482_)),PATT_(cnm__, 8 + ((1) << 2))),(ptr)(&ls632_)));
                  }
                  else {
                     gl1135_ = feat__;
                     gl90_ = TYPE_(gl1135_);
                     if ((gl90_ == 156)) {
                        rout_feat__ = (ptr)feat__;
                        (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                        ROU156_cprint_cname_(rout_feat__,outfile__);
                        (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                     }
                     else {
                        gl1138_ = feat__;
                        gl91_ = TYPE_(gl1138_);
                        if ((gl91_ == 151)) {
                           const_feat__ = (ptr)feat__;
                           (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                           CON151_cprint_cname_(const_feat__,outfile__);
                           (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                        }
                        else {
                           gl1141_ = feat__;
                           gl92_ = TYPE_(gl1141_);
                           if ((gl92_ == 152)) {
                              shared_feat__ = (ptr)feat__;
                              (void)SAT99_s_(outfile__,(ptr)(&ls2473_));
                              SHA152_cprint_cname_(shared_feat__,outfile__);
                              (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(SAT99_c_(outfile__,' '),PATT_(info__, 16 + ((0) << 2))),'\n'),1);
                           }
                           else {
                           }
                        }
                     }
                  }
               }
               else {
                  SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2483_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls632_)));
               }
            }
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1144_: ;
      SAT99_close_(outfile__);
   }

   ret0__:
   return;
}

void SAT95_print_makefile_(ptr self__)
{
   SATHER_STR_(20,9,ls2485_,"Makefile");
   SATHER_STR_(20,29,ls2488_,"Error in opening \"Makefile\"\n");
   SATHER_STR_(20,3,ls2489_,"= ");
   SATHER_STR_(20,12,ls2490_,"MAINFLAGS= ");
   SATHER_STR_(20,7,ls2491_,"-DGC_ ");
   SATHER_STR_(20,9,ls2492_,"CFLAGS= ");
   SATHER_STR_(20,5,ls2493_," -g ");
   SATHER_STR_(20,8,ls2494_,"-DGC_ \n");
   SATHER_STR_(20,7,ls2495_,"CLIB= ");
   SATHER_STR_(20,10,ls2496_,"OBJECTS= ");
   SATHER_STR_(20,5,ls2497_,"CC= ");
   SATHER_STR_(20,2,ls780_,"\n");
   SATHER_STR_(20,17,ls2498_,"SATHER_LD= $(CC)");
   SATHER_STR_(20,3,ls652_,"\n\n");
   SATHER_STR_(20,47,ls2499_," \\\nMAIN_.o \\\nSINIT_.o \\\nCLASINFO_.o $(OBJECTS)");
   SATHER_STR_(20,4,ls2500_," \\\n");
   SATHER_STR_(20,3,ls919_,".o");
   SATHER_STR_(20,43,ls2501_,"\t$(SATHER_LD) $(CFLAGS) *.o $(OBJECTS) -o ");
   SATHER_STR_(20,6,ls2502_," -L$(");
   SATHER_STR_(20,9,ls2503_,")/bin.$(");
   SATHER_STR_(20,14,ls2504_,") $(CLIB) -lm");
   SATHER_STR_(20,6,ls2505_," -lgc");
   SATHER_STR_(20,19,ls2506_,"MAIN_.o : MAIN_.c\n");
   SATHER_STR_(20,9,ls2507_,"\t $(CC) ");
   SATHER_STR_(20,14,ls2508_,"$(MAINFLAGS) ");
   SATHER_STR_(20,5,ls2509_,"-I$(");
   SATHER_STR_(20,21,ls2510_,")/sys/C -c MAIN_.c\n\n");
   SATHER_STR_(20,21,ls2511_,"SINIT_.o : SINIT_.c\n");
   SATHER_STR_(20,22,ls2512_,")/sys/C -c SINIT_.c\n\n");
   SATHER_STR_(20,27,ls2513_,"CLASINFO_.o : CLASINFO_.c\n");
   SATHER_STR_(20,25,ls2514_,")/sys/C -c CLASINFO_.c\n\n");
   SATHER_STR_(20,7,ls2515_,".c.o:\n");
   SATHER_STR_(20,22,ls2516_,"\t$(CC) $(CFLAGS) -I$(");
   SATHER_STR_(20,16,ls2517_,")/sys/C -c $<\n\n");
   ptr    mkfile_name__ = 0;
   ptr    mkfile__ = 0;
   ptr    home__ = 0;
   ptr    envi__ = 0;
   int    i__ = S_int_VOID_;
   int    cc_sz__ = S_int_VOID_;
   int    i_93_ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;

   mkfile_name__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2485_));
   mkfile__ = (ptr)new_(99,0);
   SAT99_open_for_write_(mkfile__,mkfile_name__);
   home__ = (ptr)(ptr)(&gs2290_);
   envi__ = (ptr)(ptr)(&gs2291_);
   if ((SAT99_error_(mkfile__) != 0)) {
      SAT95_error_exit_(self__,(ptr)(&ls2488_));
   }
   else {
      (void)SAT99_nl_(SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,home__),(ptr)(&ls2489_)),COM82_sather_home_));
      (void)SAT99_s_(mkfile__,(ptr)(&ls2490_));
      if (COM82_has_gc_) {
         (void)SAT99_s_(mkfile__,(ptr)(&ls2491_));
      }
      else {
      }
      (void)SAT99_nl_(mkfile__);
      (void)SAT99_s_(mkfile__,(ptr)(&ls2492_));
      if ((GLO68_cc_flags_ != 0)) {
         i__ = (int)0;
         cc_sz__ = (int)IATT_(GLO68_cc_flags_,4);
         while (1) {
            if ((i__ >= cc_sz__)) {
               goto goto_tag_1145_;
            }
            else {
            }
            if (GLO94_is_clib_option_(0,PATT_(GLO68_cc_flags_, 8 + ((i__) << 2)))) {
               GLO68_clib_options_ = (ptr)STR20_c_(STR20_s_(GLO68_clib_options_,PATT_(GLO68_cc_flags_, 8 + ((i__) << 2))),' ');
            }
            else {
               (void)SAT99_c_(SAT99_s_(mkfile__,PATT_(GLO68_cc_flags_, 8 + ((i__) << 2))),' ');
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_1145_: ;
      }
      else {
      }
      if (COM82_dbg_mode_) {
         (void)SAT99_s_(mkfile__,(ptr)(&ls2493_));
      }
      else {
      }
      if (COM82_has_gc_) {
         (void)SAT99_s_(mkfile__,(ptr)(&ls2494_));
      }
      else {
         (void)SAT99_c_(mkfile__,'\n');
      }
      (void)SAT99_c_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2495_)),GLO68_clib_options_),'\n');
      (void)SAT99_c_(SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,envi__),(ptr)(&ls2489_)),COM82_target_environment_),'\n');
      (void)SAT99_c_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2496_)),COM82_object_files_),'\n');
      if ((GLO68_c_compiler_ != 0)) {
         (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2497_)),GLO68_c_compiler_),(ptr)(&ls780_));
      }
      else {
         (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2497_)),(ptr)(&gs2301_)),(ptr)(&ls780_));
      }
      (void)SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2498_)),(ptr)(&ls652_));
      (void)SAT99_c_(SAT99_s_(mkfile__,GLO68_final_prog_name_),':');
      (void)SAT99_s_(mkfile__,(ptr)(&ls2499_));
      i_93_ = (int)0;
      sz__ = (int)IATT_(GLO68_class_inst_,24);
      while (1) {
         if ((i_93_ >= sz__)) {
            goto goto_tag_1146_;
         }
         else {
         }
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i_93_);
         if ((co__ != 0)) {
            if (GLO94_handle_class_p_(0,co__)) {
               if ((((((IATT_(co__,20) == RES97_OB_ici_) | (IATT_(co__,20) == RES97_C_ici_)) | (IATT_(co__,20) == RES97_FOB_ici_)) | (IATT_(co__,20) == RES97_SELF_TYPE_ici_)) | (IATT_(co__,20) == RES97_UNDEFINE_ici_))) {
               }
               else {
                  (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2500_)),PATT_(co__,92)),(ptr)(&ls919_));
               }
            }
            else {
            }
         }
         else {
         }
         i_93_ = (int)(i_93_ + 1);
      }
   goto_tag_1146_: ;
      (void)SAT99_nl_(mkfile__);
      (void)SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2501_)),GLO68_final_prog_name_),(ptr)(&ls2502_)),home__),(ptr)(&ls2503_)),envi__),(ptr)(&ls2504_));
      if (COM82_has_gc_) {
         (void)SAT99_s_(mkfile__,(ptr)(&ls2505_));
      }
      else {
      }
      (void)SAT99_s_(mkfile__,(ptr)(&ls652_));
      (void)SAT99_s_(mkfile__,(ptr)(&ls2506_));
      (void)SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2507_)),(ptr)(&ls2508_));
      (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2509_)),home__),(ptr)(&ls2510_));
      (void)SAT99_s_(mkfile__,(ptr)(&ls2511_));
      (void)SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2507_)),(ptr)(&ls2508_));
      (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2509_)),home__),(ptr)(&ls2512_));
      (void)SAT99_s_(mkfile__,(ptr)(&ls2513_));
      (void)SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2507_)),(ptr)(&ls2508_));
      (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2509_)),home__),(ptr)(&ls2514_));
      (void)SAT99_s_(mkfile__,(ptr)(&ls2515_));
      (void)SAT99_s_(SAT99_s_(SAT99_s_(mkfile__,(ptr)(&ls2516_)),home__),(ptr)(&ls2517_));
      SAT99_close_(mkfile__);
   }

   ret0__:
   return;
}

char SAT95_argv_needed_(ptr self__)
{
   char res__ = S_char_VOID_;
   ptr gl1147_;
   static int gl1148_;
   static union dtype_ gl1149_;
   ptr    rout_type__ = 0;
   ptr    param_types__ = 0;

   if ((GLO68_main_rout_ != 0)) {
      rout_type__ = (ptr)ROU156_typeof_(GLO68_main_rout_);
      gl1147_ = rout_type__;
      cache_dispatch_(gl1147_,1709,gl1148_,INTVAL_(gl1149_));
      param_types__ = (ptr)PFN_(gl1149_)(gl1147_);
      if ((param_types__ == 0)) {
         goto ret0__;
      }
      else {
         if ((IATT_(param_types__,16) == 0)) {
            goto ret0__;
         }
         else {
         }
      }
      if (LST102_conforms_to_(LST102_push_(LST102_create_(0,1),INS150_create_(0,SAT95_get_array_str_ind_(self__),(- 1))),param_types__)) {
         res__ = (char)1;
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

char SAT95_old_version_argv_needed_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,5,ls1465_,"main");
   SATHER_STR_(20,24,ls2449_,"Error : Creator class \"");
   SATHER_STR_(20,13,ls2344_,"\" not found\n");
   SATHER_STR_(20,26,ls2521_,"Error : \"main\" not found\n");
   SATHER_STR_(20,41,ls2522_,"Error : \"main\" not defined as a routine\n");
   ptr gl1150_;
   static int gl1151_;
   static union dtype_ gl1152_;
   int gl94_;
   ptr gl1153_;
   static int gl1154_;
   static union dtype_ gl1155_;
   int    main_ind__ = S_int_VOID_;
   ptr    main_feat__ = 0;
   ptr    main_rout__ = 0;
   ptr    rout_type__ = 0;
   ptr    param_types__ = 0;

   main_ind__ = (int)STR69_insert_str_(GLO68_str_table_,(ptr)(&ls1465_));
   if ((GLO68_creator_classob_s_ == 0)) {
      SAT95_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2449_)),STR69_at_index_(GLO68_str_table_,GLO68_creator_class_name_)),(ptr)(&ls2344_)));
   }
   else {
   }
   main_feat__ = (ptr)CLA148_get_feature_(GLO68_creator_classob_s_,main_ind__);
   if ((main_feat__ == 0)) {
      SAT95_error_msg_(self__,(ptr)(&ls2521_));
   }
   else {
      gl1150_ = main_feat__;
      gl94_ = TYPE_(gl1150_);
      if ((gl94_ != 156)) {
         SAT95_error_msg_(self__,(ptr)(&ls2522_));
      }
      else {
         main_rout__ = (ptr)main_feat__;
         CATT_(main_rout__,11) = (char)1;
         rout_type__ = (ptr)ROU156_typeof_(main_rout__);
         gl1153_ = rout_type__;
         cache_dispatch_(gl1153_,1709,gl1154_,INTVAL_(gl1155_));
         param_types__ = (ptr)PFN_(gl1155_)(gl1153_);
         if ((param_types__ == 0)) {
            goto ret0__;
         }
         else {
            if ((IATT_(param_types__,16) == 0)) {
               goto ret0__;
            }
            else {
            }
         }
         if (LST102_conforms_to_(LST102_push_(LST102_create_(0,1),INS150_create_(0,SAT95_get_array_str_ind_(self__),(- 1))),param_types__)) {
            res__ = (char)1;
         }
         else {
         }
      }
   }

   ret0__:
   return (res__);
}

ptr SAT95_get_simple_classnames_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,20,ls2525_,"Error in handling <");
   SATHER_STR_(20,8,ls2362_,"> and <");
   SATHER_STR_(20,19,ls2526_,"> in command file\n");
   SATHER_STR_(20,30,ls2528_,"Error: Cannot analyze class \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   ptr    tmpres__ = 0;
   ptr    c_names__ = 0;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    info__ = 0;
   ptr    cnm__ = 0;
   ptr    cnm_lst__ = 0;
   int    tsz__ = S_int_VOID_;

   tmpres__ = (ptr)LIS98_create_(0,3);
   res__ = (ptr)LIS98_create_(0,3);
   c_names__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((4) << 2));
   sz__ = (int)C_N180_info_size_(c_names__);
   i__ = (int)0;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1156_;
      }
      else {
      }
      info__ = (ptr)C_N180_ith_info_(c_names__,i__);
      cnm__ = (ptr)GLO94_extract_poss_names_(0,PATT_(info__, 16 + ((1) << 2)));
      if ((cnm__ == 0)) {
         SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2525_)),PATT_(info__, 16 + ((0) << 2))),(ptr)(&ls2362_)),PATT_(info__, 16 + ((1) << 2))),(ptr)(&ls2526_)));
      }
      else {
         cnm_lst__ = (ptr)GLO94_extract_simple_classnames_(0,PATT_(cnm__, 8 + ((0) << 2)),0);
         if ((cnm_lst__ != 0)) {
            tmpres__ = (ptr)GLO94_union_classnames_(0,tmpres__,cnm_lst__);
         }
         else {
            SAT95_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2528_)),PATT_(cnm__, 8 + ((0) << 2))),(ptr)(&ls632_)));
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1156_: ;
   i__ = (int)0;
   tsz__ = (int)IATT_(tmpres__,4);
   while (1) {
      if ((i__ >= tsz__)) {
         goto goto_tag_1157_;
      }
      else {
      }
      if ((! RES71_base_classname_p_(0,IATT_(tmpres__, 16 + ((i__) << 2))))) {
         res__ = (ptr)LIS98_push_(res__,IATT_(tmpres__, 16 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1157_: ;

   ret0__:
   return (res__);
}

int SAT95_get_array_str_ind_(ptr self__)
{
   int res__ = S_int_VOID_;
   SATHER_STR_(20,68,ls2529_,"(Error) : Check whether type of argument of \"main\" is \"ARRAY{STR}\"\n");
   ptr    tmp_key__ = 0;
   ptr    co__ = 0;

   if ((SAT95_array_str_ind_ == 0)) {
      tmp_key__ = (ptr)LST147_push_(LST147_push_(LST147_create_(0,2),38),RES97_STR_ici_);
      co__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,tmp_key__);
      if ((co__ != 0)) {
         SAT95_array_str_ind_ = (int)IATT_(co__,20);
         res__ = (int)IATT_(co__,20);
      }
      else {
         SAT95_error_msg_(self__,(ptr)(&ls2529_));
         res__ = (int)RES97_OB_ici_;
      }
   }
   else {
      res__ = (int)SAT95_array_str_ind_;
   }

   ret0__:
   return (res__);
}

void SAT95_print_gen_rt_table_(ptr self__, ptr outfile__, ptr table_prefix__, char cond__)
{
   SATHER_STR_(20,15,ls2532_,"unsigned int* ");
   SATHER_STR_(20,15,ls2533_,"_table_[] = {\n");
   SATHER_STR_(20,6,ls2534_,"_ent_");
   SATHER_STR_(20,4,ls693_,"};\n");
   SATHER_STR_(20,6,ls2535_,"int* ");
   SATHER_STR_(20,18,ls2536_,"_table_[] = {0};\n");
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   if (cond__) {
      (void)SAT99_indent_(outfile__);
      (void)SAT99_ind_inc_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2532_)),table_prefix__),(ptr)(&ls2533_)));
      i__ = (int)0;
      while (1) {
         if ((i__ >= IATT_(GLO68_class_inst_,24))) {
            goto goto_tag_1158_;
         }
         else {
         }
         (void)SAT99_indent_(outfile__);
         co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
         if ((co__ != 0)) {
            if (GLO94_handle_class_p_(0,co__)) {
               if ((((((IATT_(co__,20) != RES97_C_ici_) & (IATT_(co__,20) != RES97_OB_ici_)) & (IATT_(co__,20) != RES97_FOB_ici_)) & (IATT_(co__,20) != RES97_UNDEFINE_ici_)) & (IATT_(co__,20) != RES97_SELF_TYPE_ici_))) {
                  (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,table_prefix__),(ptr)(&ls2534_)),PATT_(co__,84));
               }
               else {
                  (void)SAT99_c_(outfile__,'0');
               }
            }
            else {
               (void)SAT99_c_(outfile__,'0');
            }
         }
         else {
            (void)SAT99_c_(outfile__,'0');
         }
         i__ = (int)(i__ + 1);
         if ((i__ < IATT_(GLO68_class_inst_,24))) {
            (void)SAT99_c_(outfile__,',');
         }
         else {
         }
         (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
      }
   goto_tag_1158_: ;
      (void)SAT99_ind_dec_(outfile__);
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls693_)),1);
   }
   else {
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2535_)),table_prefix__),(ptr)(&ls2536_)),1);
   }

   ret0__:
   return;
}


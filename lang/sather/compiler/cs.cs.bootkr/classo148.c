/* classo148.c : Sather class: CLASSOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*constant*/ int SYS13_Attribute_;
extern /*constant*/ int SYS13_Shared_;
extern int INT15_rshift_();
extern int INT15_lshift_();
extern /*constant*/ int SYS13_Constant_;
extern /*constant*/ int SYS13_Routine_;
extern int INT15_mod_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern int STR20_length_();
extern ptr STR20_head_();
extern ptr STR20_to_upper_case_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern ptr STR20_to_lower_case_();
extern ptr ROU24_create_();
extern ptr LST38_create_();
extern ptr LST43_pcopy_();
extern ptr SIM55_create_();
extern ptr DIS56_create_();
extern ptr PAR61_create_lst_();
extern ptr ROU63_create_();
extern ptr ROU63_pcopy_();
extern ptr ANY65_create_();
extern int CLA67_num_params_();
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern ptr CLA67_name_str_();
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern int STR69_insert_str_();
extern /*constant*/ int RES71_bool_ind_;
extern /*constant*/ int RES71_res_ind_;
extern /*constant*/ int RES71_init_ind_;
extern /*shared*/ char GLO68_pre_semant_;
extern /*shared*/ ptr GLO68_c_macros_;
extern /*shared*/ ptr GLO68_class_stat_tbl_;
extern /*shared*/ ptr GLO68_tmpct_;
extern ptr STR69_at_index_();
extern /*shared*/ char COM82_verbose_code_;
extern /*shared*/ char COM82_k_and_r_c_;
extern ptr INT86_get_();
extern /*shared*/ char GLO68_print_feat_info_;
extern /*constant*/ ptr INS88_c_names_file_;
extern /*constant*/ ptr INS88_offset_file_;
extern ptr CLA93_at_index_();
extern int GLO94_featname_from_key_();
extern SAT95_cs_options_cprint_();
extern ERR96_compiler_error_msg_();
extern /*constant*/ int RES97_OB_ici_;
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern /*constant*/ int RES97_SELF_TYPE_ici_;
extern ptr LIS98_create_();
extern ptr LIS98_push_();
extern ptr LST102_pop_();
extern ptr LST102_create_();
extern ptr LST102_push_();
extern ptr ID_103_create_();
extern char LIS98_not_in_();
extern ptr LIS98_append_();
extern /*constant*/ int RES97_FOB_ici_;
extern /*constant*/ int RES97_BOOL_ici_;
extern /*constant*/ int OP_110_and_op_ind_;
extern ptr OP_110_create_();
extern /*constant*/ int RES97_CHAR_ici_;
extern /*constant*/ int RES97_INT_ici_;
extern /*constant*/ int RES97_REAL_ici_;
extern /*constant*/ int RES97_DOUBLE_ici_;
extern int OLD101_get_ctype_();
extern ptr EXP117_dup_();
extern char LST102_param_type_conforms_to_();
extern ptr LIS98_union_();
extern ptr LST120_push_();
extern int OLD101_get_def_time_stamp_();
extern int OLD101_get_time_stamp_();
extern ptr LST123_push_();
extern /*constant*/ int RES97_C_ici_;
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern ptr SAT99_inc_ln_();
extern int GLO94_classind_from_key_();
extern GLO94_cprint_sather_str_type_();
extern GLO94_cprint_global_tmpnm_str_();
extern char GLO94_handle_feature_p_();
extern ptr SAT99_i_();
extern ptr LST133_push_unique_();
extern ptr LST133_create_();
extern ptr LST133_dup_();
extern ptr LST133_compact_();
extern LST133_compactAttr_();
extern ptr SAT99_indent_();
extern int GLO94_check_des_of_();
extern int GLO94_help_insert_();
extern ptr ASS141_create_();
extern ASS141_out_of_line_();
extern int GLO94_key_of_class_feat_();
extern char GLO94_check_is_on_();
extern ptr ERR96_def_filename_();
extern int LST147_pop_();
extern ptr LST147_push_();
extern ptr TYP149_inst_cls_();
extern ptr TYP149_full_name_();
extern int TYP149_ctype_();
extern CON151_cprint_decln_with_poss_init_();
extern int ATT153_compute_own_offset_();
extern CS_160_wrong_number_of_type_parameters_();
extern CS_160_cyclic_inheritance_();
extern int FEA162_featob_s_name_();
extern char FEA162_undefined_p_();
extern ptr INT164_create_();
extern int FEA162_class_inst_ind_();
extern CS_160_non_inheritable_();
extern CS_160_unimplemented_feature_();
extern CS_160_undefined_feature_();
extern FEA162_mark_private_();
extern FEA162_mark_abstract_();
extern FEA162_resolve_predef_types_();
extern CS_160_incompatible_inheritance_();
extern CS_160_basic_with_attributes_();
extern /*constant*/ int C_T168_c_ptr_;
extern CS_160_basic_cross_inheritance_();
extern ptr INT164_insert_();
extern char INT164_get_();
extern ptr INT164_union_();
extern ptr INT164_cursor_();
extern FEA162_semant_();
extern ptr INT164_sym_difference_();
extern char INT164_is_a_subset_of_();
extern FEA162_do_gen_temps_();
extern FEA162_gen_goto_tags_();
extern FEA162_eval_constant_();
extern ptr SYM186_get_feature_();
extern SYM186_add_feature_();
extern SYM186_del_feature_();
extern ptr SYM186_create_();
extern ptr SEM188_dup_();
extern FEA162_validate_dispatches_and_get_ext_strs_();
extern SEM188_cprint_extern_();
extern FEA162_cprint_decln_();
extern ptr FEA162_typeof_();
extern FEA162_cprint_offset_();
extern FEA162_cprint_routine_();
extern FEA162_cprint_extern_();
extern FEA162_cprint_store_dispval_();
extern PRI187_cprint_restore_exec_info_();
extern FEA162_cprint_init_code_();
extern ptr LIS213_create_();
extern ptr LIS213_push_();
extern int INT214_item_();
extern int INT214_next_();
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
extern struct { int tp_; int sz_; char st_; } gs1060_;
extern struct { int tp_; int sz_; char st_; } gs1062_;
extern struct { int tp_; int sz_; char st_; } gs1064_;
extern struct { int tp_; int sz_; char st_; } gs1066_;
extern struct { int tp_; int sz_; char st_; } gs66_;
extern struct { int tp_; int sz_; char st_; } gs1068_;
extern struct { int tp_; int sz_; char st_; } gs68_;
extern struct { int tp_; int sz_; char st_; } gs69_;
extern struct { int tp_; int sz_; char st_; } gs70_;
extern struct { int tp_; int sz_; char st_; } gs71_;
extern struct { int tp_; int sz_; char st_; } gs1070_;
extern struct { int tp_; int sz_; char st_; } gs73_;
extern struct { int tp_; int sz_; char st_; } gs74_;
extern struct { int tp_; int sz_; char st_; } gs1072_;
extern struct { int tp_; int sz_; char st_; } gs1074_;
extern struct { int tp_; int sz_; char st_; } gs72_;
extern struct { int tp_; int sz_; char st_; } gs62_;
extern struct { int tp_; int sz_; char st_; } gs63_;
extern struct { int tp_; int sz_; char st_; } gs64_;
extern struct { int tp_; int sz_; char st_; } gs65_;
extern struct { int tp_; int sz_; char st_; } gs75_;
extern struct { int tp_; int sz_; char st_; } gs67_;
extern struct { int tp_; int sz_; char st_; } gs1165_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs2279_;
extern struct { int tp_; int sz_; char st_; } gs2280_;
extern struct { int tp_; int sz_; char st_; } gs496_;
#include "macros_.h"



/*constant*/ ptr CLA148_abstract_kw_name_ = (ptr)(&gs66_);
/*constant*/ ptr CLA148_else_kw_name_ = (ptr)(&gs7_);
/*constant*/ ptr CLA148_elsif_kw_name_ = (ptr)(&gs8_);
/*constant*/ ptr CLA148_end_kw_name_ = (ptr)(&gs9_);
/*constant*/ ptr CLA148_if_kw_name_ = (ptr)(&gs10_);
/*constant*/ ptr CLA148_inline_kw_name_ = (ptr)(&gs11_);
/*constant*/ ptr CLA148_is_kw_name_ = (ptr)(&gs12_);
/*constant*/ ptr CLA148_loop_kw_name_ = (ptr)(&gs13_);
/*constant*/ ptr CLA148_not_kw_name_ = (ptr)(&gs14_);
/*constant*/ ptr CLA148_or_kw_name_ = (ptr)(&gs15_);
/*constant*/ ptr CLA148_private_kw_name_ = (ptr)(&gs16_);
/*constant*/ ptr CLA148_protect_kw_name_ = (ptr)(&gs69_);
/*constant*/ ptr CLA148_readonly_kw_name_ = (ptr)(&gs73_);
/*constant*/ ptr CLA148_against_kw_name_ = (ptr)(&gs70_);
/*constant*/ ptr CLA148_raise_kw_name_ = (ptr)(&gs68_);
/*constant*/ ptr CLA148_return_kw_name_ = (ptr)(&gs17_);
/*constant*/ ptr CLA148_shared_kw_name_ = (ptr)(&gs18_);
/*constant*/ ptr CLA148_switch_kw_name_ = (ptr)(&gs19_);
/*constant*/ ptr CLA148_typecase_kw_name_ = (ptr)(&gs71_);
/*constant*/ ptr CLA148_then_kw_name_ = (ptr)(&gs20_);
/*constant*/ ptr CLA148_until_kw_name_ = (ptr)(&gs21_);
/*constant*/ ptr CLA148_when_kw_name_ = (ptr)(&gs22_);
/*constant*/ ptr CLA148_while_kw_name_ = (ptr)(&gs74_);
ptr CLA148_initialize_();
/*constant*/ ptr CLA148_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr CLA148_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr CLA148_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr CLA148_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr CLA148_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr CLA148_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr CLA148_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr CLA148_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr CLA148_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr CLA148_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr CLA148_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr CLA148_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr CLA148_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr CLA148_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr CLA148_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr CLA148_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr CLA148_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr CLA148_void_name_ = (ptr)(&gs37_);
/*constant*/ ptr CLA148_array_classname_ = (ptr)(&gs38_);
/*constant*/ ptr CLA148_array2_classname_ = (ptr)(&gs39_);
/*constant*/ ptr CLA148_array3_classname_ = (ptr)(&gs40_);
/*constant*/ ptr CLA148_array4_classname_ = (ptr)(&gs41_);
/*constant*/ ptr CLA148_bool_classname_ = (ptr)(&gs42_);
/*constant*/ ptr CLA148_c_classname_ = (ptr)(&gs43_);
/*constant*/ ptr CLA148_char_classname_ = (ptr)(&gs44_);
/*constant*/ ptr CLA148_double_classname_ = (ptr)(&gs45_);
/*constant*/ ptr CLA148_file_classname_ = (ptr)(&gs46_);
/*constant*/ ptr CLA148_int_classname_ = (ptr)(&gs47_);
/*constant*/ ptr CLA148_real_classname_ = (ptr)(&gs48_);
/*constant*/ ptr CLA148_self_type_classname_ = (ptr)(&gs49_);
/*constant*/ ptr CLA148_str_classname_ = (ptr)(&gs50_);
/*constant*/ ptr CLA148_str_cursor_classname_ = (ptr)(&gs51_);
/*constant*/ ptr CLA148_ob_classname_ = (ptr)(&gs52_);
/*constant*/ ptr CLA148_sys_classname_ = (ptr)(&gs53_);
/*constant*/ ptr CLA148_fob_classname_ = (ptr)(&gs54_);
/*constant*/ ptr CLA148_sux_classname_ = (ptr)(&gs59_);
/*constant*/ ptr CLA148_undefine_classname_ = (ptr)(&gs55_);
/*constant*/ ptr CLA148_err_classname_ = (ptr)(&gs56_);
/*constant*/ ptr CLA148_in_classname_ = (ptr)(&gs57_);
/*constant*/ ptr CLA148_out_classname_ = (ptr)(&gs58_);
/*constant*/ int CLA148_and_kw_ind_ = 1;
/*constant*/ int CLA148_assert_kw_ind_ = 2;
/*constant*/ int CLA148_break_kw_ind_ = 3;
/*constant*/ int CLA148_class_kw_ind_ = 4;
/*constant*/ int CLA148_constant_kw_ind_ = 5;
/*constant*/ int CLA148_debug_kw_ind_ = 6;
/*constant*/ int CLA148_else_kw_ind_ = 7;
/*constant*/ int CLA148_elsif_kw_ind_ = 8;
/*constant*/ int CLA148_end_kw_ind_ = 9;
/*constant*/ int CLA148_if_kw_ind_ = 10;
/*constant*/ int CLA148_inline_kw_ind_ = 11;
/*constant*/ int CLA148_is_kw_ind_ = 12;
/*constant*/ int CLA148_loop_kw_ind_ = 13;
/*constant*/ int CLA148_not_kw_ind_ = 14;
/*constant*/ int CLA148_or_kw_ind_ = 15;
/*constant*/ int CLA148_private_kw_ind_ = 16;
/*constant*/ int CLA148_return_kw_ind_ = 17;
/*constant*/ int CLA148_shared_kw_ind_ = 18;
/*constant*/ int CLA148_switch_kw_ind_ = 19;
/*constant*/ int CLA148_then_kw_ind_ = 20;
/*constant*/ int CLA148_until_kw_ind_ = 21;
/*constant*/ int CLA148_when_kw_ind_ = 22;
/*constant*/ int CLA148_asize_ind_ = 23;
/*constant*/ int CLA148_asize1_ind_ = 24;
/*constant*/ int CLA148_asize2_ind_ = 25;
/*constant*/ int CLA148_asize3_ind_ = 26;
/*constant*/ int CLA148_asize4_ind_ = 27;
/*constant*/ int CLA148_copy_ind_ = 28;
/*constant*/ int CLA148_deep_copy_ind_ = 29;
/*constant*/ int CLA148_extend_ind_ = 30;
/*constant*/ int CLA148_new_ind_ = 31;
/*constant*/ int CLA148_type_ind_ = 32;
/*constant*/ int CLA148_res_ind_ = 33;
/*constant*/ int CLA148_self_ind_ = 34;
/*constant*/ int CLA148_false_ind_ = 35;
/*constant*/ int CLA148_true_ind_ = 36;
/*constant*/ int CLA148_void_ind_ = 37;
/*constant*/ int CLA148_first_base_class_ind_ = 38;
/*constant*/ int CLA148_array_ind_ = 38;
/*constant*/ int CLA148_array2_ind_ = 39;
/*constant*/ int CLA148_array3_ind_ = 40;
/*constant*/ int CLA148_array4_ind_ = 41;
/*constant*/ int CLA148_bool_ind_ = 42;
/*constant*/ int CLA148_c_ind_ = 43;
/*constant*/ int CLA148_char_ind_ = 44;
/*constant*/ int CLA148_double_ind_ = 45;
/*constant*/ int CLA148_file_ind_ = 46;
/*constant*/ int CLA148_int_ind_ = 47;
/*constant*/ int CLA148_real_ind_ = 48;
/*constant*/ int CLA148_self_type_ind_ = 49;
/*constant*/ int CLA148_str_ind_ = 50;
/*constant*/ int CLA148_str_cursor_ind_ = 51;
/*constant*/ int CLA148_ob_ind_ = 52;
/*constant*/ int CLA148_sys_ind_ = 53;
/*constant*/ int CLA148_fob_ind_ = 54;
/*constant*/ int CLA148_undefine_ind_ = 55;
/*constant*/ int CLA148_err_ind_ = 56;
/*constant*/ int CLA148_in_ind_ = 57;
/*constant*/ int CLA148_out_ind_ = 58;
/*constant*/ int CLA148_sux_ind_ = 59;
/*constant*/ int CLA148_last_base_class_ind_ = 59;
/*constant*/ int CLA148_exception_ind_ = 60;
/*constant*/ int CLA148_init_ind_ = 61;
/*constant*/ int CLA148_alias_kw_ind_ = 62;
/*constant*/ int CLA148_undefine_kw_ind_ = 63;
/*constant*/ int CLA148_require_kw_ind_ = 64;
/*constant*/ int CLA148_ensure_kw_ind_ = 65;
/*constant*/ int CLA148_abstract_kw_ind_ = 66;
/*constant*/ int CLA148_invariant_kw_ind_ = 67;
/*constant*/ int CLA148_raise_kw_ind_ = 68;
/*constant*/ int CLA148_protect_kw_ind_ = 69;
/*constant*/ int CLA148_against_kw_ind_ = 70;
/*constant*/ int CLA148_typecase_kw_ind_ = 71;
/*constant*/ int CLA148_attr_kw_ind_ = 72;
/*constant*/ int CLA148_readonly_kw_ind_ = 73;
/*constant*/ int CLA148_while_kw_ind_ = 74;
/*constant*/ int CLA148_include_kw_ind_ = 75;
/*constant*/ int CLA148_arg_ind_ = 76;
/*constant*/ int CLA148_last_reserved_word_ind_ = 76;
char CLA148_base_classname_p_();
/*constant*/ int CLA148_c_ptr_ = 1;
/*constant*/ int CLA148_c_char_ = 2;
/*constant*/ int CLA148_c_int_ = 3;
/*constant*/ int CLA148_c_float_ = 4;
/*constant*/ int CLA148_c_double_ = 5;
/*constant*/ int CLA148_c_void_ = 6;
/*constant*/ int CLA148_c_ptr_size_ = 4;
/*constant*/ int CLA148_c_char_size_ = 1;
/*constant*/ int CLA148_c_int_size_ = 4;
/*constant*/ int CLA148_c_float_size_ = 4;
/*constant*/ int CLA148_c_double_size_ = 8;
/*constant*/ ptr CLA148_c_ptr_name_ = (ptr)(&gs1215_);
/*constant*/ ptr CLA148_c_char_name_ = (ptr)(&gs1216_);
/*constant*/ ptr CLA148_c_int_name_ = (ptr)(&gs1217_);
/*constant*/ ptr CLA148_c_float_name_ = (ptr)(&gs1218_);
/*constant*/ ptr CLA148_c_double_name_ = (ptr)(&gs1219_);
/*constant*/ ptr CLA148_source_files_kw_ = (ptr)(&gs1060_);
/*constant*/ ptr CLA148_object_files_kw_ = (ptr)(&gs1062_);
/*constant*/ ptr CLA148_cc_flags_kw_ = (ptr)(&gs1064_);
/*constant*/ ptr CLA148_c_macro_kw_ = (ptr)(&gs1066_);
/*constant*/ ptr CLA148_c_name_kw_ = (ptr)(&gs1068_);
/*constant*/ ptr CLA148_include_kw_ = (ptr)(&gs1070_);
/*constant*/ ptr CLA148_sather_home_kw_ = (ptr)(&gs1072_);
/*constant*/ ptr CLA148_c_compiler_kw_ = (ptr)(&gs1074_);
/*constant*/ int CLA148_source_files_ind_ = 0;
/*constant*/ int CLA148_object_files_ind_ = 1;
/*constant*/ int CLA148_cc_flags_ind_ = 2;
/*constant*/ int CLA148_c_macro_ind_ = 3;
/*constant*/ int CLA148_c_name_ind_ = 4;
/*constant*/ int CLA148_include_ind_ = 5;
/*constant*/ int CLA148_sather_home_ind_ = 6;
/*constant*/ int CLA148_c_compiler_ind_ = 7;
/*constant*/ int CLA148_compile_keys_fst_ind_ = 0;
/*constant*/ int CLA148_compile_keys_lst_ind_ = 7;
/*constant*/ int CLA148_num_compile_keys_ = 8;
/*constant*/ int CLA148_non_compile_key_ind_ = 8;
/*constant*/ int CLA148_eof_tok_ = -1;
/*constant*/ int CLA148_ident_tok_ = -2;
/*constant*/ int CLA148_qexp_tok_ = -3;
char CLA148_compile_key_p_();
/*constant*/ int CLA148_print_indent_ = 2;
/*constant*/ ptr CLA148_and_kw_name_ = (ptr)(&gs1_);
/*constant*/ ptr CLA148_assert_kw_name_ = (ptr)(&gs2_);
/*constant*/ ptr CLA148_attr_kw_name_ = (ptr)(&gs72_);
/*constant*/ ptr CLA148_break_kw_name_ = (ptr)(&gs3_);
/*constant*/ ptr CLA148_class_kw_name_ = (ptr)(&gs4_);
/*constant*/ ptr CLA148_constant_kw_name_ = (ptr)(&gs5_);
/*constant*/ ptr CLA148_debug_kw_name_ = (ptr)(&gs6_);
/*constant*/ ptr CLA148_alias_kw_name_ = (ptr)(&gs62_);
/*constant*/ ptr CLA148_undefine_kw_name_ = (ptr)(&gs63_);
/*constant*/ ptr CLA148_require_kw_name_ = (ptr)(&gs64_);
/*constant*/ ptr CLA148_ensure_kw_name_ = (ptr)(&gs65_);
/*constant*/ ptr CLA148_include_kw_name_ = (ptr)(&gs75_);
/*constant*/ ptr CLA148_invariant_kw_name_ = (ptr)(&gs67_);
/*constant*/ int CLA148_prefix_size_ = 3;
/*constant*/ int CLA148_filename_size_ = 6;
/*shared*/ char CLA148_auto_generating_p_;
ptr CLA148_create_();
CLA148_copy_features_();
CLA148_update_ind_();
CLA148_mark_is_used_();
ptr CLA148_make_prefix_();
ptr CLA148_make_cfilename_();
char CLA148_parametrized_p_();
ptr CLA148_name_str_();
ptr CLA148_full_name_();
CLA148_update_ctype_();
ptr CLA148_get_feature_();
ptr CLA148_store_feature_unique_();
CLA148_eliminate_dup_();
char CLA148_expand_cinh_();
ptr CLA148_alias_feature_();
/*constant*/ ptr CLA148_invar_feature_name_ = (ptr)(&gs496_);
ptr CLA148_get_invariant_();
CLA148_add_invariant_();
CLA148_add_initializer_();
CLA148_resolve_predef_types_and_compute_num_attrs_();
int CLA148_compute_pdepth_();
CLA148_add_unique_anc_();
CLA148_compute_anc_();
CLA148_compute_des_();
CLA148_compute_pdes_();
CLA148_add_new_des_();
CLA148_add_new_anc_();
CLA148_adjust_parent_alldes_();
CLA148_compute_time_stamp_();
CLA148_compute_attr_offsets_();
CLA148_pre_semant_();
CLA148_semant_();
CLA148_mark_callees_and_callers_();
CLA148_misc_info_();
CLA148_cprint_macros_();
CLA148_cprint_header_and_macros_();
CLA148_cprint_externs_();
CLA148_cprint_ext_strs_();
CLA148_cprint_declns_();
CLA148_cprint_attr_tbl_();
CLA148_cprint_feat_tbl_();
CLA148_cprint_des_tbl_();
CLA148_cprint_routines_();
CLA148_cprint_ctype_();
int CLA148_cprint_extern_feats_();
CLA148_cprint_insert_feats_();
CLA148_cprint_post_rtcode_();
CLA148_cprint_pre_rtcode_();
CLA148_cprint_init_shareds_and_consts_();
extern int attr_ent_CLA148[];

ptr CLA148_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   CATT_(self__,4) = (char)0;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char CLA148_base_classname_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ >= 38) & (i__ <= 59));

   ret0__:
   return (res__);
}

char CLA148_compile_key_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ <= 7) & (i__ >= 0));

   ret0__:
   return (res__);
}

ptr CLA148_create_(self__,d__,k__,pi__)
ptr self__;
ptr d__;
ptr k__;
ptr pi__;
{
   ptr res__ = 0;
   int    psz__ = S_int_VOID_;
   int    tsz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    i_360_ = S_int_VOID_;
   int    i_361_ = S_int_VOID_;

   psz__ = S_int_VOID_;
   if ((pi__ != 0)) {
      psz__ = (int)IATT_(pi__,16);
   }
   else {
   }
   tsz__ = (int)CLA67_num_params_(d__);
   if ((tsz__ != psz__)) {
      CS_160_wrong_number_of_type_parameters_(0,PATT_(d__,16),IATT_(d__,8));
      if ((tsz__ < psz__)) {
         if ((pi__ != 0)) {
            i__ = (int)psz__;
            while (1) {
               if ((i__ <= tsz__)) {
                  goto goto_tag_3785_;
               }
               else {
               }
               (void)LST102_pop_(pi__);
               i__ = (int)(i__ - 1);
            }
         goto_tag_3785_: ;
            if ((IATT_(pi__,16) == 0)) {
               pi__ = (ptr)0;
            }
            else {
            }
         }
         else {
         }
         i_360_ = (int)psz__;
         while (1) {
            if ((i_360_ <= tsz__)) {
               goto goto_tag_3786_;
            }
            else {
            }
            (void)LST147_pop_(k__);
            i_360_ = (int)(i_360_ - 1);
         }
      goto_tag_3786_: ;
      }
      else {
         if ((pi__ == 0)) {
            pi__ = (ptr)LST102_create_(0,2);
         }
         else {
         }
         i_361_ = (int)psz__;
         while (1) {
            if ((i_361_ >= tsz__)) {
               goto goto_tag_3787_;
            }
            else {
            }
            pi__ = (ptr)LST102_push_(pi__,GLO68_ob_typeob_s_);
            k__ = (ptr)LST147_push_(k__,(- RES97_OB_ici_));
            i_361_ = (int)(i_361_ + 1);
         }
      goto_tag_3787_: ;
      }
   }
   else {
   }
   res__ = (ptr)new_(148,0);
   IATT_(res__,128) = (int)IATT_(d__,36);
   IATT_(res__,132) = (int)IATT_(d__,36);
   if ((tsz__ > 0)) {
      CATT_(res__,5) = (char)1;
   }
   else {
   }
   PATT_(res__,40) = (ptr)pi__;
   PATT_(res__,28) = (ptr)d__;
   PATT_(res__,44) = (ptr)k__;
   CATT_(res__,4) = (char)CATT_(d__,4);
   PATT_(res__,68) = (ptr)INT164_create_(0);
   PATT_(res__,112) = (ptr)INT164_create_(0);
   PATT_(res__,116) = (ptr)INT164_create_(0);
   PATT_(res__,120) = (ptr)INT164_create_(0);
   PATT_(res__,136) = (ptr)INT164_create_(0);
   PATT_(res__,140) = (ptr)INT164_create_(0);
   IATT_(res__,24) = (int)IATT_(d__,40);

   ret0__:
   return (res__);
}

CLA148_copy_features_(self__)
ptr self__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,8,ls405_,"class \"");
   SATHER_STR_(20,24,ls406_,"\"Copying features twice");

   if ((PATT_(self__,32) != 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls262_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls405_)),CLA67_name_str_(PATT_(self__,28))),(ptr)(&ls406_)));
   }
   else {
   }
   if ((PATT_(PATT_(self__,28),20) != 0)) {
      PATT_(self__,32) = (ptr)LST43_pcopy_(PATT_(PATT_(self__,28),20),PATT_(PATT_(self__,28),24),PATT_(self__,40));
   }
   else {
   }

   ret0__:
   return;
}

CLA148_update_ind_(self__,i__)
ptr self__;
int i__;
{

   IATT_(self__,20) = (int)i__;
   PATT_(self__,84) = (ptr)CLA148_make_prefix_(self__);
   PATT_(self__,92) = (ptr)CLA148_make_cfilename_(self__);

   ret0__:
   return;
}

CLA148_mark_is_used_(self__)
ptr self__;
{
   ptr gl3788_;
   static int gl3789_;
   static union dtype_ gl3790_;
   int    def_ind__ = S_int_VOID_;
   ptr    co__ = 0;

   if ((! CATT_(self__,4))) {
      CATT_(self__,9) = (char)1;
   }
   else {
   }
   def_ind__ = (int)IATT_(PATT_(self__,28),12);
   if (((((def_ind__ == 38) | (def_ind__ == 39)) | (def_ind__ == 40)) | (def_ind__ == 41))) {
      gl3788_ = PATT_(PATT_(self__,40), 28 + ((0) << 2));
      cache_dispatch_(gl3788_,418,gl3789_,INTVAL_(gl3790_));
      co__ = (ptr)PFN_(gl3790_)(gl3788_);
      CLA148_mark_is_used_(co__);
   }
   else {
   }

   ret0__:
   return;
}

ptr CLA148_make_prefix_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    nm__ = 0;
   int    nc__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   nm__ = (ptr)CLA67_name_str_(PATT_(self__,28));
   nc__ = (int)STR20_length_(nm__);
   if ((nc__ < 3)) {
      res__ = (ptr)STR20_to_upper_case_(STR20_head_(nm__,3));
      i__ = (int)nc__;
      while (1) {
         if ((i__ >= 3)) {
            goto goto_tag_3791_;
         }
         else {
         }
         res__ = (ptr)STR20_c_(res__,'_');
         i__ = (int)(i__ + 1);
      }
   goto_tag_3791_: ;
      res__ = (ptr)STR20_i_(res__,IATT_(self__,20));
   }
   else {
      res__ = (ptr)STR20_i_(STR20_to_upper_case_(STR20_head_(nm__,3)),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

ptr CLA148_make_cfilename_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    nm__ = 0;
   int    nc__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   nm__ = (ptr)CLA67_name_str_(PATT_(self__,28));
   nc__ = (int)STR20_length_(nm__);
   if ((nc__ < 6)) {
      res__ = (ptr)STR20_to_lower_case_(STR20_head_(nm__,6));
      i__ = (int)nc__;
      while (1) {
         if ((i__ >= 6)) {
            goto goto_tag_3792_;
         }
         else {
         }
         res__ = (ptr)STR20_c_(res__,'_');
         i__ = (int)(i__ + 1);
      }
   goto_tag_3792_: ;
      res__ = (ptr)STR20_i_(res__,IATT_(self__,20));
   }
   else {
      res__ = (ptr)STR20_i_(STR20_to_lower_case_(STR20_head_(nm__,6)),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

char CLA148_parametrized_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if ((PATT_(self__,40) != 0)) {
      res__ = (char)(IATT_(PATT_(self__,40),16) > 0);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr CLA148_name_str_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)CLA67_name_str_(PATT_(self__,28));

   ret0__:
   return (res__);
}

ptr CLA148_full_name_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3793_;
   static int gl3794_;
   static union dtype_ gl3795_;
   ptr gl362_;
   int    psz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if ((PATT_(self__,88) != 0)) {
      res__ = (ptr)PATT_(self__,88);
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)STR20_s_(STR20_create_(0),CLA67_name_str_(PATT_(self__,28)));
   psz__ = S_int_VOID_;
   if ((PATT_(self__,40) != 0)) {
      psz__ = (int)IATT_(PATT_(self__,40),16);
   }
   else {
   }
   if ((psz__ > 0)) {
      res__ = (ptr)STR20_c_(res__,'{');
      i__ = (int)0;
      while (1) {
         if ((i__ >= psz__)) {
            goto goto_tag_3796_;
         }
         else {
         }
         gl3793_ = PATT_(PATT_(self__,40), 28 + ((i__) << 2));
         cache_dispatch_(gl3793_,426,gl3794_,INTVAL_(gl3795_));
         gl362_ = PFN_(gl3795_)(gl3793_);
         res__ = (ptr)STR20_s_(res__,gl362_);
         i__ = (int)(i__ + 1);
         if ((! (i__ >= psz__))) {
            res__ = (ptr)STR20_c_(res__,',');
         }
         else {
         }
      }
   goto_tag_3796_: ;
      res__ = (ptr)STR20_c_(res__,'}');
   }
   else {
   }
   PATT_(self__,88) = (ptr)res__;

   ret0__:
   return (res__);
}

CLA148_update_ctype_(self__)
ptr self__;
{
   ptr    co__ = 0;

   if ((IATT_(self__,96) == 0)) {
      switch (IATT_(PATT_(self__,28),12)) {
         case (47) :
            IATT_(self__,96) = (int)IATT_(self__,20);
            IATT_(self__,104) = (int)3;
            break;
         case (44) :
         case (42) :
            IATT_(self__,96) = (int)IATT_(self__,20);
            IATT_(self__,104) = (int)2;
            break;
         case (48) :
            IATT_(self__,96) = (int)IATT_(self__,20);
            IATT_(self__,104) = (int)4;
            break;
         case (45) :
            IATT_(self__,96) = (int)IATT_(self__,20);
            IATT_(self__,104) = (int)5;
            break;
         case (54) :
            IATT_(self__,96) = (int)IATT_(self__,20);
            IATT_(self__,104) = (int)1;
            break;
         default:
            IATT_(self__,104) = (int)1;
            ;
      }
   }
   else {
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(self__,96));
      switch (IATT_(PATT_(co__,28),12)) {
         case (47) :
            IATT_(self__,104) = (int)3;
            break;
         case (44) :
         case (42) :
            IATT_(self__,104) = (int)2;
            break;
         case (48) :
            IATT_(self__,104) = (int)4;
            break;
         case (45) :
            IATT_(self__,104) = (int)5;
            break;
         default:
            IATT_(self__,104) = (int)1;
            ;
      }
   }

   ret0__:
   return;
}

ptr CLA148_get_feature_(self__,index__)
ptr self__;
int index__;
{
   ptr res__ = 0;
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,8,ls405_,"class \"");
   SATHER_STR_(20,26,ls440_,") -- Missing symbol table");

   if ((PATT_(self__,48) == 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls262_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls405_)),CLA148_full_name_(self__)),(ptr)(&ls440_)));
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)SYM186_get_feature_(PATT_(self__,48),index__);

   ret0__:
   return (res__);
}

ptr CLA148_store_feature_unique_(self__,new_flst__,fo__)
ptr self__;
ptr new_flst__;
ptr fo__;
{
   ptr res__ = 0;
   ptr gl3797_;
   static int gl3798_;
   static union dtype_ gl3799_;
   int gl363_;

   res__ = (ptr)LST133_push_unique_(new_flst__,fo__);
   gl3797_ = fo__;
   cache_dispatch_(gl3797_,445,gl3798_,INTVAL_(gl3799_));
   gl363_ = IFN_(gl3799_)(gl3797_);
   SYM186_add_feature_(PATT_(self__,48),gl363_,fo__);

   ret0__:
   return (res__);
}

CLA148_eliminate_dup_(self__)
ptr self__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,45,ls451_,"Unexpected class inheritance after expansion");
   ptr gl3800_;
   static int gl3801_;
   static union dtype_ gl3802_;
   int gl364_;
   ptr gl3803_;
   static int gl3804_;
   static union dtype_ gl3805_;
   char gl365_;
   ptr gl3806_;
   static int gl3807_;
   static union dtype_ gl3808_;
   int gl366_;
   ptr gl3809_;
   static int gl3810_;
   static union dtype_ gl3811_;
   int gl367_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    fo__ = 0;

   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3812_;
      }
      else {
      }
      fo__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      GLO68_curr_feature_ = (ptr)fo__;
      gl3800_ = fo__;
      gl364_ = TYPE_(gl3800_);
      gl3803_ = fo__;
      cache_dispatch_(gl3803_,449,gl3804_,INTVAL_(gl3805_));
      gl365_ = CFN_(gl3805_)(gl3803_);
      if (((gl364_ == 153) & gl365_)) {
         gl3806_ = fo__;
         cache_dispatch_(gl3806_,445,gl3807_,INTVAL_(gl3808_));
         gl366_ = IFN_(gl3808_)(gl3806_);
         SYM186_del_feature_(PATT_(self__,48),gl366_);
         PATT_(PATT_(self__,32), 24 + ((i__) << 2)) = (ptr)0;
      }
      else {
         gl3809_ = fo__;
         gl367_ = TYPE_(gl3809_);
         if ((gl367_ == 155)) {
            ERR96_compiler_error_msg_(0,(ptr)(&ls262_),STR20_s_(STR20_create_(0),(ptr)(&ls451_)));
            PATT_(PATT_(self__,32), 24 + ((i__) << 2)) = (ptr)0;
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3812_: ;

   ret0__:
   return;
}

char CLA148_expand_cinh_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,3,ls52_,"OB");
   SATHER_STR_(20,9,ls463_,"UNDEFINE");
   SATHER_STR_(20,5,ls49_,"SAME");
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,23,ls480_,"Inherited class index ");
   SATHER_STR_(20,11,ls481_," not found");
   ptr gl3813_;
   static int gl3814_;
   static union dtype_ gl3815_;
   int gl368_;
   ptr gl3816_;
   static int gl3817_;
   static union dtype_ gl3818_;
   int gl369_;
   ptr gl3819_;
   static int gl3820_;
   static union dtype_ gl3821_;
   int gl370_;
   ptr gl3822_;
   static int gl3823_;
   static union dtype_ gl3824_;
   ptr gl3825_;
   static int gl3826_;
   static union dtype_ gl3827_;
   int gl371_;
   ptr gl3828_;
   static int gl3829_;
   static union dtype_ gl3830_;
   int gl373_;
   ptr gl3831_;
   static int gl3832_;
   static union dtype_ gl3833_;
   ptr gl3834_;
   static int gl3835_;
   static union dtype_ gl3836_;
   ptr gl3837_;
   static int gl3838_;
   static union dtype_ gl3839_;
   int gl374_;
   int gl375_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    new_flst__ = 0;
   ptr    init_attr_lst__ = 0;
   char    ok__ = S_char_VOID_;
   ptr    att__ = 0;
   int    index__ = S_int_VOID_;
   ptr    inhc__ = 0;
   ptr    prevfname__ = 0;
   ptr    previnst__ = 0;
   ptr    prevfeat__ = 0;
   int    j__ = S_int_VOID_;
   ptr    tmp_flst__ = 0;
   int    tsz__ = S_int_VOID_;
   ptr    att_372_ = 0;
   ptr    routf__ = 0;

   if (CATT_(self__,6)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   if ((PATT_(self__,52) != 0)) {
      CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(PATT_(self__,28),40),CLA148_full_name_(self__));
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   PATT_(self__,48) = (ptr)SYM186_create_(0,(sz__ + 2),self__);
   new_flst__ = (ptr)LST133_create_(new_flst__,(sz__ * 2));
   init_attr_lst__ = (ptr)LST133_create_(init_attr_lst__,2);
   ok__ = (char)1;
   if ((PATT_(self__,32) == 0)) {
      res__ = (char)ok__;
      PATT_(self__,32) = (ptr)new_flst__;
      CATT_(self__,6) = (char)1;
      goto ret0__;
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3840_;
      }
      else {
      }
      GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl3813_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl368_ = TYPE_(gl3813_);
      if ((gl368_ == 157)) {
         new_flst__ = (ptr)CLA148_alias_feature_(self__,new_flst__,PATT_(PATT_(self__,32), 24 + ((i__) << 2)));
      }
      else {
         gl3816_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         gl369_ = TYPE_(gl3816_);
         if ((gl369_ != 155)) {
            new_flst__ = (ptr)CLA148_store_feature_unique_(self__,new_flst__,PATT_(PATT_(self__,32), 24 + ((i__) << 2)));
            gl3819_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            gl370_ = TYPE_(gl3819_);
            if ((gl370_ == 153)) {
               att__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
               if ((PATT_(att__,44) != 0)) {
                  init_attr_lst__ = (ptr)CLA148_store_feature_unique_(self__,init_attr_lst__,att__);
               }
               else {
               }
            }
            else {
            }
         }
         else {
            gl3822_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl3822_,460,gl3823_,INTVAL_(gl3824_));
            index__ = (int)IFN_(gl3824_)(gl3822_);
            if ((index__ == RES97_OB_ici_)) {
               CS_160_non_inheritable_(0,(ptr)(&ls52_),IATT_(self__,24));
            }
            else {
               if ((index__ == RES97_UNDEFINE_ici_)) {
                  CS_160_non_inheritable_(0,(ptr)(&ls463_),IATT_(self__,24));
               }
               else {
                  if ((index__ == RES97_SELF_TYPE_ici_)) {
                     CS_160_non_inheritable_(0,(ptr)(&ls49_),IATT_(self__,24));
                  }
                  else {
                     if ((PATT_(self__,52) == 0)) {
                        PATT_(self__,52) = (ptr)LIS213_create_(0,5);
                     }
                     else {
                     }
                     inhc__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
                     if ((inhc__ != 0)) {
                        PATT_(self__,52) = (ptr)LIS213_push_(PATT_(self__,52),inhc__);
                        prevfname__ = (ptr)GLO68_curr_filename_;
                        previnst__ = (ptr)GLO68_curr_class_inst_;
                        prevfeat__ = (ptr)GLO68_curr_feature_;
                        GLO68_curr_filename_ = (ptr)PATT_(PATT_(inhc__,28),16);
                        GLO68_curr_class_inst_ = (ptr)inhc__;
                        ok__ = (char)(ok__ & CLA148_expand_cinh_(inhc__));
                        GLO68_curr_filename_ = (ptr)prevfname__;
                        GLO68_curr_class_inst_ = (ptr)previnst__;
                        GLO68_curr_feature_ = (ptr)prevfeat__;
                        j__ = (int)0;
                        tmp_flst__ = (ptr)LST133_dup_(PATT_(inhc__,32));
                        tsz__ = S_int_VOID_;
                        if ((tmp_flst__ != 0)) {
                           tsz__ = (int)IATT_(tmp_flst__,12);
                        }
                        else {
                        }
                        while (1) {
                           if ((j__ >= tsz__)) {
                              goto goto_tag_3841_;
                           }
                           else {
                           }
                           new_flst__ = (ptr)CLA148_store_feature_unique_(self__,new_flst__,PATT_(tmp_flst__, 24 + ((j__) << 2)));
                           gl3825_ = PATT_(tmp_flst__, 24 + ((j__) << 2));
                           gl371_ = TYPE_(gl3825_);
                           if ((gl371_ == 153)) {
                              att_372_ = (ptr)PATT_(tmp_flst__, 24 + ((j__) << 2));
                              if ((PATT_(att_372_,44) != 0)) {
                                 init_attr_lst__ = (ptr)CLA148_store_feature_unique_(self__,init_attr_lst__,att_372_);
                              }
                              else {
                              }
                           }
                           else {
                              gl3828_ = PATT_(tmp_flst__, 24 + ((j__) << 2));
                              gl373_ = TYPE_(gl3828_);
                              if ((gl373_ == 156)) {
                                 routf__ = (ptr)PATT_(tmp_flst__, 24 + ((j__) << 2));
                                 if (CATT_(routf__,6)) {
                                    if ((PATT_(self__,36) == 0)) {
                                       PATT_(self__,36) = (ptr)ID_103_create_(0,IATT_(routf__,16),IATT_(self__,24));
                                    }
                                    else {
                                       PATT_(self__,36) = (ptr)OP_110_create_(0,8,LST120_push_(LST120_push_(new1_(120,2,0),PATT_(self__,36)),ID_103_create_(0,IATT_(routf__,16),IATT_(self__,24))),IATT_(self__,24));
                                    }
                                 }
                                 else {
                                 }
                              }
                              else {
                              }
                           }
                           j__ = (int)(j__ + 1);
                        }
                     goto_tag_3841_: ;
                     }
                     else {
                        ERR96_compiler_error_msg_(0,(ptr)(&ls262_),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls480_)),index__),(ptr)(&ls481_)));
                        ok__ = (char)0;
                     }
                  }
               }
            }
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3840_: ;
   res__ = (char)ok__;
   PATT_(self__,32) = (ptr)new_flst__;
   CATT_(self__,6) = (char)1;
   CLA148_eliminate_dup_(self__);
   PATT_(self__,32) = (ptr)LST133_compact_(PATT_(self__,32));
   if ((! CATT_(self__,4))) {
      i__ = (int)0;
      while (1) {
         if ((i__ == IATT_(PATT_(self__,32),12))) {
            goto goto_tag_3842_;
         }
         else {
         }
         gl3831_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl3831_,309,gl3832_,INTVAL_(gl3833_));
         if (CATT_(gl3831_,INTVAL_(gl3833_))) {
            gl3834_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl3834_,476,gl3835_,INTVAL_(gl3836_));
            gl374_ = IATT_(gl3834_,INTVAL_(gl3836_));
            gl3837_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl3837_,298,gl3838_,INTVAL_(gl3839_));
            gl375_ = IATT_(gl3837_,INTVAL_(gl3839_));
            CS_160_unimplemented_feature_(0,gl374_,gl375_);
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_3842_: ;
   }
   else {
   }
   if (((! CATT_(self__,4)) & (! CLA148_base_classname_p_(self__,IATT_(PATT_(self__,28),12))))) {
      CLA148_add_initializer_(self__,init_attr_lst__);
      if ((PATT_(self__,36) != 0)) {
         CLA148_add_invariant_(self__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr CLA148_alias_feature_(self__,new_flst__,f__)
ptr self__;
ptr new_flst__;
ptr f__;
{
   ptr res__ = 0;
   ptr gl3843_;
   static int gl3844_;
   static union dtype_ gl3845_;
   ptr gl3846_;
   static int gl3847_;
   static union dtype_ gl3848_;
   ptr gl3849_;
   static int gl3850_;
   static union dtype_ gl3851_;
   ptr gl3852_;
   static int gl3853_;
   static union dtype_ gl3854_;
   ptr gl3855_;
   static int gl3856_;
   static union dtype_ gl3857_;
   ptr gl3858_;
   static int gl3859_;
   static union dtype_ gl3860_;
   ptr    any__ = 0;
   ptr    fea__ = 0;

   res__ = (ptr)new_flst__;
   any__ = (ptr)CLA148_get_feature_(self__,IATT_(f__,36));
   fea__ = S_ptr_VOID_;
   if ((any__ == 0)) {
      CS_160_undefined_feature_(0,IATT_(f__,36),IATT_(f__,16));
      goto ret0__;
   }
   else {
      gl3843_ = any__;
      cache_dispatch_(gl3843_,471,gl3844_,INTVAL_(gl3845_));
      any__ = (ptr)PFN_(gl3845_)(gl3843_);
   }
   fea__ = (ptr)any__;
   gl3846_ = fea__;
   cache_dispatch_(gl3846_,298,gl3847_,INTVAL_(gl3848_));
   IATT_(gl3846_,INTVAL_(gl3848_)) = (int)IATT_(f__,16);
   gl3849_ = fea__;
   cache_dispatch_(gl3849_,476,gl3850_,INTVAL_(gl3851_));
   IATT_(gl3849_,INTVAL_(gl3851_)) = (int)IATT_(f__,12);
   gl3852_ = fea__;
   cache_dispatch_(gl3852_,492,gl3853_,INTVAL_(gl3854_));
   CATT_(gl3852_,INTVAL_(gl3854_)) = (char)0;
   if (CATT_(f__,5)) {
      gl3855_ = fea__;
      cache_dispatch_(gl3855_,493,gl3856_,INTVAL_(gl3857_));
      VFN_(gl3857_)(gl3855_);
   }
   else {
   }
   if (CATT_(f__,4)) {
      gl3858_ = fea__;
      cache_dispatch_(gl3858_,494,gl3859_,INTVAL_(gl3860_));
      VFN_(gl3860_)(gl3858_);
   }
   else {
   }
   res__ = (ptr)CLA148_store_feature_unique_(self__,res__,fea__);

   ret0__:
   return (res__);
}

ptr CLA148_get_invariant_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)CLA148_get_feature_(self__,STR69_insert_str_(GLO68_str_table_,(ptr)(&gs496_)));

   ret0__:
   return (res__);
}

CLA148_add_invariant_(self__)
ptr self__;
{
   int    invarrout__ = S_int_VOID_;
   ptr    invf__ = 0;

   CLA148_auto_generating_p_ = (char)1;
   invarrout__ = (int)STR69_insert_str_(GLO68_str_table_,(ptr)(&gs496_));
   invf__ = (ptr)ROU63_pcopy_(ROU63_create_(0,invarrout__,ROU24_create_(0,0,0,0),0,SIM55_create_(0,42),LST38_create_(0,5),IATT_(self__,24),IATT_(self__,24)),PATT_(PATT_(self__,28),24),PATT_(self__,40));
   PATT_(invf__,64) = (ptr)LST123_push_(PATT_(invf__,64),ASS141_create_(0,ID_103_create_(0,33,IATT_(self__,24)),PATT_(self__,36),IATT_(self__,24)));
   PATT_(self__,32) = (ptr)CLA148_store_feature_unique_(self__,PATT_(self__,32),invf__);
   PATT_(invf__,28) = (ptr)PATT_(self__,28);
   CLA148_auto_generating_p_ = (char)0;

   ret0__:
   return;
}

CLA148_add_initializer_(self__,init_attr_lst__)
ptr self__;
ptr init_attr_lst__;
{
   SATHER_STR_(20,8,ls508_,"initarg");
   ptr gl3861_;
   static int gl3862_;
   static union dtype_ gl3863_;
   ptr gl3864_;
   static int gl3865_;
   static union dtype_ gl3866_;
   ptr gl3867_;
   static int gl3868_;
   static union dtype_ gl3869_;
   ptr gl376_;
   int gl377_;
   int    initarg_ind__ = S_int_VOID_;
   ptr    initf__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    att__ = 0;
   ptr    init_expr__ = 0;
   ptr    asgn__ = 0;

   CLA148_auto_generating_p_ = (char)1;
   initarg_ind__ = (int)STR69_insert_str_(GLO68_str_table_,(ptr)(&ls508_));
   initf__ = (ptr)ROU63_pcopy_(ROU63_create_(0,61,ROU24_create_(0,0,0,0),PAR61_create_lst_(0,ANY65_create_(0,LIS98_push_(LIS98_create_(0,1),initarg_ind__),DIS56_create_(0,SIM55_create_(0,52)))),SIM55_create_(0,49),LST38_create_(0,5),IATT_(self__,24),IATT_(self__,24)),PATT_(PATT_(self__,28),24),PATT_(self__,40));
   i__ = S_int_VOID_;
   sz__ = (int)IATT_(init_attr_lst__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3870_;
      }
      else {
      }
      att__ = (ptr)PATT_(init_attr_lst__, 24 + ((i__) << 2));
      gl3861_ = PATT_(att__,44);
      cache_dispatch_(gl3861_,471,gl3862_,INTVAL_(gl3863_));
      init_expr__ = (ptr)PFN_(gl3863_)(gl3861_);
      gl3864_ = init_expr__;
      cache_dispatch_(gl3864_,471,gl3865_,INTVAL_(gl3866_));
      gl376_ = PFN_(gl3866_)(gl3864_);
      gl3867_ = init_expr__;
      cache_dispatch_(gl3867_,298,gl3868_,INTVAL_(gl3869_));
      gl377_ = IATT_(gl3867_,INTVAL_(gl3869_));
      asgn__ = (ptr)ASS141_create_(0,ID_103_create_(0,IATT_(att__,24),IATT_(att__,16)),gl376_,gl377_);
      ASS141_out_of_line_(asgn__,PATT_(PATT_(att__,28),16));
      PATT_(initf__,64) = (ptr)LST123_push_(PATT_(initf__,64),asgn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3870_: ;
   PATT_(initf__,64) = (ptr)LST123_push_(PATT_(initf__,64),ASS141_create_(0,ID_103_create_(0,33,IATT_(self__,24)),ID_103_create_(0,34,IATT_(self__,24)),IATT_(self__,24)));
   PATT_(self__,32) = (ptr)CLA148_store_feature_unique_(self__,PATT_(self__,32),initf__);
   PATT_(initf__,28) = (ptr)PATT_(self__,28);
   CLA148_auto_generating_p_ = (char)0;

   ret0__:
   return;
}

CLA148_resolve_predef_types_and_compute_num_attrs_(self__)
ptr self__;
{
   ptr gl3871_;
   static int gl3872_;
   static union dtype_ gl3873_;
   ptr gl3874_;
   static int gl3875_;
   static union dtype_ gl3876_;
   ptr gl3877_;
   static int gl3878_;
   static union dtype_ gl3879_;
   int gl378_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    count__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   count__ = (int)0;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3880_;
      }
      else {
      }
      GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl3871_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      cache_dispatch_(gl3871_,522,gl3872_,INTVAL_(gl3873_));
      VFN_(gl3873_)(gl3871_,IATT_(self__,20));
      gl3874_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      cache_dispatch_(gl3874_,523,gl3875_,INTVAL_(gl3876_));
      PATT_(gl3874_,INTVAL_(gl3876_)) = (ptr)self__;
      gl3877_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl378_ = TYPE_(gl3877_);
      if ((gl378_ == 153)) {
         count__ = (int)(count__ + 1);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3880_: ;
   IATT_(self__,56) = (int)count__;

   ret0__:
   return;
}

int CLA148_compute_pdepth_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   ptr gl3881_;
   static int gl3882_;
   static union dtype_ gl3883_;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   int    md__ = S_int_VOID_;
   ptr    co__ = 0;
   int    d__ = S_int_VOID_;

   if ((! CLA148_parametrized_p_(self__))) {
      res__ = (int)0;
   }
   else {
      if ((IATT_(self__,80) != 0)) {
         res__ = (int)IATT_(self__,80);
      }
      else {
         i__ = (int)0;
         psz__ = S_int_VOID_;
         if ((PATT_(self__,40) != 0)) {
            psz__ = (int)IATT_(PATT_(self__,40),16);
         }
         else {
         }
         md__ = (int)0;
         while (1) {
            if ((i__ >= psz__)) {
               goto goto_tag_3884_;
            }
            else {
            }
            gl3881_ = PATT_(PATT_(self__,40), 28 + ((i__) << 2));
            cache_dispatch_(gl3881_,418,gl3882_,INTVAL_(gl3883_));
            co__ = (ptr)PFN_(gl3883_)(gl3881_);
            d__ = (int)CLA148_compute_pdepth_(co__);
            if ((d__ > md__)) {
               md__ = (int)d__;
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_3884_: ;
         IATT_(self__,80) = (int)(1 + md__);
         res__ = (int)IATT_(self__,80);
      }
   }

   ret0__:
   return (res__);
}

CLA148_add_unique_anc_(self__,anc_index__)
ptr self__;
int anc_index__;
{
   ptr    co__ = 0;
   int    anc_name__ = S_int_VOID_;
   ptr    co1__ = 0;

   co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,anc_index__);
   if (LIS98_not_in_(PATT_(self__,64),anc_index__)) {
      anc_name__ = (int)IATT_(PATT_(co__,28),12);
      switch (anc_name__) {
         case (42) :
         case (44) :
         case (45) :
         case (47) :
         case (48) :
         case (54) :
            if (((IATT_(self__,96) != 0) & (IATT_(self__,96) != anc_index__))) {
               CS_160_incompatible_inheritance_(0,CLA148_full_name_(self__),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,96))),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,anc_index__)));
            }
            else {
            }
            IATT_(self__,96) = (int)anc_index__;
            break;
         case (38) :
         case (39) :
         case (40) :
         case (41) :
            if ((IATT_(self__,96) != 0)) {
               co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(self__,96));
               if ((IATT_(PATT_(co1__,28),12) == anc_name__)) {
               }
               else {
                  CS_160_incompatible_inheritance_(0,CLA148_full_name_(self__),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,96))),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,anc_index__)));
               }
            }
            else {
            }
            IATT_(self__,96) = (int)anc_index__;
            break;
         default:
            ;
            ;
      }
      PATT_(self__,64) = (ptr)LIS98_push_(PATT_(self__,64),anc_index__);
   }
   else {
   }

   ret0__:
   return;
}

CLA148_compute_anc_(self__)
ptr self__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,15,ls535_,"Missing parent");
   SATHER_STR_(20,29,ls538_,"Array should be parametrized");
   SATHER_STR_(20,32,ls539_,"Error in computing element type");
   int gl3885_;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    p_anc__ = 0;
   ptr    parent__ = 0;
   int    j__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   ptr    co__ = 0;
   int    my_name__ = S_int_VOID_;
   int    i_379_ = S_int_VOID_;
   ptr    co_380_ = 0;
   int    prev_ctype__ = S_int_VOID_;

   if ((PATT_(self__,64) != 0)) {
      goto ret0__;
   }
   else {
   }
   PATT_(self__,64) = (ptr)LIS98_create_(PATT_(self__,64),5);
   sz__ = S_int_VOID_;
   if ((PATT_(self__,52) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,52),4);
   }
   else {
   }
   i__ = (int)0;
   p_anc__ = (ptr)LIS98_create_(p_anc__,10);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3886_;
      }
      else {
      }
      parent__ = (ptr)PATT_(PATT_(self__,52), 16 + ((i__) << 2));
      if ((parent__ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls535_));
         goto ret0__;
      }
      else {
      }
      CLA148_compute_anc_(parent__);
      p_anc__ = (ptr)LIS98_append_(p_anc__,PATT_(parent__,64));
      p_anc__ = (ptr)LIS98_push_(p_anc__,IATT_(parent__,20));
      i__ = (int)(i__ + 1);
   }
goto_tag_3886_: ;
   j__ = (int)0;
   psz__ = (int)IATT_(p_anc__,4);
   while (1) {
      if ((j__ >= psz__)) {
         goto goto_tag_3887_;
      }
      else {
      }
      if ((IATT_(p_anc__, 16 + ((j__) << 2)) == IATT_(self__,20))) {
         CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
      }
      else {
      }
      CLA148_add_unique_anc_(self__,IATT_(p_anc__, 16 + ((j__) << 2)));
      j__ = (int)(j__ + 1);
   }
goto_tag_3887_: ;
   if ((IATT_(self__,96) != 0)) {
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(self__,96));
      if (((((((IATT_(PATT_(co__,28),12) == 44) | (IATT_(PATT_(co__,28),12) == 47)) | (IATT_(PATT_(co__,28),12) == 48)) | (IATT_(PATT_(co__,28),12) == 45)) | (IATT_(PATT_(co__,28),12) == 42)) | (IATT_(PATT_(co__,28),12) == 54))) {
         if ((IATT_(self__,56) > 0)) {
            CS_160_basic_with_attributes_(0,IATT_(self__,24));
         }
         else {
         }
      }
      else {
         if (((((IATT_(PATT_(co__,28),12) == 38) | (IATT_(PATT_(co__,28),12) == 39)) | (IATT_(PATT_(co__,28),12) == 40)) | (IATT_(PATT_(co__,28),12) == 41))) {
            if ((PATT_(co__,40) != 0)) {
               if ((IATT_(PATT_(co__,40),16) != 1)) {
                  ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls538_));
                  goto ret0__;
               }
               else {
               }
            }
            else {
               ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls538_));
               goto ret0__;
            }
            if ((PATT_(co__,124) == 0)) {
               ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls539_));
               PATT_(co__,124) = (ptr)PATT_(PATT_(co__,40), 28 + ((0) << 2));
            }
            else {
            }
            PATT_(self__,124) = (ptr)PATT_(co__,124);
            if ((IATT_(self__,100) == 0)) {
               IATT_(self__,100) = (int)IATT_(co__,100);
            }
            else {
            }
         }
         else {
         }
      }
   }
   else {
      my_name__ = (int)IATT_(PATT_(self__,28),12);
      if (((((my_name__ == 38) | (my_name__ == 39)) | (my_name__ == 40)) | (my_name__ == 41))) {
         if ((PATT_(self__,40) != 0)) {
            if ((IATT_(PATT_(self__,40),16) != 1)) {
               ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls538_));
               goto ret0__;
            }
            else {
            }
         }
         else {
            ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls538_));
            goto ret0__;
         }
         if ((PATT_(self__,124) == 0)) {
            PATT_(self__,124) = (ptr)PATT_(PATT_(self__,40), 28 + ((0) << 2));
         }
         else {
         }
         switch (my_name__) {
            case (38) :
               IATT_(self__,96) = (int)IATT_(self__,20);
               IATT_(self__,100) = (int)1;
               break;
            case (39) :
               IATT_(self__,96) = (int)IATT_(self__,20);
               IATT_(self__,100) = (int)2;
               break;
            case (40) :
               IATT_(self__,96) = (int)IATT_(self__,20);
               IATT_(self__,100) = (int)3;
               break;
            case (41) :
               IATT_(self__,96) = (int)IATT_(self__,20);
               IATT_(self__,100) = (int)4;
               break;
            default:
               ;
               ;
         }
      }
      else {
      }
   }
   CLA148_update_ctype_(self__);
   if (((IATT_(self__,104) == 1) & (IATT_(self__,96) != RES97_FOB_ici_))) {
      CLA148_add_unique_anc_(self__,RES97_OB_ici_);
   }
   else {
   }
   if ((IATT_(self__,96) != 0)) {
      gl3885_= IATT_(self__,96);
      switch (gl3885_) {
         default:
            if (RES97_BOOL_ici_ == gl3885_) {
               i_379_ = (int)0;
               while (1) {
                  if ((i_379_ >= psz__)) {
                     goto goto_tag_3888_;
                  }
                  else {
                  }
                  if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                     CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                  }
                  else {
                  }
                  co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                  if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                     CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                     goto goto_tag_3888_;
                  }
                  else {
                  }
                  i_379_ = (int)(i_379_ + 1);
               }
            goto_tag_3888_: ;
            }
            else {
               if (RES97_CHAR_ici_ == gl3885_) {
                  i_379_ = (int)0;
                  while (1) {
                     if ((i_379_ >= psz__)) {
                        goto goto_tag_3888_1_;
                     }
                     else {
                     }
                     if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                        CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                     }
                     else {
                     }
                     co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                     if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                        CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                        goto goto_tag_3888_1_;
                     }
                     else {
                     }
                     i_379_ = (int)(i_379_ + 1);
                  }
               goto_tag_3888_1_: ;
               }
               else {
                  if (RES97_INT_ici_ == gl3885_) {
                     i_379_ = (int)0;
                     while (1) {
                        if ((i_379_ >= psz__)) {
                           goto goto_tag_3888_2_;
                        }
                        else {
                        }
                        if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                           CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                        }
                        else {
                        }
                        co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                        if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                           CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                           goto goto_tag_3888_2_;
                        }
                        else {
                        }
                        i_379_ = (int)(i_379_ + 1);
                     }
                  goto_tag_3888_2_: ;
                  }
                  else {
                     if (RES97_REAL_ici_ == gl3885_) {
                        i_379_ = (int)0;
                        while (1) {
                           if ((i_379_ >= psz__)) {
                              goto goto_tag_3888_3_;
                           }
                           else {
                           }
                           if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                              CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                           }
                           else {
                           }
                           co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                           if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                              CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                              goto goto_tag_3888_3_;
                           }
                           else {
                           }
                           i_379_ = (int)(i_379_ + 1);
                        }
                     goto_tag_3888_3_: ;
                     }
                     else {
                        if (RES97_DOUBLE_ici_ == gl3885_) {
                           i_379_ = (int)0;
                           while (1) {
                              if ((i_379_ >= psz__)) {
                                 goto goto_tag_3888_4_;
                              }
                              else {
                              }
                              if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                                 CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                              }
                              else {
                              }
                              co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                              if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                                 CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                                 goto goto_tag_3888_4_;
                              }
                              else {
                              }
                              i_379_ = (int)(i_379_ + 1);
                           }
                        goto_tag_3888_4_: ;
                        }
                        else {
                           if (RES97_FOB_ici_ == gl3885_) {
                              i_379_ = (int)0;
                              while (1) {
                                 if ((i_379_ >= psz__)) {
                                    goto goto_tag_3888_5_;
                                 }
                                 else {
                                 }
                                 if ((IATT_(p_anc__, 16 + ((i_379_) << 2)) == IATT_(self__,20))) {
                                    CS_160_cyclic_inheritance_(0,PATT_(PATT_(self__,28),16),IATT_(self__,24),CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                                 }
                                 else {
                                 }
                                 co_380_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(p_anc__, 16 + ((i_379_) << 2)));
                                 if ((IATT_(co_380_,96) != IATT_(self__,96))) {
                                    CS_160_basic_cross_inheritance_(0,CLA148_full_name_(CLA93_at_index_(GLO68_class_inst_,IATT_(self__,20))));
                                    goto goto_tag_3888_5_;
                                 }
                                 else {
                                 }
                                 i_379_ = (int)(i_379_ + 1);
                              }
                           goto_tag_3888_5_: ;
                           }
                           else {
                              ;
                           }
                        }
                     }
                  }
               }
            }
            ;
      }
   }
   else {
   }
   prev_ctype__ = (int)OLD101_get_ctype_(0,self__);
   if (((prev_ctype__ < 0) | (prev_ctype__ != IATT_(self__,104)))) {
      CATT_(self__,14) = (char)1;
      CATT_(self__,12) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

CLA148_compute_des_(self__)
ptr self__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,19,ls553_,"Missing class inst");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    ancestor__ = 0;

   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,64) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,64),4);
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3889_;
      }
      else {
      }
      ancestor__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(PATT_(self__,64), 16 + ((i__) << 2)));
      if ((ancestor__ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls553_));
         goto ret0__;
      }
      else {
      }
      PATT_(ancestor__,68) = (ptr)INT164_insert_(PATT_(ancestor__,68),IATT_(self__,20));
      i__ = (int)(i__ + 1);
   }
goto_tag_3889_: ;

   ret0__:
   return;
}

CLA148_compute_pdes_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;
   ptr    co__ = 0;

   i__ = (int)0;
   if ((PATT_(self__,72) == 0)) {
      PATT_(self__,72) = (ptr)INT164_create_(0);
   }
   else {
   }
   while (1) {
      if ((i__ >= IATT_(GLO68_class_inst_,24))) {
         goto goto_tag_3890_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if (((co__ != 0) & (co__ != self__))) {
         if ((PATT_(co__,28) == PATT_(self__,28))) {
            if (LST102_param_type_conforms_to_(PATT_(co__,40),PATT_(self__,40))) {
               PATT_(self__,72) = (ptr)INT164_insert_(PATT_(self__,72),i__);
               CLA148_add_new_des_(self__,co__);
               CLA148_add_new_anc_(co__,self__);
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
goto_tag_3890_: ;

   ret0__:
   return;
}

CLA148_add_new_des_(self__,newDes__)
ptr self__;
ptr newDes__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,23,ls561_,"Missing ancestor class");
   ptr    ancestor__ = 0;
   int    sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    ancestor_381_ = 0;

   ancestor__ = S_ptr_VOID_;
   if (INT164_get_(PATT_(self__,76),IATT_(newDes__,20))) {
      goto ret0__;
   }
   else {
   }
   PATT_(self__,76) = (ptr)INT164_insert_(PATT_(self__,76),IATT_(newDes__,20));
   PATT_(self__,76) = (ptr)INT164_union_(PATT_(self__,76),PATT_(newDes__,76));
   sz__ = S_int_VOID_;
   i__ = S_int_VOID_;
   if ((PATT_(self__,64) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,64),4);
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3891_;
      }
      else {
      }
      ancestor_381_ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(PATT_(self__,64), 16 + ((i__) << 2)));
      if ((ancestor_381_ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls561_));
      }
      else {
      }
      CLA148_add_new_des_(ancestor_381_,newDes__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3891_: ;

   ret0__:
   return;
}

CLA148_add_new_anc_(self__,newAnc__)
ptr self__;
ptr newAnc__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,25,ls568_,"Missing descendant class");
   ptr    des_cur__ = 0;
   ptr    descendant__ = 0;

   if ((! LIS98_not_in_(PATT_(self__,64),IATT_(newAnc__,20)))) {
      goto ret0__;
   }
   else {
   }
   PATT_(self__,64) = (ptr)LIS98_push_(PATT_(self__,64),IATT_(newAnc__,20));
   PATT_(self__,64) = (ptr)LIS98_union_(PATT_(self__,64),PATT_(newAnc__,64));
   des_cur__ = (ptr)INT164_cursor_(PATT_(self__,76));
   while (1) {
      if (CATT_(des_cur__,4)) {
         goto goto_tag_3892_;
      }
      else {
      }
      descendant__ = (ptr)CLA93_at_index_(GLO68_class_inst_,INT214_item_(des_cur__));
      if ((descendant__ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls568_));
      }
      else {
      }
      CLA148_add_new_anc_(descendant__,newAnc__);
      (void)INT214_next_(des_cur__);
   }
goto_tag_3892_: ;

   ret0__:
   return;
}

CLA148_adjust_parent_alldes_(self__)
ptr self__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,19,ls553_,"Missing class inst");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    ancestor__ = 0;
   int    j__ = S_int_VOID_;
   int    alldes_sz__ = S_int_VOID_;
   int    index__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = S_int_VOID_;
   if ((PATT_(self__,64) != 0)) {
      sz__ = (int)IATT_(PATT_(self__,64),4);
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3893_;
      }
      else {
      }
      ancestor__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(PATT_(self__,64), 16 + ((i__) << 2)));
      if ((ancestor__ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls262_),(ptr)(&ls553_));
      }
      else {
      }
      j__ = (int)0;
      alldes_sz__ = (int)IATT_(PATT_(self__,76),12);
      while (1) {
         if ((j__ >= alldes_sz__)) {
            goto goto_tag_3894_;
         }
         else {
         }
         index__ = (int)IATT_(PATT_(self__,76), 16 + ((j__) << 2));
         if ((index__ > 1)) {
            PATT_(ancestor__,76) = (ptr)INT164_insert_(PATT_(ancestor__,76),index__);
         }
         else {
         }
         j__ = (int)(j__ + 1);
      }
   goto_tag_3894_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_3893_: ;

   ret0__:
   return;
}

CLA148_compute_time_stamp_(self__)
ptr self__;
{
   ptr gl3895_;
   static int gl3896_;
   static union dtype_ gl3897_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;
   int    psz__ = S_int_VOID_;
   ptr    co_382_ = 0;

   if (CATT_(self__,15)) {
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   sz__ = (int)IATT_(PATT_(self__,64),4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3898_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(PATT_(self__,64), 16 + ((i__) << 2)));
      if ((co__ != self__)) {
         CLA148_compute_time_stamp_(co__);
         if ((IATT_(co__,132) > IATT_(self__,132))) {
            IATT_(self__,132) = (int)IATT_(co__,132);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3898_: ;
   i__ = (int)0;
   psz__ = S_int_VOID_;
   if ((PATT_(self__,40) != 0)) {
      psz__ = (int)IATT_(PATT_(self__,40),16);
   }
   else {
   }
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_3899_;
      }
      else {
      }
      gl3895_ = PATT_(PATT_(self__,40), 28 + ((i__) << 2));
      cache_dispatch_(gl3895_,418,gl3896_,INTVAL_(gl3897_));
      co_382_ = (ptr)PFN_(gl3897_)(gl3895_);
      if ((IATT_(co_382_,132) > IATT_(self__,132))) {
         IATT_(self__,132) = (int)IATT_(co_382_,132);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3899_: ;
   if (((IATT_(self__,128) != OLD101_get_def_time_stamp_(0,self__)) | (IATT_(self__,132) != OLD101_get_time_stamp_(0,self__)))) {
      CATT_(self__,12) = (char)1;
   }
   else {
   }
   CATT_(self__,15) = (char)1;

   ret0__:
   return;
}

CLA148_compute_attr_offsets_(self__)
ptr self__;
{
   ptr gl3900_;
   static int gl3901_;
   static union dtype_ gl3902_;
   int gl383_;
   ptr gl3903_;
   static int gl3904_;
   static union dtype_ gl3905_;
   int gl384_;
   char    contains_ptr__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    nextloc__ = S_int_VOID_;
   ptr    attr_feat__ = 0;

   LST133_compactAttr_(PATT_(self__,32));
   if ((IATT_(self__,104) == 0)) {
      CLA148_update_ctype_(self__);
   }
   else {
   }
   switch (IATT_(self__,104)) {
      case (4) :
         IATT_(self__,60) = (int)1;
         IATT_(self__,108) = (int)4;
         break;
      case (3) :
         IATT_(self__,60) = (int)1;
         IATT_(self__,108) = (int)4;
         break;
      case (5) :
         IATT_(self__,60) = (int)1;
         IATT_(self__,108) = (int)8;
         break;
      case (2) :
         IATT_(self__,60) = (int)1;
         IATT_(self__,108) = (int)1;
         break;
      default:
         contains_ptr__ = (char)0;
         i__ = (int)0;
         sz__ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            sz__ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         nextloc__ = (int)4;
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_3906_;
            }
            else {
            }
            gl3900_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            gl383_ = TYPE_(gl3900_);
            if ((gl383_ == 153)) {
               attr_feat__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
               GLO68_curr_feature_ = (ptr)attr_feat__;
               nextloc__ = (int)ATT153_compute_own_offset_(attr_feat__,nextloc__);
               if ((IATT_(attr_feat__,48) == 1)) {
                  contains_ptr__ = (char)1;
               }
               else {
               }
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_3906_: ;
         if ((IATT_(self__,100) == 0)) {
            if ((! contains_ptr__)) {
               IATT_(self__,60) = (int)1;
            }
            else {
               IATT_(self__,60) = (int)0;
            }
         }
         else {
            gl3903_ = PATT_(self__,124);
            cache_dispatch_(gl3903_,374,gl3904_,INTVAL_(gl3905_));
            gl384_ = IFN_(gl3905_)(gl3903_);
            if ((gl384_ != 1)) {
               if ((! contains_ptr__)) {
                  IATT_(self__,60) = (int)1;
               }
               else {
                  IATT_(self__,60) = (int)0;
               }
            }
            else {
               IATT_(self__,60) = (int)0;
            }
         }
         IATT_(self__,108) = (int)INT15_lshift_(INT15_rshift_((nextloc__ + 3),2),2);
         ;
   }

   ret0__:
   return;
}

CLA148_pre_semant_(self__)
ptr self__;
{
   ptr gl3907_;
   static int gl3908_;
   static union dtype_ gl3909_;
   int gl385_;
   ptr gl3910_;
   static int gl3911_;
   static union dtype_ gl3912_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   GLO68_pre_semant_ = (char)1;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3913_;
      }
      else {
      }
      gl3907_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl385_ = TYPE_(gl3907_);
      if ((gl385_ != 156)) {
         GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         gl3910_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl3910_,588,gl3911_,INTVAL_(gl3912_));
         VFN_(gl3912_)(gl3910_,PATT_(self__,48));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3913_: ;
   GLO68_pre_semant_ = (char)0;

   ret0__:
   return;
}

CLA148_semant_(self__)
ptr self__;
{
   ptr gl3914_;
   static int gl3915_;
   static union dtype_ gl3916_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3917_;
      }
      else {
      }
      GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl3914_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      cache_dispatch_(gl3914_,588,gl3915_,INTVAL_(gl3916_));
      VFN_(gl3916_)(gl3914_,PATT_(self__,48));
      i__ = (int)(i__ + 1);
   }
goto_tag_3917_: ;
   if ((PATT_(self__,36) != 0)) {
      CATT_(CLA148_get_invariant_(self__),11) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

CLA148_mark_callees_and_callers_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    index__ = S_int_VOID_;
   ptr    co__ = 0;
   int    i_386_ = S_int_VOID_;
   int    sz_387_ = S_int_VOID_;
   int    index_388_ = S_int_VOID_;
   ptr    co_389_ = 0;
   ptr    empty_set__ = 0;
   int    i_390_ = S_int_VOID_;
   int    sz_391_ = S_int_VOID_;
   int    ith_cname__ = S_int_VOID_;
   ptr    new_macros__ = 0;
   ptr    old_macros__ = 0;
   ptr    changes__ = 0;

   if ((IATT_(self__,20) != RES97_OB_ici_)) {
      if (CATT_(self__,12)) {
         i__ = S_int_VOID_;
         sz__ = (int)IATT_(PATT_(self__,136),12);
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_3918_;
            }
            else {
            }
            index__ = (int)IATT_(PATT_(self__,136), 16 + ((i__) << 2));
            if ((index__ > 0)) {
               co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
               CATT_(co__,13) = (char)1;
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_3918_: ;
         i_386_ = S_int_VOID_;
         sz_387_ = (int)IATT_(PATT_(self__,140),12);
         while (1) {
            if ((i_386_ >= sz_387_)) {
               goto goto_tag_3919_;
            }
            else {
            }
            index_388_ = (int)IATT_(PATT_(self__,140), 16 + ((i_386_) << 2));
            if ((index_388_ > 0)) {
               co_389_ = (ptr)CLA93_at_index_(GLO68_class_inst_,index_388_);
               CATT_(co_389_,13) = (char)1;
            }
            else {
            }
            i_386_ = (int)(i_386_ + 1);
         }
      goto_tag_3919_: ;
      }
      else {
         if ((! CATT_(self__,13))) {
            empty_set__ = (ptr)INT164_create_(0);
            i_390_ = S_int_VOID_;
            sz_391_ = (int)IATT_(PATT_(self__,112),12);
            while (1) {
               if ((i_390_ >= sz_391_)) {
                  goto goto_tag_3920_;
               }
               else {
               }
               ith_cname__ = (int)IATT_(PATT_(self__,112), 16 + ((i_390_) << 2));
               if ((ith_cname__ > 0)) {
                  new_macros__ = (ptr)copy_(INT86_get_(GLO68_c_macros_,ith_cname__),1);
                  old_macros__ = (ptr)INT86_get_(PATT_(GLO68_class_stat_tbl_,16),ith_cname__);
                  if ((new_macros__ != old_macros__)) {
                     if (((new_macros__ != 0) & (old_macros__ != 0))) {
                        changes__ = (ptr)INT164_sym_difference_(new_macros__,old_macros__);
                        if ((! INT164_is_a_subset_of_(changes__,empty_set__))) {
                           CATT_(self__,13) = (char)1;
                           goto goto_tag_3920_;
                        }
                        else {
                        }
                     }
                     else {
                     }
                  }
                  else {
                  }
               }
               else {
               }
               i_390_ = (int)(i_390_ + 1);
            }
         goto_tag_3920_: ;
         }
         else {
         }
      }
   }
   else {
   }

   ret0__:
   return;
}

CLA148_misc_info_(self__)
ptr self__;
{
   ptr gl3921_;
   static int gl3922_;
   static union dtype_ gl3923_;
   ptr gl3924_;
   static int gl3925_;
   static union dtype_ gl3926_;
   ptr gl3927_;
   static int gl3928_;
   static union dtype_ gl3929_;
   int gl392_;
   ptr gl3930_;
   static int gl3931_;
   static union dtype_ gl3932_;
   ptr gl3933_;
   static int gl3934_;
   static union dtype_ gl3935_;
   int gl393_;
   ptr gl3936_;
   static int gl3937_;
   static union dtype_ gl3938_;
   ptr gl3939_;
   static int gl3940_;
   static union dtype_ gl3941_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;
   ptr    fo__ = 0;
   ptr    shared_feat__ = 0;
   ptr    const_feat__ = 0;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3942_;
      }
      else {
      }
      fo__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      GLO68_curr_feature_ = (ptr)fo__;
      gl3921_ = fo__;
      cache_dispatch_(gl3921_,601,gl3922_,INTVAL_(gl3923_));
      VFN_(gl3923_)(gl3921_);
      gl3924_ = fo__;
      switch (TYPE_(gl3924_)) {
         case (152) :
            shared_feat__ = (ptr)fo__;
            if ((PATT_(shared_feat__,48) != 0)) {
               GLO68_tmpct_ = (ptr)LIS98_append_(GLO68_tmpct_,PATT_(shared_feat__,48));
            }
            else {
            }
            break;
         case (151) :
            const_feat__ = (ptr)fo__;
            if ((PATT_(const_feat__,48) != 0)) {
               GLO68_tmpct_ = (ptr)LIS98_append_(GLO68_tmpct_,PATT_(const_feat__,48));
            }
            else {
            }
            break;
         default:
            ;
            ;
      }
      gl3927_ = fo__;
      gl392_ = TYPE_(gl3927_);
      if ((gl392_ == 156)) {
         gl3930_ = fo__;
         cache_dispatch_(gl3930_,607,gl3931_,INTVAL_(gl3932_));
         VFN_(gl3932_)(gl3930_);
      }
      else {
         gl3933_ = fo__;
         gl393_ = TYPE_(gl3933_);
         if ((gl393_ == 151)) {
            gl3936_ = fo__;
            cache_dispatch_(gl3936_,608,gl3937_,INTVAL_(gl3938_));
            VFN_(gl3938_)(gl3936_);
         }
         else {
         }
      }
      gl3939_ = fo__;
      cache_dispatch_(gl3939_,609,gl3940_,INTVAL_(gl3941_));
      VFN_(gl3941_)(gl3939_);
      i__ = (int)(i__ + 1);
   }
goto_tag_3942_: ;

   ret0__:
   return;
}

CLA148_cprint_macros_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl3943_;
   static int gl3944_;
   static union dtype_ gl3945_;
   ptr    c_class__ = 0;
   ptr    allmacros__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    ith_cname__ = S_int_VOID_;
   ptr    macros__ = 0;
   ptr    feat__ = 0;
   int    i_394_ = S_int_VOID_;
   int    sz_395_ = S_int_VOID_;
   int    ith_macro__ = S_int_VOID_;

   c_class__ = (ptr)CLA93_at_index_(GLO68_class_inst_,RES97_C_ici_);
   allmacros__ = (ptr)INT164_create_(0);
   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,112),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3946_;
      }
      else {
      }
      ith_cname__ = (int)IATT_(PATT_(self__,112), 16 + ((i__) << 2));
      if ((ith_cname__ > 0)) {
         macros__ = (ptr)INT86_get_(GLO68_c_macros_,ith_cname__);
         if ((macros__ != 0)) {
            allmacros__ = (ptr)INT164_union_(allmacros__,macros__);
         }
         else {
         }
         if ((c_class__ != 0)) {
            feat__ = (ptr)CLA148_get_feature_(c_class__,ith_cname__);
            gl3943_ = feat__;
            cache_dispatch_(gl3943_,618,gl3944_,INTVAL_(gl3945_));
            VFN_(gl3945_)(gl3943_,outfile__);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3946_: ;
   i_394_ = S_int_VOID_;
   sz_395_ = (int)IATT_(allmacros__,12);
   while (1) {
      if ((i_394_ >= sz_395_)) {
         goto goto_tag_3947_;
      }
      else {
      }
      ith_macro__ = (int)IATT_(allmacros__, 16 + ((i_394_) << 2));
      if ((ith_macro__ > 0)) {
         (void)SAT99_inc_ln_(SAT99_c_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,ith_macro__)),'\n'),1);
      }
      else {
      }
      i_394_ = (int)(i_394_ + 1);
   }
goto_tag_3947_: ;
   (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);

   ret0__:
   return;
}

CLA148_cprint_header_and_macros_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls622_,"/* ");
   SATHER_STR_(20,20,ls623_,".c : Sather class: ");
   SATHER_STR_(20,3,ls624_,", ");
   SATHER_STR_(20,6,ls627_," */\n\n");
   SATHER_STR_(20,19,ls628_,"#include \"all_.h\"\n");
   SATHER_STR_(20,11,ls629_,"#include \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,35,ls636_,"\nstatic char __sather_compiled__;\n");

   (void)SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls622_)),PATT_(self__,92)),(ptr)(&ls623_)),CLA148_full_name_(self__)),(ptr)(&ls624_));
   SAT95_cs_options_cprint_(0,outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls627_));
   (void)SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls628_)),(ptr)(&ls629_)),(ptr)(&gs2280_)),(ptr)(&ls632_));
   if (COM82_verbose_code_) {
      (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls629_)),(ptr)(&gs2279_)),(ptr)(&ls632_));
      (void)SAT99_inc_ln_(outfile__,1);
   }
   else {
   }
   (void)SAT99_s_(outfile__,(ptr)(&ls636_));
   (void)SAT99_inc_ln_(outfile__,6);
   CLA148_cprint_macros_(self__,outfile__);

   ret0__:
   return;
}

CLA148_cprint_externs_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,9,ls643_,"Code <3,");
   SATHER_STR_(20,3,ls644_,">\n");
   ptr gl3948_;
   static int gl3949_;
   static union dtype_ gl3950_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    key__ = S_int_VOID_;
   int    fnm__ = S_int_VOID_;
   int    ci__ = S_int_VOID_;
   ptr    fo__ = 0;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,116),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3951_;
      }
      else {
      }
      key__ = (int)IATT_(PATT_(self__,116), 16 + ((i__) << 2));
      if ((key__ != (- 1))) {
         fnm__ = (int)GLO94_featname_from_key_(0,key__);
         ci__ = (int)GLO94_classind_from_key_(0,key__);
         fo__ = (ptr)CLA148_get_feature_(CLA93_at_index_(GLO68_class_inst_,ci__),fnm__);
         if ((fo__ == 0)) {
            ERR96_compiler_error_msg_(0,(ptr)(&ls262_),STR20_s_(STR20_i_(STR20_c_(STR20_i_(STR20_c_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls643_)),key__),','),fnm__),','),ci__),(ptr)(&ls644_)));
         }
         else {
         }
         gl3948_ = fo__;
         cache_dispatch_(gl3948_,618,gl3949_,INTVAL_(gl3950_));
         VFN_(gl3950_)(gl3948_,outfile__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3951_: ;

   ret0__:
   return;
}

CLA148_cprint_ext_strs_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,8,ls647_,"extern ");
   SATHER_STR_(20,3,ls650_,";\n");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    ith_ext_str__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,120),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3952_;
      }
      else {
      }
      ith_ext_str__ = (int)IATT_(PATT_(self__,120), 16 + ((i__) << 2));
      if ((ith_ext_str__ > 0)) {
         (void)SAT99_s_(outfile__,(ptr)(&ls647_));
         GLO94_cprint_sather_str_type_(0,outfile__);
         GLO94_cprint_global_tmpnm_str_(0,ith_ext_str__,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3952_: ;

   ret0__:
   return;
}

CLA148_cprint_declns_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls652_,"\n\n");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl3953_;
   static int gl3954_;
   static union dtype_ gl3955_;
   int gl396_;
   ptr gl3956_;
   static int gl3957_;
   static union dtype_ gl3958_;
   int gl397_;
   ptr gl3959_;
   static int gl3960_;
   static union dtype_ gl3961_;
   ptr gl3962_;
   static int gl3963_;
   static union dtype_ gl3964_;
   int gl398_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;
   ptr    const_feat__ = 0;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls652_)),2);
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3965_;
      }
      else {
      }
      gl3953_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl396_ = TYPE_(gl3953_);
      gl3956_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl397_ = TYPE_(gl3956_);
      if (((gl396_ != 153) & (gl397_ != 151))) {
         GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         if (GLO94_handle_feature_p_(0,PATT_(PATT_(self__,32), 24 + ((i__) << 2)))) {
            gl3959_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl3959_,654,gl3960_,INTVAL_(gl3961_));
            VFN_(gl3961_)(gl3959_,outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         }
         else {
         }
      }
      else {
         gl3962_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         gl398_ = TYPE_(gl3962_);
         if ((gl398_ == 151)) {
            const_feat__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            GLO68_curr_feature_ = (ptr)const_feat__;
            if (GLO94_handle_feature_p_(0,PATT_(PATT_(self__,32), 24 + ((i__) << 2)))) {
               CON151_cprint_decln_with_poss_init_(const_feat__,outfile__);
               (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
            }
            else {
            }
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3965_: ;

   ret0__:
   return;
}

CLA148_cprint_attr_tbl_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,14,ls657_,"int attr_ent_");
   SATHER_STR_(20,5,ls658_,"[]={");
   SATHER_STR_(20,2,ls659_,",");
   SATHER_STR_(20,5,ls663_,"};\n\n");
   ptr gl3966_;
   static int gl3967_;
   static union dtype_ gl3968_;
   ptr gl3969_;
   static int gl3970_;
   static union dtype_ gl3971_;
   int gl399_;
   ptr gl3972_;
   static int gl3973_;
   static union dtype_ gl3974_;
   ptr gl3975_;
   static int gl3976_;
   static union dtype_ gl3977_;
   int gl400_;
   ptr gl3978_;
   static int gl3979_;
   static union dtype_ gl3980_;
   int gl401_;
   ptr gl3981_;
   static int gl3982_;
   static union dtype_ gl3983_;
   ptr    e_type__ = 0;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   (void)SAT99_s_(SAT99_i_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls657_)),PATT_(self__,84)),(ptr)(&ls658_)),IATT_(self__,108)),(ptr)(&ls659_)),IATT_(self__,100)),(ptr)(&ls659_));
   e_type__ = (ptr)PATT_(self__,124);
   if ((e_type__ == 0)) {
      (void)SAT99_i_(SAT99_s_(SAT99_i_(outfile__,0),(ptr)(&ls659_)),IATT_(self__,56));
   }
   else {
      gl3966_ = e_type__;
      cache_dispatch_(gl3966_,418,gl3967_,INTVAL_(gl3968_));
      (void)SAT99_i_(SAT99_s_(SAT99_i_(outfile__,IATT_(PFN_(gl3968_)(gl3966_),104)),(ptr)(&ls659_)),IATT_(self__,56));
   }
   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3984_;
      }
      else {
      }
      gl3969_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl399_ = TYPE_(gl3969_);
      if ((gl399_ == 153)) {
         gl3975_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl3975_,661,gl3976_,INTVAL_(gl3977_));
         gl3972_ = PFN_(gl3977_)(gl3975_);
         cache_dispatch_(gl3972_,374,gl3973_,INTVAL_(gl3974_));
         gl400_ = IFN_(gl3974_)(gl3972_);
         (void)SAT99_i_(SAT99_c_(outfile__,','),gl400_);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3984_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_3985_;
      }
      else {
      }
      gl3978_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl401_ = TYPE_(gl3978_);
      if ((gl401_ == 153)) {
         (void)SAT99_c_(outfile__,',');
         gl3981_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl3981_,662,gl3982_,INTVAL_(gl3983_));
         VFN_(gl3983_)(gl3981_,outfile__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3985_: ;
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls663_)),2);

   ret0__:
   return;
}

CLA148_cprint_feat_tbl_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,21,ls665_,"char *feat_typename_");
   SATHER_STR_(20,3,ls666_,"=\"");
   SATHER_STR_(20,4,ls667_,"\";\n");
   SATHER_STR_(20,23,ls668_,"unsigned int feat_ent_");
   SATHER_STR_(20,5,ls658_,"[]={");
   SATHER_STR_(20,2,ls659_,",");
   SATHER_STR_(20,22,ls669_,",(int)&feat_typename_");
   SATHER_STR_(20,3,ls679_,",0");
   SATHER_STR_(20,5,ls663_,"};\n\n");
   ptr gl3986_;
   static int gl3987_;
   static union dtype_ gl3988_;
   ptr gl3989_;
   static int gl3990_;
   static union dtype_ gl3991_;
   int gl402_;
   ptr gl3992_;
   static int gl3993_;
   static union dtype_ gl3994_;
   ptr gl3995_;
   static int gl3996_;
   static union dtype_ gl3997_;
   ptr gl3998_;
   static int gl3999_;
   static union dtype_ gl4000_;
   ptr gl4001_;
   static int gl4002_;
   static union dtype_ gl4003_;
   ptr gl4004_;
   static int gl4005_;
   static union dtype_ gl4006_;
   int    fsz__ = S_int_VOID_;
   ptr    e_type__ = 0;
   int    i__ = S_int_VOID_;
   ptr    a_featob_s__ = 0;
   ptr    satype__ = 0;
   ptr    s_featob_s__ = 0;
   ptr    satype_403_ = 0;
   ptr    c_featob_s__ = 0;
   ptr    satype_404_ = 0;

   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls665_)),PATT_(self__,84)),(ptr)(&ls666_)),CLA148_full_name_(self__)),(ptr)(&ls667_)),1);
   (void)SAT99_s_(SAT99_i_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls668_)),PATT_(self__,84)),(ptr)(&ls658_)),fsz__),(ptr)(&ls659_)),IATT_(self__,104)),(ptr)(&ls659_));
   e_type__ = (ptr)PATT_(self__,124);
   if ((e_type__ == 0)) {
      (void)SAT99_i_(outfile__,0);
   }
   else {
      gl3986_ = e_type__;
      cache_dispatch_(gl3986_,418,gl3987_,INTVAL_(gl3988_));
      (void)SAT99_i_(outfile__,IATT_(PFN_(gl3988_)(gl3986_),20));
   }
   (void)SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls669_)),PATT_(self__,84));
   i__ = (int)0;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4007_;
      }
      else {
      }
      gl3989_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      cache_dispatch_(gl3989_,445,gl3990_,INTVAL_(gl3991_));
      gl402_ = IFN_(gl3991_)(gl3989_);
      (void)SAT99_i_(SAT99_c_(outfile__,','),gl402_);
      i__ = (int)(i__ + 1);
   }
goto_tag_4007_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4008_;
      }
      else {
      }
      gl3992_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      switch (TYPE_(gl3992_)) {
         case (153) :
            (void)SAT99_i_(SAT99_c_(outfile__,','),1);
            break;
         case (152) :
            (void)SAT99_i_(SAT99_c_(outfile__,','),3);
            break;
         case (151) :
            (void)SAT99_i_(SAT99_c_(outfile__,','),4);
            break;
         case (156) :
            (void)SAT99_i_(SAT99_c_(outfile__,','),2);
            break;
         default:
            ;
            ;
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4008_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4009_;
      }
      else {
      }
      gl3995_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      switch (TYPE_(gl3995_)) {
         case (153) :
            a_featob_s__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            satype__ = (ptr)PATT_(a_featob_s__,40);
            gl3998_ = satype__;
            cache_dispatch_(gl3998_,418,gl3999_,INTVAL_(gl4000_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl4000_)(gl3998_),20));
            break;
         case (152) :
            s_featob_s__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            satype_403_ = (ptr)PATT_(s_featob_s__,40);
            gl4001_ = satype_403_;
            cache_dispatch_(gl4001_,418,gl4002_,INTVAL_(gl4003_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl4003_)(gl4001_),20));
            break;
         case (151) :
            c_featob_s__ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            satype_404_ = (ptr)PATT_(c_featob_s__,40);
            gl4004_ = satype_404_;
            cache_dispatch_(gl4004_,418,gl4005_,INTVAL_(gl4006_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl4006_)(gl4004_),20));
            break;
         case (156) :
            (void)SAT99_s_(outfile__,(ptr)(&ls679_));
            break;
         default:
            ;
            ;
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4009_: ;
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls663_)),2);

   ret0__:
   return;
}

CLA148_cprint_des_tbl_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,22,ls685_,"unsigned int des_ent_");
   SATHER_STR_(20,5,ls658_,"[]={");
   SATHER_STR_(20,15,ls692_,"(unsigned int)");
   SATHER_STR_(20,4,ls693_,"};\n");
   int    last_cls_ind__ = S_int_VOID_;
   int    num_cls__ = S_int_VOID_;
   int    num_cols__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    l__ = S_int_VOID_;
   int    u__ = S_int_VOID_;
   int    des_bit_vector__ = S_int_VOID_;

   last_cls_ind__ = (int)(IATT_(GLO68_class_inst_,24) - 1);
   num_cls__ = (int)(last_cls_ind__ + 1);
   num_cols__ = S_int_VOID_;
   if ((INT15_mod_(num_cls__,32) == 0)) {
      num_cols__ = (int)(num_cls__ / 32);
   }
   else {
      num_cols__ = (int)((num_cls__ / 32) + 1);
   }
   j__ = S_int_VOID_;
   (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls685_)),PATT_(self__,84)),(ptr)(&ls658_));
   (void)SAT99_indent_(outfile__);
   while (1) {
      if ((j__ >= num_cols__)) {
         goto goto_tag_4010_;
      }
      else {
      }
      l__ = (int)(j__ * 32);
      u__ = (int)((l__ + 32) - 1);
      if ((u__ > last_cls_ind__)) {
         u__ = (int)last_cls_ind__;
      }
      else {
      }
      des_bit_vector__ = (int)GLO94_check_des_of_(0,IATT_(self__,20),l__,u__);
      if ((des_bit_vector__ == 0)) {
         (void)SAT99_i_(outfile__,des_bit_vector__);
      }
      else {
         if (COM82_k_and_r_c_) {
            (void)SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls692_)),des_bit_vector__);
         }
         else {
            (void)SAT99_c_(SAT99_i_(outfile__,des_bit_vector__),'u');
         }
      }
      j__ = (int)(j__ + 1);
      if ((j__ < num_cols__)) {
         (void)SAT99_c_(outfile__,',');
      }
      else {
      }
   }
goto_tag_4010_: ;
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls693_)),1);

   ret0__:
   return;
}

CLA148_cprint_routines_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl4011_;
   static int gl4012_;
   static union dtype_ gl4013_;
   int gl405_;
   ptr gl4014_;
   static int gl4015_;
   static union dtype_ gl4016_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4017_;
      }
      else {
      }
      gl4011_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl405_ = TYPE_(gl4011_);
      if ((gl405_ == 156)) {
         if (GLO94_handle_feature_p_(0,PATT_(PATT_(self__,32), 24 + ((i__) << 2)))) {
            GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            gl4014_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl4014_,695,gl4015_,INTVAL_(gl4016_));
            VFN_(gl4016_)(gl4014_,outfile__);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4017_: ;

   ret0__:
   return;
}

CLA148_cprint_ctype_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   switch (IATT_(self__,104)) {
      case (1) :
         (void)SAT99_s_(outfile__,(ptr)(&gs1215_));
         break;
      case (2) :
         (void)SAT99_s_(outfile__,(ptr)(&gs1216_));
         break;
      case (3) :
         (void)SAT99_s_(outfile__,(ptr)(&gs1217_));
         break;
      case (4) :
         (void)SAT99_s_(outfile__,(ptr)(&gs1218_));
         break;
      case (5) :
         (void)SAT99_s_(outfile__,(ptr)(&gs1219_));
         break;
      default:
         ;
         ;
   }

   ret0__:
   return;
}

int CLA148_cprint_extern_feats_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int res__ = S_int_VOID_;
   ptr gl4018_;
   static int gl4019_;
   static union dtype_ gl4020_;
   int gl406_;
   ptr gl4021_;
   static int gl4022_;
   static union dtype_ gl4023_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4024_;
      }
      else {
      }
      gl4018_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl406_ = TYPE_(gl4018_);
      if ((gl406_ != 153)) {
         GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         if (GLO94_handle_feature_p_(0,PATT_(PATT_(self__,32), 24 + ((i__) << 2)))) {
            gl4021_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
            cache_dispatch_(gl4021_,618,gl4022_,INTVAL_(gl4023_));
            VFN_(gl4023_)(gl4021_,outfile__);
         }
         else {
         }
      }
      else {
      }
      res__ = (int)(res__ + 1);
      i__ = (int)(i__ + 1);
   }
goto_tag_4024_: ;

   ret0__:
   return (res__);
}

CLA148_cprint_insert_feats_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,17,ls709_,"dispatch_table_[");
   SATHER_STR_(20,5,ls710_,"] = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl4025_;
   static int gl4026_;
   static union dtype_ gl4027_;
   char gl407_;
   ptr gl4028_;
   static int gl4029_;
   static union dtype_ gl4030_;
   ptr gl4031_;
   static int gl4032_;
   static union dtype_ gl4033_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;
   char    printed__ = S_char_VOID_;
   int    name__ = S_int_VOID_;
   int    where__ = S_int_VOID_;

   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   printed__ = (char)0;
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4034_;
      }
      else {
      }
      GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl4025_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      cache_dispatch_(gl4025_,705,gl4026_,INTVAL_(gl4027_));
      gl407_ = CATT_(gl4025_,INTVAL_(gl4027_));
      if ((gl407_ | GLO68_print_feat_info_)) {
         gl4028_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl4028_,445,gl4029_,INTVAL_(gl4030_));
         name__ = (int)IFN_(gl4030_)(gl4028_);
         where__ = (int)GLO94_help_insert_(0,IATT_(self__,20),name__);
         (void)SAT99_indent_(outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls709_)),where__),(ptr)(&ls710_)),GLO94_key_of_class_feat_(0,IATT_(self__,20),name__)),(ptr)(&ls650_)),1);
         (void)SAT99_indent_(outfile__);
         (void)SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls709_)),(where__ + 1)),(ptr)(&ls710_));
         gl4031_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl4031_,712,gl4032_,INTVAL_(gl4033_));
         VFN_(gl4033_)(gl4031_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         printed__ = (char)1;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4034_: ;
   if (printed__) {
      (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
   }
   else {
   }

   ret0__:
   return;
}

CLA148_cprint_post_rtcode_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   if (GLO94_check_is_on_(0)) {
      (void)SAT99_inc_ln_(SAT99_indent_(SAT99_c_(outfile__,'\n')),1);
      PRI187_cprint_restore_exec_info_(0,outfile__);
   }
   else {
   }

   ret0__:
   return;
}

CLA148_cprint_pre_rtcode_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,27,ls718_,"<apparently not from file>");
   SATHER_STR_(20,27,ls719_,"SHARED_UPDATE_EXEC_INFO_(\"");
   SATHER_STR_(20,4,ls721_,"\",\"");
   SATHER_STR_(20,5,ls722_,"\");\n");
   ptr    filename__ = 0;

   if (GLO94_check_is_on_(0)) {
      (void)SAT99_inc_ln_(SAT99_indent_(SAT99_c_(outfile__,'\n')),1);
      filename__ = S_ptr_VOID_;
      if ((PATT_(self__,28) != 0)) {
         if ((PATT_(PATT_(self__,28),16) != 0)) {
            filename__ = (ptr)PATT_(PATT_(self__,28),16);
         }
         else {
         }
      }
      else {
      }
      if ((filename__ == 0)) {
         filename__ = (ptr)(ptr)(&ls718_);
      }
      else {
      }
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls719_)),ERR96_def_filename_(0,IATT_(self__,24))),(ptr)(&ls721_)),CLA148_full_name_(self__)),(ptr)(&ls722_)),1);
   }
   else {
   }

   ret0__:
   return;
}

CLA148_cprint_init_shareds_and_consts_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl4035_;
   static int gl4036_;
   static union dtype_ gl4037_;
   int gl408_;
   ptr gl4038_;
   static int gl4039_;
   static union dtype_ gl4040_;
   int gl409_;
   ptr gl4041_;
   static int gl4042_;
   static union dtype_ gl4043_;
   int    i__ = S_int_VOID_;
   int    fsz__ = S_int_VOID_;

   if (CATT_(self__,8)) {
      goto ret0__;
   }
   else {
   }
   CATT_(self__,7) = (char)1;
   i__ = (int)0;
   fsz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      fsz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= fsz__)) {
         goto goto_tag_4044_;
      }
      else {
      }
      gl4035_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl408_ = TYPE_(gl4035_);
      gl4038_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
      gl409_ = TYPE_(gl4038_);
      if (((gl408_ == 151) | (gl409_ == 152))) {
         GLO68_curr_feature_ = (ptr)PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         gl4041_ = PATT_(PATT_(self__,32), 24 + ((i__) << 2));
         cache_dispatch_(gl4041_,724,gl4042_,INTVAL_(gl4043_));
         VFN_(gl4043_)(gl4041_,outfile__);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4044_: ;
   CATT_(self__,7) = (char)0;
   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


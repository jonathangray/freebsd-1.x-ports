/* global68.c : Sather class: GLOBALS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr LST147_create_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr LST147_push_();
extern ptr CLA148_create_();
extern ptr INS150_create_();
extern ptr INS150_dispatched_();
extern ptr INT164_create_();
extern ptr LIS165_create_();
extern ptr UPA167_canonicalize_();
extern ptr UPA167_concat_();
extern /*constant*/ int C_T168_c_ptr_;
extern ptr LST43_create_();
extern ptr CLA67_create_();
extern /*constant*/ int RES71_void_ind_;
extern ptr STR78_create_();
extern /*shared*/ ptr COM82_sather_home_;
extern /*constant*/ ptr INS88_rt_subdir_;
extern /*constant*/ ptr INS88_precomp_subdir_;
extern /*constant*/ ptr INS88_sys_subdir_;
extern /*constant*/ ptr INS88_sys_cmdfile_;
extern /*constant*/ ptr INS88_msgfile_;
extern /*constant*/ int RES97_OB_ici_;
extern ptr LIS98_create_();
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
extern struct { int tp_; int sz_; char st_; } gs2276_;
extern struct { int tp_; int sz_; char st_; } gs2277_;
extern struct { int tp_; int sz_; char st_; } gs2278_;
extern struct { int tp_; int sz_; char st_; } gs2285_;
extern struct { int tp_; int sz_; char st_; } gs2286_;
#include "macros_.h"



/*constant*/ int GLO68_indent_step_ = 3;
/*shared*/ ptr GLO68_str_table_;
/*shared*/ ptr GLO68_class_defs_;
/*shared*/ ptr GLO68_class_inst_;
/*shared*/ ptr GLO68_class_stat_tbl_;
/*shared*/ ptr GLO68_dot_sather_reader_;
/*shared*/ int GLO68_curr_lineno_;
/*shared*/ int GLO68_curr_loop_lineno_;
/*shared*/ ptr GLO68_curr_filename_;
/*shared*/ ptr GLO68_curr_infile_;
/*shared*/ ptr GLO68_curr_file_stat_;
/*shared*/ ptr GLO68_curr_class_inst_;
/*shared*/ ptr GLO68_curr_feature_;
/*shared*/ char GLO68_semant_lval_;
/*shared*/ ptr GLO68_void_classob_;
/*shared*/ ptr GLO68_void_classob_s_;
/*shared*/ ptr GLO68_voidtype_s_;
/*shared*/ ptr GLO68_ob_typeob_s_;
/*shared*/ ptr GLO68_array_typeob_s_;
/*shared*/ ptr GLO68_array2_typeob_s_;
/*shared*/ ptr GLO68_array3_typeob_s_;
/*shared*/ ptr GLO68_array4_typeob_s_;
/*shared*/ ptr GLO68_bool_typeob_s_;
/*shared*/ ptr GLO68_c_typeob_s_;
/*shared*/ ptr GLO68_char_typeob_s_;
/*shared*/ ptr GLO68_double_typeob_s_;
/*shared*/ ptr GLO68_err_typeob_s_;
/*shared*/ ptr GLO68_file_typeob_s_;
/*shared*/ ptr GLO68_in_typeob_s_;
/*shared*/ ptr GLO68_int_typeob_s_;
/*shared*/ ptr GLO68_out_typeob_s_;
/*shared*/ ptr GLO68_real_typeob_s_;
/*shared*/ ptr GLO68_self_type_typeob_s_;
/*shared*/ ptr GLO68_str_typeob_s_;
/*shared*/ ptr GLO68_str_cursor_typeob_s_;
/*shared*/ ptr GLO68_sys_typeob_s_;
/*shared*/ ptr GLO68_fob_typeob_s_;
/*shared*/ ptr GLO68_sux_typeob_s_;
/*shared*/ ptr GLO68_undefine_typeob_s_;
/*shared*/ ptr GLO68_sys_dir_;
/*shared*/ ptr GLO68_rt_dir_;
/*shared*/ ptr GLO68_precomp_dir_;
/*shared*/ ptr GLO68_main_init_file_;
/*shared*/ ptr GLO68_sys_cmdfile_name_;
/*shared*/ ptr GLO68_msgfile_name_;
/*shared*/ ptr GLO68_final_prog_name_;
/*shared*/ int GLO68_creator_class_name_;
/*shared*/ ptr GLO68_creator_classob_;
/*shared*/ ptr GLO68_creator_classob_s_;
/*shared*/ int GLO68_next_key_;
/*shared*/ int GLO68_dispatch_table_size_;
/*shared*/ ptr GLO68_dispatch_flags_;
/*shared*/ ptr GLO68_clib_options_;
/*shared*/ char GLO68_self_exists_;
/*shared*/ ptr GLO68_features_called_from_c_;
/*shared*/ ptr GLO68_name_mappings_;
/*shared*/ ptr GLO68_main_rout_;
/*shared*/ ptr GLO68_tmpct_;
/*shared*/ ptr GLO68_str_consts_;
/*shared*/ ptr GLO68_str_indices_;
/*shared*/ char GLO68_print_feat_info_;
/*shared*/ char GLO68_print_des_info_;
/*shared*/ int GLO68_g_tag_;
/*shared*/ int GLO68_rt_type_;
/*shared*/ char GLO68_pre_semant_;
/*shared*/ ptr GLO68_other_cnames_;
/*shared*/ ptr GLO68_c_compiler_;
/*shared*/ ptr GLO68_cc_flags_;
/*shared*/ ptr GLO68_c_macros_;
/*shared*/ ptr GLO68_keys_set_;
GLO68_init_file_info_();
GLO68_init_typeob_s_();
ptr GLO68_initialize_();
extern int attr_ent_GLO68[];

GLO68_init_file_info_(self__)
ptr self__;
{

   GLO68_rt_dir_ = (ptr)UPA167_concat_(0,COM82_sather_home_,UPA167_canonicalize_(0,(ptr)(&gs2277_)));
   GLO68_precomp_dir_ = (ptr)UPA167_concat_(0,COM82_sather_home_,(ptr)(&gs2278_));
   if ((GLO68_sys_dir_ == 0)) {
      GLO68_sys_dir_ = (ptr)UPA167_concat_(0,COM82_sather_home_,UPA167_canonicalize_(0,(ptr)(&gs2276_)));
   }
   else {
   }
   if ((GLO68_sys_cmdfile_name_ == 0)) {
      GLO68_sys_cmdfile_name_ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),GLO68_sys_dir_),(ptr)(&gs2285_));
   }
   else {
   }
   GLO68_msgfile_name_ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),GLO68_sys_dir_),(ptr)(&gs2286_));

   ret0__:
   return;
}

GLO68_init_typeob_s_(self__)
ptr self__;
{

   GLO68_void_classob_ = (ptr)CLA67_create_(0,37,0,0,LST43_create_(0,1),0);
   GLO68_void_classob_s_ = (ptr)CLA148_create_(0,GLO68_void_classob_,LST147_push_(LST147_create_(0,1),37),0);
   IATT_(GLO68_void_classob_s_,104) = (int)1;
   GLO68_voidtype_s_ = (ptr)INS150_create_(0,0,(- 1));
   GLO68_ob_typeob_s_ = (ptr)INS150_dispatched_(INS150_create_(0,RES97_OB_ici_,(- 1)));
   GLO68_array_typeob_s_ = (ptr)INS150_create_(0,RES97_ARRAY_ici_,(- 1));
   GLO68_array2_typeob_s_ = (ptr)INS150_create_(0,RES97_ARRAY2_ici_,(- 1));
   GLO68_array3_typeob_s_ = (ptr)INS150_create_(0,RES97_ARRAY3_ici_,(- 1));
   GLO68_array4_typeob_s_ = (ptr)INS150_create_(0,RES97_ARRAY4_ici_,(- 1));
   GLO68_bool_typeob_s_ = (ptr)INS150_create_(0,RES97_BOOL_ici_,(- 1));
   GLO68_c_typeob_s_ = (ptr)INS150_create_(0,RES97_C_ici_,(- 1));
   GLO68_char_typeob_s_ = (ptr)INS150_create_(0,RES97_CHAR_ici_,(- 1));
   GLO68_double_typeob_s_ = (ptr)INS150_create_(0,RES97_DOUBLE_ici_,(- 1));
   GLO68_err_typeob_s_ = (ptr)INS150_create_(0,RES97_ERR_ici_,(- 1));
   GLO68_file_typeob_s_ = (ptr)INS150_create_(0,RES97_FILE_ici_,(- 1));
   GLO68_in_typeob_s_ = (ptr)INS150_create_(0,RES97_IN_ici_,(- 1));
   GLO68_int_typeob_s_ = (ptr)INS150_create_(0,RES97_INT_ici_,(- 1));
   GLO68_out_typeob_s_ = (ptr)INS150_create_(0,RES97_OUT_ici_,(- 1));
   GLO68_real_typeob_s_ = (ptr)INS150_create_(0,RES97_REAL_ici_,(- 1));
   GLO68_self_type_typeob_s_ = (ptr)INS150_create_(0,RES97_SELF_TYPE_ici_,(- 1));
   GLO68_str_typeob_s_ = (ptr)INS150_create_(0,RES97_STR_ici_,(- 1));
   GLO68_str_cursor_typeob_s_ = (ptr)INS150_create_(0,RES97_STR_CURSOR_ici_,(- 1));
   GLO68_sys_typeob_s_ = (ptr)INS150_create_(0,RES97_SYS_ici_,(- 1));
   GLO68_fob_typeob_s_ = (ptr)INS150_create_(0,RES97_FOB_ici_,(- 1));
   GLO68_sux_typeob_s_ = (ptr)INS150_create_(0,RES97_SUX_ici_,(- 1));
   GLO68_undefine_typeob_s_ = (ptr)INS150_create_(0,RES97_UNDEFINE_ici_,(- 1));

   ret0__:
   return;
}

ptr GLO68_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* cs____23.c : Sather class: CS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ int omerrs;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);
extern /*shared*/ ptr fin;
extern /*shared*/ ptr buf;
extern void lex_init();
extern void yyparse();
extern void exit(int i__);

extern ptr ERR7_s_(ptr self__, ptr st__);
extern ptr OUT9_c_(ptr self__, char ch__);
extern ptr OUT9_s_(ptr self__, ptr st__);
extern void FIL11_open_for_read_(ptr self__, ptr nm__);
extern int FIL11_error_(ptr self__);
extern int FIL11_get_i_(ptr self__);
extern char FIL11_get_c_(ptr self__);
extern char FIL11_check_eof_(ptr self__);
extern void FIL11_close_(ptr self__);
extern ptr FIL11_in_(ptr self__);
extern ptr STR12_skip_space_(ptr self__);
extern char STR12_is_done_(ptr self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char STR20_is_equal_(ptr self__, ptr st__);
extern ptr STR20_cursor_(ptr self__);
extern ptr STR12_get_word_(ptr self__);
extern ptr STR20_to_upper_case_(ptr self__);
extern ptr STR20_c_(ptr self__, char ch__);
extern int STR20_length_(ptr self__);
extern ptr STR20_substring_(ptr self__, int n__, int m__);
extern ptr STR20_head_(ptr self__, int n__);
extern ptr INT15_to_s_(int self__);
extern ptr OUT9_r_(ptr self__, float re__);
extern void CLA66_install_(ptr self__, ptr new_def__);
extern ptr CLA66_get_obj_(ptr self__, int nm__);
extern /*shared*/ ptr GLO68_dot_sather_reader_;
extern void GLO68_init_file_info_(ptr self__);
extern /*shared*/ ptr GLO68_sys_cmdfile_name_;
extern /*shared*/ ptr GLO68_c_compiler_;
extern /*shared*/ ptr GLO68_cc_flags_;
extern /*shared*/ ptr GLO68_c_macros_;
extern /*shared*/ ptr GLO68_str_table_;
extern int STR69_insert_str_(ptr self__, ptr s__);
extern /*shared*/ ptr GLO68_msgfile_name_;
extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_curr_infile_;
extern /*shared*/ ptr GLO68_curr_file_stat_;
extern ptr OUT80_s_(ptr self__, ptr st__);
extern ptr DOT81_create_(ptr self__);
extern /*shared*/ ptr COM82_cmdfile_name_;
extern void DOT81_init_(ptr self__, ptr cmdfile_name__);
extern char DOT81_read_cmdfile_(ptr self__);
extern /*constant*/ int COM84_c_compiler_ind_;
extern /*constant*/ int COM84_cc_flags_ind_;
extern /*constant*/ int COM84_c_macro_ind_;
extern ptr INT86_create_(ptr self__);
extern void INT86_ins_ent_(ptr self__, int i__, int j__);
extern /*constant*/ ptr INS88_release_version_;
extern /*constant*/ int INS88_deep_save_version_;
extern ptr OUT80_i_(ptr self__, int in__);
extern /*shared*/ ptr INS88_default_environment_;
extern /*constant*/ ptr INS88_guess_envi_;
extern ptr OUT80_nl_(ptr self__);
extern /*constant*/ ptr INS88_install_date_;
extern /*constant*/ ptr INS88_install_host_;
extern /*shared*/ char COM82_warnings_only_;
extern /*constant*/ int COM89_stop_compile_;
extern /*constant*/ int COM89_privilege_only_;
extern ptr UNI90_login_name_(ptr self__);
extern /*constant*/ ptr INS88_repair_who_;
extern ptr DOT81_ith_source_file_(ptr self__, int i__);
extern ptr FIL91_create_(ptr self__, ptr f__);
extern /*shared*/ int GLO68_curr_lineno_;
extern /*shared*/ char COM82_do_timing_;
extern /*shared*/ ptr TIM92_start_code_read_;
extern ptr TIM73_create_(ptr self__);
extern /*shared*/ ptr GLO68_class_defs_;
extern /*shared*/ int GLO68_creator_class_name_;
extern /*shared*/ ptr GLO68_creator_classob_;
extern ptr OUT80_out_(ptr self__);
extern ptr CLA66_create_(ptr self__, int size__);
extern /*shared*/ ptr GLO68_class_inst_;
extern ptr CLA93_create_(ptr self__, int size__);
extern ptr STR69_create_(ptr self__);
extern /*constant*/ ptr INS88_env_v_environment_;
extern ptr UNI90_getenv_(ptr self__, ptr name__);
extern /*constant*/ ptr INS88_env_v_sat_home_;
extern void COM82_set_sather_home_(ptr self__, ptr sat_home__);
extern /*shared*/ ptr COM82_target_dir_;
extern /*shared*/ ptr COM82_sather_home_;
extern int UNI90_putenv_(ptr self__, ptr name__, ptr val__);
extern /*shared*/ char COM82_is_opt_sather_home_;
extern /*shared*/ char COM82_all_np_classes_;
extern /*shared*/ char COM82_k_and_r_c_;
extern /*shared*/ char COM82_verbose_code_;
extern /*shared*/ char COM82_gen_base_;
extern /*shared*/ char COM82_rt_code_check_;
extern /*shared*/ char COM82_gen_all_;
extern /*shared*/ char COM82_has_gc_;
extern /*shared*/ ptr COM82_target_environment_;
extern /*shared*/ char COM82_warn_rt_val_;
extern /*shared*/ int COM82_compiler_mode_;
extern /*constant*/ int COM82_browser_mode_;
extern /*shared*/ char GLO68_print_feat_info_;
extern /*shared*/ char COM82_dbg_mode_;
extern /*shared*/ char COM82_new_compilation_;
extern /*shared*/ char COM82_print_desinfo_;
extern void ARR77_clear_(ptr self__);
extern /*shared*/ char COM82_dot_prefix_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ ptr GLO68_final_prog_name_;
extern /*shared*/ char GLO68_print_des_info_;
extern ptr UNI90_getcwd_(ptr self__);
extern void STR69_augment_(ptr self__);
extern /*constant*/ ptr INS88_rm_command_;
extern /*shared*/ ptr TIM92_rmdir_time_;
extern ptr TIM73_time_syscall_(ptr self__, ptr com__);
extern void GLO94_system_(ptr self__, ptr com__);
extern /*constant*/ ptr INS88_mkdir_command_;
extern /*constant*/ ptr INS88_echo_command_;
extern /*shared*/ ptr TIM92_mkdir_time_;
extern char SAT95_install_predefined_classes_(ptr self__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern void GLO68_init_typeob_s_(ptr self__);
extern /*constant*/ int RES97_LAST_PREDEF_ici_;
extern ptr LIS98_create_(ptr self__, int init_size__);
extern ptr LIS98_push_(ptr self__, int e__);
extern ptr SAT95_get_simple_classnames_(ptr self__);
extern ptr LIS98_union_(ptr self__, ptr list__);
extern ptr CLA66_at_index_(ptr self__, int i__);
extern int CLA67_num_params_(ptr self__);
extern int SAT95_install_root_classes_(ptr self__, ptr names__);
extern void SAT95_all_create_inst_(ptr self__);
extern void SAT95_all_expand_cinh_(ptr self__);
extern void SAT95_all_resolve_predef_types_and_compute_num_attrs_(ptr self__, ptr used_classes__);
extern void SAT95_all_compute_anc_and_des_and_time_stamp_(ptr self__);
extern void SAT95_all_compute_attr_offsets_(ptr self__);
extern void SAT95_find_called_features_(ptr self__);
extern /*shared*/ ptr TIM92_start_semant_;
extern void SAT95_all_semant_(ptr self__);
extern void SAT95_print_descendant_information_(ptr self__);
extern /*shared*/ ptr TIM92_start_print_;
extern void SAT95_all_class_inst_cprint_(ptr self__);
extern int SAT176_error_(ptr self__);
extern void SAT95_cprint_init_shareds_and_consts_(ptr self__);
extern void SAT95_cprint_init_(ptr self__);
extern void SAT95_cprint_class_info_(ptr self__);
extern /*constant*/ ptr INS88_offset_file_;
extern void SAT99_open_for_write_(ptr self__, ptr nm__);
extern int SAT99_error_(ptr self__);
extern void SAT95_print_defines_(ptr self__, ptr outfile__);
extern void SAT99_close_(ptr self__);
extern void SAT95_print_sather_to_c_macros_(ptr self__);
extern void SAT95_print_browser_info_(ptr self__);
extern /*constant*/ int COM84_object_files_ind_;
extern /*shared*/ ptr COM82_object_files_;
extern /*shared*/ ptr TIM92_start_command_read_;
extern double TIM73_time_spent_(ptr self__);
extern /*shared*/ ptr TIM92_start_main_;
extern /*shared*/ ptr TIM92_start_processing_;
extern /*shared*/ ptr TIM92_start_last_stage_;
extern /*shared*/ ptr TIM92_rmfiles_com_time_;
extern /*shared*/ ptr TIM92_start_make_;
extern /*shared*/ ptr TIM92_cp_com_time_;
extern /*shared*/ ptr TIM92_end_main_;
extern /*shared*/ ptr TIM92_mk_com_time_;
extern /*shared*/ ptr TIM92_mv_com_time_;
extern ptr STR70_create_(ptr self__, int init_str_len__);
extern void OLD101_init_class_stat_tbl_(ptr self__);
extern void OLD101_adjust_tables_(ptr self__);
extern void OLD101_store_class_stat_tbl_(ptr self__);
extern void OLD101_store_name_ind_map_(ptr self__);
extern void SAT95_print_makefile_(ptr self__);
extern struct { int tp_; int sz_; char st_; } gs2304_;
extern struct { int tp_; int sz_; char st_; } gs2309_;
extern struct { int tp_; int sz_; char st_; } gs2279_;
extern struct { int tp_; int sz_; char st_; } gs1384_;
extern struct { int tp_; int sz_; char st_; } gs2288_;
extern struct { int tp_; int sz_; char st_; } gs2290_;
extern struct { int tp_; int sz_; char st_; } gs2291_;
extern struct { int tp_; int sz_; char st_; } gs2297_;
extern struct { int tp_; int sz_; char st_; } gs2298_;
extern struct { int tp_; int sz_; char st_; } gs2299_;
extern struct { int tp_; int sz_; char st_; } gs2300_;
#include "macros_.h"



/*shared*/ ptr CS_23_outfile_;
/*shared*/ ptr CS_23_stdin_;
/*shared*/ char CS_23_predef_ok_;
void CS_23_error_msg_(ptr self__, ptr s__);
void CS_23_error_exit_(ptr self__, ptr s__);
void CS_23_warning_msg_(ptr self__, ptr s__);
char CS_23_read_commands_(ptr self__);
void CS_23_release_msg_(ptr self__);
void CS_23_system_msg_(ptr self__);
char CS_23_read_source_files_(ptr self__);
void CS_23_read_code_(ptr self__);
void CS_23_process_classdef_(ptr self__, ptr def__);
void CS_23_main_init_(ptr self__);
/*shared*/ char CS_23_sdb_inc_compile_warning_;
void CS_23_set_options_(ptr self__, ptr argv__);
void CS_23_setup_dir_(ptr self__);
void CS_23_process_classes_(ptr self__);
void CS_23_handle_object_files_(ptr self__);
void CS_23_print_times_(ptr self__);
int CS_23_main_(ptr self__, ptr argv__);
ptr CS_23_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_CS_23[];

void CS_23_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void CS_23_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

void CS_23_warning_msg_(ptr self__, ptr s__)
{

   (void)ERR7_s_(0,s__);

   ret0__:
   return;
}

char CS_23_read_commands_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,19,ls1256_,"Error in reading \"");
   SATHER_STR_(20,50,ls1259_,"** Warning: More than one \"(c_compiler)\"; using \"");
   SATHER_STR_(20,6,ls1260_,"\" **\n");
   ptr    dsr__ = 0;
   ptr    c_compiler__ = 0;
   int    csz__ = S_int_VOID_;
   ptr    curr_cc_flags__ = 0;
   int    cc_sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    curr_c_macros__ = 0;
   int    i_5_ = S_int_VOID_;
   int    imax__ = S_int_VOID_;
   ptr    entry__ = 0;
   int    j__ = S_int_VOID_;
   int    jmax__ = S_int_VOID_;
   int    qexp_ind__ = S_int_VOID_;
   int    cname_ind__ = S_int_VOID_;

   GLO68_dot_sather_reader_ = (ptr)DOT81_create_(0);
   dsr__ = (ptr)GLO68_dot_sather_reader_;
   DOT81_init_(dsr__,COM82_cmdfile_name_);
   if ((PATT_(dsr__,12) == 0)) {
      CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),COM82_cmdfile_name_),(ptr)(&ls632_)));
   }
   else {
      if ((SAT176_error_(PATT_(dsr__,12)) != 0)) {
         CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),COM82_cmdfile_name_),(ptr)(&ls632_)));
      }
      else {
      }
   }
   if ((! DOT81_read_cmdfile_(dsr__))) {
      CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1256_)),COM82_cmdfile_name_),(ptr)(&ls632_)));
   }
   else {
   }
   GLO68_init_file_info_(0);
   DOT81_init_(dsr__,GLO68_sys_cmdfile_name_);
   if ((PATT_(dsr__,12) == 0)) {
      CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),GLO68_sys_cmdfile_name_),(ptr)(&ls632_)));
   }
   else {
      if ((SAT176_error_(PATT_(dsr__,12)) != 0)) {
         CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),GLO68_sys_cmdfile_name_),(ptr)(&ls632_)));
      }
      else {
      }
   }
   if ((! DOT81_read_cmdfile_(dsr__))) {
      CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1256_)),GLO68_sys_cmdfile_name_),(ptr)(&ls632_)));
   }
   else {
   }
   if ((GLO68_c_compiler_ == 0)) {
      c_compiler__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((7) << 2));
      csz__ = (int)0;
      if ((PATT_(c_compiler__,4) != 0)) {
         csz__ = (int)IATT_(PATT_(c_compiler__,4),4);
      }
      else {
      }
      if ((csz__ > 0)) {
         if ((csz__ > 1)) {
            (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls1259_)),PATT_(PATT_(c_compiler__,4), 16 + ((0) << 2))),(ptr)(&ls1260_));
         }
         else {
         }
         GLO68_c_compiler_ = (ptr)PATT_(PATT_(c_compiler__,4), 16 + ((0) << 2));
      }
      else {
      }
   }
   else {
   }
   if ((GLO68_cc_flags_ == 0)) {
      curr_cc_flags__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((2) << 2));
      cc_sz__ = (int)0;
      if ((PATT_(curr_cc_flags__,4) != 0)) {
         cc_sz__ = (int)IATT_(PATT_(curr_cc_flags__,4),4);
      }
      else {
      }
      if ((cc_sz__ != 0)) {
         GLO68_cc_flags_ = (ptr)new1_(77,cc_sz__,0);
         i__ = (int)0;
         while (1) {
            if ((i__ >= cc_sz__)) {
               goto goto_tag_539_;
            }
            else {
            }
            PATT_(GLO68_cc_flags_, 8 + ((i__) << 2)) = (ptr)PATT_(PATT_(curr_cc_flags__,4), 16 + ((i__) << 2));
            i__ = (int)(i__ + 1);
         }
      goto_tag_539_: ;
      }
      else {
      }
   }
   else {
   }
   curr_c_macros__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((3) << 2));
   GLO68_c_macros_ = (ptr)INT86_create_(0);
   i_5_ = S_int_VOID_;
   imax__ = (int)IATT_(PATT_(curr_c_macros__,4),4);
   while (1) {
      if ((i_5_ >= imax__)) {
         goto goto_tag_540_;
      }
      else {
      }
      entry__ = (ptr)PATT_(PATT_(curr_c_macros__,4), 16 + ((i_5_) << 2));
      j__ = S_int_VOID_;
      jmax__ = S_int_VOID_;
      qexp_ind__ = S_int_VOID_;
      if ((entry__ != 0)) {
         jmax__ = (int)IATT_(entry__,4);
      }
      else {
      }
      if ((jmax__ > 0)) {
         qexp_ind__ = (int)STR69_insert_str_(GLO68_str_table_,PATT_(entry__, 16 + ((0) << 2)));
         while (1) {
            if ((j__ >= jmax__)) {
               goto goto_tag_541_;
            }
            else {
            }
            cname_ind__ = (int)STR69_insert_str_(GLO68_str_table_,PATT_(entry__, 16 + ((j__) << 2)));
            INT86_ins_ent_(GLO68_c_macros_,cname_ind__,qexp_ind__);
            j__ = (int)(j__ + 1);
         }
      goto_tag_541_: ;
      }
      else {
      }
      i_5_ = (int)(i_5_ + 1);
   }
goto_tag_540_: ;
   GLO68_init_file_info_(0);
   res__ = (char)1;

   ret0__:
   return (res__);
}

void CS_23_release_msg_(ptr self__)
{
   SATHER_STR_(20,12,ls1271_,"Sather Rel ");
   SATHER_STR_(20,6,ls1273_," Bin ");
   SATHER_STR_(20,7,ls1275_," Envi ");
   SATHER_STR_(20,3,ls1276_," (");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,21,ls1280_,"Configuration as of ");
   SATHER_STR_(20,5,ls1282_," on ");
   SATHER_STR_(20,70,ls1284_,"(c) ICSI Berkeley, 1991, 1992, 1993. (c) CSIRO Australia, 1992, 1993.");
   SATHER_STR_(20,79,ls1285_,"This is free software, PROVIDED \"AS IS\", WITH ABSOLUTELY NO WARRANTY EXPRESSED");
   SATHER_STR_(20,80,ls1286_,"OR IMPLIED. ANY USE IS AT YOUR OWN RISK. You are welcome to distribute, use and");
   SATHER_STR_(20,75,ls1287_,"reuse it under the restrictions described in $SATHER_HOME/doc/license.txt.");

   (void)OUT80_nl_(OUT80_s_(OUT80_nl_(OUT80_s_(OUT80_nl_(OUT80_s_(OUT80_nl_(OUT80_s_(OUT80_nl_(OUT80_s_(OUT80_s_(OUT80_s_(OUT80_s_(OUT80_nl_(OUT80_s_(OUT80_s_(OUT80_s_(OUT80_s_(OUT80_s_(OUT80_i_(OUT80_s_(OUT80_s_(OUT80_s_(CS_23_outfile_,(ptr)(&ls1271_)),(ptr)(&gs2298_)),(ptr)(&ls1273_)),28),(ptr)(&ls1275_)),INS88_default_environment_),(ptr)(&ls1276_)),(ptr)(&gs2297_)),(ptr)(&ls1278_))),(ptr)(&ls1280_)),(ptr)(&gs2299_)),(ptr)(&ls1282_)),(ptr)(&gs2300_))),(ptr)(&ls1284_))),(ptr)(&ls1285_))),(ptr)(&ls1286_))),(ptr)(&ls1287_)));

   ret0__:
   return;
}

void CS_23_system_msg_(ptr self__)
{
   ptr    msgfile__ = 0;
   int    action__ = S_int_VOID_;
   char    ch__ = S_char_VOID_;
   ptr    who__ = 0;

   msgfile__ = (ptr)new_(11,0);
   FIL11_open_for_read_(msgfile__,GLO68_msgfile_name_);
   if ((FIL11_error_(msgfile__) == 0)) {
      action__ = (int)FIL11_get_i_(msgfile__);
      if ((! COM82_warnings_only_)) {
         ch__ = (char)FIL11_get_c_(msgfile__);
         while (1) {
            if (FIL11_check_eof_(msgfile__)) {
               goto goto_tag_542_;
            }
            else {
            }
            (void)OUT9_c_(0,ch__);
            ch__ = (char)FIL11_get_c_(msgfile__);
         }
      goto_tag_542_: ;
      }
      else {
      }
      FIL11_close_(msgfile__);
      switch (action__) {
         case (1) :
            exit(1);
            break;
         case (2) :
            who__ = (ptr)UNI90_login_name_(0);
            if ((who__ != 0)) {
               if ((! STR20_is_equal_(who__,(ptr)(&gs2288_)))) {
                  exit(1);
               }
               else {
               }
            }
            else {
               exit(1);
            }
            break;
         default:
            ;
            ;
      }
   }
   else {
   }

   ret0__:
   return;
}

char CS_23_read_source_files_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,37,ls1301_,"Warning: Cannot get status of file \"");
   ptr    dsr__ = 0;
   int    i__ = S_int_VOID_;

   res__ = (char)1;
   dsr__ = (ptr)GLO68_dot_sather_reader_;
   i__ = (int)0;
   GLO68_curr_filename_ = (ptr)DOT81_ith_source_file_(dsr__,i__);
   while (1) {
      if ((GLO68_curr_filename_ == 0)) {
         goto goto_tag_543_;
      }
      else {
      }
      GLO68_curr_infile_ = (ptr)new_(11,0);
      FIL11_open_for_read_(GLO68_curr_infile_,GLO68_curr_filename_);
      if ((FIL11_error_(GLO68_curr_infile_) != 0)) {
         CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),GLO68_curr_filename_),(ptr)(&ls632_)));
         res__ = (char)0;
      }
      else {
         fin = (ptr)PATT_(GLO68_curr_infile_,4);
         GLO68_curr_file_stat_ = (ptr)FIL91_create_(0,GLO68_curr_infile_);
         if ((IATT_(GLO68_curr_file_stat_,4) == (- 1))) {
            (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls1301_)),GLO68_curr_filename_),(ptr)(&ls632_));
         }
         else {
         }
         GLO68_curr_lineno_ = (int)1;
         yyparse();
         FIL11_close_(GLO68_curr_infile_);
         GLO68_curr_file_stat_ = (ptr)0;
      }
      i__ = (int)(i__ + 1);
      GLO68_curr_filename_ = (ptr)DOT81_ith_source_file_(dsr__,i__);
   }
goto_tag_543_: ;

   ret0__:
   return (res__);
}

void CS_23_read_code_(ptr self__)
{
   SATHER_STR_(20,15,ls1305_,"* Reading ...\n");
   SATHER_STR_(20,31,ls1306_,"Error in reading user classes\n");

   lex_init();
   CS_23_setup_dir_(self__);
   if (COM82_do_timing_) {
      TIM92_start_code_read_ = (ptr)TIM73_create_(0);
   }
   else {
   }
   if ((! COM82_warnings_only_)) {
      (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1305_));
   }
   else {
   }
   if ((! CS_23_read_source_files_(self__))) {
      CS_23_error_exit_(self__,(ptr)(&ls1306_));
   }
   else {
   }

   ret0__:
   return;
}

void CS_23_process_classdef_(ptr self__, ptr def__)
{

   CLA66_install_(GLO68_class_defs_,def__);
   if ((IATT_(def__,12) == GLO68_creator_class_name_)) {
      GLO68_creator_classob_ = (ptr)CLA66_get_obj_(GLO68_class_defs_,IATT_(def__,12));
   }
   else {
   }

   ret0__:
   return;
}

void CS_23_main_init_(ptr self__)
{

   CS_23_outfile_ = (ptr)OUT80_out_(0);
   CS_23_stdin_ = (ptr)FIL11_in_(0);
   GLO68_class_defs_ = (ptr)CLA66_create_(0,50);
   GLO68_class_inst_ = (ptr)CLA93_create_(0,50);
   GLO68_str_table_ = (ptr)STR69_create_(0);

   ret0__:
   return;
}

void CS_23_set_options_(ptr self__, ptr argv__)
{
   SATHER_STR_(20,33,ls1315_,"(ERROR): No main class provided\n");
   SATHER_STR_(20,1,ls1016_,"");
   SATHER_STR_(20,5,ls1322_,"-dir");
   SATHER_STR_(20,23,ls1323_,"(-dir) option ignored\n");
   SATHER_STR_(20,5,ls1324_,"-com");
   SATHER_STR_(20,23,ls1325_,"(-com) option ignored\n");
   SATHER_STR_(20,5,ls1326_,"-hom");
   SATHER_STR_(20,23,ls1328_,"(-hom) option ignored\n");
   SATHER_STR_(20,5,ls1329_,"-cre");
   SATHER_STR_(20,4,ls1330_,"-kr");
   SATHER_STR_(20,6,ls1331_,"-ansi");
   SATHER_STR_(20,5,ls1332_,"-ver");
   SATHER_STR_(20,5,ls1333_,"-gen");
   SATHER_STR_(20,5,ls1334_,"-chk");
   SATHER_STR_(20,5,ls1335_,"-all");
   SATHER_STR_(20,4,ls1336_,"-gc");
   SATHER_STR_(20,4,ls1337_,"yes");
   SATHER_STR_(20,3,ls1338_,"no");
   SATHER_STR_(20,22,ls1339_,"(-gc) option ignored\n");
   SATHER_STR_(20,3,ls1340_,"-x");
   SATHER_STR_(20,21,ls1341_,"(-x) option ignored\n");
   SATHER_STR_(20,5,ls1342_,"-res");
   SATHER_STR_(20,5,ls1343_,"-sys");
   SATHER_STR_(20,23,ls1344_,"(-sys) option ignored\n");
   SATHER_STR_(20,6,ls1345_,"-info");
   SATHER_STR_(20,5,ls1346_,"-dbg");
   SATHER_STR_(20,5,ls1347_,"-sdb");
   SATHER_STR_(20,5,ls1348_,"-ben");
   SATHER_STR_(20,5,ls1349_,"-ncs");
   SATHER_STR_(20,3,ls1350_,"-w");
   SATHER_STR_(20,5,ls1351_,"-pds");
   SATHER_STR_(20,4,ls1352_,"-cc");
   SATHER_STR_(20,22,ls1353_,"(-cc) option ignored\n");
   SATHER_STR_(20,8,ls1354_,"-cflags");
   SATHER_STR_(20,26,ls1360_,"(-cflags) option ignored\n");
   SATHER_STR_(20,5,ls1361_,"-dot");
   SATHER_STR_(20,39,ls1362_,"(Warning) : Ambiguous creator class - ");
   SATHER_STR_(20,5,ls1363_," or ");
   SATHER_STR_(20,9,ls1364_,"; using ");
   SATHER_STR_(20,3,ls1368_,"kr");
   SATHER_STR_(20,2,ls1371_,".");
   SATHER_STR_(20,5,ls1372_,".cs/");
   SATHER_STR_(20,3,ls1373_,"./");
   int    i__ = S_int_VOID_;
   int    argc__ = S_int_VOID_;
   ptr    arc_env_v__ = 0;
   ptr    sh_env_v__ = 0;
   char    ansi_kr_explicit__ = S_char_VOID_;
   ptr    curs__ = 0;
   int    i_6_ = S_int_VOID_;
   ptr    tmp__ = 0;
   ptr    arch__ = 0;
   int    arch_s__ = S_int_VOID_;
   char    arch_kr__ = S_char_VOID_;
   ptr    working_dir__ = 0;
   ptr    prefix__ = 0;

   i__ = (int)1;
   argc__ = (int)IATT_(argv__,4);
   if ((argc__ < 2)) {
      CS_23_error_exit_(self__,(ptr)(&ls1315_));
   }
   else {
   }
   arc_env_v__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2291_));
   if ((arc_env_v__ != 0)) {
      if ((! STR20_is_equal_(arc_env_v__,(ptr)(&ls1016_)))) {
         INS88_default_environment_ = (ptr)STR20_s_(STR20_create_(0),arc_env_v__);
      }
      else {
      }
   }
   else {
   }
   sh_env_v__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2290_));
   if ((sh_env_v__ != 0)) {
      COM82_set_sather_home_(0,sh_env_v__);
   }
   else {
   }
   ansi_kr_explicit__ = S_char_VOID_;
   while (1) {
      if ((i__ >= argc__)) {
         goto goto_tag_544_;
      }
      else {
      }
      if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1322_))) {
         if (((i__ + 1) < argc__)) {
            COM82_target_dir_ = (ptr)copy_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),1);
         }
         else {
            (void)ERR7_s_(0,(ptr)(&ls1323_));
         }
         i__ = (int)(i__ + 2);
      }
      else {
         if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1324_))) {
            if (((i__ + 1) < argc__)) {
               COM82_cmdfile_name_ = (ptr)copy_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),1);
            }
            else {
               (void)ERR7_s_(0,(ptr)(&ls1325_));
            }
            i__ = (int)(i__ + 2);
         }
         else {
            if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1326_))) {
               if (((i__ + 1) < argc__)) {
                  COM82_set_sather_home_(0,PATT_(argv__, 8 + (((i__ + 1)) << 2)));
                  (void)UNI90_putenv_(0,(ptr)(&gs2290_),COM82_sather_home_);
                  COM82_is_opt_sather_home_ = (char)1;
               }
               else {
                  (void)ERR7_s_(0,(ptr)(&ls1328_));
               }
               i__ = (int)(i__ + 2);
            }
            else {
               if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1329_))) {
                  COM82_all_np_classes_ = (char)1;
                  i__ = (int)(i__ + 1);
               }
               else {
                  if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1330_))) {
                     COM82_k_and_r_c_ = (char)1;
                     ansi_kr_explicit__ = (char)1;
                     i__ = (int)(i__ + 1);
                  }
                  else {
                     if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1331_))) {
                        COM82_k_and_r_c_ = (char)0;
                        ansi_kr_explicit__ = (char)1;
                        i__ = (int)(i__ + 1);
                     }
                     else {
                        if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1332_))) {
                           COM82_verbose_code_ = (char)1;
                           i__ = (int)(i__ + 1);
                        }
                        else {
                           if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1333_))) {
                              COM82_gen_base_ = (char)1;
                              i__ = (int)(i__ + 1);
                           }
                           else {
                              if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1334_))) {
                                 COM82_rt_code_check_ = (char)1;
                                 i__ = (int)(i__ + 1);
                              }
                              else {
                                 if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1335_))) {
                                    COM82_gen_all_ = (char)1;
                                    i__ = (int)(i__ + 1);
                                 }
                                 else {
                                    if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1336_))) {
                                       if (((i__ + 1) < argc__)) {
                                          if (STR20_is_equal_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),(ptr)(&ls1337_))) {
                                             COM82_has_gc_ = (char)1;
                                             i__ = (int)(i__ + 2);
                                          }
                                          else {
                                             if (STR20_is_equal_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),(ptr)(&ls1338_))) {
                                                COM82_has_gc_ = (char)0;
                                                i__ = (int)(i__ + 2);
                                             }
                                             else {
                                                (void)ERR7_s_(0,(ptr)(&ls1339_));
                                                i__ = (int)(i__ + 1);
                                             }
                                          }
                                       }
                                       else {
                                       }
                                    }
                                    else {
                                       if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1340_))) {
                                          if (((i__ + 1) < argc__)) {
                                             COM82_target_environment_ = (ptr)PATT_(argv__, 8 + (((i__ + 1)) << 2));
                                             i__ = (int)(i__ + 2);
                                          }
                                          else {
                                             (void)ERR7_s_(0,(ptr)(&ls1341_));
                                             i__ = (int)(i__ + 1);
                                          }
                                       }
                                       else {
                                          if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1342_))) {
                                             COM82_warn_rt_val_ = (char)1;
                                             i__ = (int)(i__ + 1);
                                          }
                                          else {
                                             if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1343_))) {
                                                if (((i__ + 1) < argc__)) {
                                                   GLO68_sys_cmdfile_name_ = (ptr)copy_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),1);
                                                }
                                                else {
                                                   (void)ERR7_s_(0,(ptr)(&ls1344_));
                                                }
                                                i__ = (int)(i__ + 2);
                                             }
                                             else {
                                                if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1345_))) {
                                                   COM82_compiler_mode_ = (int)1;
                                                   GLO68_print_feat_info_ = (char)1;
                                                   i__ = (int)(i__ + 1);
                                                }
                                                else {
                                                   if ((STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1346_)) | STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1347_)))) {
                                                      COM82_compiler_mode_ = (int)1;
                                                      COM82_dbg_mode_ = (char)1;
                                                      GLO68_print_feat_info_ = (char)1;
                                                      if ((! COM82_warnings_only_)) {
                                                         CS_23_sdb_inc_compile_warning_ = (char)1;
                                                      }
                                                      else {
                                                      }
                                                      i__ = (int)(i__ + 1);
                                                   }
                                                   else {
                                                      if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1348_))) {
                                                         COM82_do_timing_ = (char)1;
                                                         i__ = (int)(i__ + 1);
                                                      }
                                                      else {
                                                         if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1349_))) {
                                                            COM82_new_compilation_ = (char)1;
                                                            i__ = (int)(i__ + 1);
                                                         }
                                                         else {
                                                            if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1350_))) {
                                                               COM82_warnings_only_ = (char)1;
                                                               i__ = (int)(i__ + 1);
                                                            }
                                                            else {
                                                               if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1351_))) {
                                                                  COM82_print_desinfo_ = (char)1;
                                                                  i__ = (int)(i__ + 1);
                                                               }
                                                               else {
                                                                  if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1352_))) {
                                                                     if (((i__ + 1) < argc__)) {
                                                                        GLO68_c_compiler_ = (ptr)copy_(PATT_(argv__, 8 + (((i__ + 1)) << 2)),1);
                                                                     }
                                                                     else {
                                                                        (void)ERR7_s_(0,(ptr)(&ls1353_));
                                                                     }
                                                                     i__ = (int)(i__ + 2);
                                                                  }
                                                                  else {
                                                                     if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1354_))) {
                                                                        if (((i__ + 1) < argc__)) {
                                                                           curs__ = (ptr)STR20_cursor_(PATT_(argv__, 8 + (((i__ + 1)) << 2)));
                                                                           i_6_ = S_int_VOID_;
                                                                           (void)STR12_skip_space_(curs__);
                                                                           if ((! STR12_is_done_(curs__))) {
                                                                              GLO68_cc_flags_ = (ptr)new1_(77,1,0);
                                                                           }
                                                                           else {
                                                                           }
                                                                           while (1) {
                                                                              if (STR12_is_done_(curs__)) {
                                                                                 goto goto_tag_545_;
                                                                              }
                                                                              else {
                                                                              }
                                                                              if ((i_6_ >= IATT_(GLO68_cc_flags_,4))) {
                                                                                 tmp__ = (ptr)GLO68_cc_flags_;
                                                                                 GLO68_cc_flags_ = (ptr)extend1_(GLO68_cc_flags_,(1 + IATT_(tmp__,4)),0);
                                                                                 ARR77_clear_(tmp__);
                                                                              }
                                                                              else {
                                                                              }
                                                                              PATT_(GLO68_cc_flags_, 8 + ((i_6_) << 2)) = (ptr)STR12_get_word_(curs__);
                                                                              i_6_ = (int)(i_6_ + 1);
                                                                           }
                                                                        goto_tag_545_: ;
                                                                        }
                                                                        else {
                                                                           (void)ERR7_s_(0,(ptr)(&ls1360_));
                                                                        }
                                                                        i__ = (int)(i__ + 2);
                                                                     }
                                                                     else {
                                                                        if (STR20_is_equal_(PATT_(argv__, 8 + ((i__) << 2)),(ptr)(&ls1361_))) {
                                                                           COM82_dot_prefix_ = (char)1;
                                                                           i__ = (int)(i__ + 1);
                                                                        }
                                                                        else {
                                                                           if ((GLO68_creator_class_name_ != 0)) {
                                                                              CS_23_warning_msg_(self__,STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1362_)),STR20_to_upper_case_(copy_(PATT_(argv__, 8 + ((i__) << 2)),1))),(ptr)(&ls1363_)),STR69_at_index_(GLO68_str_table_,GLO68_creator_class_name_)),(ptr)(&ls1364_)),STR20_to_upper_case_(copy_(PATT_(argv__, 8 + ((i__) << 2)),1))),'\n'));
                                                                           }
                                                                           else {
                                                                           }
                                                                           GLO68_creator_class_name_ = (int)STR69_insert_str_(GLO68_str_table_,STR20_to_upper_case_(copy_(PATT_(argv__, 8 + ((i__) << 2)),1)));
                                                                           GLO68_final_prog_name_ = (ptr)copy_(PATT_(argv__, 8 + ((i__) << 2)),1);
                                                                           i__ = (int)(i__ + 1);
                                                                        }
                                                                     }
                                                                  }
                                                               }
                                                            }
                                                         }
                                                      }
                                                   }
                                                }
                                             }
                                          }
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }
goto_tag_544_: ;
   if ((COM82_rt_code_check_ | GLO68_print_feat_info_)) {
      GLO68_print_des_info_ = (char)1;
   }
   else {
   }
   arch__ = (ptr)COM82_target_environment_;
   arch_s__ = (int)STR20_length_(arch__);
   arch_kr__ = (char)0;
   if ((arch_s__ >= 2)) {
      if (STR20_is_equal_(STR20_substring_(arch__,(arch_s__ - 2),arch_s__),(ptr)(&ls1368_))) {
         arch_kr__ = (char)1;
         if ((! ansi_kr_explicit__)) {
            COM82_k_and_r_c_ = (char)1;
         }
         else {
         }
      }
      else {
      }
   }
   else {
   }
   if ((COM82_k_and_r_c_ & (! arch_kr__))) {
      COM82_target_environment_ = (ptr)STR20_s_(COM82_target_environment_,(ptr)(&ls1368_));
   }
   else {
   }
   if ((GLO68_creator_class_name_ == 0)) {
      CS_23_error_exit_(self__,(ptr)(&ls1315_));
   }
   else {
   }
   if ((COM82_target_dir_ == 0)) {
      working_dir__ = (ptr)UNI90_getcwd_(0);
      prefix__ = S_ptr_VOID_;
      if (COM82_dot_prefix_) {
         prefix__ = (ptr)(ptr)(&ls1371_);
      }
      else {
         prefix__ = (ptr)(ptr)(&ls1016_);
      }
      if ((working_dir__ != 0)) {
         COM82_target_dir_ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),working_dir__),'/'),prefix__),GLO68_final_prog_name_),(ptr)(&ls1372_));
      }
      else {
         COM82_target_dir_ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1373_)),prefix__),GLO68_final_prog_name_),(ptr)(&ls1372_));
      }
   }
   else {
      COM82_target_dir_ = (ptr)STR20_s_(STR20_s_(STR20_c_(COM82_target_dir_,'/'),GLO68_final_prog_name_),(ptr)(&ls1372_));
   }
   if ((! COM82_new_compilation_)) {
      STR69_augment_(GLO68_str_table_);
   }
   else {
   }

   ret0__:
   return;
}

void CS_23_setup_dir_(ptr self__)
{
   SATHER_STR_(20,9,ls1376_,"if [ -d ");
   SATHER_STR_(20,9,ls1377_," ]\nthen ");
   SATHER_STR_(20,6,ls1379_," -rf ");
   SATHER_STR_(20,4,ls1380_,"\nfi");
   SATHER_STR_(20,27,ls1381_,"* Cleaning target dir ...\n");
   SATHER_STR_(20,59,ls1383_,"REMINDER: correct line numbers with -sdb may require -ncs.");
   SATHER_STR_(20,11,ls1385_,"if [ ! -d ");
   SATHER_STR_(20,3,ls1387_,"; ");
   SATHER_STR_(20,25,ls1389_," \"* Created directory --");
   SATHER_STR_(20,3,ls1390_,"\" ");
   int    len__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    target_dir__ = 0;
   ptr    rmdir__ = 0;
   ptr    mkdir__ = 0;

   len__ = (int)STR20_length_(COM82_target_dir_);
   i__ = (int)(len__ - 1);
   while (1) {
      if ((i__ < 0)) {
         goto goto_tag_546_;
      }
      else {
      }
      if ((CATT_(COM82_target_dir_, 8 + ((i__))) == '/')) {
         i__ = (int)(i__ - 1);
      }
      else {
         goto goto_tag_546_;
      }
   }
goto_tag_546_: ;
   target_dir__ = (ptr)STR20_head_(COM82_target_dir_,(i__ + 1));
   if (COM82_new_compilation_) {
      rmdir__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1376_)),target_dir__),(ptr)(&ls1377_)),(ptr)(&gs2304_)),(ptr)(&ls1379_)),target_dir__),(ptr)(&ls1380_));
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1381_));
      }
      else {
      }
      if (COM82_do_timing_) {
         TIM92_rmdir_time_ = (ptr)TIM73_time_syscall_(0,rmdir__);
      }
      else {
         GLO94_system_(0,rmdir__);
      }
   }
   else {
      if (CS_23_sdb_inc_compile_warning_) {
         (void)OUT80_nl_(OUT80_s_(CS_23_outfile_,(ptr)(&ls1383_)));
      }
      else {
      }
   }
   mkdir__ = (ptr)STR20_s_(STR20_s_(STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1385_)),target_dir__),(ptr)(&ls1377_)),(ptr)(&gs1384_)),' '),target_dir__),(ptr)(&ls1387_));
   if ((! COM82_warnings_only_)) {
      mkdir__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_s_(mkdir__,(ptr)(&gs2309_)),(ptr)(&ls1389_)),target_dir__),(ptr)(&ls1390_));
   }
   else {
   }
   mkdir__ = (ptr)STR20_s_(mkdir__,(ptr)(&ls1380_));
   if (COM82_do_timing_) {
      TIM92_mkdir_time_ = (ptr)TIM73_time_syscall_(0,mkdir__);
   }
   else {
      GLO94_system_(0,mkdir__);
   }

   ret0__:
   return;
}

void CS_23_process_classes_(ptr self__)
{
   SATHER_STR_(20,3,ls76_,"CS");
   SATHER_STR_(20,38,ls1394_,"Cannot install all predefined classes");
   SATHER_STR_(20,30,ls1398_,"* Actualizing parameters ...\n");
   SATHER_STR_(20,28,ls1400_,"* Customizing children ...\n");
   SATHER_STR_(20,27,ls1402_,"* Resolving built-ins ...\n");
   SATHER_STR_(20,32,ls1404_,"* Closing subtype relation ...\n");
   SATHER_STR_(20,28,ls1406_,"* Laying out instances ...\n");
   SATHER_STR_(20,25,ls1408_,"* Resolving clients ...\n");
   SATHER_STR_(20,29,ls1411_,"* Analysing consistency ...\n");
   SATHER_STR_(20,38,ls1416_," ERRORS found; Exit from compilation\n");
   SATHER_STR_(20,22,ls1418_,"* Coding classes ...\n");
   SATHER_STR_(20,22,ls1420_,"* Coding shareds ...\n");
   SATHER_STR_(20,19,ls1421_,"* Coding main ...\n");
   SATHER_STR_(20,27,ls1423_,"* Coding class tables ...\n");
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,27,ls1431_,"Not all main classes found");
   int    i__ = S_int_VOID_;
   ptr    lst_names__ = 0;
   ptr    def__ = 0;
   ptr    offset_info_fname__ = 0;
   ptr    offset_info__ = 0;

   if (SAT95_install_predefined_classes_(0)) {
      CS_23_predef_ok_ = (char)1;
   }
   else {
      ERR96_compiler_error_msg_(0,(ptr)(&ls76_),(ptr)(&ls1394_));
      exit(1);
   }
   GLO68_init_typeob_s_(0);
   i__ = (int)(RES97_LAST_PREDEF_ici_ + 1);
   lst_names__ = (ptr)LIS98_create_(0,50);
   if (((GLO68_creator_class_name_ > 0) & (! COM82_all_np_classes_))) {
      lst_names__ = (ptr)LIS98_union_(LIS98_push_(lst_names__,GLO68_creator_class_name_),SAT95_get_simple_classnames_(0));
   }
   else {
      while (1) {
         if ((i__ >= IATT_(GLO68_class_defs_,24))) {
            goto goto_tag_547_;
         }
         else {
         }
         def__ = (ptr)CLA66_at_index_(GLO68_class_defs_,i__);
         if ((def__ != 0)) {
            if ((CLA67_num_params_(def__) == 0)) {
               lst_names__ = (ptr)LIS98_push_(lst_names__,IATT_(def__,12));
            }
            else {
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_547_: ;
   }
   if ((SAT95_install_root_classes_(0,lst_names__) == IATT_(lst_names__,4))) {
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1398_));
      }
      else {
      }
      SAT95_all_create_inst_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1400_));
      }
      else {
      }
      SAT95_all_expand_cinh_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1402_));
      }
      else {
      }
      SAT95_all_resolve_predef_types_and_compute_num_attrs_(0,lst_names__);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1404_));
      }
      else {
      }
      SAT95_all_compute_anc_and_des_and_time_stamp_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1406_));
      }
      else {
      }
      SAT95_all_compute_attr_offsets_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1408_));
      }
      else {
      }
      SAT95_find_called_features_(0);
      if (COM82_do_timing_) {
         TIM92_start_semant_ = (ptr)TIM73_create_(0);
      }
      else {
      }
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1411_));
      }
      else {
      }
      SAT95_all_semant_(0);
      if (COM82_print_desinfo_) {
         SAT95_print_descendant_information_(0);
      }
      else {
      }
      if ((omerrs > 0)) {
         CS_23_error_exit_(self__,STR20_s_(INT15_to_s_(omerrs),(ptr)(&ls1416_)));
      }
      else {
      }
      if (COM82_do_timing_) {
         TIM92_start_print_ = (ptr)TIM73_create_(0);
      }
      else {
      }
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1418_));
      }
      else {
      }
      SAT95_all_class_inst_cprint_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1420_));
      }
      else {
      }
      SAT95_cprint_init_shareds_and_consts_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1421_));
      }
      else {
      }
      SAT95_cprint_init_(0);
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(CS_23_outfile_,(ptr)(&ls1423_));
      }
      else {
      }
      SAT95_cprint_class_info_(0);
      if (COM82_verbose_code_) {
         offset_info_fname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2279_));
         offset_info__ = (ptr)new_(99,0);
         SAT99_open_for_write_(offset_info__,offset_info_fname__);
         if ((SAT99_error_(offset_info__) != 0)) {
            CS_23_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),(ptr)(&gs2279_)),(ptr)(&ls632_)));
         }
         else {
            SAT95_print_defines_(0,offset_info__);
            SAT99_close_(offset_info__);
         }
      }
      else {
      }
      SAT95_print_sather_to_c_macros_(0);
      if (GLO68_print_feat_info_) {
         SAT95_print_browser_info_(0);
      }
      else {
      }
   }
   else {
      ERR96_compiler_error_msg_(0,(ptr)(&ls76_),(ptr)(&ls1431_));
   }

   ret0__:
   return;
}

void CS_23_handle_object_files_(ptr self__)
{
   ptr    object_files_keyob__ = 0;
   ptr    object_files__ = 0;
   int    csz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   object_files_keyob__ = (ptr)PATT_(PATT_(GLO68_dot_sather_reader_,16), 8 + ((1) << 2));
   object_files__ = (ptr)PATT_(object_files_keyob__,4);
   csz__ = (int)0;
   if ((object_files__ != 0)) {
      csz__ = (int)IATT_(object_files__,4);
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= csz__)) {
         goto goto_tag_548_;
      }
      else {
      }
      COM82_object_files_ = (ptr)STR20_s_(STR20_c_(COM82_object_files_,' '),PATT_(object_files__, 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_548_: ;

   ret0__:
   return;
}

void CS_23_print_times_(ptr self__)
{
   SATHER_STR_(20,52,ls1454_,"Timing in seconds:\nInitialization/Set-up options = ");
   SATHER_STR_(20,4,ls1455_,"  (");
   SATHER_STR_(20,37,ls1456_,")\nReading commands/System message = ");
   SATHER_STR_(20,22,ls1457_,")\nRead \".sa\" files = ");
   SATHER_STR_(20,30,ls1458_,")\nGeneral class processing = ");
   SATHER_STR_(20,20,ls1459_,")\nSemantic check = ");
   SATHER_STR_(20,19,ls1460_,")\nPrint C-files = ");
   SATHER_STR_(20,31,ls1461_,")\nMiscellaneous before make = ");
   SATHER_STR_(20,23,ls1462_,")\nCompiling C files = ");
   SATHER_STR_(20,16,ls1463_,")\nTotal time = ");
   SATHER_STR_(20,35,ls1464_,"\n\nPls send to clim@icsi; Thanks!\n\n");
   double    t1__ = S_double_VOID_;
   double    t2__ = S_double_VOID_;
   double    t3__ = S_double_VOID_;
   double    t4__ = S_double_VOID_;
   double    t5__ = S_double_VOID_;
   double    t6__ = S_double_VOID_;
   double    t7__ = S_double_VOID_;
   double    t8__ = S_double_VOID_;
   double    total__ = S_double_VOID_;

   if (COM82_do_timing_) {
      t1__ = (double)(((TIM73_time_spent_(TIM92_start_command_read_) - TIM73_time_spent_(TIM92_start_main_)) + TIM73_time_spent_(TIM92_rmdir_time_)) + TIM73_time_spent_(TIM92_mkdir_time_));
      t2__ = (double)(TIM73_time_spent_(TIM92_start_code_read_) - TIM73_time_spent_(TIM92_start_command_read_));
      t3__ = (double)(TIM73_time_spent_(TIM92_start_processing_) - TIM73_time_spent_(TIM92_start_code_read_));
      t4__ = (double)(TIM73_time_spent_(TIM92_start_semant_) - TIM73_time_spent_(TIM92_start_processing_));
      t5__ = (double)(TIM73_time_spent_(TIM92_start_print_) - TIM73_time_spent_(TIM92_start_semant_));
      t6__ = (double)((TIM73_time_spent_(TIM92_start_last_stage_) - TIM73_time_spent_(TIM92_start_print_)) + TIM73_time_spent_(TIM92_rmfiles_com_time_));
      t7__ = (double)((TIM73_time_spent_(TIM92_start_make_) - TIM73_time_spent_(TIM92_start_last_stage_)) + TIM73_time_spent_(TIM92_cp_com_time_));
      t8__ = (double)(((TIM73_time_spent_(TIM92_end_main_) - TIM73_time_spent_(TIM92_start_make_)) + TIM73_time_spent_(TIM92_mk_com_time_)) + TIM73_time_spent_(TIM92_mv_com_time_));
      total__ = (double)(((((((TIM73_time_spent_(TIM92_end_main_) - TIM73_time_spent_(TIM92_start_main_)) + TIM73_time_spent_(TIM92_rmdir_time_)) + TIM73_time_spent_(TIM92_mkdir_time_)) + TIM73_time_spent_(TIM92_rmfiles_com_time_)) + TIM73_time_spent_(TIM92_cp_com_time_)) + TIM73_time_spent_(TIM92_mk_com_time_)) + TIM73_time_spent_(TIM92_mv_com_time_));
      (void)OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(OUT9_r_(OUT9_s_(0,(ptr)(&ls1454_)),t1__),(ptr)(&ls1455_)),(t1__ / total__)),(ptr)(&ls1456_)),t2__),(ptr)(&ls1455_)),(t2__ / total__)),(ptr)(&ls1457_)),t3__),(ptr)(&ls1455_)),(t3__ / total__)),(ptr)(&ls1458_)),t4__),(ptr)(&ls1455_)),(t4__ / total__)),(ptr)(&ls1459_)),t5__),(ptr)(&ls1455_)),(t5__ / total__)),(ptr)(&ls1460_)),t6__),(ptr)(&ls1455_)),(t6__ / total__)),(ptr)(&ls1461_)),t7__),(ptr)(&ls1455_)),(t7__ / total__)),(ptr)(&ls1462_)),t8__),(ptr)(&ls1455_)),(t8__ / total__)),(ptr)(&ls1463_)),total__),(ptr)(&ls1464_));
   }
   else {
   }

   ret0__:
   return;
}

int CS_23_main_(ptr self__, ptr argv__)
{
   int res__ = S_int_VOID_;
   SATHER_STR_(20,40,ls1466_,"Error in reading compiler command file\n");

   res__ = (int)0;
   TIM92_start_main_ = (ptr)TIM73_create_(0);
   CS_23_main_init_(self__);
   CS_23_release_msg_(self__);
   CS_23_set_options_(self__,argv__);
   if (COM82_do_timing_) {
      TIM92_start_command_read_ = (ptr)TIM73_create_(0);
   }
   else {
   }
   if ((! CS_23_read_commands_(self__))) {
      CS_23_error_exit_(self__,(ptr)(&ls1466_));
   }
   else {
   }
   CS_23_system_msg_(self__);
   buf = (ptr)STR70_create_(0,0);
   OLD101_init_class_stat_tbl_(0);
   OLD101_adjust_tables_(0);
   CS_23_read_code_(self__);
   if (COM82_do_timing_) {
      TIM92_start_processing_ = (ptr)TIM73_create_(0);
   }
   else {
   }
   CS_23_process_classes_(self__);
   if (COM82_do_timing_) {
      TIM92_start_last_stage_ = (ptr)TIM73_create_(0);
   }
   else {
   }
   OLD101_store_class_stat_tbl_(0);
   OLD101_store_name_ind_map_(0);
   CS_23_handle_object_files_(self__);
   SAT95_print_makefile_(0);
   TIM92_end_main_ = (ptr)TIM73_create_(0);
   CS_23_print_times_(self__);

   ret0__:
   return (res__);
}

ptr CS_23_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


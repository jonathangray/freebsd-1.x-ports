/* global94.c : Sather class: GLOBAL_PROC, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR7_s_();
extern ptr ERR7_c_();
extern char CHA14_is_alphabetic_();
extern int INT15_bit_and_();
extern int INT15_rshift_();
extern int INT15_u_mod_();
extern int INT15_lshift_();
extern char INT15_nth_bit_();
extern ptr STR20_head_();
extern char STR20_is_equal_();
extern ptr STR20_tail_();
extern int STR20_length_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern char CHA14_is_digit_();
extern char CHA14_to_upper_case_();
extern int INT15_bit_or_();
extern /*shared*/ int GLO68_next_key_;
extern /*shared*/ int GLO68_dispatch_table_size_;
extern /*shared*/ ptr GLO68_dispatch_flags_;
extern /*shared*/ char GLO68_print_feat_info_;
extern /*shared*/ int GLO68_rt_type_;
extern /*shared*/ ptr GLO68_str_indices_;
extern /*shared*/ ptr GLO68_str_consts_;
extern ptr TIM73_time_syscall_();
extern /*constant*/ int RES71_copy_ind_;
extern /*constant*/ int RES71_new_ind_;
extern /*constant*/ int RES71_asize_ind_;
extern /*constant*/ int RES71_asize1_ind_;
extern ptr OUT80_s_();
extern /*constant*/ int RES71_asize2_ind_;
extern /*shared*/ char COM82_rt_code_check_;
extern /*shared*/ char COM82_gen_all_;
extern /*shared*/ char COM82_warnings_only_;
extern /*shared*/ char COM82_new_compilation_;
extern /*shared*/ ptr COM82_target_dir_;
extern /*shared*/ char COM82_do_timing_;
extern /*constant*/ ptr INS88_ln_command_;
extern /*constant*/ int RES71_asize3_ind_;
extern int UNI90_unlink_();
extern int UNI90_system_();
extern /*shared*/ ptr TIM92_cp_com_time_;
extern /*shared*/ int UNI90_unix_error_;
extern /*constant*/ int RES71_asize4_ind_;
extern /*constant*/ int RES71_extend_ind_;
extern ERR96_warning_msg_();
extern /*constant*/ int RES71_type_ind_;
extern int LIS98_contains_();
extern ptr LIS98_push_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*constant*/ int RES71_self_ind_;
extern /*constant*/ int RES71_res_ind_;
extern /*constant*/ int RES71_void_ind_;
extern ptr SAT99_s_();
extern ptr SAT99_i_();
extern ptr SAT99_c_();
extern ptr SAT99_indent_();
extern ptr SAT99_inc_ln_();
extern /*shared*/ int GLO68_g_tag_;
extern /*shared*/ char GLO68_self_exists_;
extern char LIS98_not_in_();
extern ptr STR70_create_();
extern ptr LIS98_create_();
extern ptr STR70_push_();
extern ptr STR70_terminate_();
extern EXP117_fob_error_();
extern EXP117_cprint_act_code_();
extern int STR69_index_of_str_();
extern ptr LIS98_push_unique_();
extern /*shared*/ ptr GLO68_class_inst_;
extern ptr CLA93_get_obj_();
extern ptr CLA93_at_index_();
extern ptr LST147_create_();
extern ptr CLA148_full_name_();
extern char TYP149_conforms_to_();
extern ptr LST147_push_();
extern char INT164_get_();
extern ptr LIS165_push_();
extern ptr UPA167_canonicalize_();
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern PRI187_cprint_rt_typechk_();
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1165_;
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
extern struct { int tp_; int sz_; char st_; } gs60_;
extern struct { int tp_; int sz_; char st_; } gs61_;
extern struct { int tp_; int sz_; char st_; } gs2302_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



int GLO94_global_key_();
GLO94_reset_global_key_();
/*constant*/ ptr GLO94_asize_fname_ = (ptr)(&gs23_);
/*constant*/ ptr GLO94_asize1_fname_ = (ptr)(&gs24_);
/*constant*/ ptr GLO94_asize2_fname_ = (ptr)(&gs25_);
/*constant*/ ptr GLO94_asize3_fname_ = (ptr)(&gs26_);
/*constant*/ ptr GLO94_asize4_fname_ = (ptr)(&gs27_);
/*constant*/ ptr GLO94_copy_fname_ = (ptr)(&gs28_);
/*constant*/ ptr GLO94_deep_copy_fname_ = (ptr)(&gs29_);
/*constant*/ ptr GLO94_extend_fname_ = (ptr)(&gs30_);
/*constant*/ ptr GLO94_init_fname_ = (ptr)(&gs61_);
/*constant*/ ptr GLO94_new_fname_ = (ptr)(&gs31_);
/*constant*/ ptr GLO94_type_fname_ = (ptr)(&gs32_);
/*constant*/ ptr GLO94_res_vname_ = (ptr)(&gs33_);
/*constant*/ ptr GLO94_self_vname_ = (ptr)(&gs34_);
/*constant*/ ptr GLO94_exception_vname_ = (ptr)(&gs60_);
/*constant*/ ptr GLO94_arg_vname_ = (ptr)(&gs1165_);
/*constant*/ ptr GLO94_false_name_ = (ptr)(&gs35_);
/*constant*/ ptr GLO94_true_name_ = (ptr)(&gs36_);
/*constant*/ ptr GLO94_void_name_ = (ptr)(&gs37_);
ptr GLO94_initialize_();
int GLO94_featname_from_key_();
int GLO94_classind_from_key_();
int GLO94_key_of_class_feat_();
int GLO94_help_insert_();
char GLO94_check_is_on_();
char GLO94_handle_class_p_();
char GLO94_handle_feature_p_();
char GLO94_is_clib_option_();
char GLO94_conform_tst_();
GLO94_add_str_const_();
GLO94_remove_user_cfile_();
GLO94_copy_user_cfile_();
GLO94_system_();
GLO94_check_f_ob_();
ptr GLO94_ctype_name_();
GLO94_cprint_ctype_name_();
GLO94_cprint_ctemp_name_();
GLO94_cprint_global_tmpnm_str_();
GLO94_cprint_local_tmpnm_str_();
GLO94_cprint_sather_str_type_();
GLO94_cprint_curr_exp_code_();
GLO94_cprint_goto_tag_();
char GLO94_cprint_ref_to_self_();
GLO94_cprint_octal_char_();
GLO94_cprint_int_as_char_seq_();
ptr GLO94_union_classnames_();
ptr GLO94_extract_simple_classnames_();
ptr GLO94_analyze_class_name_();
ptr GLO94_extract_poss_names_();
ptr GLO94_extract_filename_();
int GLO94_check_des_of_();
ptr GLO94_octal_();
ptr GLO94_ob_ob_();
extern int attr_ent_GLO94[];

int GLO94_global_key_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)GLO68_next_key_;
   GLO68_next_key_ = (int)(GLO68_next_key_ + 1);

   ret0__:
   return (res__);
}

GLO94_reset_global_key_(self__)
ptr self__;
{

   GLO68_next_key_ = (int)1;

   ret0__:
   return;
}

ptr GLO94_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

int GLO94_featname_from_key_(self__,k__)
ptr self__;
int k__;
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_rshift_(k__,14);

   ret0__:
   return (res__);
}

int GLO94_classind_from_key_(self__,k__)
ptr self__;
int k__;
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_bit_and_(k__,(INT15_lshift_(1,14) - 1));

   ret0__:
   return (res__);
}

int GLO94_key_of_class_feat_(self__,cnm__,feat__)
ptr self__;
int cnm__;
int feat__;
{
   int res__ = S_int_VOID_;

   res__ = (int)(INT15_lshift_(feat__,14) + cnm__);

   ret0__:
   return (res__);
}

int GLO94_help_insert_(self__,cnm__,feat__)
ptr self__;
int cnm__;
int feat__;
{
   int res__ = S_int_VOID_;
   int    key__ = S_int_VOID_;
   int    hash__ = S_int_VOID_;

   key__ = (int)GLO94_key_of_class_feat_(self__,cnm__,feat__);
   hash__ = (int)INT15_lshift_(INT15_rshift_(INT15_u_mod_((key__ * key__),GLO68_dispatch_table_size_),1),1);
   while (1) {
      if ((IATT_(GLO68_dispatch_flags_, 8 + (((hash__ + 1)) << 2)) == 0)) {
         IATT_(GLO68_dispatch_flags_, 8 + ((hash__) << 2)) = (int)1;
         IATT_(GLO68_dispatch_flags_, 8 + (((hash__ + 1)) << 2)) = (int)1;
         res__ = (int)hash__;
         goto goto_tag_1034_;
      }
      else {
         hash__ = (int)(hash__ + 2);
         if ((hash__ >= GLO68_dispatch_table_size_)) {
            hash__ = (int)0;
         }
         else {
         }
      }
   }
goto_tag_1034_: ;

   ret0__:
   return (res__);
}

char GLO94_check_is_on_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)COM82_rt_code_check_;

   ret0__:
   return (res__);
}

char GLO94_handle_class_p_(self__,co__)
ptr self__;
ptr co__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(CATT_(co__,9) | (COM82_gen_all_ & (! CATT_(co__,4))));

   ret0__:
   return (res__);
}

char GLO94_handle_feature_p_(self__,f__)
ptr self__;
ptr f__;
{
   char res__ = S_char_VOID_;
   ptr gl1035_;
   static int gl1036_;
   static union dtype_ gl1037_;
   ptr gl1038_;
   static int gl1039_;
   static union dtype_ gl1040_;
   char gl61_;
   char gl62_;

   gl1035_ = f__;
   cache_dispatch_(gl1035_,589,gl1036_,INTVAL_(gl1037_));
   gl62_ = CATT_(gl1035_,INTVAL_(gl1037_));
   gl1038_ = f__;
   cache_dispatch_(gl1038_,309,gl1039_,INTVAL_(gl1040_));
   gl61_ = CATT_(gl1038_,INTVAL_(gl1040_));
   res__ = (char)(gl62_ | ((COM82_gen_all_ | GLO68_print_feat_info_) & (! gl61_)));

   ret0__:
   return (res__);
}

char GLO94_is_clib_option_(self__,s__)
ptr self__;
ptr s__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,3,ls898_,"-l");

   res__ = (char)STR20_is_equal_(STR20_head_(s__,2),(ptr)(&ls898_));

   ret0__:
   return (res__);
}

char GLO94_conform_tst_(self__,c1__,c2__,exp__)
ptr self__;
ptr c1__;
ptr c2__;
ptr exp__;
{
   char res__ = S_char_VOID_;
   ptr gl1041_;
   static int gl1042_;
   static union dtype_ gl1043_;
   ptr gl1044_;
   static int gl1045_;
   static union dtype_ gl1046_;

   gl1041_ = c1__;
   cache_dispatch_(gl1041_,903,gl1042_,INTVAL_(gl1043_));
   res__ = (char)CFN_(gl1043_)(gl1041_,c2__);
   gl1044_ = exp__;
   cache_dispatch_(gl1044_,870,gl1045_,INTVAL_(gl1046_));
   IATT_(gl1044_,INTVAL_(gl1046_)) = (int)GLO68_rt_type_;

   ret0__:
   return (res__);
}

GLO94_add_str_const_(self__,str_const__)
ptr self__;
ptr str_const__;
{
   int    k__ = S_int_VOID_;

   k__ = (int)LIS98_contains_(GLO68_str_indices_,IATT_(str_const__,28));
   if ((k__ < 0)) {
      GLO68_str_consts_ = (ptr)LIS165_push_(GLO68_str_consts_,str_const__);
      GLO68_str_indices_ = (ptr)LIS98_push_(GLO68_str_indices_,IATT_(str_const__,28));
   }
   else {
   }

   ret0__:
   return;
}

GLO94_remove_user_cfile_(self__,fn__,outfile__)
ptr self__;
ptr fn__;
ptr outfile__;
{
   SATHER_STR_(20,32,ls913_,"* Removing unused user C file \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,32,ls916_,"*** Warning: Failure to remove ");
   SATHER_STR_(20,3,ls917_,".c");
   SATHER_STR_(20,3,ls918_,".s");
   SATHER_STR_(20,3,ls919_,".o");
   SATHER_STR_(20,3,ls920_,".h");
   SATHER_STR_(20,3,ls921_,".y");
   SATHER_STR_(20,46,ls922_,"*** Please remove any other files related to ");
   SATHER_STR_(20,6,ls923_," ***\n");
   ptr    suffix__ = 0;
   char    orig_suffix__ = S_char_VOID_;
   char    orig_suffix_63_ = S_char_VOID_;
   char    is_done__ = S_char_VOID_;
   int    last_ind__ = S_int_VOID_;

   suffix__ = (ptr)STR20_tail_(fn__,2);
   orig_suffix__ = S_char_VOID_;
   if ((STR20_length_(suffix__) == 2)) {
      orig_suffix_63_ = (char)CATT_(suffix__, 8 + ((1)));
   }
   else {
   }
   is_done__ = (char)0;
   last_ind__ = (int)(STR20_length_(fn__) - 1);
   while (1) {
      if (is_done__) {
         goto goto_tag_1047_;
      }
      else {
      }
      if ((! COM82_warnings_only_)) {
         (void)OUT80_s_(OUT80_s_(OUT80_s_(outfile__,(ptr)(&ls913_)),fn__),(ptr)(&ls632_));
      }
      else {
      }
      if ((UNI90_unlink_(0,fn__) != 0)) {
         (void)ERR7_c_(ERR7_s_(ERR7_s_(0,(ptr)(&ls916_)),fn__),'\n');
      }
      else {
      }
      if ((STR20_is_equal_(suffix__,(ptr)(&ls917_)) | STR20_is_equal_(suffix__,(ptr)(&ls918_)))) {
         CATT_(suffix__, 8 + ((1))) = (char)'o';
         CATT_(fn__, 8 + ((last_ind__))) = (char)'o';
      }
      else {
         if (STR20_is_equal_(suffix__,(ptr)(&ls919_))) {
            is_done__ = (char)1;
         }
         else {
            if (STR20_is_equal_(suffix__,(ptr)(&ls920_))) {
               is_done__ = (char)1;
            }
            else {
               if (STR20_is_equal_(suffix__,(ptr)(&ls921_))) {
                  CATT_(suffix__, 8 + ((1))) = (char)'c';
                  CATT_(fn__, 8 + ((last_ind__))) = (char)'c';
               }
               else {
                  if ((! COM82_warnings_only_)) {
                     (void)OUT80_s_(OUT80_s_(OUT80_s_(outfile__,(ptr)(&ls922_)),fn__),(ptr)(&ls923_));
                  }
                  else {
                  }
                  is_done__ = (char)1;
               }
            }
         }
      }
   }
goto_tag_1047_: ;
   CATT_(fn__, 8 + ((last_ind__))) = (char)orig_suffix__;

   ret0__:
   return;
}

GLO94_copy_user_cfile_(self__,fn__,outfile__)
ptr self__;
ptr fn__;
ptr outfile__;
{
   ptr    cp_com__ = 0;

   fn__ = (ptr)UPA167_canonicalize_(0,fn__);
   if ((! COM82_new_compilation_)) {
   }
   else {
      cp_com__ = (ptr)STR20_s_(STR20_c_(STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),(ptr)(&gs2302_)),' '),fn__),' '),COM82_target_dir_);
      if (COM82_do_timing_) {
         TIM92_cp_com_time_ = (ptr)TIM73_time_syscall_(0,cp_com__);
      }
      else {
         GLO94_system_(0,cp_com__);
      }
   }

   ret0__:
   return;
}

GLO94_system_(self__,com__)
ptr self__;
ptr com__;
{
   SATHER_STR_(20,20,ls938_,"*** Shell command \"");
   SATHER_STR_(20,25,ls939_,"\" not executed. Reason: ");
   SATHER_STR_(20,21,ls941_,", cf <errno.h> ***\n\n");
   int    status__ = S_int_VOID_;

   status__ = (int)UNI90_system_(0,com__);
   if ((INT15_rshift_(INT15_bit_and_(status__,0xFF00),8) == 0xFF)) {
      ERR96_warning_msg_(0,STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls938_)),com__),(ptr)(&ls939_)),UNI90_unix_error_),(ptr)(&ls941_)));
   }
   else {
   }

   ret0__:
   return;
}

GLO94_check_f_ob_(self__,exp__,name__,symtab__)
ptr self__;
ptr exp__;
int name__;
ptr symtab__;
{
   ptr gl1048_;
   static int gl1049_;
   static union dtype_ gl1050_;
   ptr gl64_;
   ptr gl65_;

   switch (name__) {
      case (28) :
      case (31) :
      case (23) :
      case (24) :
      case (25) :
      case (26) :
      case (27) :
      case (30) :
      case (32) :
         gl64_ = STR69_at_index_(GLO68_str_table_,name__);
         gl65_ = CLA148_full_name_(PATT_(symtab__,4));
         gl1048_ = exp__;
         cache_dispatch_(gl1048_,952,gl1049_,INTVAL_(gl1050_));
         VFN_(gl1050_)(gl1048_,gl64_,gl65_);
         break;
      case (34) :
      case (33) :
      case (37) :
         break;
      default:
         ;
   }

   ret0__:
   return;
}

ptr GLO94_ctype_name_(self__,ctype__)
ptr self__;
int ctype__;
{
   ptr res__ = 0;

   switch (ctype__) {
      case (1) :
         res__ = (ptr)(ptr)(&gs1215_);
         break;
      case (2) :
         res__ = (ptr)(ptr)(&gs1216_);
         break;
      case (3) :
         res__ = (ptr)(ptr)(&gs1217_);
         break;
      case (4) :
         res__ = (ptr)(ptr)(&gs1218_);
         break;
      case (5) :
         res__ = (ptr)(ptr)(&gs1219_);
         break;
      default:
         ;
         ;
   }

   ret0__:
   return (res__);
}

GLO94_cprint_ctype_name_(self__,ctype__,outfile__)
ptr self__;
int ctype__;
ptr outfile__;
{

   switch (ctype__) {
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

GLO94_cprint_ctemp_name_(self__,intval__,outfile__)
ptr self__;
int intval__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls957_,"gl");

   if ((intval__ < 0)) {
      intval__ = (int)(- intval__);
   }
   else {
   }
   (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls957_)),intval__),'_');

   ret0__:
   return;
}

GLO94_cprint_global_tmpnm_str_(self__,strval__,outfile__)
ptr self__;
int strval__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls958_,"gs");

   (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls958_)),strval__),'_');

   ret0__:
   return;
}

GLO94_cprint_local_tmpnm_str_(self__,strval__,outfile__)
ptr self__;
int strval__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls960_,"ls");

   (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls960_)),strval__),'_');

   ret0__:
   return;
}

GLO94_cprint_sather_str_type_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,40,ls961_,"struct { int tp_; int sz_; char st_; } ");

   (void)SAT99_s_(outfile__,(ptr)(&ls961_));

   ret0__:
   return;
}

GLO94_cprint_curr_exp_code_(self__,exp__,temp_nm__,outfile__)
ptr self__;
ptr exp__;
int temp_nm__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl1051_;
   static int gl1052_;
   static union dtype_ gl1053_;
   ptr gl1054_;
   static int gl1055_;
   static union dtype_ gl1056_;
   ptr gl1057_;
   static int gl1058_;
   static union dtype_ gl1059_;
   int gl66_;
   int gl67_;

   (void)SAT99_indent_(outfile__);
   GLO94_cprint_ctemp_name_(0,temp_nm__,outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls964_));
   gl1051_ = exp__;
   cache_dispatch_(gl1051_,965,gl1052_,INTVAL_(gl1053_));
   VFN_(gl1053_)(gl1051_,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   gl1054_ = exp__;
   cache_dispatch_(gl1054_,870,gl1055_,INTVAL_(gl1056_));
   gl66_ = IATT_(gl1054_,INTVAL_(gl1056_));
   gl1057_ = exp__;
   cache_dispatch_(gl1057_,298,gl1058_,INTVAL_(gl1059_));
   gl67_ = IATT_(gl1057_,INTVAL_(gl1059_));
   PRI187_cprint_rt_typechk_(0,outfile__,gl66_,temp_nm__,gl67_);

   ret0__:
   return;
}

GLO94_cprint_goto_tag_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   SATHER_STR_(20,10,ls968_,"goto_tag_");

   (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls968_)),i__),'_');
   if ((GLO68_g_tag_ > 0)) {
      (void)SAT99_c_(SAT99_i_(outfile__,GLO68_g_tag_),'_');
   }
   else {
   }

   ret0__:
   return;
}

char GLO94_cprint_ref_to_self_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,7,ls970_,"self__");

   if (GLO68_self_exists_) {
      res__ = (char)1;
      (void)SAT99_s_(outfile__,(ptr)(&ls970_));
   }
   else {
      (void)SAT99_c_(outfile__,'0');
   }

   ret0__:
   return (res__);
}

GLO94_cprint_octal_char_(self__,v__,outfile__)
ptr self__;
int v__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)3;
   while (1) {
      if ((i__ < 0)) {
         goto goto_tag_1060_;
      }
      else {
      }
      (void)SAT99_c_(outfile__,'\\');
      if (INT15_nth_bit_(v__,((i__ * 8) + 7))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 6))) {
            (void)SAT99_c_(outfile__,'3');
         }
         else {
            (void)SAT99_c_(outfile__,'2');
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 6))) {
            (void)SAT99_c_(outfile__,'1');
         }
         else {
            (void)SAT99_c_(outfile__,'0');
         }
      }
      if (INT15_nth_bit_(v__,((i__ * 8) + 5))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 4))) {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'7');
            }
            else {
               (void)SAT99_c_(outfile__,'6');
            }
         }
         else {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'5');
            }
            else {
               (void)SAT99_c_(outfile__,'4');
            }
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 4))) {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'3');
            }
            else {
               (void)SAT99_c_(outfile__,'2');
            }
         }
         else {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'1');
            }
            else {
               (void)SAT99_c_(outfile__,'0');
            }
         }
      }
      if (INT15_nth_bit_(v__,((i__ * 8) + 2))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 1))) {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'7');
            }
            else {
               (void)SAT99_c_(outfile__,'6');
            }
         }
         else {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'5');
            }
            else {
               (void)SAT99_c_(outfile__,'4');
            }
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 1))) {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'3');
            }
            else {
               (void)SAT99_c_(outfile__,'2');
            }
         }
         else {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'1');
            }
            else {
               (void)SAT99_c_(outfile__,'0');
            }
         }
      }
      i__ = (int)(i__ - 1);
   }
goto_tag_1060_: ;

   ret0__:
   return;
}

GLO94_cprint_int_as_char_seq_(self__,v__,outfile__)
ptr self__;
int v__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)3;
   while (1) {
      if ((i__ < 0)) {
         goto goto_tag_1061_;
      }
      else {
      }
      (void)SAT99_c_(outfile__,'\\');
      if (INT15_nth_bit_(v__,((i__ * 8) + 7))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 6))) {
            (void)SAT99_c_(outfile__,'3');
         }
         else {
            (void)SAT99_c_(outfile__,'2');
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 6))) {
            (void)SAT99_c_(outfile__,'1');
         }
         else {
            (void)SAT99_c_(outfile__,'0');
         }
      }
      if (INT15_nth_bit_(v__,((i__ * 8) + 5))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 4))) {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'7');
            }
            else {
               (void)SAT99_c_(outfile__,'6');
            }
         }
         else {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'5');
            }
            else {
               (void)SAT99_c_(outfile__,'4');
            }
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 4))) {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'3');
            }
            else {
               (void)SAT99_c_(outfile__,'2');
            }
         }
         else {
            if (INT15_nth_bit_(v__,((i__ * 8) + 3))) {
               (void)SAT99_c_(outfile__,'1');
            }
            else {
               (void)SAT99_c_(outfile__,'0');
            }
         }
      }
      if (INT15_nth_bit_(v__,((i__ * 8) + 2))) {
         if (INT15_nth_bit_(v__,((i__ * 8) + 1))) {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'7');
            }
            else {
               (void)SAT99_c_(outfile__,'6');
            }
         }
         else {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'5');
            }
            else {
               (void)SAT99_c_(outfile__,'4');
            }
         }
      }
      else {
         if (INT15_nth_bit_(v__,((i__ * 8) + 1))) {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'3');
            }
            else {
               (void)SAT99_c_(outfile__,'2');
            }
         }
         else {
            if (INT15_nth_bit_(v__,(i__ * 8))) {
               (void)SAT99_c_(outfile__,'1');
            }
            else {
               (void)SAT99_c_(outfile__,'0');
            }
         }
      }
      i__ = (int)(i__ - 1);
      if ((i__ >= 0)) {
         (void)SAT99_c_(outfile__,',');
      }
      else {
      }
   }
goto_tag_1061_: ;

   ret0__:
   return;
}

ptr GLO94_union_classnames_(self__,l1__,l2__)
ptr self__;
ptr l1__;
ptr l2__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)1;
   sz__ = S_int_VOID_;
   if ((l2__ != 0)) {
      sz__ = (int)IATT_(l2__,4);
   }
   else {
   }
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1062_;
      }
      else {
      }
      if (LIS98_not_in_(l1__,IATT_(l2__, 16 + ((i__) << 2)))) {
         l1__ = (ptr)LIS98_push_(l1__,IATT_(l2__, 16 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1062_: ;
   res__ = (ptr)l1__;

   ret0__:
   return (res__);
}

ptr GLO94_extract_simple_classnames_(self__,str__,start_index__)
ptr self__;
ptr str__;
int start_index__;
{
   ptr res__ = 0;
   ptr    classname__ = 0;
   int    classname_ind__ = S_int_VOID_;
   int    state__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    subres__ = 0;
   char    ch__ = S_char_VOID_;
   int    new_ind__ = S_int_VOID_;

   classname__ = (ptr)STR70_create_(0,5);
   classname_ind__ = S_int_VOID_;
   state__ = (int)0;
   i__ = (int)start_index__;
   sz__ = (int)STR20_length_(str__);
   subres__ = S_ptr_VOID_;
   res__ = (ptr)LIS98_push_(LIS98_create_(0,2),(- 1));
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1063_;
      }
      else {
      }
      ch__ = (char)CATT_(str__, 8 + ((i__)));
      switch (state__) {
         case (0) :
            if ((CHA14_is_alphabetic_(ch__) | (ch__ == '_'))) {
               classname__ = (ptr)STR70_push_(classname__,ch__);
               state__ = (int)1;
            }
            else {
               state__ = (int)7;
            }
            break;
         case (1) :
            if (((CHA14_is_alphabetic_(ch__) | CHA14_is_digit_(ch__)) | (ch__ == '_'))) {
               classname__ = (ptr)STR70_push_(classname__,ch__);
            }
            else {
               if ((ch__ == '{')) {
                  state__ = (int)2;
               }
               else {
                  if (((ch__ == ',') | (ch__ == '}'))) {
                     classname__ = (ptr)STR70_terminate_(classname__);
                     classname_ind__ = (int)STR69_index_of_str_(GLO68_str_table_,classname__);
                     res__ = (ptr)LIS98_push_unique_(res__,classname_ind__);
                     IATT_(res__, 16 + ((0) << 2)) = (int)i__;
                     goto ret0__;
                  }
                  else {
                     state__ = (int)7;
                  }
               }
            }
            break;
         case (2) :
            subres__ = (ptr)GLO94_extract_simple_classnames_(self__,str__,i__);
            if ((subres__ == 0)) {
               res__ = (ptr)0;
               goto ret0__;
            }
            else {
            }
            if (((IATT_(subres__, 16 + ((0) << 2)) <= 0) | (IATT_(subres__, 16 + ((0) << 2)) > IATT_(str__,4)))) {
               res__ = (ptr)0;
               goto ret0__;
            }
            else {
               new_ind__ = (int)IATT_(subres__, 16 + ((0) << 2));
               if ((CATT_(str__, 8 + ((new_ind__))) == ',')) {
                  res__ = (ptr)GLO94_union_classnames_(self__,res__,subres__);
                  i__ = (int)new_ind__;
               }
               else {
                  if ((CATT_(str__, 8 + ((new_ind__))) == '}')) {
                     res__ = (ptr)GLO94_union_classnames_(self__,res__,subres__);
                     IATT_(res__, 16 + ((0) << 2)) = (int)(new_ind__ + 1);
                     goto ret0__;
                  }
                  else {
                     state__ = (int)7;
                  }
               }
            }
            break;
         default:
            ;
            ;
      }
      if ((state__ == 7)) {
         res__ = (ptr)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1063_: ;
   if (((state__ == 0) | (state__ == 1))) {
      classname__ = (ptr)STR70_terminate_(classname__);
      classname_ind__ = (int)STR69_index_of_str_(GLO68_str_table_,classname__);
      res__ = (ptr)LIS98_push_(res__,classname_ind__);
      IATT_(res__, 16 + ((0) << 2)) = (int)i__;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr GLO94_analyze_class_name_(self__,str__,start_index__)
ptr self__;
ptr str__;
int start_index__;
{
   ptr res__ = 0;
   ptr    classname__ = 0;
   ptr    local_key__ = 0;
   ptr    co__ = 0;
   int    classname_ind__ = S_int_VOID_;
   int    state__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    subres__ = 0;
   char    ch__ = S_char_VOID_;
   ptr    tmp_key__ = 0;
   int    new_ind__ = S_int_VOID_;
   ptr    tmp_key_68_ = 0;

   classname__ = (ptr)STR70_create_(0,5);
   local_key__ = (ptr)LST147_create_(0,1);
   co__ = S_ptr_VOID_;
   classname_ind__ = S_int_VOID_;
   state__ = (int)0;
   i__ = (int)start_index__;
   sz__ = (int)STR20_length_(str__);
   subres__ = S_ptr_VOID_;
   res__ = (ptr)LIS98_create_(0,2);
   while (1) {
      if (((i__ >= sz__) | (CATT_(str__, 8 + ((i__))) == '\00'))) {
         goto goto_tag_1064_;
      }
      else {
      }
      ch__ = (char)CATT_(str__, 8 + ((i__)));
      switch (state__) {
         case (0) :
            if ((CHA14_is_alphabetic_(ch__) | (ch__ == '_'))) {
               classname__ = (ptr)STR70_push_(classname__,CHA14_to_upper_case_(ch__));
               state__ = (int)1;
            }
            else {
               state__ = (int)7;
            }
            break;
         case (1) :
            if (((CHA14_is_alphabetic_(ch__) | CHA14_is_digit_(ch__)) | (ch__ == '_'))) {
               classname__ = (ptr)STR70_push_(classname__,CHA14_to_upper_case_(ch__));
            }
            else {
               if ((ch__ == '{')) {
                  classname__ = (ptr)STR70_terminate_(classname__);
                  classname_ind__ = (int)STR69_index_of_str_(GLO68_str_table_,classname__);
                  local_key__ = (ptr)LST147_push_(local_key__,classname_ind__);
                  state__ = (int)2;
               }
               else {
                  if (((ch__ == ',') | (ch__ == '}'))) {
                     IATT_(res__, 16 + ((1) << 2)) = (int)i__;
                     tmp_key__ = (ptr)LST147_push_(LST147_create_(0,1),STR69_index_of_str_(GLO68_str_table_,classname__));
                     co__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,tmp_key__);
                     if ((co__ == 0)) {
                        res__ = (ptr)0;
                        goto ret0__;
                     }
                     else {
                     }
                     IATT_(res__, 16 + ((0) << 2)) = (int)IATT_(co__,20);
                     goto ret0__;
                  }
                  else {
                     state__ = (int)7;
                  }
               }
            }
            break;
         case (2) :
            subres__ = (ptr)GLO94_analyze_class_name_(self__,str__,i__);
            if ((subres__ == 0)) {
               res__ = (ptr)0;
               goto ret0__;
            }
            else {
            }
            if (((IATT_(subres__, 16 + ((1) << 2)) <= 0) | (IATT_(subres__, 16 + ((1) << 2)) > IATT_(str__,4)))) {
               res__ = (ptr)0;
               goto ret0__;
            }
            else {
               new_ind__ = (int)IATT_(subres__, 16 + ((1) << 2));
               if ((CATT_(str__, 8 + ((new_ind__))) == ',')) {
                  local_key__ = (ptr)LST147_push_(local_key__,IATT_(subres__, 16 + ((0) << 2)));
                  i__ = (int)new_ind__;
               }
               else {
                  if ((CATT_(str__, 8 + ((new_ind__))) == '}')) {
                     local_key__ = (ptr)LST147_push_(local_key__,IATT_(subres__, 16 + ((0) << 2)));
                     co__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,local_key__);
                     if ((co__ == 0)) {
                        res__ = (ptr)0;
                     }
                     else {
                        IATT_(res__, 16 + ((0) << 2)) = (int)IATT_(co__,20);
                        IATT_(res__, 16 + ((1) << 2)) = (int)(new_ind__ + 1);
                     }
                     goto ret0__;
                  }
                  else {
                     state__ = (int)7;
                  }
               }
            }
            break;
         default:
            ;
            ;
      }
      if ((state__ == 7)) {
         res__ = (ptr)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1064_: ;
   if ((state__ == 1)) {
      IATT_(res__, 16 + ((1) << 2)) = (int)i__;
      classname__ = (ptr)STR70_terminate_(classname__);
      tmp_key_68_ = (ptr)LST147_push_(LST147_create_(0,1),STR69_index_of_str_(GLO68_str_table_,classname__));
      co__ = (ptr)CLA93_get_obj_(GLO68_class_inst_,tmp_key_68_);
      if ((co__ == 0)) {
         res__ = (ptr)0;
         goto ret0__;
      }
      else {
      }
      IATT_(res__, 16 + ((0) << 2)) = (int)IATT_(co__,20);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr GLO94_extract_poss_names_(self__,s__)
ptr self__;
ptr s__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   char    split_p__ = S_char_VOID_;
   ptr    s1__ = 0;
   ptr    s2__ = 0;
   int    j__ = S_int_VOID_;

   i__ = (int)0;
   split_p__ = (char)0;
   while (1) {
      if (((i__ >= IATT_(s__,4)) | (CATT_(s__, 8 + ((i__))) == '\00'))) {
         goto goto_tag_1065_;
      }
      else {
      }
      if ((CATT_(s__, 8 + ((i__))) == ':')) {
         if ((CATT_(s__, 8 + (((i__ + 1)))) == ':')) {
            split_p__ = (char)1;
            goto goto_tag_1065_;
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1065_: ;
   if (((i__ <= 0) | (! split_p__))) {
      goto ret0__;
   }
   else {
   }
   s1__ = (ptr)new1_(20,(i__ + 1),1);
   s2__ = (ptr)new1_(20,(IATT_(s__,4) - (i__ + 2)),1);
   j__ = (int)0;
   while (1) {
      if ((j__ >= i__)) {
         goto goto_tag_1066_;
      }
      else {
      }
      CATT_(s1__, 8 + ((j__))) = (char)CATT_(s__, 8 + ((j__)));
      j__ = (int)(j__ + 1);
   }
goto_tag_1066_: ;
   CATT_(s1__, 8 + ((j__))) = (char)'\00';
   j__ = (int)(i__ + 2);
   while (1) {
      if (((j__ >= IATT_(s__,4)) | (CATT_(s__, 8 + ((j__))) == '\00'))) {
         goto goto_tag_1067_;
      }
      else {
      }
      CATT_(s2__, 8 + (((j__ - (i__ + 2))))) = (char)CATT_(s__, 8 + ((j__)));
      j__ = (int)(j__ + 1);
   }
goto_tag_1067_: ;
   CATT_(s2__, 8 + (((j__ - (i__ + 2))))) = (char)'\00';
   res__ = (ptr)new1_(77,2,0);
   PATT_(res__, 8 + ((0) << 2)) = (ptr)s1__;
   PATT_(res__, 8 + ((1) << 2)) = (ptr)s2__;

   ret0__:
   return (res__);
}

ptr GLO94_extract_filename_(self__,fullname__)
ptr self__;
ptr fullname__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    start_of_fname__ = S_int_VOID_;
   int    len__ = S_int_VOID_;
   char    seen_back_slash__ = S_char_VOID_;

   i__ = (int)0;
   start_of_fname__ = (int)0;
   len__ = (int)STR20_length_(fullname__);
   seen_back_slash__ = S_char_VOID_;
   while (1) {
      if ((i__ >= len__)) {
         goto goto_tag_1068_;
      }
      else {
      }
      if ((CATT_(fullname__, 8 + ((i__))) == '/')) {
         if ((! seen_back_slash__)) {
            start_of_fname__ = (int)(i__ + 1);
         }
         else {
         }
      }
      else {
         if ((CATT_(fullname__, 8 + ((i__))) == '\\')) {
            seen_back_slash__ = (char)1;
         }
         else {
            seen_back_slash__ = (char)0;
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1068_: ;
   res__ = (ptr)STR20_tail_(fullname__,(len__ - start_of_fname__));

   ret0__:
   return (res__);
}

int GLO94_check_des_of_(self__,ici__,l__,u__)
ptr self__;
int ici__;
int l__;
int u__;
{
   int res__ = S_int_VOID_;
   SATHER_STR_(20,52,ls1004_,"Warning: Verify use of \"GLOBAL_PROC::check_anc_of\"\n");
   ptr    co__ = 0;
   ptr    des_set__ = 0;
   int    curr_bit__ = S_int_VOID_;

   if ((((l__ - u__) + 1) > 32)) {
      (void)ERR7_s_(0,(ptr)(&ls1004_));
   }
   else {
   }
   co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,ici__);
   if ((co__ != 0)) {
      des_set__ = (ptr)PATT_(co__,76);
      if ((des_set__ != 0)) {
         curr_bit__ = (int)1;
         while (1) {
            if ((l__ > u__)) {
               goto goto_tag_1069_;
            }
            else {
            }
            if (INT164_get_(des_set__,l__)) {
               res__ = (int)INT15_bit_or_(res__,curr_bit__);
            }
            else {
            }
            curr_bit__ = (int)INT15_lshift_(curr_bit__,1);
            l__ = (int)(l__ + 1);
         }
      goto_tag_1069_: ;
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr GLO94_octal_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;
   int    j__ = S_int_VOID_;

   res__ = (ptr)STR20_c_(STR20_create_(0),'0');
   if (INT15_nth_bit_(i__,31)) {
      if (INT15_nth_bit_(i__,30)) {
         res__ = (ptr)STR20_c_(res__,'3');
      }
      else {
         res__ = (ptr)STR20_c_(res__,'2');
      }
   }
   else {
      if (INT15_nth_bit_(i__,31)) {
         res__ = (ptr)STR20_c_(res__,'1');
      }
      else {
      }
   }
   j__ = (int)29;
   while (1) {
      if ((j__ < 0)) {
         goto goto_tag_1070_;
      }
      else {
      }
      if (INT15_nth_bit_(i__,j__)) {
         if (INT15_nth_bit_(i__,(j__ + 1))) {
            if (INT15_nth_bit_(i__,(j__ + 2))) {
               res__ = (ptr)STR20_c_(res__,'7');
            }
            else {
               res__ = (ptr)STR20_c_(res__,'6');
            }
         }
         else {
            if (INT15_nth_bit_(i__,(j__ + 2))) {
               res__ = (ptr)STR20_c_(res__,'5');
            }
            else {
               res__ = (ptr)STR20_c_(res__,'4');
            }
         }
      }
      else {
         if (INT15_nth_bit_(i__,(j__ + 1))) {
            if (INT15_nth_bit_(i__,(j__ + 2))) {
               res__ = (ptr)STR20_c_(res__,'3');
            }
            else {
               res__ = (ptr)STR20_c_(res__,'2');
            }
         }
         else {
            if (INT15_nth_bit_(i__,(j__ + 2))) {
               res__ = (ptr)STR20_c_(res__,'1');
            }
            else {
            }
         }
      }
      j__ = (int)(j__ - 3);
   }
goto_tag_1070_: ;

   ret0__:
   return (res__);
}

ptr GLO94_ob_ob_(self__,ob__)
ptr self__;
ptr ob__;
{
   ptr res__ = 0;

   res__ = (ptr)ob__;

   ret0__:
   return (res__);
}


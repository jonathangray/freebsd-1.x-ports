/* oldnew101.c : Sather class: OLDNEW_HANDLER, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR7_s_();
extern ptr ERR7_i_();
extern ptr ERR7_c_();
extern ptr OUT9_s_();
extern ptr CLA148_full_name_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern char STR20_is_equal_();
extern int CLA161_get_ind_();
extern ptr CLA161_at_index_();
extern ptr CLA161_alloc_create_();
extern ptr CLA161_create_();
extern CLA161_record_curr_compilation_();
extern CLA161_persist_();
extern STR171_open_for_write_();
extern int STR171_error_();
extern ptr STR171_i_();
extern ptr STR171_c_();
extern ptr STR171_s_();
extern STR171_close_();
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_class_stat_tbl_;
extern /*shared*/ ptr GLO68_cc_flags_;
extern /*shared*/ ptr GLO68_str_table_;
extern int STR78_get_();
extern /*shared*/ char COM82_new_compilation_;
extern /*shared*/ ptr COM82_target_dir_;
extern /*shared*/ char COM82_rt_code_check_;
extern /*shared*/ int COM82_compiler_mode_;
extern /*constant*/ int COM82_browser_mode_;
extern /*shared*/ char COM82_gen_all_;
extern /*constant*/ ptr INS88_class_stat_file_;
extern /*shared*/ char COM82_gen_base_;
extern /*shared*/ char COM82_verbose_code_;
extern /*shared*/ ptr COM82_sather_home_;
extern char STR218_get_();
extern CLA93_add_unique_obj_at_();
extern CLA93_add_unique_obj_();
extern int STR218_size_();
extern CLA93_expand_();
extern /*constant*/ ptr INS88_name_ind_file_;
extern ERR96_compiler_error_msg_();
extern struct { int tp_; int sz_; char st_; } gs2282_;
extern struct { int tp_; int sz_; char st_; } gs2284_;
#include "macros_.h"



OLD101_install_new_classob_s_();
char OLD101_updated_p_();
int OLD101_get_def_time_stamp_();
int OLD101_get_time_stamp_();
int OLD101_get_ctype_();
OLD101_init_class_stat_tbl_();
OLD101_adjust_tables_();
OLD101_store_class_stat_tbl_();
OLD101_store_name_ind_map_();
ptr OLD101_initialize_();
extern int attr_ent_OLD101[];

OLD101_install_new_classob_s_(self__,loc__,k__,co__)
ptr self__;
int loc__;
ptr k__;
ptr co__;
{
   SATHER_STR_(20,41,ls2863_,"Warning: Numbering of predefined class \"");
   SATHER_STR_(20,20,ls2864_,"\" has changed from ");
   SATHER_STR_(20,5,ls2865_," to ");
   int    prev_ind__ = S_int_VOID_;
   int    prev_ind_95_ = S_int_VOID_;

   if (COM82_new_compilation_) {
      if ((loc__ > 0)) {
         CLA93_add_unique_obj_at_(GLO68_class_inst_,k__,co__,loc__);
      }
      else {
         prev_ind__ = (int)CLA161_get_ind_(GLO68_class_stat_tbl_,CLA148_full_name_(co__));
         if ((prev_ind__ > 0)) {
            CLA93_add_unique_obj_at_(GLO68_class_inst_,k__,co__,prev_ind__);
         }
         else {
            CLA93_add_unique_obj_(GLO68_class_inst_,k__,co__);
         }
      }
   }
   else {
      prev_ind_95_ = (int)CLA161_get_ind_(GLO68_class_stat_tbl_,CLA148_full_name_(co__));
      if ((loc__ > 0)) {
         if ((prev_ind_95_ > 0)) {
            if ((prev_ind_95_ != loc__)) {
               (void)ERR7_c_(ERR7_i_(ERR7_s_(ERR7_i_(ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls2863_)),CLA148_full_name_(co__)),(ptr)(&ls2864_)),prev_ind_95_),(ptr)(&ls2865_)),loc__),'\n');
            }
            else {
            }
         }
         else {
         }
         CLA93_add_unique_obj_at_(GLO68_class_inst_,k__,co__,loc__);
      }
      else {
         if ((prev_ind_95_ > 0)) {
            CLA93_add_unique_obj_at_(GLO68_class_inst_,k__,co__,prev_ind_95_);
         }
         else {
            CLA93_add_unique_obj_(GLO68_class_inst_,k__,co__);
         }
      }
   }

   ret0__:
   return;
}

char OLD101_updated_p_(self__,co__)
ptr self__;
ptr co__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(CATT_(co__,12) | CATT_(co__,13));

   ret0__:
   return (res__);
}

int OLD101_get_def_time_stamp_(self__,co__)
ptr self__;
ptr co__;
{
   int res__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   c_stat__ = (ptr)CLA161_at_index_(GLO68_class_stat_tbl_,IATT_(co__,20));
   if ((c_stat__ == 0)) {
      goto ret0__;
   }
   else {
   }
   res__ = (int)IATT_(c_stat__,12);

   ret0__:
   return (res__);
}

int OLD101_get_time_stamp_(self__,co__)
ptr self__;
ptr co__;
{
   int res__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   c_stat__ = (ptr)CLA161_at_index_(GLO68_class_stat_tbl_,IATT_(co__,20));
   if ((c_stat__ == 0)) {
      goto ret0__;
   }
   else {
   }
   res__ = (int)IATT_(c_stat__,16);

   ret0__:
   return (res__);
}

int OLD101_get_ctype_(self__,co__)
ptr self__;
ptr co__;
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   i__ = (int)STR78_get_(PATT_(GLO68_class_stat_tbl_,24),CLA148_full_name_(co__));
   if ((i__ <= 0)) {
      res__ = (int)(- 1);
   }
   else {
      c_stat__ = (ptr)CLA161_at_index_(GLO68_class_stat_tbl_,i__);
      if ((c_stat__ == 0)) {
         res__ = (int)(- 1);
      }
      else {
         res__ = (int)IATT_(c_stat__,8);
      }
   }

   ret0__:
   return (res__);
}

OLD101_init_class_stat_tbl_(self__)
ptr self__;
{
   SATHER_STR_(20,31,ls2866_,"Previous compilation: -chk is ");
   SATHER_STR_(20,5,ls2867_," on\n");
   SATHER_STR_(20,6,ls2868_," off\n");
   SATHER_STR_(20,30,ls2869_,"Current compilation: -chk is ");
   ptr    fn__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   fn__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2282_));
   if (COM82_new_compilation_) {
      GLO68_class_stat_tbl_ = (ptr)CLA161_alloc_create_(0);
   }
   else {
      GLO68_class_stat_tbl_ = (ptr)CLA161_create_(0,fn__);
   }
   if (CATT_(GLO68_class_stat_tbl_,9)) {
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((CATT_(GLO68_class_stat_tbl_,4) != COM82_rt_code_check_)) {
      if ((! COM82_new_compilation_)) {
         (void)OUT9_s_(0,(ptr)(&ls2866_));
         if (CATT_(GLO68_class_stat_tbl_,4)) {
            (void)OUT9_s_(0,(ptr)(&ls2867_));
         }
         else {
            (void)OUT9_s_(0,(ptr)(&ls2868_));
         }
      }
      else {
      }
      (void)OUT9_s_(0,(ptr)(&ls2869_));
      if (COM82_rt_code_check_) {
         (void)OUT9_s_(0,(ptr)(&ls2867_));
      }
      else {
         (void)OUT9_s_(0,(ptr)(&ls2868_));
      }
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((CATT_(GLO68_class_stat_tbl_,5) != (COM82_compiler_mode_ == 1))) {
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((CATT_(GLO68_class_stat_tbl_,6) != COM82_gen_all_)) {
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((CATT_(GLO68_class_stat_tbl_,7) != COM82_gen_base_)) {
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((CATT_(GLO68_class_stat_tbl_,8) != COM82_verbose_code_)) {
      COM82_new_compilation_ = (char)1;
   }
   else {
   }
   if ((PATT_(GLO68_class_stat_tbl_,40) != 0)) {
      if ((! STR20_is_equal_(PATT_(GLO68_class_stat_tbl_,40),COM82_target_dir_))) {
         COM82_new_compilation_ = (char)1;
      }
      else {
      }
   }
   else {
   }
   if (((PATT_(GLO68_class_stat_tbl_,36) != 0) & (COM82_sather_home_ != 0))) {
      if ((! COM82_new_compilation_)) {
         COM82_new_compilation_ = (char)(! STR20_is_equal_(PATT_(GLO68_class_stat_tbl_,36),COM82_sather_home_));
      }
      else {
      }
   }
   else {
      COM82_new_compilation_ = (char)1;
   }
   if ((GLO68_cc_flags_ != 0)) {
      if ((PATT_(GLO68_class_stat_tbl_,48) != 0)) {
         i__ = (int)0;
         sz__ = (int)IATT_(GLO68_cc_flags_,4);
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_1193_;
            }
            else {
            }
            if ((! STR218_get_(PATT_(GLO68_class_stat_tbl_,48),PATT_(GLO68_cc_flags_, 8 + ((i__) << 2))))) {
               COM82_new_compilation_ = (char)1;
               goto goto_tag_1193_;
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_1193_: ;
      }
      else {
         COM82_new_compilation_ = (char)1;
      }
   }
   else {
      if ((PATT_(GLO68_class_stat_tbl_,48) != 0)) {
         if ((STR218_size_(PATT_(GLO68_class_stat_tbl_,48)) != 0)) {
            COM82_new_compilation_ = (char)1;
         }
         else {
         }
      }
      else {
      }
   }

   ret0__:
   return;
}

OLD101_adjust_tables_(self__)
ptr self__;
{

   if ((IATT_(GLO68_class_stat_tbl_,56) != 0)) {
      CLA93_expand_(GLO68_class_inst_,(IATT_(GLO68_class_stat_tbl_,56) + 1));
   }
   else {
   }

   ret0__:
   return;
}

OLD101_store_class_stat_tbl_(self__)
ptr self__;
{
   SATHER_STR_(20,42,ls2870_,"NOTE: No class status information stored\n");
   ptr    fn__ = 0;

   fn__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2282_));
   if ((GLO68_class_stat_tbl_ == 0)) {
      (void)ERR7_s_(0,(ptr)(&ls2870_));
   }
   else {
   }
   CLA161_record_curr_compilation_(GLO68_class_stat_tbl_);
   CATT_(GLO68_class_stat_tbl_,4) = (char)COM82_rt_code_check_;
   CATT_(GLO68_class_stat_tbl_,5) = (char)(COM82_compiler_mode_ == 1);
   CATT_(GLO68_class_stat_tbl_,6) = (char)COM82_gen_all_;
   CATT_(GLO68_class_stat_tbl_,7) = (char)COM82_gen_base_;
   CATT_(GLO68_class_stat_tbl_,8) = (char)COM82_verbose_code_;
   PATT_(GLO68_class_stat_tbl_,40) = (ptr)COM82_target_dir_;
   CLA161_persist_(GLO68_class_stat_tbl_,fn__);

   ret0__:
   return;
}

OLD101_store_name_ind_map_(self__)
ptr self__;
{
   SATHER_STR_(20,15,ls549_,"OLDNEW_HANDLER");
   SATHER_STR_(20,78,ls2871_,"Compiler error (code 2) in incremental compilation information; Please report");
   ptr    fn__ = 0;
   ptr    f__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   fn__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&gs2284_));
   f__ = (ptr)new_(171,0);
   STR171_open_for_write_(f__,fn__);
   if ((STR171_error_(f__) != 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls549_),(ptr)(&ls2871_));
   }
   else {
      i__ = (int)1;
      sz__ = (int)IATT_(GLO68_str_table_,12);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1194_;
         }
         else {
         }
         (void)STR171_c_(STR171_s_(STR171_c_(STR171_i_(f__,i__),'$'),PATT_(PATT_(GLO68_str_table_,4), 8 + ((i__) << 2))),'\10');
         i__ = (int)(i__ + 1);
      }
   goto_tag_1194_: ;
      STR171_close_(f__);
   }

   ret0__:
   return;
}

ptr OLD101_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* class_161.c : Sather class: CLASS_STAT_TBL, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR7_s_();
extern ptr FIL11_s_();
extern ptr FIL11_c_();
extern ptr FIL11_i_();
extern ptr CLA148_full_name_();
extern ptr CLA166_create_();
extern ptr GEN189_create_();
extern ptr GEN189_first_();
extern ptr CLA191_create_();
extern CLA191_update_();
extern ptr GEN189_item_();
extern ptr GEN189_next_();
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_c_compiler_;
extern /*shared*/ ptr GLO68_cc_flags_;
extern /*shared*/ ptr GLO68_keys_set_;
extern /*shared*/ ptr GLO68_c_macros_;
extern /*shared*/ ptr GLO68_name_mappings_;
extern int C_F74_st_mtime_();
extern ptr TIM75_ctime_();
extern ptr STR78_create_();
extern int STR78_get_();
extern STR78_insert_();
extern STR78_delete_();
extern /*shared*/ ptr COM82_sather_home_;
extern ptr INT86_create_();
extern ptr LIS87_create_();
extern /*constant*/ int INS88_deep_save_version_;
extern ptr LIS87_push_();
extern ptr STR218_create_();
extern ptr FIL219_open_for_read_();
extern char FIL219_error_();
extern ptr PER220_deep_restore_vn_();
extern FIL219_close_();
extern ptr FIL219_open_for_write_();
extern PER220_deep_save_vn_();
extern ERR96_error_exit_();
extern ptr DEE222_deep_copy_();
extern ptr FIL91_create_from_fn_();
extern ptr CLA93_at_index_();
extern char GLO94_handle_class_p_();
extern ptr STR218_insert_();
#include "macros_.h"



/*shared*/ int CLA161_version_;
ptr CLA161_create_();
ptr CLA161_alloc_create_();
CLA161_display_();
CLA161_display_strings_();
CLA161_persist_();
int CLA161_get_ind_();
int CLA161_get_cfile_time_();
CLA161_mark_relevant_();
ptr CLA161_irrelevant_classes_();
ptr CLA161_irrelevant_cfiles_();
CLA161_clear_();
CLA161_record_mapping_();
ptr CLA161_record_new_cfiles_();
CLA161_record_class_stat_();
CLA161_record_curr_compilation_();
ptr CLA161_at_index_();
ptr CLA161_initialize_();
extern int attr_ent_CLA161[];

ptr CLA161_create_(self__,fname__)
ptr self__;
ptr fname__;
{
   ptr res__ = 0;
   ptr gl4467_;
   ptr    f__ = 0;

   f__ = (ptr)FIL219_open_for_read_(0,fname__);
   if (FIL219_error_(f__)) {
      res__ = (ptr)CLA161_alloc_create_(self__);
   }
   else {
      res__ = (ptr)PER220_deep_restore_vn_(0,f__,CLA161_version_);
      if ((res__ == 0)) {
         res__ = (ptr)CLA161_alloc_create_(self__);
         goto ret0__;
      }
      else {
      }
      gl4467_ = res__;
      if (((161) != 161)) {
         res__ = (ptr)CLA161_alloc_create_(self__);
         goto ret0__;
      }
      else {
      }
      CATT_(res__,9) = (char)0;
      FIL219_close_(f__);
   }

   ret0__:
   return (res__);
}

ptr CLA161_alloc_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(161,0);
   CATT_(res__,9) = (char)1;
   CATT_(res__,10) = (char)0;
   PATT_(res__,24) = (ptr)STR78_create_(0);
   PATT_(res__,28) = (ptr)new1_(217,40,0);
   PATT_(res__,32) = (ptr)STR78_create_(0);
   PATT_(res__,12) = (ptr)CLA166_create_(0,1);
   PATT_(res__,16) = (ptr)INT86_create_(0);
   PATT_(res__,20) = (ptr)STR78_create_(0);

   ret0__:
   return (res__);
}

CLA161_display_(self__,f__)
ptr self__;
ptr f__;
{
   SATHER_STR_(20,10,ls2798_,"Class = <");
   SATHER_STR_(20,30,ls2799_,">;  definition time-stamp  = ");
   SATHER_STR_(20,25,ls2801_,"\n\tcomputed time-stamp = ");
   SATHER_STR_(20,28,ls2802_,"> has no update statistics\n");
   SATHER_STR_(20,16,ls2803_,"User C file = \"");
   SATHER_STR_(20,11,ls2804_,"\"; time = ");
   ptr    t__ = 0;
   ptr    t_curs__ = 0;
   ptr    elt__ = 0;
   int    index__ = S_int_VOID_;
   ptr    c_stat__ = 0;
   ptr    elt_470_ = 0;
   int    file_time__ = S_int_VOID_;

   t__ = (ptr)PATT_(PATT_(self__,24),4);
   t_curs__ = (ptr)GEN189_create_(0,t__);
   (void)GEN189_first_(t_curs__);
   while (1) {
      if (CATT_(t_curs__,4)) {
         goto goto_tag_4468_;
      }
      else {
      }
      elt__ = (ptr)GEN189_item_(t_curs__);
      index__ = (int)IATT_(elt__,12);
      if (((index__ < IATT_(PATT_(self__,28),4)) & (index__ > 0))) {
         c_stat__ = (ptr)PATT_(PATT_(self__,28), 8 + ((index__) << 2));
         if ((c_stat__ != 0)) {
            (void)FIL11_s_(FIL11_i_(FIL11_c_(FIL11_s_(FIL11_s_(f__,(ptr)(&ls2798_)),PATT_(elt__,4)),','),index__),(ptr)(&ls2799_));
            if ((IATT_(c_stat__,12) != 0)) {
               (void)FIL11_s_(f__,TIM75_ctime_(IATT_(c_stat__,12)));
            }
            else {
               (void)FIL11_c_(f__,'0');
            }
            (void)FIL11_s_(f__,(ptr)(&ls2801_));
            if ((IATT_(c_stat__,16) != 0)) {
               (void)FIL11_s_(f__,TIM75_ctime_(IATT_(c_stat__,16)));
            }
            else {
               (void)FIL11_c_(f__,'0');
            }
            (void)FIL11_c_(f__,'\n');
         }
         else {
         }
      }
      else {
         (void)FIL11_s_(FIL11_i_(FIL11_c_(FIL11_s_(FIL11_s_(f__,(ptr)(&ls2798_)),PATT_(elt__,4)),','),index__),(ptr)(&ls2802_));
      }
      (void)GEN189_next_(t_curs__);
   }
goto_tag_4468_: ;
   t__ = (ptr)PATT_(PATT_(self__,32),4);
   t_curs__ = (ptr)GEN189_create_(0,t__);
   (void)GEN189_first_(t_curs__);
   while (1) {
      if (CATT_(t_curs__,4)) {
         goto goto_tag_4469_;
      }
      else {
      }
      elt_470_ = (ptr)GEN189_item_(t_curs__);
      (void)FIL11_s_(FIL11_s_(FIL11_s_(f__,(ptr)(&ls2803_)),PATT_(elt_470_,4)),(ptr)(&ls2804_));
      file_time__ = (int)IATT_(elt_470_,12);
      (void)FIL11_c_(FIL11_s_(f__,TIM75_ctime_(file_time__)),'\n');
      (void)GEN189_next_(t_curs__);
   }
goto_tag_4469_: ;

   ret0__:
   return;
}

CLA161_display_strings_(self__,f__)
ptr self__;
ptr f__;
{
   SATHER_STR_(20,14,ls2807_,"Class name = ");
   SATHER_STR_(20,11,ls2808_,"; Index = ");
   ptr    t__ = 0;
   ptr    t_curs__ = 0;
   ptr    elt__ = 0;

   t__ = (ptr)PATT_(PATT_(self__,24),4);
   t_curs__ = (ptr)GEN189_create_(0,t__);
   (void)GEN189_first_(t_curs__);
   while (1) {
      if (CATT_(t_curs__,4)) {
         goto goto_tag_4470_;
      }
      else {
      }
      elt__ = (ptr)GEN189_item_(t_curs__);
      (void)FIL11_c_(FIL11_i_(FIL11_s_(FIL11_s_(FIL11_s_(f__,(ptr)(&ls2807_)),PATT_(elt__,4)),(ptr)(&ls2808_)),IATT_(elt__,12)),'\n');
      (void)GEN189_next_(t_curs__);
   }
goto_tag_4470_: ;

   ret0__:
   return;
}

CLA161_persist_(self__,fname__)
ptr self__;
ptr fname__;
{
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,21,ls2810_,"\"; object not saved\n");
   ptr    f__ = 0;

   f__ = (ptr)FIL219_open_for_write_(0,fname__);
   if (FIL219_error_(f__)) {
      (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls1253_)),fname__),(ptr)(&ls2810_));
   }
   else {
      PER220_deep_save_vn_(0,self__,f__,CLA161_version_);
      FIL219_close_(f__);
   }

   ret0__:
   return;
}

int CLA161_get_ind_(self__,cn__)
ptr self__;
ptr cn__;
{
   int res__ = S_int_VOID_;

   res__ = (int)STR78_get_(PATT_(self__,24),cn__);

   ret0__:
   return (res__);
}

int CLA161_get_cfile_time_(self__,fn__)
ptr self__;
ptr fn__;
{
   int res__ = S_int_VOID_;

   res__ = (int)STR78_get_(PATT_(self__,32),fn__);

   ret0__:
   return (res__);
}

CLA161_mark_relevant_(self__,co__)
ptr self__;
ptr co__;
{
   SATHER_STR_(20,80,ls2816_,"Compiler error (code 1) in incremental compilation information; Please report.\n");
   int    i__ = S_int_VOID_;
   ptr    s__ = 0;
   int    tmp_i__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   i__ = (int)IATT_(co__,20);
   s__ = (ptr)CLA148_full_name_(co__);
   tmp_i__ = (int)STR78_get_(PATT_(self__,24),s__);
   if ((tmp_i__ <= 0)) {
   }
   else {
      if ((i__ != tmp_i__)) {
         ERR96_error_exit_(0,(ptr)(&ls2816_));
      }
      else {
      }
      if (((i__ > 0) & (i__ < IATT_(PATT_(self__,28),4)))) {
         c_stat__ = (ptr)PATT_(PATT_(self__,28), 8 + ((i__) << 2));
         if ((c_stat__ != 0)) {
            CATT_(c_stat__,4) = (char)1;
         }
         else {
         }
      }
      else {
         PATT_(self__,28) = (ptr)extend1_(PATT_(self__,28),(2 * IATT_(PATT_(self__,28),4)),0);
         CLA161_mark_relevant_(self__,co__);
      }
   }

   ret0__:
   return;
}

ptr CLA161_irrelevant_classes_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    t__ = 0;
   ptr    t_curs__ = 0;
   ptr    elt__ = 0;
   int    index__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   res__ = (ptr)LIS87_create_(0,10);
   t__ = (ptr)PATT_(PATT_(self__,24),4);
   t_curs__ = (ptr)GEN189_create_(0,t__);
   (void)GEN189_first_(t_curs__);
   while (1) {
      if (CATT_(t_curs__,4)) {
         goto goto_tag_4471_;
      }
      else {
      }
      elt__ = (ptr)GEN189_item_(t_curs__);
      index__ = (int)IATT_(elt__,12);
      if (((index__ < IATT_(PATT_(self__,28),4)) & (index__ > 0))) {
         c_stat__ = (ptr)PATT_(PATT_(self__,28), 8 + ((index__) << 2));
         if ((c_stat__ != 0)) {
            if ((! CATT_(c_stat__,4))) {
               res__ = (ptr)LIS87_push_(res__,PATT_(c_stat__,24));
            }
            else {
            }
         }
         else {
         }
      }
      else {
      }
      (void)GEN189_next_(t_curs__);
   }
goto_tag_4471_: ;

   ret0__:
   return (res__);
}

ptr CLA161_irrelevant_cfiles_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr    t__ = 0;
   ptr    t_curs__ = 0;
   ptr    elt__ = 0;

   res__ = (ptr)LIS87_create_(0,5);
   if ((PATT_(self__,52) != 0)) {
      t__ = (ptr)PATT_(PATT_(self__,52),4);
      t_curs__ = (ptr)GEN189_create_(0,t__);
      (void)GEN189_first_(t_curs__);
      while (1) {
         if (CATT_(t_curs__,4)) {
            goto goto_tag_4472_;
         }
         else {
         }
         elt__ = (ptr)GEN189_item_(t_curs__);
         res__ = (ptr)LIS87_push_(res__,PATT_(elt__,4));
         (void)GEN189_next_(t_curs__);
      }
   goto_tag_4472_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

CLA161_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   PATT_(self__,24) = (ptr)STR78_create_(0);
   PATT_(self__,32) = (ptr)STR78_create_(0);
   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,28),4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4473_;
      }
      else {
      }
      PATT_(PATT_(self__,28), 8 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4473_: ;

   ret0__:
   return;
}

CLA161_record_mapping_(self__,fn__,index__)
ptr self__;
ptr fn__;
int index__;
{

   STR78_insert_(PATT_(self__,24),copy_(fn__,1),index__);

   ret0__:
   return;
}

ptr CLA161_record_new_cfiles_(self__,cfiles__)
ptr self__;
ptr cfiles__;
{
   ptr res__ = 0;
   SATHER_STR_(20,37,ls1301_,"Warning: Cannot get status of file \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   int    i__ = S_int_VOID_;
   int    csz__ = S_int_VOID_;
   ptr    new_user_cfile_to_time__ = 0;
   ptr    ith_cfile__ = 0;
   int    old_time__ = S_int_VOID_;
   ptr    cfile_stat__ = 0;
   int    new_time__ = S_int_VOID_;

   i__ = S_int_VOID_;
   csz__ = S_int_VOID_;
   if ((cfiles__ != 0)) {
      csz__ = (int)IATT_(cfiles__,4);
   }
   else {
   }
   res__ = (ptr)new1_(221,csz__,1);
   new_user_cfile_to_time__ = (ptr)STR78_create_(0);
   PATT_(self__,52) = (ptr)DEE222_deep_copy_(0,PATT_(self__,32));
   while (1) {
      if ((i__ >= csz__)) {
         goto goto_tag_4474_;
      }
      else {
      }
      ith_cfile__ = (ptr)PATT_(cfiles__, 16 + ((i__) << 2));
      STR78_delete_(PATT_(self__,52),ith_cfile__);
      old_time__ = (int)STR78_get_(PATT_(self__,32),ith_cfile__);
      cfile_stat__ = (ptr)FIL91_create_from_fn_(0,ith_cfile__);
      if ((IATT_(cfile_stat__,4) == (- 1))) {
         (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls1301_)),ith_cfile__),(ptr)(&ls632_));
      }
      else {
         new_time__ = (int)C_F74_st_mtime_(PATT_(cfile_stat__,8));
         if ((old_time__ == new_time__)) {
            CATT_(res__, 8 + ((i__))) = (char)1;
         }
         else {
         }
         STR78_insert_(new_user_cfile_to_time__,copy_(ith_cfile__,1),new_time__);
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4474_: ;
   PATT_(self__,32) = (ptr)new_user_cfile_to_time__;
   CATT_(self__,10) = (char)1;

   ret0__:
   return (res__);
}

CLA161_record_class_stat_(self__,co__)
ptr self__;
ptr co__;
{
   int    index__ = S_int_VOID_;
   ptr    c_stat__ = 0;

   index__ = (int)IATT_(co__,20);
   if (((index__ > 0) & (index__ < IATT_(PATT_(self__,28),4)))) {
      c_stat__ = (ptr)PATT_(PATT_(self__,28), 8 + ((index__) << 2));
      if ((c_stat__ == 0)) {
         c_stat__ = (ptr)CLA191_create_(0,co__);
         PATT_(PATT_(self__,28), 8 + ((index__) << 2)) = (ptr)c_stat__;
         goto ret0__;
      }
      else {
      }
      CLA191_update_(c_stat__,co__);
   }
   else {
      PATT_(self__,28) = (ptr)extend1_(PATT_(self__,28),(2 * IATT_(PATT_(self__,28),4)),0);
      CLA161_record_class_stat_(self__,co__);
   }

   ret0__:
   return;
}

CLA161_record_curr_compilation_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   ptr    co__ = 0;
   int    i_471_ = S_int_VOID_;
   int    sz_472_ = S_int_VOID_;
   ptr    new_cc_flags__ = 0;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(GLO68_class_inst_,24);
   CLA161_clear_(self__);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4475_;
      }
      else {
      }
      co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,i__);
      if ((co__ != 0)) {
         CLA161_record_mapping_(self__,CLA148_full_name_(co__),IATT_(co__,20));
         if (GLO94_handle_class_p_(0,co__)) {
            CLA161_record_class_stat_(self__,co__);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4475_: ;
   IATT_(self__,56) = (int)sz__;
   PATT_(self__,52) = (ptr)0;
   PATT_(self__,36) = (ptr)copy_(COM82_sather_home_,1);
   PATT_(self__,44) = (ptr)copy_(GLO68_c_compiler_,1);
   if ((GLO68_cc_flags_ != 0)) {
      i_471_ = (int)0;
      sz_472_ = (int)IATT_(GLO68_cc_flags_,4);
      new_cc_flags__ = (ptr)STR218_create_(0);
      while (1) {
         if ((i_471_ >= sz_472_)) {
            goto goto_tag_4476_;
         }
         else {
         }
         new_cc_flags__ = (ptr)STR218_insert_(new_cc_flags__,PATT_(GLO68_cc_flags_, 8 + ((i_471_) << 2)));
         i_471_ = (int)(i_471_ + 1);
      }
   goto_tag_4476_: ;
      PATT_(self__,48) = (ptr)new_cc_flags__;
   }
   else {
      PATT_(self__,48) = (ptr)0;
   }
   PATT_(self__,12) = (ptr)GLO68_keys_set_;
   PATT_(self__,16) = (ptr)GLO68_c_macros_;
   PATT_(self__,20) = (ptr)GLO68_name_mappings_;

   ret0__:
   return;
}

ptr CLA161_at_index_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;

   if (((i__ > 0) & (i__ < IATT_(PATT_(self__,28),4)))) {
      res__ = (ptr)PATT_(PATT_(self__,28), 8 + ((i__) << 2));
   }
   else {
      PATT_(self__,28) = (ptr)extend1_(PATT_(self__,28),(2 * IATT_(PATT_(self__,28),4)),0);
      res__ = (ptr)CLA161_at_index_(self__,i__);
   }

   ret0__:
   return (res__);
}

ptr CLA161_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


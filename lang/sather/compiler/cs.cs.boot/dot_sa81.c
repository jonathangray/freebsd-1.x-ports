/* dot_sa81.c : Sather class: DOT_SATHER_HANDLER, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);

extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern char STR20_is_equal_(ptr self__, ptr st__);
extern ptr UPA167_canonicalize_(ptr self__, ptr st__);
extern ptr SAT176_create_(ptr self__, ptr nm__);
extern ptr LIS177_create_(ptr self__, int init_size__);
extern int SAT176_error_(ptr self__);
extern ptr LIS177_push_(ptr self__, ptr e__);
extern ptr C_N180_create_(ptr self__);
extern int SAT176_get_token_(ptr self__);
extern ptr SAT176_curr_str_(ptr self__);
extern char LIS177_is_empty_(ptr self__);
extern void COM178_terminate_(ptr self__);
extern void SAT176_close_(ptr self__);
extern ptr LIS177_pop_(ptr self__);
extern ptr LIS177_top_(ptr self__);
extern void COM178_insert_(ptr self__, ptr nm__);
extern void COM178_start_(ptr self__, ptr nm__);
extern int COM178_info_size_(ptr self__);
extern ptr COM178_ith_str_(ptr self__, int i__);
extern /*shared*/ char COM82_is_opt_sather_home_;
extern ptr GEN83_create_(ptr self__);
extern /*shared*/ ptr COM82_sather_home_;
extern ptr C_M85_create_(ptr self__);
extern void COM82_set_sather_home_(ptr self__, ptr sat_home__);
extern /*constant*/ ptr INS88_env_v_home_;
extern /*constant*/ ptr INS88_env_v_sat_home_;
extern ptr UNI90_getenv_(ptr self__, ptr name__);
extern int UNI90_putenv_(ptr self__, ptr name__, ptr val__);
extern /*constant*/ ptr INS88_env_v_environment_;
extern /*shared*/ ptr INS88_default_environment_;
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern ptr OBJ100_create_(ptr self__);
extern struct { int tp_; int sz_; char st_; } gs1060_;
extern struct { int tp_; int sz_; char st_; } gs1062_;
extern struct { int tp_; int sz_; char st_; } gs1606_;
extern struct { int tp_; int sz_; char st_; } gs1064_;
extern struct { int tp_; int sz_; char st_; } gs1066_;
extern struct { int tp_; int sz_; char st_; } gs1068_;
extern struct { int tp_; int sz_; char st_; } gs1070_;
extern struct { int tp_; int sz_; char st_; } gs1072_;
extern struct { int tp_; int sz_; char st_; } gs2289_;
extern struct { int tp_; int sz_; char st_; } gs1074_;
extern struct { int tp_; int sz_; char st_; } gs2290_;
extern struct { int tp_; int sz_; char st_; } gs2291_;
#include "macros_.h"



/*constant*/ ptr DOT81_object_files_kw_ = (ptr)(&gs1062_);
/*constant*/ ptr DOT81_cc_flags_kw_ = (ptr)(&gs1064_);
/*constant*/ ptr DOT81_c_macro_kw_ = (ptr)(&gs1066_);
/*constant*/ ptr DOT81_c_name_kw_ = (ptr)(&gs1068_);
/*constant*/ ptr DOT81_include_kw_ = (ptr)(&gs1070_);
/*constant*/ ptr DOT81_sather_home_kw_ = (ptr)(&gs1072_);
/*constant*/ ptr DOT81_c_compiler_kw_ = (ptr)(&gs1074_);
/*constant*/ int DOT81_source_files_ind_ = 0;
/*constant*/ int DOT81_object_files_ind_ = 1;
/*constant*/ int DOT81_cc_flags_ind_ = 2;
/*constant*/ int DOT81_c_macro_ind_ = 3;
/*constant*/ int DOT81_c_name_ind_ = 4;
/*constant*/ int DOT81_include_ind_ = 5;
/*constant*/ int DOT81_sather_home_ind_ = 6;
/*constant*/ int DOT81_c_compiler_ind_ = 7;
/*constant*/ int DOT81_compile_keys_fst_ind_ = 0;
/*constant*/ int DOT81_compile_keys_lst_ind_ = 7;
/*constant*/ int DOT81_num_compile_keys_ = 8;
/*constant*/ int DOT81_non_compile_key_ind_ = 8;
/*constant*/ int DOT81_eof_tok_ = -1;
/*constant*/ int DOT81_ident_tok_ = -2;
/*constant*/ int DOT81_qexp_tok_ = -3;
char DOT81_compile_key_p_(ptr self__, int i__);
ptr DOT81_initialize_(ptr self__, ptr initarg__);
/*constant*/ ptr DOT81_def_cmdfile_name_ = (ptr)(&gs1606_);
/*constant*/ int DOT81_err_unknown_file_ = 1;
/*constant*/ ptr DOT81_source_files_kw_ = (ptr)(&gs1060_);
void DOT81_error_msg_(ptr self__, ptr s__);
void DOT81_error_exit_(ptr self__, ptr s__);
ptr DOT81_create_(ptr self__);
void DOT81_init_(ptr self__, ptr cmdfile_name__);
char DOT81_found_p_(ptr self__, ptr fname__);
char DOT81_read_cmdfile_(ptr self__);
ptr DOT81_ith_source_file_(ptr self__, int i__);
extern int attr_ent_DOT81[];

char DOT81_compile_key_p_(ptr self__, int i__)
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ <= 7) & (i__ >= 0));

   ret0__:
   return (res__);
}

ptr DOT81_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void DOT81_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void DOT81_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

ptr DOT81_create_(ptr self__)
{
   ptr res__ = 0;
   ptr gl990_;
   int    i__ = S_int_VOID_;

   res__ = (ptr)new_(81,0);
   PATT_(res__,8) = (ptr)LIS177_create_(PATT_(res__,8),2);
   gl990_ = PATT_(res__,16);
   PATT_(res__,16) = (ptr)new1_(179,8,0);
   i__ = (int)0;
   while (1) {
      if ((i__ > 7)) {
         goto goto_tag_991_;
      }
      else {
      }
      switch (i__) {
         case (1) :
            PATT_(PATT_(res__,16), 8 + ((i__) << 2)) = (ptr)OBJ100_create_(0);
            break;
         case (4) :
            PATT_(PATT_(res__,16), 8 + ((i__) << 2)) = (ptr)C_N180_create_(0);
            break;
         case (3) :
            PATT_(PATT_(res__,16), 8 + ((i__) << 2)) = (ptr)C_M85_create_(0);
            break;
         default:
            PATT_(PATT_(res__,16), 8 + ((i__) << 2)) = (ptr)GEN83_create_(0);
            ;
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_991_: ;

   ret0__:
   return (res__);
}

void DOT81_init_(ptr self__, ptr cmdfile_name__)
{
   SATHER_STR_(20,38,ls1615_,"Unknown home directory; HOME unknown\n");
   SATHER_STR_(20,32,ls1617_,"Error in opening command file \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   ptr    tmpcmdfile__ = 0;
   ptr    home_dir__ = 0;
   ptr    home__ = 0;

   if ((cmdfile_name__ == 0)) {
      cmdfile_name__ = (ptr)(ptr)(&gs1606_);
   }
   else {
   }
   tmpcmdfile__ = (ptr)SAT176_create_(tmpcmdfile__,cmdfile_name__);
   if ((SAT176_error_(tmpcmdfile__) != 0)) {
      home_dir__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2289_));
      if ((home_dir__ == 0)) {
         DOT81_error_exit_(self__,STR20_s_(STR20_create_(0),(ptr)(&ls1615_)));
      }
      else {
         home_dir__ = (ptr)UPA167_canonicalize_(0,home_dir__);
      }
      home__ = (ptr)STR20_c_(STR20_s_(STR20_create_(0),home_dir__),'/');
      cmdfile_name__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),home__),cmdfile_name__);
      tmpcmdfile__ = (ptr)SAT176_create_(tmpcmdfile__,cmdfile_name__);
      if ((SAT176_error_(tmpcmdfile__) != 0)) {
         CATT_(self__,4) = (char)1;
         IATT_(self__,20) = (int)1;
         DOT81_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1617_)),cmdfile_name__),(ptr)(&ls632_)));
      }
      else {
         PATT_(self__,12) = (ptr)tmpcmdfile__;
         PATT_(self__,8) = (ptr)LIS177_push_(PATT_(self__,8),tmpcmdfile__);
      }
   }
   else {
      PATT_(self__,12) = (ptr)tmpcmdfile__;
      PATT_(self__,8) = (ptr)LIS177_push_(PATT_(self__,8),tmpcmdfile__);
   }

   ret0__:
   return;
}

char DOT81_found_p_(ptr self__, ptr fname__)
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(PATT_(self__,8),4))) {
         goto goto_tag_992_;
      }
      else {
      }
      if (STR20_is_equal_(fname__,PATT_(PATT_(PATT_(self__,8), 16 + ((i__) << 2)),12))) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_992_: ;

   ret0__:
   return (res__);
}

char DOT81_read_cmdfile_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,19,ls816_,"DOT_SATHER_HANDLER");
   SATHER_STR_(20,22,ls1622_,"Unrecognized string \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,25,ls1627_,"Cycle in include files \"");
   SATHER_STR_(20,26,ls1628_,"Can't open include file \"");
   SATHER_STR_(20,24,ls1629_,"Unexpected quoted expr\"");
   ptr gl993_;
   static int gl994_;
   static union dtype_ gl995_;
   ptr gl996_;
   static int gl997_;
   static union dtype_ gl998_;
   ptr gl57_;
   ptr gl999_;
   static int gl1000_;
   static union dtype_ gl1001_;
   ptr gl58_;
   ptr gl1002_;
   static int gl1003_;
   static union dtype_ gl1004_;
   ptr    oldcmdfile__ = 0;
   int    tok__ = S_int_VOID_;
   int    curr_key__ = S_int_VOID_;
   ptr    varval__ = 0;
   ptr    includefilename__ = 0;
   char    ok__ = S_char_VOID_;

   oldcmdfile__ = S_ptr_VOID_;
   tok__ = (int)SAT176_get_token_(PATT_(self__,12));
   curr_key__ = S_int_VOID_;
   while (1) {
      if ((DOT81_compile_key_p_(self__,tok__) | (tok__ == -1))) {
         goto goto_tag_1005_;
      }
      else {
      }
      ERR96_format_error_msg_file_(0,PATT_(PATT_(self__,12),12),IATT_(PATT_(self__,12),16),(ptr)(&ls816_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1622_)),SAT176_curr_str_(PATT_(self__,12))),(ptr)(&ls785_)));
      tok__ = (int)SAT176_get_token_(PATT_(self__,12));
   }
goto_tag_1005_: ;
   curr_key__ = (int)tok__;
   tok__ = (int)SAT176_get_token_(PATT_(self__,12));
   varval__ = S_ptr_VOID_;
   if ((! COM82_is_opt_sather_home_)) {
      varval__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2290_));
      if ((varval__ == 0)) {
         (void)UNI90_putenv_(0,(ptr)(&gs2290_),COM82_sather_home_);
         varval__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2290_));
      }
      else {
      }
   }
   else {
   }
   varval__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2291_));
   if ((varval__ == 0)) {
      (void)UNI90_putenv_(0,(ptr)(&gs2291_),INS88_default_environment_);
      varval__ = (ptr)UNI90_getenv_(0,(ptr)(&gs2291_));
   }
   else {
   }
   while (1) {
      if (LIS177_is_empty_(PATT_(self__,8))) {
         goto goto_tag_1006_;
      }
      else {
      }
      switch (tok__) {
         case (-1) :
            if (((curr_key__ == 3) | (curr_key__ == 4))) {
               gl993_ = PATT_(PATT_(self__,16), 8 + ((curr_key__) << 2));
               cache_dispatch_(gl993_,989,gl994_,INTVAL_(gl995_));
               VFN_(gl995_)(gl993_);
            }
            else {
            }
            SAT176_close_(PATT_(self__,12));
            oldcmdfile__ = (ptr)LIS177_pop_(PATT_(self__,8));
            PATT_(self__,12) = (ptr)LIS177_top_(PATT_(self__,8));
            res__ = (char)1;
            goto ret0__;
            break;
         case (-2) :
            if ((curr_key__ == 6)) {
               COM82_set_sather_home_(0,SAT176_curr_str_(PATT_(self__,12)));
               (void)UNI90_putenv_(0,(ptr)(&gs2290_),COM82_sather_home_);
            }
            else {
               if ((curr_key__ == 5)) {
                  includefilename__ = (ptr)UPA167_canonicalize_(0,SAT176_curr_str_(PATT_(self__,12)));
                  if (DOT81_found_p_(self__,includefilename__)) {
                     ERR96_format_error_msg_file_(0,PATT_(PATT_(self__,12),12),IATT_(PATT_(self__,12),16),(ptr)(&ls816_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1627_)),includefilename__),(ptr)(&ls785_)));
                  }
                  else {
                  }
                  PATT_(self__,12) = (ptr)SAT176_create_(PATT_(self__,12),includefilename__);
                  if ((SAT176_error_(PATT_(self__,12)) != 0)) {
                     ERR96_format_error_msg_file_(0,PATT_(PATT_(self__,12),12),IATT_(PATT_(self__,12),16),(ptr)(&ls816_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1628_)),SAT176_curr_str_(PATT_(self__,12))),(ptr)(&ls785_)));
                  }
                  else {
                     PATT_(self__,8) = (ptr)LIS177_push_(PATT_(self__,8),PATT_(self__,12));
                     ok__ = (char)DOT81_read_cmdfile_(self__);
                  }
               }
               else {
                  gl57_ = UPA167_canonicalize_(0,SAT176_curr_str_(PATT_(self__,12)));
                  gl996_ = PATT_(PATT_(self__,16), 8 + ((curr_key__) << 2));
                  cache_dispatch_(gl996_,249,gl997_,INTVAL_(gl998_));
                  VFN_(gl998_)(gl996_,gl57_);
               }
            }
            break;
         case (-3) :
            if ((curr_key__ != 3)) {
               ERR96_format_error_msg_file_(0,PATT_(PATT_(self__,12),12),IATT_(PATT_(self__,12),16),(ptr)(&ls816_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1629_)),SAT176_curr_str_(PATT_(self__,12))),(ptr)(&ls785_)));
            }
            else {
               gl58_ = SAT176_curr_str_(PATT_(self__,12));
               gl999_ = PATT_(PATT_(self__,16), 8 + ((curr_key__) << 2));
               cache_dispatch_(gl999_,1032,gl1000_,INTVAL_(gl1001_));
               VFN_(gl1001_)(gl999_,gl58_);
            }
            break;
         default:
            if (((curr_key__ == 3) | (curr_key__ == 4))) {
               gl1002_ = PATT_(PATT_(self__,16), 8 + ((curr_key__) << 2));
               cache_dispatch_(gl1002_,989,gl1003_,INTVAL_(gl1004_));
               VFN_(gl1004_)(gl1002_);
            }
            else {
            }
            curr_key__ = (int)tok__;
            ;
      }
      tok__ = (int)SAT176_get_token_(PATT_(self__,12));
   }
goto_tag_1006_: ;

   ret0__:
   return (res__);
}

ptr DOT81_ith_source_file_(ptr self__, int i__)
{
   ptr res__ = 0;
   ptr gl1007_;
   static int gl1008_;
   static union dtype_ gl1009_;
   int gl59_;
   ptr gl1010_;
   static int gl1011_;
   static union dtype_ gl1012_;
   ptr    command__ = 0;

   command__ = (ptr)PATT_(PATT_(self__,16), 8 + ((0) << 2));
   gl1007_ = command__;
   cache_dispatch_(gl1007_,1035,gl1008_,INTVAL_(gl1009_));
   gl59_ = IFN_(gl1009_)(gl1007_);
   if (((i__ >= 0) & (i__ < gl59_))) {
      gl1010_ = command__;
      cache_dispatch_(gl1010_,1036,gl1011_,INTVAL_(gl1012_));
      res__ = (ptr)PFN_(gl1012_)(gl1010_,i__);
   }
   else {
   }

   ret0__:
   return (res__);
}


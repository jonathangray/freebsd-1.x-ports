/* err_in96.c : Sather class: ERR_INFO, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern error_exit();

extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr ERR7_s_();
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ char COM82_warnings_only_;
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_nl_();
extern ptr LIS87_create_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern ptr LIS87_push_();
extern ptr TYP149_inst_cls_();
extern ptr CLA148_full_name_();
extern char TYP149_is_dispatched_();
extern ptr LIS98_create_();
extern ptr LIS98_push_();
#include "macros_.h"



ERR96_warning_msg_();
ERR96_error_msg_();
ERR96_compiler_error_msg_();
ERR96_error_exit_();
ERR96_format_warning_msg_();
ERR96_format_error_msg_();
ERR96_format_warning_msg_file_();
ERR96_format_error_msg_file_();
ERR96_format_error_exit_();
ptr ERR96_filename_();
/*shared*/ ptr ERR96_ool_file_names_;
/*shared*/ ptr ERR96_ool_lines_;
int ERR96_out_of_line_err_info_();
int ERR96_def_lineno_();
ptr ERR96_def_filename_();
ptr ERR96_def_classname_();
int ERR96_feat_lineno_();
ERR96_type_mismatch_err_();
ptr ERR96_initialize_();
extern int attr_ent_ERR96[];

ERR96_warning_msg_(self__,s__)
ptr self__;
ptr s__;
{
   SATHER_STR_(20,3,ls1490_,"* ");

   if ((! COM82_warnings_only_)) {
      (void)ERR7_s_(ERR7_s_(0,(ptr)(&ls1490_)),s__);
   }
   else {
   }

   ret0__:
   return;
}

ERR96_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

ERR96_compiler_error_msg_(self__,classname__,msg__)
ptr self__;
ptr classname__;
ptr msg__;
{
   SATHER_STR_(20,2,ls1493_,"(");
   SATHER_STR_(20,19,ls1494_,"): COMPILER ERROR ");
   ptr    mess__ = 0;

   mess__ = (ptr)STR20_nl_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1493_)),classname__),(ptr)(&ls1494_)),msg__));
   ERR96_error_msg_(self__,mess__);

   ret0__:
   return;
}

ERR96_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

ERR96_format_warning_msg_(self__,ln__,s__)
ptr self__;
int ln__;
ptr s__;
{

   ERR96_format_warning_msg_file_(self__,ERR96_def_filename_(self__,ln__),ln__,ERR96_def_classname_(self__),s__);

   ret0__:
   return;
}

ERR96_format_error_msg_(self__,ln__,s__)
ptr self__;
int ln__;
ptr s__;
{

   ERR96_format_error_msg_file_(self__,ERR96_def_filename_(self__,ln__),ln__,ERR96_def_classname_(self__),s__);

   ret0__:
   return;
}

ERR96_format_warning_msg_file_(self__,file__,ln__,cls__,s__)
ptr self__;
ptr file__;
int ln__;
ptr cls__;
ptr s__;
{
   SATHER_STR_(20,9,ls1497_,"\", line ");
   SATHER_STR_(20,12,ls1499_,": in class ");
   SATHER_STR_(20,17,ls1500_," (Warning)\n  -- ");
   SATHER_STR_(20,3,ls652_,"\n\n");

   ERR96_warning_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'"'),file__),(ptr)(&ls1497_)),ERR96_def_lineno_(self__,ln__)),(ptr)(&ls1499_)),cls__),(ptr)(&ls1500_)),s__),(ptr)(&ls652_)));

   ret0__:
   return;
}

ERR96_format_error_msg_file_(self__,file__,ln__,cls__,s__)
ptr self__;
ptr file__;
int ln__;
ptr cls__;
ptr s__;
{
   SATHER_STR_(20,9,ls1497_,"\", line ");
   SATHER_STR_(20,12,ls1499_,": in class ");
   SATHER_STR_(20,7,ls1501_,"\n  -- ");
   SATHER_STR_(20,3,ls652_,"\n\n");

   ERR96_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'"'),file__),(ptr)(&ls1497_)),ERR96_def_lineno_(self__,ln__)),(ptr)(&ls1499_)),cls__),(ptr)(&ls1501_)),s__),(ptr)(&ls652_)));

   ret0__:
   return;
}

ERR96_format_error_exit_(self__,ln__,s__)
ptr self__;
int ln__;
ptr s__;
{
   SATHER_STR_(20,9,ls1497_,"\", line ");
   SATHER_STR_(20,12,ls1499_,": in class ");
   SATHER_STR_(20,7,ls1501_,"\n  -- ");
   SATHER_STR_(20,3,ls652_,"\n\n");

   ERR96_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'"'),ERR96_def_filename_(self__,ln__)),(ptr)(&ls1497_)),ERR96_def_lineno_(self__,ln__)),(ptr)(&ls1499_)),ERR96_def_classname_(self__)),(ptr)(&ls1501_)),s__),(ptr)(&ls652_)));

   ret0__:
   return;
}

ptr ERR96_filename_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)GLO68_curr_filename_;

   ret0__:
   return (res__);
}

int ERR96_out_of_line_err_info_(self__,f__,ln__)
ptr self__;
ptr f__;
int ln__;
{
   int res__ = S_int_VOID_;
   int    last__ = S_int_VOID_;

   if ((ln__ < 0)) {
      res__ = (int)ln__;
      goto ret0__;
   }
   else {
   }
   last__ = (int)(IATT_(ERR96_ool_file_names_,4) - 1);
   if ((last__ != (- 1))) {
      if (((PATT_(ERR96_ool_file_names_, 16 + ((last__) << 2)) == f__) & (IATT_(ERR96_ool_lines_, 16 + ((last__) << 2)) == ln__))) {
         res__ = (int)(- (last__ + 1));
         goto ret0__;
      }
      else {
      }
   }
   else {
   }
   ERR96_ool_file_names_ = (ptr)LIS87_push_(ERR96_ool_file_names_,f__);
   ERR96_ool_lines_ = (ptr)LIS98_push_(ERR96_ool_lines_,ln__);
   res__ = (int)(- IATT_(ERR96_ool_file_names_,4));

   ret0__:
   return (res__);
}

int ERR96_def_lineno_(self__,ln__)
ptr self__;
int ln__;
{
   int res__ = S_int_VOID_;

   if ((ln__ < 0)) {
      res__ = (int)IATT_(ERR96_ool_lines_, 16 + ((((- ln__) - 1)) << 2));
   }
   else {
      res__ = (int)ln__;
   }

   ret0__:
   return (res__);
}

ptr ERR96_def_filename_(self__,ln__)
ptr self__;
int ln__;
{
   ptr res__ = 0;
   SATHER_STR_(20,27,ls718_,"<apparently not from file>");
   ptr gl1159_;
   static int gl1160_;
   static union dtype_ gl1161_;
   ptr    where__ = 0;

   if ((ln__ < 0)) {
      res__ = (ptr)PATT_(ERR96_ool_file_names_, 16 + ((((- ln__) - 1)) << 2));
   }
   else {
      gl1159_ = GLO68_curr_feature_;
      cache_dispatch_(gl1159_,329,gl1160_,INTVAL_(gl1161_));
      where__ = (ptr)PATT_(gl1159_,INTVAL_(gl1161_));
      if ((where__ != 0)) {
         res__ = (ptr)PATT_(where__,16);
      }
      else {
         res__ = (ptr)(ptr)(&ls718_);
      }
   }

   ret0__:
   return (res__);
}

ptr ERR96_def_classname_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,5,ls786_," in ");
   SATHER_STR_(20,23,ls1506_,"<Apparently generated>");
   ptr gl1162_;
   static int gl1163_;
   static union dtype_ gl1164_;
   ptr    where__ = 0;
   ptr    where_inst__ = 0;

   gl1162_ = GLO68_curr_feature_;
   cache_dispatch_(gl1162_,329,gl1163_,INTVAL_(gl1164_));
   where__ = (ptr)PATT_(gl1162_,INTVAL_(gl1164_));
   if ((where__ != 0)) {
      res__ = (ptr)STR69_at_index_(GLO68_str_table_,IATT_(where__,12));
      res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls785_)),res__),(ptr)(&ls785_));
      where_inst__ = (ptr)PATT_(GLO68_curr_class_inst_,28);
      if (((GLO68_class_inst_ != 0) & (where_inst__ != where__))) {
         res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),res__),(ptr)(&ls786_)),(ptr)(&ls785_)),STR69_at_index_(GLO68_str_table_,IATT_(where_inst__,12))),(ptr)(&ls785_));
      }
      else {
      }
   }
   else {
      res__ = (ptr)(ptr)(&ls1506_);
   }

   ret0__:
   return (res__);
}

int ERR96_feat_lineno_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   ptr gl1165_;
   static int gl1166_;
   static union dtype_ gl1167_;

   if ((GLO68_curr_feature_ != 0)) {
      gl1165_ = GLO68_curr_feature_;
      cache_dispatch_(gl1165_,298,gl1166_,INTVAL_(gl1167_));
      res__ = (int)IATT_(gl1165_,INTVAL_(gl1167_));
   }
   else {
      res__ = (int)(- 1);
   }

   ret0__:
   return (res__);
}

ERR96_type_mismatch_err_(self__,where__,comnt__,t1__,t2__,ln__)
ptr self__;
ptr where__;
ptr comnt__;
ptr t1__;
ptr t2__;
int ln__;
{
   SATHER_STR_(20,2,ls1512_,"$");
   SATHER_STR_(20,5,ls37_,"void");
   SATHER_STR_(20,18,ls1514_,"Type mismatch in ");
   SATHER_STR_(20,5,ls1515_," -- ");
   SATHER_STR_(20,22,ls1516_," does not conform to ");
   ptr gl1168_;
   static int gl1169_;
   static union dtype_ gl1170_;
   ptr gl1171_;
   static int gl1172_;
   static union dtype_ gl1173_;
   ptr gl1174_;
   static int gl1175_;
   static union dtype_ gl1176_;
   ptr gl1177_;
   static int gl1178_;
   static union dtype_ gl1179_;
   ptr    t1_nm__ = 0;
   ptr    t2_nm__ = 0;

   t1_nm__ = S_ptr_VOID_;
   if ((t1__ != 0)) {
      gl1168_ = t1__;
      cache_dispatch_(gl1168_,418,gl1169_,INTVAL_(gl1170_));
      t1_nm__ = (ptr)CLA148_full_name_(PFN_(gl1170_)(gl1168_));
      gl1171_ = t1__;
      cache_dispatch_(gl1171_,1511,gl1172_,INTVAL_(gl1173_));
      if (CFN_(gl1173_)(gl1171_)) {
         t1_nm__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1512_)),t1_nm__);
      }
      else {
      }
   }
   else {
      t1_nm__ = (ptr)(ptr)(&ls37_);
   }
   t2_nm__ = S_ptr_VOID_;
   if ((t2__ != 0)) {
      gl1174_ = t2__;
      cache_dispatch_(gl1174_,418,gl1175_,INTVAL_(gl1176_));
      t2_nm__ = (ptr)CLA148_full_name_(PFN_(gl1176_)(gl1174_));
      gl1177_ = t2__;
      cache_dispatch_(gl1177_,1511,gl1178_,INTVAL_(gl1179_));
      if (CFN_(gl1179_)(gl1177_)) {
         t2_nm__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1512_)),t2_nm__);
      }
      else {
      }
   }
   else {
      t2_nm__ = (ptr)(ptr)(&ls37_);
   }
   ERR96_format_error_msg_(self__,ln__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1514_)),where__),(ptr)(&ls1515_)),t1_nm__),(ptr)(&ls1516_)),t2_nm__),comnt__));

   ret0__:
   return;
}

ptr ERR96_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* sys___13.c : Sather class: SYS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ int num_classes_;
extern /*shared*/ int max_name_index_;
extern ptr makestr_(ptr st__);
extern /*shared*/ ptr prog_dir_;
extern /*shared*/ ptr prog_name_;
extern int safe_ob_base_size_(int ici__, int ln__);
extern int safe_cl_ctype_(int ici__, int ln__);
extern int safe_cl_feat_num_(int ici__, int ln__);
extern int safe_ob_arr_dim_(int ici__, int ln__);
extern int safe_ob_arr_ctype_(int ici__, int ln__);
extern int safe_ob_attr_num_(int ici__, int ln__);
extern int safe_ob_attr_ctype_(int ici__, int j__, int ln__);
extern int safe_cl_arr_satype_(int ici__, int ln__);
extern char safe_is_a_des_of_(int i__, int j__, int ln__);
extern int safe_cl_feat_name_(int ici__, int f__, int ln__);
extern int safe_cl_feat_cat_(int ici__, int f__, int ln__);
extern int safe_cl_feat_satype_(int ici__, int f__, int ln__);
extern int safe_get_dispatch_(int ici__, int nm__, int ln__);

extern ptr ERR7_s_(ptr self__, ptr st__);
extern ptr FIL11_create_(ptr self__);
extern void FIL11_open_for_read_(ptr self__, ptr nm__);
extern int FIL11_error_(ptr self__);
extern ptr STR78_create_(ptr self__);
extern char FIL11_check_eof_(ptr self__);
extern int FIL11_get_i_(ptr self__);
extern ptr FIL11_get_s_up_to_(ptr self__, char c0__);
extern void STR78_insert_(ptr self__, ptr s__, int v__);
extern void FIL11_close_(ptr self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int FIL11_get_ci_(ptr self__);
extern int STR78_get_(ptr self__, ptr s__);
#include "macros_.h"



/*shared*/ ptr SYS13_class_table_;
/*shared*/ ptr SYS13_class_rtable_;
/*shared*/ ptr SYS13_feature_table_;
/*shared*/ ptr SYS13_feature_rtable_;
/*shared*/ char SYS13_initialized_;
/*constant*/ int SYS13_Attribute_ = 1;
/*constant*/ int SYS13_Routine_ = 2;
/*constant*/ int SYS13_Shared_ = 3;
/*constant*/ int SYS13_Constant_ = 4;
char SYS13_init_(ptr self__);
int SYS13_number_of_classes_(ptr self__);
int SYS13_class_index_(ptr self__, ptr nm__);
ptr SYS13_class_name_(ptr self__, int i__);
int SYS13_class_base_size_(ptr self__, int i__);
int SYS13_class_c_type_(ptr self__, int i__);
int SYS13_class_number_of_features_(ptr self__, int i__);
int SYS13_class_array_dim_(ptr self__, int i__);
int SYS13_class_array_c_type_(ptr self__, int i__);
int SYS13_class_number_of_attributes_(ptr self__, int i__);
int SYS13_class_attribute_c_type_(ptr self__, int ci__, int a__);
int SYS13_class_array_element_type_(ptr self__, int i__);
char SYS13_is_a_descendent_of_(ptr self__, int i__, int j__);
ptr SYS13_feature_name_(ptr self__, int ci__, int f__);
int SYS13_feature_category_(ptr self__, int ci__, int f__);
int SYS13_feature_index_(ptr self__, int ci__, ptr f__);
int SYS13_feature_type_(ptr self__, int ci__, int f__);
int SYS13_feature_c_type_(ptr self__, int ci__, int f__);
int SYS13_feature_location_(ptr self__, int ci__, int f__);
extern int attr_ent_SYS13[];

char SYS13_init_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,5,ls2477_,".sa_");
   SATHER_STR_(20,27,ls3028_,"Missing information file \"");
   SATHER_STR_(20,26,ls3029_,"\"; Check compiler option\n");
   SATHER_STR_(20,10,ls2284_,"name_ind_");
   ptr    info_fname__ = 0;
   ptr    infile__ = 0;
   ptr    st__ = 0;
   int    ind__ = S_int_VOID_;
   ptr    info_fname_1_ = 0;
   ptr    infile_2_ = 0;
   ptr    st_3_ = 0;
   int    ind_4_ = S_int_VOID_;
   int    c__ = S_int_VOID_;

   res__ = (char)1;
   if (SYS13_initialized_) {
      goto ret0__;
   }
   else {
   }
   info_fname__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),makestr_(prog_dir_)),makestr_(prog_name_)),(ptr)(&ls2477_));
   infile__ = (ptr)FIL11_create_(0);
   FIL11_open_for_read_(infile__,info_fname__);
   if ((FIL11_error_(infile__) != 0)) {
      (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls3028_)),info_fname__),(ptr)(&ls3029_));
      res__ = (char)0;
      goto ret0__;
   }
   else {
      st__ = S_ptr_VOID_;
      ind__ = S_int_VOID_;
      while (1) {
         if (FIL11_check_eof_(infile__)) {
            goto goto_tag_514_;
         }
         else {
         }
         ind__ = (int)FIL11_get_i_(infile__);
         if (FIL11_check_eof_(infile__)) {
            goto goto_tag_514_;
         }
         else {
         }
         st__ = (ptr)FIL11_get_s_up_to_(infile__,'\n');
         PATT_(SYS13_class_table_, 8 + ((ind__) << 2)) = (ptr)st__;
         STR78_insert_(SYS13_class_rtable_,st__,ind__);
      }
   goto_tag_514_: ;
      FIL11_close_(infile__);
   }
   info_fname_1_ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),makestr_(prog_dir_)),(ptr)(&ls2284_));
   infile_2_ = (ptr)FIL11_create_(0);
   FIL11_open_for_read_(infile_2_,info_fname_1_);
   if ((FIL11_error_(infile_2_) != 0)) {
      (void)ERR7_s_(ERR7_s_(ERR7_s_(0,(ptr)(&ls3028_)),info_fname_1_),(ptr)(&ls3029_));
      res__ = (char)0;
      goto ret0__;
   }
   else {
      st_3_ = S_ptr_VOID_;
      ind_4_ = S_int_VOID_;
      while (1) {
         if (FIL11_check_eof_(infile_2_)) {
            goto goto_tag_515_;
         }
         else {
         }
         ind_4_ = (int)FIL11_get_i_(infile_2_);
         if (FIL11_check_eof_(infile_2_)) {
            goto goto_tag_515_;
         }
         else {
         }
         c__ = (int)FIL11_get_ci_(infile_2_);
         st_3_ = (ptr)FIL11_get_s_up_to_(infile_2_,'\10');
         PATT_(SYS13_feature_table_, 8 + ((ind_4_) << 2)) = (ptr)st_3_;
         STR78_insert_(SYS13_feature_rtable_,st_3_,ind_4_);
      }
   goto_tag_515_: ;
      FIL11_close_(infile_2_);
   }
   SYS13_initialized_ = (char)1;

   ret0__:
   return (res__);
}

int SYS13_number_of_classes_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)num_classes_;

   ret0__:
   return (res__);
}

int SYS13_class_index_(ptr self__, ptr nm__)
{
   int res__ = S_int_VOID_;

   res__ = (int)STR78_get_(SYS13_class_rtable_,nm__);

   ret0__:
   return (res__);
}

ptr SYS13_class_name_(ptr self__, int i__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(SYS13_class_table_, 8 + ((i__) << 2));

   ret0__:
   return (res__);
}

int SYS13_class_base_size_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_ob_base_size_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_c_type_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_cl_ctype_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_number_of_features_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_cl_feat_num_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_array_dim_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_ob_arr_dim_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_array_c_type_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_ob_arr_ctype_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_number_of_attributes_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_ob_attr_num_(i__,0);

   ret0__:
   return (res__);
}

int SYS13_class_attribute_c_type_(ptr self__, int ci__, int a__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_ob_attr_ctype_(ci__,a__,0);

   ret0__:
   return (res__);
}

int SYS13_class_array_element_type_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_cl_arr_satype_(i__,0);

   ret0__:
   return (res__);
}

char SYS13_is_a_descendent_of_(ptr self__, int i__, int j__)
{
   char res__ = S_char_VOID_;

   res__ = (char)safe_is_a_des_of_(i__,j__,0);

   ret0__:
   return (res__);
}

ptr SYS13_feature_name_(ptr self__, int ci__, int f__)
{
   ptr res__ = 0;
   int    ind__ = S_int_VOID_;

   ind__ = (int)safe_cl_feat_name_(ci__,f__,0);
   res__ = (ptr)PATT_(SYS13_feature_table_, 8 + ((ind__) << 2));

   ret0__:
   return (res__);
}

int SYS13_feature_category_(ptr self__, int ci__, int f__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_cl_feat_cat_(ci__,f__,0);

   ret0__:
   return (res__);
}

int SYS13_feature_index_(ptr self__, int ci__, ptr f__)
{
   int res__ = S_int_VOID_;
   int    fi__ = S_int_VOID_;
   int    f_num__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;

   fi__ = (int)STR78_get_(SYS13_feature_rtable_,f__);
   f_num__ = (int)safe_cl_feat_num_(ci__,0);
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ >= f_num__)) {
         goto goto_tag_516_;
      }
      else {
      }
      ind__ = (int)safe_cl_feat_name_(ci__,i__,0);
      if ((ind__ == fi__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_516_: ;
   res__ = (int)(- 1);

   ret0__:
   return (res__);
}

int SYS13_feature_type_(ptr self__, int ci__, int f__)
{
   int res__ = S_int_VOID_;

   res__ = (int)safe_cl_feat_satype_(ci__,f__,0);

   ret0__:
   return (res__);
}

int SYS13_feature_c_type_(ptr self__, int ci__, int f__)
{
   int res__ = S_int_VOID_;
   int    satype__ = S_int_VOID_;

   satype__ = (int)safe_cl_feat_satype_(ci__,f__,0);
   if ((satype__ != 0)) {
      res__ = (int)safe_cl_ctype_(satype__,0);
   }
   else {
   }

   ret0__:
   return (res__);
}

int SYS13_feature_location_(ptr self__, int ci__, int f__)
{
   int res__ = S_int_VOID_;
   int    name__ = S_int_VOID_;

   name__ = (int)safe_cl_feat_name_(ci__,f__,0);
   res__ = (int)safe_get_dispatch_(ci__,name__,0);

   ret0__:
   return (res__);
}


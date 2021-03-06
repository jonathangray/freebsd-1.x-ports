/* str_co109.c : Sather class: STR_CONST_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR12_create_(ptr self__, ptr st__);
extern char CHA14_is_digit_(char self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int STR20_length_(ptr self__);
extern void ROU156_add_str_const_(ptr self__, ptr str_const__);
extern ptr INT164_insert_(ptr self__, int k__);
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
extern void PRI187_cprint_sather_str_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__);
extern void PRI187_cprint_sather_str1_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__);
extern /*shared*/ ptr GLO68_str_typeob_s_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern void GLO94_add_str_const_(ptr self__, ptr str_const__);
extern void GLO94_cprint_global_tmpnm_str_(ptr self__, int strval__, ptr outfile__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern /*constant*/ int RES97_STR_ici_;
extern ptr SAT99_i_(ptr self__, int in__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern void GLO94_cprint_local_tmpnm_str_(ptr self__, int strval__, ptr outfile__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int STR109_print_indent_ = 2;
ptr STR109_create_(ptr self__, int v__, int ln__);
void STR109_out_of_line_(ptr self__, ptr fn__);
ptr STR109_dup_(ptr self__);
void STR109_put_kwdname_(ptr self__, int nm__);
ptr STR109_sather_code_(ptr self__);
ptr STR109_initialize_(ptr self__, ptr initarg__);
void STR109_resolve_predef_types_(ptr self__, int index__);
void STR109_semant_(ptr self__, ptr symtab__);
ptr STR109_typeof_(ptr self__);
int STR109_get_offset_(ptr self__);
void STR109_cprint_offset_(ptr self__, ptr outfile__);
ptr STR109_get_constval_(ptr self__);
void STR109_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void STR109_cprint_cname_(ptr self__, ptr outfile__);
void STR109_cprint_extern_(ptr self__, ptr outfile__);
void STR109_cprint_access_value_(ptr self__, ptr outfile__);
void STR109_cprint_init_code_(ptr self__, ptr outfile__);
char STR109_valid_init_expr_(ptr self__);
char STR109_assignable_p_(ptr self__);
ptr STR109_gen_temps_(ptr self__);
ptr STR109_expr_eval_constant_(ptr self__);
void STR109_get_ext_strs_(ptr self__);
char STR109_to_be_pre_evaluated_(ptr self__);
char STR109_access_value_only_(ptr self__);
void STR109_fob_error_(ptr self__, ptr op_name__, ptr cls_name__);
void STR109_cprint_pre_code_(ptr self__, ptr outfile__);
void STR109_cprint_act_code_(ptr self__, ptr outfile__);
void STR109_cprint_cast_code_(ptr self__, ptr outfile__);
ptr STR109_val_(ptr self__);
int STR109_temp_name_(ptr self__);
int STR109_find_actual_length_(ptr self__);
/*constant*/ int STR109_cont1_ = 1;
void STR109_cprint_mach_indep_(ptr self__, ptr outfile__);
void STR109_cprint_mach_indep_global_(ptr self__, ptr outfile__);
extern int attr_ent_STR109[];

ptr STR109_create_(ptr self__, int v__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(109,0);
   IATT_(res__,28) = (int)v__;
   IATT_(res__,32) = (int)ln__;
   PATT_(res__,12) = (ptr)GLO68_str_typeob_s_;

   ret0__:
   return (res__);
}

void STR109_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,32) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,32));

   ret0__:
   return;
}

ptr STR109_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void STR109_put_kwdname_(ptr self__, int nm__)
{
   ptr gl1351_;
   static int gl1352_;
   static union dtype_ gl1353_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1351_ = x__;
   cache_dispatch_(gl1351_,796,gl1352_,INTVAL_(gl1353_));
   IATT_(gl1351_,INTVAL_(gl1353_)) = (int)nm__;

   ret0__:
   return;
}

ptr STR109_sather_code_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(self__,28)));

   ret0__:
   return (res__);
}

ptr STR109_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void STR109_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void STR109_semant_(ptr self__, ptr symtab__)
{
   ptr gl1354_;
   static int gl1355_;
   static union dtype_ gl1356_;
   int gl116_;
   ptr gl1357_;
   static int gl1358_;
   static union dtype_ gl1359_;
   ptr    rout_featob_s__ = 0;

   gl1354_ = GLO68_curr_feature_;
   gl116_ = TYPE_(gl1354_);
   if ((gl116_ != 156)) {
      CATT_(self__,4) = (char)1;
   }
   else {
   }
   PATT_(self__,12) = (ptr)GLO68_str_typeob_s_;
   gl1357_ = GLO68_curr_feature_;
   switch (TYPE_(gl1357_)) {
      case (156) :
         rout_featob_s__ = (ptr)GLO68_curr_feature_;
         ROU156_add_str_const_(rout_featob_s__,self__);
         break;
      case (152) :
      case (151) :
         GLO94_add_str_const_(0,self__);
         PATT_(GLO68_curr_class_inst_,120) = (ptr)INT164_insert_(PATT_(GLO68_curr_class_inst_,120),IATT_(self__,28));
         break;
      default:
         ;
         ;
   }

   ret0__:
   return;
}

ptr STR109_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int STR109_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void STR109_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr STR109_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void STR109_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{
   SATHER_STR_(20,19,ls866_,"STR_CONST_EXPROB_S");
   SATHER_STR_(20,27,ls1749_,"Weird initial continuation");
   SATHER_STR_(20,3,ls1750_,",\"");
   int    len__ = S_int_VOID_;

   if ((cont__ != 1)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls866_),(ptr)(&ls1749_));
   }
   else {
      len__ = (int)STR109_find_actual_length_(self__);
      (void)SAT99_c_(SAT99_i_(SAT99_c_(SAT99_i_(outfile__,RES97_STR_ici_),','),(len__ + 1)),',');
      if (CATT_(self__,4)) {
         GLO94_cprint_global_tmpnm_str_(0,IATT_(self__,28),outfile__);
      }
      else {
         GLO94_cprint_local_tmpnm_str_(0,IATT_(self__,28),outfile__);
      }
      (void)SAT99_c_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls1750_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),'"');
   }

   ret0__:
   return;
}

void STR109_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void STR109_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void STR109_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void STR109_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char STR109_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

char STR109_assignable_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr STR109_gen_temps_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr STR109_expr_eval_constant_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void STR109_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

char STR109_to_be_pre_evaluated_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char STR109_access_value_only_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

void STR109_fob_error_(ptr self__, ptr op_name__, ptr cls_name__)
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

void STR109_cprint_pre_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void STR109_cprint_act_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,8,ls1751_,"(ptr)(&");

   STR109_cprint_cast_code_(self__,outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls1751_));
   if (CATT_(self__,4)) {
      GLO94_cprint_global_tmpnm_str_(0,IATT_(self__,28),outfile__);
   }
   else {
      GLO94_cprint_local_tmpnm_str_(0,IATT_(self__,28),outfile__);
   }
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void STR109_cprint_cast_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,2,ls1493_,"(");
   SATHER_STR_(20,9,ls350_,"EXPROB_S");
   SATHER_STR_(20,20,ls1685_,"Invalid C type cast");
   SATHER_STR_(20,2,ls1278_,")");

   if ((IATT_(self__,20) != 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls1493_));
      switch (IATT_(self__,20)) {
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
            ERR96_compiler_error_msg_(0,(ptr)(&ls350_),(ptr)(&ls1685_));
            ;
      }
      (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
   }
   else {
   }

   ret0__:
   return;
}

ptr STR109_val_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,1,ls1016_,"");

   res__ = (ptr)STR12_create_(0,(ptr)(&ls1016_));

   ret0__:
   return (res__);
}

int STR109_temp_name_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,28);

   ret0__:
   return (res__);
}

int STR109_find_actual_length_(ptr self__)
{
   int res__ = S_int_VOID_;
   ptr    tmpstr__ = 0;
   int    num_chars__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   char    seen_back_slash__ = S_char_VOID_;
   int    num_digits__ = S_int_VOID_;

   tmpstr__ = (ptr)STR69_at_index_(GLO68_str_table_,IATT_(self__,28));
   num_chars__ = (int)STR20_length_(tmpstr__);
   res__ = (int)num_chars__;
   i__ = (int)0;
   seen_back_slash__ = (char)0;
   num_digits__ = S_int_VOID_;
   while (1) {
      if ((i__ >= num_chars__)) {
         goto goto_tag_1360_;
      }
      else {
      }
      if ((CATT_(tmpstr__, 8 + ((i__))) == '\\')) {
         if (seen_back_slash__) {
            seen_back_slash__ = (char)0;
            num_digits__ = (int)0;
         }
         else {
            res__ = (int)(res__ - 1);
            seen_back_slash__ = (char)1;
            num_digits__ = (int)0;
         }
      }
      else {
         if (CHA14_is_digit_(CATT_(tmpstr__, 8 + ((i__))))) {
            if (seen_back_slash__) {
               if ((num_digits__ == 2)) {
                  seen_back_slash__ = (char)0;
                  num_digits__ = (int)0;
               }
               else {
                  num_digits__ = (int)(num_digits__ + 1);
                  res__ = (int)(res__ - 1);
               }
            }
            else {
            }
         }
         else {
            seen_back_slash__ = (char)0;
            num_digits__ = (int)0;
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1360_: ;

   ret0__:
   return (res__);
}

void STR109_cprint_mach_indep_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,3,ls650_,";\n");

   PRI187_cprint_sather_str_mi_(0,outfile__,1,self__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);

   ret0__:
   return;
}

void STR109_cprint_mach_indep_global_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,3,ls650_,";\n");

   PRI187_cprint_sather_str1_mi_(0,outfile__,1,self__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);

   ret0__:
   return;
}


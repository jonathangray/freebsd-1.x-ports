/* assert135.c : Sather class: ASSERT_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*constant*/ int OP_72_and_op_ind_;
extern /*shared*/ char COM82_rt_code_check_;
extern /*shared*/ char COM82_dbg_mode_;
extern ptr STR20_create_();
extern ptr STR20_s_();
extern char TYP149_bool_type_p_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern ptr ERR96_def_filename_();
extern ptr SAT99_indent_();
extern int ERR96_def_lineno_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern ptr SAT99_i_();
extern ptr SAT99_inc_ln_();
extern ptr OP_110_create_();
extern ptr EXP117_dup_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern ptr EXP117_gen_temps_();
extern EXP117_get_ext_strs_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern ptr LST120_push_();
extern DBT190_addCLine_();
extern struct { int tp_; int sz_; char st_; } gs1060_;
extern struct { int tp_; int sz_; char st_; } gs1062_;
extern struct { int tp_; int sz_; char st_; } gs1064_;
extern struct { int tp_; int sz_; char st_; } gs1066_;
extern struct { int tp_; int sz_; char st_; } gs1068_;
extern struct { int tp_; int sz_; char st_; } gs1070_;
extern struct { int tp_; int sz_; char st_; } gs1072_;
extern struct { int tp_; int sz_; char st_; } gs1074_;
#include "macros_.h"



/*constant*/ ptr ASS135_source_files_kw_ = (ptr)(&gs1060_);
/*constant*/ ptr ASS135_object_files_kw_ = (ptr)(&gs1062_);
/*constant*/ ptr ASS135_cc_flags_kw_ = (ptr)(&gs1064_);
/*constant*/ ptr ASS135_c_macro_kw_ = (ptr)(&gs1066_);
/*constant*/ ptr ASS135_c_name_kw_ = (ptr)(&gs1068_);
/*constant*/ ptr ASS135_include_kw_ = (ptr)(&gs1070_);
/*constant*/ ptr ASS135_sather_home_kw_ = (ptr)(&gs1072_);
/*constant*/ ptr ASS135_c_compiler_kw_ = (ptr)(&gs1074_);
/*constant*/ int ASS135_source_files_ind_ = 0;
/*constant*/ int ASS135_object_files_ind_ = 1;
/*constant*/ int ASS135_cc_flags_ind_ = 2;
/*constant*/ int ASS135_c_macro_ind_ = 3;
/*constant*/ int ASS135_c_name_ind_ = 4;
/*constant*/ int ASS135_include_ind_ = 5;
/*constant*/ int ASS135_sather_home_ind_ = 6;
/*constant*/ int ASS135_c_compiler_ind_ = 7;
/*constant*/ int ASS135_compile_keys_fst_ind_ = 0;
/*constant*/ int ASS135_compile_keys_lst_ind_ = 7;
/*constant*/ int ASS135_num_compile_keys_ = 8;
/*constant*/ int ASS135_non_compile_key_ind_ = 8;
/*constant*/ int ASS135_eof_tok_ = -1;
/*constant*/ int ASS135_ident_tok_ = -2;
/*constant*/ int ASS135_qexp_tok_ = -3;
char ASS135_compile_key_p_();
ptr ASS135_initialize_();
/*constant*/ int ASS135_print_indent_ = 2;
ptr ASS135_create_();
ASS135_out_of_line_();
ptr ASS135_dup_();
ASS135_put_kwdname_();
ptr ASS135_sather_code_();
ASS135_resolve_predef_types_();
ASS135_semant_();
ptr ASS135_typeof_();
int ASS135_get_offset_();
ASS135_cprint_offset_();
ptr ASS135_get_constval_();
ASS135_cont_cprint_code_();
ASS135_cprint_cname_();
ASS135_cprint_extern_();
ASS135_cprint_access_value_();
ASS135_cprint_init_code_();
char ASS135_typechk_exprs_();
ptr ASS135_gen_temps_();
ASS135_gen_goto_tags_();
ASS135_validate_dispatches_and_get_ext_strs_();
ASS135_cprint_code_();
ASS135_and_assertion_();
extern int attr_ent_ASS135[];

char ASS135_compile_key_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ <= 7) & (i__ >= 0));

   ret0__:
   return (res__);
}

ptr ASS135_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ASS135_create_(self__,a__,ln__)
ptr self__;
ptr a__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(135,0);
   PATT_(res__,16) = (ptr)a__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

ASS135_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ASS135_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3211_;
   static int gl3212_;
   static union dtype_ gl3213_;
   ptr gl309_;

   gl3211_ = PATT_(self__,16);
   cache_dispatch_(gl3211_,471,gl3212_,INTVAL_(gl3213_));
   gl309_ = PFN_(gl3213_)(gl3211_);
   res__ = (ptr)ASS135_create_(self__,gl309_,IATT_(self__,4));

   ret0__:
   return (res__);
}

ASS135_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3214_;
   static int gl3215_;
   static union dtype_ gl3216_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3214_ = x__;
   cache_dispatch_(gl3214_,796,gl3215_,INTVAL_(gl3216_));
   IATT_(gl3214_,INTVAL_(gl3216_)) = (int)nm__;

   ret0__:
   return;
}

ptr ASS135_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ASS135_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3217_;
   static int gl3218_;
   static union dtype_ gl3219_;

   gl3217_ = PATT_(self__,16);
   cache_dispatch_(gl3217_,522,gl3218_,INTVAL_(gl3219_));
   VFN_(gl3219_)(gl3217_,index__);

   ret0__:
   return;
}

ASS135_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,56,ls2655_,"(ASSERT_STMTOB_S): Assertion does not return BOOL value");
   ptr gl3220_;
   static int gl3221_;
   static union dtype_ gl3222_;
   ptr gl3223_;
   static int gl3224_;
   static union dtype_ gl3225_;
   ptr gl310_;
   ptr gl3226_;
   static int gl3227_;
   static union dtype_ gl3228_;
   ptr gl3229_;
   static int gl3230_;
   static union dtype_ gl3231_;
   ptr gl3232_;
   static int gl3233_;
   static union dtype_ gl3234_;
   char gl311_;
   ptr gl3235_;
   static int gl3236_;
   static union dtype_ gl3237_;

   PATT_(self__,12) = (ptr)PATT_(symtab__,4);
   gl3220_ = PATT_(self__,16);
   cache_dispatch_(gl3220_,588,gl3221_,INTVAL_(gl3222_));
   VFN_(gl3222_)(gl3220_,symtab__);
   gl3223_ = PATT_(self__,16);
   cache_dispatch_(gl3223_,1577,gl3224_,INTVAL_(gl3225_));
   gl310_ = PATT_(gl3223_,INTVAL_(gl3225_));
   if ((gl310_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2655_)));
      gl3226_ = PATT_(self__,16);
      cache_dispatch_(gl3226_,1577,gl3227_,INTVAL_(gl3228_));
      PATT_(gl3226_,INTVAL_(gl3228_)) = (ptr)GLO68_bool_typeob_s_;
   }
   else {
      gl3232_ = PATT_(self__,16);
      cache_dispatch_(gl3232_,1577,gl3233_,INTVAL_(gl3234_));
      gl3229_ = PATT_(gl3232_,INTVAL_(gl3234_));
      cache_dispatch_(gl3229_,1768,gl3230_,INTVAL_(gl3231_));
      gl311_ = CFN_(gl3231_)(gl3229_);
      if ((! gl311_)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2655_)));
         gl3235_ = PATT_(self__,16);
         cache_dispatch_(gl3235_,1577,gl3236_,INTVAL_(gl3237_));
         PATT_(gl3235_,INTVAL_(gl3237_)) = (ptr)GLO68_bool_typeob_s_;
      }
      else {
      }
   }
   if ((CATT_(GLO68_curr_class_inst_,12) | CATT_(GLO68_curr_class_inst_,13))) {
      goto ret0__;
   }
   else {
   }

   ret0__:
   return;
}

ptr ASS135_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ASS135_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ASS135_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ASS135_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ASS135_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

ASS135_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ASS135_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ASS135_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ASS135_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char ASS135_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr ASS135_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3238_;
   static int gl3239_;
   static union dtype_ gl3240_;

   gl3238_ = PATT_(self__,16);
   cache_dispatch_(gl3238_,1582,gl3239_,INTVAL_(gl3240_));
   res__ = (ptr)PFN_(gl3240_)(gl3238_);

   ret0__:
   return (res__);
}

ASS135_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{


   ret0__:
   return;
}

ASS135_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   ptr gl3241_;
   static int gl3242_;
   static union dtype_ gl3243_;

   gl3241_ = PATT_(self__,16);
   cache_dispatch_(gl3241_,1677,gl3242_,INTVAL_(gl3243_));
   VFN_(gl3243_)(gl3241_);

   ret0__:
   return;
}

ASS135_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,9,ls2656_,"assert_(");
   SATHER_STR_(20,3,ls1750_,",\"");
   SATHER_STR_(20,5,ls722_,"\");\n");
   ptr gl3244_;
   static int gl3245_;
   static union dtype_ gl3246_;
   ptr gl3247_;
   static int gl3248_;
   static union dtype_ gl3249_;

   if (COM82_rt_code_check_) {
      gl3244_ = PATT_(self__,16);
      cache_dispatch_(gl3244_,1589,gl3245_,INTVAL_(gl3246_));
      VFN_(gl3246_)(gl3244_,outfile__);
      (void)SAT99_indent_(outfile__);
      if ((COM82_dbg_mode_ == 1)) {
         DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
      }
      else {
      }
      (void)SAT99_s_(outfile__,(ptr)(&ls2656_));
      gl3247_ = PATT_(self__,16);
      cache_dispatch_(gl3247_,965,gl3248_,INTVAL_(gl3249_));
      VFN_(gl3249_)(gl3247_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(self__,4)),(ptr)(&ls1750_)),ERR96_def_filename_(0,IATT_(self__,4))),(ptr)(&ls722_)),1);
   }
   else {
   }

   ret0__:
   return;
}

ASS135_and_assertion_(self__,a__,ln__)
ptr self__;
ptr a__;
int ln__;
{
   ptr gl3250_;
   static int gl3251_;
   static union dtype_ gl3252_;
   ptr gl312_;

   gl3250_ = PATT_(a__,16);
   cache_dispatch_(gl3250_,471,gl3251_,INTVAL_(gl3252_));
   gl312_ = PFN_(gl3252_)(gl3250_);
   PATT_(self__,16) = (ptr)OP_110_create_(0,8,LST120_push_(LST120_push_(new1_(120,2,0),PATT_(self__,16)),gl312_),ln__);

   ret0__:
   return;
}


/* call_s142.c : Sather class: CALL_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ char COM82_warn_rt_val_;
extern /*shared*/ char COM82_dbg_mode_;
extern ptr STR20_create_();
extern ptr STR20_s_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_warning_msg_();
extern ptr ERR96_def_filename_();
extern ptr SAT99_indent_();
extern int ERR96_def_lineno_();
extern ptr SAT99_s_();
extern ptr SAT99_inc_ln_();
extern EXP117_out_of_line_();
extern ptr EXP117_dup_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern ptr EXP117_gen_temps_();
extern EXP117_get_ext_strs_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern DBT190_addCLine_();
#include "macros_.h"



/*constant*/ int CAL142_print_indent_ = 2;
ptr CAL142_create_();
CAL142_out_of_line_();
ptr CAL142_dup_();
CAL142_put_kwdname_();
ptr CAL142_sather_code_();
ptr CAL142_initialize_();
CAL142_resolve_predef_types_();
CAL142_semant_();
ptr CAL142_typeof_();
int CAL142_get_offset_();
CAL142_cprint_offset_();
ptr CAL142_get_constval_();
CAL142_cont_cprint_code_();
CAL142_cprint_cname_();
CAL142_cprint_extern_();
CAL142_cprint_access_value_();
CAL142_cprint_init_code_();
char CAL142_typechk_exprs_();
ptr CAL142_gen_temps_();
CAL142_gen_goto_tags_();
CAL142_validate_dispatches_and_get_ext_strs_();
CAL142_cprint_code_();
extern int attr_ent_CAL142[];

ptr CAL142_create_(self__,exp__,ln__)
ptr self__;
ptr exp__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(142,0);
   PATT_(res__,12) = (ptr)exp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

CAL142_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl3626_;
   static int gl3627_;
   static union dtype_ gl3628_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   gl3626_ = PATT_(self__,12);
   cache_dispatch_(gl3626_,518,gl3627_,INTVAL_(gl3628_));
   VFN_(gl3628_)(gl3626_,fn__);

   ret0__:
   return;
}

ptr CAL142_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3629_;
   static int gl3630_;
   static union dtype_ gl3631_;
   ptr gl346_;

   gl3629_ = PATT_(self__,12);
   cache_dispatch_(gl3629_,471,gl3630_,INTVAL_(gl3631_));
   gl346_ = PFN_(gl3631_)(gl3629_);
   res__ = (ptr)CAL142_create_(self__,gl346_,IATT_(self__,4));

   ret0__:
   return (res__);
}

CAL142_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3632_;
   static int gl3633_;
   static union dtype_ gl3634_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3632_ = x__;
   cache_dispatch_(gl3632_,796,gl3633_,INTVAL_(gl3634_));
   IATT_(gl3632_,INTVAL_(gl3634_)) = (int)nm__;

   ret0__:
   return;
}

ptr CAL142_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CAL142_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

CAL142_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3635_;
   static int gl3636_;
   static union dtype_ gl3637_;

   gl3635_ = PATT_(self__,12);
   cache_dispatch_(gl3635_,522,gl3636_,INTVAL_(gl3637_));
   VFN_(gl3637_)(gl3635_,index__);

   ret0__:
   return;
}

CAL142_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,48,ls2652_,"(CALL_STMTOB_S): Call statement returns a value");
   ptr gl3638_;
   static int gl3639_;
   static union dtype_ gl3640_;
   ptr gl3641_;
   static int gl3642_;
   static union dtype_ gl3643_;
   ptr gl347_;

   gl3638_ = PATT_(self__,12);
   cache_dispatch_(gl3638_,588,gl3639_,INTVAL_(gl3640_));
   VFN_(gl3640_)(gl3638_,symtab__);
   if (COM82_warn_rt_val_) {
      gl3641_ = PATT_(self__,12);
      cache_dispatch_(gl3641_,1577,gl3642_,INTVAL_(gl3643_));
      gl347_ = PATT_(gl3641_,INTVAL_(gl3643_));
      if ((gl347_ != 0)) {
         ERR96_format_warning_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2652_)));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

ptr CAL142_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int CAL142_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

CAL142_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr CAL142_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

CAL142_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

CAL142_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

CAL142_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

CAL142_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

CAL142_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char CAL142_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr CAL142_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3644_;
   static int gl3645_;
   static union dtype_ gl3646_;

   gl3644_ = PATT_(self__,12);
   cache_dispatch_(gl3644_,1582,gl3645_,INTVAL_(gl3646_));
   res__ = (ptr)PFN_(gl3646_)(gl3644_);

   ret0__:
   return (res__);
}

CAL142_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{


   ret0__:
   return;
}

CAL142_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   ptr gl3647_;
   static int gl3648_;
   static union dtype_ gl3649_;

   gl3647_ = PATT_(self__,12);
   cache_dispatch_(gl3647_,1677,gl3648_,INTVAL_(gl3649_));
   VFN_(gl3649_)(gl3647_);

   ret0__:
   return;
}

CAL142_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,7,ls2653_,"(void)");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl3650_;
   static int gl3651_;
   static union dtype_ gl3652_;
   ptr gl3653_;
   static int gl3654_;
   static union dtype_ gl3655_;
   ptr gl348_;
   ptr gl3656_;
   static int gl3657_;
   static union dtype_ gl3658_;

   gl3650_ = PATT_(self__,12);
   cache_dispatch_(gl3650_,1589,gl3651_,INTVAL_(gl3652_));
   VFN_(gl3652_)(gl3650_,outfile__);
   (void)SAT99_indent_(outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   gl3653_ = PATT_(self__,12);
   cache_dispatch_(gl3653_,1577,gl3654_,INTVAL_(gl3655_));
   gl348_ = PATT_(gl3653_,INTVAL_(gl3655_));
   if ((gl348_ != 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2653_));
   }
   else {
   }
   gl3656_ = PATT_(self__,12);
   cache_dispatch_(gl3656_,965,gl3657_,INTVAL_(gl3658_));
   VFN_(gl3658_)(gl3656_,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);

   ret0__:
   return;
}


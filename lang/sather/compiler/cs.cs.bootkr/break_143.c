/* break_143.c : Sather class: BREAK_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern int BLO202_get_goto_tag_();
extern /*shared*/ char COM82_dbg_mode_;
extern ptr STR20_create_();
extern ptr STR20_s_();
extern char INS150_conforms_to_();
extern GLO94_cprint_goto_tag_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern ptr ERR96_def_filename_();
extern ptr SAT99_indent_();
extern int ERR96_def_lineno_();
extern ptr SAT99_s_();
extern ptr SAT99_inc_ln_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern ptr EXP117_gen_temps_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern DBT190_addCLine_();
#include "macros_.h"



/*constant*/ int BRE143_print_indent_ = 2;
ptr BRE143_create_();
BRE143_out_of_line_();
ptr BRE143_dup_();
BRE143_put_kwdname_();
ptr BRE143_sather_code_();
ptr BRE143_initialize_();
BRE143_resolve_predef_types_();
BRE143_semant_();
ptr BRE143_typeof_();
int BRE143_get_offset_();
BRE143_cprint_offset_();
ptr BRE143_get_constval_();
BRE143_cont_cprint_code_();
BRE143_cprint_cname_();
BRE143_cprint_extern_();
BRE143_cprint_access_value_();
BRE143_cprint_init_code_();
char BRE143_typechk_exprs_();
ptr BRE143_gen_temps_();
BRE143_gen_goto_tags_();
BRE143_validate_dispatches_and_get_ext_strs_();
BRE143_cprint_code_();
extern int attr_ent_BRE143[];

ptr BRE143_create_(self__,ln__)
ptr self__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(143,0);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

BRE143_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr BRE143_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)BRE143_create_(self__,IATT_(self__,4));
   if ((PATT_(self__,16) != 0)) {
      PATT_(res__,16) = (ptr)PATT_(self__,16);
   }
   else {
   }

   ret0__:
   return (res__);
}

BRE143_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3659_;
   static int gl3660_;
   static union dtype_ gl3661_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3659_ = x__;
   cache_dispatch_(gl3659_,796,gl3660_,INTVAL_(gl3661_));
   IATT_(gl3659_,INTVAL_(gl3661_)) = (int)nm__;

   ret0__:
   return;
}

ptr BRE143_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr BRE143_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

BRE143_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3662_;
   static int gl3663_;
   static union dtype_ gl3664_;

   if ((PATT_(self__,16) != 0)) {
      gl3662_ = PATT_(self__,16);
      cache_dispatch_(gl3662_,522,gl3663_,INTVAL_(gl3664_));
      VFN_(gl3664_)(gl3662_,index__);
   }
   else {
   }

   ret0__:
   return;
}

BRE143_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,51,ls2645_,"(BREAK_STMTOB_S): Expression does not return value");
   SATHER_STR_(20,63,ls2646_,"(BREAK_STMTOB_S): Expression does not return object type value");
   ptr gl3665_;
   static int gl3666_;
   static union dtype_ gl3667_;
   ptr gl3668_;
   static int gl3669_;
   static union dtype_ gl3670_;
   ptr gl3671_;
   static int gl3672_;
   static union dtype_ gl3673_;
   ptr    et__ = 0;

   if ((PATT_(self__,16) != 0)) {
      gl3665_ = PATT_(self__,16);
      cache_dispatch_(gl3665_,588,gl3666_,INTVAL_(gl3667_));
      VFN_(gl3667_)(gl3665_,symtab__);
      gl3668_ = PATT_(self__,16);
      cache_dispatch_(gl3668_,1577,gl3669_,INTVAL_(gl3670_));
      et__ = (ptr)PATT_(gl3668_,INTVAL_(gl3670_));
      if ((et__ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2645_)));
         gl3671_ = PATT_(self__,16);
         cache_dispatch_(gl3671_,1577,gl3672_,INTVAL_(gl3673_));
         PATT_(gl3671_,INTVAL_(gl3673_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
         if ((! INS150_conforms_to_(et__,GLO68_ob_typeob_s_))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2646_)));
         }
         else {
         }
      }
   }
   else {
   }

   ret0__:
   return;
}

ptr BRE143_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int BRE143_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

BRE143_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr BRE143_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

BRE143_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

BRE143_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

BRE143_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

BRE143_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

BRE143_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char BRE143_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr BRE143_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3674_;
   static int gl3675_;
   static union dtype_ gl3676_;

   if ((PATT_(self__,16) != 0)) {
      gl3674_ = PATT_(self__,16);
      cache_dispatch_(gl3674_,1582,gl3675_,INTVAL_(gl3676_));
      res__ = (ptr)PFN_(gl3676_)(gl3674_);
   }
   else {
   }

   ret0__:
   return (res__);
}

BRE143_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{
   SATHER_STR_(20,55,ls2647_,"(BREAK_STMTOB_S): \"break\" statement found outside loop");
   ptr gl3677_;
   static int gl3678_;
   static union dtype_ gl3679_;

   if ((PATT_(self__,16) == 0)) {
      if ((block__ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2647_)));
         goto ret0__;
      }
      else {
         gl3677_ = block__;
         cache_dispatch_(gl3677_,2613,gl3678_,INTVAL_(gl3679_));
         IATT_(self__,12) = (int)IFN_(gl3679_)(gl3677_);
      }
   }
   else {
   }

   ret0__:
   return;
}

BRE143_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

BRE143_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,55,ls2647_,"(BREAK_STMTOB_S): \"break\" statement found outside loop");
   SATHER_STR_(20,14,ls2648_,"goto ret0__;\n");
   SATHER_STR_(20,6,ls2649_,"goto ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,10,ls2650_,"EH_THROW(");
   SATHER_STR_(20,3,ls2651_,");");
   ptr gl3680_;
   static int gl3681_;
   static union dtype_ gl3682_;
   ptr gl3683_;
   static int gl3684_;
   static union dtype_ gl3685_;

   (void)SAT99_indent_(outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   if ((PATT_(self__,16) == 0)) {
      if ((IATT_(self__,12) == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2647_)));
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2648_)),1);
      }
      else {
         (void)SAT99_s_(outfile__,(ptr)(&ls2649_));
         GLO94_cprint_goto_tag_(0,IATT_(self__,12),outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
   }
   else {
      gl3680_ = PATT_(self__,16);
      cache_dispatch_(gl3680_,1589,gl3681_,INTVAL_(gl3682_));
      VFN_(gl3682_)(gl3680_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2650_));
      gl3683_ = PATT_(self__,16);
      cache_dispatch_(gl3683_,965,gl3684_,INTVAL_(gl3685_));
      VFN_(gl3685_)(gl3683_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2651_));
   }

   ret0__:
   return;
}


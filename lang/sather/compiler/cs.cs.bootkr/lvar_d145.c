/* lvar_d145.c : Sather class: LVAR_DECL_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr TYP149_dup_();
extern TYP149_resolve_predef_types_();
extern ptr STR20_s_();
extern ptr TYP149_inst_cls_();
extern char TYP149_is_dispatched_();
extern char TYP149_nonptr_p_();
extern TYP149_cprint_ctype_();
extern int TYP149_inst_ind_();
extern TYP149_cprint_void_();
extern FEA162_remember_local_();
extern char SYM186_defined_in_non_outer_scopes_p_();
extern SYM186_enter_sym_();
extern DBT190_addCLine_();
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ char COM82_dbg_mode_;
extern int GLO94_global_key_();
extern char GLO94_conform_tst_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern ERR96_type_mismatch_err_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern ptr SAT99_i_();
extern ptr SAT99_indent_();
extern ptr SAT99_inc_ln_();
extern ptr ERR96_def_filename_();
extern int ERR96_def_lineno_();
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern ptr EXP117_dup_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern ptr EXP117_gen_temps_();
extern EXP117_get_ext_strs_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
#include "macros_.h"



/*constant*/ int LVA145_print_indent_ = 2;
ptr LVA145_create_();
LVA145_out_of_line_();
ptr LVA145_dup_();
LVA145_put_kwdname_();
ptr LVA145_sather_code_();
ptr LVA145_initialize_();
LVA145_resolve_predef_types_();
LVA145_semant_();
ptr LVA145_typeof_();
int LVA145_get_offset_();
LVA145_cprint_offset_();
ptr LVA145_get_constval_();
LVA145_cont_cprint_code_();
LVA145_cprint_cname_();
LVA145_cprint_extern_();
LVA145_cprint_access_value_();
LVA145_cprint_init_code_();
char LVA145_typechk_exprs_();
ptr LVA145_gen_temps_();
LVA145_gen_goto_tags_();
LVA145_validate_dispatches_and_get_ext_strs_();
LVA145_cprint_code_();
char LVA145_undefined_p_();
int LVA145_declob_s_name_();
LVA145_cprint_decln_();
extern int attr_ent_LVA145[];

ptr LVA145_create_(self__,nm__,tp__,exp__,ln__)
ptr self__;
int nm__;
ptr tp__;
ptr exp__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(145,0);
   IATT_(res__,12) = (int)nm__;
   PATT_(res__,16) = (ptr)tp__;
   PATT_(res__,20) = (ptr)exp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

LVA145_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LVA145_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3689_;
   static int gl3690_;
   static union dtype_ gl3691_;
   ptr gl3692_;
   static int gl3693_;
   static union dtype_ gl3694_;
   ptr gl349_;
   ptr gl350_;
   ptr gl3695_;
   static int gl3696_;
   static union dtype_ gl3697_;
   ptr gl351_;

   if ((PATT_(self__,20) != 0)) {
      gl3689_ = PATT_(self__,16);
      cache_dispatch_(gl3689_,471,gl3690_,INTVAL_(gl3691_));
      gl349_ = PFN_(gl3691_)(gl3689_);
      gl3692_ = PATT_(self__,20);
      cache_dispatch_(gl3692_,471,gl3693_,INTVAL_(gl3694_));
      gl350_ = PFN_(gl3694_)(gl3692_);
      res__ = (ptr)LVA145_create_(self__,IATT_(self__,12),gl349_,gl350_,IATT_(self__,4));
   }
   else {
      gl3695_ = PATT_(self__,16);
      cache_dispatch_(gl3695_,471,gl3696_,INTVAL_(gl3697_));
      gl351_ = PFN_(gl3697_)(gl3695_);
      res__ = (ptr)LVA145_create_(self__,IATT_(self__,12),gl351_,0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}

LVA145_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3698_;
   static int gl3699_;
   static union dtype_ gl3700_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3698_ = x__;
   cache_dispatch_(gl3698_,796,gl3699_,INTVAL_(gl3700_));
   IATT_(gl3698_,INTVAL_(gl3700_)) = (int)nm__;

   ret0__:
   return;
}

ptr LVA145_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LVA145_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LVA145_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3701_;
   static int gl3702_;
   static union dtype_ gl3703_;
   ptr gl3704_;
   static int gl3705_;
   static union dtype_ gl3706_;

   gl3701_ = PATT_(self__,16);
   cache_dispatch_(gl3701_,522,gl3702_,INTVAL_(gl3703_));
   VFN_(gl3703_)(gl3701_,index__);
   if ((PATT_(self__,20) != 0)) {
      gl3704_ = PATT_(self__,20);
      cache_dispatch_(gl3704_,522,gl3705_,INTVAL_(gl3706_));
      VFN_(gl3706_)(gl3704_,index__);
   }
   else {
   }

   ret0__:
   return;
}

LVA145_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,59,ls2584_,"(LVAR_DECL_STMTOB_S): No return value from init expression");
   SATHER_STR_(20,18,ls2585_,"local declaration");
   SATHER_STR_(20,1,ls1016_,"");
   ptr gl3707_;
   static int gl3708_;
   static union dtype_ gl3709_;
   ptr gl352_;
   ptr gl3710_;
   static int gl3711_;
   static union dtype_ gl3712_;
   ptr gl3713_;
   static int gl3714_;
   static union dtype_ gl3715_;
   ptr gl353_;
   ptr gl3716_;
   static int gl3717_;
   static union dtype_ gl3718_;
   ptr gl354_;
   ptr gl3719_;
   static int gl3720_;
   static union dtype_ gl3721_;
   ptr gl355_;
   ptr gl3722_;
   static int gl3723_;
   static union dtype_ gl3724_;
   ptr gl3725_;
   static int gl3726_;
   static union dtype_ gl3727_;
   int gl3728_;
   ptr gl3729_;
   static int gl3730_;
   static union dtype_ gl3731_;
   ptr    new_type_spec__ = 0;
   ptr    co__ = 0;

   if (SYM186_defined_in_non_outer_scopes_p_(symtab__,IATT_(self__,12))) {
      IATT_(self__,24) = (int)GLO94_global_key_(0);
   }
   else {
   }
   SYM186_enter_sym_(symtab__,IATT_(self__,12),self__);
   gl352_ = self__;
   gl3707_ = GLO68_curr_feature_;
   cache_dispatch_(gl3707_,2122,gl3708_,INTVAL_(gl3709_));
   VFN_(gl3709_)(gl3707_,gl352_);
   if ((PATT_(self__,20) != 0)) {
      gl3710_ = PATT_(self__,20);
      cache_dispatch_(gl3710_,588,gl3711_,INTVAL_(gl3712_));
      VFN_(gl3712_)(gl3710_,symtab__);
      gl3713_ = PATT_(self__,20);
      cache_dispatch_(gl3713_,1577,gl3714_,INTVAL_(gl3715_));
      gl353_ = PATT_(gl3713_,INTVAL_(gl3715_));
      if ((gl353_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2584_)));
      }
      else {
         gl3716_ = PATT_(self__,20);
         cache_dispatch_(gl3716_,1577,gl3717_,INTVAL_(gl3718_));
         gl354_ = PATT_(gl3716_,INTVAL_(gl3718_));
         if ((! GLO94_conform_tst_(0,gl354_,PATT_(self__,16),PATT_(self__,20)))) {
            gl3719_ = PATT_(self__,20);
            cache_dispatch_(gl3719_,1577,gl3720_,INTVAL_(gl3721_));
            gl355_ = PATT_(gl3719_,INTVAL_(gl3721_));
            ERR96_type_mismatch_err_(0,(ptr)(&ls2585_),(ptr)(&ls1016_),gl355_,PATT_(self__,16),IATT_(self__,4));
            new_type_spec__ = (ptr)copy_(GLO68_ob_typeob_s_,1);
            gl3725_ = PATT_(self__,16);
            cache_dispatch_(gl3725_,298,gl3726_,INTVAL_(gl3727_));
gl3728_ = IATT_(gl3725_,INTVAL_(gl3727_));
            gl3722_ = new_type_spec__;
            cache_dispatch_(gl3722_,298,gl3723_,INTVAL_(gl3724_));
            IATT_(gl3722_,INTVAL_(gl3724_)) = gl3728_;
            PATT_(self__,16) = (ptr)new_type_spec__;
         }
         else {
         }
      }
   }
   else {
   }
   gl3729_ = PATT_(self__,16);
   cache_dispatch_(gl3729_,418,gl3730_,INTVAL_(gl3731_));
   co__ = (ptr)PFN_(gl3731_)(gl3729_);
   if (CATT_(co__,14)) {
      CATT_(GLO68_curr_class_inst_,13) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

ptr LVA145_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,16);

   ret0__:
   return (res__);
}

int LVA145_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LVA145_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LVA145_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LVA145_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LVA145_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls1551_,"__");

   (void)SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,12)));
   if ((IATT_(self__,24) > 0)) {
      (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,'_'),IATT_(self__,24)),'_');
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls1551_));
   }

   ret0__:
   return;
}

LVA145_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LVA145_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   LVA145_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

LVA145_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char LVA145_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr LVA145_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3732_;
   static int gl3733_;
   static union dtype_ gl3734_;

   if ((PATT_(self__,20) != 0)) {
      gl3732_ = PATT_(self__,20);
      cache_dispatch_(gl3732_,1582,gl3733_,INTVAL_(gl3734_));
      res__ = (ptr)PFN_(gl3734_)(gl3732_);
   }
   else {
   }

   ret0__:
   return (res__);
}

LVA145_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{


   ret0__:
   return;
}

LVA145_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   SATHER_STR_(20,45,ls2587_,"(LVAR_DECL_STMTOB_S): Dispatch on basic type");
   ptr gl3735_;
   static int gl3736_;
   static union dtype_ gl3737_;
   ptr gl3738_;
   static int gl3739_;
   static union dtype_ gl3740_;
   ptr gl3741_;
   static int gl3742_;
   static union dtype_ gl3743_;

   gl3735_ = PATT_(self__,16);
   cache_dispatch_(gl3735_,1511,gl3736_,INTVAL_(gl3737_));
   if (CFN_(gl3737_)(gl3735_)) {
      gl3738_ = PATT_(self__,16);
      cache_dispatch_(gl3738_,1549,gl3739_,INTVAL_(gl3740_));
      if (CFN_(gl3740_)(gl3738_)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2587_)));
      }
      else {
      }
   }
   else {
   }
   if ((PATT_(self__,20) != 0)) {
      gl3741_ = PATT_(self__,20);
      cache_dispatch_(gl3741_,1677,gl3742_,INTVAL_(gl3743_));
      VFN_(gl3743_)(gl3741_);
   }
   else {
   }

   ret0__:
   return;
}

LVA145_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,6,ls2588_," = S_");
   SATHER_STR_(20,9,ls2589_,"_VOID_;\n");
   SATHER_STR_(20,5,ls1590_," = (");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl3744_;
   static int gl3745_;
   static union dtype_ gl3746_;
   ptr gl3747_;
   static int gl3748_;
   static union dtype_ gl3749_;
   ptr gl3750_;
   static int gl3751_;
   static union dtype_ gl3752_;
   ptr gl3753_;
   static int gl3754_;
   static union dtype_ gl3755_;

   if ((PATT_(self__,20) == 0)) {
      (void)SAT99_indent_(outfile__);
      LVA145_cprint_cname_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2588_));
      gl3744_ = PATT_(self__,16);
      cache_dispatch_(gl3744_,696,gl3745_,INTVAL_(gl3746_));
      VFN_(gl3746_)(gl3744_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2589_)),1);
   }
   else {
      gl3747_ = PATT_(self__,20);
      cache_dispatch_(gl3747_,1589,gl3748_,INTVAL_(gl3749_));
      VFN_(gl3749_)(gl3747_,outfile__);
      (void)SAT99_indent_(outfile__);
      if ((COM82_dbg_mode_ == 1)) {
         DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
      }
      else {
      }
      LVA145_cprint_cname_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls1590_));
      gl3750_ = PATT_(self__,16);
      cache_dispatch_(gl3750_,696,gl3751_,INTVAL_(gl3752_));
      VFN_(gl3752_)(gl3750_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
      gl3753_ = PATT_(self__,20);
      cache_dispatch_(gl3753_,965,gl3754_,INTVAL_(gl3755_));
      VFN_(gl3755_)(gl3753_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }

   ret0__:
   return;
}

char LVA145_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl3756_;
   static int gl3757_;
   static union dtype_ gl3758_;
   int gl356_;

   gl3756_ = PATT_(self__,16);
   cache_dispatch_(gl3756_,1547,gl3757_,INTVAL_(gl3758_));
   gl356_ = IFN_(gl3758_)(gl3756_);
   res__ = (char)(gl356_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int LVA145_declob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

LVA145_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   ptr gl3759_;
   static int gl3760_;
   static union dtype_ gl3761_;
   ptr gl3762_;
   static int gl3763_;
   static union dtype_ gl3764_;

   gl3759_ = PATT_(self__,16);
   cache_dispatch_(gl3759_,696,gl3760_,INTVAL_(gl3761_));
   VFN_(gl3761_)(gl3759_,outfile__);
   (void)SAT99_c_(outfile__,' ');
   (void)SAT99_indent_(outfile__);
   LVA145_cprint_cname_(self__,outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls964_));
   gl3762_ = PATT_(self__,16);
   cache_dispatch_(gl3762_,1591,gl3763_,INTVAL_(gl3764_));
   VFN_(gl3764_)(gl3762_,outfile__);

   ret0__:
   return;
}


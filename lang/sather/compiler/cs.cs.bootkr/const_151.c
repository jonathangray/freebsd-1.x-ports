/* const_151.c : Sather class: CONST_DECL_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern CLA148_cprint_init_shareds_and_consts_();
extern CLA148_cprint_pre_rtcode_();
extern ptr TYP149_dup_();
extern ptr STR20_c_();
extern ptr TYP149_sather_code_();
extern int TYP149_inst_ind_();
extern char TYP149_is_dispatched_();
extern char TYP149_nonptr_p_();
extern char TYP149_conforms_to_();
extern ptr CLA148_full_name_();
extern ptr CLA148_get_feature_();
extern ptr TYP149_inst_cls_();
extern CLA148_cprint_ctype_();
extern TYP149_resolve_predef_types_();
extern CON194_cprint_act_code_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern ptr CLA93_at_index_();
extern char GLO94_handle_feature_p_();
extern char GLO94_handle_class_p_();
extern ERR96_format_error_msg_();
extern /*constant*/ int RES97_C_ici_;
extern int ERR96_out_of_line_err_info_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern ptr SAT99_ind_init_();
extern ptr SAT99_inc_ln_();
extern ptr SAT99_indent_();
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern char GLO94_conform_tst_();
extern ERR96_format_error_exit_();
extern int STR109_temp_name_();
extern EXP117_cprint_init_code_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern ptr EXP117_dup_();
extern ptr EXP117_sather_code_();
extern ptr EXP117_expr_eval_constant_();
extern ptr EXP117_gen_temps_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern char EXP117_valid_init_expr_();
#include "macros_.h"



int CON151_get_offset_();
CON151_cprint_offset_();
ptr CON151_get_constval_();
CON151_cont_cprint_code_();
CON151_cprint_cname_();
CON151_cprint_extern_();
CON151_cprint_access_value_();
CON151_cprint_init_code_();
/*constant*/ int CON151_print_indent_ = 2;
ptr CON151_create_();
CON151_out_of_line_();
ptr CON151_dup_();
CON151_put_kwdname_();
ptr CON151_sather_code_();
int CON151_class_inst_ind_();
int CON151_featob_s_name_();
CON151_mark_private_();
CON151_mark_abstract_();
CON151_mark_spec_();
CON151_mark_shared_();
CON151_mark_readonly_();
char CON151_undefined_p_();
int CON151_compute_own_offset_();
CON151_eval_constant_();
ptr CON151_rettype_();
CON151_remember_local_();
CON151_semant_prologue_();
CON151_do_gen_temps_();
CON151_gen_goto_tags_();
CON151_validate_dispatches_and_get_ext_strs_();
char CON151_compatible_des_feat_();
CON151_update_used_in_dispatch_();
CON151_consistent_defs_();
CON151_cprint_decln_();
CON151_cprint_routine_();
CON151_cprint_store_dispval_();
int CON151_declob_s_name_();
CON151_cprint_code_();
ptr CON151_initialize_();
CON151_resolve_predef_types_();
CON151_semant_();
ptr CON151_typeof_();
int CON151_is_a_str_const_();
CON151_cprint_decln_with_poss_init_();
extern int attr_ent_CON151[];

int CON151_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

CON151_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr CON151_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (CATT_(self__,12)) {
      res__ = (ptr)PATT_(self__,52);
      goto ret0__;
   }
   else {
   }
   CON151_eval_constant_(self__);
   res__ = (ptr)PATT_(self__,52);

   ret0__:
   return (res__);
}

CON151_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

CON151_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   if ((IATT_(PATT_(self__,32),20) == RES97_C_ici_)) {
      (void)SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,16)));
   }
   else {
      (void)SAT99_c_(SAT99_s_(SAT99_c_(SAT99_s_(outfile__,PATT_(PATT_(self__,32),84)),'_'),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),'_');
   }

   ret0__:
   return;
}

CON151_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,8,ls647_,"extern ");
   SATHER_STR_(20,3,ls650_,";\n");
   int    old_ind__ = S_int_VOID_;

   if ((! CATT_(self__,12))) {
      CON151_eval_constant_(self__);
   }
   else {
   }
   (void)SAT99_s_(outfile__,(ptr)(&ls647_));
   old_ind__ = (int)IATT_(outfile__,12);
   (void)SAT99_ind_init_(outfile__);
   CON151_cprint_decln_(self__,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   IATT_(outfile__,12) = (int)old_ind__;

   ret0__:
   return;
}

CON151_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl4084_;
   static int gl4085_;
   static union dtype_ gl4086_;

   if ((CATT_(self__,12) & (PATT_(self__,52) != 0))) {
      gl4084_ = PATT_(self__,52);
      cache_dispatch_(gl4084_,965,gl4085_,INTVAL_(gl4086_));
      VFN_(gl4086_)(gl4084_,outfile__);
   }
   else {
      CON151_cprint_cname_(self__,outfile__);
   }

   ret0__:
   return;
}

CON151_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,94,ls1604_,"(CONST_DECL_FEATOB_S): Weak loop dependency for class initialization, unable to order classes");
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl4087_;
   static int gl4088_;
   static union dtype_ gl4089_;
   ptr gl4090_;
   static int gl4091_;
   static union dtype_ gl4092_;
   ptr gl4093_;
   static int gl4094_;
   static union dtype_ gl4095_;
   ptr    co__ = 0;

   if ((CATT_(self__,15) | (IATT_(PATT_(self__,32),20) == RES97_C_ici_))) {
      CATT_(self__,15) = (char)1;
      goto ret0__;
   }
   else {
      if ((GLO68_curr_class_inst_ != PATT_(self__,32))) {
         if (CATT_(PATT_(self__,32),7)) {
            ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1604_)));
            CATT_(self__,15) = (char)1;
            goto ret0__;
         }
         else {
            co__ = (ptr)GLO68_curr_class_inst_;
            GLO68_curr_class_inst_ = (ptr)PATT_(self__,32);
            CLA148_cprint_init_shareds_and_consts_(PATT_(self__,32),outfile__);
            GLO68_curr_class_inst_ = (ptr)co__;
            goto ret0__;
         }
      }
      else {
      }
   }
   CATT_(self__,15) = (char)1;
   if ((CATT_(self__,12) & (PATT_(self__,52) != 0))) {
   }
   else {
      gl4087_ = PATT_(self__,44);
      cache_dispatch_(gl4087_,724,gl4088_,INTVAL_(gl4089_));
      VFN_(gl4089_)(gl4087_,outfile__);
      if (GLO94_handle_feature_p_(0,self__)) {
         CLA148_cprint_pre_rtcode_(PATT_(self__,32),outfile__);
         gl4090_ = PATT_(self__,44);
         cache_dispatch_(gl4090_,1589,gl4091_,INTVAL_(gl4092_));
         VFN_(gl4092_)(gl4090_,outfile__);
         (void)SAT99_indent_(outfile__);
         CON151_cprint_cname_(self__,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         gl4093_ = PATT_(self__,44);
         cache_dispatch_(gl4093_,965,gl4094_,INTVAL_(gl4095_));
         VFN_(gl4095_)(gl4093_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
      else {
      }
   }

   ret0__:
   return;
}

ptr CON151_create_(self__,nm__,t__,e__,priv__,c_def__,ln__)
ptr self__;
int nm__;
ptr t__;
ptr e__;
char priv__;
ptr c_def__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(151,0);
   IATT_(res__,16) = (int)nm__;
   PATT_(res__,40) = (ptr)t__;
   PATT_(res__,44) = (ptr)e__;
   CATT_(res__,11) = (char)1;
   CATT_(res__,5) = (char)priv__;
   PATT_(res__,28) = (ptr)c_def__;
   PATT_(res__,32) = (ptr)GLO68_curr_class_inst_;
   IATT_(res__,20) = (int)ln__;

   ret0__:
   return (res__);
}

CON151_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

ptr CON151_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4096_;
   static int gl4097_;
   static union dtype_ gl4098_;
   ptr gl4099_;
   static int gl4100_;
   static union dtype_ gl4101_;
   ptr gl415_;
   ptr gl416_;
   ptr gl4102_;
   static int gl4103_;
   static union dtype_ gl4104_;
   ptr gl417_;

   if ((PATT_(self__,44) != 0)) {
      gl4096_ = PATT_(self__,40);
      cache_dispatch_(gl4096_,471,gl4097_,INTVAL_(gl4098_));
      gl415_ = PFN_(gl4098_)(gl4096_);
      gl4099_ = PATT_(self__,44);
      cache_dispatch_(gl4099_,471,gl4100_,INTVAL_(gl4101_));
      gl416_ = PFN_(gl4101_)(gl4099_);
      res__ = (ptr)CON151_create_(self__,IATT_(self__,16),gl415_,gl416_,CATT_(self__,5),PATT_(self__,28),IATT_(self__,20));
   }
   else {
      gl4102_ = PATT_(self__,40);
      cache_dispatch_(gl4102_,471,gl4103_,INTVAL_(gl4104_));
      gl417_ = PFN_(gl4104_)(gl4102_);
      res__ = (ptr)CON151_create_(self__,IATT_(self__,16),gl417_,0,CATT_(self__,5),PATT_(self__,28),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

CON151_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4105_;
   static int gl4106_;
   static union dtype_ gl4107_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4105_ = x__;
   cache_dispatch_(gl4105_,796,gl4106_,INTVAL_(gl4107_));
   IATT_(gl4105_,INTVAL_(gl4107_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON151_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,10,ls1594_,"constant ");
   SATHER_STR_(20,5,ls1574_," := ");
   ptr gl4108_;
   static int gl4109_;
   static union dtype_ gl4110_;
   ptr gl4111_;
   static int gl4112_;
   static union dtype_ gl4113_;
   ptr gl418_;
   ptr gl419_;

   gl4108_ = PATT_(self__,44);
   cache_dispatch_(gl4108_,801,gl4109_,INTVAL_(gl4110_));
   gl419_ = PFN_(gl4110_)(gl4108_);
   gl4111_ = PATT_(self__,40);
   cache_dispatch_(gl4111_,801,gl4112_,INTVAL_(gl4113_));
   gl418_ = PFN_(gl4113_)(gl4111_);
   res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1594_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),':'),gl418_),(ptr)(&ls1574_)),gl419_);

   ret0__:
   return (res__);
}

int CON151_class_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int CON151_featob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,16);

   ret0__:
   return (res__);
}

CON151_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

CON151_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

CON151_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

CON151_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

CON151_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char CON151_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl4114_;
   static int gl4115_;
   static union dtype_ gl4116_;
   int gl420_;

   gl4114_ = PATT_(self__,40);
   cache_dispatch_(gl4114_,1547,gl4115_,INTVAL_(gl4116_));
   gl420_ = IFN_(gl4116_)(gl4114_);
   res__ = (char)(gl420_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int CON151_compute_own_offset_(self__,nextloc__)
ptr self__;
int nextloc__;
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

CON151_eval_constant_(self__)
ptr self__;
{
   SATHER_STR_(20,71,ls1595_,"(CONST_DECL_FEATOB_S): Loop dependency for constant feature definition");
   ptr gl4117_;
   static int gl4118_;
   static union dtype_ gl4119_;

   if (CATT_(self__,12)) {
      goto ret0__;
   }
   else {
   }
   if (CATT_(self__,13)) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1595_)));
      goto ret0__;
   }
   else {
   }
   CATT_(self__,13) = (char)1;
   gl4117_ = PATT_(self__,44);
   cache_dispatch_(gl4117_,1596,gl4118_,INTVAL_(gl4119_));
   PATT_(self__,52) = (ptr)PFN_(gl4119_)(gl4117_);
   CATT_(self__,13) = (char)0;
   CATT_(self__,12) = (char)1;

   ret0__:
   return;
}

ptr CON151_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

CON151_remember_local_(self__,lvar__)
ptr self__;
ptr lvar__;
{


   ret0__:
   return;
}

CON151_semant_prologue_(self__)
ptr self__;
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

CON151_do_gen_temps_(self__)
ptr self__;
{
   ptr gl4120_;
   static int gl4121_;
   static union dtype_ gl4122_;

   if ((PATT_(self__,48) == 0)) {
      gl4120_ = PATT_(self__,44);
      cache_dispatch_(gl4120_,1582,gl4121_,INTVAL_(gl4122_));
      PATT_(self__,48) = (ptr)PFN_(gl4122_)(gl4120_);
   }
   else {
   }

   ret0__:
   return;
}

CON151_gen_goto_tags_(self__)
ptr self__;
{


   ret0__:
   return;
}

CON151_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   SATHER_STR_(20,35,ls1550_,"(DECLOB_S): Dispatch on basic type");
   ptr gl4123_;
   static int gl4124_;
   static union dtype_ gl4125_;
   ptr gl4126_;
   static int gl4127_;
   static union dtype_ gl4128_;

   gl4123_ = PATT_(self__,40);
   cache_dispatch_(gl4123_,1511,gl4124_,INTVAL_(gl4125_));
   if (CFN_(gl4125_)(gl4123_)) {
      gl4126_ = PATT_(self__,40);
      cache_dispatch_(gl4126_,1549,gl4127_,INTVAL_(gl4128_));
      if (CFN_(gl4128_)(gl4126_)) {
         ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1550_)));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

char CON151_compatible_des_feat_(self__,feat__,lval_p__)
ptr self__;
ptr feat__;
char lval_p__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,68,ls1597_,"(CONST_DECL_FEATOB_S): Inconsistent use of \"private\" for constant \"");
   SATHER_STR_(20,7,ls1562_,"\" in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl4129_;
   static int gl4130_;
   static union dtype_ gl4131_;

   gl4129_ = PATT_(feat__,40);
   cache_dispatch_(gl4129_,903,gl4130_,INTVAL_(gl4131_));
   res__ = (char)CFN_(gl4131_)(gl4129_,PATT_(self__,40));
   if ((CATT_(self__,5) != CATT_(feat__,5))) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1597_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),(ptr)(&ls1562_)),CLA148_full_name_(PATT_(self__,32))),(ptr)(&ls1481_)),CLA148_full_name_(PATT_(feat__,32))),(ptr)(&ls785_)));
   }
   else {
   }

   ret0__:
   return (res__);
}

CON151_update_used_in_dispatch_(self__)
ptr self__;
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

CON151_consistent_defs_(self__,nm__,lval_p__)
ptr self__;
int nm__;
char lval_p__;
{
   SATHER_STR_(20,22,ls2124_,"(FEATOB_S): Feature \"");
   SATHER_STR_(20,49,ls2125_,"\" has incompatible definitions for dispatch in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,52,ls2126_,"\" are different kinds of features for dispatch in \"");
   SATHER_STR_(20,42,ls2127_,"\" is missing for dispatch in descendent \"");
   SATHER_STR_(20,13,ls2128_,"\" of class \"");
   ptr gl4132_;
   static int gl4133_;
   static union dtype_ gl4134_;
   ptr gl4135_;
   int gl421_;
   ptr    co__ = 0;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   ptr    descendents__ = 0;
   int    index__ = S_int_VOID_;
   ptr    co1__ = 0;
   ptr    referrent__ = 0;
   ptr    feat__ = 0;

   if (((IATT_(self__,36) == 1) & (lval_p__ == 0))) {
      goto ret0__;
   }
   else {
      if (((IATT_(self__,36) == 2) & (lval_p__ == 1))) {
         goto ret0__;
      }
      else {
         if ((IATT_(self__,36) == 3)) {
            goto ret0__;
         }
         else {
         }
      }
   }
   if ((IATT_(self__,36) == 0)) {
      if (lval_p__) {
         IATT_(self__,36) = (int)2;
      }
      else {
         IATT_(self__,36) = (int)1;
      }
   }
   else {
      if ((IATT_(self__,36) == 1)) {
         if (lval_p__) {
            IATT_(self__,36) = (int)3;
         }
         else {
         }
      }
      else {
         if ((IATT_(self__,36) == 2)) {
            if ((! lval_p__)) {
               IATT_(self__,36) = (int)3;
            }
            else {
            }
         }
         else {
         }
      }
   }
   co__ = (ptr)PATT_(self__,32);
   i__ = (int)0;
   psz__ = S_int_VOID_;
   descendents__ = (ptr)PATT_(co__,76);
   if ((descendents__ != 0)) {
      psz__ = (int)IATT_(descendents__,12);
   }
   else {
   }
   CON151_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4136_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4132_ = referrent__;
               gl421_ = TYPE_(gl4132_);
               gl4135_ = self__;
               if ((gl421_ == (151))) {
                  feat__ = (ptr)referrent__;
                  CON151_update_used_in_dispatch_(feat__);
                  if ((! CON151_compatible_des_feat_(self__,feat__,lval_p__))) {
                     ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2125_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
                  }
                  else {
                  }
               }
               else {
                  ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2126_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2127_)),CLA148_full_name_(co1__)),(ptr)(&ls2128_)),CLA148_full_name_(co__)),(ptr)(&ls785_)));
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4136_: ;

   ret0__:
   return;
}

CON151_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,14,ls1603_,"/*constant*/ ");
   ptr gl4137_;
   static int gl4138_;
   static union dtype_ gl4139_;

   if ((! CATT_(self__,12))) {
      CON151_eval_constant_(self__);
   }
   else {
   }
   (void)SAT99_s_(outfile__,(ptr)(&ls1603_));
   gl4137_ = PATT_(self__,40);
   cache_dispatch_(gl4137_,418,gl4138_,INTVAL_(gl4139_));
   CLA148_cprint_ctype_(PFN_(gl4139_)(gl4137_),outfile__);
   (void)SAT99_c_(outfile__,' ');
   CON151_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

CON151_cprint_routine_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

CON151_cprint_store_dispval_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,7,ls1586_,"(int)&");

   (void)SAT99_s_(outfile__,(ptr)(&ls1586_));
   CON151_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

int CON151_declob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,16);

   ret0__:
   return (res__);
}

CON151_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr CON151_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

CON151_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl4140_;
   static int gl4141_;
   static union dtype_ gl4142_;
   ptr gl4143_;
   static int gl4144_;
   static union dtype_ gl4145_;

   gl4140_ = PATT_(self__,40);
   cache_dispatch_(gl4140_,522,gl4141_,INTVAL_(gl4142_));
   VFN_(gl4142_)(gl4140_,index__);
   gl4143_ = PATT_(self__,44);
   cache_dispatch_(gl4143_,522,gl4144_,INTVAL_(gl4145_));
   VFN_(gl4145_)(gl4143_,index__);

   ret0__:
   return;
}

CON151_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,62,ls1598_,"(CONST_DECL_FEATOB_S): Unknown type in the initial expression");
   SATHER_STR_(20,68,ls1599_,"(CONST_DECL_FEATOB_S): Type mismatch in constant feature definition");
   SATHER_STR_(20,82,ls1600_,"(CONST_DECL_FEATOB_S): Invalid reference found in constant feature initialization");
   ptr gl4146_;
   static int gl4147_;
   static union dtype_ gl4148_;
   ptr gl4149_;
   static int gl4150_;
   static union dtype_ gl4151_;
   ptr gl422_;
   ptr gl4152_;
   static int gl4153_;
   static union dtype_ gl4154_;
   ptr gl4155_;
   static int gl4156_;
   static union dtype_ gl4157_;
   ptr gl423_;
   ptr gl4158_;
   static int gl4159_;
   static union dtype_ gl4160_;
   char gl424_;
   ptr gl4161_;
   static int gl4162_;
   static union dtype_ gl4163_;
   ptr    co__ = 0;

   CON151_semant_prologue_(self__);
   gl4146_ = PATT_(self__,44);
   cache_dispatch_(gl4146_,588,gl4147_,INTVAL_(gl4148_));
   VFN_(gl4148_)(gl4146_,symtab__);
   gl4149_ = PATT_(self__,44);
   cache_dispatch_(gl4149_,1577,gl4150_,INTVAL_(gl4151_));
   gl422_ = PATT_(gl4149_,INTVAL_(gl4151_));
   if ((gl422_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1598_)));
      gl4152_ = PATT_(self__,44);
      cache_dispatch_(gl4152_,1577,gl4153_,INTVAL_(gl4154_));
      PATT_(gl4152_,INTVAL_(gl4154_)) = (ptr)GLO68_ob_typeob_s_;
   }
   else {
   }
   gl4155_ = PATT_(self__,44);
   cache_dispatch_(gl4155_,1577,gl4156_,INTVAL_(gl4157_));
   gl423_ = PATT_(gl4155_,INTVAL_(gl4157_));
   if ((! GLO94_conform_tst_(0,gl423_,PATT_(self__,40),PATT_(self__,44)))) {
      ERR96_format_error_exit_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1599_)));
   }
   else {
      gl4158_ = PATT_(self__,44);
      cache_dispatch_(gl4158_,1580,gl4159_,INTVAL_(gl4160_));
      gl424_ = CFN_(gl4160_)(gl4158_);
      if ((! gl424_)) {
         ERR96_format_error_exit_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1600_)));
      }
      else {
      }
   }
   gl4161_ = PATT_(self__,40);
   cache_dispatch_(gl4161_,418,gl4162_,INTVAL_(gl4163_));
   co__ = (ptr)PFN_(gl4163_)(gl4161_);
   if (CATT_(co__,14)) {
      CATT_(GLO68_curr_class_inst_,12) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

ptr CON151_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,40);

   ret0__:
   return (res__);
}

int CON151_is_a_str_const_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   ptr gl4164_;
   static int gl4165_;
   static union dtype_ gl4166_;
   int gl425_;
   ptr    str_const__ = 0;

   if ((! CATT_(self__,12))) {
      CON151_eval_constant_(self__);
   }
   else {
   }
   if (CATT_(self__,12)) {
      if ((PATT_(self__,52) != 0)) {
         gl4164_ = PATT_(self__,52);
         gl425_ = TYPE_(gl4164_);
         if ((gl425_ == 109)) {
            str_const__ = (ptr)PATT_(self__,52);
            res__ = (int)STR109_temp_name_(str_const__);
         }
         else {
         }
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

CON151_cprint_decln_with_poss_init_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,14,ls1603_,"/*constant*/ ");
   SATHER_STR_(20,4,ls964_," = ");
   ptr gl4167_;
   static int gl4168_;
   static union dtype_ gl4169_;
   ptr gl4170_;
   static int gl4171_;
   static union dtype_ gl4172_;

   if ((! CATT_(self__,12))) {
      CON151_eval_constant_(self__);
   }
   else {
   }
   (void)SAT99_s_(outfile__,(ptr)(&ls1603_));
   gl4167_ = PATT_(self__,40);
   cache_dispatch_(gl4167_,418,gl4168_,INTVAL_(gl4169_));
   CLA148_cprint_ctype_(PFN_(gl4169_)(gl4167_),outfile__);
   (void)SAT99_c_(outfile__,' ');
   CON151_cprint_cname_(self__,outfile__);
   if ((CATT_(self__,12) & (PATT_(self__,52) != 0))) {
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl4170_ = PATT_(self__,52);
      cache_dispatch_(gl4170_,965,gl4171_,INTVAL_(gl4172_));
      VFN_(gl4172_)(gl4170_,outfile__);
   }
   else {
   }

   ret0__:
   return;
}


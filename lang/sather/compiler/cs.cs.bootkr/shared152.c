/* shared152.c : Sather class: SHARED_DECL_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern CLA148_cprint_init_shareds_and_consts_();
extern CLA148_cprint_pre_rtcode_();
extern TYP149_cprint_ctype_();
extern TYP149_cprint_void_();
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
extern ERR96_type_mismatch_err_();
extern ERR96_format_error_exit_();
extern EXP117_cprint_init_code_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern ptr EXP117_dup_();
extern ptr EXP117_sather_code_();
extern ptr EXP117_gen_temps_();
extern EXP117_resolve_predef_types_();
extern EXP117_semant_();
extern char EXP117_valid_init_expr_();
#include "macros_.h"



ptr SHA152_typeof_();
int SHA152_get_offset_();
SHA152_cprint_offset_();
ptr SHA152_get_constval_();
SHA152_cont_cprint_code_();
SHA152_cprint_cname_();
SHA152_cprint_extern_();
SHA152_cprint_access_value_();
SHA152_cprint_init_code_();
/*constant*/ int SHA152_print_indent_ = 2;
ptr SHA152_create_();
SHA152_out_of_line_();
ptr SHA152_dup_();
SHA152_put_kwdname_();
ptr SHA152_sather_code_();
int SHA152_class_inst_ind_();
int SHA152_featob_s_name_();
SHA152_mark_private_();
SHA152_mark_abstract_();
SHA152_mark_spec_();
SHA152_mark_shared_();
SHA152_mark_readonly_();
char SHA152_undefined_p_();
int SHA152_compute_own_offset_();
SHA152_eval_constant_();
ptr SHA152_rettype_();
SHA152_remember_local_();
SHA152_semant_prologue_();
SHA152_do_gen_temps_();
SHA152_gen_goto_tags_();
SHA152_validate_dispatches_and_get_ext_strs_();
char SHA152_compatible_des_feat_();
SHA152_update_used_in_dispatch_();
SHA152_consistent_defs_();
SHA152_cprint_decln_();
SHA152_cprint_routine_();
SHA152_cprint_store_dispval_();
int SHA152_declob_s_name_();
SHA152_cprint_code_();
ptr SHA152_initialize_();
SHA152_resolve_predef_types_();
SHA152_semant_();
extern int attr_ent_SHA152[];

ptr SHA152_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,40);

   ret0__:
   return (res__);
}

int SHA152_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

SHA152_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr SHA152_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

SHA152_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

SHA152_cprint_cname_(self__,outfile__)
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

SHA152_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,8,ls647_,"extern ");
   SATHER_STR_(20,3,ls650_,";\n");
   int    old_ind__ = S_int_VOID_;

   (void)SAT99_s_(outfile__,(ptr)(&ls647_));
   old_ind__ = (int)IATT_(outfile__,12);
   (void)SAT99_ind_init_(outfile__);
   SHA152_cprint_decln_(self__,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   IATT_(outfile__,12) = (int)old_ind__;

   ret0__:
   return;
}

SHA152_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   SHA152_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

SHA152_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,70,ls1587_,"(SHARED_DECL_FEATOB_S): Loop dependency for shared feature definition");
   SATHER_STR_(20,95,ls1588_,"(SHARED_DECL_FEATOB_S): Weak loop dependency for class initialization, unable to order classes");
   SATHER_STR_(20,5,ls1590_," = (");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,4,ls964_," = ");
   ptr gl4173_;
   static int gl4174_;
   static union dtype_ gl4175_;
   ptr gl4176_;
   static int gl4177_;
   static union dtype_ gl4178_;
   ptr gl4179_;
   static int gl4180_;
   static union dtype_ gl4181_;
   ptr gl4182_;
   static int gl4183_;
   static union dtype_ gl4184_;
   ptr gl4185_;
   static int gl4186_;
   static union dtype_ gl4187_;
   ptr    co__ = 0;

   if ((CATT_(self__,13) & (GLO68_curr_class_inst_ != PATT_(self__,32)))) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1587_)));
      goto ret0__;
   }
   else {
   }
   if ((CATT_(self__,14) | (IATT_(PATT_(self__,32),20) == RES97_C_ici_))) {
      CATT_(self__,14) = (char)1;
      goto ret0__;
   }
   else {
      if ((GLO68_curr_class_inst_ != PATT_(self__,32))) {
         if (CATT_(PATT_(self__,32),7)) {
            ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1588_)));
            CATT_(self__,14) = (char)1;
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
   CATT_(self__,14) = (char)1;
   if ((PATT_(self__,44) != 0)) {
      CATT_(self__,13) = (char)1;
      gl4173_ = PATT_(self__,44);
      cache_dispatch_(gl4173_,724,gl4174_,INTVAL_(gl4175_));
      VFN_(gl4175_)(gl4173_,outfile__);
      if (GLO94_handle_feature_p_(0,self__)) {
         CLA148_cprint_pre_rtcode_(PATT_(self__,32),outfile__);
         gl4176_ = PATT_(self__,44);
         cache_dispatch_(gl4176_,1589,gl4177_,INTVAL_(gl4178_));
         VFN_(gl4178_)(gl4176_,outfile__);
         (void)SAT99_indent_(outfile__);
         SHA152_cprint_cname_(self__,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1590_));
         gl4179_ = PATT_(self__,40);
         cache_dispatch_(gl4179_,696,gl4180_,INTVAL_(gl4181_));
         VFN_(gl4181_)(gl4179_,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
         gl4182_ = PATT_(self__,44);
         cache_dispatch_(gl4182_,965,gl4183_,INTVAL_(gl4184_));
         VFN_(gl4184_)(gl4182_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
      else {
      }
      CATT_(self__,13) = (char)0;
   }
   else {
      if (GLO94_handle_feature_p_(0,self__)) {
         (void)SAT99_indent_(outfile__);
         SHA152_cprint_cname_(self__,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         gl4185_ = PATT_(self__,40);
         cache_dispatch_(gl4185_,1591,gl4186_,INTVAL_(gl4187_));
         VFN_(gl4187_)(gl4185_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
      else {
      }
   }

   ret0__:
   return;
}

ptr SHA152_create_(self__,nm__,t__,e__,priv__,c_def__,ln__)
ptr self__;
int nm__;
ptr t__;
ptr e__;
char priv__;
ptr c_def__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(152,0);
   IATT_(res__,16) = (int)nm__;
   PATT_(res__,40) = (ptr)t__;
   PATT_(res__,44) = (ptr)e__;
   CATT_(res__,11) = (char)(e__ != 0);
   CATT_(res__,5) = (char)priv__;
   PATT_(res__,28) = (ptr)c_def__;
   PATT_(res__,32) = (ptr)GLO68_curr_class_inst_;
   IATT_(res__,20) = (int)ln__;

   ret0__:
   return (res__);
}

SHA152_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

ptr SHA152_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4188_;
   static int gl4189_;
   static union dtype_ gl4190_;
   ptr gl4191_;
   static int gl4192_;
   static union dtype_ gl4193_;
   ptr gl426_;
   ptr gl427_;
   ptr gl4194_;
   static int gl4195_;
   static union dtype_ gl4196_;
   ptr gl428_;

   if ((PATT_(self__,44) != 0)) {
      gl4188_ = PATT_(self__,40);
      cache_dispatch_(gl4188_,471,gl4189_,INTVAL_(gl4190_));
      gl426_ = PFN_(gl4190_)(gl4188_);
      gl4191_ = PATT_(self__,44);
      cache_dispatch_(gl4191_,471,gl4192_,INTVAL_(gl4193_));
      gl427_ = PFN_(gl4193_)(gl4191_);
      res__ = (ptr)SHA152_create_(self__,IATT_(self__,16),gl426_,gl427_,CATT_(self__,5),PATT_(self__,28),IATT_(self__,20));
   }
   else {
      gl4194_ = PATT_(self__,40);
      cache_dispatch_(gl4194_,471,gl4195_,INTVAL_(gl4196_));
      gl428_ = PFN_(gl4196_)(gl4194_);
      res__ = (ptr)SHA152_create_(self__,IATT_(self__,16),gl428_,0,CATT_(self__,5),PATT_(self__,28),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

SHA152_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4197_;
   static int gl4198_;
   static union dtype_ gl4199_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4197_ = x__;
   cache_dispatch_(gl4197_,796,gl4198_,INTVAL_(gl4199_));
   IATT_(gl4197_,INTVAL_(gl4199_)) = (int)nm__;

   ret0__:
   return;
}

ptr SHA152_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,8,ls1573_,"shared ");
   SATHER_STR_(20,5,ls1574_," := ");
   ptr gl4200_;
   static int gl4201_;
   static union dtype_ gl4202_;
   ptr gl429_;
   ptr gl4203_;
   static int gl4204_;
   static union dtype_ gl4205_;
   ptr gl430_;

   gl4200_ = PATT_(self__,40);
   cache_dispatch_(gl4200_,801,gl4201_,INTVAL_(gl4202_));
   gl429_ = PFN_(gl4202_)(gl4200_);
   res__ = (ptr)STR20_s_(STR20_c_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1573_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),':'),gl429_);
   if ((PATT_(self__,44) != 0)) {
      gl4203_ = PATT_(self__,44);
      cache_dispatch_(gl4203_,801,gl4204_,INTVAL_(gl4205_));
      gl430_ = PFN_(gl4205_)(gl4203_);
      res__ = (ptr)STR20_s_(STR20_s_(res__,(ptr)(&ls1574_)),gl430_);
   }
   else {
      res__ = (ptr)STR20_c_(res__,';');
   }

   ret0__:
   return (res__);
}

int SHA152_class_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int SHA152_featob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,16);

   ret0__:
   return (res__);
}

SHA152_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

SHA152_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

SHA152_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

SHA152_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

SHA152_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char SHA152_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl4206_;
   static int gl4207_;
   static union dtype_ gl4208_;
   int gl431_;

   gl4206_ = PATT_(self__,40);
   cache_dispatch_(gl4206_,1547,gl4207_,INTVAL_(gl4208_));
   gl431_ = IFN_(gl4208_)(gl4206_);
   res__ = (char)(gl431_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int SHA152_compute_own_offset_(self__,nextloc__)
ptr self__;
int nextloc__;
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

SHA152_eval_constant_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr SHA152_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

SHA152_remember_local_(self__,lvar__)
ptr self__;
ptr lvar__;
{


   ret0__:
   return;
}

SHA152_semant_prologue_(self__)
ptr self__;
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

SHA152_do_gen_temps_(self__)
ptr self__;
{
   ptr gl4209_;
   static int gl4210_;
   static union dtype_ gl4211_;

   if (((PATT_(self__,44) != 0) & (PATT_(self__,48) == 0))) {
      gl4209_ = PATT_(self__,44);
      cache_dispatch_(gl4209_,1582,gl4210_,INTVAL_(gl4211_));
      PATT_(self__,48) = (ptr)PFN_(gl4211_)(gl4209_);
   }
   else {
   }

   ret0__:
   return;
}

SHA152_gen_goto_tags_(self__)
ptr self__;
{


   ret0__:
   return;
}

SHA152_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   SATHER_STR_(20,35,ls1550_,"(DECLOB_S): Dispatch on basic type");
   ptr gl4212_;
   static int gl4213_;
   static union dtype_ gl4214_;
   ptr gl4215_;
   static int gl4216_;
   static union dtype_ gl4217_;

   gl4212_ = PATT_(self__,40);
   cache_dispatch_(gl4212_,1511,gl4213_,INTVAL_(gl4214_));
   if (CFN_(gl4214_)(gl4212_)) {
      gl4215_ = PATT_(self__,40);
      cache_dispatch_(gl4215_,1549,gl4216_,INTVAL_(gl4217_));
      if (CFN_(gl4217_)(gl4215_)) {
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

char SHA152_compatible_des_feat_(self__,feat__,lval_p__)
ptr self__;
ptr feat__;
char lval_p__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,75,ls1575_,"(SHARED_DECL_FEATOB_S): Inconsistent use of \"private\" for shared feature \"");
   SATHER_STR_(20,7,ls1562_,"\" in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl4218_;
   static int gl4219_;
   static union dtype_ gl4220_;
   ptr gl4221_;
   static int gl4222_;
   static union dtype_ gl4223_;

   if (lval_p__) {
      gl4218_ = PATT_(self__,40);
      cache_dispatch_(gl4218_,903,gl4219_,INTVAL_(gl4220_));
      res__ = (char)CFN_(gl4220_)(gl4218_,PATT_(feat__,40));
   }
   else {
      gl4221_ = PATT_(feat__,40);
      cache_dispatch_(gl4221_,903,gl4222_,INTVAL_(gl4223_));
      res__ = (char)CFN_(gl4223_)(gl4221_,PATT_(self__,40));
   }
   if ((CATT_(self__,5) != CATT_(feat__,5))) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1575_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),(ptr)(&ls1562_)),CLA148_full_name_(PATT_(self__,32))),(ptr)(&ls1481_)),CLA148_full_name_(PATT_(feat__,32))),(ptr)(&ls785_)));
   }
   else {
   }

   ret0__:
   return (res__);
}

SHA152_update_used_in_dispatch_(self__)
ptr self__;
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

SHA152_consistent_defs_(self__,nm__,lval_p__)
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
   ptr gl4224_;
   static int gl4225_;
   static union dtype_ gl4226_;
   ptr gl4227_;
   int gl432_;
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
   SHA152_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4228_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4224_ = referrent__;
               gl432_ = TYPE_(gl4224_);
               gl4227_ = self__;
               if ((gl432_ == (152))) {
                  feat__ = (ptr)referrent__;
                  SHA152_update_used_in_dispatch_(feat__);
                  if ((! SHA152_compatible_des_feat_(self__,feat__,lval_p__))) {
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
goto_tag_4228_: ;

   ret0__:
   return;
}

SHA152_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,12,ls1585_,"/*shared*/ ");
   ptr gl4229_;
   static int gl4230_;
   static union dtype_ gl4231_;

   (void)SAT99_s_(outfile__,(ptr)(&ls1585_));
   gl4229_ = PATT_(self__,40);
   cache_dispatch_(gl4229_,418,gl4230_,INTVAL_(gl4231_));
   CLA148_cprint_ctype_(PFN_(gl4231_)(gl4229_),outfile__);
   (void)SAT99_c_(outfile__,' ');
   SHA152_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

SHA152_cprint_routine_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

SHA152_cprint_store_dispval_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,7,ls1586_,"(int)&");

   (void)SAT99_s_(outfile__,(ptr)(&ls1586_));
   SHA152_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

int SHA152_declob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,16);

   ret0__:
   return (res__);
}

SHA152_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr SHA152_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

SHA152_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl4232_;
   static int gl4233_;
   static union dtype_ gl4234_;
   ptr gl4235_;
   static int gl4236_;
   static union dtype_ gl4237_;

   gl4232_ = PATT_(self__,40);
   cache_dispatch_(gl4232_,522,gl4233_,INTVAL_(gl4234_));
   VFN_(gl4234_)(gl4232_,index__);
   if ((PATT_(self__,44) != 0)) {
      gl4235_ = PATT_(self__,44);
      cache_dispatch_(gl4235_,522,gl4236_,INTVAL_(gl4237_));
      VFN_(gl4237_)(gl4235_,index__);
   }
   else {
   }

   ret0__:
   return;
}

SHA152_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,63,ls1578_,"(SHARED_DECL_FEATOB_S): Unknown type in the initial expression");
   SATHER_STR_(20,15,ls1579_,"initialization");
   SATHER_STR_(20,1,ls1016_,"");
   SATHER_STR_(20,81,ls1581_,"(SHARED_DECL_FEATOB_S): Invalid reference found in shared feature initialization");
   ptr gl4238_;
   static int gl4239_;
   static union dtype_ gl4240_;
   ptr gl4241_;
   static int gl4242_;
   static union dtype_ gl4243_;
   ptr gl433_;
   ptr gl4244_;
   static int gl4245_;
   static union dtype_ gl4246_;
   ptr gl4247_;
   static int gl4248_;
   static union dtype_ gl4249_;
   ptr gl434_;
   ptr gl4250_;
   static int gl4251_;
   static union dtype_ gl4252_;
   ptr gl435_;
   ptr gl4253_;
   static int gl4254_;
   static union dtype_ gl4255_;
   char gl436_;
   ptr gl4256_;
   static int gl4257_;
   static union dtype_ gl4258_;
   ptr    co__ = 0;

   SHA152_semant_prologue_(self__);
   if ((PATT_(self__,44) != 0)) {
      gl4238_ = PATT_(self__,44);
      cache_dispatch_(gl4238_,588,gl4239_,INTVAL_(gl4240_));
      VFN_(gl4240_)(gl4238_,symtab__);
      gl4241_ = PATT_(self__,44);
      cache_dispatch_(gl4241_,1577,gl4242_,INTVAL_(gl4243_));
      gl433_ = PATT_(gl4241_,INTVAL_(gl4243_));
      if ((gl433_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1578_)));
         gl4244_ = PATT_(self__,44);
         cache_dispatch_(gl4244_,1577,gl4245_,INTVAL_(gl4246_));
         PATT_(gl4244_,INTVAL_(gl4246_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
      }
      gl4247_ = PATT_(self__,44);
      cache_dispatch_(gl4247_,1577,gl4248_,INTVAL_(gl4249_));
      gl434_ = PATT_(gl4247_,INTVAL_(gl4249_));
      if ((! GLO94_conform_tst_(0,gl434_,PATT_(self__,40),PATT_(self__,44)))) {
         gl4250_ = PATT_(self__,44);
         cache_dispatch_(gl4250_,1577,gl4251_,INTVAL_(gl4252_));
         gl435_ = PATT_(gl4250_,INTVAL_(gl4252_));
         ERR96_type_mismatch_err_(0,(ptr)(&ls1579_),(ptr)(&ls1016_),gl435_,PATT_(self__,40),IATT_(self__,20));
      }
      else {
      }
      gl4253_ = PATT_(self__,44);
      cache_dispatch_(gl4253_,1580,gl4254_,INTVAL_(gl4255_));
      gl436_ = CFN_(gl4255_)(gl4253_);
      if ((! gl436_)) {
         ERR96_format_error_exit_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls1581_)));
      }
      else {
      }
   }
   else {
   }
   gl4256_ = PATT_(self__,40);
   cache_dispatch_(gl4256_,418,gl4257_,INTVAL_(gl4258_));
   co__ = (ptr)PFN_(gl4258_)(gl4256_);
   if (CATT_(co__,14)) {
      CATT_(GLO68_curr_class_inst_,12) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}


/* rout_f156.c : Sather class: ROUT_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*constant*/ int GLO68_indent_step_;
extern /*shared*/ char COM82_k_and_r_c_;
extern /*shared*/ char COM82_dbg_mode_;
extern /*shared*/ char COM82_rt_code_check_;
extern ptr CLA93_at_index_();
extern char GLO94_handle_class_p_();
extern GLO94_cprint_ctemp_name_();
extern ERR96_format_error_msg_();
extern /*constant*/ int RES97_C_ici_;
extern ptr LIS98_create_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern ptr SAT99_ind_init_();
extern ptr SAT99_inc_ln_();
extern ptr LST102_create_();
extern ptr LST102_push_();
extern int ERR96_out_of_line_err_info_();
extern ptr LIS98_append_();
extern char LST102_conforms_to_();
extern ptr ERR96_def_filename_();
extern int ERR96_def_lineno_();
extern ptr SAT99_indent_();
extern STR109_cprint_mach_indep_();
extern GLO94_cprint_ctype_name_();
extern ptr SAT99_nl_();
extern int LIS98_contains_();
extern /*shared*/ char TYP115_init_expr_explicit_p_;
extern ptr LIS98_push_();
extern char GLO94_check_is_on_();
extern ptr SAT99_i_();
extern LST123_validate_dispatches_and_get_ext_strs_();
extern LST123_resolve_predef_types_();
extern LST123_semant_();
extern ptr LST123_dup_();
extern ptr LST123_gen_temps_();
extern LST123_put_kwdname_();
extern LST129_resolve_predef_types_();
extern LST129_semant_();
extern ptr LST129_create_();
extern ptr LST129_dup_();
extern ptr LST129_push_();
extern LST129_validate_dispatches_and_get_ext_strs_();
extern ASS135_resolve_predef_types_();
extern ASS135_semant_();
extern ptr ASS135_dup_();
extern ptr ASS135_gen_temps_();
extern LST129_cprint_code_();
extern LST129_cprint_names_();
extern ASS135_cprint_code_();
extern LST123_cprint_code_();
extern CLA148_cprint_init_shareds_and_consts_();
extern TYP149_resolve_predef_types_();
extern ptr TYP149_inst_cls_();
extern ptr TYP149_dup_();
extern ptr CLA148_full_name_();
extern ptr TYP149_paramstype_();
extern ptr TYP149_rettype_();
extern char INS150_conforms_to_();
extern ptr INS150_inst_cls_();
extern int INS150_inst_ind_();
extern ptr CLA148_get_feature_();
extern TYP149_cprint_ctype_();
extern CLA148_cprint_ctype_();
extern TYP149_cprint_void_();
extern ptr FEA162_rettype_();
extern char TYP149_conforms_to_();
extern char INT164_get_();
extern ptr LIS165_create_();
extern int TYP149_inst_ind_();
extern char TYP149_is_dispatched_();
extern ptr LIS165_push_();
extern /*constant*/ ptr CLA148_invar_feature_name_;
extern SYM186_enter_new_scope_();
extern SYM186_leave_new_scope_();
extern PRI187_cprint_update_exec_info1_();
extern PRI187_cprint_restore_exec_info_();
extern DBT190_addCLine_();
extern ptr STM199_gen_temps_();
extern STM199_gen_goto_tags_();
extern ptr ROU215_create_();
extern struct { int tp_; int sz_; char st_; } gs496_;
#include "macros_.h"



ROU156_resolve_predef_types_();
ROU156_semant_();
ptr ROU156_typeof_();
int ROU156_get_offset_();
ROU156_cprint_offset_();
ptr ROU156_get_constval_();
ROU156_cont_cprint_code_();
ROU156_cprint_cname_();
ROU156_cprint_extern_();
ROU156_cprint_access_value_();
ROU156_cprint_init_code_();
/*constant*/ int ROU156_print_indent_ = 2;
ptr ROU156_create_();
ROU156_out_of_line_();
ptr ROU156_dup_();
ROU156_put_kwdname_();
ptr ROU156_sather_code_();
int ROU156_class_inst_ind_();
int ROU156_featob_s_name_();
ROU156_mark_private_();
ROU156_mark_abstract_();
ROU156_mark_spec_();
ROU156_mark_shared_();
ROU156_mark_readonly_();
char ROU156_undefined_p_();
int ROU156_compute_own_offset_();
ROU156_eval_constant_();
ptr ROU156_rettype_();
ROU156_remember_local_();
ROU156_semant_prologue_();
ROU156_do_gen_temps_();
ROU156_gen_goto_tags_();
ROU156_validate_dispatches_and_get_ext_strs_();
char ROU156_compatible_des_feat_();
ROU156_update_used_in_dispatch_();
ROU156_consistent_defs_();
ROU156_cprint_decln_();
ROU156_cprint_routine_();
ROU156_cprint_store_dispval_();
ptr ROU156_initialize_();
char ROU156_param_conforms_to_();
ROU156_add_str_const_();
ROU156_cprint_rtcode_();
ROU156_cprint_post_rtcode_();
extern int attr_ent_ROU156[];

ROU156_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl4344_;
   static int gl4345_;
   static union dtype_ gl4346_;

   LST129_resolve_predef_types_(PATT_(self__,56),index__);
   if ((PATT_(self__,60) != 0)) {
      gl4344_ = PATT_(self__,60);
      cache_dispatch_(gl4344_,522,gl4345_,INTVAL_(gl4346_));
      VFN_(gl4346_)(gl4344_,index__);
   }
   else {
   }
   if ((PATT_(self__,64) != 0)) {
      LST123_resolve_predef_types_(PATT_(self__,64),index__);
   }
   else {
   }
   if ((PATT_(self__,48) != 0)) {
      LST123_resolve_predef_types_(PATT_(self__,48),index__);
   }
   else {
   }
   if ((PATT_(self__,40) != 0)) {
      ASS135_resolve_predef_types_(PATT_(self__,40),index__);
   }
   else {
   }
   if ((PATT_(self__,44) != 0)) {
      ASS135_resolve_predef_types_(PATT_(self__,44),index__);
   }
   else {
   }

   ret0__:
   return;
}

ROU156_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl4347_;
   static int gl4348_;
   static union dtype_ gl4349_;
   ptr    co__ = 0;

   ROU156_semant_prologue_(self__);
   SYM186_enter_new_scope_(symtab__);
   LST129_semant_(PATT_(self__,56),symtab__);
   LST123_semant_(PATT_(self__,64),symtab__);
   if ((PATT_(self__,48) != 0)) {
      LST123_semant_(PATT_(self__,48),symtab__);
   }
   else {
   }
   if ((PATT_(self__,40) != 0)) {
      ASS135_semant_(PATT_(self__,40),symtab__);
   }
   else {
   }
   if ((PATT_(self__,44) != 0)) {
      ASS135_semant_(PATT_(self__,44),symtab__);
   }
   else {
   }
   SYM186_leave_new_scope_(symtab__);
   if ((PATT_(self__,60) != 0)) {
      gl4347_ = PATT_(self__,60);
      cache_dispatch_(gl4347_,418,gl4348_,INTVAL_(gl4349_));
      co__ = (ptr)PFN_(gl4349_)(gl4347_);
      if (CATT_(co__,14)) {
         CATT_(GLO68_curr_class_inst_,12) = (char)1;
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

ptr ROU156_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,68);

   ret0__:
   return (res__);
}

int ROU156_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ROU156_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ROU156_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ROU156_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

ROU156_cprint_cname_(self__,outfile__)
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

ROU156_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,8,ls647_,"extern ");
   SATHER_STR_(20,3,ls650_,";\n");
   int    old_ind__ = S_int_VOID_;

   (void)SAT99_s_(outfile__,(ptr)(&ls647_));
   old_ind__ = (int)IATT_(outfile__,12);
   (void)SAT99_ind_init_(outfile__);
   ROU156_cprint_decln_(self__,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   IATT_(outfile__,12) = (int)old_ind__;

   ret0__:
   return;
}

ROU156_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ROU156_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,88,ls2172_,"(ROUT_FEATOB_S): Weak loop dependency for class initialization, unable to order classes");
   ptr    co__ = 0;

   if (TYP115_init_expr_explicit_p_) {
      if ((GLO68_curr_class_inst_ != PATT_(self__,32))) {
         if (CATT_(PATT_(self__,32),7)) {
            ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_create_(0),(ptr)(&ls2172_)));
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
   else {
   }

   ret0__:
   return;
}

ptr ROU156_create_(self__,nm__,precond__,postcond__,o__,pl__,rt__,sl__,priv__,c_def__,ln__,eln__)
ptr self__;
int nm__;
ptr precond__;
ptr postcond__;
ptr o__;
ptr pl__;
ptr rt__;
ptr sl__;
char priv__;
ptr c_def__;
int ln__;
int eln__;
{
   ptr res__ = 0;
   ptr gl4350_;
   static int gl4351_;
   static union dtype_ gl4352_;
   ptr gl447_;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   ptr    paramtype_lst__ = 0;

   res__ = (ptr)new_(156,0);
   IATT_(res__,16) = (int)nm__;
   if ((pl__ == 0)) {
      pl__ = (ptr)LST129_create_(0,1);
   }
   else {
   }
   PATT_(res__,56) = (ptr)pl__;
   PATT_(res__,60) = (ptr)rt__;
   PATT_(res__,64) = (ptr)sl__;
   PATT_(res__,40) = (ptr)precond__;
   PATT_(res__,44) = (ptr)postcond__;
   PATT_(res__,48) = (ptr)o__;
   CATT_(res__,5) = (char)priv__;
   PATT_(res__,28) = (ptr)c_def__;
   PATT_(res__,32) = (ptr)GLO68_curr_class_inst_;
   PATT_(res__,72) = (ptr)LST129_create_(0,3);
   IATT_(res__,20) = (int)ln__;
   IATT_(res__,52) = (int)eln__;
   PATT_(res__,80) = (ptr)LIS165_create_(0,3);
   PATT_(res__,84) = (ptr)LIS98_create_(0,3);
   i__ = (int)0;
   psz__ = S_int_VOID_;
   if ((pl__ != 0)) {
      psz__ = (int)IATT_(pl__,12);
   }
   else {
   }
   if ((psz__ > 0)) {
      paramtype_lst__ = (ptr)LST102_create_(0,psz__);
      while (1) {
         if ((i__ >= psz__)) {
            goto goto_tag_4353_;
         }
         else {
         }
         gl4350_ = PATT_(pl__, 32 + ((i__) << 2));
         cache_dispatch_(gl4350_,676,gl4351_,INTVAL_(gl4352_));
         gl447_ = PATT_(gl4350_,INTVAL_(gl4352_));
         paramtype_lst__ = (ptr)LST102_push_(paramtype_lst__,gl447_);
         i__ = (int)(i__ + 1);
      }
   goto_tag_4353_: ;
      PATT_(res__,68) = (ptr)ROU215_create_(0,paramtype_lst__,rt__,ln__);
   }
   else {
      PATT_(res__,68) = (ptr)ROU215_create_(0,0,rt__,ln__);
   }

   ret0__:
   return (res__);
}

ROU156_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

ptr ROU156_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4354_;
   static int gl4355_;
   static union dtype_ gl4356_;
   ptr    new_plist__ = 0;
   ptr    new_rout_rettype__ = 0;
   ptr    new_slist__ = 0;
   ptr    new_pre__ = 0;
   ptr    new_post__ = 0;
   ptr    new_old__ = 0;

   new_plist__ = S_ptr_VOID_;
   if ((PATT_(self__,56) != 0)) {
      new_plist__ = (ptr)LST129_dup_(PATT_(self__,56));
   }
   else {
   }
   new_rout_rettype__ = S_ptr_VOID_;
   if ((PATT_(self__,60) != 0)) {
      gl4354_ = PATT_(self__,60);
      cache_dispatch_(gl4354_,471,gl4355_,INTVAL_(gl4356_));
      new_rout_rettype__ = (ptr)PFN_(gl4356_)(gl4354_);
   }
   else {
   }
   new_slist__ = S_ptr_VOID_;
   if ((PATT_(self__,64) != 0)) {
      new_slist__ = (ptr)LST123_dup_(PATT_(self__,64));
   }
   else {
   }
   new_pre__ = S_ptr_VOID_;
   new_post__ = S_ptr_VOID_;
   if ((PATT_(self__,40) != 0)) {
      new_pre__ = (ptr)ASS135_dup_(PATT_(self__,40));
   }
   else {
   }
   if ((PATT_(self__,44) != 0)) {
      new_post__ = (ptr)ASS135_dup_(PATT_(self__,44));
   }
   else {
   }
   new_old__ = S_ptr_VOID_;
   if ((PATT_(self__,48) != 0)) {
      new_old__ = (ptr)LST123_dup_(PATT_(self__,48));
   }
   else {
   }
   res__ = (ptr)ROU156_create_(self__,IATT_(self__,16),new_pre__,new_post__,new_old__,new_plist__,new_rout_rettype__,new_slist__,CATT_(self__,5),PATT_(self__,28),IATT_(self__,20),IATT_(self__,52));
   if (CATT_(self__,4)) {
      ROU156_mark_abstract_(res__);
   }
   else {
   }
   if (CATT_(self__,6)) {
      ROU156_mark_spec_(res__);
   }
   else {
   }
   if (CATT_(self__,7)) {
      ROU156_mark_shared_(res__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ROU156_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4357_;
   static int gl4358_;
   static union dtype_ gl4359_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4357_ = x__;
   cache_dispatch_(gl4357_,796,gl4358_,INTVAL_(gl4359_));
   IATT_(gl4357_,INTVAL_(gl4359_)) = (int)nm__;

   ret0__:
   return;
}

ptr ROU156_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ROU156_class_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int ROU156_featob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,16);

   ret0__:
   return (res__);
}

ROU156_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

ROU156_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

ROU156_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

ROU156_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

ROU156_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char ROU156_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

int ROU156_compute_own_offset_(self__,nextloc__)
ptr self__;
int nextloc__;
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

ROU156_eval_constant_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr ROU156_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,60);

   ret0__:
   return (res__);
}

ROU156_remember_local_(self__,lvar__)
ptr self__;
ptr lvar__;
{

   PATT_(self__,72) = (ptr)LST129_push_(PATT_(self__,72),lvar__);

   ret0__:
   return;
}

ROU156_semant_prologue_(self__)
ptr self__;
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

ROU156_do_gen_temps_(self__)
ptr self__;
{
   ptr gl4360_;
   static int gl4361_;
   static union dtype_ gl4362_;
   ptr gl448_;
   int    i__ = S_int_VOID_;
   int    ssz__ = S_int_VOID_;

   PATT_(self__,76) = (ptr)LIS98_create_(PATT_(self__,76),9);
   if ((PATT_(self__,64) != 0)) {
      i__ = (int)0;
      ssz__ = (int)IATT_(PATT_(self__,64),12);
      while (1) {
         if ((i__ >= ssz__)) {
            goto goto_tag_4363_;
         }
         else {
         }
         gl4360_ = PATT_(PATT_(self__,64), 28 + ((i__) << 2));
         cache_dispatch_(gl4360_,1582,gl4361_,INTVAL_(gl4362_));
         gl448_ = PFN_(gl4362_)(gl4360_);
         PATT_(self__,76) = (ptr)LIS98_append_(PATT_(self__,76),gl448_);
         i__ = (int)(i__ + 1);
      }
   goto_tag_4363_: ;
   }
   else {
   }
   if ((PATT_(self__,48) != 0)) {
      PATT_(self__,76) = (ptr)LIS98_append_(PATT_(self__,76),LST123_gen_temps_(PATT_(self__,48)));
   }
   else {
   }
   if ((PATT_(self__,40) != 0)) {
      PATT_(self__,76) = (ptr)LIS98_append_(PATT_(self__,76),ASS135_gen_temps_(PATT_(self__,40)));
   }
   else {
   }
   if ((PATT_(self__,44) != 0)) {
      PATT_(self__,76) = (ptr)LIS98_append_(PATT_(self__,76),ASS135_gen_temps_(PATT_(self__,44)));
   }
   else {
   }

   ret0__:
   return;
}

ROU156_gen_goto_tags_(self__)
ptr self__;
{
   ptr gl4364_;
   static int gl4365_;
   static union dtype_ gl4366_;
   ptr gl449_;
   int    i__ = S_int_VOID_;
   int    ssz__ = S_int_VOID_;

   if ((PATT_(self__,64) != 0)) {
      LST123_put_kwdname_(PATT_(self__,64),IATT_(self__,16));
      i__ = (int)0;
      ssz__ = (int)IATT_(PATT_(self__,64),12);
      while (1) {
         if ((i__ >= ssz__)) {
            goto goto_tag_4367_;
         }
         else {
         }
         gl449_ = 0;
         gl4364_ = PATT_(PATT_(self__,64), 28 + ((i__) << 2));
         cache_dispatch_(gl4364_,607,gl4365_,INTVAL_(gl4366_));
         VFN_(gl4366_)(gl4364_,gl449_);
         i__ = (int)(i__ + 1);
      }
   goto_tag_4367_: ;
   }
   else {
   }

   ret0__:
   return;
}

ROU156_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{

   LST129_validate_dispatches_and_get_ext_strs_(PATT_(self__,56));
   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,64));

   ret0__:
   return;
}

char ROU156_compatible_des_feat_(self__,feat__,lval_p__)
ptr self__;
ptr feat__;
char lval_p__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,61,ls2143_,"(ROUT_FEATOB_S): Inconsistent use of \"private\" for routine \"");
   SATHER_STR_(20,7,ls1562_,"\" in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl4368_;
   static int gl4369_;
   static union dtype_ gl4370_;
   ptr gl4371_;
   static int gl4372_;
   static union dtype_ gl4373_;
   ptr gl450_;
   ptr gl4374_;
   static int gl4375_;
   static union dtype_ gl4376_;
   ptr gl451_;
   ptr gl4377_;
   static int gl4378_;
   static union dtype_ gl4379_;
   ptr gl452_;
   ptr gl4380_;
   static int gl4381_;
   static union dtype_ gl4382_;
   ptr gl453_;
   ptr gl4383_;
   static int gl4384_;
   static union dtype_ gl4385_;
   ptr gl4386_;
   static int gl4387_;
   static union dtype_ gl4388_;
   ptr gl454_;
   ptr gl4389_;
   static int gl4390_;
   static union dtype_ gl4391_;
   ptr gl4392_;
   static int gl4393_;
   static union dtype_ gl4394_;
   ptr gl4395_;
   static int gl4396_;
   static union dtype_ gl4397_;
   ptr gl455_;
   ptr gl4398_;
   static int gl4399_;
   static union dtype_ gl4400_;
   ptr gl456_;
   ptr gl4401_;
   static int gl4402_;
   static union dtype_ gl4403_;
   ptr gl4404_;
   static int gl4405_;
   static union dtype_ gl4406_;
   ptr gl457_;
   ptr gl458_;
   ptr    tp__ = 0;
   ptr    selftp__ = 0;

   if ((CATT_(self__,5) != CATT_(feat__,5))) {
      ERR96_format_error_msg_(0,IATT_(self__,20),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2143_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16))),(ptr)(&ls1562_)),CLA148_full_name_(PATT_(self__,32))),(ptr)(&ls1481_)),CLA148_full_name_(PATT_(feat__,32))),(ptr)(&ls785_)));
   }
   else {
   }
   tp__ = (ptr)PATT_(feat__,68);
   selftp__ = (ptr)PATT_(self__,68);
   if ((tp__ == 0)) {
      if ((selftp__ == 0)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      gl4368_ = selftp__;
      cache_dispatch_(gl4368_,1709,gl4369_,INTVAL_(gl4370_));
      res__ = (char)LST102_conforms_to_(PFN_(gl4370_)(gl4368_),0);
      if (res__) {
         gl4371_ = selftp__;
         cache_dispatch_(gl4371_,1705,gl4372_,INTVAL_(gl4373_));
         gl450_ = PFN_(gl4373_)(gl4371_);
         if ((gl450_ != 0)) {
            res__ = (char)0;
         }
         else {
         }
      }
      else {
      }
   }
   else {
      gl4374_ = tp__;
      cache_dispatch_(gl4374_,1705,gl4375_,INTVAL_(gl4376_));
      gl451_ = PFN_(gl4376_)(gl4374_);
      if ((gl451_ == 0)) {
         gl4377_ = selftp__;
         cache_dispatch_(gl4377_,1705,gl4378_,INTVAL_(gl4379_));
         gl452_ = PFN_(gl4379_)(gl4377_);
         if ((gl452_ == 0)) {
            res__ = (char)1;
         }
         else {
            res__ = (char)0;
         }
      }
      else {
         gl4380_ = selftp__;
         cache_dispatch_(gl4380_,1705,gl4381_,INTVAL_(gl4382_));
         gl453_ = PFN_(gl4382_)(gl4380_);
         if ((gl453_ == 0)) {
            res__ = (char)0;
         }
         else {
            gl4383_ = selftp__;
            cache_dispatch_(gl4383_,1705,gl4384_,INTVAL_(gl4385_));
            gl454_ = PFN_(gl4385_)(gl4383_);
            gl4386_ = tp__;
            cache_dispatch_(gl4386_,1705,gl4387_,INTVAL_(gl4388_));
            gl4389_ = tp__;
            cache_dispatch_(gl4389_,1705,gl4390_,INTVAL_(gl4391_));
            gl4392_ = selftp__;
            cache_dispatch_(gl4392_,1705,gl4393_,INTVAL_(gl4394_));
            res__ = (char)(INS150_conforms_to_(PFN_(gl4388_)(gl4386_),gl454_) | INT164_get_(PATT_(INS150_inst_cls_(PFN_(gl4394_)(gl4392_)),76),INS150_inst_ind_(PFN_(gl4391_)(gl4389_))));
         }
      }
      if (res__) {
         gl4395_ = selftp__;
         cache_dispatch_(gl4395_,1709,gl4396_,INTVAL_(gl4397_));
         gl455_ = PFN_(gl4397_)(gl4395_);
         if ((gl455_ == 0)) {
            gl4398_ = tp__;
            cache_dispatch_(gl4398_,1709,gl4399_,INTVAL_(gl4400_));
            gl456_ = PFN_(gl4400_)(gl4398_);
            if ((gl456_ != 0)) {
               res__ = (char)0;
            }
            else {
            }
         }
         else {
            gl4401_ = selftp__;
            cache_dispatch_(gl4401_,1709,gl4402_,INTVAL_(gl4403_));
            gl457_ = PFN_(gl4403_)(gl4401_);
            gl4404_ = tp__;
            cache_dispatch_(gl4404_,1709,gl4405_,INTVAL_(gl4406_));
            gl458_ = PFN_(gl4406_)(gl4404_);
            res__ = (char)ROU156_param_conforms_to_(self__,gl457_,gl458_);
         }
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

ROU156_update_used_in_dispatch_(self__)
ptr self__;
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

ROU156_consistent_defs_(self__,nm__,lval_p__)
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
   ptr gl4407_;
   static int gl4408_;
   static union dtype_ gl4409_;
   ptr gl4410_;
   int gl459_;
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
   ROU156_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4411_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4407_ = referrent__;
               gl459_ = TYPE_(gl4407_);
               gl4410_ = self__;
               if ((gl459_ == (156))) {
                  feat__ = (ptr)referrent__;
                  ROU156_update_used_in_dispatch_(feat__);
                  if ((! ROU156_compatible_des_feat_(self__,feat__,lval_p__))) {
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
goto_tag_4411_: ;

   ret0__:
   return;
}

ROU156_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,6,ls2150_,"void ");
   SATHER_STR_(20,3,ls624_,", ");
   SATHER_STR_(20,1,ls1016_,"");
   SATHER_STR_(20,7,ls970_,"self__");
   ptr gl4412_;
   static int gl4413_;
   static union dtype_ gl4414_;

   if ((PATT_(self__,60) != 0)) {
      gl4412_ = PATT_(self__,60);
      cache_dispatch_(gl4412_,696,gl4413_,INTVAL_(gl4414_));
      VFN_(gl4414_)(gl4412_,outfile__);
      (void)SAT99_c_(outfile__,' ');
   }
   else {
      if ((! COM82_k_and_r_c_)) {
         (void)SAT99_s_(outfile__,(ptr)(&ls2150_));
      }
      else {
      }
   }
   ROU156_cprint_cname_(self__,outfile__);
   (void)SAT99_c_(outfile__,'(');
   if ((! COM82_k_and_r_c_)) {
      PATT_(PATT_(self__,56),20) = (ptr)(ptr)(&ls624_);
      PATT_(PATT_(self__,56),24) = (ptr)(ptr)(&ls1016_);
      if ((IATT_(PATT_(self__,32),20) != RES97_C_ici_)) {
         CLA148_cprint_ctype_(PATT_(self__,32),outfile__);
         (void)SAT99_c_(outfile__,' ');
         (void)SAT99_s_(outfile__,(ptr)(&ls970_));
         if ((IATT_(PATT_(self__,56),12) > 0)) {
            (void)SAT99_s_(outfile__,PATT_(PATT_(self__,56),20));
         }
         else {
         }
      }
      else {
      }
      if ((IATT_(PATT_(self__,56),12) > 0)) {
         LST129_cprint_code_(PATT_(self__,56),outfile__);
      }
      else {
      }
   }
   else {
   }
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

ROU156_cprint_routine_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,8,ls2160_,"(self__");
   SATHER_STR_(20,3,ls1051_,")\n");
   SATHER_STR_(20,9,ls2162_,"self__;\n");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,3,ls2163_,"{\n");
   SATHER_STR_(20,9,ls2164_,"res__ = ");
   SATHER_STR_(20,12,ls2165_,"static int ");
   SATHER_STR_(20,21,ls2166_,"static union dtype_ ");
   SATHER_STR_(20,9,ls2167_,"ret0__:\n");
   SATHER_STR_(20,9,ls2168_,"return;\n");
   SATHER_STR_(20,17,ls2169_,"return (res__);\n");
   SATHER_STR_(20,4,ls2170_,"}\n\n");
   ptr gl4415_;
   static int gl4416_;
   static union dtype_ gl4417_;
   ptr gl4418_;
   static int gl4419_;
   static union dtype_ gl4420_;
   ptr gl4421_;
   static int gl4422_;
   static union dtype_ gl4423_;
   ptr gl4424_;
   static int gl4425_;
   static union dtype_ gl4426_;
   ptr gl461_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    i_460_ = S_int_VOID_;
   int    tsz__ = S_int_VOID_;

   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,20)),ERR96_def_lineno_(0,IATT_(self__,20)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   if (COM82_k_and_r_c_) {
      if ((PATT_(self__,60) != 0)) {
         gl4415_ = PATT_(self__,60);
         cache_dispatch_(gl4415_,696,gl4416_,INTVAL_(gl4417_));
         VFN_(gl4417_)(gl4415_,outfile__);
         (void)SAT99_c_(outfile__,' ');
      }
      else {
      }
      ROU156_cprint_cname_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2160_));
      if ((IATT_(PATT_(self__,56),12) > 0)) {
         (void)SAT99_c_(outfile__,',');
         LST129_cprint_names_(PATT_(self__,56),outfile__);
      }
      else {
      }
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls1051_)),1);
      CLA148_cprint_ctype_(PATT_(self__,32),outfile__);
      (void)SAT99_c_(outfile__,' ');
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2162_)),1);
      PATT_(PATT_(self__,56),20) = (ptr)(ptr)(&ls650_);
      PATT_(PATT_(self__,56),24) = (ptr)(ptr)(&ls650_);
      LST129_cprint_code_(PATT_(self__,56),outfile__);
   }
   else {
      ROU156_cprint_decln_(self__,outfile__);
      (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
   }
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2163_)),1);
   IATT_(outfile__,12) = (int)3;
   if ((PATT_(self__,60) != 0)) {
      (void)SAT99_indent_(outfile__);
      gl4418_ = PATT_(self__,60);
      cache_dispatch_(gl4418_,696,gl4419_,INTVAL_(gl4420_));
      VFN_(gl4420_)(gl4418_,outfile__);
      (void)SAT99_c_(outfile__,' ');
      (void)SAT99_s_(outfile__,(ptr)(&ls2164_));
      gl4421_ = PATT_(self__,60);
      cache_dispatch_(gl4421_,1591,gl4422_,INTVAL_(gl4423_));
      VFN_(gl4423_)(gl4421_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }
   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,80),4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_4427_;
      }
      else {
      }
      STR109_cprint_mach_indep_(PATT_(PATT_(self__,80), 16 + ((i__) << 2)),outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_4427_: ;
   if ((PATT_(self__,76) != 0)) {
      i_460_ = (int)0;
      tsz__ = S_int_VOID_;
      if ((PATT_(self__,76) != 0)) {
         tsz__ = (int)IATT_(PATT_(self__,76),4);
      }
      else {
      }
      while (1) {
         if ((i_460_ >= tsz__)) {
            goto goto_tag_4428_;
         }
         else {
         }
         (void)SAT99_indent_(outfile__);
         if ((IATT_(PATT_(self__,76), 16 + ((i_460_) << 2)) < 0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2165_));
            GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,76), 16 + ((i_460_) << 2)),outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls650_));
            (void)SAT99_indent_(outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls2166_));
            GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,76), 16 + (((i_460_ + 2)) << 2)),outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls650_));
            (void)SAT99_inc_ln_(outfile__,2);
            i_460_ = (int)(i_460_ + 4);
         }
         else {
            GLO94_cprint_ctype_name_(0,IATT_(PATT_(self__,76), 16 + (((i_460_ + 1)) << 2)),outfile__);
            (void)SAT99_c_(outfile__,' ');
            GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,76), 16 + ((i_460_) << 2)),outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
            i_460_ = (int)(i_460_ + 2);
         }
      }
   goto_tag_4428_: ;
   }
   else {
   }
   PATT_(PATT_(self__,72),20) = (ptr)(ptr)(&ls650_);
   PATT_(PATT_(self__,56),24) = (ptr)(ptr)(&ls650_);
   LST129_cprint_code_(PATT_(self__,72),outfile__);
   ROU156_cprint_rtcode_(self__,outfile__);
   if (((COM82_rt_code_check_ & (PATT_(self__,40) != 0)) & (! CATT_(self__,6)))) {
      ASS135_cprint_code_(PATT_(self__,40),outfile__);
   }
   else {
   }
   if ((PATT_(self__,64) != 0)) {
      (void)SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1);
      LST123_cprint_code_(PATT_(self__,64),outfile__);
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_nl_(outfile__),1);
   (void)SAT99_indent_(outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2167_)),1);
   if ((! CATT_(self__,6))) {
      ROU156_cprint_post_rtcode_(self__,outfile__);
   }
   else {
   }
   (void)SAT99_indent_(outfile__);
   gl4424_ = GLO68_curr_feature_;
   cache_dispatch_(gl4424_,1705,gl4425_,INTVAL_(gl4426_));
   gl461_ = PFN_(gl4426_)(gl4424_);
   if ((gl461_ == 0)) {
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2168_)),1);
   }
   else {
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2169_)),1);
   }
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,20)),ERR96_def_lineno_(0,IATT_(self__,52)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2170_)),2);
   IATT_(outfile__,12) = (int)0;

   ret0__:
   return;
}

ROU156_cprint_store_dispval_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,6,ls2171_,"(int)");

   (void)SAT99_s_(outfile__,(ptr)(&ls2171_));
   ROU156_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

ptr ROU156_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char ROU156_param_conforms_to_(self__,par__,ch__)
ptr self__;
ptr par__;
ptr ch__;
{
   char res__ = S_char_VOID_;
   ptr gl4429_;
   static int gl4430_;
   static union dtype_ gl4431_;
   char gl462_;
   ptr gl4432_;
   static int gl4433_;
   static union dtype_ gl4434_;
   ptr gl4435_;
   static int gl4436_;
   static union dtype_ gl4437_;
   int gl463_;
   int gl464_;
   ptr gl4438_;
   static int gl4439_;
   static union dtype_ gl4440_;
   int gl465_;
   ptr gl4441_;
   static int gl4442_;
   static union dtype_ gl4443_;
   int gl466_;
   ptr gl4444_;
   static int gl4445_;
   static union dtype_ gl4446_;
   ptr gl4447_;
   static int gl4448_;
   static union dtype_ gl4449_;
   char gl467_;
   char gl468_;
   int    n__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    ch_co__ = 0;

   if ((ch__ == 0)) {
      if ((par__ == 0)) {
         res__ = (char)1;
      }
      else {
         if ((IATT_(par__,16) == 0)) {
            res__ = (char)1;
         }
         else {
         }
      }
      goto ret0__;
   }
   else {
   }
   res__ = (char)1;
   n__ = (int)IATT_(par__,16);
   if ((IATT_(ch__,16) != IATT_(par__,16))) {
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= n__)) {
         goto goto_tag_4450_;
      }
      else {
      }
      gl4429_ = PATT_(par__, 28 + ((i__) << 2));
      cache_dispatch_(gl4429_,903,gl4430_,INTVAL_(gl4431_));
      gl462_ = CFN_(gl4431_)(gl4429_,PATT_(ch__, 28 + ((i__) << 2)));
      if ((! gl462_)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      gl4432_ = PATT_(par__, 28 + ((i__) << 2));
      cache_dispatch_(gl4432_,1547,gl4433_,INTVAL_(gl4434_));
      gl463_ = IFN_(gl4434_)(gl4432_);
      gl4435_ = PATT_(ch__, 28 + ((i__) << 2));
      cache_dispatch_(gl4435_,1547,gl4436_,INTVAL_(gl4437_));
      gl464_ = IFN_(gl4437_)(gl4435_);
      if ((gl463_ != gl464_)) {
         gl4438_ = PATT_(ch__, 28 + ((i__) << 2));
         cache_dispatch_(gl4438_,1547,gl4439_,INTVAL_(gl4440_));
         gl465_ = IFN_(gl4440_)(gl4438_);
         ch_co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,gl465_);
         gl4441_ = PATT_(par__, 28 + ((i__) << 2));
         cache_dispatch_(gl4441_,1547,gl4442_,INTVAL_(gl4443_));
         gl466_ = IFN_(gl4443_)(gl4441_);
         if ((! INT164_get_(PATT_(ch_co__,76),gl466_))) {
            res__ = (char)0;
            goto ret0__;
         }
         else {
         }
      }
      else {
         gl4444_ = PATT_(par__, 28 + ((i__) << 2));
         cache_dispatch_(gl4444_,1511,gl4445_,INTVAL_(gl4446_));
         gl468_ = CFN_(gl4446_)(gl4444_);
         gl4447_ = PATT_(ch__, 28 + ((i__) << 2));
         cache_dispatch_(gl4447_,1511,gl4448_,INTVAL_(gl4449_));
         gl467_ = CFN_(gl4449_)(gl4447_);
         if ((gl468_ & (! gl467_))) {
            res__ = (char)0;
            goto ret0__;
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4450_: ;

   ret0__:
   return (res__);
}

ROU156_add_str_const_(self__,str_const__)
ptr self__;
ptr str_const__;
{
   int    k__ = S_int_VOID_;

   k__ = (int)LIS98_contains_(PATT_(self__,84),IATT_(str_const__,28));
   if ((k__ < 0)) {
      PATT_(self__,80) = (ptr)LIS165_push_(PATT_(self__,80),str_const__);
      PATT_(self__,84) = (ptr)LIS98_push_(PATT_(self__,84),IATT_(str_const__,28));
   }
   else {
   }

   ret0__:
   return;
}

ROU156_cprint_rtcode_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   if (GLO94_check_is_on_(0)) {
      (void)SAT99_c_(outfile__,'\n');
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(outfile__,1);
      PRI187_cprint_update_exec_info1_(0,outfile__,ERR96_def_filename_(0,IATT_(self__,20)),CLA148_full_name_(PATT_(self__,32)),STR69_at_index_(GLO68_str_table_,IATT_(self__,16)));
   }
   else {
   }

   ret0__:
   return;
}

ROU156_cprint_post_rtcode_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,36,ls2155_,"if (self__ /= S_ptr_VOID_) assert_(");
   SATHER_STR_(20,11,ls2156_,"_(self__),");
   SATHER_STR_(20,3,ls1750_,",\"");
   SATHER_STR_(20,5,ls722_,"\");\n");

   if (GLO94_check_is_on_(0)) {
      if ((PATT_(self__,44) != 0)) {
         ASS135_cprint_code_(PATT_(self__,44),outfile__);
      }
      else {
      }
      if ((PATT_(PATT_(self__,32),36) != 0)) {
         (void)SAT99_indent_(outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_s_(SAT99_c_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2155_)),PATT_(PATT_(self__,32),84)),'_'),(ptr)(&gs496_)),(ptr)(&ls2156_)),IATT_(self__,20)),(ptr)(&ls1750_)),ERR96_def_filename_(0,IATT_(self__,20))),(ptr)(&ls722_)),1);
      }
      else {
      }
      (void)SAT99_c_(outfile__,'\n');
      (void)SAT99_indent_(outfile__);
      (void)SAT99_inc_ln_(outfile__,1);
      PRI187_cprint_restore_exec_info_(0,outfile__);
   }
   else {
   }

   ret0__:
   return;
}


/* cinh_f155.c : Sather class: CINH_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr TYP149_dup_();
extern int TYP149_inst_ind_();
extern ptr CLA148_get_feature_();
extern ptr CLA148_full_name_();
extern ptr CLA93_at_index_();
extern char GLO94_handle_class_p_();
extern ERR96_format_error_exit_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
#include "macros_.h"



ptr CIN155_initialize_();
CIN155_resolve_predef_types_();
CIN155_semant_();
ptr CIN155_typeof_();
int CIN155_get_offset_();
CIN155_cprint_offset_();
ptr CIN155_get_constval_();
CIN155_cont_cprint_code_();
CIN155_cprint_cname_();
CIN155_cprint_extern_();
CIN155_cprint_access_value_();
CIN155_cprint_init_code_();
/*constant*/ int CIN155_print_indent_ = 2;
ptr CIN155_create_();
CIN155_out_of_line_();
ptr CIN155_dup_();
CIN155_put_kwdname_();
ptr CIN155_sather_code_();
int CIN155_class_inst_ind_();
int CIN155_featob_s_name_();
CIN155_mark_private_();
CIN155_mark_abstract_();
CIN155_mark_spec_();
CIN155_mark_shared_();
CIN155_mark_readonly_();
char CIN155_undefined_p_();
int CIN155_compute_own_offset_();
CIN155_eval_constant_();
ptr CIN155_rettype_();
CIN155_remember_local_();
CIN155_semant_prologue_();
CIN155_do_gen_temps_();
CIN155_gen_goto_tags_();
CIN155_validate_dispatches_and_get_ext_strs_();
char CIN155_compatible_des_feat_();
CIN155_update_used_in_dispatch_();
CIN155_consistent_defs_();
CIN155_cprint_decln_();
CIN155_cprint_routine_();
CIN155_cprint_store_dispval_();
extern int attr_ent_CIN155[];

ptr CIN155_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

CIN155_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

CIN155_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr CIN155_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int CIN155_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

CIN155_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr CIN155_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

CIN155_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

CIN155_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

CIN155_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,41,ls2129_,"(FEATOB_S): Invalid reference of feature");

   ERR96_format_error_exit_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2129_)));

   ret0__:
   return;
}

CIN155_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

CIN155_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr CIN155_create_(self__,tp__,ln__,c_def__)
ptr self__;
ptr tp__;
int ln__;
ptr c_def__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(155,0);
   PATT_(res__,36) = (ptr)tp__;
   PATT_(res__,24) = (ptr)c_def__;
   PATT_(res__,28) = (ptr)GLO68_curr_class_inst_;
   IATT_(res__,16) = (int)ln__;

   ret0__:
   return (res__);
}

CIN155_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr CIN155_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4330_;
   static int gl4331_;
   static union dtype_ gl4332_;
   ptr gl445_;

   gl4330_ = PATT_(self__,36);
   cache_dispatch_(gl4330_,471,gl4331_,INTVAL_(gl4332_));
   gl445_ = PFN_(gl4332_)(gl4330_);
   res__ = (ptr)CIN155_create_(self__,gl445_,IATT_(self__,16),PATT_(self__,24));
   CATT_(res__,4) = (char)CATT_(self__,4);

   ret0__:
   return (res__);
}

CIN155_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4333_;
   static int gl4334_;
   static union dtype_ gl4335_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4333_ = x__;
   cache_dispatch_(gl4333_,796,gl4334_,INTVAL_(gl4335_));
   IATT_(gl4333_,INTVAL_(gl4335_)) = (int)nm__;

   ret0__:
   return;
}

ptr CIN155_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int CIN155_class_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   ptr gl4336_;
   static int gl4337_;
   static union dtype_ gl4338_;

   gl4336_ = PATT_(self__,36);
   cache_dispatch_(gl4336_,1547,gl4337_,INTVAL_(gl4338_));
   res__ = (int)IFN_(gl4338_)(gl4336_);

   ret0__:
   return (res__);
}

int CIN155_featob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   SATHER_STR_(20,51,ls2132_,"(CINH_FEATOB_S): Invalid reference of feature name");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2132_)));

   ret0__:
   return (res__);
}

CIN155_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

CIN155_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

CIN155_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

CIN155_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

CIN155_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char CIN155_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

int CIN155_compute_own_offset_(self__,nextloc__)
ptr self__;
int nextloc__;
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

CIN155_eval_constant_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr CIN155_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

CIN155_remember_local_(self__,lvar__)
ptr self__;
ptr lvar__;
{


   ret0__:
   return;
}

CIN155_semant_prologue_(self__)
ptr self__;
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

CIN155_do_gen_temps_(self__)
ptr self__;
{


   ret0__:
   return;
}

CIN155_gen_goto_tags_(self__)
ptr self__;
{


   ret0__:
   return;
}

CIN155_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

char CIN155_compatible_des_feat_(self__,feat__,lval_p__)
ptr self__;
ptr feat__;
char lval_p__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

CIN155_update_used_in_dispatch_(self__)
ptr self__;
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

CIN155_consistent_defs_(self__,nm__,lval_p__)
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
   ptr gl4339_;
   static int gl4340_;
   static union dtype_ gl4341_;
   ptr gl4342_;
   int gl446_;
   ptr    co__ = 0;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   ptr    descendents__ = 0;
   int    index__ = S_int_VOID_;
   ptr    co1__ = 0;
   ptr    referrent__ = 0;
   ptr    feat__ = 0;

   if (((IATT_(self__,32) == 1) & (lval_p__ == 0))) {
      goto ret0__;
   }
   else {
      if (((IATT_(self__,32) == 2) & (lval_p__ == 1))) {
         goto ret0__;
      }
      else {
         if ((IATT_(self__,32) == 3)) {
            goto ret0__;
         }
         else {
         }
      }
   }
   if ((IATT_(self__,32) == 0)) {
      if (lval_p__) {
         IATT_(self__,32) = (int)2;
      }
      else {
         IATT_(self__,32) = (int)1;
      }
   }
   else {
      if ((IATT_(self__,32) == 1)) {
         if (lval_p__) {
            IATT_(self__,32) = (int)3;
         }
         else {
         }
      }
      else {
         if ((IATT_(self__,32) == 2)) {
            if ((! lval_p__)) {
               IATT_(self__,32) = (int)3;
            }
            else {
            }
         }
         else {
         }
      }
   }
   co__ = (ptr)PATT_(self__,28);
   i__ = (int)0;
   psz__ = S_int_VOID_;
   descendents__ = (ptr)PATT_(co__,76);
   if ((descendents__ != 0)) {
      psz__ = (int)IATT_(descendents__,12);
   }
   else {
   }
   CIN155_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4343_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4339_ = referrent__;
               gl446_ = TYPE_(gl4339_);
               gl4342_ = self__;
               if ((gl446_ == (155))) {
                  feat__ = (ptr)referrent__;
                  CIN155_update_used_in_dispatch_(feat__);
                  if ((! CIN155_compatible_des_feat_(self__,feat__,lval_p__))) {
                     ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2125_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
                  }
                  else {
                  }
               }
               else {
                  ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2126_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2127_)),CLA148_full_name_(co1__)),(ptr)(&ls2128_)),CLA148_full_name_(co__)),(ptr)(&ls785_)));
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4343_: ;

   ret0__:
   return;
}

CIN155_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2130_,"(FEATOB_S): Feature is not declarable");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2130_)));

   ret0__:
   return;
}

CIN155_cprint_routine_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

CIN155_cprint_store_dispval_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


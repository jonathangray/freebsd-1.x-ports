/* alias_157.c : Sather class: ALIAS_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ERR96_format_error_exit_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr CLA148_get_feature_();
extern ptr CLA148_full_name_();
extern ptr CLA93_at_index_();
extern char GLO94_handle_class_p_();
#include "macros_.h"



ptr ALI157_initialize_();
ALI157_resolve_predef_types_();
ALI157_semant_();
ptr ALI157_typeof_();
int ALI157_get_offset_();
ALI157_cprint_offset_();
ptr ALI157_get_constval_();
ALI157_cont_cprint_code_();
ALI157_cprint_cname_();
ALI157_cprint_extern_();
ALI157_cprint_access_value_();
ALI157_cprint_init_code_();
/*constant*/ int ALI157_print_indent_ = 2;
ptr ALI157_create_();
ALI157_out_of_line_();
ptr ALI157_dup_();
ALI157_put_kwdname_();
ptr ALI157_sather_code_();
int ALI157_class_inst_ind_();
int ALI157_featob_s_name_();
ALI157_mark_private_();
ALI157_mark_abstract_();
ALI157_mark_spec_();
ALI157_mark_shared_();
ALI157_mark_readonly_();
char ALI157_undefined_p_();
int ALI157_compute_own_offset_();
ALI157_eval_constant_();
ptr ALI157_rettype_();
ALI157_remember_local_();
ALI157_semant_prologue_();
ALI157_do_gen_temps_();
ALI157_gen_goto_tags_();
ALI157_validate_dispatches_and_get_ext_strs_();
char ALI157_compatible_des_feat_();
ALI157_update_used_in_dispatch_();
ALI157_consistent_defs_();
ALI157_cprint_decln_();
ALI157_cprint_routine_();
ALI157_cprint_store_dispval_();
extern int attr_ent_ALI157[];

ptr ALI157_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ALI157_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

ALI157_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr ALI157_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ALI157_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ALI157_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ALI157_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ALI157_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

ALI157_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ALI157_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,41,ls2129_,"(FEATOB_S): Invalid reference of feature");

   ERR96_format_error_exit_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2129_)));

   ret0__:
   return;
}

ALI157_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ALI157_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ALI157_create_(self__,nm__,orig__,cls__,ln__)
ptr self__;
int nm__;
int orig__;
ptr cls__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(157,0);
   IATT_(res__,12) = (int)nm__;
   IATT_(res__,36) = (int)orig__;
   PATT_(res__,28) = (ptr)GLO68_curr_class_inst_;
   PATT_(res__,24) = (ptr)cls__;
   IATT_(res__,16) = (int)ln__;

   ret0__:
   return (res__);
}

ALI157_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr ALI157_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)ALI157_create_(self__,IATT_(self__,12),IATT_(self__,36),PATT_(self__,24),IATT_(self__,16));
   CATT_(res__,5) = (char)CATT_(self__,5);

   ret0__:
   return (res__);
}

ALI157_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4451_;
   static int gl4452_;
   static union dtype_ gl4453_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4451_ = x__;
   cache_dispatch_(gl4451_,796,gl4452_,INTVAL_(gl4453_));
   IATT_(gl4451_,INTVAL_(gl4453_)) = (int)nm__;

   ret0__:
   return;
}

ptr ALI157_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ALI157_class_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int ALI157_featob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

ALI157_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

ALI157_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

ALI157_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

ALI157_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

ALI157_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char ALI157_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

int ALI157_compute_own_offset_(self__,nextloc__)
ptr self__;
int nextloc__;
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

ALI157_eval_constant_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr ALI157_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ALI157_remember_local_(self__,lvar__)
ptr self__;
ptr lvar__;
{


   ret0__:
   return;
}

ALI157_semant_prologue_(self__)
ptr self__;
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

ALI157_do_gen_temps_(self__)
ptr self__;
{


   ret0__:
   return;
}

ALI157_gen_goto_tags_(self__)
ptr self__;
{


   ret0__:
   return;
}

ALI157_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

char ALI157_compatible_des_feat_(self__,feat__,lval_p__)
ptr self__;
ptr feat__;
char lval_p__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ALI157_update_used_in_dispatch_(self__)
ptr self__;
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

ALI157_consistent_defs_(self__,nm__,lval_p__)
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
   ptr gl4454_;
   static int gl4455_;
   static union dtype_ gl4456_;
   ptr gl4457_;
   int gl469_;
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
   ALI157_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4458_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4454_ = referrent__;
               gl469_ = TYPE_(gl4454_);
               gl4457_ = self__;
               if ((gl469_ == (157))) {
                  feat__ = (ptr)referrent__;
                  ALI157_update_used_in_dispatch_(feat__);
                  if ((! ALI157_compatible_des_feat_(self__,feat__,lval_p__))) {
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
goto_tag_4458_: ;

   ret0__:
   return;
}

ALI157_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2130_,"(FEATOB_S): Feature is not declarable");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2130_)));

   ret0__:
   return;
}

ALI157_cprint_routine_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

ALI157_cprint_store_dispval_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


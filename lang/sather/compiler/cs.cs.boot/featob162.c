/* featob162.c : Sather class: FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void ERR96_format_error_exit_(ptr self__, int ln__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr CLA148_get_feature_(ptr self__, int index__);
extern ptr CLA148_full_name_(ptr self__);
extern ptr CLA93_at_index_(ptr self__, int i__);
extern char GLO94_handle_class_p_(ptr self__, ptr co__);
#include "macros_.h"



ptr FEA162_initialize_(ptr self__, ptr initarg__);
void FEA162_resolve_predef_types_(ptr self__, int index__);
void FEA162_semant_(ptr self__, ptr symtab__);
ptr FEA162_typeof_(ptr self__);
int FEA162_get_offset_(ptr self__);
void FEA162_cprint_offset_(ptr self__, ptr outfile__);
ptr FEA162_get_constval_(ptr self__);
void FEA162_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void FEA162_cprint_cname_(ptr self__, ptr outfile__);
void FEA162_cprint_extern_(ptr self__, ptr outfile__);
void FEA162_cprint_access_value_(ptr self__, ptr outfile__);
void FEA162_cprint_init_code_(ptr self__, ptr outfile__);
/*constant*/ int FEA162_print_indent_ = 2;
ptr FEA162_create_(ptr self__, int ln__, ptr c_def__);
void FEA162_out_of_line_(ptr self__, ptr fn__);
ptr FEA162_dup_(ptr self__);
void FEA162_put_kwdname_(ptr self__, int nm__);
ptr FEA162_sather_code_(ptr self__);
int FEA162_class_inst_ind_(ptr self__);
int FEA162_featob_s_name_(ptr self__);
void FEA162_mark_private_(ptr self__);
void FEA162_mark_abstract_(ptr self__);
void FEA162_mark_spec_(ptr self__);
void FEA162_mark_shared_(ptr self__);
void FEA162_mark_readonly_(ptr self__);
char FEA162_undefined_p_(ptr self__);
int FEA162_compute_own_offset_(ptr self__, int nextloc__);
void FEA162_eval_constant_(ptr self__);
ptr FEA162_rettype_(ptr self__);
void FEA162_remember_local_(ptr self__, ptr lvar__);
void FEA162_semant_prologue_(ptr self__);
void FEA162_do_gen_temps_(ptr self__);
void FEA162_gen_goto_tags_(ptr self__);
void FEA162_validate_dispatches_and_get_ext_strs_(ptr self__);
char FEA162_compatible_des_feat_(ptr self__, ptr feat__, char lval_p__);
void FEA162_update_used_in_dispatch_(ptr self__);
void FEA162_consistent_defs_(ptr self__, int nm__, char lval_p__);
void FEA162_cprint_decln_(ptr self__, ptr outfile__);
void FEA162_cprint_routine_(ptr self__, ptr outfile__);
void FEA162_cprint_store_dispval_(ptr self__, ptr outfile__);
extern int attr_ent_FEA162[];

ptr FEA162_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void FEA162_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void FEA162_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr FEA162_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int FEA162_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void FEA162_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr FEA162_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void FEA162_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void FEA162_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void FEA162_cprint_extern_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,41,ls2129_,"(FEATOB_S): Invalid reference of feature");

   ERR96_format_error_exit_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2129_)));

   ret0__:
   return;
}

void FEA162_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void FEA162_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr FEA162_create_(ptr self__, int ln__, ptr c_def__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(162,0);
   IATT_(res__,16) = (int)ln__;
   PATT_(res__,24) = (ptr)c_def__;
   PATT_(res__,28) = (ptr)GLO68_curr_class_inst_;

   ret0__:
   return (res__);
}

void FEA162_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr FEA162_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)FEA162_create_(self__,IATT_(self__,16),PATT_(self__,24));

   ret0__:
   return (res__);
}

void FEA162_put_kwdname_(ptr self__, int nm__)
{
   ptr gl4477_;
   static int gl4478_;
   static union dtype_ gl4479_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4477_ = x__;
   cache_dispatch_(gl4477_,796,gl4478_,INTVAL_(gl4479_));
   IATT_(gl4477_,INTVAL_(gl4479_)) = (int)nm__;

   ret0__:
   return;
}

ptr FEA162_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int FEA162_class_inst_ind_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int FEA162_featob_s_name_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

void FEA162_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void FEA162_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void FEA162_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void FEA162_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void FEA162_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char FEA162_undefined_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

int FEA162_compute_own_offset_(ptr self__, int nextloc__)
{
   int res__ = S_int_VOID_;

   res__ = (int)nextloc__;

   ret0__:
   return (res__);
}

void FEA162_eval_constant_(ptr self__)
{


   ret0__:
   return;
}

ptr FEA162_rettype_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void FEA162_remember_local_(ptr self__, ptr lvar__)
{


   ret0__:
   return;
}

void FEA162_semant_prologue_(ptr self__)
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

void FEA162_do_gen_temps_(ptr self__)
{


   ret0__:
   return;
}

void FEA162_gen_goto_tags_(ptr self__)
{


   ret0__:
   return;
}

void FEA162_validate_dispatches_and_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

char FEA162_compatible_des_feat_(ptr self__, ptr feat__, char lval_p__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

void FEA162_update_used_in_dispatch_(ptr self__)
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

void FEA162_consistent_defs_(ptr self__, int nm__, char lval_p__)
{
   SATHER_STR_(20,22,ls2124_,"(FEATOB_S): Feature \"");
   SATHER_STR_(20,49,ls2125_,"\" has incompatible definitions for dispatch in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,52,ls2126_,"\" are different kinds of features for dispatch in \"");
   SATHER_STR_(20,42,ls2127_,"\" is missing for dispatch in descendent \"");
   SATHER_STR_(20,13,ls2128_,"\" of class \"");
   ptr gl4480_;
   static int gl4481_;
   static union dtype_ gl4482_;
   ptr gl4483_;
   int gl473_;
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
   FEA162_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4484_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4480_ = referrent__;
               gl473_ = TYPE_(gl4480_);
               gl4483_ = self__;
               if ((gl473_ == (162))) {
                  feat__ = (ptr)referrent__;
                  FEA162_update_used_in_dispatch_(feat__);
                  if ((! FEA162_compatible_des_feat_(self__,feat__,lval_p__))) {
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
goto_tag_4484_: ;

   ret0__:
   return;
}

void FEA162_cprint_decln_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,38,ls2130_,"(FEATOB_S): Feature is not declarable");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2130_)));

   ret0__:
   return;
}

void FEA162_cprint_routine_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

void FEA162_cprint_store_dispval_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}


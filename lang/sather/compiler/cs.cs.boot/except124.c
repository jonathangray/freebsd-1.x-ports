/* except124.c : Sather class: EXCEPT_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void LST123_semant_(ptr self__, ptr symtab__);
extern void SYM186_leave_new_scope_(ptr self__);
extern ptr LST123_gen_temps_(ptr self__);
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*shared*/ char GLO68_print_feat_info_;
extern void LVA145_cprint_cname_(ptr self__, ptr outfile__);
extern void LVA145_cprint_decln_(ptr self__, ptr outfile__);
extern ptr STR20_create_(ptr self__);
extern char TYP149_conforms_to_(ptr self__, ptr tp__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char TYP149_is_dispatched_(ptr self__);
extern int TYP149_inst_ind_(ptr self__);
extern char GLO94_check_is_on_(ptr self__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_i_(ptr self__, int in__);
extern ptr SAT99_ind_dec_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void LST123_gen_goto_tags_(ptr self__, ptr block__);
extern void LST123_validate_dispatches_and_get_ext_strs_(ptr self__);
extern void LST123_cprint_code_(ptr self__, ptr outfile__);
extern ptr LST123_dup_(ptr self__);
extern void LST123_resolve_predef_types_(ptr self__, int index__);
#include "macros_.h"



/*constant*/ int EXC124_print_indent_ = 2;
ptr EXC124_create_(ptr self__, ptr norm__, ptr hdlr__, ptr exc__, int ln__);
void EXC124_out_of_line_(ptr self__, ptr fn__);
ptr EXC124_dup_(ptr self__);
void EXC124_put_kwdname_(ptr self__, int nm__);
ptr EXC124_sather_code_(ptr self__);
ptr EXC124_initialize_(ptr self__, ptr initarg__);
void EXC124_resolve_predef_types_(ptr self__, int index__);
void EXC124_semant_(ptr self__, ptr symtab__);
ptr EXC124_typeof_(ptr self__);
int EXC124_get_offset_(ptr self__);
void EXC124_cprint_offset_(ptr self__, ptr outfile__);
ptr EXC124_get_constval_(ptr self__);
void EXC124_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void EXC124_cprint_cname_(ptr self__, ptr outfile__);
void EXC124_cprint_extern_(ptr self__, ptr outfile__);
void EXC124_cprint_access_value_(ptr self__, ptr outfile__);
void EXC124_cprint_init_code_(ptr self__, ptr outfile__);
char EXC124_typechk_exprs_(ptr self__, ptr tp__);
ptr EXC124_gen_temps_(ptr self__);
void EXC124_gen_goto_tags_(ptr self__, ptr block__);
void EXC124_validate_dispatches_and_get_ext_strs_(ptr self__);
void EXC124_cprint_code_(ptr self__, ptr outfile__);
extern int attr_ent_EXC124[];

ptr EXC124_create_(ptr self__, ptr norm__, ptr hdlr__, ptr exc__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(124,0);
   PATT_(res__,20) = (ptr)exc__;
   PATT_(res__,12) = (ptr)norm__;
   PATT_(res__,16) = (ptr)hdlr__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void EXC124_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr EXC124_dup_(ptr self__)
{
   ptr res__ = 0;

   if ((PATT_(self__,16) == 0)) {
      res__ = (ptr)EXC124_create_(self__,LST123_dup_(PATT_(self__,12)),0,0,IATT_(self__,4));
   }
   else {
      res__ = (ptr)EXC124_create_(self__,LST123_dup_(PATT_(self__,12)),LST123_dup_(PATT_(self__,16)),LST123_dup_(PATT_(self__,20)),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}

void EXC124_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3061_;
   static int gl3062_;
   static union dtype_ gl3063_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3061_ = x__;
   cache_dispatch_(gl3061_,796,gl3062_,INTVAL_(gl3063_));
   IATT_(gl3061_,INTVAL_(gl3063_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXC124_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXC124_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void EXC124_resolve_predef_types_(ptr self__, int index__)
{

   LST123_resolve_predef_types_(PATT_(self__,12),index__);
   if ((PATT_(self__,16) != 0)) {
      LST123_resolve_predef_types_(PATT_(self__,20),index__);
      LST123_resolve_predef_types_(PATT_(self__,16),index__);
   }
   else {
   }

   ret0__:
   return;
}

void EXC124_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,58,ls2604_,"(EXCEPT_STMTOB_S): Exception type must be an object type.");
   ptr gl3064_;
   static int gl3065_;
   static union dtype_ gl3066_;
   char gl299_;
   ptr gl3067_;
   static int gl3068_;
   static union dtype_ gl3069_;
   int gl300_;
   ptr    exc__ = 0;
   ptr    typ__ = 0;

   LST123_semant_(PATT_(self__,12),symtab__);
   if ((PATT_(self__,16) != 0)) {
      SYM186_enter_new_scope_(symtab__);
      LST123_semant_(PATT_(self__,20),symtab__);
      exc__ = (ptr)PATT_(PATT_(self__,20), 28 + ((0) << 2));
      typ__ = (ptr)PATT_(exc__,16);
      if ((! (typ__ == 0))) {
         gl3064_ = typ__;
         cache_dispatch_(gl3064_,903,gl3065_,INTVAL_(gl3066_));
         gl299_ = CFN_(gl3066_)(gl3064_,GLO68_ob_typeob_s_);
         if ((! gl299_)) {
            gl3067_ = typ__;
            cache_dispatch_(gl3067_,298,gl3068_,INTVAL_(gl3069_));
            gl300_ = IATT_(gl3067_,INTVAL_(gl3069_));
            ERR96_format_error_msg_(0,gl300_,STR20_s_(STR20_create_(0),(ptr)(&ls2604_)));
         }
         else {
         }
      }
      else {
      }
      LST123_semant_(PATT_(self__,16),symtab__);
      SYM186_leave_new_scope_(symtab__);
   }
   else {
   }

   ret0__:
   return;
}

ptr EXC124_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int EXC124_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void EXC124_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr EXC124_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void EXC124_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void EXC124_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXC124_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXC124_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXC124_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char EXC124_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr EXC124_gen_temps_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)LST123_gen_temps_(PATT_(self__,12));
   if ((PATT_(self__,16) != 0)) {
      if ((res__ != 0)) {
         res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,20)));
      }
      else {
         res__ = (ptr)LST123_gen_temps_(PATT_(self__,20));
      }
      if ((res__ != 0)) {
         res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,16)));
      }
      else {
         res__ = (ptr)LST123_gen_temps_(PATT_(self__,16));
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

void EXC124_gen_goto_tags_(ptr self__, ptr block__)
{

   LST123_gen_goto_tags_(PATT_(self__,12),block__);
   if ((PATT_(self__,16) != 0)) {
      LST123_gen_goto_tags_(PATT_(self__,16),block__);
   }
   else {
   }

   ret0__:
   return;
}

void EXC124_validate_dispatches_and_get_ext_strs_(ptr self__)
{

   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,12));
   if ((PATT_(self__,16) != 0)) {
      LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,16));
   }
   else {
   }

   ret0__:
   return;
}

void EXC124_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,12,ls2605_,"EH_CATCH (\n");
   SATHER_STR_(20,5,ls2606_,", {\n");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,2,ls780_,"\n");
   SATHER_STR_(20,5,ls2607_,"},{\n");
   SATHER_STR_(20,19,ls2608_,"CATCH_EXEC_INFO_;\n");
   SATHER_STR_(20,4,ls2609_,"});");
   ptr gl3070_;
   static int gl3071_;
   static union dtype_ gl3072_;
   ptr gl3073_;
   static int gl3074_;
   static union dtype_ gl3075_;
   int gl301_;
   ptr    err__ = 0;
   ptr    typ__ = 0;

   if ((PATT_(self__,16) == 0)) {
      LST123_cprint_code_(PATT_(self__,12),outfile__);
   }
   else {
      err__ = (ptr)PATT_(PATT_(self__,20), 28 + ((0) << 2));
      typ__ = (ptr)PATT_(err__,16);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2605_)),1);
      (void)SAT99_indent_(SAT99_ind_inc_(outfile__));
      gl3070_ = typ__;
      cache_dispatch_(gl3070_,1511,gl3071_,INTVAL_(gl3072_));
      if (CFN_(gl3072_)(gl3070_)) {
         if ((! GLO68_print_feat_info_)) {
            GLO68_print_feat_info_ = (char)1;
         }
         else {
         }
         (void)SAT99_c_(outfile__,'-');
      }
      else {
      }
      gl3073_ = typ__;
      cache_dispatch_(gl3073_,1547,gl3074_,INTVAL_(gl3075_));
      gl301_ = IFN_(gl3075_)(gl3073_);
      (void)SAT99_i_(outfile__,gl301_);
      (void)SAT99_c_(outfile__,',');
      LVA145_cprint_cname_(err__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2606_)),1);
      (void)SAT99_indent_(outfile__);
      LVA145_cprint_decln_(err__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      (void)SAT99_indent_(outfile__);
      LST123_cprint_code_(PATT_(self__,12),outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls780_)),1);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2607_)),1);
      if (GLO94_check_is_on_(0)) {
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2608_)),1);
      }
      else {
      }
      LST123_cprint_code_(PATT_(self__,16),outfile__);
      (void)SAT99_ind_dec_(outfile__);
      (void)SAT99_indent_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2609_)));
   }

   ret0__:
   return;
}


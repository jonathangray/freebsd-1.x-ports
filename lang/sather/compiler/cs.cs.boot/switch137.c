/* switch137.c : Sather class: SWITCH_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void LST126_gen_goto_tags_(ptr self__, ptr block__);
extern void LST126_validate_dispatches_and_get_ext_strs_(ptr self__);
extern ptr LST123_dup_(ptr self__);
extern void LST123_resolve_predef_types_(ptr self__, int index__);
extern void LST123_semant_(ptr self__, ptr symtab__);
extern void LST126_semant_(ptr self__, ptr symtab__);
extern ptr LST126_dup_(ptr self__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
extern char WHE136_typechk_exprs_(ptr self__, ptr tp__);
extern void LST123_cprint_code_(ptr self__, ptr outfile__);
extern void WHE136_cprint_case_code_(ptr self__, ptr outfile__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int TYP149_ctype_(ptr self__);
extern char TYP149_int_type_p_(ptr self__);
extern char TYP149_char_type_p_(ptr self__);
extern void SYM186_leave_new_scope_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern /*shared*/ char COM82_dbg_mode_;
extern int GLO94_global_key_(ptr self__);
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr LIS98_push_(ptr self__, int e__);
extern ptr LIS98_create_(ptr self__, int init_size__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_ind_dec_(ptr self__);
extern ptr SAT99_i_(ptr self__, int in__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr EXP117_dup_(ptr self__);
extern void EXP117_resolve_predef_types_(ptr self__, int index__);
extern void EXP117_semant_(ptr self__, ptr symtab__);
extern ptr EXP117_gen_temps_(ptr self__);
extern void EXP117_get_ext_strs_(ptr self__);
extern void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
extern ptr LST123_gen_temps_(ptr self__);
extern void LST123_gen_goto_tags_(ptr self__, ptr block__);
extern void LST123_validate_dispatches_and_get_ext_strs_(ptr self__);
extern void LST126_resolve_predef_types_(ptr self__, int index__);
extern ptr LST126_gen_temps_(ptr self__);
#include "macros_.h"



/*constant*/ int SWI137_print_indent_ = 2;
ptr SWI137_create_(ptr self__, ptr t__, ptr wp__, ptr ep__, int ln__);
void SWI137_out_of_line_(ptr self__, ptr fn__);
ptr SWI137_dup_(ptr self__);
void SWI137_put_kwdname_(ptr self__, int nm__);
ptr SWI137_sather_code_(ptr self__);
ptr SWI137_initialize_(ptr self__, ptr initarg__);
void SWI137_resolve_predef_types_(ptr self__, int index__);
void SWI137_semant_(ptr self__, ptr symtab__);
ptr SWI137_typeof_(ptr self__);
int SWI137_get_offset_(ptr self__);
void SWI137_cprint_offset_(ptr self__, ptr outfile__);
ptr SWI137_get_constval_(ptr self__);
void SWI137_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void SWI137_cprint_cname_(ptr self__, ptr outfile__);
void SWI137_cprint_extern_(ptr self__, ptr outfile__);
void SWI137_cprint_access_value_(ptr self__, ptr outfile__);
void SWI137_cprint_init_code_(ptr self__, ptr outfile__);
char SWI137_typechk_exprs_(ptr self__, ptr tp__);
ptr SWI137_gen_temps_(ptr self__);
void SWI137_gen_goto_tags_(ptr self__, ptr block__);
void SWI137_validate_dispatches_and_get_ext_strs_(ptr self__);
void SWI137_cprint_code_(ptr self__, ptr outfile__);
void SWI137_cprint_if_form_code_(ptr self__, ptr outfile__, int i__, int j__);
void SWI137_cprint_then_parts_code_(ptr self__, ptr outfile__);
void SWI137_cprint_default_code_(ptr self__, ptr outfile__, int i__, int j__);
void SWI137_cprint_caseform_code_(ptr self__, ptr outfile__);
extern int attr_ent_SWI137[];

ptr SWI137_create_(ptr self__, ptr t__, ptr wp__, ptr ep__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(137,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)wp__;
   PATT_(res__,20) = (ptr)ep__;
   IATT_(res__,24) = (int)ln__;

   ret0__:
   return (res__);
}

void SWI137_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,24) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,24));

   ret0__:
   return;
}

ptr SWI137_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3286_;
   static int gl3287_;
   static union dtype_ gl3288_;
   ptr gl318_;
   ptr gl3289_;
   static int gl3290_;
   static union dtype_ gl3291_;
   ptr gl319_;

   if ((PATT_(self__,20) != 0)) {
      gl3286_ = PATT_(self__,12);
      cache_dispatch_(gl3286_,471,gl3287_,INTVAL_(gl3288_));
      gl318_ = PFN_(gl3288_)(gl3286_);
      res__ = (ptr)SWI137_create_(self__,gl318_,LST126_dup_(PATT_(self__,16)),LST123_dup_(PATT_(self__,20)),IATT_(self__,24));
   }
   else {
      gl3289_ = PATT_(self__,12);
      cache_dispatch_(gl3289_,471,gl3290_,INTVAL_(gl3291_));
      gl319_ = PFN_(gl3291_)(gl3289_);
      res__ = (ptr)SWI137_create_(self__,gl319_,LST126_dup_(PATT_(self__,16)),0,IATT_(self__,24));
   }

   ret0__:
   return (res__);
}

void SWI137_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3292_;
   static int gl3293_;
   static union dtype_ gl3294_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3292_ = x__;
   cache_dispatch_(gl3292_,796,gl3293_,INTVAL_(gl3294_));
   IATT_(gl3292_,INTVAL_(gl3294_)) = (int)nm__;

   ret0__:
   return;
}

ptr SWI137_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SWI137_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void SWI137_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3295_;
   static int gl3296_;
   static union dtype_ gl3297_;

   gl3295_ = PATT_(self__,12);
   cache_dispatch_(gl3295_,522,gl3296_,INTVAL_(gl3297_));
   VFN_(gl3297_)(gl3295_,index__);
   LST126_resolve_predef_types_(PATT_(self__,16),index__);
   if ((PATT_(self__,20) != 0)) {
      LST123_resolve_predef_types_(PATT_(self__,20),index__);
   }
   else {
   }

   ret0__:
   return;
}

void SWI137_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,53,ls2622_,"(SWITCH_STMTOB_S): Test in \"case\" has no return type");
   SATHER_STR_(20,53,ls2625_,"(SWITCH_STMTOB_S): Type mismatch in \"case\" statement");
   ptr gl3298_;
   static int gl3299_;
   static union dtype_ gl3300_;
   ptr gl3301_;
   static int gl3302_;
   static union dtype_ gl3303_;
   ptr gl320_;
   ptr gl3304_;
   static int gl3305_;
   static union dtype_ gl3306_;
   ptr gl3307_;
   static int gl3308_;
   static union dtype_ gl3309_;
   ptr gl3310_;
   static int gl3311_;
   static union dtype_ gl3312_;
   ptr gl3313_;
   static int gl3314_;
   static union dtype_ gl3315_;
   ptr gl321_;
   int    i__ = S_int_VOID_;
   int    wsz__ = S_int_VOID_;
   ptr    w_stmt__ = 0;

   gl3298_ = PATT_(self__,12);
   cache_dispatch_(gl3298_,588,gl3299_,INTVAL_(gl3300_));
   VFN_(gl3300_)(gl3298_,symtab__);
   gl3301_ = PATT_(self__,12);
   cache_dispatch_(gl3301_,1577,gl3302_,INTVAL_(gl3303_));
   gl320_ = PATT_(gl3301_,INTVAL_(gl3303_));
   if ((gl320_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,24),STR20_s_(STR20_create_(0),(ptr)(&ls2622_)));
      gl3304_ = PATT_(self__,12);
      cache_dispatch_(gl3304_,1577,gl3305_,INTVAL_(gl3306_));
      PATT_(gl3304_,INTVAL_(gl3306_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
      gl3310_ = PATT_(self__,12);
      cache_dispatch_(gl3310_,1577,gl3311_,INTVAL_(gl3312_));
      gl3307_ = PATT_(gl3310_,INTVAL_(gl3312_));
      cache_dispatch_(gl3307_,298,gl3308_,INTVAL_(gl3309_));
      IATT_(gl3307_,INTVAL_(gl3309_)) = (int)IATT_(self__,24);
   }
   else {
   }
   LST126_semant_(PATT_(self__,16),symtab__);
   i__ = (int)0;
   wsz__ = (int)IATT_(PATT_(self__,16),12);
   while (1) {
      if ((i__ >= wsz__)) {
         goto goto_tag_3316_;
      }
      else {
      }
      w_stmt__ = (ptr)PATT_(PATT_(self__,16), 28 + ((i__) << 2));
      gl3313_ = PATT_(self__,12);
      cache_dispatch_(gl3313_,1577,gl3314_,INTVAL_(gl3315_));
      gl321_ = PATT_(gl3313_,INTVAL_(gl3315_));
      if ((! WHE136_typechk_exprs_(w_stmt__,gl321_))) {
         ERR96_format_error_msg_(0,IATT_(self__,24),STR20_s_(STR20_create_(0),(ptr)(&ls2625_)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3316_: ;
   if ((PATT_(self__,20) != 0)) {
      SYM186_enter_new_scope_(symtab__);
      LST123_semant_(PATT_(self__,20),symtab__);
      SYM186_leave_new_scope_(symtab__);
   }
   else {
   }
   i__ = (int)0;
   CATT_(self__,4) = (char)1;
   while (1) {
      if ((i__ >= wsz__)) {
         goto goto_tag_3317_;
      }
      else {
      }
      CATT_(self__,4) = (char)(CATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),4) & CATT_(self__,4));
      i__ = (int)(i__ + 1);
   }
goto_tag_3317_: ;

   ret0__:
   return;
}

ptr SWI137_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int SWI137_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void SWI137_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr SWI137_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void SWI137_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void SWI137_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SWI137_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SWI137_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SWI137_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char SWI137_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr SWI137_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3318_;
   static int gl3319_;
   static union dtype_ gl3320_;
   ptr gl3321_;
   static int gl3322_;
   static union dtype_ gl3323_;
   ptr gl3324_;
   static int gl3325_;
   static union dtype_ gl3326_;

   gl3318_ = PATT_(self__,12);
   cache_dispatch_(gl3318_,1582,gl3319_,INTVAL_(gl3320_));
   res__ = (ptr)PFN_(gl3320_)(gl3318_);
   if ((! CATT_(self__,4))) {
      if ((IATT_(self__,28) <= 0)) {
         IATT_(self__,28) = (int)GLO94_global_key_(0);
         gl3324_ = PATT_(self__,12);
         cache_dispatch_(gl3324_,1577,gl3325_,INTVAL_(gl3326_));
         gl3321_ = PATT_(gl3324_,INTVAL_(gl3326_));
         cache_dispatch_(gl3321_,374,gl3322_,INTVAL_(gl3323_));
         IATT_(self__,32) = (int)IFN_(gl3323_)(gl3321_);
         if ((res__ != 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,28)),IATT_(self__,32));
         }
         else {
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,28)),IATT_(self__,32));
         }
      }
      else {
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,28)),IATT_(self__,32));
      }
   }
   else {
   }
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST126_gen_temps_(PATT_(self__,16)));
   }
   else {
      res__ = (ptr)LST126_gen_temps_(PATT_(self__,16));
   }
   if ((PATT_(self__,20) != 0)) {
      if ((res__ != 0)) {
         res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,20)));
      }
      else {
         res__ = (ptr)LST123_gen_temps_(PATT_(self__,20));
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

void SWI137_gen_goto_tags_(ptr self__, ptr block__)
{

   LST126_gen_goto_tags_(PATT_(self__,16),block__);
   if ((PATT_(self__,20) != 0)) {
      LST123_gen_goto_tags_(PATT_(self__,20),block__);
   }
   else {
   }

   ret0__:
   return;
}

void SWI137_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   ptr gl3327_;
   static int gl3328_;
   static union dtype_ gl3329_;

   LST126_validate_dispatches_and_get_ext_strs_(PATT_(self__,16));
   if ((PATT_(self__,20) != 0)) {
      LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,20));
   }
   else {
   }
   gl3327_ = PATT_(self__,12);
   cache_dispatch_(gl3327_,1677,gl3328_,INTVAL_(gl3329_));
   VFN_(gl3329_)(gl3327_);

   ret0__:
   return;
}

void SWI137_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,9,ls2636_,"switch (");
   SATHER_STR_(20,5,ls2602_,") {\n");
   SATHER_STR_(20,3,ls2489_,"= ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,15,ls2627_,"goto goto_tag_");
   SATHER_STR_(20,4,ls2628_,"_;\n");
   SATHER_STR_(20,10,ls968_,"goto_tag_");
   SATHER_STR_(20,6,ls2630_,"_: ;\n");
   ptr gl3330_;
   static int gl3331_;
   static union dtype_ gl3332_;
   ptr gl3333_;
   static int gl3334_;
   static union dtype_ gl3335_;
   ptr gl3336_;
   static int gl3337_;
   static union dtype_ gl3338_;
   ptr gl3339_;
   static int gl3340_;
   static union dtype_ gl3341_;
   char gl322_;
   char gl323_;
   ptr gl3342_;
   static int gl3343_;
   static union dtype_ gl3344_;
   ptr gl3345_;
   static int gl3346_;
   static union dtype_ gl3347_;
   ptr gl3348_;
   static int gl3349_;
   static union dtype_ gl3350_;
   ptr gl3351_;
   static int gl3352_;
   static union dtype_ gl3353_;
   ptr gl3354_;
   static int gl3355_;
   static union dtype_ gl3356_;
   ptr gl3357_;
   static int gl3358_;
   static union dtype_ gl3359_;
   ptr gl324_;

   gl3333_ = PATT_(self__,12);
   cache_dispatch_(gl3333_,1577,gl3334_,INTVAL_(gl3335_));
   gl3330_ = PATT_(gl3333_,INTVAL_(gl3335_));
   cache_dispatch_(gl3330_,1778,gl3331_,INTVAL_(gl3332_));
   gl322_ = CFN_(gl3332_)(gl3330_);
   gl3339_ = PATT_(self__,12);
   cache_dispatch_(gl3339_,1577,gl3340_,INTVAL_(gl3341_));
   gl3336_ = PATT_(gl3339_,INTVAL_(gl3341_));
   cache_dispatch_(gl3336_,2029,gl3337_,INTVAL_(gl3338_));
   gl323_ = CFN_(gl3338_)(gl3336_);
   if ((gl322_ | gl323_)) {
      gl3342_ = PATT_(self__,12);
      cache_dispatch_(gl3342_,1589,gl3343_,INTVAL_(gl3344_));
      VFN_(gl3344_)(gl3342_,outfile__);
      if ((COM82_dbg_mode_ == 1)) {
         DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,24)),ERR96_def_lineno_(0,IATT_(self__,24)),PATT_(outfile__,8),IATT_(outfile__,16));
      }
      else {
      }
      if ((CATT_(self__,4) & (IATT_(self__,28) == 0))) {
         (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2636_));
         gl3345_ = PATT_(self__,12);
         cache_dispatch_(gl3345_,965,gl3346_,INTVAL_(gl3347_));
         VFN_(gl3347_)(gl3345_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2602_)),1);
      }
      else {
         (void)SAT99_indent_(outfile__);
         GLO94_cprint_ctemp_name_(0,IATT_(self__,28),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls2489_));
         gl3348_ = PATT_(self__,12);
         cache_dispatch_(gl3348_,965,gl3349_,INTVAL_(gl3350_));
         VFN_(gl3350_)(gl3348_,outfile__);
         (void)SAT99_s_(SAT99_indent_(SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1)),(ptr)(&ls2636_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,28),outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2602_)),1);
      }
      (void)SAT99_ind_inc_(outfile__);
      SWI137_cprint_caseform_code_(self__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);
   }
   else {
      gl3351_ = PATT_(self__,12);
      cache_dispatch_(gl3351_,1589,gl3352_,INTVAL_(gl3353_));
      VFN_(gl3353_)(gl3351_,outfile__);
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,IATT_(self__,28),outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl3354_ = PATT_(self__,12);
      cache_dispatch_(gl3354_,965,gl3355_,INTVAL_(gl3356_));
      VFN_(gl3356_)(gl3354_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      IATT_(self__,36) = (int)GLO94_global_key_(0);
      SWI137_cprint_if_form_code_(self__,outfile__,0,0);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2627_)),IATT_(self__,36)),(ptr)(&ls2628_)),1);
      SWI137_cprint_then_parts_code_(self__,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls968_)),IATT_(self__,36)),(ptr)(&ls2630_)),1);
   }

   ret0__:
   return;
}

void SWI137_cprint_if_form_code_(ptr self__, ptr outfile__, int i__, int j__)
{
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,5,ls2601_,"if (");
   SATHER_STR_(20,5,ls1788_," == ");
   SATHER_STR_(20,5,ls2602_,") {\n");
   SATHER_STR_(20,15,ls2627_,"goto goto_tag_");
   SATHER_STR_(20,4,ls2628_,"_;\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   SATHER_STR_(20,8,ls2600_,"else {\n");
   ptr gl3360_;
   static int gl3361_;
   static union dtype_ gl3362_;
   ptr gl3363_;
   static int gl3364_;
   static union dtype_ gl3365_;

   if ((i__ == IATT_(PATT_(self__,16),12))) {
      if ((PATT_(self__,20) != 0)) {
         LST123_cprint_code_(PATT_(self__,20),outfile__);
      }
      else {
         (void)SAT99_indent_(outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
   }
   else {
      if ((j__ == IATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12),12))) {
         SWI137_cprint_if_form_code_(self__,outfile__,(i__ + 1),0);
      }
      else {
         gl3360_ = PATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12), 28 + ((j__) << 2));
         cache_dispatch_(gl3360_,1589,gl3361_,INTVAL_(gl3362_));
         VFN_(gl3362_)(gl3360_,outfile__);
         if ((COM82_dbg_mode_ == 1)) {
            DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,24)),ERR96_def_lineno_(0,IATT_(self__,24)),PATT_(outfile__,8),IATT_(outfile__,16));
         }
         else {
         }
         (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2601_));
         gl3363_ = PATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12), 28 + ((j__) << 2));
         cache_dispatch_(gl3363_,965,gl3364_,INTVAL_(gl3365_));
         VFN_(gl3365_)(gl3363_,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1788_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,28),outfile__);
         (void)SAT99_inc_ln_(SAT99_ind_inc_(SAT99_s_(outfile__,(ptr)(&ls2602_))),1);
         (void)SAT99_ind_inc_(SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2627_)),IATT_(self__,36)),'_'),i__),(ptr)(&ls2628_)),1))),(ptr)(&ls2448_)),1)),(ptr)(&ls2600_)),1));
         SWI137_cprint_if_form_code_(self__,outfile__,i__,(j__ + 1));
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);
      }
   }

   ret0__:
   return;
}

void SWI137_cprint_then_parts_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,10,ls968_,"goto_tag_");
   SATHER_STR_(20,6,ls2630_,"_: ;\n");
   SATHER_STR_(20,15,ls2627_,"goto goto_tag_");
   SATHER_STR_(20,4,ls2628_,"_;\n");
   int    i__ = S_int_VOID_;
   int    wsz__ = S_int_VOID_;

   i__ = (int)0;
   wsz__ = (int)IATT_(PATT_(self__,16),12);
   while (1) {
      if ((i__ >= wsz__)) {
         goto goto_tag_3366_;
      }
      else {
      }
      (void)SAT99_inc_ln_(SAT99_ind_inc_(SAT99_s_(SAT99_i_(SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls968_)),IATT_(self__,36)),'_'),i__),(ptr)(&ls2630_))),1);
      LST123_cprint_code_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),16),outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2627_)),IATT_(self__,36)),(ptr)(&ls2628_)),1);
      i__ = (int)(i__ + 1);
      (void)SAT99_ind_dec_(outfile__);
   }
goto_tag_3366_: ;

   ret0__:
   return;
}

void SWI137_cprint_default_code_(ptr self__, ptr outfile__, int i__, int j__)
{
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,5,ls2601_,"if (");
   SATHER_STR_(20,5,ls1788_," == ");
   SATHER_STR_(20,5,ls2602_,") {\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   SATHER_STR_(20,8,ls2600_,"else {\n");
   ptr gl3367_;
   static int gl3368_;
   static union dtype_ gl3369_;
   ptr gl3370_;
   static int gl3371_;
   static union dtype_ gl3372_;

   if ((i__ == IATT_(PATT_(self__,16),12))) {
      if ((PATT_(self__,20) != 0)) {
         LST123_cprint_code_(PATT_(self__,20),outfile__);
      }
      else {
         (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls650_)),1);
      }
   }
   else {
      if ((j__ == IATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12),12))) {
         SWI137_cprint_default_code_(self__,outfile__,(i__ + 1),0);
      }
      else {
         if ((PATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),20), 28 + ((j__) << 2)) == 0)) {
            gl3367_ = PATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12), 28 + ((j__) << 2));
            cache_dispatch_(gl3367_,1589,gl3368_,INTVAL_(gl3369_));
            VFN_(gl3369_)(gl3367_,outfile__);
            if ((COM82_dbg_mode_ == 1)) {
               DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,24)),ERR96_def_lineno_(0,IATT_(self__,24)),PATT_(outfile__,8),IATT_(outfile__,16));
            }
            else {
            }
            (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2601_));
            gl3370_ = PATT_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),12), 28 + ((j__) << 2));
            cache_dispatch_(gl3370_,965,gl3371_,INTVAL_(gl3372_));
            VFN_(gl3372_)(gl3370_,outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls1788_));
            GLO94_cprint_ctemp_name_(0,IATT_(self__,28),outfile__);
            (void)SAT99_ind_inc_(SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2602_)),1));
            LST123_cprint_code_(PATT_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),16),outfile__);
            (void)SAT99_ind_inc_(SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1)),(ptr)(&ls2600_)),1));
            SWI137_cprint_default_code_(self__,outfile__,i__,(j__ + 1));
            (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);
         }
         else {
            SWI137_cprint_default_code_(self__,outfile__,i__,(j__ + 1));
         }
      }
   }

   ret0__:
   return;
}

void SWI137_cprint_caseform_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,10,ls2635_,"default:\n");
   SATHER_STR_(20,3,ls650_,";\n");
   int    i__ = S_int_VOID_;
   int    wsz__ = S_int_VOID_;

   i__ = (int)0;
   wsz__ = (int)IATT_(PATT_(self__,16),12);
   while (1) {
      if ((i__ >= wsz__)) {
         goto goto_tag_3373_;
      }
      else {
      }
      WHE136_cprint_case_code_(PATT_(PATT_(self__,16), 28 + ((i__) << 2)),outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3373_: ;
   (void)SAT99_indent_(outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2635_)),1);
   (void)SAT99_ind_inc_(outfile__);
   SWI137_cprint_default_code_(self__,outfile__,0,0);
   (void)SAT99_indent_(outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   (void)SAT99_ind_dec_(outfile__);

   ret0__:
   return;
}


/* cond_s140.c : Sather class: COND_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void LST123_resolve_predef_types_(ptr self__, int index__);
extern void LST123_semant_(ptr self__, ptr symtab__);
extern ptr LST123_gen_temps_(ptr self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char TYP149_bool_type_p_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void SYM186_leave_new_scope_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern void STM199_cprint_code_(ptr self__, ptr outfile__);
extern /*shared*/ char COM82_dbg_mode_;
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_format_error_exit_(ptr self__, int ln__, ptr s__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_ind_dec_(ptr self__);
extern void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
extern ptr EXP117_dup_(ptr self__);
extern void EXP117_resolve_predef_types_(ptr self__, int index__);
extern void EXP117_semant_(ptr self__, ptr symtab__);
extern ptr EXP117_gen_temps_(ptr self__);
extern void LST123_gen_goto_tags_(ptr self__, ptr block__);
extern void LST123_validate_dispatches_and_get_ext_strs_(ptr self__);
extern void LST123_cprint_code_(ptr self__, ptr outfile__);
extern void EXP117_get_ext_strs_(ptr self__);
extern ptr LST123_dup_(ptr self__);
#include "macros_.h"



/*constant*/ int CON140_print_indent_ = 2;
ptr CON140_create_(ptr self__, ptr t__, ptr tp__, ptr eip__, ptr ep__, int ln__);
void CON140_out_of_line_(ptr self__, ptr fn__);
ptr CON140_dup_(ptr self__);
void CON140_put_kwdname_(ptr self__, int nm__);
ptr CON140_sather_code_(ptr self__);
ptr CON140_initialize_(ptr self__, ptr initarg__);
void CON140_resolve_predef_types_(ptr self__, int index__);
void CON140_semant_(ptr self__, ptr symtab__);
ptr CON140_typeof_(ptr self__);
int CON140_get_offset_(ptr self__);
void CON140_cprint_offset_(ptr self__, ptr outfile__);
ptr CON140_get_constval_(ptr self__);
void CON140_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void CON140_cprint_cname_(ptr self__, ptr outfile__);
void CON140_cprint_extern_(ptr self__, ptr outfile__);
void CON140_cprint_access_value_(ptr self__, ptr outfile__);
void CON140_cprint_init_code_(ptr self__, ptr outfile__);
char CON140_typechk_exprs_(ptr self__, ptr tp__);
ptr CON140_gen_temps_(ptr self__);
void CON140_gen_goto_tags_(ptr self__, ptr block__);
void CON140_validate_dispatches_and_get_ext_strs_(ptr self__);
void CON140_cprint_code_(ptr self__, ptr outfile__);
void CON140_cprint_elsif_code_(ptr self__, ptr outfile__, int level__);
extern int attr_ent_CON140[];

ptr CON140_create_(ptr self__, ptr t__, ptr tp__, ptr eip__, ptr ep__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(140,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)tp__;
   PATT_(res__,20) = (ptr)eip__;
   PATT_(res__,24) = (ptr)ep__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void CON140_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CON140_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3464_;
   static int gl3465_;
   static union dtype_ gl3466_;
   ptr gl331_;
   ptr gl3467_;
   static int gl3468_;
   static union dtype_ gl3469_;
   ptr gl332_;

   if ((PATT_(self__,24) != 0)) {
      gl3464_ = PATT_(self__,12);
      cache_dispatch_(gl3464_,471,gl3465_,INTVAL_(gl3466_));
      gl331_ = PFN_(gl3466_)(gl3464_);
      res__ = (ptr)CON140_create_(self__,gl331_,LST123_dup_(PATT_(self__,16)),LST123_dup_(PATT_(self__,20)),LST123_dup_(PATT_(self__,24)),IATT_(self__,4));
   }
   else {
      gl3467_ = PATT_(self__,12);
      cache_dispatch_(gl3467_,471,gl3468_,INTVAL_(gl3469_));
      gl332_ = PFN_(gl3469_)(gl3467_);
      res__ = (ptr)CON140_create_(self__,gl332_,LST123_dup_(PATT_(self__,16)),LST123_dup_(PATT_(self__,20)),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}

void CON140_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3470_;
   static int gl3471_;
   static union dtype_ gl3472_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3470_ = x__;
   cache_dispatch_(gl3470_,796,gl3471_,INTVAL_(gl3472_));
   IATT_(gl3470_,INTVAL_(gl3472_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON140_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON140_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void CON140_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3473_;
   static int gl3474_;
   static union dtype_ gl3475_;

   gl3473_ = PATT_(self__,12);
   cache_dispatch_(gl3473_,522,gl3474_,INTVAL_(gl3475_));
   VFN_(gl3475_)(gl3473_,index__);
   LST123_resolve_predef_types_(PATT_(self__,16),index__);
   LST123_resolve_predef_types_(PATT_(self__,20),index__);
   if ((PATT_(self__,24) != 0)) {
      LST123_resolve_predef_types_(PATT_(self__,24),index__);
   }
   else {
   }

   ret0__:
   return;
}

void CON140_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,49,ls2596_,"(COND_STMTOB_S): Test in \"if\" has no return type");
   SATHER_STR_(20,65,ls2597_,"(COND_STMTOB_S): Test in \"if\" statement should return BOOL value");
   ptr gl3476_;
   static int gl3477_;
   static union dtype_ gl3478_;
   ptr gl3479_;
   static int gl3480_;
   static union dtype_ gl3481_;
   ptr gl333_;
   ptr gl3482_;
   static int gl3483_;
   static union dtype_ gl3484_;
   ptr gl3485_;
   static int gl3486_;
   static union dtype_ gl3487_;
   ptr gl3488_;
   static int gl3489_;
   static union dtype_ gl3490_;
   ptr gl3491_;
   static int gl3492_;
   static union dtype_ gl3493_;
   ptr gl3494_;
   static int gl3495_;
   static union dtype_ gl3496_;
   char gl334_;
   ptr gl3497_;
   static int gl3498_;
   static union dtype_ gl3499_;

   gl3476_ = PATT_(self__,12);
   cache_dispatch_(gl3476_,588,gl3477_,INTVAL_(gl3478_));
   VFN_(gl3478_)(gl3476_,symtab__);
   gl3479_ = PATT_(self__,12);
   cache_dispatch_(gl3479_,1577,gl3480_,INTVAL_(gl3481_));
   gl333_ = PATT_(gl3479_,INTVAL_(gl3481_));
   if ((gl333_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2596_)));
      gl3482_ = PATT_(self__,12);
      cache_dispatch_(gl3482_,1577,gl3483_,INTVAL_(gl3484_));
      PATT_(gl3482_,INTVAL_(gl3484_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
      gl3488_ = PATT_(self__,12);
      cache_dispatch_(gl3488_,1577,gl3489_,INTVAL_(gl3490_));
      gl3485_ = PATT_(gl3488_,INTVAL_(gl3490_));
      cache_dispatch_(gl3485_,298,gl3486_,INTVAL_(gl3487_));
      IATT_(gl3485_,INTVAL_(gl3487_)) = (int)IATT_(self__,4);
   }
   else {
   }
   gl3494_ = PATT_(self__,12);
   cache_dispatch_(gl3494_,1577,gl3495_,INTVAL_(gl3496_));
   gl3491_ = PATT_(gl3494_,INTVAL_(gl3496_));
   cache_dispatch_(gl3491_,1768,gl3492_,INTVAL_(gl3493_));
   gl334_ = CFN_(gl3493_)(gl3491_);
   if ((! gl334_)) {
      ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2597_)));
      gl3497_ = PATT_(self__,12);
      cache_dispatch_(gl3497_,1577,gl3498_,INTVAL_(gl3499_));
      PATT_(gl3497_,INTVAL_(gl3499_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
   }
   else {
   }
   SYM186_enter_new_scope_(symtab__);
   LST123_semant_(PATT_(self__,16),symtab__);
   SYM186_leave_new_scope_(symtab__);
   LST123_semant_(PATT_(self__,20),symtab__);
   if ((PATT_(self__,24) != 0)) {
      SYM186_enter_new_scope_(symtab__);
      LST123_semant_(PATT_(self__,24),symtab__);
      SYM186_leave_new_scope_(symtab__);
   }
   else {
   }

   ret0__:
   return;
}

ptr CON140_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int CON140_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void CON140_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr CON140_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void CON140_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void CON140_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON140_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON140_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON140_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char CON140_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr CON140_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3500_;
   static int gl3501_;
   static union dtype_ gl3502_;

   gl3500_ = PATT_(self__,12);
   cache_dispatch_(gl3500_,1582,gl3501_,INTVAL_(gl3502_));
   res__ = (ptr)PFN_(gl3502_)(gl3500_);
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,16)));
   }
   else {
      res__ = (ptr)LST123_gen_temps_(PATT_(self__,16));
   }
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,20)));
   }
   else {
      res__ = (ptr)LST123_gen_temps_(PATT_(self__,20));
   }
   if ((PATT_(self__,24) != 0)) {
      if ((res__ != 0)) {
         res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,24)));
      }
      else {
         res__ = (ptr)LST123_gen_temps_(PATT_(self__,24));
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

void CON140_gen_goto_tags_(ptr self__, ptr block__)
{

   LST123_gen_goto_tags_(PATT_(self__,16),block__);
   LST123_gen_goto_tags_(PATT_(self__,20),block__);
   if ((PATT_(self__,24) != 0)) {
      LST123_gen_goto_tags_(PATT_(self__,24),block__);
   }
   else {
   }

   ret0__:
   return;
}

void CON140_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   ptr gl3503_;
   static int gl3504_;
   static union dtype_ gl3505_;

   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,16));
   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,20));
   if ((PATT_(self__,24) != 0)) {
      LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,24));
   }
   else {
   }
   gl3503_ = PATT_(self__,12);
   cache_dispatch_(gl3503_,1677,gl3504_,INTVAL_(gl3505_));
   VFN_(gl3505_)(gl3503_);

   ret0__:
   return;
}

void CON140_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,5,ls2601_,"if (");
   SATHER_STR_(20,5,ls2602_,") {\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   ptr gl3506_;
   static int gl3507_;
   static union dtype_ gl3508_;
   ptr gl3509_;
   static int gl3510_;
   static union dtype_ gl3511_;

   gl3506_ = PATT_(self__,12);
   cache_dispatch_(gl3506_,1589,gl3507_,INTVAL_(gl3508_));
   VFN_(gl3508_)(gl3506_,outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2601_));
   gl3509_ = PATT_(self__,12);
   cache_dispatch_(gl3509_,965,gl3510_,INTVAL_(gl3511_));
   VFN_(gl3511_)(gl3509_,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2602_)),1);
   (void)SAT99_ind_inc_(outfile__);
   LST123_cprint_code_(PATT_(self__,16),outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);
   CON140_cprint_elsif_code_(self__,outfile__,0);

   ret0__:
   return;
}

void CON140_cprint_elsif_code_(ptr self__, ptr outfile__, int level__)
{
   SATHER_STR_(20,8,ls2600_,"else {\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   ptr gl3512_;
   static int gl3513_;
   static union dtype_ gl3514_;

   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2600_)),1);
   if ((level__ == IATT_(PATT_(self__,20),12))) {
      if ((PATT_(self__,24) != 0)) {
         (void)SAT99_ind_inc_(outfile__);
         LST123_cprint_code_(PATT_(self__,24),outfile__);
         (void)SAT99_ind_dec_(outfile__);
      }
      else {
      }
   }
   else {
      (void)SAT99_ind_inc_(outfile__);
      gl3512_ = PATT_(PATT_(self__,20), 28 + ((level__) << 2));
      cache_dispatch_(gl3512_,1552,gl3513_,INTVAL_(gl3514_));
      VFN_(gl3514_)(gl3512_,outfile__);
      CON140_cprint_elsif_code_(self__,outfile__,(level__ + 1));
      (void)SAT99_ind_dec_(outfile__);
   }
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2448_)),1);

   ret0__:
   return;
}


/* elsif_139.c : Sather class: ELSIF_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr LST123_dup_(ptr self__);
extern void LST123_resolve_predef_types_(ptr self__, int index__);
extern void LST123_semant_(ptr self__, ptr symtab__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char TYP149_bool_type_p_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void SYM186_leave_new_scope_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern /*shared*/ char COM82_dbg_mode_;
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
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
extern ptr LST123_gen_temps_(ptr self__);
#include "macros_.h"



/*constant*/ int ELS139_print_indent_ = 2;
ptr ELS139_create_(ptr self__, ptr t__, ptr tp__, int ln__);
void ELS139_out_of_line_(ptr self__, ptr fn__);
ptr ELS139_dup_(ptr self__);
void ELS139_put_kwdname_(ptr self__, int nm__);
ptr ELS139_sather_code_(ptr self__);
ptr ELS139_initialize_(ptr self__, ptr initarg__);
void ELS139_resolve_predef_types_(ptr self__, int index__);
void ELS139_semant_(ptr self__, ptr symtab__);
ptr ELS139_typeof_(ptr self__);
int ELS139_get_offset_(ptr self__);
void ELS139_cprint_offset_(ptr self__, ptr outfile__);
ptr ELS139_get_constval_(ptr self__);
void ELS139_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void ELS139_cprint_cname_(ptr self__, ptr outfile__);
void ELS139_cprint_extern_(ptr self__, ptr outfile__);
void ELS139_cprint_access_value_(ptr self__, ptr outfile__);
void ELS139_cprint_init_code_(ptr self__, ptr outfile__);
char ELS139_typechk_exprs_(ptr self__, ptr tp__);
ptr ELS139_gen_temps_(ptr self__);
void ELS139_gen_goto_tags_(ptr self__, ptr block__);
void ELS139_validate_dispatches_and_get_ext_strs_(ptr self__);
void ELS139_cprint_code_(ptr self__, ptr outfile__);
extern int attr_ent_ELS139[];

ptr ELS139_create_(ptr self__, ptr t__, ptr tp__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(139,0);
   PATT_(res__,12) = (ptr)t__;
   PATT_(res__,16) = (ptr)tp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void ELS139_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ELS139_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3419_;
   static int gl3420_;
   static union dtype_ gl3421_;
   ptr gl328_;

   gl3419_ = PATT_(self__,12);
   cache_dispatch_(gl3419_,471,gl3420_,INTVAL_(gl3421_));
   gl328_ = PFN_(gl3421_)(gl3419_);
   res__ = (ptr)ELS139_create_(self__,gl328_,LST123_dup_(PATT_(self__,16)),IATT_(self__,4));

   ret0__:
   return (res__);
}

void ELS139_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3422_;
   static int gl3423_;
   static union dtype_ gl3424_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3422_ = x__;
   cache_dispatch_(gl3422_,796,gl3423_,INTVAL_(gl3424_));
   IATT_(gl3422_,INTVAL_(gl3424_)) = (int)nm__;

   ret0__:
   return;
}

ptr ELS139_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ELS139_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void ELS139_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3425_;
   static int gl3426_;
   static union dtype_ gl3427_;

   gl3425_ = PATT_(self__,12);
   cache_dispatch_(gl3425_,522,gl3426_,INTVAL_(gl3427_));
   VFN_(gl3427_)(gl3425_,index__);
   LST123_resolve_predef_types_(PATT_(self__,16),index__);

   ret0__:
   return;
}

void ELS139_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,53,ls2610_,"(ELSIF_STMTOB_S): Test in \"elsif\" has no return type");
   SATHER_STR_(20,69,ls2611_,"(ELSIF_STMTOB_S): Test in \"elsif\" statement should return BOOL value");
   ptr gl3428_;
   static int gl3429_;
   static union dtype_ gl3430_;
   ptr gl3431_;
   static int gl3432_;
   static union dtype_ gl3433_;
   ptr gl329_;
   ptr gl3434_;
   static int gl3435_;
   static union dtype_ gl3436_;
   ptr gl3437_;
   static int gl3438_;
   static union dtype_ gl3439_;
   ptr gl3440_;
   static int gl3441_;
   static union dtype_ gl3442_;
   ptr gl3443_;
   static int gl3444_;
   static union dtype_ gl3445_;
   ptr gl3446_;
   static int gl3447_;
   static union dtype_ gl3448_;
   char gl330_;
   ptr gl3449_;
   static int gl3450_;
   static union dtype_ gl3451_;

   gl3428_ = PATT_(self__,12);
   cache_dispatch_(gl3428_,588,gl3429_,INTVAL_(gl3430_));
   VFN_(gl3430_)(gl3428_,symtab__);
   gl3431_ = PATT_(self__,12);
   cache_dispatch_(gl3431_,1577,gl3432_,INTVAL_(gl3433_));
   gl329_ = PATT_(gl3431_,INTVAL_(gl3433_));
   if ((gl329_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2610_)));
      gl3434_ = PATT_(self__,12);
      cache_dispatch_(gl3434_,1577,gl3435_,INTVAL_(gl3436_));
      PATT_(gl3434_,INTVAL_(gl3436_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
      gl3440_ = PATT_(self__,12);
      cache_dispatch_(gl3440_,1577,gl3441_,INTVAL_(gl3442_));
      gl3437_ = PATT_(gl3440_,INTVAL_(gl3442_));
      cache_dispatch_(gl3437_,298,gl3438_,INTVAL_(gl3439_));
      IATT_(gl3437_,INTVAL_(gl3439_)) = (int)IATT_(self__,4);
   }
   else {
   }
   gl3446_ = PATT_(self__,12);
   cache_dispatch_(gl3446_,1577,gl3447_,INTVAL_(gl3448_));
   gl3443_ = PATT_(gl3446_,INTVAL_(gl3448_));
   cache_dispatch_(gl3443_,1768,gl3444_,INTVAL_(gl3445_));
   gl330_ = CFN_(gl3445_)(gl3443_);
   if ((! gl330_)) {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2611_)));
      gl3449_ = PATT_(self__,12);
      cache_dispatch_(gl3449_,1577,gl3450_,INTVAL_(gl3451_));
      PATT_(gl3449_,INTVAL_(gl3451_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
   }
   else {
   }
   SYM186_enter_new_scope_(symtab__);
   LST123_semant_(PATT_(self__,16),symtab__);
   SYM186_leave_new_scope_(symtab__);

   ret0__:
   return;
}

ptr ELS139_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ELS139_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void ELS139_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr ELS139_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ELS139_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void ELS139_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ELS139_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ELS139_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ELS139_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char ELS139_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr ELS139_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3452_;
   static int gl3453_;
   static union dtype_ gl3454_;

   gl3452_ = PATT_(self__,12);
   cache_dispatch_(gl3452_,1582,gl3453_,INTVAL_(gl3454_));
   res__ = (ptr)PFN_(gl3454_)(gl3452_);
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,16)));
   }
   else {
      res__ = (ptr)LST123_gen_temps_(PATT_(self__,16));
   }

   ret0__:
   return (res__);
}

void ELS139_gen_goto_tags_(ptr self__, ptr block__)
{

   LST123_gen_goto_tags_(PATT_(self__,16),block__);

   ret0__:
   return;
}

void ELS139_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   ptr gl3455_;
   static int gl3456_;
   static union dtype_ gl3457_;

   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,16));
   gl3455_ = PATT_(self__,12);
   cache_dispatch_(gl3455_,1677,gl3456_,INTVAL_(gl3457_));
   VFN_(gl3457_)(gl3455_);

   ret0__:
   return;
}

void ELS139_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,5,ls2601_,"if (");
   SATHER_STR_(20,5,ls2602_,") {\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   ptr gl3458_;
   static int gl3459_;
   static union dtype_ gl3460_;
   ptr gl3461_;
   static int gl3462_;
   static union dtype_ gl3463_;

   gl3458_ = PATT_(self__,12);
   cache_dispatch_(gl3458_,1589,gl3459_,INTVAL_(gl3460_));
   VFN_(gl3460_)(gl3458_,outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2601_));
   gl3461_ = PATT_(self__,12);
   cache_dispatch_(gl3461_,965,gl3462_,INTVAL_(gl3463_));
   VFN_(gl3463_)(gl3461_,outfile__);
   (void)SAT99_inc_ln_(SAT99_ind_inc_(SAT99_s_(outfile__,(ptr)(&ls2602_))),1);
   LST123_cprint_code_(PATT_(self__,16),outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);

   ret0__:
   return;
}


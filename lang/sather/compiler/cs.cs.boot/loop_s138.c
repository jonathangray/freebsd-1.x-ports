/* loop_s138.c : Sather class: LOOP_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void LST123_resolve_predef_types_(ptr self__, int index__);
extern void LST123_semant_(ptr self__, ptr symtab__);
extern void LST123_cprint_code_(ptr self__, ptr outfile__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern char TYP149_bool_type_p_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void SYM186_leave_new_scope_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern /*shared*/ int GLO68_g_tag_;
extern /*shared*/ char COM82_dbg_mode_;
extern void GLO94_cprint_goto_tag_(ptr self__, int i__, ptr outfile__);
extern int GLO94_global_key_(ptr self__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
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
extern void EXP117_get_ext_strs_(ptr self__);
extern ptr LST123_gen_temps_(ptr self__);
extern ptr LST123_dup_(ptr self__);
#include "macros_.h"



/*constant*/ int LOO138_print_indent_ = 2;
ptr LOO138_create_(ptr self__, ptr t__, ptr s__, int ln__);
void LOO138_out_of_line_(ptr self__, ptr fn__);
ptr LOO138_dup_(ptr self__);
void LOO138_put_kwdname_(ptr self__, int nm__);
ptr LOO138_sather_code_(ptr self__);
ptr LOO138_initialize_(ptr self__, ptr initarg__);
void LOO138_resolve_predef_types_(ptr self__, int index__);
void LOO138_semant_(ptr self__, ptr symtab__);
ptr LOO138_typeof_(ptr self__);
int LOO138_get_offset_(ptr self__);
void LOO138_cprint_offset_(ptr self__, ptr outfile__);
ptr LOO138_get_constval_(ptr self__);
void LOO138_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void LOO138_cprint_cname_(ptr self__, ptr outfile__);
void LOO138_cprint_extern_(ptr self__, ptr outfile__);
void LOO138_cprint_access_value_(ptr self__, ptr outfile__);
void LOO138_cprint_init_code_(ptr self__, ptr outfile__);
char LOO138_typechk_exprs_(ptr self__, ptr tp__);
ptr LOO138_gen_temps_(ptr self__);
void LOO138_gen_goto_tags_(ptr self__, ptr block__);
void LOO138_validate_dispatches_and_get_ext_strs_(ptr self__);
void LOO138_cprint_code_(ptr self__, ptr outfile__);
int LOO138_get_goto_tag_(ptr self__);
extern int attr_ent_LOO138[];

ptr LOO138_create_(ptr self__, ptr t__, ptr s__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(138,0);
   PATT_(res__,16) = (ptr)t__;
   PATT_(res__,20) = (ptr)s__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void LOO138_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LOO138_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3374_;
   static int gl3375_;
   static union dtype_ gl3376_;
   ptr gl325_;

   if ((PATT_(self__,16) != 0)) {
      gl3374_ = PATT_(self__,16);
      cache_dispatch_(gl3374_,471,gl3375_,INTVAL_(gl3376_));
      gl325_ = PFN_(gl3376_)(gl3374_);
      res__ = (ptr)LOO138_create_(self__,gl325_,LST123_dup_(PATT_(self__,20)),IATT_(self__,4));
   }
   else {
      res__ = (ptr)LOO138_create_(self__,0,LST123_dup_(PATT_(self__,20)),IATT_(self__,4));
   }

   ret0__:
   return (res__);
}

void LOO138_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3377_;
   static int gl3378_;
   static union dtype_ gl3379_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3377_ = x__;
   cache_dispatch_(gl3377_,796,gl3378_,INTVAL_(gl3379_));
   IATT_(gl3377_,INTVAL_(gl3379_)) = (int)nm__;

   ret0__:
   return;
}

ptr LOO138_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LOO138_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void LOO138_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3380_;
   static int gl3381_;
   static union dtype_ gl3382_;

   if ((PATT_(self__,16) != 0)) {
      gl3380_ = PATT_(self__,16);
      cache_dispatch_(gl3380_,522,gl3381_,INTVAL_(gl3382_));
      VFN_(gl3382_)(gl3380_,index__);
   }
   else {
   }
   LST123_resolve_predef_types_(PATT_(self__,20),index__);

   ret0__:
   return;
}

void LOO138_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,51,ls2614_,"(LOOP_STMTOB_S): Test in \"loop\" has no return type");
   SATHER_STR_(20,69,ls2615_,"(LOOP_STMTOB_S): Test in \"loop\" statement should return a BOOL value");
   ptr gl3383_;
   static int gl3384_;
   static union dtype_ gl3385_;
   ptr gl3386_;
   static int gl3387_;
   static union dtype_ gl3388_;
   ptr gl326_;
   ptr gl3389_;
   static int gl3390_;
   static union dtype_ gl3391_;
   ptr gl3392_;
   static int gl3393_;
   static union dtype_ gl3394_;
   ptr gl3395_;
   static int gl3396_;
   static union dtype_ gl3397_;
   ptr gl3398_;
   static int gl3399_;
   static union dtype_ gl3400_;
   ptr gl3401_;
   static int gl3402_;
   static union dtype_ gl3403_;
   char gl327_;
   ptr gl3404_;
   static int gl3405_;
   static union dtype_ gl3406_;

   if ((PATT_(self__,16) != 0)) {
      gl3383_ = PATT_(self__,16);
      cache_dispatch_(gl3383_,588,gl3384_,INTVAL_(gl3385_));
      VFN_(gl3385_)(gl3383_,symtab__);
      gl3386_ = PATT_(self__,16);
      cache_dispatch_(gl3386_,1577,gl3387_,INTVAL_(gl3388_));
      gl326_ = PATT_(gl3386_,INTVAL_(gl3388_));
      if ((gl326_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2614_)));
         gl3389_ = PATT_(self__,16);
         cache_dispatch_(gl3389_,1577,gl3390_,INTVAL_(gl3391_));
         PATT_(gl3389_,INTVAL_(gl3391_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
         gl3395_ = PATT_(self__,16);
         cache_dispatch_(gl3395_,1577,gl3396_,INTVAL_(gl3397_));
         gl3392_ = PATT_(gl3395_,INTVAL_(gl3397_));
         cache_dispatch_(gl3392_,298,gl3393_,INTVAL_(gl3394_));
         IATT_(gl3392_,INTVAL_(gl3394_)) = (int)IATT_(self__,4);
      }
      else {
      }
      gl3401_ = PATT_(self__,16);
      cache_dispatch_(gl3401_,1577,gl3402_,INTVAL_(gl3403_));
      gl3398_ = PATT_(gl3401_,INTVAL_(gl3403_));
      cache_dispatch_(gl3398_,1768,gl3399_,INTVAL_(gl3400_));
      gl327_ = CFN_(gl3400_)(gl3398_);
      if ((! gl327_)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2615_)));
         gl3404_ = PATT_(self__,16);
         cache_dispatch_(gl3404_,1577,gl3405_,INTVAL_(gl3406_));
         PATT_(gl3404_,INTVAL_(gl3406_)) = (ptr)copy_(GLO68_bool_typeob_s_,1);
      }
      else {
      }
   }
   else {
   }
   SYM186_enter_new_scope_(symtab__);
   LST123_semant_(PATT_(self__,20),symtab__);
   SYM186_leave_new_scope_(symtab__);

   ret0__:
   return;
}

ptr LOO138_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LOO138_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void LOO138_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr LOO138_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void LOO138_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void LOO138_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LOO138_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LOO138_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LOO138_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char LOO138_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr LOO138_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3407_;
   static int gl3408_;
   static union dtype_ gl3409_;

   if ((PATT_(self__,16) != 0)) {
      gl3407_ = PATT_(self__,16);
      cache_dispatch_(gl3407_,1582,gl3408_,INTVAL_(gl3409_));
      res__ = (ptr)PFN_(gl3409_)(gl3407_);
   }
   else {
   }
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,20)));
   }
   else {
      res__ = (ptr)LST123_gen_temps_(PATT_(self__,20));
   }

   ret0__:
   return (res__);
}

void LOO138_gen_goto_tags_(ptr self__, ptr block__)
{

   LST123_gen_goto_tags_(PATT_(self__,20),self__);

   ret0__:
   return;
}

void LOO138_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   ptr gl3410_;
   static int gl3411_;
   static union dtype_ gl3412_;

   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,20));
   if ((PATT_(self__,16) != 0)) {
      gl3410_ = PATT_(self__,16);
      cache_dispatch_(gl3410_,1677,gl3411_,INTVAL_(gl3412_));
      VFN_(gl3412_)(gl3410_);
   }
   else {
   }

   ret0__:
   return;
}

void LOO138_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,13,ls2617_,"while (1) {\n");
   SATHER_STR_(20,5,ls2601_,"if (");
   SATHER_STR_(20,10,ls2618_,") break;\n");
   SATHER_STR_(20,3,ls2448_,"}\n");
   SATHER_STR_(20,5,ls2619_,": ;\n");
   ptr gl3413_;
   static int gl3414_;
   static union dtype_ gl3415_;
   ptr gl3416_;
   static int gl3417_;
   static union dtype_ gl3418_;
   int    old_g_tag__ = S_int_VOID_;

   old_g_tag__ = (int)GLO68_g_tag_;
   if ((IATT_(self__,24) > 0)) {
      GLO68_g_tag_ = (int)IATT_(self__,24);
   }
   else {
   }
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2617_)),1);
   if ((PATT_(self__,16) != 0)) {
      (void)SAT99_ind_inc_(outfile__);
      gl3413_ = PATT_(self__,16);
      cache_dispatch_(gl3413_,1589,gl3414_,INTVAL_(gl3415_));
      VFN_(gl3415_)(gl3413_,outfile__);
      (void)SAT99_s_(SAT99_indent_(outfile__),(ptr)(&ls2601_));
      gl3416_ = PATT_(self__,16);
      cache_dispatch_(gl3416_,965,gl3417_,INTVAL_(gl3418_));
      VFN_(gl3418_)(gl3416_,outfile__);
      (void)SAT99_ind_dec_(SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2618_)),1));
   }
   else {
   }
   (void)SAT99_ind_inc_(outfile__);
   LST123_cprint_code_(PATT_(self__,20),outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_indent_(SAT99_ind_dec_(outfile__)),(ptr)(&ls2448_)),1);
   if ((IATT_(self__,12) > 0)) {
      (void)SAT99_ind_inc_(SAT99_indent_(SAT99_ind_dec_(outfile__)));
      GLO94_cprint_goto_tag_(0,IATT_(self__,12),outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2619_)),1);
   }
   else {
   }
   GLO68_g_tag_ = (int)old_g_tag__;
   IATT_(self__,24) = (int)(IATT_(self__,24) + 1);

   ret0__:
   return;
}

int LOO138_get_goto_tag_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((! (IATT_(self__,12) > 0))) {
      IATT_(self__,12) = (int)GLO94_global_key_(0);
   }
   else {
   }
   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}


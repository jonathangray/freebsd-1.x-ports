/* assign141.c : Sather class: ASSIGN_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int TYP149_ctype_(ptr self__);
extern void TYP149_cprint_ctype_(ptr self__, ptr outfile__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ char GLO68_semant_lval_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*shared*/ char COM82_dbg_mode_;
extern char GLO94_conform_tst_(ptr self__, ptr c1__, ptr c2__, ptr exp__);
extern int GLO94_global_key_(ptr self__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_type_mismatch_err_(ptr self__, ptr where__, ptr comnt__, ptr t1__, ptr t2__, int ln__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr LIS98_push_(ptr self__, int e__);
extern ptr LIS98_create_(ptr self__, int init_size__);
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_indent_(ptr self__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern void EXP117_out_of_line_(ptr self__, ptr fn__);
extern ptr EXP117_dup_(ptr self__);
extern void EXP117_resolve_predef_types_(ptr self__, int index__);
extern void EXP117_semant_(ptr self__, ptr symtab__);
extern char EXP117_assignable_p_(ptr self__);
extern ptr EXP117_gen_temps_(ptr self__);
extern char EXP117_to_be_pre_evaluated_(ptr self__);
extern void EXP117_get_ext_strs_(ptr self__);
extern void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
#include "macros_.h"



/*constant*/ int ASS141_print_indent_ = 2;
ptr ASS141_create_(ptr self__, ptr lv__, ptr exp__, int ln__);
void ASS141_out_of_line_(ptr self__, ptr fn__);
ptr ASS141_dup_(ptr self__);
void ASS141_put_kwdname_(ptr self__, int nm__);
ptr ASS141_sather_code_(ptr self__);
ptr ASS141_initialize_(ptr self__, ptr initarg__);
void ASS141_resolve_predef_types_(ptr self__, int index__);
void ASS141_semant_(ptr self__, ptr symtab__);
ptr ASS141_typeof_(ptr self__);
int ASS141_get_offset_(ptr self__);
void ASS141_cprint_offset_(ptr self__, ptr outfile__);
ptr ASS141_get_constval_(ptr self__);
void ASS141_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void ASS141_cprint_cname_(ptr self__, ptr outfile__);
void ASS141_cprint_extern_(ptr self__, ptr outfile__);
void ASS141_cprint_access_value_(ptr self__, ptr outfile__);
void ASS141_cprint_init_code_(ptr self__, ptr outfile__);
char ASS141_typechk_exprs_(ptr self__, ptr tp__);
ptr ASS141_gen_temps_(ptr self__);
void ASS141_gen_goto_tags_(ptr self__, ptr block__);
void ASS141_validate_dispatches_and_get_ext_strs_(ptr self__);
void ASS141_cprint_code_(ptr self__, ptr outfile__);
extern int attr_ent_ASS141[];

ptr ASS141_create_(ptr self__, ptr lv__, ptr exp__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(141,0);
   PATT_(res__,12) = (ptr)lv__;
   PATT_(res__,16) = (ptr)exp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void ASS141_out_of_line_(ptr self__, ptr fn__)
{
   ptr gl3515_;
   static int gl3516_;
   static union dtype_ gl3517_;
   ptr gl3518_;
   static int gl3519_;
   static union dtype_ gl3520_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   gl3515_ = PATT_(self__,12);
   cache_dispatch_(gl3515_,518,gl3516_,INTVAL_(gl3517_));
   VFN_(gl3517_)(gl3515_,fn__);
   gl3518_ = PATT_(self__,16);
   cache_dispatch_(gl3518_,518,gl3519_,INTVAL_(gl3520_));
   VFN_(gl3520_)(gl3518_,fn__);

   ret0__:
   return;
}

ptr ASS141_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3521_;
   static int gl3522_;
   static union dtype_ gl3523_;
   ptr gl3524_;
   static int gl3525_;
   static union dtype_ gl3526_;
   ptr gl335_;
   ptr gl336_;

   gl3521_ = PATT_(self__,12);
   cache_dispatch_(gl3521_,471,gl3522_,INTVAL_(gl3523_));
   gl335_ = PFN_(gl3523_)(gl3521_);
   gl3524_ = PATT_(self__,16);
   cache_dispatch_(gl3524_,471,gl3525_,INTVAL_(gl3526_));
   gl336_ = PFN_(gl3526_)(gl3524_);
   res__ = (ptr)ASS141_create_(self__,gl335_,gl336_,IATT_(self__,4));

   ret0__:
   return (res__);
}

void ASS141_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3527_;
   static int gl3528_;
   static union dtype_ gl3529_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3527_ = x__;
   cache_dispatch_(gl3527_,796,gl3528_,INTVAL_(gl3529_));
   IATT_(gl3527_,INTVAL_(gl3529_)) = (int)nm__;

   ret0__:
   return;
}

ptr ASS141_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ASS141_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void ASS141_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3530_;
   static int gl3531_;
   static union dtype_ gl3532_;
   ptr gl3533_;
   static int gl3534_;
   static union dtype_ gl3535_;

   gl3530_ = PATT_(self__,12);
   cache_dispatch_(gl3530_,522,gl3531_,INTVAL_(gl3532_));
   VFN_(gl3532_)(gl3530_,index__);
   gl3533_ = PATT_(self__,16);
   cache_dispatch_(gl3533_,522,gl3534_,INTVAL_(gl3535_));
   VFN_(gl3535_)(gl3533_,index__);

   ret0__:
   return;
}

void ASS141_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,42,ls2590_,"(ASSIGN_STMTOB_S): LHS has no known type\n");
   SATHER_STR_(20,45,ls2591_,"(ASSIGN_STMTOB_S): RHS does not return value");
   SATHER_STR_(20,11,ls2592_,"assignment");
   SATHER_STR_(20,1,ls1016_,"");
   SATHER_STR_(20,41,ls2594_,"(ASSIGN_STMTOB_S): LHS is not assignable");
   ptr gl3536_;
   static int gl3537_;
   static union dtype_ gl3538_;
   ptr gl3539_;
   static int gl3540_;
   static union dtype_ gl3541_;
   ptr gl337_;
   ptr gl3542_;
   static int gl3543_;
   static union dtype_ gl3544_;
   ptr gl3545_;
   static int gl3546_;
   static union dtype_ gl3547_;
   ptr gl3548_;
   static int gl3549_;
   static union dtype_ gl3550_;
   ptr gl3551_;
   static int gl3552_;
   static union dtype_ gl3553_;
   ptr gl338_;
   ptr gl3554_;
   static int gl3555_;
   static union dtype_ gl3556_;
   ptr gl3557_;
   static int gl3558_;
   static union dtype_ gl3559_;
   ptr gl3560_;
   static int gl3561_;
   static union dtype_ gl3562_;
   ptr gl339_;
   ptr gl340_;
   ptr gl3563_;
   static int gl3564_;
   static union dtype_ gl3565_;
   ptr gl3566_;
   static int gl3567_;
   static union dtype_ gl3568_;
   ptr gl341_;
   ptr gl342_;
   ptr gl3569_;
   static int gl3570_;
   static union dtype_ gl3571_;
   ptr gl3572_;
   static int gl3573_;
   static union dtype_ gl3574_;
   ptr    new_res_type__ = 0;

   GLO68_semant_lval_ = (char)1;
   gl3536_ = PATT_(self__,12);
   cache_dispatch_(gl3536_,588,gl3537_,INTVAL_(gl3538_));
   VFN_(gl3538_)(gl3536_,symtab__);
   gl3539_ = PATT_(self__,12);
   cache_dispatch_(gl3539_,1577,gl3540_,INTVAL_(gl3541_));
   gl337_ = PATT_(gl3539_,INTVAL_(gl3541_));
   if ((gl337_ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2590_)));
      gl3542_ = PATT_(self__,12);
      cache_dispatch_(gl3542_,1577,gl3543_,INTVAL_(gl3544_));
      PATT_(gl3542_,INTVAL_(gl3544_)) = (ptr)GLO68_ob_typeob_s_;
   }
   else {
   }
   GLO68_semant_lval_ = (char)0;
   gl3545_ = PATT_(self__,12);
   cache_dispatch_(gl3545_,1676,gl3546_,INTVAL_(gl3547_));
   if (CFN_(gl3547_)(gl3545_)) {
      gl3548_ = PATT_(self__,16);
      cache_dispatch_(gl3548_,588,gl3549_,INTVAL_(gl3550_));
      VFN_(gl3550_)(gl3548_,symtab__);
      gl3551_ = PATT_(self__,16);
      cache_dispatch_(gl3551_,1577,gl3552_,INTVAL_(gl3553_));
      gl338_ = PATT_(gl3551_,INTVAL_(gl3553_));
      if ((gl338_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2591_)));
         gl3554_ = PATT_(self__,16);
         cache_dispatch_(gl3554_,1577,gl3555_,INTVAL_(gl3556_));
         PATT_(gl3554_,INTVAL_(gl3556_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
         gl3557_ = PATT_(self__,16);
         cache_dispatch_(gl3557_,1577,gl3558_,INTVAL_(gl3559_));
         gl339_ = PATT_(gl3557_,INTVAL_(gl3559_));
         gl3560_ = PATT_(self__,12);
         cache_dispatch_(gl3560_,1577,gl3561_,INTVAL_(gl3562_));
         gl340_ = PATT_(gl3560_,INTVAL_(gl3562_));
         if ((! GLO94_conform_tst_(0,gl339_,gl340_,PATT_(self__,16)))) {
            gl3563_ = PATT_(self__,16);
            cache_dispatch_(gl3563_,1577,gl3564_,INTVAL_(gl3565_));
            gl341_ = PATT_(gl3563_,INTVAL_(gl3565_));
            gl3566_ = PATT_(self__,12);
            cache_dispatch_(gl3566_,1577,gl3567_,INTVAL_(gl3568_));
            gl342_ = PATT_(gl3566_,INTVAL_(gl3568_));
            ERR96_type_mismatch_err_(0,(ptr)(&ls2592_),(ptr)(&ls1016_),gl341_,gl342_,IATT_(self__,4));
            new_res_type__ = (ptr)copy_(GLO68_ob_typeob_s_,1);
            gl3569_ = PATT_(self__,12);
            cache_dispatch_(gl3569_,1577,gl3570_,INTVAL_(gl3571_));
            PATT_(gl3569_,INTVAL_(gl3571_)) = (ptr)new_res_type__;
         }
         else {
         }
      }
   }
   else {
      ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls2594_)));
      gl3572_ = PATT_(self__,16);
      cache_dispatch_(gl3572_,1577,gl3573_,INTVAL_(gl3574_));
      PATT_(gl3572_,INTVAL_(gl3574_)) = (ptr)GLO68_ob_typeob_s_;
   }

   ret0__:
   return;
}

ptr ASS141_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ASS141_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void ASS141_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr ASS141_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ASS141_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void ASS141_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ASS141_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ASS141_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ASS141_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char ASS141_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr ASS141_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3575_;
   static int gl3576_;
   static union dtype_ gl3577_;
   ptr gl3578_;
   static int gl3579_;
   static union dtype_ gl3580_;
   ptr gl343_;
   ptr gl3581_;
   static int gl3582_;
   static union dtype_ gl3583_;
   ptr gl3584_;
   static int gl3585_;
   static union dtype_ gl3586_;
   ptr gl3587_;
   static int gl3588_;
   static union dtype_ gl3589_;
   char gl344_;
   char gl345_;
   ptr gl3590_;
   static int gl3591_;
   static union dtype_ gl3592_;
   ptr gl3593_;
   static int gl3594_;
   static union dtype_ gl3595_;
   char    need_to_pre_eval_rhs__ = S_char_VOID_;

   gl3575_ = PATT_(self__,12);
   cache_dispatch_(gl3575_,1582,gl3576_,INTVAL_(gl3577_));
   res__ = (ptr)PFN_(gl3577_)(gl3575_);
   if ((res__ != 0)) {
      gl3578_ = PATT_(self__,16);
      cache_dispatch_(gl3578_,1582,gl3579_,INTVAL_(gl3580_));
      gl343_ = PFN_(gl3580_)(gl3578_);
      res__ = (ptr)LIS98_append_(res__,gl343_);
   }
   else {
      gl3581_ = PATT_(self__,16);
      cache_dispatch_(gl3581_,1582,gl3582_,INTVAL_(gl3583_));
      res__ = (ptr)PFN_(gl3583_)(gl3581_);
   }
   gl3584_ = PATT_(self__,16);
   cache_dispatch_(gl3584_,1678,gl3585_,INTVAL_(gl3586_));
   gl344_ = CFN_(gl3586_)(gl3584_);
   gl3587_ = PATT_(self__,12);
   cache_dispatch_(gl3587_,1678,gl3588_,INTVAL_(gl3589_));
   gl345_ = CFN_(gl3589_)(gl3587_);
   need_to_pre_eval_rhs__ = (char)(gl344_ & gl345_);
   if ((IATT_(self__,20) > 0)) {
      if ((res__ != 0)) {
         res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,20)),IATT_(self__,24));
      }
      else {
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,20)),IATT_(self__,24));
      }
   }
   else {
      if ((IATT_(self__,20) == 0)) {
         if (need_to_pre_eval_rhs__) {
            IATT_(self__,20) = (int)GLO94_global_key_(0);
            gl3593_ = PATT_(self__,16);
            cache_dispatch_(gl3593_,1577,gl3594_,INTVAL_(gl3595_));
            gl3590_ = PATT_(gl3593_,INTVAL_(gl3595_));
            cache_dispatch_(gl3590_,374,gl3591_,INTVAL_(gl3592_));
            IATT_(self__,24) = (int)IFN_(gl3592_)(gl3590_);
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,20)),IATT_(self__,24));
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,20)),IATT_(self__,24));
            }
         }
         else {
            IATT_(self__,20) = (int)(- 1);
         }
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

void ASS141_gen_goto_tags_(ptr self__, ptr block__)
{


   ret0__:
   return;
}

void ASS141_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   ptr gl3596_;
   static int gl3597_;
   static union dtype_ gl3598_;
   ptr gl3599_;
   static int gl3600_;
   static union dtype_ gl3601_;

   gl3596_ = PATT_(self__,12);
   cache_dispatch_(gl3596_,1677,gl3597_,INTVAL_(gl3598_));
   VFN_(gl3598_)(gl3596_);
   gl3599_ = PATT_(self__,16);
   cache_dispatch_(gl3599_,1677,gl3600_,INTVAL_(gl3601_));
   VFN_(gl3601_)(gl3599_);

   ret0__:
   return;
}

void ASS141_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,5,ls1590_," = (");
   SATHER_STR_(20,2,ls1278_,")");
   ptr gl3602_;
   static int gl3603_;
   static union dtype_ gl3604_;
   ptr gl3605_;
   static int gl3606_;
   static union dtype_ gl3607_;
   ptr gl3608_;
   static int gl3609_;
   static union dtype_ gl3610_;
   ptr gl3611_;
   static int gl3612_;
   static union dtype_ gl3613_;
   ptr gl3614_;
   static int gl3615_;
   static union dtype_ gl3616_;
   ptr gl3617_;
   static int gl3618_;
   static union dtype_ gl3619_;
   ptr gl3620_;
   static int gl3621_;
   static union dtype_ gl3622_;
   ptr gl3623_;
   static int gl3624_;
   static union dtype_ gl3625_;

   gl3602_ = PATT_(self__,16);
   cache_dispatch_(gl3602_,1589,gl3603_,INTVAL_(gl3604_));
   VFN_(gl3604_)(gl3602_,outfile__);
   if ((IATT_(self__,20) > 0)) {
      GLO94_cprint_ctemp_name_(0,IATT_(self__,20),outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl3605_ = PATT_(self__,16);
      cache_dispatch_(gl3605_,965,gl3606_,INTVAL_(gl3607_));
      VFN_(gl3607_)(gl3605_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }
   gl3608_ = PATT_(self__,12);
   cache_dispatch_(gl3608_,1589,gl3609_,INTVAL_(gl3610_));
   VFN_(gl3610_)(gl3608_,outfile__);
   (void)SAT99_indent_(outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   if ((IATT_(self__,20) > 0)) {
      gl3611_ = PATT_(self__,12);
      cache_dispatch_(gl3611_,965,gl3612_,INTVAL_(gl3613_));
      VFN_(gl3613_)(gl3611_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      GLO94_cprint_ctemp_name_(0,IATT_(self__,20),outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
      gl3614_ = PATT_(self__,12);
      cache_dispatch_(gl3614_,965,gl3615_,INTVAL_(gl3616_));
      VFN_(gl3616_)(gl3614_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls1590_));
      gl3620_ = PATT_(self__,12);
      cache_dispatch_(gl3620_,1577,gl3621_,INTVAL_(gl3622_));
      gl3617_ = PATT_(gl3620_,INTVAL_(gl3622_));
      cache_dispatch_(gl3617_,696,gl3618_,INTVAL_(gl3619_));
      VFN_(gl3619_)(gl3617_,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
      gl3623_ = PATT_(self__,16);
      cache_dispatch_(gl3623_,965,gl3624_,INTVAL_(gl3625_));
      VFN_(gl3625_)(gl3623_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }

   ret0__:
   return;
}


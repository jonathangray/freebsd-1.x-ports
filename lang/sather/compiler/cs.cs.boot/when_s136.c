/* when_s136.c : Sather class: WHEN_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void LST120_resolve_predef_types_(ptr self__, int index__);
extern void LST120_semant_(ptr self__, ptr symtab__);
extern ptr LST123_dup_(ptr self__);
extern void LST123_resolve_predef_types_(ptr self__, int index__);
extern ptr LST120_create_(ptr self__, int init_size__);
extern char TYP149_int_type_p_(ptr self__);
extern char TYP149_char_type_p_(ptr self__);
extern void SYM186_leave_new_scope_(ptr self__);
extern void SYM186_enter_new_scope_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
extern /*shared*/ char COM82_dbg_mode_;
extern char GLO94_conform_tst_(ptr self__, ptr c1__, ptr c2__, ptr exp__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern void ERR96_type_mismatch_err_(ptr self__, ptr where__, ptr comnt__, ptr t1__, ptr t2__, int ln__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr SAT99_indent_(ptr self__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_ind_inc_(ptr self__);
extern ptr SAT99_ind_dec_(ptr self__);
extern ptr EXP117_expr_eval_constant_(ptr self__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
extern ptr LST120_gen_temps_(ptr self__);
extern void LST120_get_ext_strs_(ptr self__);
extern ptr LST120_dup_(ptr self__);
extern void LST123_semant_(ptr self__, ptr symtab__);
extern ptr LST123_gen_temps_(ptr self__);
extern void LST123_gen_goto_tags_(ptr self__, ptr block__);
extern void LST123_validate_dispatches_and_get_ext_strs_(ptr self__);
extern void LST123_cprint_code_(ptr self__, ptr outfile__);
#include "macros_.h"



/*constant*/ int WHE136_print_indent_ = 2;
ptr WHE136_create_(ptr self__, ptr e__, ptr t__, int ln__);
void WHE136_out_of_line_(ptr self__, ptr fn__);
ptr WHE136_dup_(ptr self__);
void WHE136_put_kwdname_(ptr self__, int nm__);
ptr WHE136_sather_code_(ptr self__);
ptr WHE136_initialize_(ptr self__, ptr initarg__);
void WHE136_resolve_predef_types_(ptr self__, int index__);
void WHE136_semant_(ptr self__, ptr symtab__);
ptr WHE136_typeof_(ptr self__);
int WHE136_get_offset_(ptr self__);
void WHE136_cprint_offset_(ptr self__, ptr outfile__);
ptr WHE136_get_constval_(ptr self__);
void WHE136_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void WHE136_cprint_cname_(ptr self__, ptr outfile__);
void WHE136_cprint_extern_(ptr self__, ptr outfile__);
void WHE136_cprint_access_value_(ptr self__, ptr outfile__);
void WHE136_cprint_init_code_(ptr self__, ptr outfile__);
char WHE136_typechk_exprs_(ptr self__, ptr tp__);
ptr WHE136_gen_temps_(ptr self__);
void WHE136_gen_goto_tags_(ptr self__, ptr block__);
void WHE136_validate_dispatches_and_get_ext_strs_(ptr self__);
void WHE136_cprint_code_(ptr self__, ptr outfile__);
void WHE136_cprint_case_code_(ptr self__, ptr outfile__);
extern int attr_ent_WHE136[];

ptr WHE136_create_(ptr self__, ptr e__, ptr t__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(136,0);
   PATT_(res__,12) = (ptr)e__;
   PATT_(res__,16) = (ptr)t__;
   IATT_(res__,24) = (int)ln__;

   ret0__:
   return (res__);
}

void WHE136_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,24) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,24));

   ret0__:
   return;
}

ptr WHE136_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)WHE136_create_(self__,LST120_dup_(PATT_(self__,12)),LST123_dup_(PATT_(self__,16)),IATT_(self__,24));

   ret0__:
   return (res__);
}

void WHE136_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3253_;
   static int gl3254_;
   static union dtype_ gl3255_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3253_ = x__;
   cache_dispatch_(gl3253_,796,gl3254_,INTVAL_(gl3255_));
   IATT_(gl3253_,INTVAL_(gl3255_)) = (int)nm__;

   ret0__:
   return;
}

ptr WHE136_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr WHE136_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void WHE136_resolve_predef_types_(ptr self__, int index__)
{

   LST120_resolve_predef_types_(PATT_(self__,12),index__);
   LST123_resolve_predef_types_(PATT_(self__,16),index__);

   ret0__:
   return;
}

void WHE136_semant_(ptr self__, ptr symtab__)
{
   ptr gl3256_;
   static int gl3257_;
   static union dtype_ gl3258_;
   ptr gl3259_;
   static int gl3260_;
   static union dtype_ gl3261_;
   ptr gl3262_;
   static int gl3263_;
   static union dtype_ gl3264_;
   ptr gl3265_;
   static int gl3266_;
   static union dtype_ gl3267_;
   char gl313_;
   char gl314_;
   int    i__ = S_int_VOID_;
   int    esz__ = S_int_VOID_;
   ptr    etype__ = 0;

   LST120_semant_(PATT_(self__,12),symtab__);
   i__ = (int)0;
   esz__ = (int)IATT_(PATT_(self__,12),12);
   PATT_(self__,20) = (ptr)LST120_create_(0,esz__);
   CATT_(self__,4) = (char)1;
   while (1) {
      if ((i__ >= esz__)) {
         goto goto_tag_3268_;
      }
      else {
      }
      gl3256_ = PATT_(PATT_(self__,12), 28 + ((i__) << 2));
      cache_dispatch_(gl3256_,1596,gl3257_,INTVAL_(gl3258_));
      PATT_(PATT_(self__,20), 28 + ((i__) << 2)) = (ptr)PFN_(gl3258_)(gl3256_);
      if ((PATT_(PATT_(self__,20), 28 + ((i__) << 2)) != 0)) {
         gl3259_ = PATT_(PATT_(self__,20), 28 + ((i__) << 2));
         cache_dispatch_(gl3259_,1577,gl3260_,INTVAL_(gl3261_));
         etype__ = (ptr)PATT_(gl3259_,INTVAL_(gl3261_));
         gl3262_ = etype__;
         cache_dispatch_(gl3262_,1778,gl3263_,INTVAL_(gl3264_));
         gl313_ = CFN_(gl3264_)(gl3262_);
         gl3265_ = etype__;
         cache_dispatch_(gl3265_,2029,gl3266_,INTVAL_(gl3267_));
         gl314_ = CFN_(gl3267_)(gl3265_);
         if ((! (gl313_ | gl314_))) {
            PATT_(PATT_(self__,20), 28 + ((i__) << 2)) = (ptr)0;
         }
         else {
         }
      }
      else {
      }
      CATT_(self__,4) = (char)(CATT_(self__,4) & (PATT_(PATT_(self__,20), 28 + ((i__) << 2)) != 0));
      i__ = (int)(i__ + 1);
   }
goto_tag_3268_: ;
   SYM186_enter_new_scope_(symtab__);
   LST123_semant_(PATT_(self__,16),symtab__);
   SYM186_leave_new_scope_(symtab__);

   ret0__:
   return;
}

ptr WHE136_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int WHE136_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void WHE136_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr WHE136_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void WHE136_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void WHE136_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void WHE136_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void WHE136_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void WHE136_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char WHE136_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,14,ls2331_,"WHEN_STMTOB_S");
   SATHER_STR_(20,35,ls2638_,"Type of \"when\" expressions unknown");
   SATHER_STR_(20,5,ls22_,"when");
   SATHER_STR_(20,1,ls1016_,"");
   ptr gl3269_;
   static int gl3270_;
   static union dtype_ gl3271_;
   ptr gl315_;
   ptr gl3272_;
   static int gl3273_;
   static union dtype_ gl3274_;
   ptr gl316_;
   ptr gl3275_;
   static int gl3276_;
   static union dtype_ gl3277_;
   ptr gl317_;
   ptr gl3278_;
   static int gl3279_;
   static union dtype_ gl3280_;
   int    i__ = S_int_VOID_;
   int    esz__ = S_int_VOID_;

   i__ = (int)0;
   esz__ = (int)IATT_(PATT_(self__,12),12);
   res__ = (char)1;
   while (1) {
      if ((i__ >= esz__)) {
         goto goto_tag_3281_;
      }
      else {
      }
      gl3269_ = PATT_(PATT_(self__,12), 28 + ((i__) << 2));
      cache_dispatch_(gl3269_,1577,gl3270_,INTVAL_(gl3271_));
      gl315_ = PATT_(gl3269_,INTVAL_(gl3271_));
      if ((gl315_ == 0)) {
         ERR96_compiler_error_msg_(0,(ptr)(&ls2331_),(ptr)(&ls2638_));
      }
      else {
         gl3272_ = PATT_(PATT_(self__,12), 28 + ((i__) << 2));
         cache_dispatch_(gl3272_,1577,gl3273_,INTVAL_(gl3274_));
         gl316_ = PATT_(gl3272_,INTVAL_(gl3274_));
         if ((! GLO94_conform_tst_(0,gl316_,tp__,PATT_(PATT_(self__,12), 28 + ((i__) << 2))))) {
            gl3275_ = PATT_(PATT_(self__,12), 28 + ((i__) << 2));
            cache_dispatch_(gl3275_,1577,gl3276_,INTVAL_(gl3277_));
            gl317_ = PATT_(gl3275_,INTVAL_(gl3277_));
            ERR96_type_mismatch_err_(0,(ptr)(&ls22_),(ptr)(&ls1016_),gl317_,tp__,IATT_(self__,24));
            res__ = (char)0;
            gl3278_ = PATT_(PATT_(self__,12), 28 + ((i__) << 2));
            cache_dispatch_(gl3278_,1577,gl3279_,INTVAL_(gl3280_));
            PATT_(gl3278_,INTVAL_(gl3280_)) = (ptr)tp__;
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3281_: ;

   ret0__:
   return (res__);
}

ptr WHE136_gen_temps_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)LST120_gen_temps_(PATT_(self__,12));
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST123_gen_temps_(PATT_(self__,16)));
   }
   else {
      res__ = (ptr)LST123_gen_temps_(PATT_(self__,16));
   }

   ret0__:
   return (res__);
}

void WHE136_gen_goto_tags_(ptr self__, ptr block__)
{

   LST123_gen_goto_tags_(PATT_(self__,16),block__);

   ret0__:
   return;
}

void WHE136_validate_dispatches_and_get_ext_strs_(ptr self__)
{

   LST123_validate_dispatches_and_get_ext_strs_(PATT_(self__,16));
   LST120_get_ext_strs_(PATT_(self__,12));

   ret0__:
   return;
}

void WHE136_cprint_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void WHE136_cprint_case_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,7,ls2641_,"case (");
   SATHER_STR_(20,5,ls2642_,") :\n");
   SATHER_STR_(20,8,ls2643_,"break;\n");
   ptr gl3282_;
   static int gl3283_;
   static union dtype_ gl3284_;
   int    i__ = S_int_VOID_;
   int    esz__ = S_int_VOID_;
   char    case_printed__ = S_char_VOID_;

   i__ = (int)0;
   esz__ = (int)IATT_(PATT_(self__,12),12);
   case_printed__ = S_char_VOID_;
   while (1) {
      if ((i__ >= esz__)) {
         goto goto_tag_3285_;
      }
      else {
      }
      if ((PATT_(PATT_(self__,20), 28 + ((i__) << 2)) != 0)) {
         (void)SAT99_indent_(outfile__);
         if ((COM82_dbg_mode_ == 1)) {
            DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,24)),ERR96_def_lineno_(0,IATT_(self__,24)),PATT_(outfile__,8),IATT_(outfile__,16));
         }
         else {
         }
         (void)SAT99_s_(outfile__,(ptr)(&ls2641_));
         gl3282_ = PATT_(PATT_(self__,20), 28 + ((i__) << 2));
         cache_dispatch_(gl3282_,965,gl3283_,INTVAL_(gl3284_));
         VFN_(gl3284_)(gl3282_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2642_)),1);
         case_printed__ = (char)1;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3285_: ;
   if (case_printed__) {
      (void)SAT99_ind_inc_(outfile__);
      LST123_cprint_code_(PATT_(self__,16),outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(SAT99_ind_dec_(SAT99_indent_(outfile__)),(ptr)(&ls2643_)),1);
   }
   else {
   }

   ret0__:
   return;
}


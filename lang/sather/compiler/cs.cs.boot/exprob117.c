/* exprob117.c : Sather class: EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_(ptr self__);
extern ptr TYP149_dup_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int EXP117_print_indent_ = 2;
ptr EXP117_create_(ptr self__, int ln__);
void EXP117_out_of_line_(ptr self__, ptr fn__);
ptr EXP117_dup_(ptr self__);
void EXP117_put_kwdname_(ptr self__, int nm__);
ptr EXP117_sather_code_(ptr self__);
ptr EXP117_initialize_(ptr self__, ptr initarg__);
void EXP117_resolve_predef_types_(ptr self__, int index__);
void EXP117_semant_(ptr self__, ptr symtab__);
ptr EXP117_typeof_(ptr self__);
int EXP117_get_offset_(ptr self__);
void EXP117_cprint_offset_(ptr self__, ptr outfile__);
ptr EXP117_get_constval_(ptr self__);
void EXP117_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void EXP117_cprint_cname_(ptr self__, ptr outfile__);
void EXP117_cprint_extern_(ptr self__, ptr outfile__);
void EXP117_cprint_access_value_(ptr self__, ptr outfile__);
void EXP117_cprint_init_code_(ptr self__, ptr outfile__);
char EXP117_valid_init_expr_(ptr self__);
char EXP117_assignable_p_(ptr self__);
ptr EXP117_gen_temps_(ptr self__);
ptr EXP117_expr_eval_constant_(ptr self__);
void EXP117_get_ext_strs_(ptr self__);
char EXP117_to_be_pre_evaluated_(ptr self__);
char EXP117_access_value_only_(ptr self__);
void EXP117_fob_error_(ptr self__, ptr op_name__, ptr cls_name__);
void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
void EXP117_cprint_cast_code_(ptr self__, ptr outfile__);
extern int attr_ent_EXP117[];

ptr EXP117_create_(ptr self__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(117,0);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void EXP117_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr EXP117_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl2925_;
   static int gl2926_;
   static union dtype_ gl2927_;

   res__ = (ptr)new_(117,0);
   if ((PATT_(self__,12) != 0)) {
      gl2925_ = PATT_(self__,12);
      cache_dispatch_(gl2925_,471,gl2926_,INTVAL_(gl2927_));
      PATT_(res__,12) = (ptr)PFN_(gl2927_)(gl2925_);
   }
   else {
   }
   IATT_(res__,16) = (int)IATT_(self__,16);
   IATT_(res__,4) = (int)IATT_(self__,4);

   ret0__:
   return (res__);
}

void EXP117_put_kwdname_(ptr self__, int nm__)
{
   ptr gl2928_;
   static int gl2929_;
   static union dtype_ gl2930_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2928_ = x__;
   cache_dispatch_(gl2928_,796,gl2929_,INTVAL_(gl2930_));
   IATT_(gl2928_,INTVAL_(gl2930_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXP117_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXP117_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void EXP117_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void EXP117_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr EXP117_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int EXP117_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void EXP117_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr EXP117_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void EXP117_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void EXP117_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXP117_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXP117_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXP117_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char EXP117_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

char EXP117_assignable_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr EXP117_gen_temps_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr EXP117_expr_eval_constant_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void EXP117_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

char EXP117_to_be_pre_evaluated_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char EXP117_access_value_only_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

void EXP117_fob_error_(ptr self__, ptr op_name__, ptr cls_name__)
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

void EXP117_cprint_pre_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXP117_cprint_act_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void EXP117_cprint_cast_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,2,ls1493_,"(");
   SATHER_STR_(20,9,ls350_,"EXPROB_S");
   SATHER_STR_(20,20,ls1685_,"Invalid C type cast");
   SATHER_STR_(20,2,ls1278_,")");

   if ((IATT_(self__,20) != 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls1493_));
      switch (IATT_(self__,20)) {
         case (1) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1215_));
            break;
         case (2) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1216_));
            break;
         case (3) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1217_));
            break;
         case (4) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1218_));
            break;
         case (5) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1219_));
            break;
         default:
            ERR96_compiler_error_msg_(0,(ptr)(&ls350_),(ptr)(&ls1685_));
            ;
      }
      (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
   }
   else {
   }

   ret0__:
   return;
}


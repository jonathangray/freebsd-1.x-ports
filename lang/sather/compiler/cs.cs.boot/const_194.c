/* const_194.c : Sather class: CONST_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern ptr STR12_create_(ptr self__, ptr st__);
extern ptr STR20_create_(ptr self__);
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



/*constant*/ int CON194_print_indent_ = 2;
ptr CON194_create_(ptr self__, int ln__);
void CON194_out_of_line_(ptr self__, ptr fn__);
ptr CON194_dup_(ptr self__);
void CON194_put_kwdname_(ptr self__, int nm__);
ptr CON194_sather_code_(ptr self__);
ptr CON194_initialize_(ptr self__, ptr initarg__);
void CON194_resolve_predef_types_(ptr self__, int index__);
void CON194_semant_(ptr self__, ptr symtab__);
ptr CON194_typeof_(ptr self__);
int CON194_get_offset_(ptr self__);
void CON194_cprint_offset_(ptr self__, ptr outfile__);
ptr CON194_get_constval_(ptr self__);
void CON194_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void CON194_cprint_cname_(ptr self__, ptr outfile__);
void CON194_cprint_extern_(ptr self__, ptr outfile__);
void CON194_cprint_access_value_(ptr self__, ptr outfile__);
void CON194_cprint_init_code_(ptr self__, ptr outfile__);
char CON194_valid_init_expr_(ptr self__);
char CON194_assignable_p_(ptr self__);
ptr CON194_gen_temps_(ptr self__);
ptr CON194_expr_eval_constant_(ptr self__);
void CON194_get_ext_strs_(ptr self__);
char CON194_to_be_pre_evaluated_(ptr self__);
char CON194_access_value_only_(ptr self__);
void CON194_fob_error_(ptr self__, ptr op_name__, ptr cls_name__);
void CON194_cprint_pre_code_(ptr self__, ptr outfile__);
void CON194_cprint_act_code_(ptr self__, ptr outfile__);
void CON194_cprint_cast_code_(ptr self__, ptr outfile__);
ptr CON194_val_(ptr self__);
extern int attr_ent_CON194[];

ptr CON194_create_(ptr self__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(194,0);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void CON194_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr CON194_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void CON194_put_kwdname_(ptr self__, int nm__)
{
   ptr gl4712_;
   static int gl4713_;
   static union dtype_ gl4714_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4712_ = x__;
   cache_dispatch_(gl4712_,796,gl4713_,INTVAL_(gl4714_));
   IATT_(gl4712_,INTVAL_(gl4714_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON194_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON194_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void CON194_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void CON194_semant_(ptr self__, ptr symtab__)
{

   PATT_(self__,12) = (ptr)GLO68_ob_typeob_s_;

   ret0__:
   return;
}

ptr CON194_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int CON194_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void CON194_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr CON194_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void CON194_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void CON194_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON194_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON194_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON194_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char CON194_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

char CON194_assignable_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr CON194_gen_temps_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON194_expr_eval_constant_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void CON194_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

char CON194_to_be_pre_evaluated_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char CON194_access_value_only_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

void CON194_fob_error_(ptr self__, ptr op_name__, ptr cls_name__)
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

void CON194_cprint_pre_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON194_cprint_act_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void CON194_cprint_cast_code_(ptr self__, ptr outfile__)
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

ptr CON194_val_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,1,ls1016_,"");

   res__ = (ptr)STR12_create_(0,(ptr)(&ls1016_));

   ret0__:
   return (res__);
}


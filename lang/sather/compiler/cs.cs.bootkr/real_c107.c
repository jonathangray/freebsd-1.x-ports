/* real_c107.c : Sather class: REAL_CONST_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_real_typeob_s_;
extern ptr STR12_create_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern ERR96_compiler_error_msg_();
extern ptr SAT99_s_();
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



/*constant*/ int REA107_print_indent_ = 2;
ptr REA107_create_();
REA107_out_of_line_();
ptr REA107_dup_();
REA107_put_kwdname_();
ptr REA107_sather_code_();
ptr REA107_initialize_();
REA107_resolve_predef_types_();
REA107_semant_();
ptr REA107_typeof_();
int REA107_get_offset_();
REA107_cprint_offset_();
ptr REA107_get_constval_();
REA107_cont_cprint_code_();
REA107_cprint_cname_();
REA107_cprint_extern_();
REA107_cprint_access_value_();
REA107_cprint_init_code_();
char REA107_valid_init_expr_();
char REA107_assignable_p_();
ptr REA107_gen_temps_();
ptr REA107_expr_eval_constant_();
REA107_get_ext_strs_();
char REA107_to_be_pre_evaluated_();
char REA107_access_value_only_();
REA107_fob_error_();
REA107_cprint_pre_code_();
REA107_cprint_act_code_();
REA107_cprint_cast_code_();
ptr REA107_val_();
extern int attr_ent_REA107[];

ptr REA107_create_(self__,v__,ln__)
ptr self__;
ptr v__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(107,0);
   PATT_(res__,28) = (ptr)v__;
   IATT_(res__,4) = (int)ln__;
   PATT_(res__,12) = (ptr)GLO68_real_typeob_s_;

   ret0__:
   return (res__);
}

REA107_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr REA107_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

REA107_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1345_;
   static int gl1346_;
   static union dtype_ gl1347_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1345_ = x__;
   cache_dispatch_(gl1345_,796,gl1346_,INTVAL_(gl1347_));
   IATT_(gl1345_,INTVAL_(gl1347_)) = (int)nm__;

   ret0__:
   return;
}

ptr REA107_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_s_(STR20_create_(0),PATT_(self__,28));

   ret0__:
   return (res__);
}

ptr REA107_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

REA107_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

REA107_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr REA107_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int REA107_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

REA107_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr REA107_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

REA107_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

REA107_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

REA107_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

REA107_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

REA107_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char REA107_valid_init_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

char REA107_assignable_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr REA107_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr REA107_expr_eval_constant_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

REA107_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

char REA107_to_be_pre_evaluated_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char REA107_access_value_only_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

REA107_fob_error_(self__,op_name__,cls_name__)
ptr self__;
ptr op_name__;
ptr cls_name__;
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

REA107_cprint_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

REA107_cprint_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   REA107_cprint_cast_code_(self__,outfile__);
   (void)SAT99_s_(outfile__,PATT_(self__,28));

   ret0__:
   return;
}

REA107_cprint_cast_code_(self__,outfile__)
ptr self__;
ptr outfile__;
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

ptr REA107_val_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR12_create_(0,PATT_(self__,28));

   ret0__:
   return (res__);
}


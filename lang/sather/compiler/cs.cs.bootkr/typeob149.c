/* typeob149.c : Sather class: TYPEOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr SAT99_s_();
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern CLA148_cprint_ctype_();
extern /*constant*/ int C_T168_c_ptr_;
#include "macros_.h"



/*constant*/ int TYP149_print_indent_ = 2;
ptr TYP149_create_();
TYP149_out_of_line_();
ptr TYP149_dup_();
TYP149_put_kwdname_();
ptr TYP149_sather_code_();
ptr TYP149_initialize_();
TYP149_resolve_predef_types_();
TYP149_semant_();
ptr TYP149_typeof_();
int TYP149_get_offset_();
TYP149_cprint_offset_();
ptr TYP149_get_constval_();
TYP149_cont_cprint_code_();
TYP149_cprint_cname_();
TYP149_cprint_extern_();
TYP149_cprint_access_value_();
TYP149_cprint_init_code_();
int TYP149_inst_ind_();
ptr TYP149_inst_cls_();
char TYP149_is_dispatched_();
ptr TYP149_dispatched_();
ptr TYP149_undispatched_();
char TYP149_arithtype_();
char TYP149_int_type_p_();
char TYP149_bool_type_p_();
char TYP149_char_type_p_();
char TYP149_real_type_p_();
char TYP149_double_type_p_();
char TYP149_str_type_p_();
char TYP149_nonptr_p_();
ptr TYP149_resolve_arithtype_();
int TYP149_ctype_();
TYP149_cprint_ctype_();
TYP149_cprint_void_();
char TYP149_array_type_p_();
char TYP149_array2_type_p_();
char TYP149_array3_type_p_();
char TYP149_array4_type_p_();
ptr TYP149_rettype_();
ptr TYP149_paramstype_();
char TYP149_conforms_to_();
char TYP149_param_type_conforms_to_();
ptr TYP149_full_name_();
extern int attr_ent_TYP149[];

ptr TYP149_create_(self__,ln__,nm__)
ptr self__;
int ln__;
int nm__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(149,1);
   IATT_(res__,12) = (int)ln__;
   IATT_(res__,8) = (int)nm__;

   ret0__:
   return (res__);
}

TYP149_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,12) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,12));

   ret0__:
   return;
}

ptr TYP149_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

TYP149_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4045_;
   static int gl4046_;
   static union dtype_ gl4047_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4045_ = x__;
   cache_dispatch_(gl4045_,796,gl4046_,INTVAL_(gl4047_));
   IATT_(gl4045_,INTVAL_(gl4047_)) = (int)nm__;

   ret0__:
   return;
}

ptr TYP149_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP149_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

TYP149_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

TYP149_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr TYP149_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int TYP149_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

TYP149_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr TYP149_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

TYP149_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

TYP149_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP149_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP149_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP149_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

int TYP149_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr TYP149_inst_cls_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char TYP149_is_dispatched_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr TYP149_dispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TYP149_undispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char TYP149_arithtype_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_int_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_bool_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_char_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_real_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_double_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_str_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_nonptr_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr TYP149_resolve_arithtype_(self__,tp__)
ptr self__;
ptr tp__;
{
   ptr res__ = 0;

   res__ = (ptr)GLO68_int_typeob_s_;

   ret0__:
   return (res__);
}

int TYP149_ctype_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)1;

   ret0__:
   return (res__);
}

TYP149_cprint_ctype_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   CLA148_cprint_ctype_(TYP149_inst_cls_(self__),outfile__);

   ret0__:
   return;
}

TYP149_cprint_void_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls2693_,"S_");
   SATHER_STR_(20,7,ls2694_,"_VOID_");
   SATHER_STR_(20,2,ls1692_,"0");

   if (TYP149_nonptr_p_(self__)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2693_));
      TYP149_cprint_ctype_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2694_));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls1692_));
   }

   ret0__:
   return;
}

char TYP149_array_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_array2_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_array3_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_array4_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr TYP149_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP149_paramstype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char TYP149_conforms_to_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP149_param_type_conforms_to_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr TYP149_full_name_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}


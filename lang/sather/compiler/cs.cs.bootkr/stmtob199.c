/* stmtob199.c : Sather class: STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
#include "macros_.h"



/*constant*/ int STM199_print_indent_ = 2;
ptr STM199_create_();
STM199_out_of_line_();
ptr STM199_dup_();
STM199_put_kwdname_();
ptr STM199_sather_code_();
ptr STM199_initialize_();
STM199_resolve_predef_types_();
STM199_semant_();
ptr STM199_typeof_();
int STM199_get_offset_();
STM199_cprint_offset_();
ptr STM199_get_constval_();
STM199_cont_cprint_code_();
STM199_cprint_cname_();
STM199_cprint_extern_();
STM199_cprint_access_value_();
STM199_cprint_init_code_();
char STM199_typechk_exprs_();
ptr STM199_gen_temps_();
STM199_gen_goto_tags_();
STM199_validate_dispatches_and_get_ext_strs_();
STM199_cprint_code_();
extern int attr_ent_STM199[];

ptr STM199_create_(self__,ln__)
ptr self__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(199,1);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

STM199_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr STM199_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STM199_create_(self__,IATT_(self__,4));

   ret0__:
   return (res__);
}

STM199_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4729_;
   static int gl4730_;
   static union dtype_ gl4731_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4729_ = x__;
   cache_dispatch_(gl4729_,796,gl4730_,INTVAL_(gl4731_));
   IATT_(gl4729_,INTVAL_(gl4731_)) = (int)nm__;

   ret0__:
   return;
}

ptr STM199_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr STM199_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

STM199_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

STM199_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr STM199_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int STM199_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

STM199_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr STM199_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

STM199_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

STM199_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

STM199_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

STM199_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

STM199_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char STM199_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr STM199_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

STM199_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{


   ret0__:
   return;
}

STM199_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

STM199_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


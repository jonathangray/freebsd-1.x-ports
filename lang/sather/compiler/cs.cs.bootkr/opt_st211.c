/* opt_st211.c : Sather class: OPT_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
#include "macros_.h"



/*constant*/ int OPT211_print_indent_ = 2;
ptr OPT211_create_();
OPT211_out_of_line_();
ptr OPT211_dup_();
OPT211_put_kwdname_();
ptr OPT211_sather_code_();
ptr OPT211_initialize_();
OPT211_resolve_predef_types_();
OPT211_semant_();
ptr OPT211_typeof_();
int OPT211_get_offset_();
OPT211_cprint_offset_();
ptr OPT211_get_constval_();
OPT211_cont_cprint_code_();
OPT211_cprint_cname_();
OPT211_cprint_extern_();
OPT211_cprint_access_value_();
OPT211_cprint_init_code_();
char OPT211_typechk_exprs_();
ptr OPT211_gen_temps_();
OPT211_gen_goto_tags_();
OPT211_validate_dispatches_and_get_ext_strs_();
OPT211_cprint_code_();
extern int attr_ent_OPT211[];

ptr OPT211_create_(self__,ln__)
ptr self__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(211,0);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

OPT211_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr OPT211_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)OPT211_create_(self__,IATT_(self__,4));

   ret0__:
   return (res__);
}

OPT211_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4775_;
   static int gl4776_;
   static union dtype_ gl4777_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4775_ = x__;
   cache_dispatch_(gl4775_,796,gl4776_,INTVAL_(gl4777_));
   IATT_(gl4775_,INTVAL_(gl4777_)) = (int)nm__;

   ret0__:
   return;
}

ptr OPT211_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr OPT211_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

OPT211_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

OPT211_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{

   PATT_(self__,12) = (ptr)PATT_(symtab__,4);

   ret0__:
   return;
}

ptr OPT211_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int OPT211_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

OPT211_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr OPT211_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

OPT211_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

OPT211_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

OPT211_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

OPT211_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

OPT211_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char OPT211_typechk_exprs_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr OPT211_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

OPT211_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{


   ret0__:
   return;
}

OPT211_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{


   ret0__:
   return;
}

OPT211_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


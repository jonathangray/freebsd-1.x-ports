/* semant188.c : Sather class: SEMANTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
#include "macros_.h"



/*constant*/ int SEM188_print_indent_ = 2;
ptr SEM188_create_();
SEM188_out_of_line_();
ptr SEM188_dup_();
SEM188_put_kwdname_();
ptr SEM188_sather_code_();
ptr SEM188_initialize_();
SEM188_resolve_predef_types_();
SEM188_semant_();
ptr SEM188_typeof_();
int SEM188_get_offset_();
SEM188_cprint_offset_();
ptr SEM188_get_constval_();
SEM188_cont_cprint_code_();
SEM188_cprint_cname_();
SEM188_cprint_extern_();
SEM188_cprint_access_value_();
SEM188_cprint_init_code_();
extern int attr_ent_SEM188[];

ptr SEM188_create_(self__,ln__,nm__)
ptr self__;
int ln__;
int nm__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(188,1);
   IATT_(res__,4) = (int)ln__;
   IATT_(res__,8) = (int)nm__;

   ret0__:
   return (res__);
}

SEM188_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr SEM188_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

SEM188_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4672_;
   static int gl4673_;
   static union dtype_ gl4674_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4672_ = x__;
   cache_dispatch_(gl4672_,796,gl4673_,INTVAL_(gl4674_));
   IATT_(gl4672_,INTVAL_(gl4674_)) = (int)nm__;

   ret0__:
   return;
}

ptr SEM188_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SEM188_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

SEM188_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{


   ret0__:
   return;
}

SEM188_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr SEM188_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int SEM188_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

SEM188_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr SEM188_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

SEM188_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

SEM188_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

SEM188_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

SEM188_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

SEM188_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


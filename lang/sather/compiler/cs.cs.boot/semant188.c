/* semant188.c : Sather class: SEMANTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
#include "macros_.h"



/*constant*/ int SEM188_print_indent_ = 2;
ptr SEM188_create_(ptr self__, int ln__, int nm__);
void SEM188_out_of_line_(ptr self__, ptr fn__);
ptr SEM188_dup_(ptr self__);
void SEM188_put_kwdname_(ptr self__, int nm__);
ptr SEM188_sather_code_(ptr self__);
ptr SEM188_initialize_(ptr self__, ptr initarg__);
void SEM188_resolve_predef_types_(ptr self__, int index__);
void SEM188_semant_(ptr self__, ptr symtab__);
ptr SEM188_typeof_(ptr self__);
int SEM188_get_offset_(ptr self__);
void SEM188_cprint_offset_(ptr self__, ptr outfile__);
ptr SEM188_get_constval_(ptr self__);
void SEM188_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void SEM188_cprint_cname_(ptr self__, ptr outfile__);
void SEM188_cprint_extern_(ptr self__, ptr outfile__);
void SEM188_cprint_access_value_(ptr self__, ptr outfile__);
void SEM188_cprint_init_code_(ptr self__, ptr outfile__);
extern int attr_ent_SEM188[];

ptr SEM188_create_(ptr self__, int ln__, int nm__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(188,1);
   IATT_(res__,4) = (int)ln__;
   IATT_(res__,8) = (int)nm__;

   ret0__:
   return (res__);
}

void SEM188_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr SEM188_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

void SEM188_put_kwdname_(ptr self__, int nm__)
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

ptr SEM188_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SEM188_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void SEM188_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void SEM188_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr SEM188_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int SEM188_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void SEM188_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr SEM188_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void SEM188_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void SEM188_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SEM188_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SEM188_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void SEM188_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}


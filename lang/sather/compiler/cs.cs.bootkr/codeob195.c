/* codeob195.c : Sather class: CODEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
#include "macros_.h"



/*constant*/ int COD195_print_indent_ = 2;
ptr COD195_create_();
COD195_out_of_line_();
ptr COD195_dup_();
COD195_put_kwdname_();
ptr COD195_sather_code_();
ptr COD195_initialize_();
extern int attr_ent_COD195[];

ptr COD195_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(195,1);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

COD195_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr COD195_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,1);

   ret0__:
   return (res__);
}

COD195_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4715_;
   static int gl4716_;
   static union dtype_ gl4717_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4715_ = x__;
   cache_dispatch_(gl4715_,796,gl4716_,INTVAL_(gl4717_));
   IATT_(gl4715_,INTVAL_(gl4717_)) = (int)nm__;

   ret0__:
   return;
}

ptr COD195_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr COD195_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* any_de65.c : Sather class: ANY_DECLOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr DEC205_create_(ptr self__, ptr t__, int ln__);
#include "macros_.h"



/*constant*/ int ANY65_print_indent_ = 2;
ptr ANY65_create_(ptr self__, ptr nms__, ptr tp__);
void ANY65_out_of_line_(ptr self__, ptr fn__);
ptr ANY65_dup_(ptr self__);
void ANY65_put_kwdname_(ptr self__, int nm__);
ptr ANY65_sather_code_(ptr self__);
ptr ANY65_initialize_(ptr self__, ptr initarg__);
ptr ANY65_pcopy_(ptr self__, ptr pl__, ptr pi__);
void ANY65_mark_private_(ptr self__);
int ANY65_size_(ptr self__);
ptr ANY65_get_type_spec_(ptr self__);
int ANY65_ith_name_(ptr self__, int i__);
extern int attr_ent_ANY65[];

ptr ANY65_create_(ptr self__, ptr nms__, ptr tp__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(65,0);
   PATT_(res__,20) = (ptr)nms__;
   PATT_(res__,12) = (ptr)tp__;
   IATT_(res__,16) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void ANY65_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr ANY65_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ANY65_put_kwdname_(ptr self__, int nm__)
{
   ptr gl930_;
   static int gl931_;
   static union dtype_ gl932_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl930_ = x__;
   cache_dispatch_(gl930_,796,gl931_,INTVAL_(gl932_));
   IATT_(gl930_,INTVAL_(gl932_)) = (int)nm__;

   ret0__:
   return;
}

ptr ANY65_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ANY65_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ANY65_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl933_;
   static int gl934_;
   static union dtype_ gl935_;
   ptr gl55_;

   gl933_ = PATT_(self__,12);
   cache_dispatch_(gl933_,407,gl934_,INTVAL_(gl935_));
   gl55_ = PFN_(gl935_)(gl933_,pl__,pi__);
   res__ = (ptr)DEC205_create_(res__,gl55_,IATT_(self__,16));

   ret0__:
   return (res__);
}

void ANY65_mark_private_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

int ANY65_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((PATT_(self__,20) != 0)) {
      res__ = (int)IATT_(PATT_(self__,20),4);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ANY65_get_type_spec_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int ANY65_ith_name_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,20), 16 + ((i__) << 2));

   ret0__:
   return (res__);
}


/* typesp35.c : Sather class: TYPESPEC_ARGS_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_();
extern ERR96_format_error_msg_file_();
extern int ERR96_out_of_line_err_info_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr LST37_pcopy_();
extern ptr TYP114_name_str_();
extern ptr TYP114_disp_type_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr TYP114_pcopy_();
extern ptr TYP115_create_();
#include "macros_.h"



/*constant*/ int TYP35_print_indent_ = 2;
ptr TYP35_create_();
TYP35_out_of_line_();
ptr TYP35_dup_();
TYP35_put_kwdname_();
ptr TYP35_sather_code_();
ptr TYP35_initialize_();
ptr TYP35_pcopy_();
extern int attr_ent_TYP35[];

ptr TYP35_create_(self__,ts__,f__,alst__)
ptr self__;
ptr ts__;
int f__;
ptr alst__;
{
   ptr res__ = 0;
   SATHER_STR_(20,21,ls1668_,"TYPESPEC_ARGS_EXPROB");
   SATHER_STR_(20,18,ls1670_,"Dispatching for \"");
   SATHER_STR_(20,11,ls1671_,"\" ignored\n");
   ptr gl588_;
   static int gl589_;
   static union dtype_ gl590_;
   int gl10_;
   ptr gl591_;
   static int gl592_;
   static union dtype_ gl593_;
   ptr gl11_;
   ptr gl594_;
   static int gl595_;
   static union dtype_ gl596_;

   res__ = (ptr)new_(35,0);
   gl588_ = ts__;
   gl10_ = TYPE_(gl588_);
   if ((gl10_ == 56)) {
      gl591_ = ts__;
      cache_dispatch_(gl591_,332,gl592_,INTVAL_(gl593_));
      gl11_ = PFN_(gl593_)(gl591_);
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls1668_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1670_)),gl11_),(ptr)(&ls1671_)));
      gl594_ = ts__;
      cache_dispatch_(gl594_,1672,gl595_,INTVAL_(gl596_));
      ts__ = (ptr)PFN_(gl596_)(gl594_);
   }
   else {
   }
   PATT_(res__,12) = (ptr)ts__;
   IATT_(res__,16) = (int)f__;
   PATT_(res__,20) = (ptr)alst__;
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

TYP35_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr TYP35_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

TYP35_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl597_;
   static int gl598_;
   static union dtype_ gl599_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl597_ = x__;
   cache_dispatch_(gl597_,796,gl598_,INTVAL_(gl599_));
   IATT_(gl597_,INTVAL_(gl599_)) = (int)nm__;

   ret0__:
   return;
}

ptr TYP35_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr TYP35_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TYP35_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl600_;
   static int gl601_;
   static union dtype_ gl602_;
   ptr gl12_;
   ptr gl603_;
   static int gl604_;
   static union dtype_ gl605_;
   ptr gl13_;

   if ((PATT_(self__,20) != 0)) {
      gl600_ = PATT_(self__,12);
      cache_dispatch_(gl600_,407,gl601_,INTVAL_(gl602_));
      gl12_ = PFN_(gl602_)(gl600_,pl__,pi__);
      res__ = (ptr)TYP115_create_(res__,gl12_,IATT_(self__,16),LST37_pcopy_(PATT_(self__,20),pl__,pi__),IATT_(self__,4));
   }
   else {
      gl603_ = PATT_(self__,12);
      cache_dispatch_(gl603_,407,gl604_,INTVAL_(gl605_));
      gl13_ = PFN_(gl605_)(gl603_,pl__,pi__);
      res__ = (ptr)TYP115_create_(res__,gl13_,IATT_(self__,16),0,IATT_(self__,4));
   }

   ret0__:
   return (res__);
}


/* lvar_d146.c : Sather class: LVAR_DECL_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_(ptr self__);
extern void ERR96_error_msg_(ptr self__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LVA145_create_(ptr self__, int nm__, ptr tp__, ptr exp__, int ln__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern ptr STR20_i_(ptr self__, int in__);
#include "macros_.h"



/*constant*/ int LVA146_print_indent_ = 2;
ptr LVA146_create_(ptr self__, int nm__, ptr tp__, ptr exp__, int ln__);
void LVA146_out_of_line_(ptr self__, ptr fn__);
ptr LVA146_dup_(ptr self__);
void LVA146_put_kwdname_(ptr self__, int nm__);
ptr LVA146_sather_code_(ptr self__);
ptr LVA146_initialize_(ptr self__, ptr initarg__);
ptr LVA146_pcopy_(ptr self__, ptr pl__, ptr pi__);
void LVA146_mark_private_(ptr self__);
extern int attr_ent_LVA146[];

ptr LVA146_create_(ptr self__, int nm__, ptr tp__, ptr exp__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,44,ls2543_," (LVAR_DECL_STMTOB) : Error in redefining \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   res__ = (ptr)new_(146,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_i_(STR20_c_(STR20_s_(STR20_create_(0),ERR96_filename_(0)),'('),ln__),')'),(ptr)(&ls2543_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls632_)));
   }
   else {
   }
   IATT_(res__,20) = (int)nm__;
   PATT_(res__,12) = (ptr)tp__;
   PATT_(res__,24) = (ptr)exp__;
   IATT_(res__,16) = (int)ln__;

   ret0__:
   return (res__);
}

void LVA146_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr LVA146_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void LVA146_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3765_;
   static int gl3766_;
   static union dtype_ gl3767_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3765_ = x__;
   cache_dispatch_(gl3765_,796,gl3766_,INTVAL_(gl3767_));
   IATT_(gl3765_,INTVAL_(gl3767_)) = (int)nm__;

   ret0__:
   return;
}

ptr LVA146_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LVA146_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LVA146_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl3768_;
   static int gl3769_;
   static union dtype_ gl3770_;
   ptr gl3771_;
   static int gl3772_;
   static union dtype_ gl3773_;
   ptr gl357_;
   ptr gl358_;
   ptr gl3774_;
   static int gl3775_;
   static union dtype_ gl3776_;
   ptr gl359_;

   if ((PATT_(self__,24) != 0)) {
      gl3768_ = PATT_(self__,12);
      cache_dispatch_(gl3768_,407,gl3769_,INTVAL_(gl3770_));
      gl357_ = PFN_(gl3770_)(gl3768_,pl__,pi__);
      gl3771_ = PATT_(self__,24);
      cache_dispatch_(gl3771_,407,gl3772_,INTVAL_(gl3773_));
      gl358_ = PFN_(gl3773_)(gl3771_,pl__,pi__);
      res__ = (ptr)LVA145_create_(res__,IATT_(self__,20),gl357_,gl358_,IATT_(self__,16));
   }
   else {
      gl3774_ = PATT_(self__,12);
      cache_dispatch_(gl3774_,407,gl3775_,INTVAL_(gl3776_));
      gl359_ = PFN_(gl3776_)(gl3774_,pl__,pi__);
      res__ = (ptr)LVA145_create_(res__,IATT_(self__,20),gl359_,0,IATT_(self__,16));
   }

   ret0__:
   return (res__);
}

void LVA146_mark_private_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}


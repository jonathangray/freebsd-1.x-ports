/* alias_64.c : Sather class: ALIAS_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_(ptr self__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr ALI157_create_(ptr self__, int nm__, int orig__, ptr cls__, int ln__);
extern void ALI157_mark_private_(ptr self__);
#include "macros_.h"



ptr ALI64_dup_(ptr self__);
void ALI64_put_kwdname_(ptr self__, int nm__);
ptr ALI64_sather_code_(ptr self__);
ptr ALI64_initialize_(ptr self__, ptr initarg__);
ptr ALI64_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int ALI64_print_indent_ = 2;
ptr ALI64_create_(ptr self__, int nm__, int orig__, int ln__);
void ALI64_out_of_line_(ptr self__, ptr fn__);
void ALI64_mark_private_(ptr self__);
void ALI64_mark_abstract_(ptr self__);
void ALI64_mark_spec_(ptr self__);
void ALI64_mark_shared_(ptr self__);
void ALI64_mark_readonly_(ptr self__);
extern int attr_ent_ALI64[];

ptr ALI64_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ALI64_put_kwdname_(ptr self__, int nm__)
{
   ptr gl927_;
   static int gl928_;
   static union dtype_ gl929_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl927_ = x__;
   cache_dispatch_(gl927_,796,gl928_,INTVAL_(gl929_));
   IATT_(gl927_,INTVAL_(gl929_)) = (int)nm__;

   ret0__:
   return;
}

ptr ALI64_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ALI64_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ALI64_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)ALI157_create_(0,IATT_(self__,16),IATT_(self__,28),PATT_(self__,12),IATT_(self__,20));
   if (CATT_(self__,5)) {
      ALI157_mark_private_(res__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ALI64_create_(ptr self__, int nm__, int orig__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,13,ls2117_,"ALIAS_FEATOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,12,ls2119_,"Repeating \"");

   res__ = (ptr)new_(64,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls2117_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
   }
   else {
   }
   if (STR69_reserved_name_p_(GLO68_str_table_,orig__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls2117_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2119_)),STR69_at_index_(GLO68_str_table_,orig__)),(ptr)(&ls785_)));
   }
   else {
   }
   IATT_(res__,16) = (int)nm__;
   IATT_(res__,28) = (int)orig__;
   IATT_(res__,20) = (int)ln__;

   ret0__:
   return (res__);
}

void ALI64_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void ALI64_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void ALI64_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void ALI64_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void ALI64_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void ALI64_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


/* alias_64.c : Sather class: ALIAS_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_();
extern ERR96_format_error_msg_file_();
extern int ERR96_out_of_line_err_info_();
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STR69_at_index_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr ALI157_create_();
extern ALI157_mark_private_();
#include "macros_.h"



ptr ALI64_dup_();
ALI64_put_kwdname_();
ptr ALI64_sather_code_();
ptr ALI64_initialize_();
ptr ALI64_pcopy_();
/*constant*/ int ALI64_print_indent_ = 2;
ptr ALI64_create_();
ALI64_out_of_line_();
ALI64_mark_private_();
ALI64_mark_abstract_();
ALI64_mark_spec_();
ALI64_mark_shared_();
ALI64_mark_readonly_();
extern int attr_ent_ALI64[];

ptr ALI64_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

ALI64_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
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

ptr ALI64_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ALI64_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ALI64_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
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

ptr ALI64_create_(self__,nm__,orig__,ln__)
ptr self__;
int nm__;
int orig__;
int ln__;
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

ALI64_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

ALI64_mark_private_(self__)
ptr self__;
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

ALI64_mark_abstract_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

ALI64_mark_spec_(self__)
ptr self__;
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

ALI64_mark_shared_(self__)
ptr self__;
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

ALI64_mark_readonly_(self__)
ptr self__;
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


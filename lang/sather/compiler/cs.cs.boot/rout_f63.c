/* rout_f63.c : Sather class: ROUT_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ char CLA148_auto_generating_p_;
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr ROU156_create_(ptr self__, int nm__, ptr precond__, ptr postcond__, ptr o__, ptr pl__, ptr rt__, ptr sl__, char priv__, ptr c_def__, int ln__, int eln__);
extern void ROU156_mark_spec_(ptr self__);
extern void ROU156_mark_readonly_(ptr self__);
extern ptr ERR96_filename_(ptr self__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST41_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr ASS44_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
#include "macros_.h"



ptr ROU63_dup_(ptr self__);
void ROU63_put_kwdname_(ptr self__, int nm__);
ptr ROU63_sather_code_(ptr self__);
ptr ROU63_initialize_(ptr self__, ptr initarg__);
ptr ROU63_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int ROU63_print_indent_ = 2;
ptr ROU63_create_(ptr self__, int nm__, ptr spec__, ptr pl__, ptr rt__, ptr sl__, int ln__, int eln__);
void ROU63_out_of_line_(ptr self__, ptr fn__);
void ROU63_mark_private_(ptr self__);
void ROU63_mark_abstract_(ptr self__);
void ROU63_mark_spec_(ptr self__);
void ROU63_mark_shared_(ptr self__);
void ROU63_mark_readonly_(ptr self__);
extern int attr_ent_ROU63[];

ptr ROU63_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ROU63_put_kwdname_(ptr self__, int nm__)
{
   ptr gl921_;
   static int gl922_;
   static union dtype_ gl923_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl921_ = x__;
   cache_dispatch_(gl921_,796,gl922_,INTVAL_(gl923_));
   IATT_(gl921_,INTVAL_(gl923_)) = (int)nm__;

   ret0__:
   return;
}

ptr ROU63_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ROU63_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ROU63_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl924_;
   static int gl925_;
   static union dtype_ gl926_;
   ptr    tmpplist__ = 0;
   ptr    tmprettype__ = 0;
   ptr    tmpslist__ = 0;
   ptr    tmppre__ = 0;
   ptr    tmppost__ = 0;
   ptr    tmpold__ = 0;

   tmpplist__ = S_ptr_VOID_;
   tmprettype__ = S_ptr_VOID_;
   tmpslist__ = S_ptr_VOID_;
   tmppre__ = S_ptr_VOID_;
   tmppost__ = S_ptr_VOID_;
   if ((PATT_(self__,40) != 0)) {
      tmpplist__ = (ptr)LST41_pcopy_(PATT_(self__,40),pl__,pi__);
   }
   else {
      tmpplist__ = (ptr)0;
   }
   if ((PATT_(self__,44) != 0)) {
      gl924_ = PATT_(self__,44);
      cache_dispatch_(gl924_,407,gl925_,INTVAL_(gl926_));
      tmprettype__ = (ptr)PFN_(gl926_)(gl924_,pl__,pi__);
   }
   else {
      tmprettype__ = (ptr)0;
   }
   if ((PATT_(self__,48) != 0)) {
      tmpslist__ = (ptr)LST38_pcopy_(PATT_(self__,48),pl__,pi__);
   }
   else {
      tmpslist__ = (ptr)0;
   }
   if ((PATT_(self__,28) != 0)) {
      tmppre__ = (ptr)ASS44_pcopy_(PATT_(self__,28),pl__,pi__);
   }
   else {
   }
   if ((PATT_(self__,32) != 0)) {
      tmppost__ = (ptr)ASS44_pcopy_(PATT_(self__,32),pl__,pi__);
   }
   else {
   }
   tmpold__ = S_ptr_VOID_;
   if ((PATT_(self__,36) != 0)) {
      tmpold__ = (ptr)LST38_pcopy_(PATT_(self__,36),pl__,pi__);
   }
   else {
   }
   res__ = (ptr)ROU156_create_(res__,IATT_(self__,16),tmppre__,tmppost__,tmpold__,tmpplist__,tmprettype__,tmpslist__,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20),IATT_(self__,52));
   if (CATT_(self__,6)) {
      ROU156_mark_spec_(res__);
   }
   else {
   }
   if (CATT_(self__,8)) {
      ROU156_mark_readonly_(res__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ROU63_create_(ptr self__, int nm__, ptr spec__, ptr pl__, ptr rt__, ptr sl__, int ln__, int eln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,12,ls500_,"ROUT_FEATOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");

   res__ = (ptr)new_(63,0);
   if ((STR69_reserved_name_p_(GLO68_str_table_,nm__) & (! CLA148_auto_generating_p_))) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),GLO68_curr_lineno_,(ptr)(&ls500_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
   }
   else {
   }
   IATT_(res__,16) = (int)nm__;
   PATT_(res__,28) = (ptr)PATT_(spec__,4);
   PATT_(res__,32) = (ptr)PATT_(spec__,8);
   PATT_(res__,36) = (ptr)PATT_(spec__,12);
   PATT_(res__,40) = (ptr)pl__;
   PATT_(res__,44) = (ptr)rt__;
   PATT_(res__,48) = (ptr)sl__;
   IATT_(res__,20) = (int)ln__;
   IATT_(res__,52) = (int)eln__;

   ret0__:
   return (res__);
}

void ROU63_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void ROU63_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void ROU63_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void ROU63_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void ROU63_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void ROU63_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}


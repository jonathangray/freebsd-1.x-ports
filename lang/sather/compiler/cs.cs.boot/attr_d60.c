/* attr_d60.c : Sather class: ATTR_DECL_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_(ptr self__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern int ANY65_size_(ptr self__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr LST43_create_(ptr self__, int init_size__);
extern ptr LST43_push_(ptr self__, ptr e__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr ATT153_create_(ptr self__, int nm__, ptr t__, char priv__, ptr c_def__, int ln__);
#include "macros_.h"



ptr ATT60_dup_(ptr self__);
void ATT60_put_kwdname_(ptr self__, int nm__);
ptr ATT60_sather_code_(ptr self__);
ptr ATT60_initialize_(ptr self__, ptr initarg__);
ptr ATT60_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int ATT60_print_indent_ = 2;
ptr ATT60_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__);
void ATT60_out_of_line_(ptr self__, ptr fn__);
void ATT60_mark_private_(ptr self__);
void ATT60_mark_abstract_(ptr self__);
void ATT60_mark_spec_(ptr self__);
void ATT60_mark_shared_(ptr self__);
void ATT60_mark_readonly_(ptr self__);
ptr ATT60_create_lst_(ptr self__, ptr do__, ptr e__);
extern int attr_ent_ATT60[];

ptr ATT60_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void ATT60_put_kwdname_(ptr self__, int nm__)
{
   ptr gl895_;
   static int gl896_;
   static union dtype_ gl897_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl895_ = x__;
   cache_dispatch_(gl895_,796,gl896_,INTVAL_(gl897_));
   IATT_(gl895_,INTVAL_(gl897_)) = (int)nm__;

   ret0__:
   return;
}

ptr ATT60_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ATT60_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ATT60_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl898_;
   static int gl899_;
   static union dtype_ gl900_;
   ptr gl51_;
   ptr gl901_;
   static int gl902_;
   static union dtype_ gl903_;

   gl898_ = PATT_(self__,28);
   cache_dispatch_(gl898_,407,gl899_,INTVAL_(gl900_));
   gl51_ = PFN_(gl900_)(gl898_,pl__,pi__);
   res__ = (ptr)ATT153_create_(res__,IATT_(self__,16),gl51_,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20));
   if ((PATT_(self__,32) != 0)) {
      gl901_ = PATT_(self__,32);
      cache_dispatch_(gl901_,407,gl902_,INTVAL_(gl903_));
      PATT_(res__,44) = (ptr)PFN_(gl903_)(gl901_,pl__,pi__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ATT60_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,17,ls1525_,"ATTR_DECL_FEATOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");

   res__ = (ptr)new_(60,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),ln__,(ptr)(&ls1525_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
   }
   else {
   }
   IATT_(res__,16) = (int)nm__;
   PATT_(res__,28) = (ptr)t__;
   PATT_(res__,32) = (ptr)e__;
   IATT_(res__,20) = (int)ln__;

   ret0__:
   return (res__);
}

void ATT60_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void ATT60_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void ATT60_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void ATT60_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void ATT60_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void ATT60_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

ptr ATT60_create_lst_(ptr self__, ptr do__, ptr e__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   ptr    tp__ = 0;

   i__ = (int)0;
   sz__ = (int)ANY65_size_(do__);
   res__ = (ptr)LST43_create_(res__,sz__);
   nm__ = S_int_VOID_;
   tp__ = S_ptr_VOID_;
   tp__ = (ptr)PATT_(do__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_904_;
      }
      else {
      }
      nm__ = (int)IATT_(PATT_(do__,20), 16 + ((i__) << 2));
      res__ = (ptr)LST43_push_(res__,ATT60_create_(0,nm__,tp__,e__,IATT_(do__,16)));
      i__ = (int)(i__ + 1);
   }
goto_tag_904_: ;

   ret0__:
   return (res__);
}


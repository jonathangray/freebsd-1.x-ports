/* const_58.c : Sather class: CONST_DECL_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ANY65_size_(ptr self__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*constant*/ int RES71_void_ind_;
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern ptr CON151_create_(ptr self__, int nm__, ptr t__, ptr e__, char priv__, ptr c_def__, int ln__);
extern ptr ID_25_create_(ptr self__, int nm__);
extern ptr ERR96_filename_(ptr self__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_error_msg_(ptr self__, ptr s__);
extern ptr EXP36_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST43_create_(ptr self__, int init_size__);
extern ptr LST43_push_(ptr self__, ptr e__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
#include "macros_.h"



ptr CON58_dup_(ptr self__);
void CON58_put_kwdname_(ptr self__, int nm__);
ptr CON58_sather_code_(ptr self__);
ptr CON58_initialize_(ptr self__, ptr initarg__);
ptr CON58_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int CON58_print_indent_ = 2;
ptr CON58_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__);
void CON58_out_of_line_(ptr self__, ptr fn__);
void CON58_mark_private_(ptr self__);
void CON58_mark_abstract_(ptr self__);
void CON58_mark_spec_(ptr self__);
void CON58_mark_shared_(ptr self__);
void CON58_mark_readonly_(ptr self__);
ptr CON58_create_lst_(ptr self__, ptr do__, ptr exp__);
extern int attr_ent_CON58[];

ptr CON58_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void CON58_put_kwdname_(ptr self__, int nm__)
{
   ptr gl868_;
   static int gl869_;
   static union dtype_ gl870_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl868_ = x__;
   cache_dispatch_(gl868_,796,gl869_,INTVAL_(gl870_));
   IATT_(gl868_,INTVAL_(gl870_)) = (int)nm__;

   ret0__:
   return;
}

ptr CON58_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr CON58_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr CON58_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl871_;
   static int gl872_;
   static union dtype_ gl873_;
   ptr gl874_;
   static int gl875_;
   static union dtype_ gl876_;
   ptr gl45_;
   ptr gl46_;
   ptr gl877_;
   static int gl878_;
   static union dtype_ gl879_;
   ptr gl47_;

   if ((PATT_(self__,32) != 0)) {
      gl871_ = PATT_(self__,28);
      cache_dispatch_(gl871_,407,gl872_,INTVAL_(gl873_));
      gl45_ = PFN_(gl873_)(gl871_,pl__,pi__);
      gl874_ = PATT_(self__,32);
      cache_dispatch_(gl874_,407,gl875_,INTVAL_(gl876_));
      gl46_ = PFN_(gl876_)(gl874_,pl__,pi__);
      res__ = (ptr)CON151_create_(res__,IATT_(self__,16),gl45_,gl46_,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20));
   }
   else {
      gl877_ = PATT_(self__,28);
      cache_dispatch_(gl877_,407,gl878_,INTVAL_(gl879_));
      gl47_ = PFN_(gl879_)(gl877_,pl__,pi__);
      res__ = (ptr)CON151_create_(res__,IATT_(self__,16),gl47_,0,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

ptr CON58_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,18,ls1538_,"CONST_DECL_FEATOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");

   res__ = (ptr)new_(58,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),ln__,(ptr)(&ls1538_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
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

void CON58_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void CON58_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void CON58_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void CON58_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void CON58_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void CON58_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

ptr CON58_create_lst_(ptr self__, ptr do__, ptr exp__)
{
   ptr res__ = 0;
   SATHER_STR_(20,18,ls1538_,"CONST_DECL_FEATOB");
   SATHER_STR_(20,40,ls1539_,"No initialization for constant features");
   SATHER_STR_(20,2,ls780_,"\n");
   SATHER_STR_(20,29,ls1535_,"No name found in declaration");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    first__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   ptr    tp__ = 0;
   ptr    c_declob__ = 0;

   i__ = (int)0;
   sz__ = (int)ANY65_size_(do__);
   res__ = (ptr)LST43_create_(res__,sz__);
   if ((exp__ == 0)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,20),(ptr)(&ls1538_),STR20_s_(STR20_create_(0),(ptr)(&ls1539_)));
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_880_;
         }
         else {
         }
         ERR96_error_msg_(0,STR20_c_(STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(PATT_(do__,20), 16 + ((i__) << 2)))),','));
         i__ = (int)(i__ + 1);
      }
   goto_tag_880_: ;
      ERR96_error_msg_(0,(ptr)(&ls780_));
      exp__ = (ptr)ID_25_create_(0,37);
   }
   else {
   }
   first__ = S_int_VOID_;
   if ((sz__ > 0)) {
      first__ = (int)IATT_(PATT_(do__,20), 16 + ((0) << 2));
   }
   else {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,20),(ptr)(&ls1538_),(ptr)(&ls1535_));
   }
   nm__ = S_int_VOID_;
   tp__ = S_ptr_VOID_;
   c_declob__ = S_ptr_VOID_;
   i__ = (int)0;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_881_;
      }
      else {
      }
      nm__ = (int)IATT_(PATT_(do__,20), 16 + ((i__) << 2));
      tp__ = (ptr)PATT_(do__,12);
      if ((i__ > 0)) {
         c_declob__ = (ptr)CON58_create_(c_declob__,nm__,tp__,ID_25_create_(0,first__),IATT_(do__,16));
      }
      else {
         c_declob__ = (ptr)CON58_create_(c_declob__,nm__,tp__,exp__,IATT_(do__,16));
      }
      res__ = (ptr)LST43_push_(res__,c_declob__);
      i__ = (int)(i__ + 1);
   }
goto_tag_881_: ;

   ret0__:
   return (res__);
}


/* shared59.c : Sather class: SHARED_DECL_FEATOB, dbg=F, gc=T, chk=F */

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
extern ptr SHA152_create_(ptr self__, int nm__, ptr t__, ptr e__, char priv__, ptr c_def__, int ln__);
extern ptr ID_25_create_(ptr self__, int nm__);
#include "macros_.h"



ptr SHA59_dup_(ptr self__);
void SHA59_put_kwdname_(ptr self__, int nm__);
ptr SHA59_sather_code_(ptr self__);
ptr SHA59_initialize_(ptr self__, ptr initarg__);
ptr SHA59_pcopy_(ptr self__, ptr pl__, ptr pi__);
/*constant*/ int SHA59_print_indent_ = 2;
ptr SHA59_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__);
void SHA59_out_of_line_(ptr self__, ptr fn__);
void SHA59_mark_private_(ptr self__);
void SHA59_mark_abstract_(ptr self__);
void SHA59_mark_spec_(ptr self__);
void SHA59_mark_shared_(ptr self__);
void SHA59_mark_readonly_(ptr self__);
ptr SHA59_create_lst_(ptr self__, ptr do__, ptr exp__);
extern int attr_ent_SHA59[];

ptr SHA59_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void SHA59_put_kwdname_(ptr self__, int nm__)
{
   ptr gl882_;
   static int gl883_;
   static union dtype_ gl884_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl882_ = x__;
   cache_dispatch_(gl882_,796,gl883_,INTVAL_(gl884_));
   IATT_(gl882_,INTVAL_(gl884_)) = (int)nm__;

   ret0__:
   return;
}

ptr SHA59_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr SHA59_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr SHA59_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl885_;
   static int gl886_;
   static union dtype_ gl887_;
   ptr gl888_;
   static int gl889_;
   static union dtype_ gl890_;
   ptr gl48_;
   ptr gl49_;
   ptr gl891_;
   static int gl892_;
   static union dtype_ gl893_;
   ptr gl50_;

   if ((PATT_(self__,32) != 0)) {
      gl885_ = PATT_(self__,28);
      cache_dispatch_(gl885_,407,gl886_,INTVAL_(gl887_));
      gl48_ = PFN_(gl887_)(gl885_,pl__,pi__);
      gl888_ = PATT_(self__,32);
      cache_dispatch_(gl888_,407,gl889_,INTVAL_(gl890_));
      gl49_ = PFN_(gl890_)(gl888_,pl__,pi__);
      res__ = (ptr)SHA152_create_(res__,IATT_(self__,16),gl48_,gl49_,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20));
   }
   else {
      gl891_ = PATT_(self__,28);
      cache_dispatch_(gl891_,407,gl892_,INTVAL_(gl893_));
      gl50_ = PFN_(gl893_)(gl891_,pl__,pi__);
      res__ = (ptr)SHA152_create_(res__,IATT_(self__,16),gl50_,0,CATT_(self__,5),PATT_(self__,12),IATT_(self__,20));
   }

   ret0__:
   return (res__);
}

ptr SHA59_create_(ptr self__, int nm__, ptr t__, ptr e__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,19,ls1533_,"SHARED_DECL_FEATOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");

   res__ = (ptr)new_(59,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),ln__,(ptr)(&ls1533_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
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

void SHA59_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,20) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,20));

   ret0__:
   return;
}

void SHA59_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void SHA59_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void SHA59_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void SHA59_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void SHA59_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

ptr SHA59_create_lst_(ptr self__, ptr do__, ptr exp__)
{
   ptr res__ = 0;
   SATHER_STR_(20,19,ls1533_,"SHARED_DECL_FEATOB");
   SATHER_STR_(20,29,ls1535_,"No name found in declaration");
   int    sz__ = S_int_VOID_;
   int    first__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   ptr    tp__ = 0;
   ptr    s_declob__ = 0;

   sz__ = (int)ANY65_size_(do__);
   res__ = (ptr)LST43_create_(res__,sz__);
   first__ = S_int_VOID_;
   if ((sz__ > 0)) {
      first__ = (int)IATT_(PATT_(do__,20), 16 + ((0) << 2));
   }
   else {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),IATT_(self__,20),(ptr)(&ls1533_),(ptr)(&ls1535_));
   }
   i__ = (int)0;
   nm__ = S_int_VOID_;
   tp__ = S_ptr_VOID_;
   s_declob__ = S_ptr_VOID_;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_894_;
      }
      else {
      }
      nm__ = (int)IATT_(PATT_(do__,20), 16 + ((i__) << 2));
      tp__ = (ptr)PATT_(do__,12);
      if (((i__ > 0) & (exp__ != 0))) {
         s_declob__ = (ptr)SHA59_create_(s_declob__,nm__,tp__,ID_25_create_(0,first__),IATT_(do__,16));
      }
      else {
         s_declob__ = (ptr)SHA59_create_(s_declob__,nm__,tp__,exp__,IATT_(do__,16));
      }
      res__ = (ptr)LST43_push_(res__,s_declob__);
      i__ = (int)(i__ + 1);
   }
goto_tag_894_: ;

   ret0__:
   return (res__);
}


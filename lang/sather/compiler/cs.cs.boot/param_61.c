/* param_61.c : Sather class: PARAM_DECLOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr ERR96_filename_(ptr self__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern int ANY65_size_(ptr self__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr LST41_create_(ptr self__, int init_size__);
extern ptr LST41_push_(ptr self__, ptr e__);
extern ptr TYP114_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr PAR154_create_(ptr self__, int nm__, ptr tp__, int ln__);
#include "macros_.h"



/*constant*/ int PAR61_print_indent_ = 2;
ptr PAR61_create_(ptr self__, int nm__, ptr tp__, int ln__);
void PAR61_out_of_line_(ptr self__, ptr fn__);
ptr PAR61_dup_(ptr self__);
void PAR61_put_kwdname_(ptr self__, int nm__);
ptr PAR61_sather_code_(ptr self__);
ptr PAR61_initialize_(ptr self__, ptr initarg__);
ptr PAR61_pcopy_(ptr self__, ptr pl__, ptr pi__);
void PAR61_mark_private_(ptr self__);
ptr PAR61_create_lst_(ptr self__, ptr do__);
extern int attr_ent_PAR61[];

ptr PAR61_create_(ptr self__, int nm__, ptr tp__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,13,ls511_,"PARAM_DECLOB");
   SATHER_STR_(20,13,ls1530_,"Redefining \"");
   SATHER_STR_(20,2,ls785_,"\"");

   res__ = (ptr)new_(61,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_format_error_msg_file_(0,ERR96_filename_(0),ln__,(ptr)(&ls511_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1530_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls785_)));
   }
   else {
   }
   IATT_(res__,20) = (int)nm__;
   PATT_(res__,12) = (ptr)tp__;
   IATT_(res__,16) = (int)ln__;

   ret0__:
   return (res__);
}

void PAR61_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr PAR61_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void PAR61_put_kwdname_(ptr self__, int nm__)
{
   ptr gl905_;
   static int gl906_;
   static union dtype_ gl907_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl905_ = x__;
   cache_dispatch_(gl905_,796,gl906_,INTVAL_(gl907_));
   IATT_(gl905_,INTVAL_(gl907_)) = (int)nm__;

   ret0__:
   return;
}

ptr PAR61_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr PAR61_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr PAR61_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl908_;
   static int gl909_;
   static union dtype_ gl910_;
   ptr gl52_;

   gl908_ = PATT_(self__,12);
   cache_dispatch_(gl908_,407,gl909_,INTVAL_(gl910_));
   gl52_ = PFN_(gl910_)(gl908_,pl__,pi__);
   res__ = (ptr)PAR154_create_(res__,IATT_(self__,20),gl52_,IATT_(self__,16));

   ret0__:
   return (res__);
}

void PAR61_mark_private_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

ptr PAR61_create_lst_(ptr self__, ptr do__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   ptr    tp__ = 0;
   ptr    p_declob__ = 0;

   i__ = (int)0;
   sz__ = (int)ANY65_size_(do__);
   res__ = (ptr)LST41_create_(res__,sz__);
   nm__ = S_int_VOID_;
   tp__ = S_ptr_VOID_;
   p_declob__ = S_ptr_VOID_;
   tp__ = (ptr)PATT_(do__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_911_;
      }
      else {
      }
      nm__ = (int)IATT_(PATT_(do__,20), 16 + ((i__) << 2));
      p_declob__ = (ptr)PAR61_create_(p_declob__,nm__,tp__,IATT_(do__,16));
      res__ = (ptr)LST41_push_(res__,p_declob__);
      i__ = (int)(i__ + 1);
   }
goto_tag_911_: ;

   ret0__:
   return (res__);
}


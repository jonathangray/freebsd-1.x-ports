/* rout_t215.c : Sather class: ROUT_TYPEOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_int_typeob_s_;
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr INS150_dup_(ptr self__);
extern void CLA148_cprint_ctype_(ptr self__, ptr outfile__);
extern ptr TYP149_rettype_(ptr self__);
extern char INS150_conforms_to_(ptr self__, ptr tp__);
extern ptr TYP149_paramstype_(ptr self__);
extern ptr INS150_full_name_(ptr self__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr LST102_dup_(ptr self__);
extern char LST102_conforms_to_(ptr self__, ptr tpp__);
extern /*constant*/ int C_T168_c_ptr_;
extern ptr LST102_full_name_(ptr self__);
#include "macros_.h"



/*constant*/ int ROU215_print_indent_ = 2;
ptr ROU215_create_(ptr self__, ptr parmstp__, ptr rtp__, int ln__);
void ROU215_out_of_line_(ptr self__, ptr fn__);
ptr ROU215_dup_(ptr self__);
void ROU215_put_kwdname_(ptr self__, int nm__);
ptr ROU215_sather_code_(ptr self__);
ptr ROU215_initialize_(ptr self__, ptr initarg__);
void ROU215_resolve_predef_types_(ptr self__, int index__);
void ROU215_semant_(ptr self__, ptr symtab__);
ptr ROU215_typeof_(ptr self__);
int ROU215_get_offset_(ptr self__);
void ROU215_cprint_offset_(ptr self__, ptr outfile__);
ptr ROU215_get_constval_(ptr self__);
void ROU215_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void ROU215_cprint_cname_(ptr self__, ptr outfile__);
void ROU215_cprint_extern_(ptr self__, ptr outfile__);
void ROU215_cprint_access_value_(ptr self__, ptr outfile__);
void ROU215_cprint_init_code_(ptr self__, ptr outfile__);
int ROU215_inst_ind_(ptr self__);
ptr ROU215_inst_cls_(ptr self__);
char ROU215_is_dispatched_(ptr self__);
ptr ROU215_dispatched_(ptr self__);
ptr ROU215_undispatched_(ptr self__);
char ROU215_arithtype_(ptr self__);
char ROU215_int_type_p_(ptr self__);
char ROU215_bool_type_p_(ptr self__);
char ROU215_char_type_p_(ptr self__);
char ROU215_real_type_p_(ptr self__);
char ROU215_double_type_p_(ptr self__);
char ROU215_str_type_p_(ptr self__);
char ROU215_nonptr_p_(ptr self__);
ptr ROU215_resolve_arithtype_(ptr self__, ptr tp__);
int ROU215_ctype_(ptr self__);
void ROU215_cprint_ctype_(ptr self__, ptr outfile__);
void ROU215_cprint_void_(ptr self__, ptr outfile__);
char ROU215_array_type_p_(ptr self__);
char ROU215_array2_type_p_(ptr self__);
char ROU215_array3_type_p_(ptr self__);
char ROU215_array4_type_p_(ptr self__);
ptr ROU215_rettype_(ptr self__);
ptr ROU215_paramstype_(ptr self__);
char ROU215_conforms_to_(ptr self__, ptr tp__);
char ROU215_param_type_conforms_to_(ptr self__, ptr tp__);
ptr ROU215_full_name_(ptr self__);
extern int attr_ent_ROU215[];

ptr ROU215_create_(ptr self__, ptr parmstp__, ptr rtp__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,62,ls2720_,"(ROUT_TYPEOB_S):Return type object should be INSTANT_TYPEOB_S");
   ptr gl4789_;
   static int gl4790_;
   static union dtype_ gl4791_;
   int gl485_;

   res__ = (ptr)new_(215,0);
   PATT_(res__,16) = (ptr)parmstp__;
   if ((rtp__ != 0)) {
      gl4789_ = rtp__;
      gl485_ = TYPE_(gl4789_);
      if ((gl485_ != 150)) {
         ERR96_format_error_msg_(0,ln__,STR20_s_(STR20_create_(0),(ptr)(&ls2720_)));
      }
      else {
      }
   }
   else {
   }
   PATT_(res__,20) = (ptr)rtp__;
   IATT_(res__,12) = (int)ln__;

   ret0__:
   return (res__);
}

void ROU215_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,12) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,12));

   ret0__:
   return;
}

ptr ROU215_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)ROU215_create_(self__,LST102_dup_(PATT_(self__,16)),INS150_dup_(PATT_(self__,20)),IATT_(self__,12));

   ret0__:
   return (res__);
}

void ROU215_put_kwdname_(ptr self__, int nm__)
{
   ptr gl4792_;
   static int gl4793_;
   static union dtype_ gl4794_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4792_ = x__;
   cache_dispatch_(gl4792_,796,gl4793_,INTVAL_(gl4794_));
   IATT_(gl4792_,INTVAL_(gl4794_)) = (int)nm__;

   ret0__:
   return;
}

ptr ROU215_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr ROU215_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void ROU215_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void ROU215_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr ROU215_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int ROU215_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void ROU215_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr ROU215_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ROU215_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void ROU215_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ROU215_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ROU215_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ROU215_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

int ROU215_inst_ind_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr ROU215_inst_cls_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char ROU215_is_dispatched_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr ROU215_dispatched_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ROU215_undispatched_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char ROU215_arithtype_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_int_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_bool_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_char_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_real_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_double_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_str_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_nonptr_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr ROU215_resolve_arithtype_(ptr self__, ptr tp__)
{
   ptr res__ = 0;

   res__ = (ptr)GLO68_int_typeob_s_;

   ret0__:
   return (res__);
}

int ROU215_ctype_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)1;

   ret0__:
   return (res__);
}

void ROU215_cprint_ctype_(ptr self__, ptr outfile__)
{

   CLA148_cprint_ctype_(ROU215_inst_cls_(self__),outfile__);

   ret0__:
   return;
}

void ROU215_cprint_void_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,3,ls2693_,"S_");
   SATHER_STR_(20,7,ls2694_,"_VOID_");
   SATHER_STR_(20,2,ls1692_,"0");

   if (ROU215_nonptr_p_(self__)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2693_));
      ROU215_cprint_ctype_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2694_));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls1692_));
   }

   ret0__:
   return;
}

char ROU215_array_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_array2_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_array3_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ROU215_array4_type_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr ROU215_rettype_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,20);

   ret0__:
   return (res__);
}

ptr ROU215_paramstype_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,16);

   ret0__:
   return (res__);
}

char ROU215_conforms_to_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;
   ptr gl4795_;
   static int gl4796_;
   static union dtype_ gl4797_;
   ptr gl4798_;
   static int gl4799_;
   static union dtype_ gl4800_;
   ptr gl486_;
   ptr    tp_rettype__ = 0;
   ptr    self_rettype__ = 0;

   if ((tp__ == 0)) {
      if ((self__ == 0)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      res__ = (char)LST102_conforms_to_(ROU215_paramstype_(self__),0);
      if (res__) {
         if ((ROU215_rettype_(self__) != 0)) {
            res__ = (char)0;
         }
         else {
         }
      }
      else {
      }
   }
   else {
      gl4795_ = tp__;
      cache_dispatch_(gl4795_,1705,gl4796_,INTVAL_(gl4797_));
      tp_rettype__ = (ptr)PFN_(gl4797_)(gl4795_);
      self_rettype__ = (ptr)ROU215_rettype_(self__);
      if ((tp_rettype__ == 0)) {
         if ((self_rettype__ == 0)) {
            res__ = (char)1;
         }
         else {
            res__ = (char)0;
         }
      }
      else {
         res__ = (char)INS150_conforms_to_(tp_rettype__,self_rettype__);
      }
      if (res__) {
         gl4798_ = tp__;
         cache_dispatch_(gl4798_,1709,gl4799_,INTVAL_(gl4800_));
         gl486_ = PFN_(gl4800_)(gl4798_);
         res__ = (char)LST102_conforms_to_(ROU215_paramstype_(self__),gl486_);
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

char ROU215_param_type_conforms_to_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr ROU215_full_name_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,7,ls2725_,"<void>");
   SATHER_STR_(20,3,ls2726_,"->");
   ptr    rstr__ = 0;
   ptr    pstr__ = 0;

   rstr__ = S_ptr_VOID_;
   pstr__ = S_ptr_VOID_;
   if ((PATT_(self__,20) == 0)) {
      rstr__ = (ptr)(ptr)(&ls2725_);
   }
   else {
      rstr__ = (ptr)INS150_full_name_(PATT_(self__,20));
   }
   if ((PATT_(self__,16) == 0)) {
      pstr__ = (ptr)(ptr)(&ls2725_);
   }
   else {
      pstr__ = (ptr)LST102_full_name_(PATT_(self__,16));
   }
   res__ = (ptr)STR20_s_(STR20_s_(pstr__,(ptr)(&ls2726_)),rstr__);

   ret0__:
   return (res__);
}


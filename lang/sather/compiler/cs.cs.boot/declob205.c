/* declob205.c : Sather class: DECLOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr TYP149_dup_(ptr self__);
extern void TYP149_resolve_predef_types_(ptr self__, int index__);
extern int TYP149_inst_ind_(ptr self__);
extern char TYP149_is_dispatched_(ptr self__);
extern char TYP149_nonptr_p_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
#include "macros_.h"



/*constant*/ int DEC205_print_indent_ = 2;
ptr DEC205_create_(ptr self__, ptr t__, int ln__);
void DEC205_out_of_line_(ptr self__, ptr fn__);
ptr DEC205_dup_(ptr self__);
void DEC205_put_kwdname_(ptr self__, int nm__);
ptr DEC205_sather_code_(ptr self__);
ptr DEC205_initialize_(ptr self__, ptr initarg__);
void DEC205_resolve_predef_types_(ptr self__, int index__);
void DEC205_semant_(ptr self__, ptr symtab__);
ptr DEC205_typeof_(ptr self__);
int DEC205_get_offset_(ptr self__);
void DEC205_cprint_offset_(ptr self__, ptr outfile__);
ptr DEC205_get_constval_(ptr self__);
void DEC205_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void DEC205_cprint_cname_(ptr self__, ptr outfile__);
void DEC205_cprint_extern_(ptr self__, ptr outfile__);
void DEC205_cprint_access_value_(ptr self__, ptr outfile__);
void DEC205_cprint_init_code_(ptr self__, ptr outfile__);
char DEC205_undefined_p_(ptr self__);
int DEC205_declob_s_name_(ptr self__);
void DEC205_validate_dispatches_and_get_ext_strs_(ptr self__);
void DEC205_cprint_code_(ptr self__, ptr outfile__);
void DEC205_cprint_decln_(ptr self__, ptr outfile__);
extern int attr_ent_DEC205[];

ptr DEC205_create_(ptr self__, ptr t__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(205,0);
   PATT_(res__,16) = (ptr)t__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void DEC205_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr DEC205_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl4744_;
   static int gl4745_;
   static union dtype_ gl4746_;
   ptr gl482_;

   gl4744_ = PATT_(self__,16);
   cache_dispatch_(gl4744_,471,gl4745_,INTVAL_(gl4746_));
   gl482_ = PFN_(gl4746_)(gl4744_);
   res__ = (ptr)DEC205_create_(self__,gl482_,IATT_(self__,4));

   ret0__:
   return (res__);
}

void DEC205_put_kwdname_(ptr self__, int nm__)
{
   ptr gl4747_;
   static int gl4748_;
   static union dtype_ gl4749_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4747_ = x__;
   cache_dispatch_(gl4747_,796,gl4748_,INTVAL_(gl4749_));
   IATT_(gl4747_,INTVAL_(gl4749_)) = (int)nm__;

   ret0__:
   return;
}

ptr DEC205_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr DEC205_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void DEC205_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl4750_;
   static int gl4751_;
   static union dtype_ gl4752_;

   gl4750_ = PATT_(self__,16);
   cache_dispatch_(gl4750_,522,gl4751_,INTVAL_(gl4752_));
   VFN_(gl4752_)(gl4750_,index__);

   ret0__:
   return;
}

void DEC205_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr DEC205_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,16);

   ret0__:
   return (res__);
}

int DEC205_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void DEC205_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr DEC205_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void DEC205_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void DEC205_cprint_cname_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,3,ls1551_,"__");

   (void)SAT99_s_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls1551_));

   ret0__:
   return;
}

void DEC205_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void DEC205_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void DEC205_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char DEC205_undefined_p_(ptr self__)
{
   char res__ = S_char_VOID_;
   ptr gl4753_;
   static int gl4754_;
   static union dtype_ gl4755_;
   int gl483_;

   gl4753_ = PATT_(self__,16);
   cache_dispatch_(gl4753_,1547,gl4754_,INTVAL_(gl4755_));
   gl483_ = IFN_(gl4755_)(gl4753_);
   res__ = (char)(gl483_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int DEC205_declob_s_name_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

void DEC205_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   SATHER_STR_(20,35,ls1550_,"(DECLOB_S): Dispatch on basic type");
   ptr gl4756_;
   static int gl4757_;
   static union dtype_ gl4758_;
   ptr gl4759_;
   static int gl4760_;
   static union dtype_ gl4761_;

   gl4756_ = PATT_(self__,16);
   cache_dispatch_(gl4756_,1511,gl4757_,INTVAL_(gl4758_));
   if (CFN_(gl4758_)(gl4756_)) {
      gl4759_ = PATT_(self__,16);
      cache_dispatch_(gl4759_,1549,gl4760_,INTVAL_(gl4761_));
      if (CFN_(gl4761_)(gl4759_)) {
         ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1550_)));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

void DEC205_cprint_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void DEC205_cprint_decln_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}


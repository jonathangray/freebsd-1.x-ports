/* declob205.c : Sather class: DECLOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern ERR96_format_error_msg_();
extern ptr SAT99_s_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern ptr STR20_create_();
extern ptr TYP149_dup_();
extern TYP149_resolve_predef_types_();
extern int TYP149_inst_ind_();
extern char TYP149_is_dispatched_();
extern char TYP149_nonptr_p_();
extern ptr STR20_s_();
#include "macros_.h"



/*constant*/ int DEC205_print_indent_ = 2;
ptr DEC205_create_();
DEC205_out_of_line_();
ptr DEC205_dup_();
DEC205_put_kwdname_();
ptr DEC205_sather_code_();
ptr DEC205_initialize_();
DEC205_resolve_predef_types_();
DEC205_semant_();
ptr DEC205_typeof_();
int DEC205_get_offset_();
DEC205_cprint_offset_();
ptr DEC205_get_constval_();
DEC205_cont_cprint_code_();
DEC205_cprint_cname_();
DEC205_cprint_extern_();
DEC205_cprint_access_value_();
DEC205_cprint_init_code_();
char DEC205_undefined_p_();
int DEC205_declob_s_name_();
DEC205_validate_dispatches_and_get_ext_strs_();
DEC205_cprint_code_();
DEC205_cprint_decln_();
extern int attr_ent_DEC205[];

ptr DEC205_create_(self__,t__,ln__)
ptr self__;
ptr t__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(205,0);
   PATT_(res__,16) = (ptr)t__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

DEC205_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr DEC205_dup_(self__)
ptr self__;
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

DEC205_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
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

ptr DEC205_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr DEC205_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

DEC205_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
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

DEC205_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr DEC205_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,16);

   ret0__:
   return (res__);
}

int DEC205_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

DEC205_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr DEC205_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

DEC205_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

DEC205_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls1551_,"__");

   (void)SAT99_s_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls1551_));

   ret0__:
   return;
}

DEC205_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

DEC205_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

DEC205_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char DEC205_undefined_p_(self__)
ptr self__;
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

int DEC205_declob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

DEC205_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
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

DEC205_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

DEC205_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}


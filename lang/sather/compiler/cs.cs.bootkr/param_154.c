/* param_154.c : Sather class: PARAM_DECLOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern ptr STR20_create_();
extern ptr TYP149_dup_();
extern ptr STR20_s_();
extern ptr TYP149_sather_code_();
extern TYP149_resolve_predef_types_();
extern ptr TYP149_inst_cls_();
extern int TYP149_inst_ind_();
extern char TYP149_is_dispatched_();
extern char TYP149_nonptr_p_();
extern TYP149_cprint_ctype_();
extern int ERR96_out_of_line_err_info_();
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern ERR96_format_error_msg_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern SYM186_enter_sym_();
#include "macros_.h"



/*constant*/ int PAR154_print_indent_ = 2;
ptr PAR154_create_();
PAR154_out_of_line_();
ptr PAR154_dup_();
PAR154_put_kwdname_();
ptr PAR154_sather_code_();
ptr PAR154_initialize_();
PAR154_resolve_predef_types_();
PAR154_semant_();
ptr PAR154_typeof_();
int PAR154_get_offset_();
PAR154_cprint_offset_();
ptr PAR154_get_constval_();
PAR154_cont_cprint_code_();
PAR154_cprint_cname_();
PAR154_cprint_extern_();
PAR154_cprint_access_value_();
PAR154_cprint_init_code_();
char PAR154_undefined_p_();
int PAR154_declob_s_name_();
PAR154_validate_dispatches_and_get_ext_strs_();
PAR154_cprint_code_();
PAR154_cprint_decln_();
extern int attr_ent_PAR154[];

ptr PAR154_create_(self__,nm__,tp__,ln__)
ptr self__;
int nm__;
ptr tp__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(154,0);
   IATT_(res__,12) = (int)nm__;
   PATT_(res__,16) = (ptr)tp__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

PAR154_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr PAR154_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4303_;
   static int gl4304_;
   static union dtype_ gl4305_;
   ptr gl442_;

   gl4303_ = PATT_(self__,16);
   cache_dispatch_(gl4303_,471,gl4304_,INTVAL_(gl4305_));
   gl442_ = PFN_(gl4305_)(gl4303_);
   res__ = (ptr)PAR154_create_(self__,IATT_(self__,12),gl442_,IATT_(self__,4));

   ret0__:
   return (res__);
}

PAR154_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4306_;
   static int gl4307_;
   static union dtype_ gl4308_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4306_ = x__;
   cache_dispatch_(gl4306_,796,gl4307_,INTVAL_(gl4308_));
   IATT_(gl4306_,INTVAL_(gl4308_)) = (int)nm__;

   ret0__:
   return;
}

ptr PAR154_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4309_;
   static int gl4310_;
   static union dtype_ gl4311_;
   ptr gl443_;

   gl4309_ = PATT_(self__,16);
   cache_dispatch_(gl4309_,801,gl4310_,INTVAL_(gl4311_));
   gl443_ = PFN_(gl4311_)(gl4309_);
   res__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),gl443_);

   ret0__:
   return (res__);
}

ptr PAR154_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

PAR154_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl4312_;
   static int gl4313_;
   static union dtype_ gl4314_;

   gl4312_ = PATT_(self__,16);
   cache_dispatch_(gl4312_,522,gl4313_,INTVAL_(gl4314_));
   VFN_(gl4314_)(gl4312_,index__);

   ret0__:
   return;
}

PAR154_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl4315_;
   static int gl4316_;
   static union dtype_ gl4317_;
   ptr    co__ = 0;

   SYM186_enter_sym_(symtab__,IATT_(self__,12),self__);
   gl4315_ = PATT_(self__,16);
   cache_dispatch_(gl4315_,418,gl4316_,INTVAL_(gl4317_));
   co__ = (ptr)PFN_(gl4317_)(gl4315_);
   if (CATT_(co__,14)) {
      CATT_(GLO68_curr_class_inst_,13) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

ptr PAR154_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,16);

   ret0__:
   return (res__);
}

int PAR154_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

PAR154_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr PAR154_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

PAR154_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

PAR154_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls1551_,"__");

   (void)SAT99_s_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,12))),(ptr)(&ls1551_));

   ret0__:
   return;
}

PAR154_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

PAR154_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   PAR154_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}

PAR154_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

char PAR154_undefined_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl4318_;
   static int gl4319_;
   static union dtype_ gl4320_;
   int gl444_;

   gl4318_ = PATT_(self__,16);
   cache_dispatch_(gl4318_,1547,gl4319_,INTVAL_(gl4320_));
   gl444_ = IFN_(gl4320_)(gl4318_);
   res__ = (char)(gl444_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int PAR154_declob_s_name_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);

   ret0__:
   return (res__);
}

PAR154_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   SATHER_STR_(20,35,ls1550_,"(DECLOB_S): Dispatch on basic type");
   ptr gl4321_;
   static int gl4322_;
   static union dtype_ gl4323_;
   ptr gl4324_;
   static int gl4325_;
   static union dtype_ gl4326_;

   gl4321_ = PATT_(self__,16);
   cache_dispatch_(gl4321_,1511,gl4322_,INTVAL_(gl4323_));
   if (CFN_(gl4323_)(gl4321_)) {
      gl4324_ = PATT_(self__,16);
      cache_dispatch_(gl4324_,1549,gl4325_,INTVAL_(gl4326_));
      if (CFN_(gl4326_)(gl4324_)) {
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

PAR154_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

PAR154_cprint_decln_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl4327_;
   static int gl4328_;
   static union dtype_ gl4329_;

   gl4327_ = PATT_(self__,16);
   cache_dispatch_(gl4327_,696,gl4328_,INTVAL_(gl4329_));
   VFN_(gl4329_)(gl4327_,outfile__);
   (void)SAT99_c_(outfile__,' ');
   PAR154_cprint_cname_(self__,outfile__);

   ret0__:
   return;
}


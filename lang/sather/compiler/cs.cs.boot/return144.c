/* return144.c : Sather class: RETURN_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr ERR96_def_filename_(ptr self__, int ln__);
extern int ERR96_def_lineno_(ptr self__, int ln__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern /*shared*/ char COM82_dbg_mode_;
extern void PRI187_cprint_restore_exec_info_(ptr self__, ptr outfile__);
extern char GLO94_check_is_on_(ptr self__);
extern void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
#include "macros_.h"



/*constant*/ int RET144_print_indent_ = 2;
ptr RET144_create_(ptr self__, int ln__);
void RET144_out_of_line_(ptr self__, ptr fn__);
ptr RET144_dup_(ptr self__);
void RET144_put_kwdname_(ptr self__, int nm__);
ptr RET144_sather_code_(ptr self__);
ptr RET144_initialize_(ptr self__, ptr initarg__);
void RET144_resolve_predef_types_(ptr self__, int index__);
void RET144_semant_(ptr self__, ptr symtab__);
ptr RET144_typeof_(ptr self__);
int RET144_get_offset_(ptr self__);
void RET144_cprint_offset_(ptr self__, ptr outfile__);
ptr RET144_get_constval_(ptr self__);
void RET144_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void RET144_cprint_cname_(ptr self__, ptr outfile__);
void RET144_cprint_extern_(ptr self__, ptr outfile__);
void RET144_cprint_access_value_(ptr self__, ptr outfile__);
void RET144_cprint_init_code_(ptr self__, ptr outfile__);
char RET144_typechk_exprs_(ptr self__, ptr tp__);
ptr RET144_gen_temps_(ptr self__);
void RET144_gen_goto_tags_(ptr self__, ptr block__);
void RET144_validate_dispatches_and_get_ext_strs_(ptr self__);
void RET144_cprint_code_(ptr self__, ptr outfile__);
extern int attr_ent_RET144[];

ptr RET144_create_(ptr self__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(144,1);
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void RET144_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr RET144_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)RET144_create_(self__,IATT_(self__,4));

   ret0__:
   return (res__);
}

void RET144_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3686_;
   static int gl3687_;
   static union dtype_ gl3688_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3686_ = x__;
   cache_dispatch_(gl3686_,796,gl3687_,INTVAL_(gl3688_));
   IATT_(gl3686_,INTVAL_(gl3688_)) = (int)nm__;

   ret0__:
   return;
}

ptr RET144_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr RET144_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void RET144_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void RET144_semant_(ptr self__, ptr symtab__)
{


   ret0__:
   return;
}

ptr RET144_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int RET144_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void RET144_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr RET144_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void RET144_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void RET144_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void RET144_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void RET144_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void RET144_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

char RET144_typechk_exprs_(ptr self__, ptr tp__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr RET144_gen_temps_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void RET144_gen_goto_tags_(ptr self__, ptr block__)
{


   ret0__:
   return;
}

void RET144_validate_dispatches_and_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

void RET144_cprint_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,14,ls2648_,"goto ret0__;\n");

   if (GLO94_check_is_on_(0)) {
      (void)SAT99_indent_(SAT99_inc_ln_(SAT99_c_(outfile__,'\n'),1));
      PRI187_cprint_restore_exec_info_(0,outfile__);
   }
   else {
   }
   (void)SAT99_indent_(outfile__);
   if ((COM82_dbg_mode_ == 1)) {
      DBT190_addCLine_(0,ERR96_def_filename_(0,IATT_(self__,4)),ERR96_def_lineno_(0,IATT_(self__,4)),PATT_(outfile__,8),IATT_(outfile__,16));
   }
   else {
   }
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2648_)),1);

   ret0__:
   return;
}


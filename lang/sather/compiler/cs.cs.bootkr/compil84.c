/* compil84.c : Sather class: COMPILE_KEYS_CONST, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern struct { int tp_; int sz_; char st_; } gs1060_;
extern struct { int tp_; int sz_; char st_; } gs1062_;
extern struct { int tp_; int sz_; char st_; } gs1064_;
extern struct { int tp_; int sz_; char st_; } gs1066_;
extern struct { int tp_; int sz_; char st_; } gs1068_;
extern struct { int tp_; int sz_; char st_; } gs1070_;
extern struct { int tp_; int sz_; char st_; } gs1072_;
extern struct { int tp_; int sz_; char st_; } gs1074_;
#include "macros_.h"



/*constant*/ ptr COM84_source_files_kw_ = (ptr)(&gs1060_);
/*constant*/ ptr COM84_object_files_kw_ = (ptr)(&gs1062_);
/*constant*/ ptr COM84_cc_flags_kw_ = (ptr)(&gs1064_);
/*constant*/ ptr COM84_c_macro_kw_ = (ptr)(&gs1066_);
/*constant*/ ptr COM84_c_name_kw_ = (ptr)(&gs1068_);
/*constant*/ ptr COM84_include_kw_ = (ptr)(&gs1070_);
/*constant*/ ptr COM84_sather_home_kw_ = (ptr)(&gs1072_);
/*constant*/ ptr COM84_c_compiler_kw_ = (ptr)(&gs1074_);
/*constant*/ int COM84_source_files_ind_ = 0;
/*constant*/ int COM84_object_files_ind_ = 1;
/*constant*/ int COM84_cc_flags_ind_ = 2;
/*constant*/ int COM84_c_macro_ind_ = 3;
/*constant*/ int COM84_c_name_ind_ = 4;
/*constant*/ int COM84_include_ind_ = 5;
/*constant*/ int COM84_sather_home_ind_ = 6;
/*constant*/ int COM84_c_compiler_ind_ = 7;
/*constant*/ int COM84_compile_keys_fst_ind_ = 0;
/*constant*/ int COM84_compile_keys_lst_ind_ = 7;
/*constant*/ int COM84_num_compile_keys_ = 8;
/*constant*/ int COM84_non_compile_key_ind_ = 8;
/*constant*/ int COM84_eof_tok_ = -1;
/*constant*/ int COM84_ident_tok_ = -2;
/*constant*/ int COM84_qexp_tok_ = -3;
char COM84_compile_key_p_();
ptr COM84_initialize_();
extern int attr_ent_COM84[];

char COM84_compile_key_p_(self__,i__)
ptr self__;
int i__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((i__ <= 7) & (i__ >= 0));

   ret0__:
   return (res__);
}

ptr COM84_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* compil82.c : Sather class: COMPILER_OPTIONS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ char GC_is_on_;

extern int STR20_length_();
extern ptr STR20_substring_();
extern ptr UPA167_canonicalize_();
extern /*constant*/ ptr INS88_sather_home_;
extern /*shared*/ ptr INS88_default_environment_;
extern struct { int tp_; int sz_; char st_; } gs1016_;
#include "macros_.h"



/*constant*/ int COM82_browser_mode_ = 1;
/*constant*/ int COM82_interpreter_mode_ = 2;
/*constant*/ int COM82_debugger_mode_ = 4;
/*shared*/ ptr COM82_target_dir_;
/*shared*/ char COM82_warnings_only_;
/*shared*/ ptr COM82_cmdfile_name_;
/*shared*/ ptr COM82_sather_home_;
/*shared*/ char COM82_is_opt_sather_home_;
/*shared*/ ptr COM82_object_files_;
COM82_set_sather_home_();
/*shared*/ char COM82_all_np_classes_;
/*shared*/ char COM82_k_and_r_c_;
/*shared*/ char COM82_verbose_code_;
/*shared*/ char COM82_gen_base_;
/*shared*/ char COM82_rt_code_check_;
/*shared*/ char COM82_gen_all_;
/*shared*/ char COM82_warn_rt_val_;
/*shared*/ int COM82_compiler_mode_;
/*shared*/ char COM82_dbg_mode_;
/*shared*/ char COM82_do_timing_;
/*shared*/ char COM82_new_compilation_;
/*shared*/ char COM82_print_desinfo_;
/*shared*/ char COM82_dot_prefix_;
/*shared*/ ptr COM82_target_environment_;
/*shared*/ char COM82_has_gc_;
ptr COM82_initialize_();
extern int attr_ent_COM82[];

COM82_set_sather_home_(self__,sat_home__)
ptr self__;
ptr sat_home__;
{
   int    len__ = S_int_VOID_;

   len__ = S_int_VOID_;
   if ((sat_home__ == 0)) {
      goto ret0__;
   }
   else {
   }
   sat_home__ = (ptr)UPA167_canonicalize_(0,sat_home__);
   len__ = (int)STR20_length_(sat_home__);
   if (((CATT_(sat_home__, 8 + (((len__ - 1)))) == '/') & (len__ > 1))) {
      sat_home__ = (ptr)STR20_substring_(sat_home__,0,(len__ - 2));
   }
   else {
   }
   COM82_sather_home_ = (ptr)sat_home__;

   ret0__:
   return;
}

ptr COM82_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


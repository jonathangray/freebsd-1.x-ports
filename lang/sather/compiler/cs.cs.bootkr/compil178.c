/* compil178.c : Sather class: COMPILE_KEYOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern error_exit();

extern ptr STR20_create_();
extern ptr STR20_s_();
#include "macros_.h"



COM178_error_msg_();
COM178_error_exit_();
COM178_start_();
COM178_terminate_();
ptr COM178_cmacro_def_();
COM178_insert_();
ptr COM178_create_();
int COM178_info_size_();
ptr COM178_ith_str_();
ptr COM178_ith_info_();
COM178_cprint_self_();
ptr COM178_initialize_();
extern int attr_ent_COM178[];

COM178_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

COM178_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

COM178_start_(self__,nm__)
ptr self__;
ptr nm__;
{
   SATHER_STR_(20,38,ls1033_,"(COMPILE_KEYOB) : Unexpected string \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   COM178_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1033_)),nm__),(ptr)(&ls632_)));

   ret0__:
   return;
}

COM178_terminate_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr COM178_cmacro_def_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

COM178_insert_(self__,nm__)
ptr self__;
ptr nm__;
{


   ret0__:
   return;
}

ptr COM178_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(178,1);

   ret0__:
   return (res__);
}

int COM178_info_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr COM178_ith_str_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr COM178_ith_info_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

COM178_cprint_self_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr COM178_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


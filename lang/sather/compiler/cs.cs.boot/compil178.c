/* compil178.c : Sather class: COMPILE_KEYOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);

extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
#include "macros_.h"



void COM178_error_msg_(ptr self__, ptr s__);
void COM178_error_exit_(ptr self__, ptr s__);
void COM178_start_(ptr self__, ptr nm__);
void COM178_terminate_(ptr self__);
ptr COM178_cmacro_def_(ptr self__, int i__);
void COM178_insert_(ptr self__, ptr nm__);
ptr COM178_create_(ptr self__);
int COM178_info_size_(ptr self__);
ptr COM178_ith_str_(ptr self__, int i__);
ptr COM178_ith_info_(ptr self__, int i__);
void COM178_cprint_self_(ptr self__, ptr outfile__);
ptr COM178_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_COM178[];

void COM178_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void COM178_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

void COM178_start_(ptr self__, ptr nm__)
{
   SATHER_STR_(20,38,ls1033_,"(COMPILE_KEYOB) : Unexpected string \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   COM178_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1033_)),nm__),(ptr)(&ls632_)));

   ret0__:
   return;
}

void COM178_terminate_(ptr self__)
{


   ret0__:
   return;
}

ptr COM178_cmacro_def_(ptr self__, int i__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void COM178_insert_(ptr self__, ptr nm__)
{


   ret0__:
   return;
}

ptr COM178_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(178,1);

   ret0__:
   return (res__);
}

int COM178_info_size_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr COM178_ith_str_(ptr self__, int i__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr COM178_ith_info_(ptr self__, int i__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void COM178_cprint_self_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr COM178_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


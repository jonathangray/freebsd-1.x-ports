/* generi83.c : Sather class: GENERIC_KEYOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern error_exit();

extern ptr OUT80_s_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr LIS87_create_();
extern ptr LIS87_push_();
#include "macros_.h"



GEN83_error_msg_();
GEN83_error_exit_();
GEN83_start_();
GEN83_terminate_();
ptr GEN83_cmacro_def_();
GEN83_insert_();
ptr GEN83_create_();
int GEN83_info_size_();
ptr GEN83_ith_str_();
ptr GEN83_ith_info_();
GEN83_cprint_self_();
ptr GEN83_initialize_();
extern int attr_ent_GEN83[];

GEN83_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

GEN83_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

GEN83_start_(self__,nm__)
ptr self__;
ptr nm__;
{
   SATHER_STR_(20,38,ls1033_,"(COMPILE_KEYOB) : Unexpected string \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   GEN83_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1033_)),nm__),(ptr)(&ls632_)));

   ret0__:
   return;
}

GEN83_terminate_(self__)
ptr self__;
{


   ret0__:
   return;
}

ptr GEN83_cmacro_def_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

GEN83_insert_(self__,nm__)
ptr self__;
ptr nm__;
{

   PATT_(self__,4) = (ptr)LIS87_push_(PATT_(self__,4),nm__);

   ret0__:
   return;
}

ptr GEN83_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(83,0);
   PATT_(res__,4) = (ptr)LIS87_create_(PATT_(res__,4),1);

   ret0__:
   return (res__);
}

int GEN83_info_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,4),4);

   ret0__:
   return (res__);
}

ptr GEN83_ith_str_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(PATT_(self__,4), 16 + ((i__) << 2));

   ret0__:
   return (res__);
}

ptr GEN83_ith_info_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

GEN83_cprint_self_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,11,ls1041_,"(generic) ");
   SATHER_STR_(20,2,ls780_,"\n");
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   (void)OUT80_s_(outfile__,(ptr)(&ls1041_));
   i__ = (int)0;
   sz__ = (int)IATT_(PATT_(self__,4),4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1013_;
      }
      else {
      }
      (void)OUT80_s_(OUT80_s_(outfile__,PATT_(PATT_(self__,4), 16 + ((i__) << 2))),(ptr)(&ls780_));
      i__ = (int)(i__ + 1);
   }
goto_tag_1013_: ;
   (void)OUT80_s_(outfile__,(ptr)(&ls780_));

   ret0__:
   return;
}

ptr GEN83_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


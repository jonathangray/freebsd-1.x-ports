/* c_macr85.c : Sather class: C_MACRO_KEYOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern error_exit();

extern ptr OUT80_s_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr LIS181_push_();
extern ptr LIS87_push_();
extern ptr LIS87_create_();
extern ptr LIS181_create_();
#include "macros_.h"



C_M85_error_msg_();
C_M85_error_exit_();
C_M85_start_();
C_M85_terminate_();
ptr C_M85_cmacro_def_();
C_M85_insert_();
ptr C_M85_create_();
int C_M85_info_size_();
ptr C_M85_ith_str_();
ptr C_M85_ith_info_();
C_M85_cprint_self_();
ptr C_M85_initialize_();
/*constant*/ int C_M85_def_num_ent_ = 10;
extern int attr_ent_C_M85[];

C_M85_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

C_M85_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

C_M85_start_(self__,nm__)
ptr self__;
ptr nm__;
{
   SATHER_STR_(20,43,ls1056_,"(C_MACRO_KEYOB) : Unexpected quoted expr \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   if ((PATT_(self__,8) == 0)) {
      PATT_(self__,8) = (ptr)LIS87_push_(LIS87_create_(PATT_(self__,8),(- 1)),nm__);
   }
   else {
      C_M85_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1056_)),nm__),(ptr)(&ls632_)));
   }

   ret0__:
   return;
}

C_M85_terminate_(self__)
ptr self__;
{

   if ((PATT_(self__,8) != 0)) {
      PATT_(self__,4) = (ptr)LIS181_push_(PATT_(self__,4),PATT_(self__,8));
      PATT_(self__,8) = (ptr)0;
   }
   else {
   }

   ret0__:
   return;
}

ptr C_M85_cmacro_def_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(PATT_(PATT_(self__,4), 16 + ((i__) << 2)), 16 + ((0) << 2));

   ret0__:
   return (res__);
}

C_M85_insert_(self__,nm__)
ptr self__;
ptr nm__;
{
   SATHER_STR_(20,48,ls1057_,"(C_MACRO_KEYOB) : Quoted expr expected before \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   if ((PATT_(self__,8) == 0)) {
      C_M85_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1057_)),nm__),(ptr)(&ls632_)));
   }
   else {
      PATT_(self__,8) = (ptr)LIS87_push_(PATT_(self__,8),nm__);
   }

   ret0__:
   return;
}

ptr C_M85_create_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(85,0);
   PATT_(res__,4) = (ptr)LIS181_create_(PATT_(res__,4),10);

   ret0__:
   return (res__);
}

int C_M85_info_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,4),4);

   ret0__:
   return (res__);
}

ptr C_M85_ith_str_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr C_M85_ith_info_(self__,i__)
ptr self__;
int i__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(PATT_(self__,4), 16 + ((i__) << 2));

   ret0__:
   return (res__);
}

C_M85_cprint_self_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,11,ls1058_,"(c_macro) ");
   SATHER_STR_(20,2,ls780_,"\n");
   int    i__ = S_int_VOID_;
   int    size__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   (void)OUT80_s_(outfile__,(ptr)(&ls1058_));
   i__ = (int)0;
   size__ = (int)IATT_(PATT_(self__,4),4);
   while (1) {
      if ((i__ >= size__)) {
         goto goto_tag_1014_;
      }
      else {
      }
      j__ = (int)0;
      sz__ = S_int_VOID_;
      if ((PATT_(PATT_(self__,4), 16 + ((i__) << 2)) != 0)) {
         sz__ = (int)IATT_(PATT_(PATT_(self__,4), 16 + ((i__) << 2)),4);
      }
      else {
      }
      while (1) {
         if ((j__ >= sz__)) {
            goto goto_tag_1015_;
         }
         else {
         }
         (void)OUT80_s_(OUT80_s_(outfile__,PATT_(PATT_(PATT_(self__,4), 16 + ((i__) << 2)), 16 + ((j__) << 2))),(ptr)(&ls780_));
         j__ = (int)(j__ + 1);
      }
   goto_tag_1015_: ;
      (void)OUT80_s_(outfile__,(ptr)(&ls780_));
      i__ = (int)(i__ + 1);
   }
goto_tag_1014_: ;
   (void)OUT80_s_(outfile__,(ptr)(&ls780_));

   ret0__:
   return;
}

ptr C_M85_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


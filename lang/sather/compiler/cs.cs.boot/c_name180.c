/* c_name180.c : Sather class: C_NAME_KEYOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void error_msg(ptr s__);
extern ptr str_ptr_(ptr s__);
extern void error_exit(ptr s__);

extern ptr OUT80_s_(ptr self__, ptr st__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr LIS181_push_(ptr self__, ptr e__);
extern ptr LIS87_create_(ptr self__, int init_size__);
extern ptr LIS87_push_(ptr self__, ptr e__);
extern ptr LIS181_create_(ptr self__, int init_size__);
#include "macros_.h"



void C_N180_error_msg_(ptr self__, ptr s__);
void C_N180_error_exit_(ptr self__, ptr s__);
void C_N180_start_(ptr self__, ptr nm__);
void C_N180_terminate_(ptr self__);
ptr C_N180_cmacro_def_(ptr self__, int i__);
void C_N180_insert_(ptr self__, ptr nm__);
ptr C_N180_create_(ptr self__);
int C_N180_info_size_(ptr self__);
ptr C_N180_ith_str_(ptr self__, int i__);
ptr C_N180_ith_info_(ptr self__, int i__);
void C_N180_cprint_self_(ptr self__, ptr outfile__);
ptr C_N180_initialize_(ptr self__, ptr initarg__);
/*constant*/ int C_N180_def_num_ent_ = 20;
extern int attr_ent_C_N180[];

void C_N180_error_msg_(ptr self__, ptr s__)
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

void C_N180_error_exit_(ptr self__, ptr s__)
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

void C_N180_start_(ptr self__, ptr nm__)
{
   SATHER_STR_(20,38,ls1033_,"(COMPILE_KEYOB) : Unexpected string \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   C_N180_error_msg_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1033_)),nm__),(ptr)(&ls632_)));

   ret0__:
   return;
}

void C_N180_terminate_(ptr self__)
{
   SATHER_STR_(20,42,ls1052_,"(C_NAME_KEYOB) : Wrong number of entries\n");

   if ((PATT_(self__,8) != 0)) {
      if ((IATT_(PATT_(self__,8),4) != 2)) {
         C_N180_error_exit_(self__,(ptr)(&ls1052_));
      }
      else {
         PATT_(self__,4) = (ptr)LIS181_push_(PATT_(self__,4),PATT_(self__,8));
         PATT_(self__,8) = (ptr)0;
      }
   }
   else {
   }

   ret0__:
   return;
}

ptr C_N180_cmacro_def_(ptr self__, int i__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void C_N180_insert_(ptr self__, ptr nm__)
{
   SATHER_STR_(20,42,ls1049_,"(C_NAME_KEYOB) : Unexpected third entry (");
   SATHER_STR_(20,4,ls1050_,") (");
   SATHER_STR_(20,3,ls1051_,")\n");

   if ((PATT_(self__,8) == 0)) {
      PATT_(self__,8) = (ptr)LIS87_push_(LIS87_create_(PATT_(self__,8),(- 1)),nm__);
   }
   else {
      if ((IATT_(PATT_(self__,8),4) >= 2)) {
         C_N180_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1049_)),PATT_(PATT_(self__,8), 16 + ((0) << 2))),(ptr)(&ls1050_)),PATT_(PATT_(self__,8), 16 + ((1) << 2))),(ptr)(&ls1050_)),nm__),(ptr)(&ls1051_)));
      }
      else {
         PATT_(self__,8) = (ptr)LIS87_push_(PATT_(self__,8),nm__);
      }
   }

   ret0__:
   return;
}

ptr C_N180_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(180,0);
   PATT_(res__,4) = (ptr)LIS181_create_(PATT_(res__,4),20);

   ret0__:
   return (res__);
}

int C_N180_info_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,4),4);

   ret0__:
   return (res__);
}

ptr C_N180_ith_str_(ptr self__, int i__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr C_N180_ith_info_(ptr self__, int i__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(PATT_(self__,4), 16 + ((i__) << 2));

   ret0__:
   return (res__);
}

void C_N180_cprint_self_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,10,ls1053_,"(c_name) ");
   SATHER_STR_(20,2,ls780_,"\n");
   int    i__ = S_int_VOID_;
   int    size__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   (void)OUT80_s_(outfile__,(ptr)(&ls1053_));
   i__ = (int)0;
   size__ = (int)IATT_(PATT_(self__,4),4);
   while (1) {
      if ((i__ >= size__)) {
         goto goto_tag_4559_;
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
            goto goto_tag_4560_;
         }
         else {
         }
         (void)OUT80_s_(OUT80_s_(outfile__,PATT_(PATT_(PATT_(self__,4), 16 + ((i__) << 2)), 16 + ((j__) << 2))),(ptr)(&ls780_));
         j__ = (int)(j__ + 1);
      }
   goto_tag_4560_: ;
      (void)OUT80_s_(outfile__,(ptr)(&ls780_));
      i__ = (int)(i__ + 1);
   }
goto_tag_4559_: ;
   (void)OUT80_s_(outfile__,(ptr)(&ls780_));

   ret0__:
   return;
}

ptr C_N180_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


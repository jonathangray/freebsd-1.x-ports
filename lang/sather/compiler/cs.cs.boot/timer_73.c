/* timer_73.c : Sather class: TIMER, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern ptr create_timer();
extern int fork();
extern ptr report_timer(int pid__);

extern void ERR96_warning_msg_(ptr self__, ptr s__);
extern int INT15_bit_and_(int self__, int i__);
extern int INT15_rshift_(int self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_i_(ptr self__, int in__);
extern int UNI90_system_(ptr self__, ptr com__);
extern /*shared*/ int UNI90_unix_error_;
extern void UNI90_exit_(ptr self__, int i__);
#include "macros_.h"



ptr TIM73_create_(ptr self__);
ptr TIM73_c_timer_create_(ptr self__, double u_time__, double s_time__);
ptr TIM73_time_syscall_(ptr self__, ptr com__);
double TIM73_minus_time_(ptr self__, ptr t__);
double TIM73_time_spent_(ptr self__);
ptr TIM73_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_TIM73[];

ptr TIM73_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)create_timer();

   ret0__:
   return (res__);
}

ptr TIM73_c_timer_create_(ptr self__, double u_time__, double s_time__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(73,1);
   DATT_(res__,8) = (double)u_time__;
   DATT_(res__,16) = (double)s_time__;

   ret0__:
   return (res__);
}

ptr TIM73_time_syscall_(ptr self__, ptr com__)
{
   ptr res__ = 0;
   SATHER_STR_(20,20,ls938_,"*** Shell command \"");
   SATHER_STR_(20,25,ls939_,"\" not executed. Reason: ");
   SATHER_STR_(20,21,ls941_,", cf <errno.h> ***\n\n");
   int    pid__ = S_int_VOID_;
   int    status__ = S_int_VOID_;

   pid__ = (int)fork();
   if ((pid__ == 0)) {
      status__ = (int)UNI90_system_(0,com__);
      if ((INT15_rshift_(INT15_bit_and_(status__,0xFF00),8) == 0xFF)) {
         ERR96_warning_msg_(0,STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls938_)),com__),(ptr)(&ls939_)),UNI90_unix_error_),(ptr)(&ls941_)));
      }
      else {
      }
      UNI90_exit_(0,0);
   }
   else {
      if ((pid__ == (- 1))) {
      }
      else {
         res__ = (ptr)report_timer(pid__);
      }
   }

   ret0__:
   return (res__);
}

double TIM73_minus_time_(ptr self__, ptr t__)
{
   double res__ = S_double_VOID_;

   res__ = (double)((DATT_(self__,8) - DATT_(t__,8)) + (DATT_(self__,16) - DATT_(t__,16)));

   ret0__:
   return (res__);
}

double TIM73_time_spent_(ptr self__)
{
   double res__ = S_double_VOID_;

   if ((self__ != 0)) {
      res__ = (double)(DATT_(self__,8) + DATT_(self__,16));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr TIM73_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


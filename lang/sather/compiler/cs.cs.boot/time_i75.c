/* time_i75.c : Sather class: TIME_INT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern float i_to_r(int i__);
extern double i_to_d(int i__);
extern char i_to_c(int i__);
extern int rshift(int i__, int j__);
extern int bit_not(int i__);
extern int lshift(int i__, int j__);
extern int arith_rshift(int i__, int j__);
extern int bit_or(int i__, int j__);
extern ptr address_of_int(int i__);
extern ptr ctime(ptr t__);
extern int u_mod(int i__, int j__);
extern int bit_and(int i__, int j__);
extern int bit_xor(int i__, int j__);
#define address_of_int(x) ((ptr)&(x))

extern int INT15_lshift_(int self__, int i__);
extern double INT15_to_d_(int self__);
extern int INT15_bit_not_(int self__);
extern double DOU18_pow_(double self__, double x__);
extern int DOU18_to_i_(double self__);
extern ptr STR20_create_sized_(ptr self__, int n__);
extern ptr STR20_i_(ptr self__, int in__);
extern double DOU18_sqrt_(double self__);
extern ptr STR20_create_(ptr self__);
extern int INT15_bit_and_(int self__, int i__);
extern int INT15_rshift_(int self__, int i__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_reverse_(ptr self__);
extern ptr STR20_from_c_str_(ptr self__, ptr st__);
#include "macros_.h"



int TIM75_u_mod_(int self__, int i__);
int TIM75_mod_(int self__, int i__);
float TIM75_to_r_(int self__);
double TIM75_to_d_(int self__);
char TIM75_to_c_(int self__);
ptr TIM75_to_s_(int self__);
int TIM75_bit_and_(int self__, int i__);
int TIM75_bit_or_(int self__, int i__);
int TIM75_bit_xor_(int self__, int i__);
int TIM75_bit_not_(int self__);
int TIM75_lshift_(int self__, int i__);
int TIM75_rshift_(int self__, int i__);
int TIM75_arith_rshift_(int self__, int i__);
int TIM75_abs_(int self__);
char TIM75_nth_bit_(int self__, int n__);
int TIM75_set_nth_bit_(int self__, int n__);
int TIM75_unset_nth_bit_(int self__, int n__);
int TIM75_max_(int self__, int i__);
int TIM75_min_(int self__, int i__);
int TIM75_pow_(int self__, int i__);
int TIM75_sqrt_(int self__);
ptr TIM75_to_octal_(int self__);
ptr TIM75_to_binary_(int self__);
ptr TIM75_to_hex_(int self__);
ptr TIM75_ctime_(int self__);
int TIM75_initialize_(int self__, ptr initarg__);
extern int attr_ent_TIM75[];

int TIM75_u_mod_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)u_mod(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_mod_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   if ((self__ >= 0)) {
      res__ = (int)TIM75_u_mod_(self__,i__);
   }
   else {
      res__ = (int)(i__ - TIM75_u_mod_((- self__),i__));
      if ((res__ == i__)) {
         res__ = (int)0;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

float TIM75_to_r_(int self__)
{
   float res__ = S_float_VOID_;

   res__ = (float)i_to_r(self__);

   ret0__:
   return (res__);
}

double TIM75_to_d_(int self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)i_to_d(self__);

   ret0__:
   return (res__);
}

char TIM75_to_c_(int self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)i_to_c(self__);

   ret0__:
   return (res__);
}

ptr TIM75_to_s_(int self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_i_(res__,self__);

   ret0__:
   return (res__);
}

int TIM75_bit_and_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_and(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_or_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_or(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_xor_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_xor(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_not_(int self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_not(self__);

   ret0__:
   return (res__);
}

int TIM75_lshift_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)lshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_rshift_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)rshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_arith_rshift_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)arith_rshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_abs_(int self__)
{
   int res__ = S_int_VOID_;

   if ((self__ < 0)) {
      res__ = (int)(- self__);
   }
   else {
      res__ = (int)self__;
   }

   ret0__:
   return (res__);
}

char TIM75_nth_bit_(int self__, int n__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(TIM75_bit_and_(self__,INT15_lshift_(1,n__)) != 0);

   ret0__:
   return (res__);
}

int TIM75_set_nth_bit_(int self__, int n__)
{
   int res__ = S_int_VOID_;

   res__ = (int)TIM75_bit_or_(self__,INT15_lshift_(1,n__));

   ret0__:
   return (res__);
}

int TIM75_unset_nth_bit_(int self__, int n__)
{
   int res__ = S_int_VOID_;

   res__ = (int)TIM75_bit_and_(self__,INT15_bit_not_(INT15_lshift_(1,n__)));

   ret0__:
   return (res__);
}

int TIM75_max_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   if ((i__ > self__)) {
      res__ = (int)i__;
   }
   else {
      res__ = (int)self__;
   }

   ret0__:
   return (res__);
}

int TIM75_min_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   if ((i__ > self__)) {
      res__ = (int)self__;
   }
   else {
      res__ = (int)i__;
   }

   ret0__:
   return (res__);
}

int TIM75_pow_(int self__, int i__)
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_((0.5 + DOU18_pow_(TIM75_to_d_(self__),INT15_to_d_(i__))));

   ret0__:
   return (res__);
}

int TIM75_sqrt_(int self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_(DOU18_sqrt_(TIM75_to_d_(self__)));

   ret0__:
   return (res__);
}

ptr TIM75_to_octal_(int self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls1692_,"0");
   int    i__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   i__ = (int)self__;
   while (1) {
      if ((i__ == 0)) {
         goto goto_tag_976_;
      }
      else {
      }
      res__ = (ptr)STR20_i_(res__,INT15_bit_and_(i__,7));
      i__ = (int)INT15_rshift_(i__,3);
   }
goto_tag_976_: ;
   res__ = (ptr)STR20_s_(res__,(ptr)(&ls1692_));
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}

ptr TIM75_to_binary_(int self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   i__ = (int)self__;
   while (1) {
      res__ = (ptr)STR20_i_(res__,INT15_bit_and_(i__,1));
      i__ = (int)INT15_rshift_(i__,1);
      if ((i__ == 0)) {
         goto goto_tag_977_;
      }
      else {
      }
   }
goto_tag_977_: ;
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}

ptr TIM75_to_hex_(int self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls3253_,"A");
   SATHER_STR_(20,2,ls3254_,"B");
   SATHER_STR_(20,2,ls43_,"C");
   SATHER_STR_(20,2,ls3255_,"D");
   SATHER_STR_(20,2,ls3256_,"E");
   SATHER_STR_(20,2,ls1738_,"F");
   SATHER_STR_(20,3,ls3257_,"x0");
   int    i__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   i__ = (int)self__;
   while (1) {
      switch (INT15_bit_and_(i__,15)) {
         case (10) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls3253_));
            break;
         case (11) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls3254_));
            break;
         case (12) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls43_));
            break;
         case (13) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls3255_));
            break;
         case (14) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls3256_));
            break;
         case (15) :
            res__ = (ptr)STR20_s_(res__,(ptr)(&ls1738_));
            break;
         default:
            res__ = (ptr)STR20_i_(res__,INT15_bit_and_(i__,15));
            ;
      }
      i__ = (int)INT15_rshift_(i__,4);
      if ((i__ == 0)) {
         goto goto_tag_978_;
      }
      else {
      }
   }
goto_tag_978_: ;
   res__ = (ptr)STR20_s_(res__,(ptr)(&ls3257_));
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}

ptr TIM75_ctime_(int self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_from_c_str_(0,ctime(address_of_int(self__)));

   ret0__:
   return (res__);
}

int TIM75_initialize_(int self__, ptr initarg__)
{
   int res__ = S_int_VOID_;

   res__ = (int)self__;

   ret0__:
   return (res__);
}


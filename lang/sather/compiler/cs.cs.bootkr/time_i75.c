/* time_i75.c : Sather class: TIME_INT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern float i_to_r();
extern double i_to_d();
extern char i_to_c();
extern int rshift();
extern int bit_not();
extern int lshift();
extern int arith_rshift();
extern int bit_or();
extern ptr address_of_int();
extern ptr ctime();
extern int u_mod();
extern int bit_and();
extern int bit_xor();
#define address_of_int(x) ((ptr)&(x))

extern int INT15_lshift_();
extern double INT15_to_d_();
extern int INT15_bit_not_();
extern double DOU18_pow_();
extern int DOU18_to_i_();
extern ptr STR20_create_sized_();
extern ptr STR20_i_();
extern double DOU18_sqrt_();
extern ptr STR20_create_();
extern int INT15_bit_and_();
extern int INT15_rshift_();
extern ptr STR20_s_();
extern ptr STR20_reverse_();
extern ptr STR20_from_c_str_();
#include "macros_.h"



int TIM75_u_mod_();
int TIM75_mod_();
float TIM75_to_r_();
double TIM75_to_d_();
char TIM75_to_c_();
ptr TIM75_to_s_();
int TIM75_bit_and_();
int TIM75_bit_or_();
int TIM75_bit_xor_();
int TIM75_bit_not_();
int TIM75_lshift_();
int TIM75_rshift_();
int TIM75_arith_rshift_();
int TIM75_abs_();
char TIM75_nth_bit_();
int TIM75_set_nth_bit_();
int TIM75_unset_nth_bit_();
int TIM75_max_();
int TIM75_min_();
int TIM75_pow_();
int TIM75_sqrt_();
ptr TIM75_to_octal_();
ptr TIM75_to_binary_();
ptr TIM75_to_hex_();
ptr TIM75_ctime_();
int TIM75_initialize_();
extern int attr_ent_TIM75[];

int TIM75_u_mod_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)u_mod(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_mod_(self__,i__)
int self__;
int i__;
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

float TIM75_to_r_(self__)
int self__;
{
   float res__ = S_float_VOID_;

   res__ = (float)i_to_r(self__);

   ret0__:
   return (res__);
}

double TIM75_to_d_(self__)
int self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)i_to_d(self__);

   ret0__:
   return (res__);
}

char TIM75_to_c_(self__)
int self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)i_to_c(self__);

   ret0__:
   return (res__);
}

ptr TIM75_to_s_(self__)
int self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_i_(res__,self__);

   ret0__:
   return (res__);
}

int TIM75_bit_and_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_and(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_or_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_or(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_xor_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_xor(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_bit_not_(self__)
int self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_not(self__);

   ret0__:
   return (res__);
}

int TIM75_lshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)lshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_rshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)rshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_arith_rshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)arith_rshift(self__,i__);

   ret0__:
   return (res__);
}

int TIM75_abs_(self__)
int self__;
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

char TIM75_nth_bit_(self__,n__)
int self__;
int n__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(TIM75_bit_and_(self__,INT15_lshift_(1,n__)) != 0);

   ret0__:
   return (res__);
}

int TIM75_set_nth_bit_(self__,n__)
int self__;
int n__;
{
   int res__ = S_int_VOID_;

   res__ = (int)TIM75_bit_or_(self__,INT15_lshift_(1,n__));

   ret0__:
   return (res__);
}

int TIM75_unset_nth_bit_(self__,n__)
int self__;
int n__;
{
   int res__ = S_int_VOID_;

   res__ = (int)TIM75_bit_and_(self__,INT15_bit_not_(INT15_lshift_(1,n__)));

   ret0__:
   return (res__);
}

int TIM75_max_(self__,i__)
int self__;
int i__;
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

int TIM75_min_(self__,i__)
int self__;
int i__;
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

int TIM75_pow_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_((0.5 + DOU18_pow_(TIM75_to_d_(self__),INT15_to_d_(i__))));

   ret0__:
   return (res__);
}

int TIM75_sqrt_(self__)
int self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_(DOU18_sqrt_(TIM75_to_d_(self__)));

   ret0__:
   return (res__);
}

ptr TIM75_to_octal_(self__)
int self__;
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

ptr TIM75_to_binary_(self__)
int self__;
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

ptr TIM75_to_hex_(self__)
int self__;
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

ptr TIM75_ctime_(self__)
int self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_from_c_str_(0,ctime(address_of_int(self__)));

   ret0__:
   return (res__);
}

int TIM75_initialize_(self__,initarg__)
int self__;
ptr initarg__;
{
   int res__ = S_int_VOID_;

   res__ = (int)self__;

   ret0__:
   return (res__);
}


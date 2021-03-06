/* int___15.c : Sather class: INT, dbg=F, gc=T, chk=F */

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
extern int u_mod();
extern int bit_and();
extern int bit_xor();

extern double DOU18_pow_();
extern int DOU18_to_i_();
extern ptr STR20_create_sized_();
extern ptr STR20_i_();
extern double DOU18_sqrt_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_reverse_();


int INT15_u_mod_();
int INT15_mod_();
float INT15_to_r_();
double INT15_to_d_();
char INT15_to_c_();
ptr INT15_to_s_();
int INT15_bit_and_();
int INT15_bit_or_();
int INT15_bit_xor_();
int INT15_bit_not_();
int INT15_lshift_();
int INT15_rshift_();
int INT15_arith_rshift_();
int INT15_abs_();
char INT15_nth_bit_();
int INT15_set_nth_bit_();
int INT15_unset_nth_bit_();
int INT15_max_();
int INT15_min_();
int INT15_pow_();
int INT15_sqrt_();
ptr INT15_to_octal_();
ptr INT15_to_binary_();
ptr INT15_to_hex_();
extern int attr_ent_INT15[];

int INT15_u_mod_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)u_mod(self__,i__);

   ret0__:
   return (res__);
}

int INT15_mod_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   if ((self__ >= 0)) {
      res__ = (int)INT15_u_mod_(self__,i__);
   }
   else {
      res__ = (int)(i__ - INT15_u_mod_((- self__),i__));
      if ((res__ == i__)) {
         res__ = (int)0;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

float INT15_to_r_(self__)
int self__;
{
   float res__ = S_float_VOID_;

   res__ = (float)i_to_r(self__);

   ret0__:
   return (res__);
}

double INT15_to_d_(self__)
int self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)i_to_d(self__);

   ret0__:
   return (res__);
}

char INT15_to_c_(self__)
int self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)i_to_c(self__);

   ret0__:
   return (res__);
}

ptr INT15_to_s_(self__)
int self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_i_(res__,self__);

   ret0__:
   return (res__);
}

int INT15_bit_and_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_and(self__,i__);

   ret0__:
   return (res__);
}

int INT15_bit_or_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_or(self__,i__);

   ret0__:
   return (res__);
}

int INT15_bit_xor_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_xor(self__,i__);

   ret0__:
   return (res__);
}

int INT15_bit_not_(self__)
int self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)bit_not(self__);

   ret0__:
   return (res__);
}

int INT15_lshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)lshift(self__,i__);

   ret0__:
   return (res__);
}

int INT15_rshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)rshift(self__,i__);

   ret0__:
   return (res__);
}

int INT15_arith_rshift_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)arith_rshift(self__,i__);

   ret0__:
   return (res__);
}

int INT15_abs_(self__)
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

char INT15_nth_bit_(self__,n__)
int self__;
int n__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(INT15_bit_and_(self__,INT15_lshift_(1,n__)) != 0);

   ret0__:
   return (res__);
}

int INT15_set_nth_bit_(self__,n__)
int self__;
int n__;
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_bit_or_(self__,INT15_lshift_(1,n__));

   ret0__:
   return (res__);
}

int INT15_unset_nth_bit_(self__,n__)
int self__;
int n__;
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_bit_and_(self__,INT15_bit_not_(INT15_lshift_(1,n__)));

   ret0__:
   return (res__);
}

int INT15_max_(self__,i__)
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

int INT15_min_(self__,i__)
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

int INT15_pow_(self__,i__)
int self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_((0.5 + DOU18_pow_(INT15_to_d_(self__),INT15_to_d_(i__))));

   ret0__:
   return (res__);
}

int INT15_sqrt_(self__)
int self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)DOU18_to_i_(DOU18_sqrt_(INT15_to_d_(self__)));

   ret0__:
   return (res__);
}

ptr INT15_to_octal_(self__)
int self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls1692_,"0");
   int    i__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   i__ = (int)self__;
   while (1) {
      if ((i__ == 0)) {
         goto goto_tag_517_;
      }
      else {
      }
      res__ = (ptr)STR20_i_(res__,INT15_bit_and_(i__,7));
      i__ = (int)INT15_rshift_(i__,3);
   }
goto_tag_517_: ;
   res__ = (ptr)STR20_s_(res__,(ptr)(&ls1692_));
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}

ptr INT15_to_binary_(self__)
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
         goto goto_tag_518_;
      }
      else {
      }
   }
goto_tag_518_: ;
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}

ptr INT15_to_hex_(self__)
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
         goto goto_tag_519_;
      }
      else {
      }
   }
goto_tag_519_: ;
   res__ = (ptr)STR20_s_(res__,(ptr)(&ls3257_));
   (void)STR20_reverse_(res__);

   ret0__:
   return (res__);
}


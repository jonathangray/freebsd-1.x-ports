/* real__17.c : Sather class: REAL, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern double ceil();
extern double fabs();
extern double fmod();
extern double pow();
extern double sqrt();
extern double r_to_d();
extern int r_to_i();
extern double floor();
extern int d_to_i();

extern ptr STR20_create_sized_();
extern ptr STR20_r_();


double REA17_to_d_();
int REA17_to_i_();
ptr REA17_to_s_();
float REA17_max_();
float REA17_min_();
double REA17_pow_();
double REA17_sqrt_();
int REA17_floor_();
int REA17_ceiling_();
int REA17_round_();
double REA17_abs_();
double REA17_mod_();
extern int attr_ent_REA17[];

double REA17_to_d_(self__)
float self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)r_to_d(self__);

   ret0__:
   return (res__);
}

int REA17_to_i_(self__)
float self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)r_to_i(self__);

   ret0__:
   return (res__);
}

ptr REA17_to_s_(self__)
float self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_r_(res__,self__);

   ret0__:
   return (res__);
}

float REA17_max_(self__,r__)
float self__;
float r__;
{
   float res__ = S_float_VOID_;

   if ((r__ > self__)) {
      res__ = (float)r__;
   }
   else {
      res__ = (float)self__;
   }

   ret0__:
   return (res__);
}

float REA17_min_(self__,r__)
float self__;
float r__;
{
   float res__ = S_float_VOID_;

   if ((r__ > self__)) {
      res__ = (float)self__;
   }
   else {
      res__ = (float)r__;
   }

   ret0__:
   return (res__);
}

double REA17_pow_(self__,x__)
float self__;
float x__;
{
   double res__ = S_double_VOID_;

   res__ = (double)pow(self__,x__);

   ret0__:
   return (res__);
}

double REA17_sqrt_(self__)
float self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)sqrt(self__);

   ret0__:
   return (res__);
}

int REA17_floor_(self__)
float self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(floor(self__));

   ret0__:
   return (res__);
}

int REA17_ceiling_(self__)
float self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(ceil(self__));

   ret0__:
   return (res__);
}

int REA17_round_(self__)
float self__;
{
   int res__ = S_int_VOID_;

   if ((self__ < 0.0)) {
      res__ = (int)d_to_i((self__ - .5));
   }
   else {
      res__ = (int)d_to_i((self__ + .5));
   }

   ret0__:
   return (res__);
}

double REA17_abs_(self__)
float self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)fabs(self__);

   ret0__:
   return (res__);
}

double REA17_mod_(self__,x__)
float self__;
double x__;
{
   double res__ = S_double_VOID_;

   res__ = (double)fmod(self__,x__);

   ret0__:
   return (res__);
}


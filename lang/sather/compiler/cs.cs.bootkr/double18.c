/* double18.c : Sather class: DOUBLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern double sqrt();
extern int d_to_i();
extern double ceil();
extern double fabs();
extern double fmod();
extern float d_to_r();
extern double floor();
extern double pow();

extern ptr STR20_create_sized_();
extern ptr STR20_d_();


float DOU18_to_r_();
int DOU18_to_i_();
ptr DOU18_to_s_();
double DOU18_max_();
double DOU18_min_();
double DOU18_pow_();
double DOU18_sqrt_();
int DOU18_floor_();
int DOU18_ceiling_();
int DOU18_round_();
double DOU18_abs_();
double DOU18_mod_();
extern int attr_ent_DOU18[];

float DOU18_to_r_(self__)
double self__;
{
   float res__ = S_float_VOID_;

   res__ = (float)d_to_r(self__);

   ret0__:
   return (res__);
}

int DOU18_to_i_(self__)
double self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(self__);

   ret0__:
   return (res__);
}

ptr DOU18_to_s_(self__)
double self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_d_(res__,self__);

   ret0__:
   return (res__);
}

double DOU18_max_(self__,r__)
double self__;
double r__;
{
   double res__ = S_double_VOID_;

   if ((r__ > self__)) {
      res__ = (double)r__;
   }
   else {
      res__ = (double)self__;
   }

   ret0__:
   return (res__);
}

double DOU18_min_(self__,r__)
double self__;
double r__;
{
   double res__ = S_double_VOID_;

   if ((r__ > self__)) {
      res__ = (double)self__;
   }
   else {
      res__ = (double)r__;
   }

   ret0__:
   return (res__);
}

double DOU18_pow_(self__,x__)
double self__;
double x__;
{
   double res__ = S_double_VOID_;

   res__ = (double)pow(self__,x__);

   ret0__:
   return (res__);
}

double DOU18_sqrt_(self__)
double self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)sqrt(self__);

   ret0__:
   return (res__);
}

int DOU18_floor_(self__)
double self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(floor(self__));

   ret0__:
   return (res__);
}

int DOU18_ceiling_(self__)
double self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(ceil(self__));

   ret0__:
   return (res__);
}

int DOU18_round_(self__)
double self__;
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

double DOU18_abs_(self__)
double self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)fabs(self__);

   ret0__:
   return (res__);
}

double DOU18_mod_(self__,x__)
double self__;
double x__;
{
   double res__ = S_double_VOID_;

   res__ = (double)fmod(self__,x__);

   ret0__:
   return (res__);
}


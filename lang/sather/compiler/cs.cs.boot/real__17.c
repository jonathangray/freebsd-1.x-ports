/* real__17.c : Sather class: REAL, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern double ceil(double x__);
extern double fabs(double x__);
extern double fmod(double x__, double y__);
extern double pow(double x__, double y__);
extern double sqrt(double x__);
extern double r_to_d(float r__);
extern int r_to_i(float r__);
extern double floor(double x__);
extern int d_to_i(double d__);

extern ptr STR20_create_sized_(ptr self__, int n__);
extern ptr STR20_r_(ptr self__, float re__);


double REA17_to_d_(float self__);
int REA17_to_i_(float self__);
ptr REA17_to_s_(float self__);
float REA17_max_(float self__, float r__);
float REA17_min_(float self__, float r__);
double REA17_pow_(float self__, float x__);
double REA17_sqrt_(float self__);
int REA17_floor_(float self__);
int REA17_ceiling_(float self__);
int REA17_round_(float self__);
double REA17_abs_(float self__);
double REA17_mod_(float self__, double x__);
extern int attr_ent_REA17[];

double REA17_to_d_(float self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)r_to_d(self__);

   ret0__:
   return (res__);
}

int REA17_to_i_(float self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)r_to_i(self__);

   ret0__:
   return (res__);
}

ptr REA17_to_s_(float self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_r_(res__,self__);

   ret0__:
   return (res__);
}

float REA17_max_(float self__, float r__)
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

float REA17_min_(float self__, float r__)
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

double REA17_pow_(float self__, float x__)
{
   double res__ = S_double_VOID_;

   res__ = (double)pow(self__,x__);

   ret0__:
   return (res__);
}

double REA17_sqrt_(float self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)sqrt(self__);

   ret0__:
   return (res__);
}

int REA17_floor_(float self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(floor(self__));

   ret0__:
   return (res__);
}

int REA17_ceiling_(float self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(ceil(self__));

   ret0__:
   return (res__);
}

int REA17_round_(float self__)
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

double REA17_abs_(float self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)fabs(self__);

   ret0__:
   return (res__);
}

double REA17_mod_(float self__, double x__)
{
   double res__ = S_double_VOID_;

   res__ = (double)fmod(self__,x__);

   ret0__:
   return (res__);
}


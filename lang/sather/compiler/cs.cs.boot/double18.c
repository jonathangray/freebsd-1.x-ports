/* double18.c : Sather class: DOUBLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern double sqrt(double x__);
extern int d_to_i(double d__);
extern double ceil(double x__);
extern double fabs(double x__);
extern double fmod(double x__, double y__);
extern float d_to_r(double d__);
extern double floor(double x__);
extern double pow(double x__, double y__);

extern ptr STR20_create_sized_(ptr self__, int n__);
extern ptr STR20_d_(ptr self__, double do__);


float DOU18_to_r_(double self__);
int DOU18_to_i_(double self__);
ptr DOU18_to_s_(double self__);
double DOU18_max_(double self__, double r__);
double DOU18_min_(double self__, double r__);
double DOU18_pow_(double self__, double x__);
double DOU18_sqrt_(double self__);
int DOU18_floor_(double self__);
int DOU18_ceiling_(double self__);
int DOU18_round_(double self__);
double DOU18_abs_(double self__);
double DOU18_mod_(double self__, double x__);
extern int attr_ent_DOU18[];

float DOU18_to_r_(double self__)
{
   float res__ = S_float_VOID_;

   res__ = (float)d_to_r(self__);

   ret0__:
   return (res__);
}

int DOU18_to_i_(double self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(self__);

   ret0__:
   return (res__);
}

ptr DOU18_to_s_(double self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_sized_(0,12);
   (void)STR20_d_(res__,self__);

   ret0__:
   return (res__);
}

double DOU18_max_(double self__, double r__)
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

double DOU18_min_(double self__, double r__)
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

double DOU18_pow_(double self__, double x__)
{
   double res__ = S_double_VOID_;

   res__ = (double)pow(self__,x__);

   ret0__:
   return (res__);
}

double DOU18_sqrt_(double self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)sqrt(self__);

   ret0__:
   return (res__);
}

int DOU18_floor_(double self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(floor(self__));

   ret0__:
   return (res__);
}

int DOU18_ceiling_(double self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)d_to_i(ceil(self__));

   ret0__:
   return (res__);
}

int DOU18_round_(double self__)
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

double DOU18_abs_(double self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)fabs(self__);

   ret0__:
   return (res__);
}

double DOU18_mod_(double self__, double x__)
{
   double res__ = S_double_VOID_;

   res__ = (double)fmod(self__,x__);

   ret0__:
   return (res__);
}


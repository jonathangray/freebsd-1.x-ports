/* in____8.c : Sather class: IN, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern double FIL11_get_d_();
extern ptr FIL11_get_s_();
extern FIL11_unget_c_();
extern ptr FIL11_in_();
extern char FIL11_get_c_();
extern char FIL11_get_b_();
extern int FIL11_get_i_();
extern float FIL11_get_r_();
extern char FIL11_check_eof_();
#include "macros_.h"



/*shared*/ ptr IN_8_file_;
char IN_8_get_c_();
char IN_8_get_b_();
int IN_8_get_i_();
float IN_8_get_r_();
double IN_8_get_d_();
ptr IN_8_get_s_();
IN_8_unget_c_();
char IN_8_check_eof_();
extern int attr_ent_IN_8[];

char IN_8_get_c_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)FIL11_get_c_(IN_8_file_);

   ret0__:
   return (res__);
}

char IN_8_get_b_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)FIL11_get_b_(IN_8_file_);

   ret0__:
   return (res__);
}

int IN_8_get_i_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)FIL11_get_i_(IN_8_file_);

   ret0__:
   return (res__);
}

float IN_8_get_r_(self__)
ptr self__;
{
   float res__ = S_float_VOID_;

   res__ = (float)FIL11_get_r_(IN_8_file_);

   ret0__:
   return (res__);
}

double IN_8_get_d_(self__)
ptr self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)FIL11_get_d_(IN_8_file_);

   ret0__:
   return (res__);
}

ptr IN_8_get_s_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)FIL11_get_s_(IN_8_file_);

   ret0__:
   return (res__);
}

IN_8_unget_c_(self__,ch__)
ptr self__;
char ch__;
{

   FIL11_unget_c_(IN_8_file_,ch__);

   ret0__:
   return;
}

char IN_8_check_eof_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)FIL11_check_eof_(IN_8_file_);

   ret0__:
   return (res__);
}


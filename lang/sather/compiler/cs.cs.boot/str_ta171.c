/* str_ta171.c : Sather class: STR_TAB_FILE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern ptr str_ptr_(ptr s__);
extern int scanf_val_();
extern ptr stdin_();
extern ptr stdout_();
extern ptr stderr_();
extern char check_eof_(ptr fp__);
extern int get_ci_(ptr fp__);
extern int fscanfi_(ptr fp__);
extern double fscanfd_(ptr fp__);
extern void ungetc(char ch__, ptr fp__);
extern void fputc(char ch__, ptr fp__);
extern void fprintfi_(ptr fp__, int in__);
extern void fprintfs_(ptr fp__, ptr st__);
extern void fprintfd_(ptr fp__, double do__);
extern ptr fopenr_(ptr s__);
extern ptr fopenw_(ptr s__);
extern ptr fopena_(ptr s__);
extern void fclose(ptr fp__);
extern void fflush(ptr fp__);
extern ptr popenr_(ptr c__);
extern ptr popenw_(ptr c__);
extern void pclose(ptr fp__);
extern void fseek(ptr fp__, int off__, int where__);

extern ptr STR20_create_(ptr self__);
extern ptr STR20_c_(ptr self__, char ch__);
extern char INT15_to_c_(int self__);
#include "macros_.h"



/*constant*/ int STR171_eof_ = -1;
/*shared*/ int STR171_error_val_;
/*constant*/ int STR171_read_error_ = 1;
/*constant*/ int STR171_eof_error_ = 2;
/*constant*/ int STR171_open_error_ = 3;
int STR171_error_(ptr self__);
ptr STR171_create_(ptr self__);
ptr STR171_in_(ptr self__);
ptr STR171_out_(ptr self__);
ptr STR171_err_(ptr self__);
char STR171_check_eof_(ptr self__);
char STR171_get_c_(ptr self__);
int STR171_get_ci_(ptr self__);
char STR171_get_b_(ptr self__);
int STR171_get_i_(ptr self__);
float STR171_get_r_(ptr self__);
double STR171_get_d_(ptr self__);
ptr STR171_get_s_(ptr self__);
ptr STR171_get_s_up_to_(ptr self__, char c0__);
void STR171_unget_c_(ptr self__, char ch__);
ptr STR171_b_(ptr self__, char bo__);
ptr STR171_c_(ptr self__, char ch__);
ptr STR171_i_(ptr self__, int in__);
ptr STR171_s_(ptr self__, ptr st__);
ptr STR171_r_(ptr self__, float re__);
ptr STR171_d_(ptr self__, double do__);
ptr STR171_nl_(ptr self__);
void STR171_open_for_read_(ptr self__, ptr nm__);
void STR171_open_for_write_(ptr self__, ptr nm__);
void STR171_open_for_append_(ptr self__, ptr nm__);
void STR171_close_(ptr self__);
ptr STR171_get_s_of_len_(ptr self__, int n__);
void STR171_flush_(ptr self__);
void STR171_open_pipe_for_read_(ptr self__, ptr command__);
void STR171_open_pipe_for_write_(ptr self__, ptr command__);
void STR171_close_pipe_(ptr self__);
void STR171_seek_relative_(ptr self__, int n__);
void STR171_seek_from_front_(ptr self__, int n__);
void STR171_seek_from_end_(ptr self__, int n__);
ptr STR171_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR171[];

int STR171_error_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    sv__ = S_int_VOID_;

   res__ = (int)STR171_error_val_;
   STR171_error_val_ = (int)0;
   if ((res__ == 0)) {
      sv__ = (int)scanf_val_();
      if ((sv__ == 0)) {
         res__ = (int)1;
      }
      else {
         if ((sv__ == -1)) {
            res__ = (int)2;
         }
         else {
         }
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR171_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(171,0);

   ret0__:
   return (res__);
}

ptr STR171_in_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,3,ls1310_,"in");
   ptr gl4517_;

   gl4517_ = res__;
   res__ = (ptr)new_(171,0);
   PATT_(res__,4) = (ptr)stdin_();
   PATT_(res__,8) = (ptr)(ptr)(&ls1310_);

   ret0__:
   return (res__);
}

ptr STR171_out_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls1309_,"out");
   ptr gl4518_;

   gl4518_ = res__;
   res__ = (ptr)new_(171,0);
   PATT_(res__,4) = (ptr)stdout_();
   PATT_(res__,8) = (ptr)(ptr)(&ls1309_);

   ret0__:
   return (res__);
}

ptr STR171_err_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls2561_,"err");
   ptr gl4519_;

   gl4519_ = res__;
   res__ = (ptr)new_(171,0);
   PATT_(res__,4) = (ptr)stderr_();
   PATT_(res__,8) = (ptr)(ptr)(&ls2561_);

   ret0__:
   return (res__);
}

char STR171_check_eof_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)check_eof_(PATT_(self__,4));

   ret0__:
   return (res__);
}

char STR171_get_c_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)INT15_to_c_(get_ci_(PATT_(self__,4)));

   ret0__:
   return (res__);
}

int STR171_get_ci_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)get_ci_(PATT_(self__,4));

   ret0__:
   return (res__);
}

char STR171_get_b_(ptr self__)
{
   char res__ = S_char_VOID_;
   int    ci__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   ci__ = (int)STR171_get_ci_(self__);
   if ((ci__ == -1)) {
      STR171_error_val_ = (int)2;
      goto ret0__;
   }
   else {
   }
   c__ = (char)INT15_to_c_(ci__);
   if ((c__ == 'T')) {
      res__ = (char)1;
   }
   else {
      if ((c__ == 'F')) {
      }
      else {
         STR171_error_val_ = (int)1;
      }
   }

   ret0__:
   return (res__);
}

int STR171_get_i_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)fscanfi_(PATT_(self__,4));

   ret0__:
   return (res__);
}

float STR171_get_r_(ptr self__)
{
   float res__ = S_float_VOID_;

   res__ = (float)fscanfd_(PATT_(self__,4));

   ret0__:
   return (res__);
}

double STR171_get_d_(ptr self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)fscanfd_(PATT_(self__,4));

   ret0__:
   return (res__);
}

ptr STR171_get_s_(ptr self__)
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)STR20_create_(0);
   ci__ = (int)get_ci_(PATT_(self__,4));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4520_;
      }
      else {
      }
      c__ = (char)INT15_to_c_(ci__);
      if (((ind__ + 1) == IATT_(res__,4))) {
         res__ = (ptr)STR20_c_(res__,c__);
      }
      else {
         CATT_(res__, 8 + ((ind__))) = (char)c__;
      }
      ind__ = (int)(ind__ + 1);
      if ((c__ == '\n')) {
         goto ret0__;
      }
      else {
      }
      ci__ = (int)get_ci_(PATT_(self__,4));
   }
goto_tag_4520_: ;

   ret0__:
   return (res__);
}

ptr STR171_get_s_up_to_(ptr self__, char c0__)
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)STR20_create_(0);
   ci__ = (int)get_ci_(PATT_(self__,4));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4521_;
      }
      else {
      }
      c__ = (char)INT15_to_c_(ci__);
      if ((c__ == c0__)) {
         goto ret0__;
      }
      else {
      }
      if (((ind__ + 1) == IATT_(res__,4))) {
         res__ = (ptr)STR20_c_(res__,c__);
      }
      else {
         CATT_(res__, 8 + ((ind__))) = (char)c__;
      }
      ind__ = (int)(ind__ + 1);
      ci__ = (int)get_ci_(PATT_(self__,4));
   }
goto_tag_4521_: ;

   ret0__:
   return (res__);
}

void STR171_unget_c_(ptr self__, char ch__)
{

   ungetc(ch__,PATT_(self__,4));

   ret0__:
   return;
}

ptr STR171_b_(ptr self__, char bo__)
{
   ptr res__ = 0;

   if (bo__) {
      fputc('T',PATT_(self__,4));
   }
   else {
      fputc('F',PATT_(self__,4));
   }
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_c_(ptr self__, char ch__)
{
   ptr res__ = 0;

   fputc(ch__,PATT_(self__,4));
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_i_(ptr self__, int in__)
{
   ptr res__ = 0;

   fprintfi_(PATT_(self__,4),in__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_s_(ptr self__, ptr st__)
{
   ptr res__ = 0;

   fprintfs_(PATT_(self__,4),str_ptr_(st__));
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_r_(ptr self__, float re__)
{
   ptr res__ = 0;

   fprintfd_(PATT_(self__,4),re__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_d_(ptr self__, double do__)
{
   ptr res__ = 0;

   fprintfd_(PATT_(self__,4),do__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR171_nl_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR171_c_(self__,'\n');

   ret0__:
   return (res__);
}

void STR171_open_for_read_(ptr self__, ptr nm__)
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,4) = (ptr)fopenr_(str_ptr_(nm__));
   if ((PATT_(self__,4) == 0)) {
      STR171_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

void STR171_open_for_write_(ptr self__, ptr nm__)
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,4) = (ptr)fopenw_(str_ptr_(nm__));
   if ((PATT_(self__,4) == 0)) {
      STR171_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

void STR171_open_for_append_(ptr self__, ptr nm__)
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,4) = (ptr)fopena_(str_ptr_(nm__));
   if ((PATT_(self__,4) == 0)) {
      STR171_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

void STR171_close_(ptr self__)
{

   if ((PATT_(self__,4) != 0)) {
      fclose(PATT_(self__,4));
   }
   else {
   }

   ret0__:
   return;
}

ptr STR171_get_s_of_len_(ptr self__, int n__)
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)new1_(20,(n__ + 1),1);
   ci__ = (int)get_ci_(PATT_(self__,4));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4522_;
      }
      else {
      }
      if ((ind__ >= n__)) {
         goto ret0__;
      }
      else {
      }
      c__ = (char)INT15_to_c_(ci__);
      CATT_(res__, 8 + ((ind__))) = (char)c__;
      ind__ = (int)(ind__ + 1);
   }
goto_tag_4522_: ;

   ret0__:
   return (res__);
}

void STR171_flush_(ptr self__)
{

   if ((PATT_(self__,4) != 0)) {
      fflush(PATT_(self__,4));
   }
   else {
   }

   ret0__:
   return;
}

void STR171_open_pipe_for_read_(ptr self__, ptr command__)
{

   PATT_(self__,4) = (ptr)popenr_(str_ptr_(command__));
   if ((PATT_(self__,4) == 0)) {
      STR171_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

void STR171_open_pipe_for_write_(ptr self__, ptr command__)
{

   PATT_(self__,4) = (ptr)popenw_(str_ptr_(command__));
   if ((PATT_(self__,4) == 0)) {
      STR171_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

void STR171_close_pipe_(ptr self__)
{

   if ((PATT_(self__,4) != 0)) {
      pclose(PATT_(self__,4));
   }
   else {
   }

   ret0__:
   return;
}

void STR171_seek_relative_(ptr self__, int n__)
{

   if ((PATT_(self__,4) != 0)) {
      fseek(PATT_(self__,4),n__,1);
   }
   else {
   }

   ret0__:
   return;
}

void STR171_seek_from_front_(ptr self__, int n__)
{

   if ((PATT_(self__,4) != 0)) {
      fseek(PATT_(self__,4),n__,0);
   }
   else {
   }

   ret0__:
   return;
}

void STR171_seek_from_end_(ptr self__, int n__)
{

   if ((PATT_(self__,4) != 0)) {
      fseek(PATT_(self__,4),n__,2);
   }
   else {
   }

   ret0__:
   return;
}

ptr STR171_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


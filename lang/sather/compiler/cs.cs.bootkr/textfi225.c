/* textfi225.c : Sather class: TEXTFILE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern error_msg();
extern ptr str_ptr_();
extern int scanf_val_();
extern ptr stdin_();
extern ptr stdout_();
extern ptr stderr_();
extern char check_eof_();
extern int get_ci_();
extern int fscanfi_();
extern double fscanfd_();
extern error_exit();
extern ungetc();
extern fputc();
extern fprintfi_();
extern fprintfs_();
extern fprintfd_();
extern ptr fopenr_();
extern ptr fopenw_();
extern ptr fopena_();
extern fclose();
extern fflush();
extern ptr popenr_();
extern ptr popenw_();
extern pclose();
extern fseek();

extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern char INT15_to_c_();
#include "macros_.h"



/*constant*/ int TEX225_eof_ = -1;
/*shared*/ int TEX225_error_val_;
/*constant*/ int TEX225_read_error_ = 1;
/*constant*/ int TEX225_eof_error_ = 2;
/*constant*/ int TEX225_open_error_ = 3;
int TEX225_error_();
ptr TEX225_create_();
ptr TEX225_in_();
ptr TEX225_out_();
ptr TEX225_err_();
char TEX225_check_eof_();
char TEX225_get_c_();
int TEX225_get_ci_();
char TEX225_get_b_();
int TEX225_get_i_();
float TEX225_get_r_();
double TEX225_get_d_();
ptr TEX225_get_s_();
ptr TEX225_get_s_up_to_();
TEX225_unget_c_();
ptr TEX225_b_();
ptr TEX225_c_();
ptr TEX225_i_();
ptr TEX225_s_();
ptr TEX225_r_();
ptr TEX225_d_();
ptr TEX225_nl_();
TEX225_open_for_read_();
TEX225_open_for_write_();
TEX225_open_for_append_();
TEX225_close_();
ptr TEX225_get_s_of_len_();
TEX225_flush_();
TEX225_open_pipe_for_read_();
TEX225_open_pipe_for_write_();
TEX225_close_pipe_();
TEX225_seek_relative_();
TEX225_seek_from_front_();
TEX225_seek_from_end_();
TEX225_error_msg_();
TEX225_error_exit_();
char TEX225_whitespace_();
char TEX225_read_next_();
int TEX225_get_token_();
ptr TEX225_initialize_();
extern int attr_ent_TEX225[];

int TEX225_error_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;
   int    sv__ = S_int_VOID_;

   res__ = (int)TEX225_error_val_;
   TEX225_error_val_ = (int)0;
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

ptr TEX225_create_(self__,nm__)
ptr self__;
ptr nm__;
{
   ptr res__ = 0;
   SATHER_STR_(20,25,ls2248_,"ERROR: Failure to open \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   ptr gl4815_;

   gl4815_ = res__;
   res__ = (ptr)new_(225,0);
   TEX225_open_for_read_(res__,nm__);
   if ((TEX225_error_(res__) != 0)) {
      IATT_(res__,12) = (int)1;
      TEX225_error_exit_(self__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2248_)),nm__),(ptr)(&ls632_)));
   }
   else {
   }
   IATT_(res__,12) = (int)1;

   ret0__:
   return (res__);
}

ptr TEX225_in_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,3,ls1310_,"in");
   ptr gl4816_;

   gl4816_ = res__;
   res__ = (ptr)new_(225,0);
   PATT_(res__,16) = (ptr)stdin_();
   PATT_(res__,8) = (ptr)(ptr)(&ls1310_);

   ret0__:
   return (res__);
}

ptr TEX225_out_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls1309_,"out");
   ptr gl4817_;

   gl4817_ = res__;
   res__ = (ptr)new_(225,0);
   PATT_(res__,16) = (ptr)stdout_();
   PATT_(res__,8) = (ptr)(ptr)(&ls1309_);

   ret0__:
   return (res__);
}

ptr TEX225_err_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls2561_,"err");
   ptr gl4818_;

   gl4818_ = res__;
   res__ = (ptr)new_(225,0);
   PATT_(res__,16) = (ptr)stderr_();
   PATT_(res__,8) = (ptr)(ptr)(&ls2561_);

   ret0__:
   return (res__);
}

char TEX225_check_eof_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)check_eof_(PATT_(self__,16));

   ret0__:
   return (res__);
}

char TEX225_get_c_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)INT15_to_c_(get_ci_(PATT_(self__,16)));

   ret0__:
   return (res__);
}

int TEX225_get_ci_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)get_ci_(PATT_(self__,16));

   ret0__:
   return (res__);
}

char TEX225_get_b_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   int    ci__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   ci__ = (int)TEX225_get_ci_(self__);
   if ((ci__ == -1)) {
      TEX225_error_val_ = (int)2;
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
         TEX225_error_val_ = (int)1;
      }
   }

   ret0__:
   return (res__);
}

int TEX225_get_i_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)fscanfi_(PATT_(self__,16));

   ret0__:
   return (res__);
}

float TEX225_get_r_(self__)
ptr self__;
{
   float res__ = S_float_VOID_;

   res__ = (float)fscanfd_(PATT_(self__,16));

   ret0__:
   return (res__);
}

double TEX225_get_d_(self__)
ptr self__;
{
   double res__ = S_double_VOID_;

   res__ = (double)fscanfd_(PATT_(self__,16));

   ret0__:
   return (res__);
}

ptr TEX225_get_s_(self__)
ptr self__;
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)STR20_create_(0);
   ci__ = (int)get_ci_(PATT_(self__,16));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4819_;
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
      ci__ = (int)get_ci_(PATT_(self__,16));
   }
goto_tag_4819_: ;

   ret0__:
   return (res__);
}

ptr TEX225_get_s_up_to_(self__,c0__)
ptr self__;
char c0__;
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)STR20_create_(0);
   ci__ = (int)get_ci_(PATT_(self__,16));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4820_;
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
      ci__ = (int)get_ci_(PATT_(self__,16));
   }
goto_tag_4820_: ;

   ret0__:
   return (res__);
}

TEX225_unget_c_(self__,ch__)
ptr self__;
char ch__;
{

   ungetc(ch__,PATT_(self__,16));

   ret0__:
   return;
}

ptr TEX225_b_(self__,bo__)
ptr self__;
char bo__;
{
   ptr res__ = 0;

   if (bo__) {
      fputc('T',PATT_(self__,16));
   }
   else {
      fputc('F',PATT_(self__,16));
   }
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_c_(self__,ch__)
ptr self__;
char ch__;
{
   ptr res__ = 0;

   fputc(ch__,PATT_(self__,16));
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_i_(self__,in__)
ptr self__;
int in__;
{
   ptr res__ = 0;

   fprintfi_(PATT_(self__,16),in__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_s_(self__,st__)
ptr self__;
ptr st__;
{
   ptr res__ = 0;

   fprintfs_(PATT_(self__,16),str_ptr_(st__));
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_r_(self__,re__)
ptr self__;
float re__;
{
   ptr res__ = 0;

   fprintfd_(PATT_(self__,16),re__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_d_(self__,do__)
ptr self__;
double do__;
{
   ptr res__ = 0;

   fprintfd_(PATT_(self__,16),do__);
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr TEX225_nl_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)TEX225_c_(self__,'\n');

   ret0__:
   return (res__);
}

TEX225_open_for_read_(self__,nm__)
ptr self__;
ptr nm__;
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,16) = (ptr)fopenr_(str_ptr_(nm__));
   if ((PATT_(self__,16) == 0)) {
      TEX225_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

TEX225_open_for_write_(self__,nm__)
ptr self__;
ptr nm__;
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,16) = (ptr)fopenw_(str_ptr_(nm__));
   if ((PATT_(self__,16) == 0)) {
      TEX225_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

TEX225_open_for_append_(self__,nm__)
ptr self__;
ptr nm__;
{

   PATT_(self__,8) = (ptr)nm__;
   PATT_(self__,16) = (ptr)fopena_(str_ptr_(nm__));
   if ((PATT_(self__,16) == 0)) {
      TEX225_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

TEX225_close_(self__)
ptr self__;
{

   if ((PATT_(self__,16) != 0)) {
      fclose(PATT_(self__,16));
   }
   else {
   }

   ret0__:
   return;
}

ptr TEX225_get_s_of_len_(self__,n__)
ptr self__;
int n__;
{
   ptr res__ = 0;
   int    ci__ = S_int_VOID_;
   int    ind__ = S_int_VOID_;
   char    c__ = S_char_VOID_;

   res__ = (ptr)new1_(20,(n__ + 1),1);
   ci__ = (int)get_ci_(PATT_(self__,16));
   ind__ = S_int_VOID_;
   while (1) {
      if ((ci__ == -1)) {
         goto goto_tag_4821_;
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
goto_tag_4821_: ;

   ret0__:
   return (res__);
}

TEX225_flush_(self__)
ptr self__;
{

   if ((PATT_(self__,16) != 0)) {
      fflush(PATT_(self__,16));
   }
   else {
   }

   ret0__:
   return;
}

TEX225_open_pipe_for_read_(self__,command__)
ptr self__;
ptr command__;
{

   PATT_(self__,16) = (ptr)popenr_(str_ptr_(command__));
   if ((PATT_(self__,16) == 0)) {
      TEX225_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

TEX225_open_pipe_for_write_(self__,command__)
ptr self__;
ptr command__;
{

   PATT_(self__,16) = (ptr)popenw_(str_ptr_(command__));
   if ((PATT_(self__,16) == 0)) {
      TEX225_error_val_ = (int)3;
   }
   else {
   }

   ret0__:
   return;
}

TEX225_close_pipe_(self__)
ptr self__;
{

   if ((PATT_(self__,16) != 0)) {
      pclose(PATT_(self__,16));
   }
   else {
   }

   ret0__:
   return;
}

TEX225_seek_relative_(self__,n__)
ptr self__;
int n__;
{

   if ((PATT_(self__,16) != 0)) {
      fseek(PATT_(self__,16),n__,1);
   }
   else {
   }

   ret0__:
   return;
}

TEX225_seek_from_front_(self__,n__)
ptr self__;
int n__;
{

   if ((PATT_(self__,16) != 0)) {
      fseek(PATT_(self__,16),n__,0);
   }
   else {
   }

   ret0__:
   return;
}

TEX225_seek_from_end_(self__,n__)
ptr self__;
int n__;
{

   if ((PATT_(self__,16) != 0)) {
      fseek(PATT_(self__,16),n__,2);
   }
   else {
   }

   ret0__:
   return;
}

TEX225_error_msg_(self__,s__)
ptr self__;
ptr s__;
{

   error_msg(str_ptr_(s__));

   ret0__:
   return;
}

TEX225_error_exit_(self__,s__)
ptr self__;
ptr s__;
{

   error_exit(str_ptr_(s__));

   ret0__:
   return;
}

char TEX225_whitespace_(self__,ch__)
ptr self__;
char ch__;
{
   char res__ = S_char_VOID_;

   switch (ch__) {
      case (' ') :
         res__ = (char)1;
         break;
      case ('\n') :
         res__ = (char)1;
         break;
      case ('\b') :
         res__ = (char)1;
         break;
      case ('\t') :
         res__ = (char)1;
         break;
      default:
         res__ = (char)0;
         ;
   }

   ret0__:
   return (res__);
}

char TEX225_read_next_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   CATT_(self__,4) = (char)TEX225_get_c_(self__);
   if ((CATT_(self__,4) == '\n')) {
      IATT_(self__,12) = (int)(IATT_(self__,12) + 1);
   }
   else {
   }
   res__ = (char)CATT_(self__,4);

   ret0__:
   return (res__);
}

int TEX225_get_token_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr TEX225_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


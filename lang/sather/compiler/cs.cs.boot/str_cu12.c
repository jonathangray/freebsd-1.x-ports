/* str_cu12.c : Sather class: STR_CURSOR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern double sscanfd(ptr p__, int l__);
extern ptr str_ptr_(ptr s__);
extern int sscanfi(ptr p__, int l__);

extern ptr STR20_create_(ptr self__);
extern ptr STR20_c_(ptr self__, char ch__);
extern int STR20_length_(ptr self__);
extern char CHA14_is_digit_(char self__);
extern char CHA14_is_space_(char self__);
#include "macros_.h"



ptr STR12_create_(ptr self__, ptr st__);
ptr STR12_get_s_(ptr self__);
int STR12_b_size_(ptr self__);
char STR12_get_b_(ptr self__);
char STR12_get_c_(ptr self__);
int STR12_digits_size_(ptr self__, int i__);
int STR12_i_size_(ptr self__);
int STR12_get_i_(ptr self__);
int STR12_r_size_(ptr self__);
float STR12_get_r_(ptr self__);
double STR12_get_d_(ptr self__);
char STR12_item_(ptr self__);
char STR12_first_(ptr self__);
char STR12_next_(ptr self__);
int STR12_space_size_(ptr self__, int i__);
ptr STR12_skip_space_(ptr self__);
char STR12_is_done_(ptr self__);
ptr STR12_reassign_(ptr self__, ptr str__);
char STR12_c_equals_(ptr self__, char c__);
char STR12_c_accept_(ptr self__, char c__);
ptr STR12_get_s_cut_(ptr self__, ptr cutset__);
ptr STR12_get_word_(ptr self__);
int STR12_update_i_(ptr self__, int x__);
float STR12_update_r_(ptr self__, float x__);
extern int attr_ent_STR12[];

ptr STR12_create_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   ptr gl508_;

   gl508_ = res__;
   res__ = (ptr)new_(12,0);
   PATT_(res__,4) = (ptr)st__;

   ret0__:
   return (res__);
}

ptr STR12_get_s_(ptr self__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   l__ = S_int_VOID_;
   while (1) {
      if ((CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == '\00')) {
         goto goto_tag_509_;
      }
      else {
      }
      if ((IATT_(res__,4) == (l__ + 1))) {
         res__ = (ptr)STR20_c_(res__,CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))));
      }
      else {
         CATT_(res__, 8 + ((l__))) = (char)CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8))));
      }
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      l__ = (int)(l__ + 1);
      if ((CATT_(PATT_(self__,4), 8 + (((IATT_(self__,8) - 1)))) == '\n')) {
         goto goto_tag_509_;
      }
      else {
      }
   }
goto_tag_509_: ;

   ret0__:
   return (res__);
}

int STR12_b_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (STR12_is_done_(self__)) {
      goto ret0__;
   }
   else {
   }
   if (((CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == 'T') | (CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == 'F'))) {
      res__ = (int)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR12_get_b_(ptr self__)
{
   char res__ = S_char_VOID_;

   if ((STR12_b_size_(self__) == 0)) {
      IATT_(self__,12) = (int)1;
      goto ret0__;
   }
   else {
   }
   if ((CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == 'T')) {
      res__ = (char)1;
   }
   else {
   }
   IATT_(self__,8) = (int)(IATT_(self__,8) + 1);

   ret0__:
   return (res__);
}

char STR12_get_c_(ptr self__)
{
   char res__ = S_char_VOID_;

   if (STR12_is_done_(self__)) {
      IATT_(self__,12) = (int)1;
      goto ret0__;
   }
   else {
   }
   res__ = (char)CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8))));
   IATT_(self__,8) = (int)(IATT_(self__,8) + 1);

   ret0__:
   return (res__);
}

int STR12_digits_size_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;

   while (1) {
      if ((! CHA14_is_digit_(CATT_(PATT_(self__,4), 8 + ((i__)))))) {
         goto goto_tag_510_;
      }
      else {
      }
      res__ = (int)(res__ + 1);
      i__ = (int)(i__ + 1);
   }
goto_tag_510_: ;

   ret0__:
   return (res__);
}

int STR12_i_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (((CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == '-') | (CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == '+'))) {
      res__ = (int)STR12_digits_size_(self__,(IATT_(self__,8) + 1));
      if ((res__ == 0)) {
         goto ret0__;
      }
      else {
         res__ = (int)(res__ + 1);
         goto ret0__;
      }
   }
   else {
      res__ = (int)STR12_digits_size_(self__,IATT_(self__,8));
   }

   ret0__:
   return (res__);
}

int STR12_get_i_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    is1__ = S_int_VOID_;

   (void)STR12_skip_space_(self__);
   is1__ = (int)STR12_i_size_(self__);
   if (((is1__ == 0) | (is1__ > 12))) {
      IATT_(self__,12) = (int)1;
      goto ret0__;
   }
   else {
   }
   res__ = (int)sscanfi(str_ptr_(PATT_(self__,4)),IATT_(self__,8));
   IATT_(self__,8) = (int)(IATT_(self__,8) + is1__);

   ret0__:
   return (res__);
}

int STR12_r_size_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    t__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   char    has_ip__ = S_char_VOID_;
   char    has_dp__ = S_char_VOID_;
   char    has_fp__ = S_char_VOID_;
   char    has_e__ = S_char_VOID_;
   char    has_exp__ = S_char_VOID_;

   t__ = S_int_VOID_;
   i__ = (int)IATT_(self__,8);
   has_ip__ = S_char_VOID_;
   has_dp__ = S_char_VOID_;
   has_fp__ = S_char_VOID_;
   has_e__ = S_char_VOID_;
   has_exp__ = S_char_VOID_;
   if (((CATT_(PATT_(self__,4), 8 + ((i__))) == '-') | (CATT_(PATT_(self__,4), 8 + ((i__))) == '+'))) {
      i__ = (int)(i__ + 1);
   }
   else {
   }
   t__ = (int)STR12_digits_size_(self__,i__);
   if ((t__ > 0)) {
      has_ip__ = (char)1;
      i__ = (int)(i__ + t__);
   }
   else {
   }
   if ((CATT_(PATT_(self__,4), 8 + ((i__))) == '.')) {
      has_dp__ = (char)1;
      i__ = (int)(i__ + 1);
   }
   else {
   }
   t__ = (int)STR12_digits_size_(self__,i__);
   if ((t__ > 0)) {
      has_fp__ = (char)1;
      i__ = (int)(i__ + t__);
   }
   else {
   }
   if (((CATT_(PATT_(self__,4), 8 + ((i__))) == 'e') | (CATT_(PATT_(self__,4), 8 + ((i__))) == 'E'))) {
      has_e__ = (char)1;
      i__ = (int)(i__ + 1);
   }
   else {
   }
   if (((CATT_(PATT_(self__,4), 8 + ((i__))) == '-') | (CATT_(PATT_(self__,4), 8 + ((i__))) == '+'))) {
      i__ = (int)(i__ + 1);
   }
   else {
   }
   t__ = (int)STR12_digits_size_(self__,i__);
   if ((t__ > 0)) {
      has_exp__ = (char)1;
      i__ = (int)(i__ + t__);
   }
   else {
   }
   if (((! has_ip__) & (! has_fp__))) {
      goto ret0__;
   }
   else {
   }
   if ((has_e__ & (! has_exp__))) {
      goto ret0__;
   }
   else {
   }
   res__ = (int)(i__ - IATT_(self__,8));

   ret0__:
   return (res__);
}

float STR12_get_r_(ptr self__)
{
   float res__ = S_float_VOID_;
   int    rs__ = S_int_VOID_;

   (void)STR12_skip_space_(self__);
   rs__ = (int)STR12_r_size_(self__);
   if (((rs__ == 0) | (rs__ > 22))) {
      IATT_(self__,12) = (int)1;
      goto ret0__;
   }
   else {
   }
   res__ = (float)sscanfd(str_ptr_(PATT_(self__,4)),IATT_(self__,8));
   IATT_(self__,8) = (int)(IATT_(self__,8) + rs__);

   ret0__:
   return (res__);
}

double STR12_get_d_(ptr self__)
{
   double res__ = S_double_VOID_;
   int    rs__ = S_int_VOID_;

   (void)STR12_skip_space_(self__);
   rs__ = (int)STR12_r_size_(self__);
   if (((rs__ == 0) | (rs__ > 22))) {
      IATT_(self__,12) = (int)1;
      goto ret0__;
   }
   else {
   }
   res__ = (double)sscanfd(str_ptr_(PATT_(self__,4)),IATT_(self__,8));
   IATT_(self__,8) = (int)(IATT_(self__,8) + rs__);

   ret0__:
   return (res__);
}

char STR12_item_(ptr self__)
{
   char res__ = S_char_VOID_;

   if ((! STR12_is_done_(self__))) {
      res__ = (char)CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8))));
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR12_first_(ptr self__)
{
   char res__ = S_char_VOID_;

   IATT_(self__,8) = (int)0;
   res__ = (char)STR12_item_(self__);

   ret0__:
   return (res__);
}

char STR12_next_(ptr self__)
{
   char res__ = S_char_VOID_;

   (void)STR12_get_c_(self__);
   res__ = (char)STR12_item_(self__);

   ret0__:
   return (res__);
}

int STR12_space_size_(ptr self__, int i__)
{
   int res__ = S_int_VOID_;
   int    j__ = S_int_VOID_;

   j__ = (int)i__;
   while (1) {
      if ((! CHA14_is_space_(CATT_(PATT_(self__,4), 8 + ((j__)))))) {
         goto goto_tag_511_;
      }
      else {
      }
      j__ = (int)(j__ + 1);
   }
goto_tag_511_: ;
   res__ = (int)(j__ - i__);

   ret0__:
   return (res__);
}

ptr STR12_skip_space_(ptr self__)
{
   ptr res__ = 0;

   IATT_(self__,8) = (int)(IATT_(self__,8) + STR12_space_size_(self__,IATT_(self__,8)));
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char STR12_is_done_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == '\00');

   ret0__:
   return (res__);
}

ptr STR12_reassign_(ptr self__, ptr str__)
{
   ptr res__ = 0;

   PATT_(self__,4) = (ptr)str__;
   IATT_(self__,8) = (int)0;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char STR12_c_equals_(ptr self__, char c__)
{
   char res__ = S_char_VOID_;

   if (STR12_is_done_(self__)) {
      IATT_(self__,12) = (int)1;
      res__ = (char)0;
   }
   else {
      res__ = (char)(CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8)))) == c__);
   }

   ret0__:
   return (res__);
}

char STR12_c_accept_(ptr self__, char c__)
{
   char res__ = S_char_VOID_;

   res__ = (char)STR12_c_equals_(self__,c__);
   if (res__) {
      (void)STR12_get_c_(self__);
      (void)STR12_skip_space_(self__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR12_get_s_cut_(ptr self__, ptr cutset__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;
   int    cutlen__ = S_int_VOID_;
   char    c__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   res__ = (ptr)STR20_create_(0);
   l__ = S_int_VOID_;
   cutlen__ = (int)STR20_length_(cutset__);
   c__ = S_char_VOID_;
   i__ = S_int_VOID_;
   while (1) {
      c__ = (char)CATT_(PATT_(self__,4), 8 + ((IATT_(self__,8))));
      if ((c__ == '\00')) {
         goto goto_tag_512_;
      }
      else {
      }
      if ((IATT_(res__,4) == (l__ + 1))) {
         res__ = (ptr)STR20_c_(res__,c__);
      }
      else {
         CATT_(res__, 8 + ((l__))) = (char)c__;
      }
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      l__ = (int)(l__ + 1);
      i__ = (int)cutlen__;
      while (1) {
         if ((i__ == 0)) {
            goto goto_tag_513_;
         }
         else {
         }
         i__ = (int)(i__ - 1);
         if ((c__ == CATT_(cutset__, 8 + ((i__))))) {
            goto ret0__;
         }
         else {
         }
      }
   goto_tag_513_: ;
   }
goto_tag_512_: ;

   ret0__:
   return (res__);
}

ptr STR12_get_word_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls3495_," \t\n");
   char    c__ = S_char_VOID_;

   res__ = (ptr)STR12_get_s_cut_(self__,(ptr)(&ls3495_));
   (void)STR12_skip_space_(self__);
   c__ = (char)CATT_(res__, 8 + (((STR20_length_(res__) - 1))));
   if ((((c__ == ' ') | (c__ == '\t')) | (c__ == '\n'))) {
      CATT_(res__, 8 + (((STR20_length_(res__) - 1)))) = (char)'\00';
   }
   else {
   }

   ret0__:
   return (res__);
}

int STR12_update_i_(ptr self__, int x__)
{
   int res__ = S_int_VOID_;

   IATT_(self__,12) = (int)0;
   res__ = (int)STR12_get_i_(self__);
   if ((IATT_(self__,12) != 0)) {
      res__ = (int)x__;
      IATT_(self__,12) = (int)0;
   }
   else {
   }
   (void)STR12_skip_space_(self__);

   ret0__:
   return (res__);
}

float STR12_update_r_(ptr self__, float x__)
{
   float res__ = S_float_VOID_;

   IATT_(self__,12) = (int)0;
   res__ = (float)STR12_get_r_(self__);
   if ((IATT_(self__,12) != 0)) {
      res__ = (float)x__;
      IATT_(self__,12) = (int)0;
   }
   else {
   }
   (void)STR12_skip_space_(self__);

   ret0__:
   return (res__);
}


/* str___20.c : Sather class: STR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern int sstrlen(ptr s__);
extern ptr sstrcat(ptr s1__, ptr s2__);
extern void sprintfi(ptr s__, int i__, int l__);
extern int sstrcmp(ptr s1__, ptr s2__);
extern void sprintfd(ptr s__, double d__, int p__, int l__);
extern ptr str_ptr_(ptr s__);
extern double sscanfd(ptr p__, int l__);
extern int sscanfi(ptr p__, int l__);
extern ptr makestr_(ptr st__);
#define sstrlen(s) strlen(s)
#define sstrcat(s1, s2) strcat(s1, s2)
#define sstrcmp(s1, s2) strcmp(s1, s2)

extern ptr STR12_create_(ptr self__, ptr st__);
extern char CHA14_to_upper_case_(char self__);
extern char CHA14_to_lower_case_(char self__);
extern char CHA14_is_alphabetic_(char self__);
extern char CHA14_is_lower_case_(char self__);
extern int INT15_min_(int self__, int i__);
extern int INT15_rshift_(int self__, int i__);
extern char CHA14_is_space_(char self__);
extern char CHA14_is_punctuation_(char self__);
extern int CHA14_to_i_(char self__);
extern int INT15_bit_and_(int self__, int i__);
extern int INT15_lshift_(int self__, int i__);
extern int INT15_bit_xor_(int self__, int i__);
#include "macros_.h"



void STR20_clear_(ptr self__);
/*shared*/ int STR20_precision_;
ptr STR20_create_(ptr self__);
ptr STR20_create_sized_(ptr self__, int n__);
ptr STR20_b_(ptr self__, char bo__);
ptr STR20_c_(ptr self__, char ch__);
ptr STR20_s_(ptr self__, ptr st__);
ptr STR20_i_(ptr self__, int in__);
ptr STR20_r_(ptr self__, float re__);
ptr STR20_d_(ptr self__, double do__);
ptr STR20_nl_(ptr self__);
char STR20_is_equal_(ptr self__, ptr st__);
char STR20_is_less_than_(ptr self__, ptr st__);
ptr STR20_to_upper_case_(ptr self__);
ptr STR20_to_lower_case_(ptr self__);
ptr STR20_capitalize_(ptr self__);
char STR20_is_upper_case_(ptr self__);
ptr STR20_head_(ptr self__, int n__);
ptr STR20_tail_(ptr self__, int n__);
int STR20_length_(ptr self__);
ptr STR20_substring_(ptr self__, int n__, int m__);
int STR20_index_of_char_(ptr self__, char ch__);
ptr STR20_reverse_(ptr self__);
char STR20_is_empty_(ptr self__);
int STR20_to_i_(ptr self__);
float STR20_to_r_(ptr self__);
double STR20_to_d_(ptr self__);
char STR20_to_b_(ptr self__);
char STR20_to_c_(ptr self__);
ptr STR20_from_c_str_(ptr self__, ptr st__);
ptr STR20_cursor_(ptr self__);
int STR20_hash_(ptr self__);
extern int attr_ent_STR20[];

void STR20_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,4))) {
         goto goto_tag_520_;
      }
      else {
      }
      CATT_(self__, 8 + ((i__))) = (char)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_520_: ;

   ret0__:
   return;
}

ptr STR20_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new1_(20,4,1);

   ret0__:
   return (res__);
}

ptr STR20_create_sized_(ptr self__, int n__)
{
   ptr res__ = 0;

   res__ = (ptr)new1_(20,n__,1);

   ret0__:
   return (res__);
}

ptr STR20_b_(ptr self__, char bo__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   if (((l__ + 2) > IATT_(self__,4))) {
      res__ = (ptr)extend1_(self__,(IATT_(self__,4) * 2),1);
   }
   else {
      res__ = (ptr)self__;
   }
   if (bo__) {
      CATT_(res__, 8 + ((l__))) = (char)'T';
   }
   else {
      CATT_(res__, 8 + ((l__))) = (char)'F';
   }

   ret0__:
   return (res__);
}

ptr STR20_c_(ptr self__, char ch__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   if (((l__ + 2) > IATT_(self__,4))) {
      res__ = (ptr)extend1_(self__,(IATT_(self__,4) * 2),1);
   }
   else {
      res__ = (ptr)self__;
   }
   CATT_(res__, 8 + ((l__))) = (char)ch__;

   ret0__:
   return (res__);
}

ptr STR20_s_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    l1__ = S_int_VOID_;
   int    l2__ = S_int_VOID_;
   int    lt__ = S_int_VOID_;
   int    ns__ = S_int_VOID_;

   l1__ = (int)STR20_length_(self__);
   l2__ = (int)STR20_length_(st__);
   lt__ = (int)((l1__ + l2__) + 1);
   if ((lt__ > IATT_(self__,4))) {
      ns__ = (int)(IATT_(self__,4) * 2);
      while (1) {
         if ((ns__ >= lt__)) {
            goto goto_tag_521_;
         }
         else {
         }
         ns__ = (int)(ns__ * 2);
      }
   goto_tag_521_: ;
      res__ = (ptr)extend1_(self__,ns__,1);
   }
   else {
      res__ = (ptr)self__;
   }
   (void)sstrcat(str_ptr_(res__),str_ptr_(st__));

   ret0__:
   return (res__);
}

ptr STR20_i_(ptr self__, int in__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;
   int    lt__ = S_int_VOID_;
   int    ns__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   lt__ = (int)(l__ + 12);
   if ((lt__ > IATT_(self__,4))) {
      ns__ = (int)(IATT_(self__,4) * 2);
      while (1) {
         if ((ns__ >= lt__)) {
            goto goto_tag_522_;
         }
         else {
         }
         ns__ = (int)(ns__ * 2);
      }
   goto_tag_522_: ;
      res__ = (ptr)extend1_(self__,ns__,1);
   }
   else {
      res__ = (ptr)self__;
   }
   sprintfi(str_ptr_(res__),in__,l__);

   ret0__:
   return (res__);
}

ptr STR20_r_(ptr self__, float re__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;
   int    lt__ = S_int_VOID_;
   int    ns__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   lt__ = (int)(l__ + 12);
   if ((lt__ > IATT_(self__,4))) {
      ns__ = (int)(IATT_(self__,4) * 2);
      while (1) {
         if ((ns__ >= lt__)) {
            goto goto_tag_523_;
         }
         else {
         }
         ns__ = (int)(ns__ * 2);
      }
   goto_tag_523_: ;
      res__ = (ptr)extend1_(self__,ns__,1);
   }
   else {
      res__ = (ptr)self__;
   }
   sprintfd(str_ptr_(res__),re__,STR20_precision_,l__);

   ret0__:
   return (res__);
}

ptr STR20_d_(ptr self__, double do__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;
   int    lt__ = S_int_VOID_;
   int    ns__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   lt__ = (int)(l__ + 12);
   if ((lt__ > IATT_(self__,4))) {
      ns__ = (int)(IATT_(self__,4) * 2);
      while (1) {
         if ((ns__ >= lt__)) {
            goto goto_tag_524_;
         }
         else {
         }
         ns__ = (int)(ns__ * 2);
      }
   goto_tag_524_: ;
      res__ = (ptr)extend1_(self__,ns__,1);
   }
   else {
      res__ = (ptr)self__;
   }
   sprintfd(str_ptr_(res__),do__,STR20_precision_,l__);

   ret0__:
   return (res__);
}

ptr STR20_nl_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_c_(self__,'\n');

   ret0__:
   return (res__);
}

char STR20_is_equal_(ptr self__, ptr st__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(0 == sstrcmp(str_ptr_(self__),str_ptr_(st__)));

   ret0__:
   return (res__);
}

char STR20_is_less_than_(ptr self__, ptr st__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(0 > sstrcmp(str_ptr_(self__),str_ptr_(st__)));

   ret0__:
   return (res__);
}

ptr STR20_to_upper_case_(ptr self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    l__ = S_int_VOID_;

   i__ = (int)0;
   l__ = (int)STR20_length_(self__);
   while (1) {
      if ((i__ == l__)) {
         goto goto_tag_525_;
      }
      else {
      }
      CATT_(self__, 8 + ((i__))) = (char)CHA14_to_upper_case_(CATT_(self__, 8 + ((i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_525_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR20_to_lower_case_(ptr self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    l__ = S_int_VOID_;

   i__ = (int)0;
   l__ = (int)STR20_length_(self__);
   while (1) {
      if ((i__ == l__)) {
         goto goto_tag_526_;
      }
      else {
      }
      CATT_(self__, 8 + ((i__))) = (char)CHA14_to_lower_case_(CATT_(self__, 8 + ((i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_526_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr STR20_capitalize_(ptr self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    l__ = S_int_VOID_;

   i__ = (int)0;
   l__ = (int)STR20_length_(self__);
   while (1) {
      if ((i__ == l__)) {
         goto goto_tag_527_;
      }
      else {
      }
      if ((i__ == 0)) {
         CATT_(self__, 8 + ((i__))) = (char)CHA14_to_upper_case_(CATT_(self__, 8 + ((i__))));
      }
      else {
         if ((CHA14_is_space_(CATT_(self__, 8 + (((i__ - 1))))) | CHA14_is_punctuation_(CATT_(self__, 8 + (((i__ - 1))))))) {
            CATT_(self__, 8 + ((i__))) = (char)CHA14_to_upper_case_(CATT_(self__, 8 + ((i__))));
         }
         else {
         }
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_527_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char STR20_is_upper_case_(ptr self__)
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;
   int    l__ = S_int_VOID_;

   i__ = (int)0;
   l__ = (int)STR20_length_(self__);
   while (1) {
      if ((i__ == l__)) {
         goto goto_tag_528_;
      }
      else {
      }
      if (CHA14_is_alphabetic_(CATT_(self__, 8 + ((i__))))) {
         if (CHA14_is_lower_case_(CATT_(self__, 8 + ((i__))))) {
            goto ret0__;
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_528_: ;
   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr STR20_head_(ptr self__, int n__)
{
   ptr res__ = 0;
   ptr gl529_;
   int    nc__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   nc__ = (int)INT15_min_(STR20_length_(self__),n__);
   gl529_ = res__;
   res__ = (ptr)new1_(20,(nc__ + 1),1);
   i__ = (int)0;
   while (1) {
      if ((i__ == nc__)) {
         goto goto_tag_530_;
      }
      else {
      }
      CATT_(res__, 8 + ((i__))) = (char)CATT_(self__, 8 + ((i__)));
      i__ = (int)(i__ + 1);
   }
goto_tag_530_: ;

   ret0__:
   return (res__);
}

ptr STR20_tail_(ptr self__, int n__)
{
   ptr res__ = 0;
   ptr gl531_;
   int    l__ = S_int_VOID_;
   int    nc__ = S_int_VOID_;
   int    st__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   nc__ = (int)INT15_min_(l__,n__);
   st__ = (int)(l__ - nc__);
   gl531_ = res__;
   res__ = (ptr)new1_(20,(nc__ + 1),1);
   i__ = (int)0;
   while (1) {
      if ((i__ == nc__)) {
         goto goto_tag_532_;
      }
      else {
      }
      CATT_(res__, 8 + ((i__))) = (char)CATT_(self__, 8 + (((st__ + i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_532_: ;

   ret0__:
   return (res__);
}

int STR20_length_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    l__ = S_int_VOID_;
   int    u__ = S_int_VOID_;
   int    m__ = S_int_VOID_;

   if ((IATT_(self__,4) < 20)) {
      res__ = (int)sstrlen(str_ptr_(self__));
   }
   else {
      if ((CATT_(self__, 8 + ((0))) == '\00')) {
         goto ret0__;
      }
      else {
         l__ = S_int_VOID_;
         u__ = (int)(IATT_(self__,4) - 1);
         while (1) {
            if ((u__ == (l__ + 1))) {
               goto goto_tag_533_;
            }
            else {
            }
            m__ = (int)(l__ + INT15_rshift_((u__ - l__),1));
            if ((CATT_(self__, 8 + ((m__))) == '\00')) {
               u__ = (int)m__;
            }
            else {
               l__ = (int)m__;
            }
         }
      goto_tag_533_: ;
         res__ = (int)u__;
      }
   }

   ret0__:
   return (res__);
}

ptr STR20_substring_(ptr self__, int n__, int m__)
{
   ptr res__ = 0;
   ptr gl534_;
   int    l__ = S_int_VOID_;
   int    nc__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   if ((l__ <= n__)) {
      res__ = (ptr)STR20_create_(self__);
      goto ret0__;
   }
   else {
   }
   nc__ = (int)(INT15_min_(l__,(m__ + 1)) - n__);
   gl534_ = res__;
   res__ = (ptr)new1_(20,(nc__ + 1),1);
   i__ = (int)0;
   while (1) {
      if ((i__ == nc__)) {
         goto goto_tag_535_;
      }
      else {
      }
      CATT_(res__, 8 + ((i__))) = (char)CATT_(self__, 8 + (((n__ + i__))));
      i__ = (int)(i__ + 1);
   }
goto_tag_535_: ;

   ret0__:
   return (res__);
}

int STR20_index_of_char_(ptr self__, char ch__)
{
   int res__ = S_int_VOID_;
   int    l__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   res__ = (int)(- 1);
   i__ = (int)0;
   while (1) {
      if ((i__ == l__)) {
         goto goto_tag_536_;
      }
      else {
      }
      if ((CATT_(self__, 8 + ((i__))) == ch__)) {
         res__ = (int)i__;
         goto goto_tag_536_;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_536_: ;

   ret0__:
   return (res__);
}

ptr STR20_reverse_(ptr self__)
{
   ptr res__ = 0;
   int    l__ = S_int_VOID_;
   int    sm__ = S_int_VOID_;
   char    t__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   l__ = (int)STR20_length_(self__);
   sm__ = (int)(l__ - 1);
   t__ = S_char_VOID_;
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == INT15_rshift_(l__,1))) {
         goto goto_tag_537_;
      }
      else {
      }
      t__ = (char)CATT_(self__, 8 + ((i__)));
      CATT_(self__, 8 + ((i__))) = (char)CATT_(self__, 8 + (((sm__ - i__))));
      CATT_(self__, 8 + (((sm__ - i__)))) = (char)t__;
      i__ = (int)(i__ + 1);
   }
goto_tag_537_: ;
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char STR20_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(CATT_(self__, 8 + ((0))) == '\00');

   ret0__:
   return (res__);
}

int STR20_to_i_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)sscanfi(str_ptr_(self__),0);

   ret0__:
   return (res__);
}

float STR20_to_r_(ptr self__)
{
   float res__ = S_float_VOID_;

   res__ = (float)sscanfd(str_ptr_(self__),0);

   ret0__:
   return (res__);
}

double STR20_to_d_(ptr self__)
{
   double res__ = S_double_VOID_;

   res__ = (double)sscanfd(str_ptr_(self__),0);

   ret0__:
   return (res__);
}

char STR20_to_b_(ptr self__)
{
   char res__ = S_char_VOID_;

   if ((CATT_(self__, 8 + ((0))) == 'T')) {
      res__ = (char)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR20_to_c_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)CATT_(self__, 8 + ((0)));

   ret0__:
   return (res__);
}

ptr STR20_from_c_str_(ptr self__, ptr st__)
{
   ptr res__ = 0;

   if ((st__ != 0)) {
      res__ = (ptr)makestr_(st__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr STR20_cursor_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR12_create_(0,self__);

   ret0__:
   return (res__);
}

int STR20_hash_(ptr self__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((CATT_(self__, 8 + ((i__))) == '\00')) {
         goto goto_tag_538_;
      }
      else {
      }
      res__ = (int)INT15_bit_xor_(res__,INT15_lshift_(CHA14_to_i_(CATT_(self__, 8 + ((i__)))),INT15_bit_and_(i__,15)));
      i__ = (int)(i__ + 1);
   }
goto_tag_538_: ;

   ret0__:
   return (res__);
}


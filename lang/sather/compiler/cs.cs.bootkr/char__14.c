/* char__14.c : Sather class: CHAR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern int c_to_i();
extern char to_upper();
extern char to_lower();
extern char is_alpha();
extern char is_upper();
extern char is_lower();
extern char is_alnum();
extern char is_space();
extern char is_print();
extern char is_digit();
extern char is_punct();
extern char is_cntrl();

extern ptr STR20_create_();
extern ptr STR20_c_();


char CHA14_is_alphabetic_();
char CHA14_is_upper_case_();
char CHA14_is_lower_case_();
char CHA14_is_digit_();
char CHA14_is_alphanumeric_();
char CHA14_is_space_();
char CHA14_is_print_();
char CHA14_is_punctuation_();
char CHA14_is_control_();
int CHA14_to_i_();
ptr CHA14_to_s_();
char CHA14_to_upper_case_();
char CHA14_to_lower_case_();
extern int attr_ent_CHA14[];

char CHA14_is_alphabetic_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_alpha(self__);

   ret0__:
   return (res__);
}

char CHA14_is_upper_case_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_upper(self__);

   ret0__:
   return (res__);
}

char CHA14_is_lower_case_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_lower(self__);

   ret0__:
   return (res__);
}

char CHA14_is_digit_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_digit(self__);

   ret0__:
   return (res__);
}

char CHA14_is_alphanumeric_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_alnum(self__);

   ret0__:
   return (res__);
}

char CHA14_is_space_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_space(self__);

   ret0__:
   return (res__);
}

char CHA14_is_print_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_print(self__);

   ret0__:
   return (res__);
}

char CHA14_is_punctuation_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_punct(self__);

   ret0__:
   return (res__);
}

char CHA14_is_control_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)is_cntrl(self__);

   ret0__:
   return (res__);
}

int CHA14_to_i_(self__)
char self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)c_to_i(self__);

   ret0__:
   return (res__);
}

ptr CHA14_to_s_(self__)
char self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_(0);
   (void)STR20_c_(res__,self__);

   ret0__:
   return (res__);
}

char CHA14_to_upper_case_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)to_upper(self__);

   ret0__:
   return (res__);
}

char CHA14_to_lower_case_(self__)
char self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)to_lower(self__);

   ret0__:
   return (res__);
}


/* char__14.c : Sather class: CHAR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern int c_to_i(char c__);
extern char to_upper(char c__);
extern char to_lower(char c__);
extern char is_alpha(char c__);
extern char is_upper(char c__);
extern char is_lower(char c__);
extern char is_alnum(char c__);
extern char is_space(char c__);
extern char is_print(char c__);
extern char is_digit(char c__);
extern char is_punct(char c__);
extern char is_cntrl(char c__);

extern ptr STR20_create_(ptr self__);
extern ptr STR20_c_(ptr self__, char ch__);


char CHA14_is_alphabetic_(char self__);
char CHA14_is_upper_case_(char self__);
char CHA14_is_lower_case_(char self__);
char CHA14_is_digit_(char self__);
char CHA14_is_alphanumeric_(char self__);
char CHA14_is_space_(char self__);
char CHA14_is_print_(char self__);
char CHA14_is_punctuation_(char self__);
char CHA14_is_control_(char self__);
int CHA14_to_i_(char self__);
ptr CHA14_to_s_(char self__);
char CHA14_to_upper_case_(char self__);
char CHA14_to_lower_case_(char self__);
extern int attr_ent_CHA14[];

char CHA14_is_alphabetic_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_alpha(self__);

   ret0__:
   return (res__);
}

char CHA14_is_upper_case_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_upper(self__);

   ret0__:
   return (res__);
}

char CHA14_is_lower_case_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_lower(self__);

   ret0__:
   return (res__);
}

char CHA14_is_digit_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_digit(self__);

   ret0__:
   return (res__);
}

char CHA14_is_alphanumeric_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_alnum(self__);

   ret0__:
   return (res__);
}

char CHA14_is_space_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_space(self__);

   ret0__:
   return (res__);
}

char CHA14_is_print_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_print(self__);

   ret0__:
   return (res__);
}

char CHA14_is_punctuation_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_punct(self__);

   ret0__:
   return (res__);
}

char CHA14_is_control_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)is_cntrl(self__);

   ret0__:
   return (res__);
}

int CHA14_to_i_(char self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_to_i(self__);

   ret0__:
   return (res__);
}

ptr CHA14_to_s_(char self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_(0);
   (void)STR20_c_(res__,self__);

   ret0__:
   return (res__);
}

char CHA14_to_upper_case_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)to_upper(self__);

   ret0__:
   return (res__);
}

char CHA14_to_lower_case_(char self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)to_lower(self__);

   ret0__:
   return (res__);
}


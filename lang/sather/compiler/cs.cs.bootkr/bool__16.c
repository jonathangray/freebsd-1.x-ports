/* bool__16.c : Sather class: BOOL, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_b_();
#include "macros_.h"



char BOO16_xor_();
char BOO16_xnor_();
char BOO16_nand_();
char BOO16_nor_();
char BOO16_implies_();
ptr BOO16_to_s_();
extern int attr_ent_BOO16[];

char BOO16_xor_(self__,b__)
char self__;
char b__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((self__ | b__) & (! (self__ & b__)));

   ret0__:
   return (res__);
}

char BOO16_xnor_(self__,b__)
char self__;
char b__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(! BOO16_xor_(self__,b__));

   ret0__:
   return (res__);
}

char BOO16_nand_(self__,b__)
char self__;
char b__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(! (self__ & b__));

   ret0__:
   return (res__);
}

char BOO16_nor_(self__,b__)
char self__;
char b__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(! (self__ | b__));

   ret0__:
   return (res__);
}

char BOO16_implies_(self__,b__)
char self__;
char b__;
{
   char res__ = S_char_VOID_;

   res__ = (char)((! self__) | b__);

   ret0__:
   return (res__);
}

ptr BOO16_to_s_(self__)
char self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_create_(0);
   (void)STR20_b_(res__,self__);

   ret0__:
   return (res__);
}


/* upaths167.c : Sather class: UPATHS, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int STR20_length_(ptr self__);
extern ptr STR20_substring_(ptr self__, int n__, int m__);
extern char STR20_is_equal_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern ptr UNI90_getenv_(ptr self__, ptr name__);
extern ptr UNI90_getcwd_(ptr self__);
extern struct { int tp_; int sz_; char st_; } gs2987_;
#include "macros_.h"



ptr UPA167_env_v_val_(ptr self__, ptr st__);
ptr UPA167_canonicalize_(ptr self__, ptr st__);
/*shared*/ ptr UPA167_automount_name_;
ptr UPA167_strip_automount_name_(ptr self__, ptr st__);
ptr UPA167_patronize_(ptr self__, ptr st__, int len_st__, ptr wd__, int len_wd__);
ptr UPA167_filename_(ptr self__, ptr st__);
ptr UPA167_dirname_(ptr self__, ptr st__);
ptr UPA167_suffix_(ptr self__, ptr st__);
ptr UPA167_filename_sans_(ptr self__, ptr st__);
ptr UPA167_concat_(ptr self__, ptr p1__, ptr p2__);
ptr UPA167_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_UPA167[];

ptr UPA167_env_v_val_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls1512_,"$");

   res__ = (ptr)UNI90_getenv_(0,st__);
   if ((res__ == 0)) {
      res__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1512_)),st__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr UPA167_canonicalize_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   SATHER_STR_(20,5,ls2289_,"HOME");
   ptr    var__ = 0;
   int    len__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   ptr    w__ = 0;

   var__ = S_ptr_VOID_;
   len__ = (int)STR20_length_(st__);
   if ((st__ == 0)) {
      goto ret0__;
   }
   else {
   }
   i__ = S_int_VOID_;
   j__ = S_int_VOID_;
   len__ = (int)STR20_length_(st__);
   while (1) {
      if ((i__ == len__)) {
         goto goto_tag_4510_;
      }
      else {
      }
      if ((CATT_(st__, 8 + ((i__))) == '$')) {
         j__ = (int)(i__ + 1);
         while (1) {
            if ((((j__ == len__) | (CATT_(st__, 8 + ((j__))) == '/')) | (CATT_(st__, 8 + ((j__))) == '.'))) {
               goto goto_tag_4511_;
            }
            else {
            }
            j__ = (int)(j__ + 1);
         }
      goto_tag_4511_: ;
         var__ = (ptr)UPA167_env_v_val_(self__,STR20_substring_(st__,(i__ + 1),(j__ - 1)));
         if ((i__ == 0)) {
            st__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),var__),STR20_substring_(st__,j__,len__));
         }
         else {
            st__ = (ptr)STR20_s_(STR20_s_(STR20_substring_(st__,0,(i__ - 1)),var__),STR20_substring_(st__,j__,len__));
         }
         len__ = (int)(((len__ - j__) + i__) + STR20_length_(var__));
         i__ = (int)(i__ + STR20_length_(var__));
      }
      else {
         i__ = (int)(i__ + 1);
      }
   }
goto_tag_4510_: ;
   if ((((CATT_(st__, 8 + ((0))) == '~') & (len__ > 1)) & (CATT_(st__, 8 + ((1))) == '/'))) {
      res__ = (ptr)STR20_s_(UPA167_env_v_val_(self__,(ptr)(&ls2289_)),STR20_substring_(st__,1,(len__ - 1)));
   }
   else {
      if ((CATT_(st__, 8 + ((0))) == '.')) {
         w__ = (ptr)UNI90_getcwd_(0);
         if ((w__ != 0)) {
            res__ = (ptr)UPA167_patronize_(self__,st__,STR20_length_(st__),w__,STR20_length_(w__));
         }
         else {
         }
      }
      else {
         res__ = (ptr)st__;
      }
   }

   ret0__:
   return (res__);
}

ptr UPA167_strip_automount_name_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    autolen__ = S_int_VOID_;
   int    len__ = S_int_VOID_;

   res__ = (ptr)st__;
   autolen__ = (int)STR20_length_(UPA167_automount_name_);
   if ((autolen__ != 0)) {
      len__ = (int)STR20_length_(st__);
      if ((len__ > autolen__)) {
         autolen__ = (int)(autolen__ - 1);
         if (STR20_is_equal_(STR20_substring_(st__,0,autolen__),UPA167_automount_name_)) {
            res__ = (ptr)STR20_substring_(st__,(autolen__ + 1),len__);
         }
         else {
         }
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr UPA167_patronize_(ptr self__, ptr st__, int len_st__, ptr wd__, int len_wd__)
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls2993_,"/");

   if ((CATT_(st__, 8 + ((0))) == '.')) {
      if (((len_st__ > 1) & (CATT_(st__, 8 + ((1))) == '/'))) {
         res__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),wd__),STR20_substring_(st__,1,(len_st__ - 1)));
      }
      else {
         if ((((len_st__ > 2) & (CATT_(st__, 8 + ((1))) == '.')) & (CATT_(st__, 8 + ((2))) == '/'))) {
            while (1) {
               if (((len_wd__ == 1) | (CATT_(wd__, 8 + ((len_wd__))) == '/'))) {
                  goto goto_tag_4512_;
               }
               else {
               }
               len_wd__ = (int)(len_wd__ - 1);
            }
         goto_tag_4512_: ;
            res__ = (ptr)UPA167_patronize_(self__,STR20_substring_(st__,3,(len_st__ - 1)),(len_st__ - 3),STR20_substring_(wd__,0,(len_wd__ - 1)),len_wd__);
         }
         else {
            if (((len_st__ == 2) & (CATT_(st__, 8 + ((1))) == '.'))) {
               while (1) {
                  if (((len_wd__ == 1) | (CATT_(wd__, 8 + ((len_wd__))) == '/'))) {
                     goto goto_tag_4513_;
                  }
                  else {
                  }
                  len_wd__ = (int)(len_wd__ - 1);
               }
            goto_tag_4513_: ;
               res__ = (ptr)STR20_substring_(wd__,0,(len_wd__ - 1));
            }
            else {
               res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),wd__),(ptr)(&ls2993_)),st__);
            }
         }
      }
   }
   else {
      res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),wd__),(ptr)(&ls2993_)),st__);
   }

   ret0__:
   return (res__);
}

ptr UPA167_filename_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    st_end__ = S_int_VOID_;
   int    stop__ = S_int_VOID_;

   st_end__ = (int)(STR20_length_(st__) - 1);
   stop__ = (int)st_end__;
   while (1) {
      if (((stop__ < 0) | (CATT_(st__, 8 + ((stop__))) == '/'))) {
         goto goto_tag_4514_;
      }
      else {
      }
      stop__ = (int)(stop__ - 1);
   }
goto_tag_4514_: ;
   res__ = (ptr)STR20_substring_(st__,(stop__ + 1),st_end__);

   ret0__:
   return (res__);
}

ptr UPA167_dirname_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    stop__ = S_int_VOID_;

   stop__ = (int)(STR20_length_(st__) - 1);
   while (1) {
      if (((stop__ == 0) | (CATT_(st__, 8 + ((stop__))) == '/'))) {
         goto goto_tag_4515_;
      }
      else {
      }
      stop__ = (int)(stop__ - 1);
   }
goto_tag_4515_: ;
   if ((stop__ == 0)) {
      res__ = (ptr)STR20_create_(0);
   }
   else {
      res__ = (ptr)STR20_substring_(st__,0,(stop__ - 1));
   }

   ret0__:
   return (res__);
}

ptr UPA167_suffix_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    st_end__ = S_int_VOID_;
   int    stop__ = S_int_VOID_;

   st_end__ = (int)(STR20_length_(st__) - 1);
   stop__ = (int)st_end__;
   while (1) {
      if ((((stop__ < 0) | (CATT_(st__, 8 + ((stop__))) == '/')) | (CATT_(st__, 8 + ((stop__))) == '.'))) {
         goto goto_tag_4516_;
      }
      else {
      }
      stop__ = (int)(stop__ - 1);
   }
goto_tag_4516_: ;
   res__ = (ptr)STR20_substring_(st__,(stop__ + 1),st_end__);

   ret0__:
   return (res__);
}

ptr UPA167_filename_sans_(ptr self__, ptr st__)
{
   ptr res__ = 0;
   int    stop__ = S_int_VOID_;
   ptr    name__ = 0;

   stop__ = (int)STR20_length_(UPA167_suffix_(self__,st__));
   name__ = (ptr)UPA167_filename_(self__,st__);
   if ((STR20_length_(name__) == stop__)) {
      res__ = (ptr)STR20_create_(0);
   }
   else {
      res__ = (ptr)STR20_substring_(name__,0,((STR20_length_(name__) - stop__) - 2));
   }

   ret0__:
   return (res__);
}

ptr UPA167_concat_(ptr self__, ptr p1__, ptr p2__)
{
   ptr res__ = 0;
   int    len__ = S_int_VOID_;
   int    p2start__ = S_int_VOID_;

   p1__ = (ptr)copy_(p1__,1);
   if ((p1__ == 0)) {
      res__ = (ptr)copy_(p2__,1);
      goto ret0__;
   }
   else {
      if ((p2__ == 0)) {
         res__ = (ptr)p1__;
         goto ret0__;
      }
      else {
      }
   }
   len__ = (int)STR20_length_(p1__);
   if ((CATT_(p1__, 8 + (((len__ - 1)))) != '/')) {
      p1__ = (ptr)STR20_c_(p1__,'/');
   }
   else {
   }
   p2start__ = (int)0;
   if (((CATT_(p2__, 8 + ((0))) == '.') & (CATT_(p2__, 8 + ((1))) == '/'))) {
      p2start__ = (int)2;
   }
   else {
      if ((CATT_(p2__, 8 + ((0))) == '/')) {
         p2start__ = (int)1;
      }
      else {
      }
   }
   if ((p2start__ != 0)) {
      p2__ = (ptr)STR20_substring_(p2__,p2start__,(STR20_length_(p2__) - p2start__));
   }
   else {
   }
   res__ = (ptr)STR20_s_(p1__,p2__);

   ret0__:
   return (res__);
}

ptr UPA167_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


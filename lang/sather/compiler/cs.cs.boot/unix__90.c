/* unix__90.c : Sather class: UNIX, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern int getgid();
extern int getpid();
extern int getppid();
extern int getuid();
extern int ioctl(int des__, int req__, int arg__);
extern int link(ptr name1__, ptr name2__);
extern void sync();
extern /*shared*/ int errno;
extern int umask(int numask__);
extern int putenv_(ptr name__);
extern ptr str_ptr_(ptr s__);
extern ptr getcwd_();
extern ptr getlogin();
extern int unlink(ptr name__);
extern int rename(ptr name1__, ptr name2__);
extern void exit(int i__);
extern int rmdir(ptr name__);
extern ptr c_object_base_(ptr sather_array__);
extern ptr getenv(ptr name__);
extern int system(ptr com__);
extern int mkdir(ptr name__, int mode__);
extern int fork();
extern int acct(ptr name__);
extern int chdir(ptr name__);
extern int chroot(ptr name__);
extern int execve(ptr prog__, ptr argv__, ptr envp__);
extern int getegid();
extern int geteuid();
#define c_object_base_(a) ((ptr)((ptr)a + ob_base_size_(TYPE_(a)) + 1*SI_))
#include <errno.h>

extern ptr STR20_from_c_str_(ptr self__, ptr st__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
#include "macros_.h"



/*shared*/ int UNI90_unix_error_;
int UNI90_acct_(ptr self__, ptr acctfile__);
int UNI90_chdir_(ptr self__, ptr dirname__);
int UNI90_chroot_(ptr self__, ptr dirname__);
int UNI90_execve_(ptr self__, ptr prog__, ptr argv__, ptr envp__);
int UNI90_fork_(ptr self__);
int UNI90_getegid_(ptr self__);
int UNI90_geteuid_(ptr self__);
int UNI90_getgid_(ptr self__);
int UNI90_getpid_(ptr self__);
int UNI90_getppid_(ptr self__);
int UNI90_getuid_(ptr self__);
int UNI90_ioctl_(ptr self__, int des__, int request__, int arg__);
int UNI90_link_(ptr self__, ptr name1__, ptr name2__);
int UNI90_mkdir_(ptr self__, ptr name__, int mode__);
int UNI90_rename_(ptr self__, ptr from__, ptr to__);
int UNI90_rmdir_(ptr self__, ptr name__);
void UNI90_sync_(ptr self__);
int UNI90_umask_(ptr self__, int numask__);
int UNI90_unlink_(ptr self__, ptr name__);
int UNI90_system_(ptr self__, ptr com__);
ptr UNI90_getenv_(ptr self__, ptr name__);
int UNI90_putenv_(ptr self__, ptr name__, ptr val__);
ptr UNI90_getcwd_(ptr self__);
ptr UNI90_login_name_(ptr self__);
void UNI90_exit_(ptr self__, int i__);
ptr UNI90_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_UNI90[];

int UNI90_acct_(ptr self__, ptr acctfile__)
{
   int res__ = S_int_VOID_;

   res__ = (int)acct(str_ptr_(acctfile__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_chdir_(ptr self__, ptr dirname__)
{
   int res__ = S_int_VOID_;

   res__ = (int)chdir(str_ptr_(dirname__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_chroot_(ptr self__, ptr dirname__)
{
   int res__ = S_int_VOID_;

   res__ = (int)chroot(str_ptr_(dirname__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_execve_(ptr self__, ptr prog__, ptr argv__, ptr envp__)
{
   int res__ = S_int_VOID_;
   ptr    new_argv__ = 0;
   int    argv_sz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    new_envp__ = 0;
   int    envp_sz__ = S_int_VOID_;
   int    i_60_ = S_int_VOID_;
   ptr    c_argv__ = 0;
   ptr    c_envp__ = 0;

   new_argv__ = S_ptr_VOID_;
   if ((argv__ != 0)) {
      argv_sz__ = (int)IATT_(argv__,4);
      new_argv__ = (ptr)new1_(76,argv_sz__,0);
      i__ = S_int_VOID_;
      while (1) {
         if ((i__ >= argv_sz__)) {
            goto goto_tag_1021_;
         }
         else {
         }
         if ((PATT_(argv__, 8 + ((i__) << 2)) != 0)) {
            PATT_(new_argv__, 8 + ((i__) << 2)) = (ptr)str_ptr_(PATT_(argv__, 8 + ((i__) << 2)));
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1021_: ;
   }
   else {
   }
   new_envp__ = S_ptr_VOID_;
   if ((envp__ != 0)) {
      envp_sz__ = (int)IATT_(envp__,4);
      new_envp__ = (ptr)new1_(76,envp_sz__,0);
      i_60_ = S_int_VOID_;
      while (1) {
         if ((i_60_ >= envp_sz__)) {
            goto goto_tag_1022_;
         }
         else {
         }
         if ((PATT_(envp__, 8 + ((i_60_) << 2)) != 0)) {
            PATT_(new_envp__, 8 + ((i_60_) << 2)) = (ptr)str_ptr_(PATT_(envp__, 8 + ((i_60_) << 2)));
         }
         else {
         }
         i_60_ = (int)(i_60_ + 1);
      }
   goto_tag_1022_: ;
   }
   else {
   }
   c_argv__ = S_ptr_VOID_;
   c_envp__ = S_ptr_VOID_;
   if ((new_argv__ != 0)) {
      c_argv__ = (ptr)c_object_base_(new_argv__);
   }
   else {
   }
   if ((new_envp__ != 0)) {
      c_envp__ = (ptr)c_object_base_(new_envp__);
   }
   else {
   }
   res__ = (int)execve(str_ptr_(prog__),c_argv__,c_envp__);
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_fork_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)fork();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_getegid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)getegid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_geteuid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)geteuid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_getgid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)getgid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_getpid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)getpid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_getppid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)getppid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_getuid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)getuid();
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_ioctl_(ptr self__, int des__, int request__, int arg__)
{
   int res__ = S_int_VOID_;

   res__ = (int)ioctl(des__,request__,arg__);
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_link_(ptr self__, ptr name1__, ptr name2__)
{
   int res__ = S_int_VOID_;

   res__ = (int)link(str_ptr_(name1__),str_ptr_(name2__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_mkdir_(ptr self__, ptr name__, int mode__)
{
   int res__ = S_int_VOID_;

   res__ = (int)mkdir(str_ptr_(name__),mode__);
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_rename_(ptr self__, ptr from__, ptr to__)
{
   int res__ = S_int_VOID_;

   res__ = (int)rename(str_ptr_(from__),str_ptr_(to__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_rmdir_(ptr self__, ptr name__)
{
   int res__ = S_int_VOID_;

   res__ = (int)rmdir(str_ptr_(name__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

void UNI90_sync_(ptr self__)
{

   sync();

   ret0__:
   return;
}

int UNI90_umask_(ptr self__, int numask__)
{
   int res__ = S_int_VOID_;

   res__ = (int)umask(numask__);

   ret0__:
   return (res__);
}

int UNI90_unlink_(ptr self__, ptr name__)
{
   int res__ = S_int_VOID_;

   res__ = (int)unlink(str_ptr_(name__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_system_(ptr self__, ptr com__)
{
   int res__ = S_int_VOID_;

   res__ = (int)system(str_ptr_(com__));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

ptr UNI90_getenv_(ptr self__, ptr name__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_from_c_str_(0,getenv(str_ptr_(name__)));
   UNI90_unix_error_ = (int)errno;

   ret0__:
   return (res__);
}

int UNI90_putenv_(ptr self__, ptr name__, ptr val__)
{
   int res__ = S_int_VOID_;
   ptr    arg__ = 0;

   arg__ = (ptr)STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),name__),'='),val__);
   res__ = (int)putenv_(str_ptr_(arg__));

   ret0__:
   return (res__);
}

ptr UNI90_getcwd_(ptr self__)
{
   ptr res__ = 0;
   ptr    c_str__ = 0;

   c_str__ = (ptr)getcwd_();
   if ((c_str__ != 0)) {
      res__ = (ptr)STR20_from_c_str_(0,c_str__);
   }
   else {
      UNI90_unix_error_ = (int)errno;
   }

   ret0__:
   return (res__);
}

ptr UNI90_login_name_(ptr self__)
{
   ptr res__ = 0;
   ptr    c_str__ = 0;

   c_str__ = (ptr)getlogin();
   if ((c_str__ != 0)) {
      res__ = (ptr)STR20_from_c_str_(0,c_str__);
   }
   else {
   }

   ret0__:
   return (res__);
}

void UNI90_exit_(ptr self__, int i__)
{

   exit(i__);

   ret0__:
   return;
}

ptr UNI90_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


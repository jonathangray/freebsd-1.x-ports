/* file_d219.c : Sather class: FILE_DESCRIPTOR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ int O_RDONLY;
extern /*shared*/ int O_WRONLY;
extern /*shared*/ int O_CREAT;
extern int fd_open();
extern /*shared*/ int errno;
extern ptr str_ptr_();
extern int fd_close();
#define fd_open(s,f,m) open(s,f,m)
#define fd_close(fd) close(fd)
#include <errno.h>
#include <fcntl.h>

extern int INT15_bit_or_();
#include "macros_.h"



char FIL219_error_();
ptr FIL219_open_for_read_();
ptr FIL219_open_for_write_();
FIL219_close_();
ptr FIL219_initialize_();
extern int attr_ent_FIL219[];

char FIL219_error_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,8) == (- 1));

   ret0__:
   return (res__);
}

ptr FIL219_open_for_read_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(219,1);
   IATT_(res__,8) = (int)fd_open(str_ptr_(fn__),O_RDONLY,0);
   if ((IATT_(res__,8) == (- 1))) {
      IATT_(res__,4) = (int)errno;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr FIL219_open_for_write_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(219,1);
   IATT_(res__,8) = (int)fd_open(str_ptr_(fn__),INT15_bit_or_(O_WRONLY,O_CREAT),0x1a4);
   if ((IATT_(res__,8) == (- 1))) {
      IATT_(res__,4) = (int)errno;
   }
   else {
   }

   ret0__:
   return (res__);
}

FIL219_close_(self__)
ptr self__;
{

   if ((IATT_(self__,8) == (- 1))) {
      goto ret0__;
   }
   else {
   }
   IATT_(self__,8) = (int)fd_close(IATT_(self__,8));
   if ((IATT_(self__,8) == (- 1))) {
      IATT_(self__,4) = (int)errno;
   }
   else {
   }

   ret0__:
   return;
}

ptr FIL219_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


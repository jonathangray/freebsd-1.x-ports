/* file_d219.c : Sather class: FILE_DESCRIPTOR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern /*shared*/ int O_RDONLY;
extern /*shared*/ int O_WRONLY;
extern /*shared*/ int O_CREAT;
extern int fd_open(ptr s__, int flags__, int mode__);
extern /*shared*/ int errno;
extern ptr str_ptr_(ptr s__);
extern int fd_close(int fd__);
#define fd_open(s,f,m) open(s,f,m)
#define fd_close(fd) close(fd)
#include <errno.h>
#include <fcntl.h>

extern int INT15_bit_or_(int self__, int i__);
#include "macros_.h"



char FIL219_error_(ptr self__);
ptr FIL219_open_for_read_(ptr self__, ptr fn__);
ptr FIL219_open_for_write_(ptr self__, ptr fn__);
void FIL219_close_(ptr self__);
ptr FIL219_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_FIL219[];

char FIL219_error_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,8) == (- 1));

   ret0__:
   return (res__);
}

ptr FIL219_open_for_read_(ptr self__, ptr fn__)
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

ptr FIL219_open_for_write_(ptr self__, ptr fn__)
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

void FIL219_close_(ptr self__)
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

ptr FIL219_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


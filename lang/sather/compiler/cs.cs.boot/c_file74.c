/* c_file74.c : Sather class: C_FILESTAT, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern void set_c_st_atime(ptr cob__, int x__);
extern void set_c_st_mtime(ptr cob__, int x__);
extern void set_c_st_ctime(ptr cob__, int x__);
extern ptr create_c_filestat();
extern int c_st_dev(ptr cob__);
extern int c_st_ino(ptr cob__);
extern int c_st_mode(ptr cob__);
extern int c_st_nlink(ptr cob__);
extern int c_st_uid(ptr cob__);
extern int c_st_gid(ptr cob__);
extern int c_st_rdev(ptr cob__);
extern int c_st_size(ptr cob__);
extern int c_st_atime(ptr cob__);
extern int c_st_mtime(ptr cob__);
extern int c_st_ctime(ptr cob__);
extern void set_c_st_dev(ptr cob__, int x__);
extern void set_c_st_ino(ptr cob__, int x__);
extern void set_c_st_mode(ptr cob__, int x__);
extern void set_c_st_nlink(ptr cob__, int x__);
extern void set_c_st_uid(ptr cob__, int x__);
extern void set_c_st_gid(ptr cob__, int x__);
extern void set_c_st_rdev(ptr cob__, int x__);
extern void set_c_st_size(ptr cob__, int x__);
#define set_c_st_mtime(cob,x) \
{((struct stat *)cob)->st_mtime = x;}
#define set_c_st_ctime(cob,x) \
{((struct stat *)cob)->st_ctime = x;}
#include <sys/types.h>
#include <sys/stat.h>
#define create_c_filestat() (ptr)malloc(sizeof(struct stat))
#define c_st_dev(cob) ((int)(((struct stat *)cob)->st_dev))
#define c_st_ino(cob) ((int)(((struct stat *)cob)->st_ino))
#define c_st_mode(cob) ((int)(((struct stat *)cob)->st_mode))
#define c_st_nlink(cob) ((int)(((struct stat *)cob)->st_nlink))
#define c_st_uid(cob) ((int)(((struct stat *)cob)->st_uid))
#define c_st_gid(cob) ((int)(((struct stat *)cob)->st_gid))
#define c_st_rdev(cob) ((int)(((struct stat *)cob)->st_rdev))
#define c_st_size(cob) ((int)(((struct stat *)cob)->st_size))
#define c_st_atime(cob) ((int)(((struct stat *)cob)->st_atime))
#define c_st_mtime(cob) ((int)(((struct stat *)cob)->st_mtime))
#define c_st_ctime(cob) ((int)(((struct stat *)cob)->st_ctime))
#define set_c_st_dev(cob,x) {((struct stat *)cob)->st_dev = x;}
#define set_c_st_ino(cob,x) {((struct stat *)cob)->st_ino = x;}
#define set_c_st_mode(cob,x) {((struct stat *)cob)->st_mode = x;}
#define set_c_st_nlink(cob,x) \
{((struct stat *)cob)->st_nlink = x;}
#define set_c_st_uid(cob,x) {((struct stat *)cob)->st_uid = x;}
#define set_c_st_gid(cob,x) {((struct stat *)cob)->st_gid = x;}
#define set_c_st_rdev(cob,x) {((struct stat *)cob)->st_rdev = x;}
#define set_c_st_size(cob,x) {((struct stat *)cob)->st_size = x;}
#define set_c_st_atime(cob,x) \
{((struct stat *)cob)->st_atime = x;}

#include "macros_.h"



ptr C_F74_create_(ptr self__);
int C_F74_st_dev_(ptr self__);
int C_F74_st_ino_(ptr self__);
int C_F74_st_mode_(ptr self__);
int C_F74_st_nlink_(ptr self__);
int C_F74_st_uid_(ptr self__);
int C_F74_st_gid_(ptr self__);
int C_F74_st_rdev_(ptr self__);
int C_F74_st_size_(ptr self__);
int C_F74_st_atime_(ptr self__);
int C_F74_st_mtime_(ptr self__);
int C_F74_st_ctime_(ptr self__);
void C_F74_set_st_dev_(ptr self__, int x__);
void C_F74_set_st_ino_(ptr self__, int x__);
void C_F74_set_st_mode_(ptr self__, int x__);
void C_F74_set_st_nlink_(ptr self__, int x__);
void C_F74_set_st_uid_(ptr self__, int x__);
void C_F74_set_st_gid_(ptr self__, int x__);
void C_F74_set_st_rdev_(ptr self__, int x__);
void C_F74_set_st_size_(ptr self__, int x__);
void C_F74_set_st_atime_(ptr self__, int x__);
void C_F74_set_st_mtime_(ptr self__, int x__);
void C_F74_set_st_ctime_(ptr self__, int x__);
ptr C_F74_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_C_F74[];

ptr C_F74_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)create_c_filestat();

   ret0__:
   return (res__);
}

int C_F74_st_dev_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_dev(self__);

   ret0__:
   return (res__);
}

int C_F74_st_ino_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_ino(self__);

   ret0__:
   return (res__);
}

int C_F74_st_mode_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_mode(self__);

   ret0__:
   return (res__);
}

int C_F74_st_nlink_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_nlink(self__);

   ret0__:
   return (res__);
}

int C_F74_st_uid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_uid(self__);

   ret0__:
   return (res__);
}

int C_F74_st_gid_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_gid(self__);

   ret0__:
   return (res__);
}

int C_F74_st_rdev_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_rdev(self__);

   ret0__:
   return (res__);
}

int C_F74_st_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_size(self__);

   ret0__:
   return (res__);
}

int C_F74_st_atime_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_atime(self__);

   ret0__:
   return (res__);
}

int C_F74_st_mtime_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_mtime(self__);

   ret0__:
   return (res__);
}

int C_F74_st_ctime_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)c_st_ctime(self__);

   ret0__:
   return (res__);
}

void C_F74_set_st_dev_(ptr self__, int x__)
{

   set_c_st_dev(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_ino_(ptr self__, int x__)
{

   set_c_st_ino(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_mode_(ptr self__, int x__)
{

   set_c_st_mode(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_nlink_(ptr self__, int x__)
{

   set_c_st_nlink(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_uid_(ptr self__, int x__)
{

   set_c_st_uid(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_gid_(ptr self__, int x__)
{

   set_c_st_gid(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_rdev_(ptr self__, int x__)
{

   set_c_st_rdev(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_size_(ptr self__, int x__)
{

   set_c_st_size(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_atime_(ptr self__, int x__)
{

   set_c_st_atime(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_mtime_(ptr self__, int x__)
{

   set_c_st_mtime(self__,x__);

   ret0__:
   return;
}

void C_F74_set_st_ctime_(ptr self__, int x__)
{

   set_c_st_ctime(self__,x__);

   ret0__:
   return;
}

ptr C_F74_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}


/* cx_builtins.c - builtin function stubs for the standalone C interpreter */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_cx_builtins_c_sccsid[] = "@(#)cx_builtins.c	1.19 20/5/92 (UKC)";

#undef WN	/* define WN to add builtins for wn functions */
#undef MEN3	/* define MEN3 to add builtins for med3lib functions */

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/file.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <dirent.h>
#include <pwd.h>
#include <grp.h>

#include <errno.h>

#ifdef WN
#include <local/wn.h>
#endif

#ifdef MEN3
#include <local/menu3.h>
extern int menerr;
#endif

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "cx_builtins.h"

#ifdef MEN3
MENU *getmenu PROTO((const char *fname, char *mname));
#endif

#ifdef ARCH_CLIPPER
int select PROTO((int width, int *readfds, int *writefds, int *exceptfds,
						struct timeval *timeout));
#endif

/*  BUG: these shouldn't be here.
 */
int setgrent PROTO((void));
int isatty PROTO((int fd));
struct tm *localtime PROTO((long *clock));

static void nullfunc PROTO((void));
static void my_bzero PROTO((char *m, int len));
static void my_bcopy PROTO((const char *src, char *dst, int len));
static void null_saveregs PROTO((void));
static void nyi_setjmp PROTO((void));
static void nyi_longjmp PROTO((void));

/* BUG: these probably shouldn't be here
 */
char *mktemp PROTO((char *template));
char *getwd PROTO((char *pathname));
#ifndef mips
void setlinebuf PROTO((FILE *fp));
#endif
int getopt PROTO((int argc, const char **argv, const char *optstring));
FILE *fdopen PROTO((int fd, const char *mode));
char *ctime PROTO((time_t *clock));

#ifdef OS_SUNOS
int ptrace PROTO((int req, int pid, char *addr, int data, char *addr2));
#else
int ptrace PROTO((int req, int pid, char *addr, int data));
#endif


extern int errno;
extern char *sys_errlist[];
extern char *environ[];
extern int sys_nerr;
extern int optind;
extern char *optarg;

#ifndef __STDC__
#ifdef OS_ULTRIX
int umask();
#else
int setbuf();
#endif
int fflush(), fclose(), fprintf(), printf(), fscanf(), scanf();
int vsprintf(), vfprintf();
int sscanf(), fgetc(), fputc(), puts(), fputs(), ungetc();
int _flsbuf(), _filbuf(), fread(), fwrite(), fseek();
void rewind(), pclose();
int memcmp();
int perror(), utimes(), atoi(), rand(), system(), abs();
int getuid(), geteuid(), getgid(), getegid(), setreuid(), setregid();
int setgroups(), getgroups(), setpwent();
int rename(), read(), write(), pipe(), lseek(), fcntl(), close(), dup(), dup2();
int getdtablesize(), fchown(), fchmod(), flock(), fsync(), ftruncate();
int ioctl(), select(), open(), creat(), link(), unlink(), chown(), chmod();
int mkdir(), symlink(), readlink(), rmdir(), access(), chdir(), chroot();
int truncate(), sync(), stat(), fstat(), lstat(), getpid(), getpgrp();
int setpgrp(), fork(), vfork(), wait3(), killpg(), getrusage();
int execve(), wait(), kill(), _exit(), execl(), execv(), execle(), execlp();
int execvp(), send(), sendto(), sendmsg(), recv(), recvfrom(), recvmsg();
int socket(), socketpair(), getsockopt(), setsockopt(), getsockname();
int bind(), connect(), listen(), getpeername(), gettimeofday();
int settimeofday(), adjtime(), shutdown(), accept();
int alarm(), sleep();
long strtol(), random();
time_t time();
void srandom();
double strtod();
#endif

/*  For testing.
 */
static unsigned char extern_uchar;
static unsigned short extern_ushort;
static unsigned long extern_ulong;
static char extern_char;
static short extern_short;
static long extern_long;
static float extern_float;
static double extern_double;

typedef void (*libfunc_addr_t)PROTO((void *));
typedef taddr_t libvar_addr_t;

typedef struct {
	const char *lf_name;
	libfunc_addr_t lf_addr;
} libfunc_entry_t;

typedef struct {
	const char *lv_name;
	libvar_addr_t lv_addr;
} libvar_entry_t;

#ifdef __STDC__
#define F(func)		{ #func, (libfunc_addr_t)func }
#define V(var)		{ #var, (libvar_addr_t)var }
#else
#define F(func)		{ "func", (libfunc_addr_t)func }
#define V(var)		{ "var", (libvar_addr_t)var }
#endif

static libfunc_entry_t Libfuncs[] = {
	"[nullfunc]",		(libfunc_addr_t)nullfunc,
	"__builtin_saveregs",	(libfunc_addr_t)null_saveregs,
	"longjmp",		(libfunc_addr_t)nyi_longjmp,
	"setjmp",		(libfunc_addr_t)nyi_setjmp,

	/* B1.1 File operations */
	F(fopen),
	F(freopen),
	F(fflush),
	F(fclose),
#ifdef NOTYET
	F(remove),
#endif
	F(rename),
#ifdef NOTYET
	F(tmpfile),
#endif
	F(tmpnam),
#ifdef NOTYET
	F(setvbuf),
#endif
	F(setbuf),

	/* B1.2 Formatted output */
	F(fprintf),
	F(printf),
	F(sprintf),
	F(vfprintf),
	F(vsprintf),

	/* B1.3 Formatted input */
	F(fscanf),
	F(scanf),
	F(sscanf),

	/* B1.4 Character input and output functions */
	F(fgetc),
	F(fgets),
	F(fputc),
	F(fputs),
	F(gets),
	F(puts),
	F(ungetc),

	/* These two are used by most stdio getc/putc macro implementations */
	F(_flsbuf),
	F(_filbuf),

	/* B1.5 Direct input and output functions */
	F(fread),
	F(fwrite),

	/* B1.6 File positioning functions */
	F(fseek),
	F(ftell),
	F(rewind),
#ifdef NOTYET
	F(fgetpos),
	F(fsetpos),
#endif

	/* B1.7 Error functions */
	F(perror),

	F(pclose),
	F(popen),

	/* B? - String and memory functions */
	F(strcpy),
	F(strncpy),
	F(strcat),
	F(strncat),
	F(strcmp),
	F(strncmp),
	F(strchr),
	F(strrchr),
#ifdef NOTYET
	F(strspn),
	F(strcspn),
	F(strpbrk),
	F(strstr),
#endif
	F(strlen),
#ifdef NOTYET
	F(strerror),
#endif
	F(strtok),
	F(memcpy),
#ifdef NOTYET
	F(memmove),
#endif
	F(memcmp),
	F(memchr),
	F(memset),

	/* B? Signals */
	F(signal),
	F(alarm),
	F(sleep),

	/* B5 Utility functions */
	F(atof),
	F(atoi),
	F(atol),
	F(strtod),
	F(strtol),
#ifdef NOTYET
	F(strtoul),
#endif
	F(srandom),
	F(random),
	F(rand),
	F(srand),
	F(calloc),
	F(malloc),
	F(realloc),
	F(free),
	F(abort),
	F(exit),
#ifdef NOTYET
	F(atexit),
#endif
	F(system),
	F(getenv),
#ifdef NOTYET
	F(bsearch),
#endif
	F(qsort),
#ifdef NOTYET
	F(labs),
#endif
#ifdef STRUCTRET
	F(div),
	F(ldiv),
#endif

	/* Directory routines */
	F(opendir),
	F(readdir),
	F(closedir),
	F(seekdir),
	F(telldir),

	/* Old (non-ANSI) library routines */
	"bzero",	(libfunc_addr_t)my_bzero,
	"bcopy",	(libfunc_addr_t)my_bcopy,
	"index",	(libfunc_addr_t)strchr,
	"rindex",	(libfunc_addr_t)strrchr,
#ifndef OS_UMIPS
	F(setlinebuf),
	F(utimes),
#endif
	F(mktemp),
	F(time),
	F(getwd),
	F(ctime),
	F(abs),
	F(getopt),
	F(fdopen),

	/* Authorisation system calls */
	F(getuid),
	F(geteuid),
	F(getgid),
	F(getegid),
#ifndef OS_UMIPS
	F(setreuid),
	F(setregid),
	F(setgroups),
#endif
	F(getgroups),
	F(setgrent),
	F(getgrgid),
	F(setpwent),
	F(getpwent),
	F(getpwnam),
	F(getpwuid),

	/* Pathname and file descriptor type system calls */
	F(read),
	F(write),
	F(pipe),
	F(lseek),
	F(fcntl),
	F(close),
	F(dup),
	F(dup2),
	F(getdtablesize),
	F(fchown),
	F(fchmod),
#ifndef OS_UMIPS
	F(flock),
	F(fsync),
#endif
	F(fcntl),
	F(ftruncate),
	F(ioctl),
	F(select),
	F(open),
	F(creat),
	F(link),
	F(rename),
	F(symlink),
	F(readlink),
	F(unlink),
	F(chown),
	F(chmod),
	F(mkdir),
	F(rmdir),
	F(access),
	F(chdir),
	F(chroot),
	F(truncate),
	F(sync),
	F(umask),
	F(stat),
	F(fstat),
	F(lstat),

	/* Process handling system calls */
	F(getpid),
	F(getpgrp),
	F(setpgrp),
	F(fork),
#ifndef OS_UMIPS
	F(vfork),
	F(wait3),
	F(killpg),
	F(getrusage),
#endif
	F(execve),
	F(wait),
	F(kill),
	F(_exit),
	F(execl),
	F(execv),
	F(execle),
	F(execlp),
	F(execvp),

	/* Socket system calls */
	F(send),
	F(sendto),
	F(sendmsg),
	F(recv),
	F(recvfrom),
	F(recvmsg),
	F(socket),
#ifndef OS_UMIPS
	F(socketpair),
#endif
	F(shutdown),
	F(getsockopt),
	F(setsockopt),
	F(getsockname),
	F(bind),
	F(connect),
	F(listen),
	F(accept),
	F(getpeername),

	/* Time system calls */
	F(gettimeofday),
#ifndef OS_UMIPS
	F(settimeofday),
	F(adjtime),
#endif
	F(localtime),
	
	/* Miscellaneous library functions */
	F(isatty),
	F(ptrace),

#ifdef WN
	F(_wn_want_own_icon),
	F(wn_add_font_path),
	F(wn_box_round),
	F(wn_close_window),
	F(wn_create_cursor),
	F(wn_create_subwin),
	F(wn_create_window),
	F(wn_define_cursor),
	F(wn_deiconise),
	F(wn_invert_line),
	F(wn_draw_line),
	F(wn_free_bitmap),
	F(wn_get_image),
	F(wn_get_nplanes),
	F(wn_get_pixel_colors),
	F(wn_get_pixels_and_planes),
	F(wn_get_pixels_by_color),
	F(wn_get_resize_event),
	F(wn_get_sysfont),
	F(wn_get_win_data),
	F(wn_get_window_size),
	F(wn_get_wn_fds),
	F(wn_getc),
	F(wn_getpuck),
	F(wn_iconise),
	F(wn_inmode),
	F(wn_make_bitmap),
	F(wn_mono_rop),
	F(wn_munge_args),
	F(wn_next_event),
	F(wn_open_font),
	F(wn_open_stdwin),
	F(wn_open_display),
	F(wn_suggest_window_size),
	F(wn_suggest_window_position),
	F(wn_put_image),
	F(wn_set_area),
	F(wn_set_cursor),
	F(wn_set_deiconise_func),
	F(wn_set_fd_mask),
	F(wn_set_pixel_colors),
	F(wn_set_sysfont),
	F(wn_set_win_data),
	F(wn_shade_area),
	F(wn_spcu),
	F(wn_strnwidth),
	F(wn_unmunge_args),
	F(wn_updating_off),
	F(wn_updating_on),
	F(wn_xtext),
	F(wn_get_height),
	F(wn_get_width),
	F(wn_move_area),
	F(wn_pushback_event),
	F(wn_trans_coords),
	F(wn_ungetpuck),
	F(wn_wait_for_release_of),
	F(wn_change_win),
	F(wn_free_area),
	F(wn_get_default),
	F(wn_get_window_cursor),
	F(wn_interpose_event_handler),
	F(wn_invert_box),
	F(wn_npixels_hint),
	F(wn_restore_area),
	F(wn_save_area),
	F(wn_set_abort_func),
	F(wn_set_record_file),
	F(wn_version),
	F(wn_warp_mouse),

	F(wn_strpos),
	F(wn_set_selection),
	F(wn_get_selection),
	F(wn_bell),
	F(wn_await_window_size_change),

	F(wn_from_wid),
	F(wn_install_wid),
	F(wn_get_window_handle),
	F(wn_show_updates),
	F(wn_rop),
	F(wn_get_wm_type),
	F(wn_make_bitmap_from_data),
	F(wn_get_wm_type),
	F(wn_last_rop_was_damaged),
#endif /* WN */

#ifdef MEN3
	F(Mclear),
	F(Mdup),
	F(Mfonts),
	F(Mnobox),
	F(Mnonsel),
	F(Mperror),
	F(Mremove),
	F(Mversion),
	F(Mmake),
	F(Mplace),
	F(Mdisplay),
	F(Mselect),
	F(Mclose),
	F(Mpushsu),
	F(Mpopsu),
	F(Mopenerr),
	F(Msize),
	F(Msclose),
	F(Msopen),
	F(Mredraw),
	F(Mstdfont),
	F(Minsert),
	F(Mfmodes),
	F(Mreset),
	F(Mputc),
	F(Mputs),
	F(getmenu),
#endif
};

static libvar_entry_t Libvars[] = {
	"errno",	(libvar_addr_t)&errno,
	V(_iob),
	"sys_nerr",	(libvar_addr_t)&sys_nerr,
	V(sys_errlist),
	V(environ),
	"optind",	(libvar_addr_t)&optind,
	"optarg",	(libvar_addr_t)&optarg,
#ifdef OS_ULTRIX
	V(_ctype__),
	"_pctype",	(libvar_addr_t)&_pctype,
#else
#ifdef OS_UMIPS
	V(_ctype),
#else
	V(_ctype_),
#endif /* !OS_UMIPS */
#endif /* !OS_ULTRIX */

/* For testing */
	"extern_char",	(libvar_addr_t)&extern_char,
	"extern_short",	(libvar_addr_t)&extern_short,
	"extern_long",	(libvar_addr_t)&extern_long,
	"extern_uchar",	(libvar_addr_t)&extern_uchar,
	"extern_ushort",(libvar_addr_t)&extern_ushort,
	"extern_ulong",	(libvar_addr_t)&extern_ulong,
	"extern_float",	(libvar_addr_t)&extern_float,
	"extern_double",(libvar_addr_t)&extern_double,

#ifdef WN
	"_wn_Bg_pixel",	(libvar_addr_t)&_wn_Bg_pixel,
	"_wn_Fg_pixel",	(libvar_addr_t)&_wn_Fg_pixel,
#endif /* WN */

#ifdef MEN3
	"menerr",	(libvar_addr_t)&menerr,
#endif
};

static void
null_saveregs()
{
}

static void
nyi_setjmp()
{
	panic("call to unimplemented builtin function setjmp");
}

static void
nyi_longjmp()
{
	panic("call to unimplemented builtin function longjmp");
}

static void
nullfunc()
{
	panic("function called via NULL function pointer");
}

static void
my_bcopy(src, dst, len)
const char *src;
char *dst;
int len;
{
	memcpy(dst, src, len);
}

static void
my_bzero(m, len)
char *m;
int len;
{
	memset(m, 0, len);
}

ci_nametype_t
cx_getaddr(name, p_addr)
const char *name;
taddr_t *p_addr;
{
	
	int i;

	for (i = 0; i < sizeof Libfuncs / sizeof *Libfuncs; ++i) {
		if (strcmp(Libfuncs[i].lf_name, name) == 0) {
			*p_addr = (taddr_t)Libfuncs[i].lf_addr;
			return CI_DIRECT_LIBFUNC;
		}
	}

	for (i = 0; i < sizeof Libvars / sizeof *Libvars; ++i) {
		if (strcmp(Libvars[i].lv_name, name) == 0) {
			*p_addr = Libvars[i].lv_addr;
			return CI_DATA;
		}
	}

	return CI_UNDEFINED;
}

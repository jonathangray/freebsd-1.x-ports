/* ml_os.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * OS dependent definitions (to make things look like 4.3bsd)
 */

#ifndef _ML_OS_
#define _ML_OS_

#ifdef THINK_C
#include <time.h>
#else
#include <sys/types.h>
#endif THINK_C

#ifdef V9
struct timeval {
    unsigned long   tv_sec;         /* seconds */
    long            tv_usec;        /* and microseconds */
};
#endif

#ifdef HPUX
#include <time.h>
#endif

/* the return type for signal handlers */
#if defined(VAX) || defined(RISCos) || defined(MACH) || defined(SGI) || defined(AIX) || defined(AUX) || defined(DYNIX)
typedef int SIGH_RET_TYPE;
#else
typedef void SIGH_RET_TYPE;
#endif

#ifdef HPUX
#define SIGWINCH	SIGWINDOW
#define SETSIG(sig, h, mask)	{	\
    struct sigvec svec;			\
    svec.sv_mask = (mask);		\
    svec.sv_onstack = 0;		\
    svec.sv_handler = (h);		\
    sigvector ((sig), &svec, 0);	\
  }
#else /* !HPUX */
#if defined(V9) 
#define SETSIG(sig, h, mask)	{	\
    signal(sig, h);			\
  }
#else /* !HPUX && !V9 */
#if defined(AIX)
#define SETSIG(sig, h, mask) {		\
    struct sigaction siga;		\
    (void)sigfillset(&siga.sa_mask);	\
    siga.sa_flags = 0;			\
    siga.sa_handler = (void (*)(int))h; \
    (void)sigaction(sig,&siga,NULL);	\
    }
#else
#ifdef THINK_C
#define SETSIG(sig, h, mask)	{	\
    e_signal(sig, h);			\
  }
#else /* [e] !HPUX && !V9 & !THINK_C */
#ifdef SOLARIS
#define SETSIG(sig, h, mask)	{	\
    struct sigaction svec;		\
    svec.sa_mask = (mask);		\
    svec.sa_flags = SA_SIGINFO;		\
    svec.sa_handler = (h);		\
    sigaction ((sig), &svec, 0);	\
  }
#else /* [e] !HPUX && !V9 & !THINK_C & !SOLARIS */
#define SETSIG(sig, h, mask)	{	\
    struct sigvec svec;			\
    svec.sv_mask = (mask);		\
    svec.sv_onstack = 0;		\
    svec.sv_handler = (h);		\
    sigvec ((sig), &svec, 0);		\
  }
#endif SOLARIS 
#endif THINK_C 
#endif V9 
#endif HPUX
#endif

/* Signal name and code for initiation of garbage-collection */
#ifdef M68
#if defined(HPUX) || defined(AUX)
#define     GC_SIG       SIGILL
#define     GC_CODE      7
#endif
#if defined(FPE_TRAPV_TRAP)
#define     GC_SIG       SIGFPE
#define     GC_CODE      FPE_TRAPV_TRAP
#endif
#if defined(NeXT)
#define     GC_SIG       SIGFPE
#define     GC_CODE      0x1c    /* TRAPV [cpTRAPcc TRAPcc] instr */
#endif
#if defined(MORE)
#define     GC_SIG       SIGFPE
#define     GC_CODE      FPE_INTOVF_TRAP
#endif
#endif

#if defined(VAX) && defined(V9)
#define FPE_INTOVF_TRAP K_INTOVF
#endif

/* In Mach386, sc_pc is called sc_eip */
#if defined(I386) && defined(MACH)
#define sc_pc	sc_eip
#endif

/* miscellaneous */
#if defined(HPUX) || defined(AUX) || defined(THINK_C)|| defined(SOLARIS)
#define bcopy(src, dst, len)	(memcpy((dst), (src), (len)))
#define bzero(dst, len)		(memset((dst), 0, (len)))
#endif

/* file-descriptor-sets for select() system call */
#ifndef FD_SET
#define	FD_SET(n, p)	((p)->fds_bits[(n)/32] |= (1 << ((n) % 32)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/32] &= ~(1 << ((n) % 32)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/32] & (1 << ((n) % 32)))
#define	FD_ZERO(p)	(bzero((char *)(p), sizeof(*(p))))
#endif

/* cache-flushing stuff */

#if defined(MIPS)
#ifdef sony_news
#include <machine/sysnews.h>
#    define FlushICache(addr, size)	  \
        sysnews(NEWS_CACHEFLUSH, addr, size, FLUSH_ICACHE)
/* SYS_sysnews will be defined in syscall.h, in OS 4.1 */
#undef SYS_sysnews
#else
#ifndef MACH
#include <sys/sysmips.h>
#endif
#include <errno.h>
extern int errno;		/* some header files are missing this decl */
#  ifdef SGI
/* NOTE: need to flush both caches on R4000 machines, since the
 * primary data cache is write-back.
 */
#  include <sys/cachectl.h>
#  define  FlushICache(addr, size)	\
	(cacheflush((void *)(addr), size, BCACHE))
#  else
#    include <sys/syscall.h>
#    include <mips/cachectl.h>
#    ifdef MACH
#    define MIPS_CACHEFLUSH 0x104
#    endif
#    define FlushICache(addr, size)	  \
        (syscall(SYS_sysmips, MIPS_CACHEFLUSH, (addr), (size), ICACHE, 0))
#  endif
#endif
#else
#ifdef NeXT
#  define FlushICache(addr, size)     asm ("trap #2")
#else
#ifdef RS6000
   extern FlushICache();
   extern iCacheLineSize();
#else
#ifdef SPARC
   extern FlushICache();
#else
#ifdef HPPA
#  define FlushICache(addr, size) flush_icache(addr, size)
#else
#ifdef THINK_C
#  define FlushICache(addr, size)     FlushCacheRange((void *)(addr), size)
#else
#  define FlushICache(addr, size)
#endif
#endif
#endif
#endif
#endif
#endif


#if defined(MACH) && defined(MIPS)

   /* Definitions for MIPS-based machines running Mach emulating 4.3bsd */

#    define STYP_RDATA	0x100		/* section contains read only data */
#    define STYP_SDATA	0x200		/* section contains small data only */
#    define STYP_SBSS	0x400		/* section contains small bss only */
#    define STYP_LIT8	0x08000000	/* literal pool for 8 byte literals */
#    define STYP_LIT4	0x10000000	/* literal pool for 4 byte literals */
#    define STYP_INIT	0x80000000	

#endif

/** Reading directories **/


#if defined(SUNOS) || defined(SGI)
#  include <sys/dirent.h>
#  define READDIR(fd,buf,sz)	getdents((fd), (buf), (sz))
   typedef struct dirent DIR_ENTRY_TY;
#else /* !SUNOS && !SGI */
#  if defined(HPUX)
#    include <sys/param.h>
    /* the following are for <ndir.h> */
#    define LONGFILENAMES
#    define DIRSIZ_MACRO
#    include <ndir.h>
#  else /* !SUNOS && !SGI !HPUX */
#    ifdef AUX
#      include <limits.h>
#    endif
#ifdef THINK_C
#define getdirentries getdirentries_e
#else
#include <sys/dir.h>
#endif THINK_C
#  endif /* HPUX */
#  if defined (AIX)
    /** wonder how long getdirent will be supported? **/
#    define READDIR(fd,buf,sz)	getdirent((fd), (buf), (sz))
#  else /* !SUNOS && !SGI && !AIX */
     static long dummy;
#    define READDIR(fd,buf,sz)      getdirentries((fd), (buf), (sz), &dummy)
#  endif /* AIX */
   typedef struct direct DIR_ENTRY_TY;
#endif /* SUNOS SGI */

#ifndef DIRBLKSIZ
#define DIRBLKSIZ 512
#endif /* !DIRBLKSIZ */

#if defined(BSD) || defined(RISCos) || defined(HPUX) || defined(SGI) || defined(AUX)
#define HAS_WRITEV
#include <sys/uio.h>
#define HAS_NONBLOCKING_IO
#endif

#ifdef NeXT
extern void *get_edata(), *get_etext();
# define EDATA                ((int)get_edata())
# define ETEXT                ((int)get_etext())
#else
#ifdef AIX
extern int _edata, _etext;
#define EDATA		      ((int)&_edata)
#define ETEXT		      ((int)&_etext)
#else
#ifdef THINK_C
extern int get_edata(), get_etext();
# define EDATA                ((int)get_edata())
# define ETEXT                ((int)get_etext())
#else
extern int edata, etext;
# define EDATA                ((int)(&edata))
# define ETEXT                ((int)(&etext))
#endif
#endif
#endif

#endif /* !_ML_OS_ */

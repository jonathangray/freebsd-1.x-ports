/* cfuns.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are the C functions that are callable from ML (via REQ_CALLC).
 */

#include "ml_os.h"
#ifdef THINK_C
#include <unix.h>
#include <fcntl.h>
#include <errno.h>
#include "MacOS.dep.h"
#else
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <sys/errno.h>
#ifdef V9
#include <sys/filio.h>
#include <sys/ttyio.h>
#else
#include <sys/ioctl.h>
#include <sys/file.h>
#ifndef HPUX
#include <sys/time.h>
#endif
#endif
#ifndef HPUX
#include <sys/param.h>
#endif
#if (defined(SPARC) && !defined(MACH))
#ifdef SOLARIS
#include <unistd.h>
#include <sys/filio.h>
#include <fcntl.h>
#include <signal.h>
extern sigset_t emptyMask;
#else
#include <vfork.h>	/* tells sparc optimizer about vfork */
#endif
#endif
#include <sys/wait.h>
#endif THINK_C
#ifdef HPUX
#include <sys/utsname.h>
#endif

#include "ml_state.h"
#include "ml_types.h"
#include "cause.h"
#include "prim.h"

/* Imported from mp.c */
extern void ml_release_proc ();
extern void ml_acquire_proc ();
extern void ml_spin_lock ();
extern void ml_max_procs ();

/* do a system call, restarting it if interrupted */
#define DO_SYSCALL(CALL,sts) {					\
	while (((sts = CALL) == -1) && (errno == EINTR))	\
            continue;						\
    }

/* return a value to the calling ML code */
#define RETURN(msp,r)	{		\
    msp->ml_arg = (r);			\
    return;}

/* return sts to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN(msp,sts)	{		\
    if (sts == -1) raise_syserror(msp, 0);	\
    else RETURN(msp, INT_CtoML(sts)) }

/* return unit to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN_UNIT(msp,sts)	{		\
    if (sts == -1) raise_syserror(msp, 0);	\
    else RETURN(msp, ML_unit) }

extern int	    errno;

/* backup_kont:
 * When a signal interrupts a blocking select call, this is called to
 * back-up the continuation to re-try the select.
 * NOTE: the call_c entry does not save the link register, so we must
 * set it here.
 */
static void backup_kont (msp)
    MLState_ptr		msp;
{
    msp->ioWaitFlag	= 0;
    msp->ml_closure	= PTR_CtoML(callc_v+1);
    msp->ml_pc		=
    msp->ml_linkreg	= CODE_ADDR(PTR_CtoML(callc_v+1));
}

#ifdef V9

#define MAX_TIMEOUT	0x7fffffff

/* v9_select:
 * A 4.2bsd interface to V9 select.
 */
static int v9_select (width, rfds, wfds, efds, *timeout)
    int		    width;
    fd_set	    *rfds, *wfds, *efds;
    struct timeval  *timeout;
{
    int		    t, sts;

    if (timeout == 0)
	t = MAX_TIMEOUT;
    else {
	t = timeout.tv_usec / 1000;
	if (timeout.tv_sec > (MAX_TIMEOUT/1000)-t)
	    t = MAX_TIMEOUT;
	else
	    t += (t.tv_sec * 1000);
    }

    if (efds != NULL)
	return -1;  /* exceptional conditions not supported on V9 */

    DO_SYSCALL(select(width, rfds, wfds, t), sts);
    return sts;
}

#define select v9_select
#endif V9


/* raise_syserror:
 * Raise the ML exception SysError with the errno and error string as the argument
 * pair.  If alt_msg is non-zero, then use it as the error string and use -1 as the
 * errno.
 */
void raise_syserror (msp, alt_msg)
    MLState_ptr		msp;
    char	    *alt_msg;
{
    extern int	    sys_nerr, syserror_id0[];
    extern char	    *sys_errlist[];
    ML_val_t	    s, arg, exn;

    if (alt_msg != 0) {
	s = ML_alloc_string (msp, alt_msg);
	errno = -1;
    }
#ifndef THINK_C
    else if ((0 <= errno) && (errno < sys_nerr))
	s = ML_alloc_string (msp, sys_errlist[errno]);
#endif
    else {
	char		buf[32];
	sprintf(buf, "<unknown error %d>", errno);
	s = ML_alloc_string (msp, buf);
    }

    REC_ALLOC2 (msp, arg, INT_CtoML(errno), s);
    REC_ALLOC2 (msp, exn, PTR_CtoML(syserror_id0+1), arg);
    msp->fault_exn = exn;

    raise_ml_exn (msp);
}


#ifdef THINK_C
#include <stdarg.h>
#define SYS_read	3
#define SYS_write	4
#define SYS_open	5
#define SYS_close	6

void raise_ThinkC_error(msp)
    MLState_ptr		msp;
    {
    raise_syserror(msp, "unimplemented");
}

int syscall(int n, ...)
	{
	int arg0, arg1, arg2;
	int i = -1;
	va_list xp;
	
	va_start(xp, n);
	arg0 = va_arg(xp, int);
	arg1 = va_arg(xp, int);
	arg2 = va_arg(xp, int);
	va_end(xp);

	switch(n) {
	    case SYS_read: {
		char *buf = (char *) arg1;
		int  fd = (int) arg0;
		
		if (fd == 0) {	/* stdin */
		    fgets(buf, (int) arg2, stdin);
		    i = strlen(buf); 
		}
		else
		    i = read(fd, buf, arg2);
		break;
	    }
	    case SYS_write: {
		int fd = (int) arg0;
		char *buffer = (char *) arg1;
		unsigned nbytes = (unsigned) arg2;

		i = write(fd, buffer, nbytes);
		break;
	    }
	    case SYS_open: {
#ifdef IGNORE_THIS
	    	/* never reached!? asm{ DC.W 0xA9FF } /* Debug */
		/* lousy trick to avoid '\r'
		if (strcmp((char*)arg0,"boot/assembly.sig") == 0)
			open_mode = O_TEXT | O_RDONLY;
			 */
		/* after boot/..., the only time we need binary mode again */
		/* is when we export i.e. arg1 == 1537 */
		if (arg1==1537)
			/* 'exportML' */
			i = open((char *) arg0, (O_WRONLY | O_CREAT | O_TRUNC | O_BINARY)); 
		else if (0)
			/* 'open_in' statement */;
		else
			/* 'use' statement */
			i = open((char *) arg0, (O_TEXT | O_RDONLY));  /* was open_mode */
#endif
		i = eopen((char *) arg0, (arg1==1537)?(O_WRONLY | O_CREAT | O_TRUNC):(O_RDONLY));
		break;
	    }
	    case SYS_close:
		i = close((int) arg0);
		break;
	    default:
		e_raise(SIGSYS);
		break;
	    }
	return i;
}

int fionread(int fd) 
{
	int pos,sz;

	if (isatty(fd)) return 1;	/* console always has something?! */
	pos = tell(fd);			/* get current position */
	if(pos < 0) return(pos);	/* error */
	sz = lseek(fd, 0, SEEK_END);	/* get file size (inefficiently) */
	if(sz < 0) return(sz);		/* error */
	pos = lseek(fd, pos, SEEK_SET);	/* recover position */
	if(pos < 0) return(pos);	/* error */
 	return (sz - pos);
}

#endif


#define MAX_SYSCALL_ARGS	6

/* ml_syscall : (int * string list) -> int
 * Perform the requested system call with the given arguments.  Unboxed
 * values are converted to C ints.
 */
#ifdef AIX
void ml_syscall(msp,arg)
    MLState_ptr           msp;
    ML_val_t      arg;
{
    raise_syserror (msp, "ml_syscall: Not implemented");
    return;
}
#else
void ml_syscall (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		    code = REC_SELINT(arg, 0);
    register ML_val_t p = REC_SEL(arg, 1);
    int		    av[MAX_SYSCALL_ARGS];
    register int    i, r;
    register ML_val_t v;

#if (MAX_PROCS > 1)
    if (code == 1) /* exit */ {
      mp_shutdown(msp, INT_MLtoC(ML_hd(p)));
    }
#endif
#if  defined(RISCos) || defined(SGI)
    code += 1000;  /* adjust syscall code for RISCos */
#endif
    for (i = 0; p != ML_nil; p = ML_tl(p), i++) {
	if (OBJ_isBOXED(v = ML_hd(p)))
	    av[i] = (int)PTR_MLtoC(v);
	else
	    av[i] = INT_MLtoC(v);
    }
#ifdef THINK_C
    if (!  setjmp(msp->SysCallEnv))
#else
#ifdef SOLARIS
    if (! sigsetjmp(msp->SysCallEnv,0))
#else
    if (! _setjmp(msp->SysCallEnv))
#endif SOLARIS
#endif THINK_C
	switch (i) {
	  case 0:
	    r = syscall(code); break;
	  case 1:
	    r = syscall(code, av[0]); break;
	  case 2:
	    r = syscall(code, av[0], av[1]); break;
	  case 3:
	    r = syscall(code, av[0], av[1], av[2]); break;
	  case 4:
	    r = syscall(code, av[0], av[1], av[2], av[3]); break;
	  case 5:
	    r = syscall(code, av[0], av[1], av[2], av[3], av[4]); break;
	  case 6:
	    r = syscall(code, av[0], av[1], av[2], av[3], av[4], av[5]); break;
	  default:
	    raise_syserror (msp, "ml_syscall: too many args");
	    return;
	}
    else {
      /* a SIGSYS occurred (because of a bad syscall) */
#ifndef THINK_C
#ifdef SOLARIS
	sigprocmask(SIG_SETMASK,&emptyMask,0);
#else
	sigsetmask (0);  /* re-enable signals */
#endif SOLARIS
#endif THINK_C
	raise_syserror (msp, "bad syscall");
	return;
    }

    if (r == -1) {
	if (errno == EINTR)
	    backup_kont(msp);
	else
	    raise_syserror(msp, 0);
	return;
    }
    else
	RETURN(msp, INT_CtoML(r));

} /* end of ml_syscall. */
#endif


/* ml_openf : (string * int) -> int
 * Open a file and return the file descriptor.
 */
void ml_openf (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    char	    *path = (char *)REC_SELPTR(arg, 0);
    int		    mode = REC_SELINT(arg, 1);
    int		    fd, flags;

#if defined(V9) || (defined(HPUX)  && defined(M68))
    switch (mode) {
      case 0: /* READ */
	DO_SYSCALL(open(path, 0), fd);
	break;
      case 1: /* WRITE */
	DO_SYSCALL(creat(path, 0666), fd);
	break;
      case 2: /* APPEND */
	DO_SYSCALL(open(path, 1), fd);
	if (fd == -1)
	    DO_SYSCALL(creat(path, 0666), fd)
	else if (lseek(fd, 0, L_INCR) == -1)
	    fd = -1;
	break;
    }
#else
    /*
    ** Note: The O_RDWR flag in case 1:, is a temporary in order
    ** to support exportML for the HPPA. The combination of
    ** flags will not affect previous applications that opened
    ** files for writing only -lg.
    */

    switch (mode) {
      case 0: flags = (O_RDONLY); break;
      case 1: flags = (O_RDWR|O_TRUNC|O_CREAT); break;
      case 2: flags = (O_WRONLY|O_APPEND|O_CREAT); break;
    }
#ifdef THINK_C
    DO_SYSCALL(eopen (path, flags), fd);
#else
    DO_SYSCALL(open (path, flags, 0666), fd);
#endif
#endif

    CHK_RETURN(msp, fd);

} /* end of ml_openf */


/* ml_closef : int -> unit
 */
void ml_closef (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		    sts;

    DO_SYSCALL (close(INT_MLtoC(arg)), sts);
    CHK_RETURN_UNIT(msp, sts);

} /* end of ml_closef */

/* ml_connect_unix : string -> fd
 * Open a client-side UNIX domain STREAM socket connection on the given pathname.
 */
void ml_connect_unix (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac does not have sockets */
    raise_ThinkC_error(msp);
#else
#ifdef SOLARIS
    char	    *path = (char *)PTR_MLtoC(arg);
    int		    fd;
    static int      connect_unix();

    fd = connect_unix(path);
    if (fd >= 0) {
      RETURN(msp, INT_CtoML(fd));
    }
    else
      raise_syserror(msp, 0);
#else
    char	    *path = (char *)PTR_MLtoC(arg);
    struct sockaddr_un sock;
    int		    fd, len, sts;

    DO_SYSCALL (socket(PF_UNIX, SOCK_STREAM, 0), fd);
    if (fd != -1) {
	sock.sun_family = AF_UNIX;
	strcpy (sock.sun_path, path);
#ifdef AIX
	len = strlen(path)+sizeof(sock.sun_len)+sizeof(sock.sun_family)+1;
	sock.sun_len = len;
#else
	len = strlen(path)+sizeof(sock.sun_family);
#endif
	DO_SYSCALL (connect(fd, (struct sockaddr *)&sock, len), sts);
	if (sts != -1) {
	    RETURN(msp, INT_CtoML(fd));
	}
	else {
	    int		olderrno = errno;
	    close (fd);
	    errno = olderrno;
	}
    }
    raise_syserror(msp, 0);

#endif
#endif
} /* end of ml_connect_unix */

/* ml_connect_inet : (string * string) -> fd
 * Open a client-side INET domain STREAM socket connection to the given host/port.
 * Currently the host must be specified as a string of the form "d.d.d.d" where
 * the d's are decimal numbers from 0 to 255 separated by "."s.  The port is specified
 * as a string representation of a decimal port number.
 * Note: eventually this interface will be extended to use symbolic names.
 */
void ml_connect_inet (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac does not have sockets */
    raise_ThinkC_error(msp);
#else
#ifdef SOLARIS
    char	    *hostname = (char *)REC_SELPTR(arg, 0);
    char	    *port = (char *)REC_SELPTR(arg, 1);
    int		    fd;
    static int      connect_inet();

    fd = connect_inet(hostname, port);
    if (fd >= 0) {
      RETURN(msp, INT_CtoML(fd));
    }
    else
      raise_syserror(msp, 0);
#else
    char	    *hostname = (char *)REC_SELPTR(arg, 0);
    char	    *port = (char *)REC_SELPTR(arg, 1);
    struct sockaddr_in saddr;
    int		    fd, s, i, sts;

#if defined(SUNOS) || (defined(BSD) && defined(MIPS)) || defined(NeXT) || defined(AUX) || defined(HPPA)
    DO_SYSCALL (socket(PF_INET, SOCK_STREAM, 0), fd);
    if (fd != -1) {
	saddr.sin_family = AF_INET;
	saddr.sin_port = htons(atoi(port));
	bzero(saddr.sin_zero, sizeof(saddr.sin_zero));
	s = i = 0;
	do {
	    s = (s << 8) | atoi(hostname);
	    while (*hostname && (*hostname != '.'))
		hostname++;
	} while (*hostname++ != '\0');
	saddr.sin_addr.s_addr = htonl(s);
	DO_SYSCALL (connect(fd, (struct sockaddr *)&saddr, sizeof(saddr)), sts);
	if (sts == 0) {
	    RETURN(msp, INT_CtoML(fd));
	}
	else {
	    int		olderrno = errno;
	    close (fd);
	    errno = olderrno;
	}
    }
    raise_syserror(msp, 0);
#else
    raise_syserror(msp, "unimplemented");
#endif
#endif
#endif
} /* end of ml_connect_unix */


/* ml_link : (bool * string * string) -> unit
 * Create a link (or symbolic link).
 */
void ml_link (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac has links, but how to access from C? */
    raise_ThinkC_error(msp);
#else
    ML_val_t	    is_sym = REC_SEL(arg, 0);
    char	    *name = (char *)REC_SELPTR(arg, 1);
    char	    *lname = (char *)REC_SELPTR(arg, 2);
    int		    sts;

    if (is_sym == ML_true)
	sts = symlink (name, lname);
    else
	sts = link (name, lname);

    CHK_RETURN(msp, sts);
#endif
} /* end of ml_link */


/* ml_unlink : string -> unit
 */
void ml_unlink (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac has links, but how to access from C? */
    raise_ThinkC_error(msp);
#else
    int		    sts;

    DO_SYSCALL(unlink((char *)PTR_MLtoC(arg)), sts);
    CHK_RETURN_UNIT(msp, sts);
#endif
} /* end of ml_unlink */

/* ml_dup : int -> int
 */
void ml_dup (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac has links, but how to access from C? */
    raise_ThinkC_error(msp);
#else
    int		    fd;

    DO_SYSCALL(dup(INT_MLtoC(arg)), fd);
    CHK_RETURN(msp, fd);
#endif
} /* end of ml_dup */

/* ml_wait_for_in : fd -> unit
 * Wait for input on the given file descriptor.
 */
void ml_wait_for_in (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg), sts;
#ifdef THINK_C
    /* Mac version not done yet!? */
    RETURN(msp, ML_unit);
    /*
    raise_ThinkC_error(msp);
    */
#else
    fd_set	    rfds;

    if (msp->inSigHandler || msp->maskSignals
#ifdef SOLARIS
    || ((! sigsetjmp (msp->SysCallEnv,0)) && 
#else
    || ((! _setjmp (msp->SysCallEnv)) && 
#endif
	(((msp->ioWaitFlag = 1), (msp->NumPendingSigs == 0))))) {
#ifdef RISCos
	/* problem with select and pipes */
	sts = 0;
#else
	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);
	sts = select(fd+1, &rfds, 0, 0, 0);
#endif
	msp->ioWaitFlag = 0;
    }
    else {
	backup_kont(msp);
#ifdef SOLARIS
	sigprocmask(SIG_SETMASK,&emptyMask,0);
#else
	sigsetmask (0);  /* re-enable signals */
#endif
	return;
    }

    if (sts == -1) {
	if (errno == EINTR)
	    backup_kont(msp);
	else
	    raise_syserror(msp, 0);
	return;
    }

    RETURN(msp, ML_unit);
#endif
} /* end of ml_wait_for_in. */


/* ml_read : (int * bytearray * int) -> int
 * Read data from the specified file into the given bytearray.  Return the
 * number of bytes read.
 */
void ml_read (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    int		    nbytes = REC_SELINT(arg, 2);
    int		    n;

#ifdef THINK_C
    if (fd == 0) {	/* stdin */
	if(fgets(buf, nbytes, stdin))
	    n = strlen(buf);
	else
	    n = -1;
    }
    else
	DO_SYSCALL (read (fd, buf, nbytes), n);
#else
    DO_SYSCALL (read (fd, buf, nbytes), n);
#endif
    CHK_RETURN(msp, n);

} /* end of ml_read */

/* ml_readi : (int * bytearray * int * int) -> int
 * Read data from the specified file into the given bytearray, starting at
 * offset.  Return the number of bytes read.
 */
void ml_readi (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    char	    *start = buf + REC_SELINT(arg, 2);
    int		    nbytes = REC_SELINT(arg, 3);
    int		    n;

#ifdef THINK_C
    DO_SYSCALL (read (fd, start, nbytes), n);  /* right!? */
#else
    DO_SYSCALL (read (fd, buf, nbytes), n);
#endif
    CHK_RETURN(msp, n);

} /* end of ml_readi */


/* write_all:
 * Write the requested number of bytes from the buffer.  Return 0 on success,
 * and -1 on errors.
 */
static int write_all (fd, buf, nbytes)
    int		    fd;
    char	    *buf;
    int		    nbytes;
{
    register int    n;

    while (nbytes > 0) {
	DO_SYSCALL (write (fd, buf, nbytes), n);
	if (n > 0) {
	    nbytes -= n;
	    buf += n;
	}
	else
	    return -1;
    }
    return 0;

} /* end of write_all. */

/* ml_write : (int * bytearray * int) -> unit
 * Write data from the given bytearray to the specified file.  Return the
 * number of bytes written.
 */
void ml_write (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    sts;

    sts = write_all (
	    REC_SELINT(arg, 0),
	    (char *)REC_SELPTR(arg, 1),
	    REC_SELINT(arg, 2));

    if (sts == -1)
	raise_syserror(msp, 0);
    else
	RETURN(msp, ML_unit);

} /* end of ml_write */

/* ml_writei : (int * bytearray * int * int) -> unit
 * Write data from the given bytearray to the specified file, starting at the
 * given offset.  This routine is guaranteed to write all the bytes.
 */
void ml_writei (msp, arg)
    MLState_ptr     msp;
    register ML_val_t arg;
{
    int		    sts;

    sts = write_all (
	    REC_SELINT(arg, 0),
	    (char *)REC_SELPTR(arg, 1) + REC_SELINT(arg, 2),
	    REC_SELINT(arg, 3));

    if (sts == -1)
	raise_syserror(msp, 0);
    else
	RETURN(msp, ML_unit);

} /* end of ml_writei */


#ifndef HAS_WRITEV
struct iovec {
    char	    *iov_base;
    int		    iov_len;
};
#endif /* [e] !HAS_WRITEV */

/* write_multiple:
 * Write a vector of blocks and return the number of blocks written.  Normally,
 * this will be iovcnt, but if a signal occurs during the write, then it can
 * be less.  Return -1 on error.
 */
static int write_multiple (msp, fd, iov, iovcnt, nbytes)
    MLState_ptr     msp;
    int		    fd;
    struct iovec    *iov;
    int		    iovcnt, nbytes;
{
#ifdef HAS_WRITEV
    int		    skip = 0, i = iovcnt;

    while (nbytes > 0) {
      advance:;
	while (skip > 0) {
	    if (iov->iov_len <= skip)
		skip -= iov->iov_len;
	    else {
	      /* write the incomplete buffer */
		int	sts;
		do {
		    sts = write_all(fd, iov->iov_base+skip, iov->iov_len-skip);
		    if (sts < 0) {
			raise_syserror(msp, 0);
			return -1;
		    }
		} while (sts != 0);
		if ((nbytes -= (iov->iov_len - skip)) == 0)
		    return iovcnt;
		else if (msp->NumPendingSigs > 0)
		    return ((iovcnt - i) + 1);
	    }
	    i--;  iov++;
	}
	DO_SYSCALL (writev(fd, iov, i), skip);
	if (skip < 0) {
	    raise_syserror(msp, 0);
	    return -1;
	}
	nbytes -= skip;
    }
#else /* [e] !HAS_WRITEV */
    int		    i, sts;

    for (i = 0;  i < iovcnt;  i++) {
/*	if ((sts = write_all (fd, vec[i].iov_base, vec[i].iov_len)) == -1)  wrong?! 02Jan92  e  */
	if ((sts = write_all (fd, iov[i].iov_base, iov[i].iov_len)) == -1)
	    return -1;
	else
	    i++;
	if (msp->NumPendingSigs > 0)
	    return i;
    }
#endif HAS_WRITEV

    return iovcnt;

} /* end of write_multiple */

#define WRITEVEC_SZ	8

/* ml_writev : (int * (bytearray * int) list) -> unit
 * For each (data, len) element in the list, write the len number of bytes to the
 * file descriptor.
 */
void ml_writev (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    ML_val_t	    p = REC_SEL(arg, 1), q = p;
    int		    nbytes = 0, i, n;
    struct iovec    vec[WRITEVEC_SZ];

    nbytes = 0;
    for (i = 0; p != ML_nil;  ) {
	ML_val_t    pair = ML_hd(p);

	p = ML_tl(p);
	vec[i].iov_base = (char *)REC_SELPTR(pair, 0);
	vec[i].iov_len  = REC_SELINT(pair, 1);
	nbytes += vec[i].iov_len;
	if ((++i == WRITEVEC_SZ) || (p == ML_nil)) {
	    if ((n = write_multiple(msp, fd, vec, i, nbytes)) < 0)
		return; /* error */
	    else if (n < i) {
	      /* a signal occurred, so set things up so that the resume continuation
	       * will complete the write operation.
	       */
		while (n > 0) {
		    q = ML_tl(q);  n--;
		}
		REC_ALLOC2 (msp, arg, INT_CtoML(fd), q);
		REC_ALLOC2 (msp, msp->ml_arg, PTR_CtoML(ml_writev), arg);
		msp->ml_closure = PTR_CtoML(callc_v+1);
		msp->ml_pc	    = CODE_ADDR(PTR_CtoML(callc_v+1));
		return;
	    }
	    nbytes = 0;
	    i = 0;
	    q = p;
	}
    } /* end of for */

} /* end of ml_writev */

/* ml_lseek : (int * int * int) -> int
 */
void ml_lseek (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		fd = REC_SELINT(arg, 0);
#ifdef THINK_C
    long	offset = REC_SELINT(arg, 1), pos;
#else
    off_t	offset = REC_SELINT(arg, 1), pos;
#endif
    int		whence = REC_SELINT(arg, 2);

    DO_SYSCALL(lseek(fd, offset, whence), pos);
    CHK_RETURN(msp, pos);

} /* end of ml_lseek */

/* ml_send_obd : (fd * bytearray * int) -> unit
 * Send out-of-band data on the specified socket file descriptor.
 */
void ml_send_obd (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac does not have sockets */
    raise_ThinkC_error(msp);
#else
#ifdef SOLARIS
    raise_syserror(msp, "unimplemented");
#else
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    register int    nbytes = REC_SELINT(arg, 2);
    register int    n;

    while (nbytes > 0) {
	DO_SYSCALL (send (fd, buf, nbytes, MSG_OOB), n);
	if (n > 0) {
	    nbytes -= n;
	    buf += n;
	}
	else
	    raise_syserror (msp, 0);
    }
#endif
#endif
} /* end of ml_send_obd */


/* ml_getdirent : int -> string list
 * Get directory entries from the directory referenced by fdesc.  If there are
 * no more entries, then return nil.
 */
static void ml_getdirent (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    int		    fd = INT_MLtoC(arg);
    char	    buf[DIRBLKSIZ];
    register int    nbytes, i;
    ML_val_t	    l = ML_nil;

    do {
	
	DO_SYSCALL (READDIR(fd, (DIR_ENTRY_TY *)buf, DIRBLKSIZ), nbytes); 
	if (nbytes == -1) {
	    raise_syserror (msp, 0);
	    return;
	}
	else {
	    ML_val_t	    s;
	    DIR_ENTRY_TY    *dp;

	    for (i = 0;  i < nbytes;  i += dp->d_reclen) {
		dp = (DIR_ENTRY_TY *)&(buf[i]);
		if (dp->d_name[0] != 0) {
		    s = ML_alloc_string (msp, dp->d_name);
		    l = ML_cons (msp, s, l);
		}
	    } /* end of for */
	}
    } while ((nbytes > 0) && (l == ML_nil));
    RETURN(msp, l);
#endif
} /* end of ml_getdirent */


/* ml_chdir : string -> unit
 */
void ml_chdir (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		sts;

    DO_SYSCALL(chdir((char *)PTR_MLtoC(arg)), sts);
    CHK_RETURN_UNIT(msp, sts);

} /* end of ml_chdir */

/* ml_mkdir : (string * int) -> unit
 */
void ml_mkdir (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    char	*path = (char *)REC_SELPTR(arg, 0);
    int		mode = REC_SELINT(arg, 1);
    int		sts;

    DO_SYSCALL(mkdir(path, mode), sts);
    CHK_RETURN_UNIT(msp, sts);
#endif
} /* end of ml_mkdir */

/* ml_readlink : string -> string
 * Read the contents of the specified symbolic link.
 */
void ml_readlink (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    char	    *lname = (char *)PTR_MLtoC(arg);
    int		    n;
    char	    buf[MAXPATHLEN];

    if ((n = readlink(lname, buf, MAXPATHLEN)) == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t	path;
	buf[n] = '\0';
	path = ML_alloc_string (msp, buf);
	RETURN(msp, path);
    }
#endif
} /* end of ml_readlink */


/* ml_truncate : (fd or string * int) -> unit
 * Truncate the specified file to the specified length.
 */
void ml_truncate (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    register ML_val_t f = REC_SEL(arg, 0);
    int		    len = REC_SELINT(arg, 1);
    int		    sts;

    if (OBJ_isBOXED(f))
#ifdef SOLARIS
	sts = truncate((char *)PTR_MLtoC(f), len);
#else
	sts = truncate(PTR_MLtoC(f), len);
#endif
    else
	sts = ftruncate(INT_MLtoC(f), len);

    CHK_RETURN(msp, sts);
#endif
} /* end of ml_truncate */

/* ml_umask : int -> int
 */
void ml_umask (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    int		oldMask;

    DO_SYSCALL(umask(INT_MLtoC(arg)), oldMask);
    CHK_RETURN(msp, oldMask);
#endif
} /* end of ml_umask */

/* ml_chmod : (fd or string * int) -> unit
 * Change the protection mode of the specified file.
 */
void ml_chmod (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac has no access protection */
    RETURN(msp, 0)
#else
    ML_val_t	    f = REC_SEL(arg, 0);
    int		    mode = REC_SELINT(arg, 1);
    int		    sts;

    if (OBJ_isBOXED(f))
	sts = chmod((char *)PTR_MLtoC(f), mode);
    else
	sts = fchmod(INT_MLtoC(f), mode);

    CHK_RETURN(msp, sts);
#endif
} /* end of ml_chmod */


/* ml_access : (string * int list) -> bool
 * Check to see if the user has the specified access to the specified file.
 * NOTE: In the long run, there should be a datatype for the return value
 * of access, since it can fail for significantly different reasons
 * (e.g., no such file, not a directory, looping path, ...). -- JHR
 */
void ml_access (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac has no access protection */
    RETURN(msp, ML_true)
#else
    char	    *path = (char *)REC_SELPTR(arg, 0);
    register ML_val_t p = REC_SEL(arg, 1);
    int		    mode = F_OK;

    for (;  p != ML_nil;  p = ML_tl(p)) {
        switch (INT_MLtoC(ML_hd(p))) {
	  case 0: mode |= R_OK; break;
	  case 1: mode |= W_OK; break;
	  case 2: mode |= X_OK; break;
	  default:
	    raise_syserror (msp, "unknown access mode");
	    return;
	} /* end of switch */
    } /* end of for */

    if (access(path, mode) == 0)
	RETURN(msp, ML_true)
    else
	RETURN(msp, ML_false)
#endif
} /* end of ml_access. */

#ifdef THINK_C
struct stat {
	int foo;
};
static int ___lstat (char *s, struct stat *buf)
	{
}
static int ____fstat (int fd,  struct stat *buf)
	{
}
#endif
#ifndef THINK_C
/* stat_file:
 * Get the file status of f.  The file can either be specified as a path, in which
 * case f will be a boxed ML string, otherwise f will be an unboxed file descriptor.
 */
static int stat_file (msp, f, buf)
    MLState_ptr     msp;
    ML_val_t	    f;
    struct stat	    *buf;
{
    int		    sts;

    if (OBJ_isBOXED(f))
	sts = lstat((char *)PTR_MLtoC(f), buf);
    else
	sts = fstat(INT_MLtoC(f), buf);

    if (sts == -1)
	raise_syserror (msp, 0);

    return sts;

} /* end of stat_file */
#endif

/* ml_getfid : (fd or string) -> fileid
 * Return the unique file id (a string created from the device and inode of the
 * file) of the specified file.
 */
void ml_getfid (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct stat	    buf;
    struct { dev_t dev; ino_t ino; } id_buf;
    ML_val_t	    p;

    if (stat_file(msp, f, &buf) == 0) {
	register int	sz = (sizeof(id_buf)+3) & ~3;

	ML_alloc_write (msp, 0, MAKE_DESC(sz, TAG_string));
	p = ML_alloc (msp, sz >> 2);

	bzero ((char *)&id_buf, sz);
	id_buf.dev = buf.st_dev;
	id_buf.ino = buf.st_ino;
	bcopy ((char *)&id_buf, (char *)PTR_MLtoC(p), sz);

	RETURN(msp, p);
    }
#endif
} /* end of ml_getfid */

/* ml_getmod : (fd or string) -> int
 * Return the file protection mode of the file specified by f.
 */
void ml_getmod (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac has no access protection. */
    RETURN(msp, INT_CtoML(0777));
#else
    struct stat	    buf;

    if (stat_file(msp, f, &buf) == 0) {
	RETURN(msp, INT_CtoML(buf.st_mode & 0777));
    }
#endif
} /* end of ml_getmod */

/* ml_ftype : (fd or string) -> int
 * Return the file type of the file specified by f.  The return values must
 * track those in System.Unsafe.FileIO (see "boot/perv.sml").
 */
void ml_ftype (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct stat	    buf;
    register ML_val_t typ;

    if (stat_file(msp, f, &buf) == 0) {
	switch (buf.st_mode & S_IFMT) {
	  case S_IFREG: typ = INT_CtoML(0); break;
	  case S_IFDIR: typ = INT_CtoML(1); break;
	  case S_IFLNK: typ = INT_CtoML(2); break;
	  case S_IFSOCK: typ = INT_CtoML(3); break;
	  case S_IFCHR: typ = INT_CtoML(4); break;
	  case S_IFBLK: typ = INT_CtoML(5); break;
	  default:
	    raise_syserror(msp, "unknown file type");
	    return;
	}
	RETURN(msp, typ);
    }
#endif
} /* end of ml_ftype */

/* ml_getownid : (fd or string) -> (int * int)
 * Return the user and group ids of the specified file.
 */
void ml_getownid (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac has no file owners. */
    ML_val_t	    obj;
    REC_ALLOC2 (msp, obj, INT_CtoML(0), INT_CtoML(0));
    RETURN(msp, obj);
#else
    struct stat	    buf;
    ML_val_t	    obj;

    if (stat_file(msp, f, &buf) == 0) {
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_uid), INT_CtoML(buf.st_gid));
	RETURN(msp, obj);
    }
#endif
} /* end of ml_getownid */

/* ml_fsize : (fd or string) -> int
 * Return the size in bytes of the specified file.
 */
void ml_fsize (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac has no stat. */
    int	pos, len, fd;
    extern int	    overflow_e0[];

    if (OBJ_isBOXED(f))	{
    	fd = open((char *)PTR_MLtoC(f), O_RDONLY);
    	if (fd < 0)	{
	    raise_syserror (msp, 0);
	    return;
	}
	len = lseek(fd, 0L, SEEK_END);
	close(fd);
    }
    else {
    	fd = INT_MLtoC(f);
	pos = tell(fd);				/* get current position */
	if(pos == EOF)
	    len = pos;				/* error */
	else	{
	    len = lseek(fd, 0, SEEK_END);	/* get file size (inefficiently) */
	    pos = lseek(fd, pos, SEEK_SET);	/* recover position */
	}
    }
    /* get the file length */
    if (len == EOF)
	raise_syserror (msp, 0);
    else	{
	if ((len & 0xC0000000) != 0) {
	    msp->fault_exn = PTR_CtoML(overflow_e0+1);
	    raise_ml_exn (msp);
	}
	else
	    RETURN(msp, INT_CtoML(len))
    }
#else
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	if ((buf.st_size & 0xC0000000) != 0) {
	    msp->fault_exn = PTR_CtoML(overflow_e0+1);
	    raise_ml_exn (msp);
	}
	else
	    RETURN(msp, INT_CtoML(buf.st_size))
    }
#endif
} /* end of ml_fsize */

/* ml_atime : (fd or string) -> (int * int)
 * Get the most recent access time of the specified file.
 */
void ml_atime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct stat	    buf;

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_atime), INT_CtoML(0));
	RETURN(msp, obj);
    }
#endif
} /* end of ml_atime */

/* ml_ctime : (fd or string) -> (int * int)
 * Get the creation time of the specified file.
 */
void ml_ctime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_ctime), INT_CtoML(0));
	RETURN(msp, obj);
    }
#endif
} /* end of ml_ctime */

/* ml_mtime : (fd or string) -> (int * int)
 * Get the most recent modification time of the specified file.
 */
void ml_mtime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_mtime), INT_CtoML(0));
	RETURN(msp, obj);
    }
#endif
} /* end of ml_mtime */


/* ml_isatty : int -> bool
 * Return true if the file descriptor fd refers to a tty device.
 */
void ml_isatty (msp, fd)
    MLState_ptr     msp;
    ML_val_t	    fd;
{
    RETURN(msp, isatty(INT_MLtoC(fd)) ? ML_true : ML_false);

} /* end of ml_isatty */


#ifndef THINK_C
/* fd_list2set:
 * Map a ML list of file descriptors to a fd_set.
 */
static fd_set *fd_list2set (fdl, fds, width)
    ML_val_t	    fdl;
    fd_set	    *fds;
    int		    *width;
{
    register int    fd, maxfd = -1;

    FD_ZERO(fds);
    while (fdl != ML_nil) {
	fd = INT_MLtoC(ML_hd(fdl));
	if (fd > maxfd)
	    maxfd = fd;
	FD_SET (fd, fds);
	fdl = ML_tl(fdl);
    }

    if (maxfd >= 0) {
	if (maxfd >= *width)
	    *width = maxfd+1;
	return fds;
    }
    else
	return (fd_set *)0;
}
#endif

#ifndef THINK_C
/* fd_set2list:
 * Map a fd_set to a ML list of ready file descriptors.
 */
static ML_val_t fd_set2list (msp, fds, width)
    MLState_ptr     msp;
    register fd_set *fds;
    register int    width;
{
    register ML_val_t p;
    register int    i;

    if (fds == 0)
	return ML_nil;

    for (i = 0, p = ML_nil;  i < width;  i++) {
	if (FD_ISSET(i, fds))
	    p = ML_cons (msp, INT_CtoML(i), p);
    }

    return p;
}
#endif

/* ml_select : (int list * int list * int list * (int * int))
 *                 -> (int list * int list * int list)
 * Check file descriptors for the readiness of I/O operations.
 */
void ml_select (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    ML_val_t	    rl = REC_SEL(arg, 0);
    ML_val_t	    wl = REC_SEL(arg, 1);
    ML_val_t	    el = REC_SEL(arg, 2);
    ML_val_t	    timeout = REC_SEL(arg, 3);
    fd_set	    rset, wset, eset;
    fd_set	    *rfds, *wfds, *efds;
    int		    width = 0, sts;
    struct timeval  t, *tp;

    rfds = fd_list2set (rl, &rset, &width);
    wfds = fd_list2set (wl, &wset, &width);
    efds = fd_list2set (el, &eset, &width);

    if (OBJ_isBOXED(timeout)) {
	t.tv_sec = REC_SELINT(timeout, 0);
	t.tv_usec = REC_SELINT(timeout, 1);
	tp = &t;
    }
    else
	tp = 0;

    if (msp->inSigHandler || msp->maskSignals
#ifdef SOLARIS
    || ((! sigsetjmp (msp->SysCallEnv,0)) && 
#else
    || ((! _setjmp (msp->SysCallEnv)) && 
#endif
	(((msp->ioWaitFlag = 1), (msp->NumPendingSigs == 0))))) {
#ifdef RISCos
	/* problem with select and pipes */
	sts = 0;
#else
	DO_SYSCALL (select (width, rfds, wfds, efds, tp), sts);
#endif
	msp->ioWaitFlag = 0;
    }
    else {
	backup_kont(msp);
#ifdef SOLARIS
	sigprocmask(SIG_SETMASK,&emptyMask,0);
#else
	sigsetmask (0);  /* re-enable signals */
#endif
	return;
    }

    if (sts == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t	    rfdl, wfdl, efdl, res;

	if (sts == 0)
	    rfdl = wfdl = efdl = ML_nil;
	else {
	    rfdl = fd_set2list (msp, rfds, width);
	    wfdl = fd_set2list (msp, wfds, width);
	    efdl = fd_set2list (msp, efds, width);
	}
	REC_ALLOC3 (msp, res, rfdl, wfdl, efdl);
	RETURN(msp, res);
    }
#endif
} /* end of ml_select */


/* ml_pipe : unit -> (int * int)
 * Create a pipe and return its input and output descriptors.
 */
void ml_pipe (msp)
    MLState_ptr     msp;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    int		fds[2];

    if (pipe(fds) == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(fds[0]), INT_CtoML(fds[1]));
	RETURN(msp, obj);
    }
#endif
} /* end of ml_pipe. */


/* ml_fionread : int -> int
 * Return the number of bytes available for reading in the given file.
 */
void ml_fionread (msp, arg)
     MLState_ptr     msp;
    int		arg;
{
#ifdef THINK_C
    int		fd = INT_MLtoC(arg);
    int		sz;
    
    sz = fionread(fd);
    if (sz < 0)
	raise_syserror (msp, 0);
    else
	RETURN(msp, INT_CtoML(sz));
#else
#ifdef SOLARIS
    int		    fd = INT_MLtoC(arg);
    off_t           cnt;
    int             ret;
    struct stat     sbuf;

    DO_SYSCALL(isastream(fd),ret);
    if (ret > 0) {
        DO_SYSCALL(ioctl(fd, FIONREAD, &cnt),ret);
        if (ret < 0)
            raise_syserror (msp, 0);
        else
	    RETURN(msp, INT_CtoML(cnt));
    }
    else if (ret == 0) {
        DO_SYSCALL(lseek(fd, 0L, SEEK_CUR), cnt);
        if (cnt >= 0) {
            DO_SYSCALL(fstat (fd, &sbuf),ret);
            if (ret < 0) 
              raise_syserror (msp, 0);
            else 
              RETURN(msp, INT_CtoML(sbuf.st_size - cnt));
        }
        else
            raise_syserror (msp, 0);
    }
    else
        raise_syserror (msp, 0);
#else
    int		fd = INT_MLtoC(arg);
    struct stat	buf;
    int		pos;
    int		count[2], r;

#  ifdef HPUX
    if (isatty(fd)) {		/* FIONREAD unsupported on tty's on 6.5 */
	fd_set		readfds;
	int		nfound;
	struct timeval	timeout;

	FD_SET(fd, &readfds);
	timeout.tv_sec = 0;  timeout.tv_usec = 0;
	nfound = select(fd+1, &readfds, 0, 0, &timeout);
	CHK_RETURN(msp, nfound);
    } /* if */
#  else
    if (ioctl(fd, FIONREAD, count) >= 0)
	RETURN(msp, INT_CtoML(count[0]));
#  endif HPUX

    if ((fstat(fd, &buf) < 0) || ((pos = lseek(fd, 0, L_INCR)) < 0))
	raise_syserror (msp, 0);
    else
	RETURN(msp, INT_CtoML(buf.st_size - pos));
#endif
#endif
} /* end of ml_fionread */


/* ml_system : string -> int
 * Issue the given shell command and return the exit status.
 */
void ml_system (msp, arg)
    MLState_ptr     msp;
    ML_val_t	arg;
{
    int		sts;

    sts = system ((char *)PTR_MLtoC(arg));
    CHK_RETURN (msp, sts);

} /* end of ml_system */


/* ml_exec : (string * string list * string list) -> (int * int)
 * Fork a process to execute the given command, with the given command-line
 * arguments and environment.  Return the file descriptors for pipes
 * connected to the process' stdin and stdout. The arg list should be
 * non-empty with the command name as the first element.
 */
void ml_exec (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    char	    *cmd = (char *)REC_SELPTR(arg, 0);
    ML_val_t	    arglst = REC_SEL(arg, 1);
    ML_val_t	    envlst = REC_SEL(arg, 2);
    int		    p1[2], p2[2];
    ML_val_t	    res;

  /* reap any dead child processes (this is to avoid overflowing the
   * process table with zombies).
   */
#if (defined(RISCos) || defined(NeXT) || defined(DYNIX) || defined(MACH) || defined(sony_news))
   /* Some vendors don't know about POSIX */
    {
	union wait status;
	while (wait3 (&status, WNOHANG, 0) > 0)
	    continue;
    }
#else
    while (waitpid (-1, 0, WNOHANG) > 0)
	continue;
#endif

    if ((pipe(p1) < 0) || (pipe(p2) < 0))
	raise_syserror (msp, 0);
#if defined(V9) || defined(SGI) || defined(AUX)
    else if (fork()) {
#else
    else if (vfork()) {
#endif
	close(p1[0]); close(p2[1]);
	REC_ALLOC2 (msp, res, INT_CtoML(p2[0]), INT_CtoML(p1[1]));
	RETURN(msp, res);
    }
    else {
	char		**argv, **envp;
	register ML_val_t p;
	register char   **cp;

      /* use the heap for temp space for the argv[] and envp[] vectors */
	argv = cp = (char **)(msp->ml_allocptr);
	for (p = arglst;  p != ML_nil;  p = ML_tl(p))
	    *cp++ = (char *)PTR_MLtoC(ML_hd(p));
	*cp++ = 0;  /* terminate the argv[] */
	envp = cp;
	for (p = envlst;  p != ML_nil;  p = ML_tl(p))
	    *cp++ = (char *)PTR_MLtoC(ML_hd(p));
	*cp++ = 0;  /* terminate the envp[] */

	close (p1[1]); close (p2[0]);
	dup2 (p1[0], 0); dup2 (p2[1], 1);
	execve(cmd, argv, envp);
	_exit(1);
    }
#endif
} /* end of ml_exec */

/* ml_exit : int -> 'a
 */
void ml_exit (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    mp_shutdown (msp, INT_MLtoC(arg));

} /* end of ml_exit */

extern ML_val_t make_str_list();

/* ml_argv : unit -> string list
 * Return the command-line argument list.
 */
void ml_argv (msp)
    MLState_ptr     msp;
{
    extern char **global_argv;

    RETURN(msp, make_str_list (msp, global_argv));

} /* end of ml_argv */

/* ml_envrion : unit -> string list
 * Return the environment list.
 */
void ml_environ (msp)
    MLState_ptr     msp;
{
#ifdef THINK_C
    /* Dummy Mac version */
    RETURN(msp, ML_nil);
#else
    extern char **environ;

    RETURN(msp, make_str_list (msp, environ));
#endif
} /* end of ml_environ */

/* ml_getpid : unit -> int
 */
void ml_getpid (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    RETURN (msp, INT_CtoML(getpid()));

} /* end of ml_getpid */

/* ml_getuid : unit -> int
 */
void ml_getuid (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    RETURN (msp, INT_CtoML(getuid()));

} /* end of ml_getuid */

/* ml_getgid : unit -> int
 */
void ml_getgid (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    RETURN (msp, INT_CtoML(getgid()));

} /* end of ml_getgid */


#ifdef SOLARIS
#include <sys/systeminfo.h>
#define BUFLEN 257
#endif

/* ml_gethostname : unit -> string
 * Return the name of our host.
 */
void ml_gethostname (msp)
    MLState_ptr     msp;
{
#ifdef THINK_C
    /* Mac version not done yet */
    ML_val_t name;
    name = ML_alloc_string (msp, "Mac");
    RETURN(msp, name);
#else
#ifdef SOLARIS
    char	buf[BUFLEN];
    int         len;

    DO_SYSCALL(sysinfo(SI_HOSTNAME, buf, BUFLEN),len);
    if (len >= 0) {
	ML_val_t name;
	name = ML_alloc_string (msp, buf);
	RETURN(msp, name);
    }
    else
	raise_syserror (msp, 0);
#else
    char	buf[64];

    if (gethostname(buf, 64) == 0) {
	ML_val_t name;
	buf[63] = '\0';  /* insure null termination */
	name = ML_alloc_string (msp, buf);
	RETURN(msp, name);
    }
    else
	raise_syserror (msp, 0);
#endif
#endif
} /* end of ml_gethostname */

/* ml_gethostid : unit -> string
 * Return the id of our host.
 */
#ifdef HPUX
void ml_gethostid (msp)
    MLState_ptr     msp;
{
    char        buf[SNLEN+1];
    ML_val_t    name;
    struct utsname utsname;
    uname(&utsname);
    bcopy ((char *)(utsname.idnumber), buf, SNLEN);
    buf[SNLEN] = '\0';  /* insure null termination */
    name = ML_alloc_string (msp, buf);
    RETURN(msp, name);
}
#else /* !HPUX */
#ifdef SOLARIS
void ml_gethostid (msp)
    MLState_ptr     msp;
{
    char        buf[BUFLEN];
    int         len;
    ML_val_t    name;

    DO_SYSCALL(sysinfo(SI_HW_SERIAL, buf, BUFLEN), len);
    if (len >= 0) {
        name = ML_alloc_string (msp, buf);
        RETURN(msp, name);
    }
    else
	raise_syserror (msp, 0);

} /* end of ml_gethostid */
#else /* !HPUX !SOLARIS */
void ml_gethostid (msp)
    MLState_ptr     msp;
{
    long	hostid;
    char        buf[sizeof(long)+1];
    ML_val_t    name;

#ifdef THINK_C
    /* Mac version not done yet */
    name = ML_alloc_string (msp, "357");
#else
    hostid = gethostid();
    bcopy ((char *)&hostid, buf, sizeof(long));
    buf[sizeof(long)] = '\0';  /* insure null termination */
    name = ML_alloc_string (msp, buf);
#endif
    RETURN(msp, name);

} /* end of ml_gethostid */
#endif
#endif

static int	blast_fd;	/* the file descriptor to blast to */

/* ml_blast_out : (int * 'a) -> 'a
 */
void ml_blast_out (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    blast_fd	    = REC_SELINT(arg, 0);
    /* blast_write doesn't work naturally on unboxed things.
       The hack to fix it is to blast out the tuple (fd,value)
       even though there's no need to save the fd; then blast_read
       can select out the value. 
    */
    msp->ml_arg = arg;
 /*   msp->mask = CONT_ARGS_MASK; shouldn't be necessary */
    callgc0 (msp, CAUSE_BLAST, 0);

} /* end of ml_blast_out */

/* blast_write:
 */
void blast_write (msp, start, end, ptr)
    MLState_ptr msp;
    int		start, end, ptr;
{
/*This version is to be used with a different version of 
  the ML code: that reads one word, uses that to determine
  how much of the rest to read.  (Current ML code just reads the
  entire file, making it impossible to blast_in two things in a row).
    int		hdr[4];

    hdr[0] = (int)(INT_CtoML(end-start+3*sizeof(int)));
    hdr[1] = start;
    hdr[2] = end;
    hdr[3] = ptr - start;
*/
    int		hdr[3];

    hdr[0] = start;
    hdr[1] = end;
    hdr[2] = ptr - start;

    if (bulletproofWrite0 (blast_fd, hdr, sizeof(hdr)) 
    || bulletproofWrite0 (blast_fd, start, end-start))
         raise_syserror (msp, 0);
    else
	RETURN(msp, INT_CtoML(end-start+3*sizeof(int)));

} /* end of blast_write */


/* ml_blast_in : string -> 'a
 * Build an object from the string.  The string has a special header (produced
 * by blast_write).
 */
void ml_blast_in (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    *obj = PTR_MLtoC(arg);
    int		    start = obj[0];
    int		    end = obj[1];
    int		    offset = obj[2];
    int		    *words = (obj + 3);

    (PTR_MLtoC(arg))[-1] = MAKE_DESC(12, TAG_string);
    relocate (start, end, words);
    RETURN(msp, REC_SEL(PTR_CtoML((int)words + offset),1));

} /* end of blast_in */


/* ml_export : int -> bool
 * Export the world to the given file and return false (the exported version
 * returns true).
 */
void ml_export (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg);
    register int    i;
    extern int	    isExported;
    extern MLState_ptr Exporters_State;
#if (MAX_PROCS > 1)
    extern void     check_suspended();

    check_suspended(msp);
#endif

  /* shed the unecessary stuff */
    /* msp->mask = CONT_ARGS_MASK; shouldn't be necessary */
    callgc0(msp, CAUSE_EXPORT, 0);
    callgc0(msp, CAUSE_EXPORT, 0);

  /* export */
    isExported = 1;
    Exporters_State = msp;
    i = export (fd);
    isExported = 0;
    Exporters_State = (MLState_ptr)0;
    if (i) raise_syserror (msp, 0);
    else RETURN(msp, ML_false);


} /* end of ml_export */


/* ml_gettime : unit -> (int * int * int * int * int * int)
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
void ml_gettime (msp)
    MLState_ptr     msp;
{
    ML_val_t	res;
    extern ML_val_t	t_sec, t_usec, s_sec, s_usec, g_sec, g_usec;

    timer();
    REC_ALLOC6 (msp, res, t_sec, t_usec, s_sec, s_usec, g_sec, g_usec);
    RETURN(msp, res);

} /* end of ml_gettime */


/* ml_timeofday : unit -> (int * int)
 * Return the time of day.
 */
void ml_timeofday (msp)    
     MLState_ptr     msp;
{
    struct timeval	t;
    ML_val_t		res;

    gettimeofday (&t, 0);
    REC_ALLOC2 (msp, res, INT_CtoML(t.tv_sec), INT_CtoML(t.tv_usec));
    RETURN(msp, res);

} /* end of ml_timeofday. */


/* ml_setitimer : (int * int * int * int * int) -> int
 * Set an interval timer; the first argument specifies which timer.
 */
void ml_setitimer (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    struct itimerval itv;
    register int    which;

    itv.it_interval.tv_sec  = REC_SELINT(arg, 1);
    itv.it_interval.tv_usec = REC_SELINT(arg, 2);
    itv.it_value.tv_sec     = REC_SELINT(arg, 3);
    itv.it_value.tv_usec    = REC_SELINT(arg, 4);

    switch (REC_SELINT(arg, 0)) {
      case 0: which = ITIMER_REAL; break;
      case 1: which = ITIMER_VIRTUAL; break;
      case 2: which = ITIMER_PROF; break;
    }

    if (setitimer (which, &itv, 0) == -1) {
        raise_syserror(msp, 0);
        return;
    }
    else
        RETURN(msp, ML_unit);
#endif
} /* end of ml_setitimer */


/* ml_setglobal : int array -> unit
 */
void ml_setglobal (msp, p)
    MLState_ptr     msp;
    ML_val_t	    p;
{
#ifdef GLOBAL_INDX
    msp->ml_globalptr = p;
#endif
    RETURN(msp, ML_unit);

} /* end of ml_setglobal */


/* ml_mkcode : string -> (code_string * (unit -> unit))
 *
 * Turn a string into a code-string, and a bootable closure.  For the time
 * being, this just means flushing the I-cache and building a trivial closure,
 * but code will eventually live in its own space.
 */
void ml_mkcode (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		begin = (int)PTR_MLtoC(arg);
    int		len = OBJ_LEN(arg) + 4;
    ML_val_t	closure, res;

    FlushICache (begin, len);

    REC_ALLOC1(msp, closure, PTR_CtoML((ML_val_t *)PTR_MLtoC(arg) + 1));
    REC_ALLOC2(msp, res, arg, closure);

    RETURN(msp, res);

} /* end of ml_mkcode */

/* ml_gc : int -> unit
 * Force a garbage collection of the specified level (0 == minor, 1 == major).
 */
void ml_gc (msp, level)
    MLState_ptr     msp;
    int		    level;
{
    msp->ml_arg = ML_unit;

    switch (INT_MLtoC(level)) {
      /* msp->mask = CONT_ARGS_MASK; shouldn't be necessary */
      case 0: callgc0 (msp, CAUSE_MINOR, 0); break;
      default: callgc0 (msp, CAUSE_MAJOR, 0); break;
    }

} /* end of ml_gc */

/* ml_enablesig : (int * bool) -> unit
 * This function is called by ML code to enable/disable a given signal.  If the
 * second argument is true, the the signal is enabled, otherwise disabled.
 */
void ml_enablesig (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    enable_sig (REC_SELINT(arg, 0), (REC_SEL(arg, 1) == ML_true));
    RETURN(msp, ML_unit);

} /* end of ml_enablesig. */

/* ml_masksigs : bool -> unit
 * Turn the masking of signals on and off.
 */
void ml_masksigs (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    msp->maskSignals = (arg == ML_true);
    RETURN (msp, ML_unit);

} /* end of ml_masksigs */

/* ml_sigpause : unit -> unit
 * Pause until the next signal.  Note, this must not restart the system call
 * on EINTR.
 */
void ml_sigpause (msp)
    MLState_ptr     msp;
{
#ifdef THINK_C
    /* Mac version not done yet */
    raise_ThinkC_error(msp);
#else
    sigpause (0);
    RETURN(msp, ML_unit);
#endif
} /* end of ml_sigpause. */

#ifdef GETSTORELIST
/* ml_getstorelist : bool -> storelist
 */
void ml_getstorelist (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		   i;
    ML_val_t	    res;
    extern int	    preserving, store_preserve;
    extern ML_val_t uniq();

    /* msp->mask = CONT_ARGS_MASK; shouldn't be necessary */
    callgc0 (msp, CAUSE_STORE, 0);
    preserving = (arg != ML_false);
    res = uniq(store_preserve);
    store_preserve = (int)STORLST_nil;
    RETURN(msp, res);

} /* end of ml_getstorelist */
#endif


int icountM;

void ml_geticount (msp)
    MLState_ptr     msp;
{   int x;
    ML_val_t		res;
#ifdef ICOUNT
    x=(int)(msp->ml_icount);
    msp->ml_icount=0;
#else
    x=0;
#endif
    REC_ALLOC2 (msp, res, INT_CtoML(icountM), INT_CtoML(x));
    icountM=0;
    RETURN(msp, res);

}

/** The C function table **/

struct table_t {
    int		    tag;
    ML_val_t	    func;
    ML_val_t	    name;
    ML_val_t	    next;
    int		    stag;
    char	    str[16];
};

#define FUNCTION(ff,nn)				\
     {MAKE_DESC(3,TAG_record),			\
     PTR_CtoML(ff),				\
     0, /* fill in later */			\
     0, /* fill in later */			\
     MAKE_DESC(sizeof(nn)-1,TAG_string),	\
     nn}

struct table_t externlist0[] =
    {
/*                                  "xxxxxxxxxxxxxxxx" MAX NAME LENGTH (16) */
	FUNCTION (ml_syscall,	    "syscall"),
	FUNCTION (ml_openf,	    "openf"),
	FUNCTION (ml_closef,	    "closef"),
	FUNCTION (ml_connect_unix,  "connect_unix"),
	FUNCTION (ml_connect_inet,  "connect_inet"),
	FUNCTION (ml_link,	    "link"),
	FUNCTION (ml_unlink,	    "unlink"),
	FUNCTION (ml_dup,	    "dup"),
	FUNCTION (ml_wait_for_in,   "wait_for_in"),
	FUNCTION (ml_read,	    "read"),
	FUNCTION (ml_readi,	    "readi"),
	FUNCTION (ml_write,	    "write"),
	FUNCTION (ml_writei,	    "writei"),
	FUNCTION (ml_writev,	    "writev"),
	FUNCTION (ml_lseek,	    "lseek"),
	FUNCTION (ml_send_obd,	    "send_obd"),
	FUNCTION (ml_getdirent,	    "getdirent"),
	FUNCTION (ml_chdir,	    "chdir"),
	FUNCTION (ml_mkdir,	    "mkdir"),
	FUNCTION (ml_readlink,	    "readlink"),
	FUNCTION (ml_truncate,	    "truncate"),
	FUNCTION (ml_umask,	    "umask"),
	FUNCTION (ml_chmod,	    "chmod"),
	FUNCTION (ml_access,	    "access"),
	FUNCTION (ml_getfid,	    "getfid"),
	FUNCTION (ml_getmod,	    "getmod"),
	FUNCTION (ml_ftype,	    "ftype"),
	FUNCTION (ml_getownid,	    "getownid"),
	FUNCTION (ml_fsize,	    "fsize"),
	FUNCTION (ml_atime,	    "atime"),
	FUNCTION (ml_ctime,	    "ctime"),
	FUNCTION (ml_mtime,	    "mtime"),
	FUNCTION (ml_isatty,	    "isatty"),
	FUNCTION (ml_select,	    "select"),
	FUNCTION (ml_pipe,	    "pipe"),
	FUNCTION (ml_fionread,	    "fionread"),
	FUNCTION (ml_system,	    "system"),
	FUNCTION (ml_exec,	    "exec"),
	FUNCTION (ml_exit,	    "exit"),
	FUNCTION (ml_argv,	    "argv"),
	FUNCTION (ml_environ,	    "environ"),
	FUNCTION (ml_getpid,	    "getpid"),
	FUNCTION (ml_getuid,	    "getuid"),
	FUNCTION (ml_getgid,	    "getgid"),
	FUNCTION (ml_gethostname,   "gethostname"),
	FUNCTION (ml_gethostid,     "gethostid"),
	FUNCTION (ml_blast_out,	    "blas"),
	FUNCTION (ml_blast_in,	    "salb"),
	FUNCTION (ml_export,	    "export"),
	FUNCTION (ml_gettime,	    "gettime"),
	FUNCTION (ml_timeofday,	    "timeofday"),
	FUNCTION (ml_setitimer,	    "setitimer"),
	FUNCTION (ml_setglobal,	    "setg"),
	FUNCTION (ml_mkcode,	    "mkcode"),
	FUNCTION (ml_gc,	    "gc"),
	FUNCTION (ml_enablesig,	    "enablesig"),
	FUNCTION (ml_masksigs,	    "masksigs"),
	FUNCTION (ml_sigpause,	    "sigpause"),
	FUNCTION (ml_geticount,     "geticount"),
	FUNCTION (ml_acquire_proc,  "acquire_proc"),
	FUNCTION (ml_release_proc,  "release_proc"),
	FUNCTION (ml_spin_lock,     "spin_lock"),
        FUNCTION (ml_max_procs,     "max_procs"),
#ifdef GETSTORELIST
	FUNCTION (ml_getstorelist,  "getstorelist"),
#endif
/*                                  "xxxxxxxxxxxxxxxx" MAX NAME LENGTH (16) */
    };
#define NEXTERNS	(sizeof(externlist0)/sizeof(struct table_t))

/* init_externlist:
 * Initialize the extern list.
 */
void init_externlist ()
{
    int		    i;
    struct table_t  *p = (struct table_t *)INT_CtoML(0);

    for (i = NEXTERNS;  --i >= 0; ) {
	externlist0[i].next = PTR_CtoML(p);
	externlist0[i].name = PTR_CtoML(externlist0[i].str);
	p = (struct table_t *)&(externlist0[i].func);
    }

} /* end of init_externlist */

#ifdef THINK_C
int nexterns = NEXTERNS;
int old_extern[NEXTERNS];
#endif

#ifdef SOLARIS
#include <limits.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <sys/stropts.h>
#include <sys/termios.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/stream.h>
#include <sys/tiuser.h>
#include <sys/tihdr.h>
#include <sys/timod.h>
#include <sys/sockmod.h>
#include <sys/un.h>
#include <netinet/in.h>

static char *getfield (sp)
  char **sp;
{
  char *bp = *sp, *p, *q;
  while(isspace(*bp)) bp++;
  if (*bp == '\0') return 0;
  p = q = bp;
  while (isgraph(*p)) {
    if ((*p == '\\') && (*(p+1) != '\0')) p++;
    *q++ = *p++;
  }
  if (*p != '\0') p++;
  *q = '\0';
  *sp = p;
  return (bp);
}

struct fillbuf {
  int  fd;
  char *ptr;
  char *eptr;
  char *data;
};

static int fillb (fp)
  struct fillbuf *fp;
{
  int ret;

  DO_SYSCALL(read(fp->fd,fp->data,BUFSIZ), ret);
  if (ret >= 0) {
    fp->ptr = fp->data;
    fp->eptr = fp->ptr + ret;
  }
  return (ret);
}

static int getline(buf, fp)
  char *buf;
  struct fillbuf *fp;
{
  char *op = buf;
  char *ip;
  int ret;

  while (1) {
    ip = fp->ptr;
    while (ip < fp->eptr) {
      if (*ip == '\n') {
        *op++ = *ip++;
        *op = '\0';
        fp->ptr = ip;
        return(op - buf);
      }
      *op++ = *ip++;
    }
    ret = fillb(fp);
    if(ret == 0) {
      *op = '\0';
      return(op - buf);
    }
    else if (ret < 0)
      return ret;
  }
}

static int errclose(fd)
  int fd;
{
  int errsave;
  int sts;

  errsave = errno;
  DO_SYSCALL(close(fd), sts);
  errno = errsave;
  return (-1);
}

static int getdev (kind, path)
  char *kind, *path;
{
  int  fd;
  char fbuf[BUFSIZ];
  char buf[BUFSIZ];
  int c, sts;
  char *p, *rest;
  struct fillbuf fil;

  DO_SYSCALL(open ("/etc/netconfig", O_RDONLY), fd);
  if (fd < 0) return(-1);

  fil.fd = fd;
  fil.data = fbuf;
  fil.eptr = fil.ptr = 0;
  while((sts = getline(buf, &fil)) > 0) { 
    if (((c = buf[0]) == '#') || (c == "\n") || (c == '\0')) continue;
    rest = buf;
    p = getfield(&rest);  /* get and test netid */
    if (p == NULL) continue;
    if (strcmp(p,kind)) continue;
    p = getfield(&rest);  /* eat semantics */
    if (p == NULL) continue;
    p = getfield(&rest);  /* eat flags */
    if (p == NULL) continue;
    p = getfield(&rest);  /* eat protofmly */
    if (p == NULL) continue;
    p = getfield(&rest);  /* eat proto */
    if (p == NULL) continue;
    p = getfield(&rest);  /* get and store dev */
    if (p == NULL) continue;
    strcpy(path, p);
    DO_SYSCALL (close(fd), sts);
    return 0;
  }
  return (errclose(fd));
}

static int getsocket(devpath)
  char *devpath;
{
  int fd;
  int sts;
  int timeout = 0;
  sigset_t set, oset;
  struct strioctl strio;
  struct si_udata udata;

  DO_SYSCALL(open(devpath, O_RDWR, 0),fd);
  if (fd < 0) return (-1);
  DO_SYSCALL(ioctl(fd, I_FIND, "sockmod"),sts);
  if (sts == 0) {
    DO_SYSCALL(ioctl(fd, I_PUSH, "sockmod"),sts);
    if (sts < 0) return(errclose(fd));
  }
  DO_SYSCALL(ioctl(fd, I_SETCLTIME, &timeout),sts);
  if (sts < 0) return(errclose(fd));
  DO_SYSCALL(ioctl(fd, I_SWROPT, SNDPIPE),sts);
  if (sts < 0) return(errclose(fd));

  sigemptyset (&set);
  sigaddset (&set, 22);
  sigprocmask(SIG_BLOCK, &set, &oset);
  strio.ic_cmd = SI_GETUDATA;
  strio.ic_timout = INFTIM;
  strio.ic_len = sizeof(udata);
  strio.ic_dp = (char *)&udata;
  DO_SYSCALL(ioctl(fd, I_STR, &strio),sts);
  if (sts < 0) return(errclose(fd));
  sigprocmask(SIG_SETMASK, &oset, 0);

  return (fd);
}

static void chkerror (fp)
  union T_primitives *fp;
{
  extern int errno;

  switch (fp->type) {
    case T_DISCON_IND :
      errno = fp->discon_ind.DISCON_reason;
      break;
    case T_ERROR_ACK :
      errno = fp->error_ack.UNIX_error;
      break;
  }
}

static int connect_inet (hostname, port)
  char *hostname, *port;
{
  char devpath[PATH_MAX];
  int fd;
  int sts;
  int i, s;
  sigset_t set, oset;
  int getmsgflg;
  struct strbuf ctl, data;
  struct strioctl strio;
  union {
    struct {
      struct T_bind_req bind;
      struct sockaddr_in saddr; 
    } bindreq;
    struct {
      struct T_conn_req req;
      struct sockaddr_in saddr; 
    } connrqst;
    union T_primitives fill;
    char fill2[68];
  } msg;
  union {
    long align;
    char buf[BUFSIZ];
  } databuf;

  sts = getdev ("tcp",devpath);
  if (sts) return (-1);
  fd = getsocket(devpath);
  if (fd < 0) return (-1);

  sigemptyset (&set);
  sigaddset (&set, 22);
  sigprocmask(SIG_BLOCK, &set, &oset);
  msg.bindreq.bind.PRIM_type = T_BIND_REQ;
  msg.bindreq.bind.ADDR_length = 0;
  msg.bindreq.bind.ADDR_offset = 0;
  msg.bindreq.bind.CONIND_number = 0;
  strio.ic_cmd = TI_BIND;
  strio.ic_timout = INFTIM;
  strio.ic_len = sizeof(msg.bindreq);
  strio.ic_dp = (char *)&msg.bindreq;
  DO_SYSCALL(ioctl(fd, I_STR, &strio),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_BIND_ACK) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }

  msg.connrqst.saddr.sin_family = AF_INET;
  msg.connrqst.saddr.sin_port = htons(atoi(port));
  memset(msg.connrqst.saddr.sin_zero, 0, sizeof(msg.connrqst.saddr.sin_zero));
  s = i = 0;
  do {
      s = (s << 8) | atoi(hostname);
      while (*hostname && (*hostname != '.'))
          hostname++;
  } while (*hostname++ != '\0');
  msg.connrqst.saddr.sin_addr.s_addr = htonl(s);
  msg.connrqst.req.PRIM_type = T_CONN_REQ;
  msg.connrqst.req.DEST_length = sizeof(msg.connrqst.saddr);
  msg.connrqst.req.DEST_offset = (int)&msg.connrqst.saddr - (int)&msg;
  msg.connrqst.req.OPT_length = 0;
  msg.connrqst.req.OPT_offset = 0;
  ctl.maxlen = sizeof(msg);
  ctl.len = sizeof(msg.connrqst);
  ctl.buf = (char *)&msg;
  DO_SYSCALL(putmsg(fd, &ctl, 0, 0),sts);
  if (sts < 0) return (errclose(fd));

  ctl.len = 0;
  getmsgflg = RS_HIPRI;
  DO_SYSCALL(getmsg(fd, &ctl, 0, &getmsgflg),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_OK_ACK) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }

  ctl.len = 0;
  data.maxlen = sizeof(databuf);
  data.len = 0;
  data.buf = (char *)&databuf;
  getmsgflg = 0;
  DO_SYSCALL(getmsg(fd, &ctl, &data, &getmsgflg),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_CONN_CON) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }
  sigprocmask(SIG_SETMASK, &oset, 0);

  return (fd);
}

static int connect_unix (path)
  char *path;
{
  char devpath[PATH_MAX];
  int fd;
  int sts;
  int i, s;
  sigset_t set, oset;
  int getmsgflg;
  struct stat statbuf;
  struct strbuf ctl, data;
  struct strioctl strio;
  union {
    struct {
      struct T_bind_req bind;
      struct bind_ux saddr; 
    } bindreq;
    struct {
      struct T_conn_req req;
      struct bind_ux saddr; 
    } connrqst;
    union T_primitives fill;
    char fill2[4372];
  } msg;
  union {
    long align;
    char buf[BUFSIZ];
  } databuf;

  sts = getdev ("ticotsord",devpath);
  if (sts) return (-1);
  fd = getsocket(devpath);
  if (fd < 0) return (-1);

  DO_SYSCALL(stat(path,&statbuf),sts);
  if (sts < 0) return (errclose(fd));
  if (!S_ISFIFO(statbuf.st_mode)) {
    errno = ENOTSOCK;
    return (errclose(fd));
  }

  sigemptyset (&set);
  sigaddset (&set, 22);
  sigprocmask(SIG_BLOCK, &set, &oset);
  memset(&msg.bindreq.saddr,0,sizeof(msg.connrqst.saddr));
  msg.bindreq.saddr.name.sun_family = AF_UNIX;
  msg.bindreq.bind.PRIM_type = T_BIND_REQ;
  msg.bindreq.bind.ADDR_length = sizeof(msg.bindreq.saddr);
  msg.bindreq.bind.ADDR_offset = (int)&msg.bindreq.saddr - (int)&msg.bindreq;
  msg.bindreq.bind.CONIND_number = 0;
  strio.ic_cmd = TI_BIND;
  strio.ic_timout = INFTIM;
  strio.ic_len = sizeof(msg.bindreq);
  strio.ic_dp = (char *)&msg.bindreq;
  DO_SYSCALL(ioctl(fd, I_STR, &strio),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_BIND_ACK) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }

  memset(&msg.connrqst.saddr.name,0,sizeof(msg.connrqst.saddr.name));
  msg.connrqst.saddr.name.sun_family = AF_UNIX;
  strcpy (msg.connrqst.saddr.name.sun_path, path);
  msg.connrqst.saddr.ux_extaddr.size = sizeof(struct ux_dev);
  msg.connrqst.saddr.ux_extaddr.addr.tu_addr.dev = statbuf.st_dev;
  msg.connrqst.saddr.ux_extaddr.addr.tu_addr.ino = statbuf.st_ino;
  msg.connrqst.req.PRIM_type = T_CONN_REQ;
  msg.connrqst.req.DEST_length = sizeof(msg.connrqst.saddr);
  msg.connrqst.req.DEST_offset = (int)&msg.connrqst.saddr - (int)&msg;
  msg.connrqst.req.OPT_length = 0;
  msg.connrqst.req.OPT_offset = 0;
  ctl.maxlen = sizeof(msg);
  ctl.len = sizeof(msg.connrqst);
  ctl.buf = (char *)&msg;
  DO_SYSCALL(putmsg(fd, &ctl, 0, 0),sts);
  if (sts < 0) return (errclose(fd));

  ctl.len = 0;
  getmsgflg = RS_HIPRI;
  DO_SYSCALL(getmsg(fd, &ctl, 0, &getmsgflg),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_OK_ACK) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }

  ctl.len = 0;
  data.maxlen = sizeof(databuf);
  data.len = 0;
  data.buf = (char *)&databuf;
  getmsgflg = 0;
  DO_SYSCALL(getmsg(fd, &ctl, &data, &getmsgflg),sts);
  if (sts < 0) return (errclose(fd));
  if (msg.fill.type != T_CONN_CON) {
    chkerror(&msg.fill);
    return (errclose(fd));
  }
  sigprocmask(SIG_SETMASK, &oset, 0);

  return (fd);
}
#endif

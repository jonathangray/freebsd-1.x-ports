/*
 * lock.c -- deal with file locking on various architectures and UNIXs.
 * dot_lock() creates a file with the same name as the parameter passed
 * with the appendage ".lock" -- this is to be compatible with certain
 * systems that don't use flock or lockf or whatever they have available
 * that they don't use.
 */

#ifdef USG
#include <unistd.h>
#endif /* USG */
#include "mush.h"
#if defined(SYSV) && !defined(USG)
#include <sys/locking.h>
#endif /* SYSV && !USG */

#ifdef DOT_LOCK

#ifndef DOLOCK_PATH
#define DOLOCK_PATH	"dotlock"	/* Path to the executable */
#endif /* DOLOCK_PATH */
#ifndef DOLOCK_NAME
#define DOLOCK_NAME	"dotlock"	/* Name of the executable */
#endif /* DOLOCK_NAME */
#define DOLOCKIT	"-l"
#define UNLOCKIT	"-u"

#ifdef LOCK_PROG		/* Define for standalone locking program */

/* System V Release 2 does not support saved-set-group-id, so it is not
 * sufficient for mush to be setgid mail (or whatever group has write
 * permission on /usr/mail).  Instead, we need an external program that
 * can be setgid, which mush then runs to create and remove lock files.
 * Compiling this file with -DDOT_LOCK -DLOCK_PROG added to your CFLAGS
 * will generate such a program:
 *
 *	cc -o dotlock -DDOT_LOCK -DLOCK_PROG lock.c xcreat.c
 *
 * For mush purposes, you should hardwire the DOLOCK_PATH to the full path
 * name of the installed executable.  This helps prevent malicious users
 * from substituting a different program.
 *
 * The program generated can also be used to perform mail locking from
 * shell scripts, so you may wish to consider installing it (or a copy)
 * in a publicly accessible location, e.g. /usr/local/bin.  Note that
 * this program is not compatible with Xenix mail locking!
 */

#undef	on_intr()
#define	on_intr()	0
#undef	off_intr()
#define	off_intr()	0
#undef	print
#define	print		printf
#undef	print_more
#define	print_more	fflush(stdout), printf
#undef	error
#define	error		printf
#undef	sprintf

/* Simple dotlock program.  Exits 0 for success, nonzero for failure.
 *
 * Usage:
 *	dotlock -lock filename
 *	dotlock -unlock filename
 *
 * The options can be abbreviated to their first letter (-l or -u);
 * any other usage fails silently.
 */

main(argc, argv)
int argc;
char **argv;
{
    char *myname = rindex(argv[0], '/');

    if (!myname)
	myname = argv[0];
    if (strcmp(myname, DOLOCK_NAME) != 0 || argc != 3) 
	exit(-1);
    
    if (strncmp(argv[1], DOLOCKIT, 2) == 0)
	dot_lock(argv[2]);
    else if (strncmp(argv[1], UNLOCKIT, 2) == 0)
	dot_unlock(argv[2]);
    exit(-1);
}

#else /* !LOCK_PROG */

extern int sgid;
#ifdef BSD
extern int rgid;
#endif /* BSD */

#ifdef SVR2
/* No saved-setgid, so fork just long enough to create the lockfile. */
lock_proc(filename, how)
char *filename, *how;
{
    int kid, pid, status;

    errno = 0;
    switch (kid = fork()) {
	case 0:
	    execle(DOLOCK_PATH, DOLOCK_NAME, how, filename, NULL, environ);
	    return kid;
	case -1:
	    error("Unable to fork to change group id");
	    return -1;
	default:
	    /* For SYSV, we're not doing SIGCLD handling, so go ahead
	     * and reap everything in sight until we get the one we want.
	     */
	    while ((pid = wait(&status)) != -1 && pid != kid)
		;
	    if (pid == kid)
		errno = ((status & 0xf) == 0) ? (status >> 8) & 0xf : 0
	    return errno ? -1 : 0;
    }
}
#endif /* SVR2 */

#endif /* LOCK_PROG */

dot_lock(filename)
char *filename;
{
    char buf[MAXPATHLEN];
    int lockfd, cnt = 0;
    SIGRET (*oldint)(), (*oldquit)();

#ifndef LOCK_PROG
#if defined(SYSV) && !defined(HPUX) && !defined(IRIX4)
    /* Only the spoolfile needs to be dot_locked -- other files are
     * handled by lock_fopen, below.  To avoid collisions with 14-char
     * file name limits, we allow dot_locking ONLY of the spoolfile.
     */
    if (strcmp(spoolfile, filename) != 0)
	return 0;
#ifdef SVR2
    return lock_proc(filename, DOLOCKIT);
#endif /* SVR2 */
#endif /* SYSV && !HPUX && !IRIX4 */
#ifdef BSD
    setregid(rgid, sgid);
#else /* BSD */
    setgid(sgid);
#endif /* BSD */
#endif /* !LOCK_PROG */
#ifdef M_XENIX
    (void) sprintf(buf, "/tmp/%.10s.mlk", login);
#else /* M_XENIX */
    (void) sprintf(buf, "%s.lock", filename);
#endif /* M_XENIX */
    on_intr();
    while ((lockfd = xcreat(buf, 0444)) == -1) {
	if (errno != EEXIST) {
	    error("unable to lock %s", filename);
	    break;
	}
	if (cnt++ == 0)
	    print("%s already locked, waiting", filename);
	else
	    print_more(".");
	sleep(1);
	if (ison(glob_flags, WAS_INTR)) {
	    print_more("\nAborted.\n");
	    break;
	}
    }
    off_intr();
    if (lockfd != -1) {
	if (cnt)
	    print("done.\n");
	(void) close(lockfd);
    }
#ifdef LOCK_PROG
    if (lockfd < 0)
	print_more("\n");
    exit(lockfd < 0? errno : 0);
#else /* !LOCK_PROG */
#ifdef BSD
    setregid(sgid, rgid);
#else
    setgid(getgid());
#endif /* BSD */
    return lockfd == -1? -1 : 0;
#endif /* LOCK_PROG */
}

dot_unlock(filename)
char *filename;
{
    char buf[MAXPATHLEN], *p;

#if !defined(M_XENIX) || defined(LOCK_PROG)
    (void) sprintf(buf, "%s.lock", filename);
#ifndef LOCK_PROG
    {
	/* If the file was locked through open_file(), we may not have
	 * a complete pathname to work with here.  Expand it and test
	 * whether we need to unlink at all.  This should really be
	 * handled by having open_file() return the name it used, but
	 * that breaks too many other things at the moment.
	 */
	int isdir = 0;
	p = getpath(buf, &isdir);
	if (isdir)
	    return 0;
	(void) strcpy(buf, p);
    }
#if defined(SYSV)  && !defined(HPUX) && !defined(IRIX4)
    if (strncmp(spoolfile, buf, strlen(spoolfile)) != 0)
	return 0;
#ifdef SVR2
    p = rindex(buf, '.');
    *p = 0;
    return lock_proc(buf, UNLOCKIT);
#endif /* SVR2 */
#endif /* SYSV && !HPUX && !IRIX4 */
#else /* LOCK_PROG */
    errno = 0;
#endif /* !LOCK_PROG */
#else /* M_XENIX && !LOCK_PROG */
    (void) sprintf(buf, "/tmp/%.10s.mlk", login);
#endif /* !M_XENIX || LOCK_PROG */
#ifndef LOCK_PROG
#ifdef BSD
    setregid(rgid, sgid);
#else /* BSD */
    setgid(sgid);
#endif /* BSD */
#endif /* !LOCK_PROG */
    (void) unlink(buf);
#ifdef LOCK_PROG
    exit(errno);
#else /* !LOCK_PROG */
#ifdef BSD
    setregid(sgid, rgid);
#else
    setgid(getgid());
#endif /* BSD */
#endif /* LOCK_PROG */
    return 0;
}
#endif /* DOT_LOCK */

#ifndef LOCK_PROG

#ifdef SYSV

/*
 * Define some BSD names for the SYSV world
 */
#ifdef USG
#define LOCK_SH F_RDLCK
#define LOCK_EX F_WRLCK
#define LOCK_UN F_UNLCK
#else /* USG */
#define LOCK_SH LK_LOCK
#define LOCK_EX LK_LOCK
#define LOCK_UN LK_UNLCK
#endif /* USG */
#define LOCK_NB 0	/* Always non-blocking in this case */

#ifdef EWOULDBLOCK
#undef EWOULDBLOCK
#endif /* EWOULDBLOCK */
#define EWOULDBLOCK	EAGAIN

#ifndef F_SETLKW
#define F_SETLKW F_SETLK
#endif /* F_SETLKW */

flock(fd, op)
int fd, op;
{
#ifndef USG
    (void) locking(fd, op, 0); /* old xenix (sys III) */
    return 0;
#else
    struct flock l;

    l.l_len = 0L;
    l.l_start = 0L;
    l.l_whence = 1;
    l.l_type = op;

    return fcntl(fd, F_SETLKW, &l);
#endif /* USG */
}

#endif /* SYSV */

static struct options *exclude_list;

/* Quick'n'dirty test to avoid opening the same file multiple times.
 * Fails if we aren't passed full paths or if the file is known by
 * more than one name, but you can't have everything.
 */
static FILE *
exclusive_fopen(filename, mode)
char *filename, *mode;
{
    struct options *tmp;
    FILE *fp;
    
    for (tmp = exclude_list; tmp; tmp = tmp->next)
	if (strcmp(tmp->option, filename) == 0) {
	    errno = EWOULDBLOCK;
	    return NULL_FILE;
	}
#ifdef DOT_LOCK
    /* For additional assurance that each file is opened only once,
     * and for ease of recovery from interrupts and errors, do the
     * dot-locking here and unlock in exclusive_fclose().
     */
    if (dot_lock(filename) != 0)
	return NULL_FILE;
#endif /* DOT_LOCK */
    if (!(fp = mask_fopen(filename, mode)))
	return NULL_FILE;
    if (tmp = (struct options *)malloc(sizeof(struct options))) {
	tmp->option = savestr(filename);
	tmp->value = (char *)fp;
	/*
	 * NOTE: The LCKDFLDIR code below depends on this stackwise
	 * insertion to be able to close/reopen the file pointer.
	 * These routines therefore cannot cleanly be used outside
	 * of lock_fopen() and close_lock(), which handle LCKDFLDIR.
	 */
	tmp->next = exclude_list;
	exclude_list = tmp;
	return fp;
    } else
	(void) fclose(fp);
    return NULL_FILE;
}

static int
exclusive_fclose(fileptr)
FILE *fileptr;
{
    struct options *tmp1, *tmp2;
    int n = 0;
    
    for (tmp1 = tmp2 = exclude_list; tmp1; tmp2 = tmp1, tmp1 = tmp1->next)
	if ((FILE *)(tmp1->value) == fileptr) {
	    if (tmp1 == tmp2)
		exclude_list = tmp1->next;
	    else
		tmp2->next = tmp1->next;
#ifdef DOT_LOCK
	    dot_unlock(tmp1->option);
#endif /* DOT_LOCK */
	    xfree(tmp1->option);
#ifndef LCKDFLDIR
	    /* LCKDFLDIR needs lk_fclose(), so let caller do it */
	    n = fclose(fileptr);
#endif /* !LCKDFLDIR */
	    xfree(tmp1);
	    break;
	}
    return n;
}

FILE *
lock_fopen(filename, mode)
char *filename;
char *mode;
{
    FILE *mail_fp = NULL_FILE;
    struct options exclude;
    int fd, lk;
    int cnt = 0;
    SIGRET (*oldint)(), (*oldquit)();
#ifdef LCKDFLDIR
    extern FILE *lk_fopen();
#endif /* !LCKDFLDIR */

    if (debug && do_set(set_options, "deadlock")) {
	(void) un_set(&set_options, "deadlock");
	return NULL_FILE;
    }

    if (!(mail_fp = exclusive_fopen(filename, mode)))
	return NULL_FILE;
    fd = fileno(mail_fp);

    if (mode[0] != 'r' || mode[1] == '+')
	lk = LOCK_EX | LOCK_NB;
    else
	lk = LOCK_SH | LOCK_NB;

    on_intr();
#ifdef LCKDFLDIR
    (void) fclose(mail_fp);
    while (isoff(glob_flags, WAS_INTR))
	if (mail_fp = lk_fopen(filename, mode, NULL, NULL, 0)) {
	    /* See note in exclusive_fopen() above */
	    exclude_list->value = (char *)mail_fp;
	    break;
	} else /* uses the open brace below the #endif LCKDFLDIR */
#else /* !LCKDFLDIR */
    while (isoff(glob_flags, WAS_INTR) && flock(fd, lk))
#endif /* LCKDFLDIR */
    {
#ifdef LCKDFLDIR
	if (Access(filename, any(mode, "aw+") ? W_OK : R_OK) == 0)
#else /* !LCKDFLDIR */
	if (errno == EWOULDBLOCK
#ifdef M_UNIX
	    || errno == EACCES
#endif /* M_UNIX */
				)
#endif /* LCKDFLDIR */
	{
	    if (isoff(glob_flags, REDIRECT))
		if (!cnt++)
		    print("\nwaiting to lock");
		else
		    print(".");
	} else {
	    error("Unable to lock \"%s\"", filename);
	    (void) exclusive_fclose(mail_fp);
	    off_intr();
	    return NULL_FILE;
	}
	(void) fflush(stdout);
	sleep(1);
    }
    if (cnt)
	print("\n");
    cnt = (ison(glob_flags, WAS_INTR) != 0);
    off_intr();
    if (cnt) {
	(void) exclusive_fclose(mail_fp);
	return NULL_FILE;
    }
    return mail_fp;
}

/*ARGSUSED*/
close_lock(filename, fp)
char *filename;
FILE *fp;
{
#ifdef LCKDFLDIR
    (void) exclusive_fclose(fp); /* Only removes the list elem */
    return lk_fclose(fp, filename, NULL, NULL);
#else /* !LCKDFLDIR */
    fflush(fp);
    (void) flock(fileno(fp), LOCK_UN);
    return exclusive_fclose(fp);
#endif /* LCKDFLDIR */
}

/* Make sure we don't leave dead lockfiles lying around */
void
droplocks()
{
    while (exclude_list)
	close_lock(exclude_list->option, (FILE *)exclude_list->value);
}

#endif /* LOCK_PROG */

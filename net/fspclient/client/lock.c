    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *  Copyright (c) 1993 by Phil Richards (pgr@prg.ox.ac.uk)             *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "client.h"
#include "lock.h"

#ifndef NOLOCKING

#ifndef KEY_PREFIX
#  define KEY_PREFIX "/usr/tmp/.FL"
#endif

static char key_string[sizeof(KEY_PREFIX)+32];

static char code_str[] =
    "0123456789:_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

#include <setjmp.h>

static jmp_buf my_interrupt_env;

#ifndef ANSI_PROTOTYPES
static RETSIGTYPE (*old_intr_handler)();
#else /* ANSI_PROTOTYPES */
static RETSIGTYPE (*old_intr_handler)(int);
#endif /* ANSI_PROTOTYPES */

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
interrupted_lock(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
interrupted_lock(int sig)
#endif /* ANSI_PROTOTYPES */
{
    /* this prints the interrupt message *AND* resets the interrupt handler */
    (*old_intr_handler)(sig);

    if (client_intr_state > 1)
	longjmp(my_interrupt_env, 0);
}

static void
#ifndef ANSI_PROTOTYPES
make_key_string(server_addr, server_port)
    u_long server_addr; 
    u_int server_port;
#else /* ANSI_PROTOTYPES */
make_key_string(u_long server_addr, u_int server_port)
#endif /* ANSI_PROTOTYPES */
{
    u_long v1, v2;
    char *p;

    (void)strcpy(key_string, KEY_PREFIX);
    for (p = key_string; *p; p++)
	;
    v1 = server_addr;
    v2 = server_port;

    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6; v1 = v1 | (v2 << (32-3*6));
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p++ = code_str[v1 & 0x3f]; v1 >>= 6;
    *p   = 0;
}

#endif

/********************************************************************/
/******* For those systems that has flock function call *************/
/********************************************************************/
#ifdef USE_FLOCK

/* this will cause an unfortunate redefinition of F_OK etc, but... */
#include <sys/file.h>

int key_persists = 1;
static u_int lock_fd;
static u_int okey;

int
#ifndef ANSI_PROTOTYPES
client_get_key()
#else /* ANSI_PROTOTYPES */
client_get_key(void)
#endif /* ANSI_PROTOTYPES */
{
    if (!setjmp(my_interrupt_env))
    {
        old_intr_handler = signal(SIGINT, interrupted_lock);

	if (flock(lock_fd, LOCK_EX) < 0)
	{
	    perror("flock");
	    exit(1);
	}

	(void)signal(SIGINT, old_intr_handler);

	if (read(lock_fd, (char*)&okey, sizeof(okey)) < 0)
	{
	    perror("read");
	    exit(1);
	}

	if (lseek(lock_fd, 0L, 0) < 0)
	{
	    perror("seek");
	    exit(1);
	}

        return(okey);
    }

    ffprintf(STDERR, "?couldn't get key from lock\n");
    return -1;
}

void
#ifndef ANSI_PROTOTYPES
client_put_key(key)
    u_int key;
#else /* ANSI_PROTOTYPES */
client_put_key(u_int key)
#endif /* ANSI_PROTOTYPES */
{
    if (write(lock_fd, (char*)&key, sizeof(key)) < 0)
    {
	perror("write");
	exit(1);
    }

    if (lseek(lock_fd, 0L, 0) < 0)
    {
	perror("seek");
	exit(1);
    }

    if (flock(lock_fd, LOCK_UN) < 0)
    {
	perror("unflock");
	exit(1);
    }
}

void
#ifndef ANSI_PROTOTYPES
client_init_key(server_addr, server_port, key)
    u_long server_addr; 
    u_int server_port;
    u_int key;
#else /* ANSI_PROTOTYPES */
client_init_key(u_long server_addr, u_int server_port, u_int key)
#endif /* ANSI_PROTOTYPES */
{
    u_long omask;
    okey = key;

    make_key_string(server_addr, server_port);

    omask = umask(0);
    lock_fd = open(key_string, O_RDWR | O_CREAT, 0666);
    (void)umask(omask);
}

void
#ifndef ANSI_PROTOTYPES
client_finish_key()
#else /* ANSI_PROTOTYPES */
client_finish_key(void)
#endif /* ANSI_PROTOTYPES */
{
    (void)close(lock_fd);
}

#endif
/********************************************************************/
/******* For those systems that has lockf function call *************/
/********************************************************************/
#ifdef USE_LOCKF

int key_persists = 1;
static u_int lock_fd;
static u_int okey;

int
#ifndef ANSI_PROTOTYPES
client_get_key()
#else /* ANSI_PROTOTYPES */
client_get_key(void)
#endif /* ANSI_PROTOTYPES */
{
    if (!setjmp(my_interrupt_env))
    {
        old_intr_handler = signal(SIGINT, interrupted_lock);

	if (lockf(lock_fd, F_LOCK, sizeof(okey)) < 0)
	{
	    perror("lockf");
	    exit(1);
	}

        (void)signal(SIGINT, old_intr_handler);

	if (read(lock_fd, &okey, sizeof(okey)) < 0)
	{
	    perror("readlk");
	    exit(1);
	}

	if (lseek(lock_fd, 0L, 0) < 0)
	{
	    perror("seek");
	    exit(1);
	}

	return(okey);
    }

    ffprintf(STDERR, "?couldn't get key from lock\n");
    return -1;
}

void
#ifndef ANSI_PROTOTYPES
client_put_key(key)
    u_int key;
#else /* ANSI_PROTOTYPES */
client_put_key(u_int key)
#endif /* ANSI_PROTOTYPES */
{
    if (write(lock_fd, &key, sizeof(key)) < 0)
    {
	perror("write");
	exit(1);
    }
    if (lseek(lock_fd, 0L, 0) < 0)
    {
	perror("seek");
	exit(1);
    }
    if (lockf(lock_fd, F_ULOCK, sizeof(key)) < 0)
    {
	perror("unlockf");
	exit(1);
    }
}

void
#ifndef ANSI_PROTOTYPES
client_init_key(server_addr, server_port, key)
    u_long server_addr; 
    u_int server_port;
    u_int key;
#else /* ANSI_PROTOTYPES */
client_init_key(u_long server_addr, u_int server_port, u_int key)
#endif /* ANSI_PROTOTYPES */
{
    u_long omask;
    okey = key;

    make_key_string(server_addr, server_port);

    omask = umask(0);
    lock_fd = open(key_string, O_RDWR | O_CREAT, 0666);
    (void)umask(omask);
}

void
#ifndef ANSI_PROTOTYPES
client_finish_key()
#else /* ANSI_PROTOTYPES */
client_finish_key(void)
#endif /* ANSI_PROTOTYPES */
{
    (void)close(lock_fd);
}

#endif
/********************************************************************/
/******* For those systems that has SysV shared memory + lockf ******/
/********************************************************************/
#ifdef USE_SHAREMEM_AND_LOCKF

#include <sys/ipc.h>
extern char *shmat();

int key_persists = 0;
static u_int *share_key;
static u_int lock_fd;

int
#ifndef ANSI_PROTOTYPES
client_get_key()
#else /* ANSI_PROTOTYPES */
client_get_key(void)
#endif /* ANSI_PROTOTYPES */
{
    if (!setjmp(my_interrupt_env))
    {
        old_intr_handler = signal(SIGINT, interrupted_lock);

	if (lockf(lock_fd, F_LOCK, 2) < 0)
	{
	    perror("lockf");
	    exit(1);
	}

	(void)signal(SIGINT, old_intr_handler);

	return(*share_key);
    }

    ffprintf(STDERR, "?couldn't get key from lock\n");
    return -1;
}

void
#ifndef ANSI_PROTOTYPES
client_put_key(key)
    u_int key;
#else /* ANSI_PROTOTYPES */
client_put_key(u_int key)
#endif /* ANSI_PROTOTYPES */
{
    *share_key = key;
    if (lockf(lock_fd, F_ULOCK, 2) < 0)
    {
	perror("unlockf");
	exit(1);
    }
}

void
#ifndef ANSI_PROTOTYPES
client_init_key(server_addr, server_port, key)
    u_long server_addr; 
    u_int server_port;
    u_int key;
#else /* ANSI_PROTOTYPES */
client_init_key(u_long server_addr, u_int server_port, u_int key)
#endif /* ANSI_PROTOTYPES */
{
    u_long omask;
    key_t lock_key;
    int   lock_shm;

    make_key_string(server_addr, server_port);

    omask = umask(0);
    lock_fd = open(key_string, O_RDWR | O_CREAT, 0666);
    (void)umask(omask);

    if ((lock_key = ftok(key_string, 3432)) < 0)
    {
	perror("ftok");
	exit(1);
    }
    if ((lock_shm = shmget(lock_key, sizeof(short), IPC_CREAT | 0666)) < 0)
    {
	perror("shmget");
	exit(1);
    }
    if (!(share_key = (u_int *)shmat(lock_shm, 0, 0)))
    {
	perror("shmat");
	exit(1);
    }
}

/* I haven't got a clue what I'm doing here -- this is all guesswork! */
void
#ifndef ANSI_PROTOTYPES
client_finish_key()
#else /* ANSI_PROTOTYPES */
client_finish_key(void)
#endif /* ANSI_PROTOTYPES */
{
    if (shmdt((char *)share_key) < 0)
    {
	perror("shmdt");
	exit(1);
    }

    (void)close(lock_fd);
}

#endif
/********************************************************************/
/******* For those who do not want to use locking *******************/
/********************************************************************/
#ifdef NOLOCKING

int key_persists = 0;
static u_int okey;

int
#ifndef ANSI_PROTOTYPES
client_get_key()
#else /* ANSI_PROTOTYPES */
client_get_key(void)
#endif /* ANSI_PROTOTYPES */
{
    return (int)okey;
}

void
#ifndef ANSI_PROTOTYPES
client_put_key(key)
    u_int key;
#else /* ANSI_PROTOTYPES */
client_put_key(u_int key)
#endif /* ANSI_PROTOTYPES */
{
    okey = key;
}

void
#ifndef ANSI_PROTOTYPES
client_init_key(server_addr, server_port, key)
    u_long server_addr; 
    u_int server_port;
    u_int key;
#else /* ANSI_PROTOTYPES */
client_init_key(u_long server_addr, u_int server_port, u_int key)
#endif /* ANSI_PROTOTYPES */
{
    okey = key;
}

void
#ifndef ANSI_PROTOTYPES
client_finish_key()
#else /* ANSI_PROTOTYPES */
client_finish_key(void)
#endif /* ANSI_PROTOTYPES */
{
    ;
}

#endif
/********************************************************************/
/********************************************************************/
/********************************************************************/

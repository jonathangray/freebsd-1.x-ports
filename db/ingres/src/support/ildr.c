/*	
 *	ilwrite() : write driver
 *		1. copy Lock request info to lockbuf
 *		2. follow action in l_act
 *		3. Error return conditions
 *			-1: lockrequest fails(only on act=1)
 *			-2: attempt to release a lock not set
 *			    by calling program
 *			-3: illegal action requested
 */
#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <netdb.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ildr.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

#define	TRUE	1
#define	FALSE	0

#ifdef	DEBUG
static int ildebug = TRUE;
#endif /* DEBUG */

SCCSID(@(#)ildr.c	8.2	6/12/88)

typedef struct Wait {
	int		wait_fd;	/* file descriptor to send lock info */
	int		wait_lock;	/* what lock we are waiting for */
	struct Lockreq	wait_req;	/* the lock request */
	struct Wait	*next;		/* next structure */
} wait_t;

static wait_t	*Wait_queue = NULL;

static int	From_server;		/* read connection from socket server */
static fd_set	Read_fmt;		/* bit mask for select */
static char	*Prog_name;		/* program name for abnormal errors */

void send_info(register int fd, int data);

/*
** abnormal
** a signal has come down and hit us. We restart the entire
** program, and hope it goes away
*/
RETSIGTYPE
abnormal(int sig)
{
#ifdef	DEBUG
	printf("DRIVER: error Signal No. = %d, restarting\n",sig);
#endif

/*	execl("/etc/lock_driver","lock_driver","restart",0);   */
	execl("/etc/ingreslock","lock_driver","restart",0);
	execl(Prog_name,Prog_name,"restart",0);
	execlp("ingreslock","lock_driver","restart",0);
	exit(4);
}/* abnormal */

/*
** new_proc
** start up a new connection to an Ingres process
*/
void
new_proc(void)
{
	register	int	fd;
	int	to_ioctl = 1;
	struct	sockaddr_in	addr;
	int	len;

	len = sizeof(addr);
	FD_ZERO(&Read_fmt);
	if ((fd = accept(From_server,(struct sockaddr *)&addr,&len)) != -1) {
		FD_SET(fd, &Read_fmt);
		ioctl(fd,FIONBIO,&to_ioctl);   /* set sockets to nonblocking */
	}
#ifdef	DEBUG
	else {
		ingres_perror("accept");
		sleep(1);
	}
	printf("DRIVER: new file %d (%o)\n",fd,(1<<fd));
#endif /* DEBUG */
}/* new_proc */

/*
** wait_on
** Set up to wait for a free lock.
*/
void
wait_on(register int fd, register int lock, struct Lockreq req)
{
	register wait_t	*ptr;

	ptr = xalloc(sizeof(wait_t), 1, 1);
	ptr->wait_fd = fd;
	ptr->wait_lock = lock;
	ptr->wait_req = req;
	ptr->next = Wait_queue;
	Wait_queue = ptr;
}/* wait_on */

/*
 *	ilcomp- string compare
 *	  	returns 0 if strings match
 *		returns -1 otherwise
 */
static int
ilcomp(register char *s1, register char *s2)
{
	register int	k;

	for (k = 0; k < KEYSIZE; k++)
		if (*s1++ != *s2++)
#ifdef	DEBUG
		{
			if (ildebug)
				printf("DRIVER: ilcomp returning -1\n");
			return (-1);
		}
#else	DEBUG
			return (-1);
#endif /* DEBUG */
	return (0);
}

/*
 *	ilunique- check for match on key
 *	
 *	return index of Locktab if match found
 *	else return -1
 */
static int
ilunique(register struct Lockreq *ll)
{
	register struct Lockform	*p;
	register int			k;

	for (k = 0; k < NLOCKS; k++) {
		p = &Locktab[k];
		if ((p->l_mod != M_EMTY)
		&& (ilcomp(p->l_key,ll->lr_key) == 0)
		&& (p->l_type == ll->lr_type)
		&& ((p->l_mod == M_EXCL) || (ll->lr_mod == M_EXCL))) {
#ifdef	DEBUG
			if (ildebug) {
				register int i;

				printf("ildr: lock ");
				for (i = 0; i < KEYSIZE; i++)
					printf("%c", ll->lr_key[i]);
				printf(" busy\n");
			}
#endif /* DEBUG */
			return(k);
		}
	}
	return(-1);
}

/*
 *	enter Lockbuf in locktable
 *	return position in Locktable
 *	error return of -1
 */
static int
ilenter(register struct Lockreq *ll, int key)
{
	int	k,l;
	register char	*f,*t;
	register struct Lockform	*p;

	for (k = 0; k < NLOCKS; k++) {
		p = &Locktab[k];
		if (p->l_mod == M_EMTY) {
			p->l_pid = key;
			p->l_type = ll->lr_type;
			Locktab[k].l_mod = p->l_mod = ll->lr_mod;
			f = ll->lr_key;
			t = p->l_key;
			for (l = 0; l < KEYSIZE; l++)
				*t++ = *f++;
			for (l = 0; l <= ll->lr_type; l++)
				Lockset[l]--;
#ifdef	DEBUG
			if (ildebug)
				printf("DRIVER: ilenter %d, mod %d, omod = %d\n",k,p->l_mod,Locktab[k].l_mod);
#endif /* DEBUG */
			return(k);
		}
	}
#ifdef	DEBUG
	if (ildebug)
		printf("DRIVER: ilenter -1\n");
#endif /* DEBUG */
	return (-1);
}

/*
** set_lock
** attempt to set a lock. If we can't, block the process and
** return -1, if we can than set the lock and return 0.
*/
int
set_lock(register int read_desc, struct Lockreq lockbuf)
{
	register int	blockflag;
	register int	i;

	/*
	** attempt to set lock.
	** sleep on blocking address if failure.
	*/

	do {
		do {
			blockflag = TRUE;
			for (i = 0; i <= lockbuf.lr_type; i++)
				if (Lockset[i] == 0) {
#ifdef	DEBUG
					if (ildebug)
						printf("ildr: lock %d not available\n", i);
#endif /* DEBUG */
					wait_on(read_desc, Lockset[i], lockbuf);
					return(-1);
				}
		} while (!blockflag);

		if ((i = ilunique(&lockbuf)) >= 0) {
			blockflag = FALSE;
			Locktab[i].l_wflag = W_ON;
			wait_on(read_desc, &Locktab[i], lockbuf);
			return(-1);
		}
	} while (!blockflag);
	ilenter(&lockbuf,read_desc);
	return (0);
}/* set_lock */

/*
** wakeup
** See if there is anythng waiting on the newly freed lock. If there is,
** tell it it can have the lock now.
*/
void
wakeup(register int lock)
{
	register wait_t	*ptr,*back;

	for (back = NULL, ptr = Wait_queue ;
	     ptr != NULL ;
	     back = ptr, ptr = ptr->next) {
		if (ptr->wait_lock == lock) {
			if (set_lock(ptr->wait_fd,ptr->wait_req) == 0) {
				send_info(ptr->wait_fd,0);
				if (back != NULL)
					back->next = ptr->next;
				else
					Wait_queue = Wait_queue->next;
				cfree(ptr);
				return;
			}
		}
	}
}/* wakeup */

/*
 *	remove the lth Lock
 *		if the correct user is requesting the move.
 */
static void
ilrm(int l, int key, int remove_all)
{
	register struct Lockform *a;
	register	k;

	a = &Locktab[l];
	if (a->l_pid == key && a->l_mod != M_EMTY) {
		if (!remove_all && a->l_type == T_DB)
			return;
		a->l_mod = M_EMTY;
		a->l_pid = 0;
		if (a->l_wflag == W_ON) {
			a->l_wflag = W_OFF;
			wakeup(&Locktab[l]);
		}
		for (k = 0; k <= a->l_type; k++) {
			if (++Lockset[k] == 1) {
				wakeup(&Lockset[k]);
			}
		}
	}
}/* ilrm */

/*
 *	ilrma releases all locks for a given process id(pd)
 *	-- called from sys1.c$exit() code.
 */
void
ilrma(int key, int remove_all)
{
	register int	i;

#ifdef	DEBUG
	printf("DRVIER: Removing all, key = %d\n",key);
#endif /* DEBUG */
	for (i = 0; i < NLOCKS; i++)
		ilrm(i,key,remove_all);
}

/*
** send_info
** Send the data down the socket. Don't do it if it would cause the driver
** to block.
*/
void
send_info(register int fd, int data)
{
	struct timeval	tv;
	fd_set		wdes;
	
	FD_SET(fd, &wdes);
	errno = 0;
	tv.tv_sec = 10;
	tv.tv_usec = 0;
	if (select(getdtablesize(), NULL, &wdes, NULL, &tv) != 1) {
		FD_CLR(fd, &Read_fmt);
		ilrma(fd,TRUE);
		close(fd);
	} else {
		if (write(fd,&data,sizeof(data)) != sizeof(data)) {
			if (errno == 0)
				return;
			FD_CLR(fd, &Read_fmt);
			ilrma(fd,TRUE);
			close(fd);
		}
	}
}/* send_info */

static int
ilfind(register struct Lockreq *ll, int key)
{
	register int	k;
	register struct Lockform	*p;

	for (k = 0; k < NLOCKS; k++) {
		p = &Locktab[k];
		if ((p->l_mod != M_EMTY)
		&& (ilcomp(p->l_key,ll->lr_key) == 0)
		&& (p->l_type == ll->lr_type)
		&& (p->l_pid == key))
			return(k);
	}
	return(-1);
}/* ilfind */

/*
 *	ilclose- releases all locks
 */
static void
ilclose(void)
{
	register int	k;
	register caddr_t c;
#ifdef	DEBUG
	printf("DRIVER: entered close\n");
#endif /* DEBUG */

	for (k = 0; k < NLOCKS; k++)
		wakeup(&Locktab[k]);
	for (k = 0; k < 4; k++)
		wakeup(&Lockset[k]);
	for (c = (caddr_t)&Locktab[0].l_pid; c < (caddr_t)&Locktab[NLOCKS];)
		*c++ = 0;
	Lockset[0] = NLOCKS;
	Lockset[1] = PLOCKS;
	Lockset[2] = RLOCKS;
	Lockset[3] = DLOCKS;
}/* ilclose */

void
ilwrite(register int read_desc)
{
	struct Lockreq	lockbuf;
	register int i;

	errno = 0;
#ifdef	DEBUG
	printf("DRIVER: entering ilwrite, read_desc = %d\n",read_desc);
#endif /* DEBUG */
	if (read(read_desc,&lockbuf, sizeof(lockbuf)) != sizeof(lockbuf)) {
#ifdef	DEBUG
		printf("Read error, errno = %d\n",errno);
#endif /* DEBUG */
		if (errno == EWOULDBLOCK)
			return;
		if (errno == ECONNRESET) {
			ilrma(read_desc,TRUE);
			close(read_desc);
			FD_CLR(read_desc, &Read_fmt);
			return;
		}
		send_info(read_desc,-5);
		return;
	}

#ifdef	DEBUG
	if (ildebug)
		printf("ildr: act %d, type %d, mode %d, read_desc %d\n",
			lockbuf.lr_act, lockbuf.lr_type, lockbuf.lr_mod, read_desc);
#endif /* DEBUG */
	if ((lockbuf.lr_act < A_RLS1)
	&& ((lockbuf.lr_type < T_CS) || (lockbuf.lr_type > T_DB)
	   || (lockbuf.lr_mod < M_EXCL) || (lockbuf.lr_mod > M_SHARE))) {
#ifdef	DEBUG
		printf("Illegal request\n");
#endif /* DEBUG */
		send_info(read_desc,-5);
		return;
	}
/*
 *		follow action from lock request
 */
	switch(lockbuf.lr_act) {
	  case A_RTN:
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: A_RTN\n");
#endif /* DEBUG */

		/*
		** attempt to set lock.
		** error return if failure.
		*/
		for (i = 0; i <= lockbuf.lr_type; i++) {
			if (Lockset[i] == 0) {
#ifdef	DEBUG
				if (ildebug)
					printf("ildr: lock %d not available\n", i);
#endif /* DEBUG */
				send_info(read_desc,-1);
				return;
			}
		}
		if (ilunique(&lockbuf) >= 0) {
			send_info(read_desc,-1);
			return;
		}
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: lock assigned\n");
#endif /* DEBUG */
		ilenter(&lockbuf,read_desc);
		break;

	  case A_SLP:
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: A_SLP\n");
#endif /* DEBUG */
		if (set_lock(read_desc,lockbuf) == -1)
			return;
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: got lock\n");
#endif /* DEBUG */
		break;
	  case A_RLS1:
				/* remove 1 lock */
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: A_RLS1\n");
#endif /* DEBUG */
		if ((i = ilfind(&lockbuf,read_desc)) >= 0) {
			ilrm(i, read_desc, 1);
		} else {
			send_info(read_desc, -2);
			return;
		}
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: released\n");
#endif /* DEBUG */
		break;

	  case A_RLSA:
				/* remove all locks for this process id*/
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: A_RLSA\n");
#endif /* DEBUG */
		ilrma(read_desc,FALSE);
		break;

	  case A_ABT:		/* remove all locks */
#ifdef	DEBUG
		if (ildebug)
			printf("Driver: A_ABT\n");
#endif /* DEBUG */
		ilclose();
		break;

	  default :
#ifdef	DEBUG
		if (ildebug)
			printf("DRIVER: garbage\n");
#endif /* DEBUG */
		send_info(read_desc,-3);
	}
	send_info(read_desc,0);
}

/*
** close_up
** try and find a closed up file descriptor.
*/

void
close_up(fd_set fmt)
{
	struct timeval	tv;
	register	int	i;
	fd_set		rdesc;

	errno = 0;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	for (i = 0 ; i < NOFILE ; i++) {
		if (FD_ISSET(i, &fmt)) {
			FD_ZERO(&rdesc);
			FD_SET(i, &rdesc);
			if (select(getdtablesize(), &rdesc, 0, 0, &tv) == -1) {
				/*
				** the server socket has closed down.
				** BOY ARE WE IN TROUBLE
				*/
				if (i == From_server) {
					sleep(1);
#ifdef	DEBUG
					printf("Restarting socket\n");
#endif /* DEBUG */
					init_socket();
				}
				if (errno == EBADF) {
					shutdown(i,2);
					close(i);
					FD_CLR(i, &Read_fmt);
					ilrma(i,TRUE);
				}
			}
		}
	}
}/* close_up */

/*
** main
** initilize the socket to the socket server, and then sit and on
** a select waiting for input.
*/
void
main(int ac, char **av)
{
        struct timeval	tv;		/* added by K.Okamoto  */
	register int	i;		/* index */
	fd_set		read_fd;	/* bit mask of useable descriptors*/
	long		num_des;	/* number of readable descriptors */

	/*
	** close all the files descriptors, so we can have more INGRES
	** processes. We are lmited by file descriptors.
	*/
#ifdef	DEBUG
	setbuf(stdout,NULL);
	printf("DRIVER: starting up\n");
	for (i = 3 ; i < NOFILE ; i++)
#else
	for (i = 1 ; i < NOFILE ; i++)
#endif /* DEBUG */
		close(i);

	/*
	** set up all the signals. Catching any signal
	** is an error. We do a "warm start" in this condition
	*/
	for (i = 0 ; i < NSIG ; i++) {
		signal(i, abnormal);
	}
       	signal(SIGPIPE, SIG_IGN);	/* ignore this one, in case a process simply dies in mid stride */
        Prog_name = *av;
	/*
	** if ac == 2, then we are restarted from the the lock driver
	** itself, and we don't have to reattach ourselves to the magic
	** ingres socket.
	*/
	if (ac == 2)
		From_server = 0;
	else {
		/* guarantee that 0 is what is attached to the socket server */
		close(0);
		From_server = init_socket();
	}
	FD_ZERO(&Read_fmt);
	FD_SET(From_server, &Read_fmt);
        tv.tv_sec = 0;
        tv.tv_usec = 0; 

	/*
	** infinite loop waiting for something to happen
	*/
	for (;;) {
		(void) memcpy(&read_fd, &Read_fmt, sizeof(read_fd));

		/*
		** wake up whenever something happens
		*/

		num_des = select(getdtablesize(),&read_fd,0,0,0);

		while ((num_des = select(getdtablesize(),&read_fd,0,0,0)) == 0) {
#ifdef	DEBUG
		printf("select returns 0 (%o)\n",read_fd);
#endif /* DEBUG */
			(void) memcpy(&read_fd, &Read_fmt, sizeof(read_fd));
		}
#ifdef	DEBUG
		printf("select returns %d (%o)\n",num_des,read_fd);
#endif /* DEBUG */


		if (num_des == -1) {
#ifdef	DEBUG
			ingres_perror("DRIVER:num_des = -1");
#endif /* DEBUG */
			/*
			** a bit of defensive programming.
			** If there is an EBADF (bad file descriptor) error 
			** then we assume that a file descriptor has shut down,
			** without tellng us. We go to a function to figure
			** out what has died.
			*/
			if (errno == EBADF)
				close_up(Read_fmt);
			sleep(1);
			continue;
		}
		if (FD_ISSET(From_server, &read_fd)) {
			num_des--;
			new_proc();
			FD_CLR(From_server, &read_fd);
		}
		if (num_des > 0) {
			for (i = 0 ; i < NOFILE ; i++) {
				if (FD_ISSET(i, &read_fd)) {
					ilwrite(i);
				}
			}
		}
	}
}/* main */

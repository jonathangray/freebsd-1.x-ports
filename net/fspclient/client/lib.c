    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *  Copyright (c) 1992, 1993 by Phil Richards (pgr@prg.ox.ac.uk)       *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "client.h"
#include "lock.h"

extern int errno;

static int myfd;
static struct sockaddr_in server_addr;
static u_short myseq = 0;
static u_short key;

int client_trace      = 0;
int client_intr_state = 0;
int client_intr_cnt   = 0;

int busy_scale = 3;
int burst_max  = 1;
u_int time_out = 0;

u_long target_delay = MINDELAY;       /* expected max delay                 */
static u_long idle_delay = 1;         /* idle retransmit timer              */

static u_long total_rtt	 = MINDELAY;  /* time taken for all packet trips    */
static u_long total_trips= 0;         /* total number of packet round trips */
static u_long total_sent = 0;	      /* total number of packets sent       */
static int timed_out;

u_long udp_sent_time;

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
timeout_handler(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
timeout_handler(int sig)
#endif /* ANSI_PROTOTYPES */
{
    timed_out++;
}

UBUF *
#ifndef ANSI_PROTOTYPES
client_interact(cmd, pos, l1, p1, l2, p2)
    u_int cmd;
    u_int l1;
    u_int l2;
    u_long pos;
    char *p1;
    char *p2;
#else /* ANSI_PROTOTYPES */
client_interact(u_int cmd, u_long pos,
		u_int l1, char *p1,
		u_int l2, char *p2)
#endif /* ANSI_PROTOTYPES */
{
    struct sockaddr_in from;
    UBUF sbuf;
    static UBUF rbuf;
    u_int u, mlen;
    fd_set mask;
    int retval, bytes, retry_send, retry_recv;
    u_long w_delay;
    u_long busy_delay = 0;	/* just to stop gcc complaining! */
    int burst_count = burst_max;
    int payload_chksum;

    sbuf.cmd = cmd;
    sbuf.len = htons(l1);
    sbuf.pos = htonl(pos);

    if (client_intr_state > 1)
	return 0;

    /* kept the next line so that the standalone clients work as expected */
    client_intr_state = 1;

    payload_chksum = 0;

    for (u = 0; u < l1; u++) {
	sbuf.buf[u] = p1[u];
	payload_chksum += (u_char)p1[u];
    }

    for (u = 0; u < l2; u++) {
	sbuf.buf[l1 + u] = p2[u];
	payload_chksum += (u_char)p2[u];
    }

    mlen = UBUF_HSIZE + l1 + l2;
    payload_chksum += mlen;

    key = client_get_key();

    timed_out = 0;
    if (time_out)
    {
	(void)signal(SIGALRM, timeout_handler);
	(void)alarm(time_out);
    }

    for (retry_send = 0, burst_count = burst_max;
         !timed_out && client_intr_state < 2;
         retry_send++)
    {
        u_int i, sum;
	struct timeval beforest;

	sbuf.key = key;
	sbuf.seq = (myseq & 0xfffc) | (retry_send & 0x0003);
	sbuf.sum = 0;

	for (i = 0, sum = payload_chksum; i < UBUF_HSIZE; i++)
	    sum += ((u_char*)(&sbuf))[i];

	sbuf.sum = sum + (sum >> 8);

	switch (retry_send)  /* adaptive retry delay adjustments */
	{
	  case  0:
	    busy_delay = (busy_scale * total_rtt) / total_trips;
	    w_delay = busy_delay;
	    burst_count = burst_max;
	    break;

	  case  1:
	    if (--burst_count > 0)
		retry_send--;
	    else
		burst_count = burst_max;

	    w_delay = (3 * busy_delay) / 2;

	    if (client_trace)
	    {
		if (burst_count & 0x1)
		    (void)write(2,"r\b",2);
		else
		    (void)write(2,"R\b",2);
	    }
	    break;

	  default:
	    if (client_trace)
	    {
		static char *idlech[2] =  { "-\\|/-\\|/", "+X+X+X+X" };
		char ch[2];

		ch[0] = idlech[burst_count & 0x1][retry_send & 0x7];
		ch[1] = '\b';
		(void)write(2,ch,2);
	    }

	    if (--burst_count > 0)
	    {
		retry_send--;
		w_delay = (3 * busy_delay) / 2;
	    }
	    else
	    {
		if (idle_delay < 256)
		    idle_delay <<= 1;
		w_delay = idle_delay * busy_delay;
		burst_count = burst_max;
	    }

	    break;
	}

	(void)gettimeofday(&beforest, (struct timezone*)0);

	/*
	** DO NOT REMOVE THIS LINE -- IT LIMITS THE NUMBER OF RETRIES POSSIBLE
	** PER SECOND; REMOVAL WILL RESULT IN VIRAL INFECTION FOLLOWED BY
	** HEADACHES.  Ok, so it won't.  But don't remove it anyway.
	*/
	if (w_delay < 500)
	    w_delay = 500;

	if (sendto(myfd, (char *)&sbuf, mlen, 0,
		   (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0)
	{
	    perror("sendto");
	    /* fake an interrupt */
	    client_intr_state = 2;
	    break;	/* drop out of the loop */
	}

	if (dbug_flag > 1)
	{
	    ffprintf(STDDBG, "\npacket sent (%d, %d):\n",
		     retry_send, burst_count);
	    ffprintf(STDDBG, "\tcommand  = %d\n", sbuf.cmd);
	    ffprintf(STDDBG, "\tchecksum = %d\n", sbuf.sum);
	    ffprintf(STDDBG, "\tkey      = %d\n", sbuf.key);
	    ffprintf(STDDBG, "\tsequence = %d\n", sbuf.seq);
	    ffprintf(STDDBG, "\tlength   = %d (%d %d %d)\n",
		     mlen, UBUF_HSIZE, sbuf.len, mlen - UBUF_HSIZE - sbuf.len);
	    ffprintf(STDDBG, "\tposition = %d\n", sbuf.pos);
	}

	total_sent++;
	udp_sent_time = time((time_t*) 0);

	FD_ZERO(&mask);
	FD_SET(myfd, &mask);

	for (retry_recv = 0, retval = -1;
	     retval == -1 && !timed_out && client_intr_state < 2;
	     retry_recv++)
	{
	    switch (retry_recv)  /* adaptive retry delay adjustments */
	    {
	      case  0:
		break;

	      case  1:
		if (client_trace)
		    (void)write(2, "E\b", 2);
		break;

	      default:
		if (client_trace)
		{
		    char ch[2], *idlech = "_s$s";
		    ch[0] = idlech[retry_recv & 0x3];
		    ch[1] = '\b';
		    (void)write(2, ch, 2);
		}
		break;
	    }

	    retval = _x_select(&mask, w_delay);
	}

	if (client_intr_state > 1)
	{
	    myseq = (myseq + 0x0004) & 0xfffc;  /* seq for next request */
	    client_put_key(key);

	    if (!key_persists)
		client_done();

	    break;
	}

	if (retval > 0)    /* an incoming message is waiting */
	{
	    struct timeval afterrf;

	    bytes = sizeof(from);
	    if ((bytes = recvfrom(myfd, (char*)&rbuf, sizeof(rbuf), 0,
				  (struct sockaddr *)&from, &bytes))
		< UBUF_HSIZE)
	    {
		if (dbug_flag > 1)
		    ffprintf(STDDBG, "\nreturn packet shattered (%d)\n", bytes);
		continue;	/* send and try again */
	    }

	    (void)gettimeofday(&afterrf, (struct timezone*)0);

	    u = rbuf.sum;
	    rbuf.sum = 0;

	    for (i = 0, sum = 0; i < bytes; i++)
		sum += ((u_char*)(&rbuf))[i];

	    sum = (sum + (sum >> 8)) & 0xff;

	    rbuf.len = htons(rbuf.len);
	    rbuf.pos = htonl(rbuf.pos);

	    if (dbug_flag > 1)
	    {
		ffprintf(STDDBG, "\npacket received (%d):\n", retry_recv);
		ffprintf(STDDBG, "\tretval   = %d\n", rbuf.cmd);
		ffprintf(STDDBG, "\tchecksum = %d (expected %d)\n", sum, u);
		ffprintf(STDDBG, "\tnext key = %d\n", rbuf.key);
		ffprintf(STDDBG, "\tsequence = %d (expected %d)\n",
			 rbuf.seq & 0xfffc, myseq);
		ffprintf(STDDBG, "\tlength   = %d (`expected' %d)\n",
			 bytes, rbuf.len + UBUF_HSIZE);
		ffprintf(STDDBG, "\tposition = %d\n", rbuf.pos);
	    }

	    if (sum != u) continue;  /* wrong check sum */

	    if ((rbuf.seq & 0xfffc) != myseq)  continue;  /* wrong seq # */
	    if (rbuf.len + UBUF_HSIZE > bytes) continue;  /* truncated   */

	    myseq = (myseq + 0x0004) & 0xfffc;  /* seq for next request */
	    key   = rbuf.key;		    /* key for next request */

	    client_put_key(key);

	    if (client_intr_state > 1)
	    {
		if (!key_persists)
		    client_done();
		continue;	/* loop back; drop out of loop `properly' */
	    }

	    /* only wipe out the character if one has been printed */
	    if (client_trace
                && (retry_send > 0 || retry_recv > 1 || burst_count < burst_max))
		(void)write(2," \b",2);

	    total_trips++;
	    total_rtt += 1000 * (afterrf.tv_sec - beforest.tv_sec) +
			 (afterrf.tv_usec - beforest.tv_usec) / 1000;

	    if (retry_send < 2 && idle_delay > 1)
		idle_delay >>= 1;

	    break;	/* get out of the main (retry) loop */
	}
    }

    if (time_out)
    {
	(void)alarm(0);
	(void)signal(SIGALRM, SIG_DFL);
    }

    if (timed_out)
    {
	ffprintf(STDERR, "?timed out\n");
	client_intr_state = 2;
    }

    if (client_intr_state > 1)
	return 0;

    if (rbuf.cmd == CC_ERR)
	ffprintf(STDERR, "?error: %s\n", rbuf.buf);

    return &rbuf;
}

void
#ifndef ANSI_PROTOTYPES
print_comm_stats(out)
    FILE *out;
#else /* ANSI_PROTOTYPES */
print_comm_stats(FILE *out)
#endif /* ANSI_PROTOTYPES */
{
    static u_long last_rtt	= MINDELAY;
    static u_long last_trips	= 1;
    static u_long last_sent	= 0;

    if (total_sent == last_sent || total_trips <= last_trips)
	return;

    if (out)
    {
	ffprintf(out,
		"cumulative round trip time = %3.01f msec; %u/%u packets (%3.01f%%)\n",
		(double)total_rtt/(double)total_trips,
		total_trips-1, total_sent,
		100.0 * ((double)(total_trips-1)/(double)total_sent));
	ffprintf(out,
		"current round trip time = %3.01f msec; %u/%u packets (%3.01f%%)\n",
		(double)(total_rtt-last_rtt)/(double)(total_trips-last_trips),
		total_trips-last_trips, total_sent-last_sent,
		100.0 * ((double)(total_trips-last_trips)
			/(double)(total_sent-last_sent)));
    }

    last_sent	= total_sent;
    last_rtt	= total_rtt;
    last_trips	= total_trips;
}

int
#ifndef ANSI_PROTOTYPES
init_client(host, port, myport)
    char *host;
    int   port;
    int myport;
#else /* ANSI_PROTOTYPES */
init_client(char *host, int port, int myport)
#endif /* ANSI_PROTOTYPES */
{
    total_rtt   = target_delay;
    total_trips = 1;
    total_sent  = 0;
    idle_delay  = 1;

    if ((myfd = _x_udp(&myport)) < 0)
    {
	perror("socket open");
	return -1;
    }

    if (_x_adr(host,port,&server_addr) == -1)
    {
	(void)close(myfd);
	perror("server addr");
	return -1;
    } 

    client_init_key(server_addr.sin_addr.s_addr, port, getpid());

    return 0;
}

void
#ifndef ANSI_PROTOTYPES
finish_client()
#else /* ANSI_PROTOTYPES */
finish_client(void)
#endif /* ANSI_PROTOTYPES */
{
    client_finish_key();
    (void)close(myfd);
}

void
#ifndef ANSI_PROTOTYPES
client_done()
#else /* ANSI_PROTOTYPES */
client_done(void)
#endif /* ANSI_PROTOTYPES */
{
#ifdef NOLOCKING
    (void)client_interact(CC_BYE, 0L, 0, NULLP, 0, NULLP);
#endif
}

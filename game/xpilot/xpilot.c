/* $Id: xpilot.c,v 1.1 1994/02/23 14:40:09 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#ifdef VMS
#include "username.h"
#include <socket.h>
#include <in.h>
#include <inet.h>
#else
#include <pwd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif
#if defined(SVR4) || defined(__svr4__)
# include <sys/sockio.h>
#endif
#ifdef VMS
#include <time.h>
#else
#include <sys/time.h>
#endif
#ifndef LINUX
#include <net/if.h>
#endif
#include <netdb.h>
#if  !defined(__apollo)
#    include <string.h>
#endif

#include "version.h"
#include "config.h"
#include "types.h"
#include "pack.h"
#include "bit.h"
#include "error.h"
#include "net.h"
#ifdef SUNCMW
#include "cmw.h"
#endif /* SUNCMW */

#if defined(VMS) && !defined(MAXHOSTNAMELEN)
#define MAXHOSTNAMELEN 64
#endif

#ifndef	lint
static char versionid[] = "@(#)$" TITLE " $";
static char sourceid[] =
    "@(#)$Id: xpilot.c,v 1.1 1994/02/23 14:40:09 jkh Exp $";
#endif

#define MAX_LINE	256


static int		socket_c,		/* Contact socket */
    			contact_port,
    			server_port,
    			login_port;
static char		nick_name[MAX_NAME_LEN],
			real_name[MAX_NAME_LEN],
    			server_host[MAXHOSTNAMELEN],
    			server_name[MAXHOSTNAMELEN],
    			hostname[MAXHOSTNAMELEN],
    			display[MAX_DISP_LEN],
			shutdown_reason[MAX_CHARS];
static int		auto_connect = false,
    			list_servers = false,
			motd = true,
    			auto_shutdown = false;
static unsigned		server_version;
static int		team = TEAM_NOT_SET;
static sockbuf_t	sbuf;			/* contact buffer */

char			**Argv;
int			Argc;


static int Query_all(int sockfd, int port, char *msg, int msglen);


static void initaddr(void)
{
    struct hostent	*hinfo;


    gethostname(hostname, sizeof hostname);

    /*
     * Get host's official name.
     */
    if ((hinfo = gethostbyname(hostname)) == NULL) {
	error("gethostbyname");
    } else {
	strcpy(hostname, hinfo->h_name);
    }
}



static void printfile(char *filename)
{
    FILE *fp;
    char c;


    if ((fp=fopen(filename, "r")) == NULL) {
/*	error(filename);	*/
	return;
    }

    while ((c=fgetc(fp)) && !feof(fp))
	putchar(c);

    fclose(fp);
}



static bool Get_contact_message(void)
{
    int			len;
    unsigned		magic;
    unsigned char	reply_to, status;
    bool		readable = false;

    while (readable == false && SocketReadable(socket_c) > 0) {

	Sockbuf_clear(&sbuf);
	len = DgramReceiveAny(sbuf.sock, sbuf.buf, sbuf.size);
	if (len <= 0) {
	    if (len == 0) {
		continue;
	    }
	    error("DgramReceiveAny, contact message");
	    exit(-1);
	}
	sbuf.len = len;

	/*
	 * Now get server's host and port.
	 */
	strcpy(server_host, DgramLastaddr());
	server_port = DgramLastport();
	strcpy(server_name, DgramLastname());

	if (Packet_scanf(&sbuf, "%u%c%c", &magic, &reply_to, &status) <= 0) {
	    errno = 0;
	    error("Incomplete contact reply message (%d)", len);
	}
	else if ((magic & 0xFFFF) != (MAGIC & 0xFFFF)) {
	    errno = 0;
	    error("Bad magic on contact message (0x%x).", magic);
	}
	else if ((server_version = MAGIC2VERSION(magic)) < MIN_SERVER_VERSION
	    || server_version > MAX_SERVER_VERSION) {
	    printf("Incompatible version with server %s (%04x,%04x).\n\n",
		server_name, MY_VERSION, MAGIC2VERSION(magic));
	}
	else if (server_version == 0x3020
	    && reply_to == CONTACT_pack
	    && status == E_VERSION) {
	    /*
	     * This server doesn't know that we can join him.
	     * Resend the contact message to him adapted to his version.
	     */
	    Sockbuf_clear(&sbuf);
	    Packet_printf(&sbuf, "%u%s%hu%c", VERSION2MAGIC(0x3020),
			  real_name, GetPortNum(sbuf.sock), CONTACT_pack);
	    if (DgramSend(sbuf.sock, server_host, server_port,
			  sbuf.buf, sbuf.len) == -1) {
		error("Can't send contact request to %s/%d",
		      server_host, server_port);
		exit(1);
	    }
	}
	else {
	    /*
	     * Found one which we can talk to.
	     */
	    readable = true;
	}
    }

    return (readable);
}



static bool Get_reply_message(sockbuf_t *ibuf)
{
    int			len;
    unsigned		magic;


    if (SocketReadable(ibuf->sock)) {
	Sockbuf_clear(ibuf);
	if ((len = read(ibuf->sock, ibuf->buf, ibuf->size)) == -1) {
	    error("Can't read reply message from %s/%d",
		server_host, server_port);
	    exit(-1);
	}

	ibuf->len = len;
	if (Packet_scanf(ibuf, "%u", &magic) <= 0) {
	    errno = 0;
	    error("Incomplete reply packet (%d)", len);
	    return (0);
	}

	if ((magic & 0xFFFF) != (MAGIC & 0xFFFF)) {
	    errno = 0;
	    error("Wrong MAGIC in reply pack (0x%x).", magic);
	    return (0);
	}

	if (MAGIC2VERSION(magic) != server_version) {
	    printf("Incompatible version with server on %s (%04x,%04x).\n\n",
		server_name, MY_VERSION, MAGIC2VERSION(magic));
	    return (0);
	}
    } else
	return (0);
    
    return (len);
}



/*
 * This is the routine that interactively (if not auto_connect) prompts
 * the user on his/her next action.  Returns true if player joined this
 * server (connected to server), or false if the player wants to have a
 * look at the next server.
 */
static bool Process_commands(sockbuf_t *ibuf)
{
    int			delay, max_robots;
    char		c, status, reply_to, str[MAX_LINE];
    unsigned short	port;


    for (;;) {

	/*
	 * Now, what do you want from the server?
	 */
	if (!auto_connect) {
	    printf("*** Server on %s. Enter command> ", server_name);

	    gets(str);
	    c = str[0];
	    if (feof(stdin)) {
		puts("");
		c = 'Q';
	    }
	    CAP_LETTER(c);
	} else {
	    if (list_servers)
		c = 'S';
	    else if (auto_shutdown)
		c = 'D';
	    else
		c = 'J';
	}

	Sockbuf_clear(ibuf);
	Packet_printf(ibuf, "%u%s%hu", VERSION2MAGIC(server_version),
		      real_name, GetPortNum(ibuf->sock));

	switch (c) {

	    /*
	     * Owner only commands:
	     */
	case 'K':
	    printf("Enter name of victim: ");
	    fflush(stdout);
	    gets(str);
	    str[MAX_NAME_LEN - 1] = '\0';
	    Packet_printf(ibuf, "%c%s", KICK_PLAYER_pack, str);
	    break;
	case 'R':
	    printf("Enter maximum number of robots: ");
	    fflush(stdout);
	    gets(str);
	    if (sscanf(str, "%d", &max_robots) <= 0) {
		max_robots = 0;
	    } else
		if (max_robots < 0)
		    max_robots = 0;
	    Packet_printf(ibuf, "%c%d", MAX_ROBOT_pack, max_robots);
	    break;
	case 'M':				/* Send a message to server. */
	    printf("Enter message: ");
	    fflush(stdout);
	    gets(str);
	    str[MAX_CHARS - 1] = '\0';
	    Packet_printf(ibuf, "%c%s", MESSAGE_pack, str);
	    break;

	    /*
	     * Public commands:
	     */
	case 'N':				/* Next server. */
	    return (false);
	    break;

	case 'S':				/* Report status. */
	    Packet_printf(ibuf, "%c", REPORT_STATUS_pack);
	    break;

	case 'D':				/* Shutdown */
	    if (!auto_shutdown) {
		printf("Enter delay in seconds or return for cancel: ");
		gets(str);
		/*
		 * No argument = cancel shutdown = arg_int=0
		 */
		if (sscanf(str, "%d", &delay) <= 0) {
		    delay = 0;
		} else
		    if (delay <= 0)
			delay = 1;

		printf("Enter reason: ");
		gets(str);
	    } else {
		strcpy(str, shutdown_reason);
		delay = 60;
	    }
	    str[MAX_CHARS - 1] = '\0';
	    Packet_printf(ibuf, "%c%d%s", SHUTDOWN_pack, delay, str);
	    break;

	case 'Q':
	    exit (0);
	    break;

	case 'L':				/* Lock the game. */
	    Packet_printf(ibuf, "%c", LOCK_GAME_pack);
	    break;

	case '\0':
	case 'J':				/* Trying to enter game. */
	    Packet_printf(ibuf, "%c%s%s%d", ENTER_GAME_pack,
			  nick_name, display, team);
	    break;

	case '?':
	case 'H':				/* Help. */
	default:
	    printf("CLIENT VERSION...: %s\n", TITLE);
	    printf("Supported commands are:\n"
		   "H/?  -   Help - this text.\n"
		   "N    -   Next server, skip this one.\n"
		   "S    -   list Status.\n"
		   "Q    -   Quit.\n"
		   "K    -   Kick a player.                (only owner)\n"
		   "M    -   send a Message.               (only owner)\n"
		   "L    -   Lock/unLock server access.    (only owner)\n"
		   "D(*) -   shutDown/cancel shutDown.     (only owner)\n"
		   "R(#) -   set maximum number of Robots. (only owner)\n"
		   "J or just Return enters the game.\n"
		   "(*) If you don't specify any delay, you will signal that\n"
		   "    the server should stop an ongoing shutdown.\n"
		   "(#) Not specifying the maximum number of robots is\n"
		   "    the same as specifying 0 robots.\n");

	    /*
	     * Next command.
	     */
	    continue;
	}

	if (write(ibuf->sock, ibuf->buf, ibuf->len) != ibuf->len) {
	    error("Couldn't send request to server (write)", ibuf->len);
	    exit(-1);
	}

	/*
	 * Get reply message.  If we failed, return false (next server).
	 */
	if (Get_reply_message(ibuf) <= 0) {
	    errno = 0;
	    error("No answer from server");
	    return (false);
	}
	if (Packet_scanf(ibuf, "%c%c", &reply_to, &status) <= 0) {
	    errno = 0;
	    error("Incomplete reply from server");
	    return (false);
	}

	/*
	 * Now try and interpret the result.
	 */
	errno = 0;
	switch (status) {

	case SUCCESS:
	    /*
	     * Oh glorious success.
	     */
	    switch (reply_to) {
	    case REPORT_STATUS_pack:
		/*
		 * Did the reply include a string?
		 */
		if (ibuf->len > ibuf->ptr - ibuf->buf
		    && (!auto_connect || list_servers)) {
		    if (list_servers)
			printf("SERVER HOST......: %s\n", server_host);
		    if (*ibuf->ptr != '\0') {
			if (ibuf->len < ibuf->size) {
			    ibuf->buf[ibuf->len] = '\0';
			} else {
			    ibuf->buf[ibuf->size - 1] = '\0';
			}
			printf("%s", ibuf->ptr);
			if (ibuf->ptr[strlen(ibuf->ptr) - 1] != '\n') {
			    printf("\n");
			}
		    }
		}
		break;

	    case SHUTDOWN_pack:
		if (delay == 0) {
		    puts("*** Shutdown stopped.");
		} else {
		    puts("*** Shutdown initiated.");
		}
		break;

	    case ENTER_GAME_pack:
		if (Packet_scanf(ibuf, "%hu", &port) <= 0) {
		    errno = 0;
		    error("Incomplete login reply from server");
		    login_port = -1;
		} else {
		    login_port = port;
		    printf("*** Login allowed\n");
		}
		break;

	    default:
		puts("*** Operation successful.");
		break;
	    }
	    break;

	case E_NOT_OWNER:
	    error("Permission denied, not owner");
	    break;
	case E_GAME_FULL:
	    error("Sorry, game full");
	    break;
	case E_TEAM_FULL:
	    error("Sorry, team %d is full", team);
	    break;
	case E_TEAM_NOT_SET:
	    error("Sorry, team play selected "
		  "and you haven't specified your team");
	    break;
	case E_GAME_LOCKED:
	    error("Sorry, game locked");
	    break;
	case E_NOT_FOUND:
	    error("That player is not logged on this server");
	    break;
	case E_IN_USE:
	    error("Your nick is already used");
	    break;
	case E_SOCKET:
	    error("Server can't setup socket");
	    break;
	case E_INVAL:
	    error("Invalid input parameters says the server (?)");
	    break;
	case E_VERSION:
	    error("We have an incompatible version says the server");
	    break;
	default:
	    error("Wrong status '%d'", status);
	    break;
	}

	if (list_servers)	/* If listing servers, go to next one */
	    return (false);

	if (auto_shutdown)	/* Do the same if we've sent a -shutdown */
	    return (false);

	/*
	 * If we wanted to enter the game and we were allowed to, return true
	 * (we are done).  If we weren't allowed, either return false (get next
	 * server) if we are auto_connecting or get next command if we aren't
	 * auto_connecting (interactive).
	 */
	if (reply_to == ENTER_GAME_pack) {
	    if (status == SUCCESS && login_port > 0) {
		return (true);
	    } else {
		if (auto_connect)
		    return (false);
	    }
	}

	/*
	 * Get next command.
	 */
    }

    /*NOTREACHED*/
}



/*
 * Setup a socket and a buffer for client-server messages.
 * We do this again for each server to prevent getting
 * old messages from past servers.
 */
static bool Connect_to_server(void)
{
    int			socket_i;		/* Info socket */
    sockbuf_t		ibuf;			/* info buffer */
    bool		result;

    if ((socket_i = CreateDgramSocket(0)) == -1) {
	error("Could not create info socket");
	exit(-1);
    }
    if (DgramConnect(socket_i, server_host, server_port) == -1) {
	error("Can't connect to server %s on port %d\n",
	      server_host, server_port);
	SocketClose(socket_i);
	return (false);
    }
    if (Sockbuf_init(&ibuf, socket_i, CLIENT_RECV_SIZE,
		     SOCKBUF_READ | SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1) {
	error("No memory for info buffer");
	SocketClose(socket_i);
	exit(-1);
    }
    result = Process_commands(&ibuf);
    SocketClose(socket_i);
    Sockbuf_cleanup(&ibuf);

    return result;
}



/*
 * Oh glorious main(), without thee we cannot exist.
 */
int main(int argc, char *argv[])
{
    int			i, n;
    struct passwd	*pwent;
    bool		connected = false;
#ifdef	LIMIT_ACCESS
    extern bool		Is_allowed(char *);
#endif
    void Parse_options(int *argcp, char **argvp, char *realName, char *host,
		       int *port, int *my_team, int *list, int *join, int *motd,
		       char *nickName, char *dispName, char *shut_msg);


    /*
     * --- Output copyright notice ---
     */
    printf("  Copyright " COPYRIGHT ".\n"
	   "  " TITLE " comes with ABSOLUTELY NO WARRANTY; "
	      "for details see the\n"
	   "  provided LICENSE file.\n\n");
    if (strcmp(LOCALGURU, "xpilot@cs.uit.no")) {
	printf("  " LOCALGURU " is responsible for the local installation.\n\n");
    }

    Argc = argc;
    Argv = argv;

    /*
     * --- Miscellaneous initialization ---
     */
#ifdef SUNCMW
    cmw_priv_init();
#endif /* CMW */
    init_error(argv[0]);
    contact_port = SERVER_PORT;
    initaddr();

    if ((socket_c = CreateDgramSocket(0)) == -1) {
	error("Could not create connection socket");
	SocketClose(socket_c);
	exit(-1);
    }
    if (Sockbuf_init(&sbuf, socket_c, CLIENT_RECV_SIZE,
		     SOCKBUF_READ | SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1) {
	error("No memory for contact buffer");
	SocketClose(socket_c);
	exit(-1);
    }

    /*
     * --- Setup core of pack ---
     */
#ifdef VMS
    getusername(real_name);
#else
    if ((pwent = getpwuid(geteuid())) == NULL
	|| pwent->pw_name[0] == '\0') {
	error("Can't get user info for user id %d", geteuid());
	exit(1);
    }
    strncpy(real_name, pwent->pw_name, sizeof(real_name) - 1);
#endif
    nick_name[0] = '\0';
    Sockbuf_clear(&sbuf);
    Packet_printf(&sbuf, "%u%s%hu%c", MAGIC,
		  real_name, GetPortNum(sbuf.sock), CONTACT_pack);


    /*
     * --- Check commandline arguments ---
     */
    Parse_options(&argc, argv, real_name, hostname,
		  &contact_port, &team, &list_servers, &auto_connect, &motd,
		  nick_name, display, shutdown_reason);

    if (list_servers) {
	auto_connect = true;
    }
    if (shutdown_reason[0] != '\0') {
	auto_shutdown = true;
	auto_connect = true;
    }

    /*
     * --- Message of the Day ---
     */
    if (motd)
	printfile(MOTDFILE);
    if (list_servers)
	printf("LISTING AVAILABLE SERVERS:\n");

#ifdef	LIMIT_ACCESS
    /*
     * If sysadm's have complained alot, check for free machines before
     * letting the user play.  If room is crowded, don't let him play.
     */
    if (!list_servers && Is_allowed(display) == false)
	exit (-1);
#endif

    /*
     * --- Try to contact server ---
     */
    if (argc > 1) {		/* Server specified on command line? */
	n = 0;
	for (i = 1; i < argc; i++) {
	    if (DgramSend(sbuf.sock, argv[i], contact_port,
			  sbuf.buf, sbuf.len) == -1) {
		error("Can't send contact request to server at %s port %d",
		      argv[i], contact_port);
	    } else {
		n++;
	    }
	}
	if (n == 0) {
	    exit(1);
	}

	if (Get_contact_message())
	    connected = Connect_to_server();

    } else {				/* Search for servers... */
#ifdef LINUX
	printf("If you have LINUX defined during compilation then\n"
	       "you need to specify which host to connect to.\n");
#else
#ifdef VMS
	/*
	 * VMS could cope with the old 'fudged' broadcast code
	 * Should add it back in
	 */
	printf("If you have VMS defined during compilation then\n"
	       "you need to specify which host to connect to.\n");
#else
	SetTimeout(10, 0);
	if (Query_all(sbuf.sock, contact_port, sbuf.buf, sbuf.len) == -1) {
	    error("Couldn't send contact requests");
	    exit(1);
	}
	D( printf("\n"); );

	/*
	 * Wait for answer.
	 */
	while (Get_contact_message()) {
	    if ((connected = Connect_to_server()) != 0) {
		break;
	    }
	}
#endif
#endif
    }

    close(socket_c);
    Sockbuf_cleanup(&sbuf);
    if (connected) {
	extern int Join(char *server, int port, char *real, char *nick,
			char *display, unsigned version);

	Join(server_host, login_port, real_name, nick_name, display,
	     server_version);
    }

    exit(connected==true ? 0 : -1);
}



#ifndef LINUX
#ifdef VMS
/*
 * Use old fudged broadcasting code
 */
int Query_all(int sockfd, int port, char *msg, int msglen)
{
    return -1;
}
#else

/*
 * Code which uses 'real' broadcasting to find server.  Provided by
 * Bert Gÿsbers.  Thanks alot!
 */

#ifndef MAX_INTERFACE
#define MAX_INTERFACE    16	/* Max. number of network interfaces. */
#endif


/*
 * Enable broadcasting on a (datagram) socket.
 */
static int Enable_broadcast(int sockfd)
{
    int         flag = 1;	/* Turn it ON */

    return setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST,
		      (void *)&flag, sizeof(flag));
}


/*
 * Query all hosts on a subnet one after another.
 * This should be avoided as much as possible.
 * It may cause network congestion and therefore fail,
 * because UDP is unreliable.
 * We only allow this horrible kludge for subnets with 8 or less
 * bits in the host part of the subnet mask.
 * Subnets with irregular subnet bits are properly handled (I hope).
 */
static int Query_subnet(int sockfd,
		 struct sockaddr_in *host_addr,
		 struct sockaddr_in *mask_addr,
		 char *msg,
		 int msglen)
{
    int i, nbits, max;
    unsigned long bit, mask, dest, host, hostmask, hostbits[256];
    struct sockaddr_in addr;

    addr = *host_addr;
    host = ntohl(host_addr->sin_addr.s_addr);
    mask = ntohl(mask_addr->sin_addr.s_addr);
    memset ((void *)hostbits, 0, sizeof hostbits);
    nbits = 0;
    hostmask = 0;

    /*
     * Only the lower 32 bits of an unsigned long are used.
     */
    for (bit = 1; (bit & 0xffffffff) != 0; bit <<= 1) {
	if ((mask & bit) != 0) {
	    continue;
	}
	if (nbits >= 8) {
	    /* break; ? */
	    error("too many host bits in subnet mask");
	    return (-1);
	}
	hostmask |= bit;
	for (i = (1 << nbits); i < 256; i++) {
	    if ((i & (1 << nbits)) != 0) {
		hostbits[i] |= bit;
	    }
	}
	nbits++;
    }
    if (nbits < 2) {
	error("malformed subnet mask");
	return (-1);
    }

    /*
     * The first and the last address are reserved for the subnet.
     * So, for an 8 bit host part only 254 hosts are tried, not 256.
     */
    max = (1 << nbits) - 2;
    for (i=1; i <= max; i++) {
	dest = (host & ~hostmask) | hostbits[i];
	addr.sin_addr.s_addr = htonl(dest);
	sendto(sockfd, msg, msglen, 0,
	       (struct sockaddr *)&addr, sizeof(addr));
	D( printf("sendto %s/%d\n",
		  inet_ntoa(addr.sin_addr), ntohs(addr.sin_port)); );
	/*
	 * Imagine a server responding to our query while we
	 * are still transmitting packets for non-existing servers
	 * and the server packet colliding with one of our packets.
	 */
	usleep(10000);
    }

    return 0;
}


/*
 * Send a datagram on all network interfaces of the local host.  Return the
 * number of packets succesfully transmitted.
 * We only use the loopback interface if we didn't do a broadcast
 * on one of the other interfaces in order to reduce the chance that
 * we get multiple responses from the same server.
 */
static int Query_all(int sockfd, int port, char *msg, int msglen)
{
    int         	fd, len, ifflags, count = 0, broadcasts = 0, haslb = 0;
    struct sockaddr_in	addr, mask, loopback;
    struct ifconf	ifconf;
    struct ifreq	*ifreqp, ifreq, ifbuf[MAX_INTERFACE];

    /*
     * Broadcasting on a socket MUST be explicitly enabled.  This seems to be
     * insider information.  8-!
     */
    if (Enable_broadcast(sockfd) == -1) {
	error("set broadcast");
	return (-1);
    }

    /*
     * Create an unbound datagram socket.  Only used for ioctls.
     */
    if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
	error("socket");
	return (-1);
    }

    /*
     * Get names and addresses of all local network interfaces.
     */
    ifconf.ifc_len = sizeof(ifbuf);
    ifconf.ifc_buf = (caddr_t)ifbuf;
    memset((void *)ifbuf, 0, sizeof(ifbuf));

    if (ioctl(fd, SIOCGIFCONF, (char *)&ifconf) == -1) {
	error("ioctl SIOCGIFCONF");
	close(fd);
	return (-1);
    }
    for (len = 0; len + sizeof(struct ifreq) <= ifconf.ifc_len;) {
	ifreqp = (struct ifreq *)&ifconf.ifc_buf[len];

	D( printf("interface name %s\n", ifreqp->ifr_name); );
	D( printf("\taddress family %d\n", ifreqp->ifr_addr.sa_family); );

	len += sizeof(struct ifreq);
#if BSD >= 199006 || HAVE_SA_LEN
	/*
	 * Recent TCP/IP implementations have a sa_len member in the socket
	 * address structure in order to support protocol families that have
	 * bigger addresses.
	 */
	if (ifreqp->ifr_addr.sa_len > sizeof(ifreqp->ifr_addr)) {
	    len += ifreqp->ifr_addr.sa_len - sizeof(ifreqp->ifr_addr);
	    D( printf("\textra address length %d\n",
		      ifreqp->ifr_addr.sa_len - sizeof(ifreqp->ifr_addr)); );
	}
#endif
	if (ifreqp->ifr_addr.sa_family != AF_INET) {
	    /*
	     * Not supported.
	     */
	    continue;
	}

	addr = *(struct sockaddr_in *)&ifreqp->ifr_addr;
	D( printf("\taddress %s\n", inet_ntoa(addr.sin_addr)); );

	/*
	 * Get interface flags.
	 */
	ifreq = *ifreqp;
	if (ioctl(fd, SIOCGIFFLAGS, (char *)&ifreq) == -1) {
	    error("ioctl SIOCGIFFLAGS");
	    continue;
	}
	ifflags = ifreq.ifr_flags;

	if ((ifflags & IFF_UP) == 0) {
	    D( printf("\tinterface is down\n"); );
	    continue;
	}
	D( printf("\tinterface %s running\n",
		  (ifflags & IFF_RUNNING) ? "is" : "not"); );

	if ((ifflags & IFF_LOOPBACK) != 0) {
	    D( printf("\tloopback interface\n"); );
	    /*
	     * Only send on the loopback if we don't broadcast.
	     */
	    loopback = *(struct sockaddr_in *)&ifreq.ifr_addr;
	    haslb = 1;
	    continue;
	} else if ((ifflags & IFF_POINTOPOINT) != 0) {
	    D( printf("\tpoint-to-point interface\n"); );
	    ifreq = *ifreqp;
	    if (ioctl(fd, SIOCGIFDSTADDR, (char *)&ifreq) == -1) {
		error("ioctl SIOCGIFDSTADDR");
		continue;
	    }
	    addr = *(struct sockaddr_in *)&ifreq.ifr_addr;
	    D(printf("\tdestination address %s\n", inet_ntoa(addr.sin_addr)););
	} else if ((ifflags & IFF_BROADCAST) != 0) {
	    D( printf("\tbroadcast interface\n"); );
	    ifreq = *ifreqp;
	    if (ioctl(fd, SIOCGIFBRDADDR, (char *)&ifreq) == -1) {
		error("ioctl SIOCGIFBRDADDR");
		continue;
	    }
	    addr = *(struct sockaddr_in *)&ifreq.ifr_addr;
	    D( printf("\tbroadcast address %s\n", inet_ntoa(addr.sin_addr)); );
	} else {
	    /*
	     * Huh?  It's not a loopback and not a point-to-point
	     * and it doesn't have a broadcast address???
	     * Something must be rotten here...
	     */
	}

	if ((ifflags & (IFF_LOOPBACK|IFF_POINTOPOINT|IFF_BROADCAST)) != 0) {
	    /*
	     * Well, we have an address (at last).
	     */
	    addr.sin_port = htons(port);
	    if (sendto(sockfd, msg, msglen, 0,
		       (struct sockaddr *)&addr, sizeof addr) == msglen) {
		D(printf("\tsendto %s/%d\n", inet_ntoa(addr.sin_addr), port););
		/*
		 * Success!
		 */
		count++;
		if ((ifflags & (IFF_LOOPBACK|IFF_POINTOPOINT|IFF_BROADCAST))
		    == IFF_BROADCAST) {
		    broadcasts++;
		}
		continue;
	    }

	    /*
	     * Failure.
	     */
	    error("sendto %s/%d failed", inet_ntoa(addr.sin_addr), port);

	    if ((ifflags & (IFF_LOOPBACK|IFF_POINTOPOINT|IFF_BROADCAST))
		!= IFF_BROADCAST) {
		/*
		 * It wasn't the broadcasting that failed.
		 */
		continue;
	    }
	    
	    /*
	     * Broadcasting failed.
	     * Try it in a different (kludgy) manner.
	     */
	}

	/*
	 * Get the netmask for this interface.
	 */
	ifreq = *ifreqp;
	if (ioctl(fd, SIOCGIFNETMASK, (char *)&ifreq) == -1) {
	    error("ioctl SIOCGIFNETMASK");
	    continue;
	}
	mask = *(struct sockaddr_in *)&ifreq.ifr_addr;
	D( printf("\tmask %s\n", inet_ntoa(mask.sin_addr)); );

	addr.sin_port = htons(port);
	if (Query_subnet(sockfd, &addr, &mask, msg, msglen) != -1) {
	    count++;
	    broadcasts++;
	}
    }

    if (broadcasts == 0 && haslb) {
	/*
	 * We didn't reach the localhost yet.
	 */
	memset(&addr, 0, sizeof(addr));
	addr.sin_addr = loopback.sin_addr;
	addr.sin_port = htons(port);
	if (sendto(sockfd, msg, msglen, 0,
		   (struct sockaddr *)&addr, sizeof addr) == msglen) {
	    D(printf("\tsendto %s/%d\n", inet_ntoa(addr.sin_addr), port););
	    count++;
	} else {
	    error("sendto %s/%d failed", inet_ntoa(addr.sin_addr), port);
	}
    }

    close(fd);

    if (count == 0) {
	errno = 0;
	count = -1;
    }

    return count;
}
#endif
#endif

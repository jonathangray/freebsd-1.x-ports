/*
 * time - obtain the time from the remote server
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * Obtain the current time from the remote server in standard unix time
 * format for use with the next NEWNEWS. If the client is unable to
 * connect to the time server or the read fails then the error is
 * reported and the program is exited.
 *
 * $Id: time.c,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: time.c,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/07  11:20:27  root
 * Removed unistd.h inclusion.
 * Display time since epoch at server as unsigned.
 *
 * Revision 1.5  1993/03/01  18:09:12  root
 * Made the epoch constant an unsigned long.
 *
 * Revision 1.4  1993/02/14  15:10:01  root
 * No changes.
 *
 * Revision 1.0  1992/08/92
 * Initial coding.
 *
 */

#include "slurp.h"

#include <netinet/in.h>


	time_t
server_time (char *hostname)
	{
	int server, ret;
	u_long timebuf;

/* First open the socket */

	if ((server = tcp_open (hostname, "time")) < 0)
		return ((time_t) 0);

	ret = read (server, &timebuf, 4);

/* Close the socket and check we got 4 bytes */

	(void) close (server);

	if (ret != 4)
		{
		log_ret ("server_time: Read error on time server socket");
		return ((time_t) 0);
		}

/* Convert byte order if needed */

	timebuf = ntohl (timebuf);

	if (debug_flag)
		(void) fprintf (stderr, "time is currently %lu at server %s\n",
						timebuf, hostname);

/* Convert the time from seconds since 1900 to seconds since 1970 */

	return ((time_t) (timebuf - 2208988800UL));
	}

/* END-OF-FILE */

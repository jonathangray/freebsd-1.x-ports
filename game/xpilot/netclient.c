/* $Id: netclient.c,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#include "types.h"
#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#include <sys/param.h>
#endif
#if defined(__hpux) || defined(VMS)
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#ifdef VMS
#include <socket.h>
#include <in.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#endif
#include <netdb.h>

#include "version.h"
#include "error.h"
#include "net.h"
#include "netclient.h"
#include "setup.h"
#include "packet.h"
#include "bit.h"
#include "paint.h"
#include "xinit.h"
#include "pack.h"
#include "draw.h"
#include "client.h"

#define TALK_RETRY	FPS


/*
 * Type definitions.
 */
typedef struct {
    long		loops;
    sockbuf_t		sbuf;
} frame_t;

/*
 * Exported variables.
 */
setup_t			*Setup;
int			receive_window_size;

/*
 * Local variables.
 */
static sockbuf_t	rbuf,
			cbuf,
			wbuf;
static frame_t		*Frames;
static int		(*receive_tbl[256])(void),
			(*reliable_tbl[256])(void);
static unsigned		magic;
static long		last_loops,
			last_keyboard_change,
			last_keyboard_ack,
			last_keyboard_update,
			last_send_anything,
			reliable_offset,
			talk_pending,
			talk_sequence_num,
			talk_last_send;
static char		talk_str[MAX_CHARS];

/*
 * Initialise the function dispatch tables.
 * There are two tables.  One for the semi-important unreliable
 * data like frame updates.
 * The other one is for the reliable data stream, which is
 * received as part of the unreliable data packets.
 */
static void Receive_init(void)
{
    int i;

    for (i = 0; i < 256; i++) {
	receive_tbl[i] = NULL;
	reliable_tbl[i] = NULL;
    }

    receive_tbl[PKT_EYES]	= Receive_eyes;
    receive_tbl[PKT_TIME_LEFT]	= Receive_time_left;
    receive_tbl[PKT_AUDIO]	= Receive_audio;
    receive_tbl[PKT_START]	= Receive_start;
    receive_tbl[PKT_END]	= Receive_end;
    receive_tbl[PKT_SELF]	= Receive_self;
    receive_tbl[PKT_DAMAGED]	= Receive_damaged;
    receive_tbl[PKT_CONNECTOR]	= Receive_connector;
    receive_tbl[PKT_LASER]	= Receive_laser;
    receive_tbl[PKT_REFUEL]	= Receive_refuel;
    receive_tbl[PKT_SHIP]	= Receive_ship;
    receive_tbl[PKT_ECM]	= Receive_ecm;
    receive_tbl[PKT_TRANS]	= Receive_trans;
    receive_tbl[PKT_PAUSED]	= Receive_paused;
    receive_tbl[PKT_ITEM]	= Receive_item;
    receive_tbl[PKT_MINE]	= Receive_mine;
    receive_tbl[PKT_BALL]	= Receive_ball;
    receive_tbl[PKT_SMART]	= Receive_smart;
    receive_tbl[PKT_SHUTDOWN]	= Receive_shutdown;
    receive_tbl[PKT_DESTRUCT]	= Receive_destruct;
    receive_tbl[PKT_FUEL]	= Receive_fuel;
    receive_tbl[PKT_CANNON]	= Receive_cannon;
    receive_tbl[PKT_TARGET]	= Receive_target;
    receive_tbl[PKT_RADAR]	= Receive_radar;
    receive_tbl[PKT_RELIABLE]	= Receive_reliable;
    receive_tbl[PKT_QUIT]	= Receive_quit;
    receive_tbl[PKT_SHOT+0]	= Receive_shot;
    receive_tbl[PKT_SHOT+1]	= Receive_shot;
    receive_tbl[PKT_SHOT+2]	= Receive_shot;
    receive_tbl[PKT_SHOT+3]	= Receive_shot;
    for (i = 0; i < DEBRIS_TYPES; i++) {
	receive_tbl[PKT_DEBRIS + i] = Receive_debris;
    }

    reliable_tbl[PKT_MESSAGE]	= Receive_message;
    reliable_tbl[PKT_PLAYER]	= Receive_player;
    reliable_tbl[PKT_SCORE]	= Receive_score;
    reliable_tbl[PKT_LEAVE]	= Receive_leave;
    reliable_tbl[PKT_WAR]	= Receive_war;
    reliable_tbl[PKT_SEEK]	= Receive_seek;
    reliable_tbl[PKT_BASE]	= Receive_base;
    reliable_tbl[PKT_QUIT]	= Receive_quit;
    reliable_tbl[PKT_STRING]	= Receive_string;
    reliable_tbl[PKT_SCORE_OBJECT] = Receive_score_object;
    reliable_tbl[PKT_TALK_ACK]	= Receive_talk_ack;
}

/*
 * Uncompress the map which is compressed using a simple
 * Run-Length-Encoding algorithm.
 * The map object type is encoded in the lower seven bits
 * of a byte.
 * If the high bit of a byte is set then the next byte
 * means the number of contiguous map data bytes that
 * have the same type.  Otherwise only one map byte
 * has this type.
 * Because we uncompress the map backwards to save on
 * memory usage there is some complexity involved.
 */
static int Uncompress_map(void)
{
    u_byte	*cmp,		/* compressed map pointer */
		*ump,		/* uncompressed map pointer */
		*p;		/* temporary search pointer */
    int		i,
		count;

    if (Setup->map_order != SETUP_MAP_ORDER_XY) {
	errno = 0;
	error("Unknown map ordering in setup (%d)", Setup->map_order);
	return -1;
    }

    /* Point to last compressed map byte */
    cmp = Setup->map_data + Setup->map_data_len - 1;

    /* Point to last uncompressed map byte */
    ump = Setup->map_data + Setup->x * Setup->y - 1;

    while (cmp >= Setup->map_data) {
	for (p = cmp; p > Setup->map_data; p--) {
	    if ((p[-1] & SETUP_COMPRESSED) == 0) {
		break;
	    }
	}
	if (p == cmp) {
	    *ump-- = *cmp--;
	    continue;
	}
	if ((cmp - p) % 2 == 0) {
	    *ump-- = *cmp--;
	}
	while (p < cmp) {
	    count = *cmp--;
	    if (count < 2) {
		errno = 0;
		error("Map compress count error %d", count);
		return -1;
	    }
	    *cmp &= ~SETUP_COMPRESSED;
	    for (i = 0; i < count; i++) {
		*ump-- = *cmp;
	    }
	    cmp--;
	    if (ump < cmp) {
		errno = 0;
		error("Map uncompression error (%d,%d)",
		    cmp - Setup->map_data, ump - Setup->map_data);
		return -1;
	    }
	}
    }
    if (ump != cmp) {
	errno = 0;
	error("map uncompress error (%d,%d)",
	    cmp - Setup->map_data, ump - Setup->map_data);
	return -1;
    }
    Setup->map_order = SETUP_MAP_UNCOMPRESSED;
    return 0;
}

/*
 * Receive the map data and some game parameters from 
 * the server.  The map data may be in compressed form.
 */
int Net_setup(void)
{
    int		n,
		len,
		size,
		done = 0,
		retries;
    long	todo = sizeof(setup_t);
    char	*ptr;

    if ((Setup = (setup_t *) malloc(sizeof(setup_t))) == NULL) {
	error("No memory for setup data");
	return -1;
    }
    ptr = (char *) Setup;
    while (todo > 0) {
	if (cbuf.ptr != cbuf.buf) {
	    Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
	}
	len = cbuf.len;
	if (len > todo) {
	    len = todo;
	}
	if (len > 0) {
	    if (done == 0) {
		n = Packet_scanf(&cbuf,
				 "%ld" "%ld%hd" "%hd%hd" "%hd%hd" "%s%s",
				 &Setup->map_data_len,
				 &Setup->mode, &Setup->lives,
				 &Setup->x, &Setup->y,
				 &Setup->frames_per_second, &Setup->map_order,
				 Setup->name, Setup->author);
		if (n <= 0) {
		    errno = 0;
		    error("Can't read setup info from reliable data buffer");
		    return -1;
		}
		/*
		 * Do some consistency checks on the server setup structure.
		 */
		if (Setup->map_data_len <= 0
		    || Setup->x <= 0
		    || Setup->y <= 0
		    || Setup->map_data_len > Setup->x * Setup->y) {
		    errno = 0;
		    error("Got bad map specs from server (%d,%d,%d)",
			Setup->map_data_len, Setup->x, Setup->y);
		    return -1;
		}
		Setup->width = Setup->x * BLOCK_SZ;
		Setup->height = Setup->y * BLOCK_SZ;
		if (Setup->map_order != SETUP_MAP_ORDER_XY
		    && Setup->map_order != SETUP_MAP_UNCOMPRESSED) {
		    errno = 0;
		    error("Unknown map order type (%d)", Setup->map_order);
		    return -1;
		}
		size = sizeof(setup_t) + Setup->x * Setup->y;
		if ((Setup = (setup_t *) realloc(ptr, size)) == NULL) {
		    error("No memory for setup and map");
		    return -1;
		}
		ptr = (char *) Setup;
		done = (char *) &Setup->map_data[0] - ptr;
		todo = Setup->map_data_len;
	    } else {
		memcpy(&ptr[done], cbuf.ptr, len);
		Sockbuf_advance(&cbuf, len + cbuf.ptr - cbuf.buf);
		done += len;
		todo -= len;
	    }
	}
	if (todo > 0) {
	    if (rbuf.ptr != rbuf.buf) {
		Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	    }
	    if (rbuf.len > 0) {
		if (rbuf.ptr[0] != PKT_RELIABLE) {
		    if (rbuf.ptr[0] == PKT_QUIT) {
			errno = 0;
			error("Server closed connection");
			return -1;
		    } else {
			errno = 0;
			error("Not a reliable packet (%d) in setup",
			    rbuf.ptr[0]);
			return -1;
		    }
		}
		if (Receive_reliable() == -1) {
		    return -1;
		}
		if (Sockbuf_flush(&wbuf) == -1) {
		    return -1;
		}
	    }
	    if (cbuf.ptr != cbuf.buf) {
		Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
	    }
	    if (cbuf.len > 0) {
		continue;
	    }
	    for (retries = 0;; retries++) {
		if (retries >= 10) {
		    errno = 0;
		    error("Can't read setup after %d retries "
			  "(todo=%d, left=%d)",
			  retries, todo, cbuf.len - (cbuf.ptr - cbuf.buf));
		    return -1;
		}
		SetTimeout(2, 0);
		while (SocketReadable(rbuf.sock) > 0) {
		    Sockbuf_clear(&rbuf);
		    if (Sockbuf_read(&rbuf) == -1) {
			error("Can't read all setup data");
			return -1;
		    }
		    if (rbuf.len > 0) {
			break;
		    }
		    SetTimeout(0, 0);
		}
		if (rbuf.len > 0) {
		    break;
		}
	    }
	}
    }
    if (Setup->map_order != SETUP_MAP_UNCOMPRESSED) {
	if (Uncompress_map() == -1) {
	    return -1;
	}
    }

    return 0;
}

/*
 * Send the first packet to the server with our name,
 * nick and display contained in it.
 * The server uses this data to verify that the packet
 * is from the right UDP connection, it already has 
 * this info from the ENTER_GAME_pack.
 */
int Net_verify(char *real, char *nick, char *disp)
{
    int		n,
		type,
		result,
		retries;
    time_t	last;

    for (retries = 0;;) {
	if (retries == 0
	    || time(NULL) - last >= 3) {
	    if (retries++ >= 10) {
		errno = 0;
		error("Can't connect to server after %d retries", retries);
		return -1;
	    }
	    Sockbuf_clear(&wbuf);
	    n = Packet_printf(&wbuf, "%c%s%s%s", PKT_VERIFY, real, nick, disp);
	    if (n <= 0
		|| Sockbuf_flush(&wbuf) <= 0) {
		error("Can't send verify packet");
		return -1;
	    }
	    time(&last);
#ifndef SILENT
	    printf("Waiting for verify response\n");
#endif
	}
	SetTimeout(1, 0);
	if (SocketReadable(rbuf.sock) == 0) {
	    continue;
	}
	Sockbuf_clear(&rbuf);
	if (Sockbuf_read(&rbuf) == -1) {
	    error("Can't read verify reply packet");
	    return -1;
	}
	if (rbuf.len <= 0) {
	    continue;
	}
	if (rbuf.ptr[0] != PKT_RELIABLE) {
	    if (rbuf.ptr[0] == PKT_QUIT) {
		errno = 0;
		error("Server closed connection");
		return -1;
	    } else {
		errno = 0;
		error("Bad packet type when verifying (%d)", rbuf.ptr[0]);
		return -1;
	    }
	}
	if (Receive_reliable() == -1) {
	    return -1;
	}
	if (Sockbuf_flush(&wbuf) == -1) {
	    return -1;
	}
	if (cbuf.len == 0) {
	    continue;
	}
	if (Receive_reply(&type, &result) <= 0) {
	    errno = 0;
	    error("Can't receive verify reply packet");
	    return -1;
	}
	if (type != PKT_VERIFY) {
	    errno = 0;
	    error("Verify wrong reply type (%d)", type);
	    return -1;
	}
	if (result != PKT_SUCCESS) {
	    errno = 0;
	    error("Verification failed (%d)", result);
	    return -1;
	}
	if (Receive_magic() <= 0) {
	    error("Can't receive magic packet after verify");
	    return -1;
	}
	break;
    }
#ifndef SILENT
    printf("Verified correctly\n");
#endif
    return 0;
}

/*
 * Open the datagram socket and allocate the network data
 * structures like buffers.
 * Currently there are three different buffers used:
 * 1) wbuf is used only for sending packets (write/printf).
 * 2) rbuf is used for receiving packets in (read/scanf).
 * 3) cbuf is used to copy the reliable data stream
 *    into from the raw and unreliable rbuf packets.
 */
int Net_init(char *server, int port)
{
    int			i,
			sock;
    unsigned		size;

    signal(SIGPIPE, SIG_IGN);

    Receive_init();

    if ((sock = CreateDgramSocket(0)) == -1) {
	error("Can't create datagram socket");
	return -1;
    }
    if (DgramConnect(sock, server, port) == -1) {
	error("Can't connect to server %s on port %d", server, port);
	close(sock);
	return -1;
    }
    wbuf.sock = sock;
    if (SetSocketNonBlocking(sock, 1) == -1) {
	error("Can't make socket non-blocking");
	return -1;
    }
    if (SetSocketSendBufferSize(sock, CLIENT_SEND_SIZE + 16) == -1) {
	error("Can't set send buffer size to %d", CLIENT_SEND_SIZE + 16);
    }
    if (SetSocketReceiveBufferSize(sock, CLIENT_RECV_SIZE + 16) == -1) {
	error("Can't set receive buffer size to %d", CLIENT_RECV_SIZE + 16);
    }

    size = receive_window_size * sizeof(frame_t);
    if ((Frames = (frame_t *) malloc(size)) == NULL) {
	error("No memory (%u)", size);
	return -1;
    }
    for (i = 0; i < receive_window_size; i++) {
	Frames[i].loops = 0;
	if (Sockbuf_init(&Frames[i].sbuf, sock, CLIENT_RECV_SIZE,
			 SOCKBUF_READ | SOCKBUF_DGRAM) == -1) {
	    error("No memory for read buffer (%u)", CLIENT_RECV_SIZE);
	    return -1;
	}
    }

    /* reliable data buffer, not a valid socket filedescriptor needed */
    if (Sockbuf_init(&cbuf, -1, CLIENT_RECV_SIZE,
		     SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK) == -1) {
	error("No memory for control buffer (%u)", CLIENT_RECV_SIZE);
	return -1;
    }

    /* write buffer */
    if (Sockbuf_init(&wbuf, sock, CLIENT_SEND_SIZE,
		     SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1) {
	error("No memory for write buffer (%u)", CLIENT_SEND_SIZE);
	return -1;
    }

    /* read buffer */
    rbuf = Frames[0].sbuf;

    /* reliable data byte stream offset */
    reliable_offset = 0;

    /* reset talk status */
    talk_sequence_num = 0;
    talk_pending = 0;

    return 0;
}

/* 
 * Cleanup all the network buffers and close the datagram socket.
 * Also try to send the server a quit packet if possible.
 * Because this quit packet may get lost we send one at the
 * beginning and one at the end.
 */
void Net_cleanup(void)
{
    int		i,
		sock = wbuf.sock;
    u_byte	ch;

    if (sock > 2) {
	ch = PKT_QUIT;
	if (write(sock, &ch, 1) != 1) {
	    GetSocketError(sock);
	    write(sock, &ch, 1);
	}
	usleep(50*1000);
    }
    if (Frames != NULL) {
	for (i = 0; i < receive_window_size; i++) {
	    if (Frames[i].sbuf.buf != NULL) {
		Sockbuf_cleanup(&Frames[i].sbuf);
	    } else {
		break;
	    }
	}
	free(Frames);
	Frames = NULL;
    }
    Sockbuf_cleanup(&cbuf);
    Sockbuf_cleanup(&wbuf);
    if (Setup != NULL) {
	free(Setup);
	Setup = NULL;
    }
    if (sock > 2) {
	ch = PKT_QUIT;
	if (write(sock, &ch, 1) != 1) {
	    GetSocketError(sock);
	    write(sock, &ch, 1);
	}
	usleep(50*1000);
	if (write(sock, &ch, 1) != 1) {
	    GetSocketError(sock);
	    write(sock, &ch, 1);
	}
	close(sock);
    }
}

/*
 * Calculate a new `keyboard-changed-id' which the server has
 * to ack.  To save space in the server-to-client packets only
 * a byte is used which gives some trouble with overflow.
 */
void Net_key_change(void)
{
    last_keyboard_change = ((last_keyboard_change + 1) & 0xFF);
    if ((last_keyboard_change > last_keyboard_ack)
	? (last_keyboard_change - last_keyboard_ack >= 128)
	: (last_keyboard_change + 256 - last_keyboard_ack >= 128)) {
	errno = 0;
	error("Server is ignoring keys (%d,%d)",
	    last_keyboard_change, last_keyboard_ack);
	last_keyboard_change = ((last_keyboard_ack + 127) & 0xFF);
    }
    Key_update();
}

/*
 * Flush the network output buffer if it has some data in it.
 * Called by the main loop before blocking on a select(2) call.
 */
int Net_flush(void)
{
    if (wbuf.len == 0) {
	wbuf.ptr = wbuf.buf;
	return 0;
    }
    Send_talk();
    if (Sockbuf_flush(&wbuf) == -1) {
	return -1;
    }
    Sockbuf_clear(&wbuf);
    last_send_anything = last_loops;
    return 1;
}

/*
 * Return the socket filedescriptor for use in a select(2) call.
 */
int Net_fd(void)
{
    return rbuf.sock;
}

/*
 * Try to send a `start play' packet to the server and get an
 * acknowledgement from the server.  This is called after
 * we have initialised all our other stuff like the user interface
 * and we also have the map already.
 */
int Net_start(void)
{
    int			retries,
			type,
			result;
    time_t		last;

    for (retries = 0;;) {
	if (retries == 0
	    || (time(NULL) - last) > 1) {
	    if (retries++ >= 10) {
		errno = 0;
		error("Can't start play after %d retries", retries);
		return -1;
	    }
	    Sockbuf_clear(&wbuf);
	    if (Packet_printf(&wbuf, "%c", PKT_PLAY) <= 0
		|| Client_power() == -1
	    	|| Sockbuf_flush(&wbuf) == -1) {
		error("Can't send start play packet");
		return -1;
	    }
	    time(&last);
	}
	if (cbuf.ptr > cbuf.buf) {
	    Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
	}
	SetTimeout(2, 0);
	while (cbuf.len <= 0
	    && SocketReadable(rbuf.sock) != 0) {
	    Sockbuf_clear(&rbuf);
	    if (Sockbuf_read(&rbuf) == -1) {
		error("Error reading play reply");
		return -1;
	    }
	    if (rbuf.len <= 0) {
		continue;
	    }
	    if (rbuf.ptr[0] != PKT_RELIABLE) {
		if (rbuf.ptr[0] == PKT_QUIT) {
		    errno = 0;
		    error("Server closed connection");
		    return -1;
		}
		else if (rbuf.ptr[0] == PKT_START) {
		    /*
		     * Packet out of order, drop it or...
		     * Skip the frame and check for a Reliable Data Packet.
		     * (HACK)
		     * In a future version we may not want a reply to
		     * the PKT_PLAY request and accept frames immediately.
		     */
		    while (++rbuf.ptr < rbuf.buf + rbuf.len) {
			if (rbuf.ptr[0] == PKT_END
			    && rbuf.ptr + 5 < rbuf.buf + rbuf.len
			    && rbuf.ptr[5] == PKT_RELIABLE) {
			    rbuf.ptr += 5;
			    break;
			}
		    }
		    if (rbuf.ptr + 11 >= rbuf.buf + rbuf.len) {
			printf("skipping unexpected frame while starting\n");
			continue;
		    }
		    printf("abusing unexpected frame while starting\n");
		} else {
		    printf("strange packet type while starting (%d)\n",
			rbuf.ptr[0]);
		    /*
		     * What the hack do we care when we wanna play.
		     * Just drop the packet for now.
		     */
		    Sockbuf_clear(&rbuf);
		    continue;
		}
	    }
	    if (Receive_reliable() == -1) {
		return -1;
	    }
	    if (Sockbuf_flush(&wbuf) == -1) {
		return -1;
	    }
	}
	if (cbuf.ptr - cbuf.buf >= cbuf.len) {
	    continue;
	}
	if (cbuf.ptr[0] != PKT_REPLY) {
	    errno = 0;
	    error("Not a reply packet after play (%d,%d,%d)",
		cbuf.ptr[0], cbuf.ptr - cbuf.buf, cbuf.len);
	    return -1;
	}
	if (Receive_reply(&type, &result) <= 0) {
	    errno = 0;
	    error("Can't receive reply packet after play");
	    return -1;
	}
	if (type != PKT_PLAY) {
	    errno = 0;
	    error("Can't receive reply packet after play");
	    return -1;
	}
	if (result != PKT_SUCCESS) {
	    errno = 0;
	    error("Start play not allowed (%d)", result);
	    return -1;
	}
	break;
    }
    packet_measure = NULL;
    Net_init_measurement();
    errno = 0;
    return 0;
}

void Net_init_measurement(void)
{
    packet_loss = 0;
    packet_drop = 0;
    packet_loop = 0;
    if (BIT(instruments, SHOW_PACKET_LOSS_METER|SHOW_PACKET_DROP_METER) != 0) {
	if (packet_measure == NULL) {
	    if ((packet_measure = (char *) malloc(FPS)) == NULL) {
		error("No memory for packet measurement");
		CLR_BIT(instruments,
			SHOW_PACKET_LOSS_METER|SHOW_PACKET_DROP_METER);
	    } else {
		memset(packet_measure, PACKET_DRAW, FPS);
	    }
	}
    }
    else if (packet_measure != NULL) {
	free(packet_measure);
	packet_measure = NULL;
    }
}

/*
 * Process a packet which most likely is a frame update,
 * perhaps with some reliable data in it.
 */
static int Net_packet(void)
{
    int		type,
		result,
		replyto,
		status;

    while (rbuf.buf + rbuf.len > rbuf.ptr) {
	type = (*rbuf.ptr & 0xFF);
	if (receive_tbl[type] == NULL) {
	    errno = 0;
	    error("Received unknown packet type (%d)", type);
	    return -1;
	}
	else if ((result = (*receive_tbl[type])()) <= 0) {
	    if (result == -1) {
		if (type != PKT_QUIT) {
		    errno = 0;
		    error("Processing packet type (%d) failed", type);
		}
		return -1;
	    }
	    /* Drop rest of incomplete packet */
	    Sockbuf_clear(&rbuf);
	    break;
	}
    }
    while (cbuf.buf + cbuf.len > cbuf.ptr) {
	type = (*cbuf.ptr & 0xFF);
	if (type == PKT_REPLY) {
	    if ((result = Receive_reply(&replyto, &status)) <= 0) {
		if (result == 0) {
		    break;
		}
		return -1;
	    }
	    /* should do something more appropriate than this with the reply */
	    errno = 0;
	    error("Got reply packet (%d,%d)", replyto, status);
	}
	else if (reliable_tbl[type] == NULL) {
	    errno = 0;
	    error("Received unknown reliable data packet type (%d)", type);
	    return -1;
	}
	else if ((result = (*reliable_tbl[type])()) <= 0) {
	    if (result == 0) {
		break;
	    }
	    return -1;
	}
    }

    return 0;
}

/*
 * Do some (simple) packet loss/drop measurement
 * the results of which can be drawn on the display.
 * This is mainly for debugging and analysis.
 */
static void Net_measurement(long loop, int status)
{
    int		i;
    long	delta;

    if (packet_measure == NULL) {
	return;
    }
    if ((delta = loop - packet_loop) < 0) {
	/*
	 * Duplicate or out of order.
	 */
	return;
    }
    if (delta >= FPS) {
	if (packet_loop == 0) {
	    packet_loop = loop - (loop % FPS);
	    return;
	}
	packet_loop = loop - (loop % FPS);
	packet_loss = 0;
	packet_drop = 0;
	for (i = 0; i < FPS; i++) {
	    switch (packet_measure[i]) {
	    case PACKET_LOSS:
		packet_loss++;
		continue;
	    case PACKET_DROP:
		packet_drop++;
		break;
	    }
	    packet_measure[i] = PACKET_LOSS;
	}
	delta = loop - packet_loop;
    }
   if (packet_measure[(int)delta] != PACKET_DRAW) {
       packet_measure[(int)delta] = status;
   }
}

/*
 * Read a packet into one of the input buffers.
 * If it is a frame update then we check to see
 * if it is an old or duplicate one.  If it isn't
 * a new frame then the packet is discarded and
 * we retry to read a packet once more.
 * It's a non-blocking read.
 */
static int Net_read(frame_t *frame)
{
    int		n;
    long	loop;
    u_byte	ch;

    frame->loops = 0;
    for (;;) {
	Sockbuf_clear(&frame->sbuf);
	if (Sockbuf_read(&frame->sbuf) == -1) {
	    error("Net input error");
	    return -1;
	}
	if (frame->sbuf.len <= 0) {
	    Sockbuf_clear(&frame->sbuf);
	    return 0;
	}
	if (frame->sbuf.ptr[0] != PKT_START) {
	    /*
	     * Don't know which type of packet this is
	     * and if it contains a frame at all (not likely).
	     * It could be a quit packet.
	     */
	    return 1;
	}
	n = Packet_scanf(&frame->sbuf, "%c%ld", &ch, &loop);
	frame->sbuf.ptr = frame->sbuf.buf;
	if (n <= 0) {
	    if (n == -1) {
		Sockbuf_clear(&frame->sbuf);
		return -1;
	    }
	    continue;
	}
	else if (loop > last_loops) {
	    frame->loops = loop;
	    return 2;
	} else {
	    /*
	     * Packet out of order.  Drop it.
	     * We may have already drawn it if it is duplicate.
	     * Perhaps we should try to extract any reliable data
	     * from it before dropping it.
	     */
	}
    }
}

/*
 * Read frames from the net until there are no more available.
 * If the server has floaded us with frame updates then we should
 * discard everything except the most recent ones.  The X server
 * may be too slow to keep up with the rate of the XPilot server
 * or there may have been a network hickup if the net is overloaded.
 */
int Net_input(void)
{
    int		i,
		j,
		n;
    frame_t	*frame,
		*last_frame,
		*oldest_frame = &Frames[0],
		tmpframe;

    for (i = 0; i < receive_window_size; i++) {
	frame = &Frames[i];
	if (frame->loops != 0) {
	    /*
	     * Already contains a frame.
	     */
	    if (frame->loops < oldest_frame->loops
		|| oldest_frame->loops == 0) {
		oldest_frame = frame;
	    }
	}
	else if (frame->sbuf.len > 0
	    && frame->sbuf.ptr == frame->sbuf.buf) {
	    /*
	     * Contains an unidentifiable packet.
	     * No more input until this one is processed.
	     */
	    break;
	} else {
	    /*
	     * Empty buffer.  Read a frame.
	     */
	    if ((n = Net_read(frame)) <= 0) {
		if (n == 0) {
		    /*
		     * No more new packets available.
		     */
		    if (i == 0) {
			/*
			 * No frames to be processed.
			 */
			return 0;
		    }
		    break;
		} else {
		    return n;
		}
	    }
	    else if (n == 1) {
		/*
		 * Contains an unidentifiable packet.
		 * No more input until this one is processed.
		 */
		break;
	    } else {
		/*
		 * Check for duplicate packets.
		 */
		for (j = i - 1; j >= 0; j--) {
		    if (frame->loops == Frames[j].loops) {
			break;
		    }
		}
		if (j >= 0) {
		    /*
		     * Duplicate.  Drop it.
		     */
		    Net_measurement(frame->loops, PACKET_DROP);
		    Sockbuf_clear(&frame->sbuf);
		    frame->loops = 0;
		    i--;	/* correct for for loop increment. */
		    continue;
		}
		if (frame->loops < oldest_frame->loops) {
		    oldest_frame = frame;
		}
	    }
	}
	if (i == receive_window_size - 1 && i > 0) {
	    /*
	     * Drop oldest packet.
	     */
	    if (oldest_frame->loops < frame->loops) {
		/*
		 * Switch buffers to prevent gaps.
		 */
		tmpframe = *oldest_frame;
		*oldest_frame = *frame;
		*frame = tmpframe;
	    }
	    Net_measurement(frame->loops, PACKET_DROP);
	    Sockbuf_clear(&frame->sbuf);
	    frame->loops = 0;
	    oldest_frame = &Frames[0];
	    /*
	     * Reset loop index.
	     */
	    i = -1;	/* correct for for loop increment. */
	    continue;
	}
    }

    /*
     * Find oldest packet.
     */
    last_frame = oldest_frame = &Frames[0];
    for (i = 1; i < receive_window_size; i++, last_frame++) {
	frame = &Frames[i];
	if (frame->loops == 0) {
	    if (frame->sbuf.len > 0) {
		/*
		 * This is an unidentifiable packet.
		 * Process it last, because it arrived last.
		 */
		continue;
	    } else {
		/*
		 * Empty.  The rest should be empty too,
		 * because we have taken care not to have gaps.
		 */
		break;
	    }
	}
	else if (frame->loops < oldest_frame->loops
	    || oldest_frame->loops == 0) {
	    oldest_frame = frame;
	}
    }

    if (oldest_frame->sbuf.len <= 0) {
	/*
	 * Couldn't find a non-empty packet.
	 */
	if (oldest_frame->loops > 0) {
	    errno = 0;
	    error("bug %s,%d", __FILE__, __LINE__);
	    oldest_frame->loops = 0;
	}
	return 0;
    }

    /*
     * Let the packet processing routines know which
     * packet they should process.
     */
    rbuf = oldest_frame->sbuf;

    /*
     * Process the packet.
     */
    n = Net_packet();

    if (last_frame > oldest_frame) {
	/*
	 * Switch buffers to prevent gaps.
	 */
	tmpframe = *oldest_frame;
	*oldest_frame = *last_frame;
	*last_frame = tmpframe;
    }
    Sockbuf_clear(&last_frame->sbuf);
    last_frame->loops = 0;
    rbuf = last_frame->sbuf;

    if (n == -1) {
	return -1;
    }

    /*
     * If the server hasn't yet acked our last keyboard change
     * and we haven't updated it in this time frame already
     * or we haven't sent anything for a while (keepalive)
     * then we send our current keyboard state.
     */
    if (last_keyboard_ack != last_keyboard_change
	&& last_keyboard_update < last_loops
	|| last_loops - last_send_anything > 5 * Setup->frames_per_second) {
	Key_update();
	last_send_anything = last_loops;
    }

    return 1 + (last_frame > oldest_frame);
}

/*
 * Receive the beginning of a new frame update packet,
 * which contains the loops number.
 */
int Receive_start(void)
{
    int		n;
    long	loops;
    u_byte	ch,
		key_ack;

    if ((n = Packet_scanf(&rbuf, "%c%ld%c", &ch, &loops, &key_ack)) <= 0) {
	return n;
    }
    if (last_loops >= loops) {
	/*
	 * Packet is duplicate or out of order.
	 */
	Net_measurement(loops, PACKET_DROP);
	printf("ignoring frame (%ld)\n", last_loops - loops);
	return 0;
    }
    last_loops = loops;
    last_keyboard_ack = (key_ack & 0xFF);
    if ((n = Handle_start(loops)) == -1) {
	return -1;
    }
    return 1;
}

/*
 * Receive the end of a new frame update packet,
 * which should contain the same loops number
 * as the frame head.  If this terminating packet
 * is missing then the packet is corrupt or incomplete.
 */
int Receive_end(void)
{
    int		n;
    long	loops;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%ld", &ch, &loops)) <= 0) {
	return n;
    }
    Net_measurement(loops, PACKET_DRAW);
    if ((n = Handle_end(loops)) == -1) {
	return -1;
    }
    return 1;
}

/*
 * Receive a message string.  This currently is rather
 * inefficiently encoded as an ascii string.
 */
int Receive_message(void)
{
    int		n;
    u_byte	ch;
    char	msg[MSG_LEN];

    if ((n = Packet_scanf(&cbuf, "%c%S", &ch, msg)) <= 0) {
	return n;
    }
    if ((n = Handle_message(msg)) == -1) {
	return -1;
    }
    return 1;
}

/*
 * Receive the remaining playing time.
 */
int Receive_time_left(void)
{
    int		n;
    u_byte	ch;
    long	sec;

    if ((n = Packet_scanf(&rbuf, "%c%ld", &ch, &sec)) <= 0) {
	return n;
    }
    if ((n = Handle_time_left(sec)) == -1) {
	return -1;
    }
    return 1;
}

/*
 * Receive the id of the player we get frame updates for (game over mode).
 */
int Receive_eyes(void)
{
    int			n,
			id;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd", &ch, &id)) <= 0) {
	return n;
    }
    if ((n = Handle_eyes(id)) == -1) {
	return -1;
    }
    return 1;
}

/*
 * Receive the packet with all player information for the HUD.
 * If this packet is missing from the frame update then the player
 * isn't actively playing, which means he's either damaged, dead,
 * paused or has game over.
 */
int Receive_self(void)
{
    int		n;
    short	x, y, vx, vy, lockId, lockDist,
    		fuelSum, fuelMax;
    u_byte	ch, heading, power, turnspeed, turnresistance,
		nextCheckPoint, lockDir,
    		numCloaks, numSensors, numMines, numRockets,
    		numEcms, numTransporters, numFrontShots,
    		numBackShots, numAfterburners, numLasers,
    		num_tanks, currentTank, stat;

    n = Packet_scanf(&rbuf, "%c" "%hd%hd%hd%hd%c" "%c%c%c"
		     "%hd%hd%c" "%c%c%c%c" "%c%c%c%c%c" "%c%c%c" "%hd%hd",
		     &ch,
		     &x, &y, &vx, &vy, &heading,
		     &power, &turnspeed, &turnresistance,
		     &lockId, &lockDist, &lockDir,
		     &nextCheckPoint, &numCloaks, &numSensors, &numMines,
		     &numRockets, &numEcms, &numTransporters,
		     &numFrontShots, &numBackShots,
		     &numAfterburners, &num_tanks, &currentTank,
		     &fuelSum, &fuelMax);

    if (n <= 0) {
	return n;
    }
    if (version >= 0x3041) {
	n = Packet_scanf(&rbuf, "%c%hd%hd%c", &numLasers,
			 &view_width, &view_height,
			 &debris_colors, &spark_rand);
	if (n <= 0) {
	    return n;
	}
	if (debris_colors > 8) {
	    debris_colors = 8;
	}
	if (view_width != draw_width || view_height != draw_height) {
	    Send_display();
	}

	if (version >= 0x3042) {
	    n = Packet_scanf(&rbuf, "%c", &stat);
	    if (n <= 0) {
		return n;
	    }
	    Game_over_action(stat);	
	}
    } else {
	numLasers = 0;
	view_width = DEF_VIEW_SIZE;
	view_height = DEF_VIEW_SIZE;
	debris_colors = 0;
    }

    n = Handle_self(x, y, vx, vy, heading,
		    (float) power,
		    (float) turnspeed,
		    (float) turnresistance / 255.0F,
		    lockId, lockDist, lockDir,
		    nextCheckPoint, numCloaks, numSensors, numMines,
		    numRockets, numEcms, numTransporters,
		    numFrontShots, numBackShots,
		    numAfterburners, numLasers,
		    num_tanks, currentTank,
		    fuelSum, fuelMax, rbuf.len);
    if (n == -1) {
	return -1;
    }
    return 1;
}

int Receive_refuel(void)
{
    int		n;
    short	x0, y0, x1, y1;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd%hd",
			  &ch, &x0, &y0, &x1, &y1)) <= 0) {
	return n;
    }
    if ((n = Handle_refuel(x0, y0, x1, y1)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_connector(void)
{
    int		n;
    short	x0, y0, x1, y1;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd%hd",
			  &ch, &x0, &y0, &x1, &y1)) <= 0) {
	return n;
    }
    if ((n = Handle_connector(x0, y0, x1, y1)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_laser(void)
{
    int		n;
    short	x, y, len;
    u_byte	ch, color, dir;

    if (version < 0x3043) {
	rbuf.ptr += 1+1+2+2+2+2;
	return 1;
    }
    if ((n = Packet_scanf(&rbuf, "%c%c%hd%hd%hd%c",
			  &ch, &color, &x, &y, &len, &dir)) <= 0) {
	return n;
    }
    if ((n = Handle_laser(color, x, y, len, dir)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_smart(void)
{
    int		n;
    short	x, y;
    u_byte	ch, dir;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%c", &ch, &x, &y, &dir)) <= 0) {
	return n;
    }
    if ((n = Handle_smart(x, y, dir)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_ball(void)
{
    int		n;
    short	x, y, id;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd", &ch, &x, &y, &id)) <= 0) {
	return n;
    }
    if ((n = Handle_ball(x, y, id)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_ship(void)
{
    int		n, shield, cloak;
    short	x, y, id;
    u_byte	ch, dir, flags;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd%c%c",
			  &ch, &x, &y, &id, &dir, &flags)) <= 0) {
	return n;
    }
    shield = ((flags & 1) != 0);
    cloak = ((flags & 2) != 0);
    if ((n = Handle_ship(x, y, id, dir, shield, cloak)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_mine(void)
{
    int		n;
    short	x, y;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd", &ch, &x, &y)) <= 0) {
	return n;
    }
    if ((n = Handle_mine(x, y)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_item(void)
{
    int		n;
    short	x, y;
    u_byte	ch, type;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%c", &ch, &x, &y, &type)) <= 0) {
	return n;
    }
    if ((n = Handle_item(x, y, type)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_destruct(void)
{
    int		n;
    short	count;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd", &ch, &count)) <= 0) {
	return n;
    }
    if ((n = Handle_destruct(count)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_shutdown(void)
{
    int		n;
    short	count, delay;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd", &ch, &count, &delay)) <= 0) {
	return n;
    }
    if ((n = Handle_shutdown(count, delay)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_debris(void)
{
    int			n, r, type;

    if (rbuf.ptr - rbuf.buf + 2 >= rbuf.len) {
	return 0;
    }
    type = (*rbuf.ptr++ & 0xFF);
    n = (*rbuf.ptr++ & 0xFF);
    if (rbuf.ptr - rbuf.buf + (n * 2) > rbuf.len) {
	return 0;
    }
    r = Handle_debris(type - PKT_DEBRIS, (u_byte*)rbuf.ptr, n);
    rbuf.ptr += n * 2;

    return (r == -1) ? -1 : 1;
}

int Receive_shot(void)
{
    int		n, color;
    short	x, y;
    u_byte	ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd", &ch, &x, &y)) <= 0) {
	return n;
    }
    color = ch - PKT_SHOT;
    if ((n = Handle_shot(x, y, color)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_ecm(void)
{
    int			n;
    short		x, y, size;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd", &ch, &x, &y, &size)) <= 0) {
	return n;
    }
    if ((n = Handle_ecm(x, y, size)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_trans(void)
{
    int			n;
    short		x1, y1, x2, y2;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd%hd", 
			  &ch, &x1, &y1, &x2, &y2)) <= 0) {
	return n;
    }
    if ((n = Handle_trans(x1, y1, x2, y2)) == -1) {
	return -1;
    } 
    return 1;
}

int Receive_paused(void)
{
    int			n;
    short		x, y, count;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd%hd", &ch, &x, &y, &count)) <= 0) {
	return n;
    }
    if ((n = Handle_paused(x, y, count)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_radar(void)
{
    int			n;
    short		x, y;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hd%hd", &ch, &x, &y)) <= 0) {
	return n;
    }
    if ((n = Handle_radar(x, y)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_damaged(void)
{
    int			n;
    u_byte		ch, damaged;

    if ((n = Packet_scanf(&rbuf, "%c%c", &ch, &damaged)) <= 0) {
	return n;
    }
    if ((n = Handle_damaged(damaged)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_leave(void)
{
    int			n;
    short		id;
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &id)) <= 0) {
	return n;
    }
    if ((n = Handle_leave(id)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_war(void)
{
    int			n;
    short		robot_id, killer_id;
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%hd%hd",
			  &ch, &robot_id, &killer_id)) <= 0) {
	return n;
    }
    if ((n = Handle_war(robot_id, killer_id)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_seek(void)
{
    int			n;
    short		programmer_id, robot_id, sought_id;
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%hd%hd%hd", &ch,
			  &programmer_id, &robot_id, &sought_id)) <= 0) {
	return n;
    }
    if ((n = Handle_seek(programmer_id, robot_id, sought_id)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_player(void)
{
    int			n;
    short		id;
    u_byte		ch, team, mychar;
    char		name[MAX_CHARS], real[MAX_CHARS], host[MAX_CHARS];

    if (version < 0x3041) {
	if ((n = Packet_scanf(&cbuf, "%c%hd%c%c%s", &ch,
			      &id, &team, &mychar, name)) <= 0) {
	    return n;
	}
	real[0] = '\0';
	host[0] = '\0';
    } else {
	if ((n = Packet_scanf(&cbuf, "%c%hd%c%c%s%s%s", &ch,
			      &id, &team, &mychar, name, real, host)) <= 0) {
	    return n;
	}
    }
    if ((n = Handle_player(id, team, mychar, name, real, host)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_score_object(void)
{
    int			n;
    unsigned short	x, y;
    short		score;
    char		msg[MAX_CHARS];
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%hd%hu%hu%s", 
			  &ch, &score, &x, &y, msg)) <= 0) {
	return n;
    }
    if ((n = Handle_score_object(score, x, y, msg)) == -1) {
	return -1;
    }

    return 1;
}

int Receive_score(void)
{
    int			n;
    short		id, score, life;
    u_byte		ch, mychar;

    if (version < 0x3040) {
	n = Packet_scanf(&cbuf, "%c%hd%hd%hd", &ch,
			 &id, &score, &life);
	mychar = '\0';
    } else {
	n = Packet_scanf(&cbuf, "%c%hd%hd%hd%c", &ch,
			 &id, &score, &life, &mychar);
    }
    if (n <= 0) {
	return n;
    }
    if ((n = Handle_score(id, score, life, mychar)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_fuel(void)
{
    int			n;
    unsigned short	num, fuel;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hu%hu", &ch, &num, &fuel)) <= 0) {
	return n;
    }
    if ((n = Handle_fuel(num, fuel << FUEL_SCALE_BITS)) == -1) {
	return -1;
    }
    Packet_printf(&wbuf, "%c%ld%hu", PKT_ACK_FUEL, last_loops, num);
    return 1;
}

int Receive_cannon(void)
{
    int			n;
    unsigned short	num, dead_time;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hu%hu", &ch, &num, &dead_time)) <= 0) {
	return n;
    }
    if ((n = Handle_cannon(num, dead_time)) == -1) {
	return -1;
    }
    Packet_printf(&wbuf, "%c%ld%hu", PKT_ACK_CANNON, last_loops, num);
    return 1;
}

int Receive_target(void)
{
    int			n;
    unsigned short	num,
			dead_time,
			damage;
    u_byte		ch;

    if ((n = Packet_scanf(&rbuf, "%c%hu%hu%hu", &ch, 
			  &num, &dead_time, &damage)) <= 0) {
	return n;
    }
    if ((n = Handle_target(num, dead_time, damage)) == -1) {
	return -1;
    }
    Packet_printf(&wbuf, "%c%ld%hu", PKT_ACK_TARGET, last_loops, num);
    return 1;
}

int Receive_base(void)
{
    int			n;
    short		id;
    unsigned short	num;
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%hd%hu", &ch, &id, &num)) <= 0) {
	return n;
    }
    if ((n = Handle_base(id, num)) == -1) {
	return -1;
    }
    return 1;
}

int Receive_magic(void)
{
    int			n;
    u_byte		ch;

    if ((n = Packet_scanf(&cbuf, "%c%u", &ch, &magic)) <= 0) {
	return n;
    }
    return 1;
}

int Receive_string(void)
{
    int			n;
    u_byte		ch,
			type;
    unsigned short	arg1,
			arg2;

    if ((n = Packet_scanf(&cbuf, "%c%c%hu%hu",
			  &ch, &type, &arg1, &arg2)) <= 0) {
	return n;
    }
    /*
     * Not implemented yet.
     */
    return 1;
}

int Send_ack(long rel_loops)
{
    if (Packet_printf(&wbuf, "%c%ld%ld", PKT_ACK,
		      reliable_offset, rel_loops) <= 0) {
	error("Can't ack reliable data");
	return -1;
    }
    return 1;
}

int Receive_reliable(void)
{
    int			n;
    short		len;
    u_byte		ch;
    long		rel,
			rel_loops;

    if ((n = Packet_scanf(&rbuf, "%c%hd%ld%ld",
			  &ch, &len, &rel, &rel_loops)) == -1) {
	return -1;
    }
    if (n == 0) {
	errno = 0;
	error("Incomplete reliable data packet");
	return -1;
    }
#if DEBUG
    if (reliable_offset >= rel + len) {
	printf("Reliable my=%ld pkt=%ld len=%d loops=%ld\n",
	       reliable_offset, rel, len, rel_loops);
    }
#endif
    if (len <= 0) {
	errno = 0;
	error("Bad reliable data length (%d)", len);
	return -1;
    }
    if (rbuf.ptr + len > rbuf.buf + rbuf.len) {
	errno = 0;
	error("Not all reliable data in packet (%d,%d,%d)",
	      rbuf.ptr - rbuf.buf, len, rbuf.len);
	rbuf.ptr += len;
	Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	return -1;
    }
    if (rel > reliable_offset) {
	/*
	 * We miss one or more packets.
	 * For now we drop this packet.
	 * We could have kept it until the missing packet(s) arrived.
	 */
	rbuf.ptr += len;
	Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	if (Send_ack(rel_loops) == -1) {
	    return -1;
	}
	return 1;
    }
    if (rel + len <= reliable_offset) {
	/*
	 * Duplicate data.  Probably an ack got lost.
	 * Send an ack for our current stream position.
	 */
	rbuf.ptr += len;
	Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	if (Send_ack(rel_loops) == -1) {
	    return -1;
	}
	return 1;
    }
    if (rel < reliable_offset) {
	len -= reliable_offset - rel;
	rbuf.ptr += reliable_offset - rel;
	rel = reliable_offset;
    }
    if (cbuf.ptr > cbuf.buf) {
	Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
    }
    if (Sockbuf_write(&cbuf, rbuf.ptr, len) != len) {
	errno = 0;
	error("Can't copy reliable data to buffer");
	rbuf.ptr += len;
	Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	return -1;
    }
    reliable_offset += len;
    rbuf.ptr += len;
    Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
    if (Send_ack(rel_loops) == -1) {
	return -1;
    }
    return 1;
}

int Receive_reply(int *replyto, int *result)
{
    int		n;
    u_byte	type, ch1, ch2;

    n = Packet_scanf(&cbuf, "%c%c%c", &type, &ch1, &ch2);
    if (n <= 0) {
	return n;
    }
    if (n != 3 || type != PKT_REPLY) {
	error("Can't receive reply packet");
	return -1;
    }
    *replyto = ch1;
    *result = ch2;
    return 1;
}

int Send_keyboard(u_byte *keyboard_vector)
{
    if (wbuf.size - wbuf.len < KEYBOARD_SIZE + 2) {
	errno = 0;
	error("Not enough write buffer space for keyboard state");
	return -1;
    }
    wbuf.buf[wbuf.len++] = PKT_KEYBOARD;
    wbuf.buf[wbuf.len++] = (last_keyboard_change & 0xFF);
    memcpy(&wbuf.buf[wbuf.len], keyboard_vector, KEYBOARD_SIZE);
    wbuf.len += KEYBOARD_SIZE;
    last_keyboard_update = last_loops;
    Send_talk();
    if (Sockbuf_flush(&wbuf) == -1) {
	error("Can't send keyboard update");
	return -1;
    }

    return 0;
}

int Send_power(float power)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_POWER,
	(int) (power * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Send_power_s(float power_s)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_POWER_S,
	(int) (power_s * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Send_turnspeed(float turnspeed)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_TURNSPEED,
	(int) (turnspeed * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Send_turnspeed_s(float turnspeed_s)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_TURNSPEED_S,
		      (int) (turnspeed_s * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Send_turnresistance(float turnresistance)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_TURNRESISTANCE,
		      (int) (turnresistance * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Send_turnresistance_s(float turnresistance_s)
{
    if (Packet_printf(&wbuf, "%c%hd", PKT_TURNRESISTANCE_S,
		      (int) (turnresistance_s * 256.0)) == -1) {
	return -1;
    }
    return 0;
}

int Receive_quit(void)
{
    unsigned char	pkt;
    sockbuf_t		*sbuf;
    char		reason[MAX_CHARS];

    if (rbuf.ptr < rbuf.buf + rbuf.len) {
	sbuf = &rbuf;
    } else {
	sbuf = &cbuf;
    }
    if (Packet_scanf(sbuf, "%c", &pkt) != 1) {
	errno = 0;
	error("Can't read quit packet");
    } else {
	if (version < 0x3043
	    || Packet_scanf(sbuf, "%s", reason) <= 0) {
	    strcpy(reason, "unknown reason");
	}
	errno = 0;
	error("Got quit packet: \"%s\"", reason);
    }
    return -1;
}


int Receive_audio(void)
{
    int			n;
    unsigned char	pkt, type, vol;

    if ((n = Packet_scanf(&rbuf, "%c%c%c", &pkt, &type, &vol)) <= 0) {
	return n;
    }
#ifdef SOUND
    if ((n = Handle_audio(type, vol)) == -1) {
	return -1;
    }
#endif /* SOUND */
    return 1;
}


int Receive_talk_ack(void)
{
    int			n;
    unsigned char	pkt;
    long		talk_ack;

    if ((n = Packet_scanf(&cbuf, "%c%ld", &pkt, &talk_ack)) <= 0) {
	return n;
    }
    if (talk_ack >= talk_pending) {
	talk_pending = 0;
    }
    return 1;
}


int Net_talk(char *str)
{
    strncpy(talk_str, str, sizeof talk_str - 1);
    talk_pending = ++talk_sequence_num;
    talk_last_send = last_loops - TALK_RETRY;
    return 0;
}


int Send_talk(void)
{
    if (talk_pending == 0) {
	return 0;
    }
    if (last_loops - talk_last_send < TALK_RETRY) {
	return 0;
    }
    if (Packet_printf(&wbuf, "%c%ld%s", PKT_TALK,
		      talk_pending, talk_str) == -1) {
	return -1;
    }
    talk_last_send = last_loops;
    return 0;
}


int Send_display(void)
{
    int			num_col;

    if (version < 0x3041) {
	return 0;
    }
    num_col = (maxColors == 8) ? 4
	: (maxColors == 16) ? 8
	: 0;
    if (Packet_printf(&wbuf, "%c%hd%hd%c%c", PKT_DISPLAY,
		      draw_width, draw_height, num_col, spark_rand) == -1) {
	return -1;
    }
    return 0;
}

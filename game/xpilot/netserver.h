/* $Id: netserver.h,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#ifndef	NETSERVER_H
#define	NETSERVER_H

#ifdef NETSERVER_C

/*
 * Different states a connection can be in.
 */
#define CONN_FREE	0x00	/* free for use */
#define CONN_LISTENING	0x01	/* before connect() */
#define CONN_SETUP	0x02	/* after verification */
#define CONN_LOGIN	0x04	/* after setup info transferred */
#define CONN_PLAYING	0x08	/* when actively playing */
#define CONN_CLOSING	0x10	/* closing down after output gone */
#define CONN_DRAIN	0x20	/* wait for all reliable data to be acked */
#define CONN_READY	0x40	/* draining after LOGIN and before PLAYING */

/*
 * In order to not let the server be locked by a collection
 * of idle connections we timeout a client if it doesn't
 * continue with logging in in a reasonable tempo.
 * Sorry, our resources are limited.
 * But the timeout should be easily configurable.
 * The timeout specifies the number of seconds each connection
 * state may last.
 */
#ifndef CONNECTION_TIMEOUT
#define CONNECTION_TIMEOUT	(FPS * 40)
#endif

/*
 * Maximum roundtrip time taken as serious for rountrip time calculations.
 */
#define MAX_RTT			(2 * FPS)

/*
 * The retransmission timeout bounds in number of frames.
 */
#define MIN_RETRANSMIT		(FPS / 8 + 1)
#define MAX_RETRANSMIT		(2 * FPS)
#define DEFAULT_RETRANSMIT	(FPS / 2)

#if defined(VMS) && !defined(MAXHOSTNAMELEN)
#define MAXHOSTNAMELEN 64
#endif


/*
 * All the connection state info.
 * Some of it is hardly ever used, if at all.
 */
typedef struct {
    int			state;			/* state of connection */
    int			drain_state;		/* state after draining done */
    unsigned		magic;			/* magic cookie */
    sockbuf_t		r;			/* input buffer */
    sockbuf_t		w;			/* output buffer */
    sockbuf_t		c;			/* reliable data buffer */
    long		start;			/* time of last state change */
    long		last_send_loops;	/* last update of reliable */
    long		reliable_offset;	/* amount of data acked */
    long		reliable_unsend;	/* next unsend reliable byte */
    long		retransmit_at_loop;	/* next retransmission time */
    int			rtt_smoothed;		/* smoothed roundtrip time */
    int			rtt_dev;		/* roundtrip time deviation */
    int			rtt_retransmit;		/* retransmission time */
    int			rtt_timeouts;		/* how many timeouts */
    int			acks;			/* good acknowledgements */
    int			setup;			/* amount of setup done */
    int			my_port;		/* server port for this player */
    int			his_port;		/* client port for this player */
    int			id;			/* index into GetInd[] or -1 */
    int			team;			/* team of player */
    int			last_key_change;	/* last keyboard change */
    unsigned		version;		/* XPilot version of client */
    long		talk_sequence_num;	/* talk acknowledgement */
    int			view_width, view_height;/* Viewable area dimensions */
    int			debris_colors;		/* Max. debris intensities */
    int			spark_rand;		/* Sparkling effect */
    char		*real;			/* real login name of player */
    char		*nick;			/* nickname of player */
    char		*dpy;			/* display of player */
    char		host[MAXHOSTNAMELEN];	/* hostname of players host */
} connection_t;

static int Compress_map(unsigned char *map, int size);
static int Init_setup(void);
static int Handle_listening(int ind);
static int Handle_setup(int ind);
static int Handle_login(int ind);
static int Handle_input(int ind);

static int Receive_send_bufsize(int ind);
static int Receive_keyboard(int ind);
static int Receive_quit(int ind);
static int Receive_play(int ind);
static int Receive_power(int ind);
static int Receive_ack(int ind);
static int Receive_ack_cannon(int ind);
static int Receive_ack_fuel(int ind);
static int Receive_ack_target(int ind);
static int Receive_discard(int ind);
static int Receive_undefined(int ind);
static int Receive_talk(int ind);
static int Receive_display(int ind);

#endif	/* NETSERVER_C */

void Init_receive(void);
int Setup_net_server(int maxconn);
void Destroy_connection(int ind, char *reason, char *file, int line);
int Setup_connection(char *real, char *nick, char *dpy,
		     int team, char *host, unsigned version);
int Input(int contact_socket);
int Send_reply(int ind, int replyto, int result);
int Send_self(int ind,
    int x, int y, int vx, int vy, int dir,
    float power, float turnspeed, float turnresistance,
    int lock_id, int lock_dist, int lock_dir,
    int check, int cloaks, int sensors, int mines,
    int missiles, int ecms, int transporters, int extra_shots, int back_shots,
    int afterburners, int lasers, int num_tanks, int current_tank,
    int fuel_sum, int fuel_max, long status);
int Send_leave(int ind, int id);
int Send_war(int ind, int robot_id, int killer_id);
int Send_seek(int ind, int programmer_id, int robot_id, int sought_id);
int Send_player(int ind, int id, int team, int mychar, char *name,
		char *real, char *disp);
int Send_score(int ind, int id, int score, int life, int mychar);
int Send_score_object(int ind, int score, int x, int y, char *string);
int Send_base(int ind, int id, int num);
int Send_fuel(int ind, int num, int fuel);
int Send_cannon(int ind, int num, int dead_time);
int Send_destruct(int ind, int count);
int Send_shutdown(int ind, int count, int delay);
int Send_debris(int ind, int type, unsigned char *p, int n);
int Send_shot(int ind, int x, int y, int color);
int Send_smart(int ind, int x, int y, int dir);
int Send_ball(int ind, int x, int y, int id);
int Send_mine(int ind, int x, int y);
int Send_target(int ind, int num, int dead_time, int damage);
int Send_audio(int ind, int type, int vol);
int Send_item(int ind, int x, int y, int type);
int Send_paused(int ind, int x, int y, int count);
int Send_ecm(int ind, int x, int y, int size);
int Send_ship(int ind, int x, int y, int id, int dir, int shield, int cloak);
int Send_refuel(int ind, int x0, int y0, int x1, int y1);
int Send_connector(int ind, int x0, int y0, int x1, int y1);
int Send_laser(int ind, int color, int x, int y, int len, int dir);
int Send_radar(int ind, int x, int y);
int Send_damaged(int ind, int damaged);
int Send_message(int ind, char *msg);
int Send_start_of_frame(int ind);
int Send_end_of_frame(int ind);
int Send_reliable(int ind);
int Send_time_left(int ind, long sec);
int Send_eyes(int ind, int id);
int Send_trans(int ind, int x1, int y1, int x2, int y2);
void Get_display_parameters(int ind, int *width, int *height,
			    int *debris_colors, int *spark_rand);
int Get_player_id(int);
#endif


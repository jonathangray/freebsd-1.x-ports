/* $Id: client.h,v 1.1 1994/02/23 14:40:04 jkh Exp $
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

#ifndef CLIENT_H
#define CLIENT_H

#include "const.h"
#include "types.h"
#include "keys.h"
#include "dbuff.h"

#define SHOW_HUD_INSTRUMENTS	(1<<0)		    
#define SHOW_HUD_VERTICAL	(1<<1)
#define SHOW_HUD_HORIZONTAL	(1<<2)
#define SHOW_FUEL_METER		(1<<3)
#define SHOW_FUEL_GAUGE		(1<<4)
#define SHOW_TURNSPEED_METER	(1<<5)
#define SHOW_POWER_METER	(1<<6)
#define SHOW_SHIP_NAME		(1<<7)
#define SHOW_SLIDING_RADAR	(1<<8)
#define SHOW_OUTLINE_WORLD	(1<<9)
#define SHOW_PACKET_SIZE_METER	(1<<10)
#define SHOW_PACKET_LOSS_METER	(1<<11)
#define SHOW_PACKET_DROP_METER	(1<<12)
#define SHOW_CLOCK		(1<<13)

#define PACKET_LOSS		0
#define PACKET_DROP		1
#define PACKET_DRAW		2

#define MAX_SCORE_OBJECTS	10

#define MAX_SPARK_SIZE		8
#define MIN_SPARK_SIZE		1
#define DEF_SPARK_SIZE		2
#define MAX_MAP_POINT_SIZE	8
#define MIN_MAP_POINT_SIZE	0
#define DEF_MAP_POINT_SIZE	2

typedef struct {
    float	ratio;
    short	id;
    short	team;
    short	score;
    short	life;
    short	mychar;
    short	war_id;
    short	name_width;	/* In pixels */
    short	name_len;	/* In bytes */
    char	name[MAX_CHARS];
    char	real[MAX_CHARS];
    char	host[MAX_CHARS];
} other_t;

typedef struct {
    int		pos;		/* Block index */
    long	fuel;		/* Amount of fuel available */
} fuelstation_t;

typedef struct {
    int		pos;		/* Block index */
    short	id,		/* Id of owner or -1 */
		team;		/* Team this base belongs to */
} homebase_t;

typedef struct {
    int		pos;		/* Block index */
    short	dead_time,	/* Frames inactive */
                dot;		/* Draw dot if inactive */
} cannontime_t;

typedef struct {
    int		pos;		/* Block index */
    short	dead_time;	/* Frames inactive */
    u_short	damage;		/* Damage to target */
} target_t;

typedef struct {
    int		pos;		/* Block index */
} checkpoint_t;

#define SCORE_OBJECT_COUNT	100
typedef struct {
    int		score,
                x,
                y,
                count,
                hud_msg_len,
    		hud_msg_width,
    		msg_width,
    		msg_len;
    char	msg[10],
                hud_msg[MAX_CHARS+10];
} score_object_t;


extern ipos	pos;
extern ipos	vel;
extern ipos	world;
extern ipos	realWorld;
extern short	wrappedWorld;
extern short	heading;
extern short	nextCheckPoint;
extern short	numCloaks;
extern short	numSensors;
extern short	numMines;
extern short	numRockets;
extern short	numEcms;
extern short 	numTransporters;
extern short	numFrontShots;
extern short	numBackShots;
extern short	numAfterburners;
extern short	numLasers;

extern short	lock_id;		/* Id of player locked onto */
extern short	lock_dir;		/* Direction of lock */
extern short	lock_dist;		/* Distance to player locked onto */

extern short	selfVisible;		/* Are we alive and playing? */
extern short	damaged;		/* Damaged by ECM */
extern short	destruct;		/* If self destructing */
extern short	shutdown_delay;
extern short	shutdown_count;

extern int	RadarHeight;
extern int	map_point_distance;	/* spacing of navigation points */
extern int	map_point_size;		/* size of navigation points */
extern int	spark_size;		/* size of sparks and debris */
extern long	control_count;		/* Display control for how long? */

extern long	fuelSum;		/* Sum of fuel in all tanks */
extern long	fuelMax;		/* How much fuel can you take? */
extern short	fuelCurrent;		/* Number of currently used tank */
extern short	numTanks;		/* Number of tanks */
extern long	fuelCount;		/* Display fuel for how long? */
extern int	fuelLevel1;		/* Fuel critical level */
extern int	fuelLevel2;		/* Fuel warning level */
extern int	fuelLevel3;		/* Fuel notify level */

extern float	power;			/* Force of thrust */
extern float	power_s;		/* Saved power fiks */
extern float	turnspeed;		/* How fast player acc-turns */
extern float	turnspeed_s;		/* Saved turnspeed */
extern float	turnresistance;		/* How much is lost in % */
extern float	turnresistance_s;	/* Saved (see above) */
extern float	spark_prob;		/* Sparkling effect configurable */
extern int	charsPerSecond;		/* Message output speed (config) */

extern float	hud_move_fact;		/* scale the hud-movement (speed) */
extern float	ptr_move_fact;		/* scale the speed pointer length */
extern long	instruments;		/* Instruments on screen (bitmask) */
extern int	packet_size;		/* Current frame update packet size */
extern int	packet_loss;		/* lost packets per second */
extern int	packet_drop;		/* dropped packets per second */
extern char	*packet_measure;	/* packet measurement in a second */
extern long	packet_loop;		/* start of measurement */

extern bool	showRealName;		/* Show realname instead of nickname */
extern char	name[MAX_CHARS];	/* Nick-name of player */
extern char	realname[MAX_CHARS];	/* Real name of player */
extern char	servername[MAX_CHARS];	/* Name of server connecting to */
extern unsigned	version;		/* Version of the server */
extern int	scoresChanged;
extern int	toggle_shield;		/* Are shields toggled by a press? */
extern int	shields;		/* When shields are considered up */


#ifdef SOUND
extern char 	sounds[MAX_CHARS];	/* audio mappings */
extern char 	audioServer[MAX_CHARS];	/* audio server */
extern int 	maxVolume;		/* maximum volume (in percent) */
#endif /* SOUND */

int Fuel_by_pos(int x, int y);
int Target_alive(int x, int y, int *damage);
int Handle_fuel(int ind, int fuel);
int Cannon_dead_time_by_pos(int x, int y, int *dot);
int Handle_cannon(int ind, int dead_time);
int Handle_target(int num, int dead_time, int damage);
int Base_info_by_pos(int x, int y, int *id, int *team);
int Handle_base(int id, int ind);
int Check_pos_by_index(int ind, int *xp, int *yp);
int Check_index_by_pos(int x, int y);
other_t *Other_by_id(int id);
int Handle_leave(int id);
int Handle_player(int id, int team, int mychar, char *player_name,
		  char *real_name, char *host_name);
int Handle_score(int id, int score, int life, int mychar);
int Handle_score_object(int score, int x, int y, char *msg);
int Handle_war(int robot_id, int killer_id);
int Handle_seek(int programmer_id, int robot_id, int sought_id);
void Map_dots(void);
void Map_blue(void);
void Client_score_table(void);
int Client_init(char *server, unsigned server_version);
int Client_setup(void);
void Client_cleanup(void);
int Client_start(void);
int Client_power(void);
int Client_fd(void);
int Client_input(int);
void Client_flush(void);
void Client_sync(void);
int Client_wrap_mode(void);
void Reset_shields(void);
void Set_toggle_shield(int onoff);
int xevent(int);
int Key_init(void);
int Key_update(void);

#endif


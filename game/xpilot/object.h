/* $Id: object.h,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#ifndef	OBJECT_H
#define	OBJECT_H

#include "const.h"
#include "types.h"
#include "bit.h"
#include "keys.h"
#include "rules.h"


#define OBJ_PLAYER		(1UL<<0)	/* Types of objects */
#define OBJ_DEBRIS		(1UL<<1)
#define OBJ_SPARK		(1UL<<2)
#define OBJ_NUKE		(1UL<<3)
#define OBJ_CANNON_DEBRIS	(1UL<<4)	/* Cannon objects */
#define OBJ_CANNON_SHOT		(1UL<<5)
#define OBJ_LASER		(1UL<<6)
#define OBJ_BALL		(1UL<<7)
#define OBJ_SHOT		(1UL<<8)	/* Misc. objects */
#define OBJ_SMART_SHOT		(1UL<<9)
#define OBJ_ROCKET_PACK		(1UL<<10)
#define OBJ_CLOAKING_DEVICE	(1UL<<11)
#define OBJ_ENERGY_PACK		(1UL<<12)
#define OBJ_WIDEANGLE_SHOT	(1UL<<13)
/* #define OBJ_TRAINER		(1UL<<14)          Not used. */
#define OBJ_SHIELD		(1UL<<15)
#define OBJ_REFUEL		(1UL<<16)
#define OBJ_COMPASS		(1UL<<17)
#define OBJ_BACK_SHOT		(1UL<<18)
#define OBJ_MINE		(1UL<<19)
#define OBJ_MINE_PACK		(1UL<<20)
#define OBJ_SENSOR_PACK		(1UL<<21)
#define OBJ_TANK		(1UL<<22)
#define OBJ_ECM			(1UL<<23)
#define OBJ_TORPEDO		(1UL<<24)
#define OBJ_HEAT_SHOT		(1UL<<25)
#define OBJ_AFTERBURNER		(1UL<<26)
#define OBJ_CONNECTOR		(1UL<<27)
#define OBJ_TRANSPORTER         (1UL<<28)
#define OBJ_FIRE		(1UL<<29)
#define OBJ_REPAIR		(1UL<<30)

#define LOCK_NONE		1
#define LOCK_PLAYER		2

#define NOT_CONNECTED		(-1)

typedef struct {
    byte	color;			/* Color of object */		
    int		id;			/* For shots => id of player */
    int		placed;			/* The object has just been placed */
    int		wrapped;		/* The object is on the screen edge */
    position	prevpos;		/* Object's previous position... */
    position	pos;			/* World coordinates */
    ipos	intpos;
    vector	vel;
    vector	acc;
    int		dir;
    float	max_speed;
    float	velocity;
    float	turnspeed;
    float	mass;
    int		type;
    long	info;			/* Miscellaneous info */
    int		life;			/* No of ticks left to live */
    int		count;			/* Misc timings */
    long	status;

    int 	owner;			/* Who's object is this ? */
                                        /* (spare for id)*/
    int		treasure;		/* Which treasure does ball belong */
    int		new_info;
    float	length;			/* Distance between ball and player */
} object;


/*
 * Fuel structure, used by player
 */
typedef struct {
    long	sum;			/* Sum of fuel in all tanks */
    long	max;			/* How much fuel can you take? */
    int		current;		/* Number of currently used tank */
    int		num_tanks;		/* Number of tanks */
    long	tank[MAX_TANKS];
    long	l1;			/* Fuel critical level */
    long	l2;			/* Fuel warning level */
    long	l3;			/* Fuel notify level */
} pl_fuel_t;

struct _visibility {
    int	canSee;
    long	lastChange;
};

/*
 * Structure holding the info for one pulse of a laser.
 */
typedef struct {
    position	pos;
    int		dir;
    int		len;
    int		life;
} pulse_t;

/* IMPORTANT
 *
 * This is the player structure, the first part MUST be similar to object_t,
 * this makes it possible to use the same basic operations on both of them
 * (mainly used in update.c).
 */
typedef struct {
    byte	color;			/* Color of object */		
    int		id;			/* Unique id of object */
    int		placed;			/* the object has just been placed */
    int		wrapped;		/* object crossed the world's edge */
    position	prevpos;		/* Previous position... */
    position	pos;			/* World coordinates */
    ipos	intpos;
    vector	vel;			/* Velocity of object */
    vector	acc;			/* Acceleration constant */
    int		dir;			/* Direction of acceleration */
    float	max_speed;		/* Maximum speed of object */
    float	velocity;		/* Absolute speed */
    float	turnspeed;		/* How fast player acc-turns */
    float	mass;			/* Mass of object (incl. cargo) */
    int		type;			/* Type of object */
    long	info;			/* Miscellaneous info */
    int		life;			/* Zero is dead. One is alive */
    int		count;			/* Miscellaneous timings */
    long	status;			/** Status, currently **/

    long	used;			/** Items you use **/
    long	have;			/** Items you have **/

    int		shield_time;		/* Shields if no playerShielding */
    pl_fuel_t	fuel;			/* ship tanks and the stored fuel */
    int		afterburners;		/* number of after burners, powerfull*/
					/* and efficient engine		     */
    float	emptymass;		/* Mass of empty ship */
    float	float_dir;		/* Direction, in float var */
    float	turnresistance;		/* How much is lost in % */
    float	turnvel;		/* Current velocity of turn (right) */
#ifdef TURN_FUEL
    float	oldturnvel;		/* Last velocity of turn (right) */
#endif
    float	turnacc;		/* Current acceleration of turn */
    long	mode;			/* Player mode, currently */
    long	score;			/* Current score of player */
    long	prev_score;		/* Last score that has been updated */
    int		prev_life;		/* Last life that has been updated */
    float	power;			/* Force of thrust */
    float	power_s;		/* Saved power fiks */
    float	turnspeed_s;		/* Saved turnspeed */
    float	hud_move_fact;		/* scale the hud-movement (speed) */
    float	ptr_move_fact;		/* scale thes peed pointer length */
    float	turnresistance_s;	/* Saved (see above) */
    float	sensor_range;		/* Range of sensors (radar) */
    int		shots;			/* Number of active shots by player */
    int		extra_shots;		/* Number of extra shots / 2 */
    int		back_shots;		/* Number of rear shots */
    int		mines;			/* Number of mines. */
    int		cloaks;			/* Number of cloaks. */
    int		sensors;		/* Number of sensors */
    int		missiles;		/* Number of missiles. */
    int		lasers;			/* Number of laser items. */
    int		num_pulses;		/* Number of laser pulses in the air. */
    int		max_pulses;		/* Max. number of laser pulses. */
    pulse_t	*pulses;		/* Info on laser pulses. */
    int		ecms;			/* Number of ecms. */
    int		transporters;		/* number of transporters */
    int		shot_max;		/* Maximum number of shots active */
    int		shot_life;		/* Number of ticks shot will live */
    float	shot_speed;		/* Speed of shots fired by player */
    float	shot_mass;		/* Mass of shots fired by player */
    long	shot_time;		/* Time of last shot fired by player */
    int		fs;			/* Connected to fuel station fs */
    int		repair_target;		/* Repairing this target */
    int		check;			/* Next check point to pass */
    int		time;			/* The time a player has used */
    int		round;			/* Number of rounds player have done */
    int		best_lap;		/* Players best lap time */
    int		best_run;		/* Best race time */
    int		last_lap;		/* Time on last pass */
    int		last_lap_time;		/* What was your last pass? */
    int		last_time;		/* What was the time? */

    int		home_base;		/* Num of home base */
    struct {
	int	    tagged;		/* Flag, what is tagged? */
	int	    pl_id;		/* Tagging player id */
	position    pos;		/* Position of locked object */
	float	    distance;		/* Distance to object */
    } lock;

    char	mychar;			/* Special char for player */
    char	prev_mychar;		/* Special char for player */
    char	name[MAX_CHARS];	/* Nick-name of player */
    char	realname[MAX_CHARS];	/* Real name of player */
    char	hostname[MAX_CHARS];	/* Hostname of client player uses */
    u_short	team;			/* What team is the player on? */
    u_short	pseudo_team;		/* Which team is used for my tanks */
					/* (detaching!) */
    object	*ball;
    /*
     * Robot variables
     */
    u_byte	robot_mode;		/* For players->RM_NOT_ROBOT */
    long	robot_count;		/* Misc timings, minimizes rand()use */
    int		robot_ind;		/* Index in the robot array */
    int		robot_lock;
    int		robot_lock_id;

    struct _visibility *visibility;

    int updateVisibility, forceVisible, damaged;
    int wormDrawCount, wormHoleHit, wormHoleDest;

    struct {
	int size;
	position pos;
    } ecmInfo;

    struct {
	int count, pl_id;
    } transInfo;

    int	rplay_fd;			/* rplay UDP socket fd */

    int conn;				/* connection index, -1 if robot */
    unsigned version;			/* XPilot version number of client */

    BITV_DECL(last_keyv, NUM_KEYS);	/* Keyboard state */
    BITV_DECL(prev_keyv, NUM_KEYS);	/* Keyboard state */
    int key_changed;

    void *audio;			/* audio private data */
} player;

#endif

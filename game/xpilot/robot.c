/* $Id: robot.c,v 1.1 1994/02/23 14:40:07 jkh Exp $
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
/* Robot code submitted by Maurice Abraham. */

#define SERVER
#include <stdlib.h>
#include "types.h"
#include "global.h"
#include "map.h"
#include "score.h"
#include "draw.h"
#include "robot.h"
#include "bit.h"
#include "saudio.h"
#include "netserver.h"

#ifndef	lint
static char sourceid[] = "@(#)robot.c,v 1.3 1992/06/26 15:25:46 bjoerns Exp";
#endif


#define EMPTY_SPACE(s)	\
    BIT(1 << (s), 1 << SPACE | 1 << BASE | 1 << WORMHOLE | 1 << POS_GRAV | \
	1 << NEG_GRAV | 1 << CWISE_GRAV | 1 << ACWISE_GRAV)

extern long KILLING_SHOTS;

int         NumRobots = 0;
static int  MAX_ROBOTS = 1;

static robot_t Robots[] =
{
    { "Mad Max", 94, 20 },
    { "Blackie", 10, 90 },
    { "Kryten", 70, 40 },
    { "Marvin", 30, 70 },
    { "R2D2", 50, 60 },
    { "C3PO", 60, 50 },
    { "K9", 50, 50 },
    { "Robby", 45, 55 },
    { "Mickey", 05, 95 },
    { "Hermes", 15, 85 },
    { "Pan", 60, 60 },
    { "Azurion", 40, 30 },
    { "Droidion", 60, 30 },
    { "Terminator", 80, 40 },
    { "Sniper", 30, 90 },
    { "Slugger", 40, 40 },
    { "Uzi", 95, 5 },
    { "Capone", 80, 50 },
    { "Tanx", 40, 70 },
    { "Chrome Star", 60, 60 },
    { "Bully", 80, 10 },
    { "Metal Hero", 40, 45 },
    { "Aurora", 60, 55 },
    { "Dalt Wisney", 30, 75 },
    { "Psycho", 65, 55 },
    { "Gorgon", 30, 40 },
    { "Pompel", 50, 50 },
    { "Pilt", 50, 50 },
    { "Sparky", 20, 40 },
    { "Cobra", 85, 60 },
    { "Falcon", 70, 20 },
    { "Boson", 25, 35 },
    { "Blazy", 40, 40 },
    { "Pixie", 15, 93 },
    { "Wimpy", 5, 98 },
    { "Bonnie", 30, 40 },
    { "Clyde", 40, 45 },
    { "Neuro", 70, 70 },
    { NULL, 0, 0 }
};


/*
 * Private functions.
 */
static void Calculate_max_robots(void);
static void New_robot(void);
static void Delete_robot(void);
static bool Check_robot_navigate(int ind, bool * num_evade);
static bool Check_robot_evade(int ind, int mine_i, int ship_i);
static bool Check_robot_target(int ind, int item_x, int item_y, int new_mode,
			       int attack_level);

#define USE_ECM(pl)							      \
{									      \
    if (pl->ecms > 0 && pl->fuel.sum > -ED_ECM)		                      \
    {									      \
	SET_BIT(pl->used, OBJ_ECM);					      \
	do_ecm(pl);							      \
	pl->ecms--;							      \
	Add_fuel(&(pl->fuel),ED_ECM);		                              \
    }									      \
}



/********** **********
 * Updating robots and the like.
 */
static void Calculate_max_robots(void)
{
    int         i;

    for (i = 0; Robots[i].name != NULL; i++);

    MAX_ROBOTS = i;
}


static void Delete_robot(void)
{
    long        i,
                low_score = LONG_MAX,
                low_i = -1;

    for (i = 0; i < NumPlayers; i++) {
	player     *pl = Players[i];

	if (pl->robot_mode == RM_NOT_ROBOT || pl->robot_mode == RM_OBJECT)
	    continue;

	if (pl->score < low_score) {
	    low_i = i;
	    low_score = pl->score;
	}
    }

    if (low_i >= 0) {
	Delete_player(low_i);
    }
}


static void New_robot(void)
{
    player     *robot;
    robot_t    *rob;
    int         i,
                num;
    static bool first_time = true;


    if (first_time) {
	Calculate_max_robots();
	first_time = false;
    }

    Init_player(NumPlayers);
    robot = Players[NumPlayers];

    do {
	num = rand() % MAX_ROBOTS;

	for (i = 0; i < NumPlayers; i++)
	    if (num == Players[i]->robot_ind)
		break;
    } while (i < NumPlayers);

    rob = &Robots[num];

    strcpy(robot->name, rob->name);
    strcpy(robot->realname, "robot");
    strcpy(robot->hostname, "robots.org");

    robot->color = WHITE;
    robot->turnspeed = MAX_PLAYER_TURNSPEED;
    robot->turnspeed_s = MAX_PLAYER_TURNSPEED;
    robot->turnresistance = 0.12;
    robot->turnresistance_s = 0.12;
    robot->power = MAX_PLAYER_POWER;
    robot->power_s = MAX_PLAYER_POWER;
    if (BIT(World.rules->mode, TEAM_PLAY))
	robot->team = 0;		/* Robots are on their own team */
    robot->mychar = 'R';
    robot->robot_mode = RM_TAKE_OFF;
    robot->robot_ind = num;
    robot->conn = NOT_CONNECTED;

    robot->fuel.l1 = 100 * FUEL_SCALE_FACT;
    robot->fuel.l2 = 200 * FUEL_SCALE_FACT;
    robot->fuel.l3 = 500 * FUEL_SCALE_FACT;

    Pick_startpos(NumPlayers);
    Go_home(NumPlayers);

    /*
     * robot->shot_speed= ShotsSpeed + (rob->attack - 50) / 5.0;
     * robot->shot_mass	= ShotsMass + (rob->defense - rob->attack) / 10.0;
     * robot->max_speed	= SPEED_LIMIT - robot->shot_speed;
     */
    NumPlayers++;
    World.teams[0].NumMembers++;
    Id++;
    NumRobots++;

    for (i = 0; i < NumPlayers - 1; i++) {
	if (Players[i]->conn != NOT_CONNECTED) {
	    Send_player(Players[i]->conn, robot->id, robot->team,
			robot->mychar, robot->name,
			robot->realname, robot->hostname);
	    Send_base(Players[i]->conn, robot->id, robot->home_base);
	}
    }

    {
	static char *msgs[] = {
	    /*
	     * Insert your own witty messages here
	     * and remove the silly ones.
	     */
	    "%s just can't stand you anymore.",
	    "%s has come to give you a hard time.",
	    "%s is looking for trouble.",
	    "%s has a very loose trigger finger.",
	    "Have fear, %s is here.",
	    "Prepare to die by the hands of %s.",
	    "%s is untouchable.",
	    "%s is in a gruesome mood.",
	    "%s is in a killing mood.",
	    "%s wants you for dessert.",
	    "%s vows to torment you in this life and the next.",
	    "%s has no sense of humour.",
	    "%s is back from the Sirius wars, and he's in a violent mood."
	};
	static int	next_msg = -1;
	char		msg[MSG_LEN];

	if (next_msg == -1) {
	    next_msg = rand();
	}
	next_msg = (next_msg + 1) % NELEM(msgs);
	sprintf(msg, msgs[next_msg], robot->name);
	Set_message(msg);
    }

#ifndef	SILENT
    printf("%s (%d, %s) starts at startpos %d.\n",
	   robot->name, NumPlayers, robot->realname, robot->home_base);
#endif

    updateScores = true;
}


static bool Check_robot_navigate(int ind, bool * num_evade)
{
    int         i, j, k;
    player     *pl;
    int         area_val[10][10];
    int         calc_val[10][10];
    int         locn_block;
    long        dx, dy;
    int         di, dj;
    bool        found_wall;
    bool        found_grav;
    bool        cannon_dead;
    bool        near_wall;
    int         best_val;
    int         best_i, best_j;
    float       best_vx, best_vy;
    int         best_dir;
    int         delta_dir;

    pl = Players[ind];

    if (pl->velocity > 2.0 || ABS(pl->vel.x) > 1.5)
	return false;

    for (i = 0; i < 10; i++) {
	for (j = 0; j < 10; j++) {
	    area_val[i][j] = 0;
	}
    }

    found_wall = false;
    found_grav = false;

    for (i = 0; i < 10; i += 2) {
	for (j = 0; j < 10; j += 2) {

	    dx = (long)(pl->pos.x / BLOCK_SZ) + (i / 2) - 2;
	    dy = (long)(pl->pos.y / BLOCK_SZ) + (j / 2) - 2;

	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (dx < 0) dx += World.x;
		else if (dx >= World.x) dx -= World.x;
		if (dy < 0) dy += World.y;
		else if (dy >= World.y) dy -= World.y;
	    }
	    if (dx < 0 || dx >= World.x || dy < 0 || dy >= World.y)
		locn_block = FILLED;
	    else
		locn_block = World.block[dx][dy];

	    switch (locn_block) {

	    case SPACE:
	    case BASE:
	    case WORMHOLE:
		area_val[i][j] = 1;
		area_val[i + 1][j] = 1;
		area_val[i + 1][j + 1] = 1;
		area_val[i][j + 1] = 1;
		break;

	    case REC_LU:
		area_val[i + 1][j] = 1;
		found_wall = true;
		break;

	    case REC_LD:
		area_val[i + 1][j + 1] = 1;
		found_wall = true;
		break;

	    case REC_RU:
		area_val[i][j] = 1;
		found_wall = true;
		break;

	    case REC_RD:
		area_val[i][j + 1] = 1;
		found_wall = true;
		break;

	    case POS_GRAV:
	    case NEG_GRAV:
	    case CWISE_GRAV:
	    case ACWISE_GRAV:
		found_grav = true;
		break;

	    case CANNON:
		cannon_dead = true;
		for (k = 0; k < World.NumCannons; k++) {
		    if (World.cannon[k].pos.x == dx
			&& World.cannon[k].pos.y == dy
			&& World.cannon[k].dead_time <= 50)
			cannon_dead = false;
		}
		if (cannon_dead) {
		    area_val[i][j] = 1;
		    area_val[i + 1][j] = 1;
		    area_val[i + 1][j + 1] = 1;
		    area_val[i][j + 1] = 1;
		}
		found_wall = true;
		break;

	    default:
		found_wall = true;
		break;
	    }
	}
    }

    if (found_grav || !found_wall)
	return false;

    /* iterate twice for weighting, central 6x6 square should be accurate */

    for (k = 0; k < 2; k++) {
	for (i = 0; i < 10; i++) {
	    for (j = 0; j < 10; j++) {

		calc_val[i][j] = 0;
		if (area_val[i][j] == 0)
		    continue;

		if (i <= 0 || i >= 9 || j <= 0 || j >= 9)
		    continue;

		calc_val[i][j] += 2 * area_val[i - 1][j];
		calc_val[i][j] += 2 * area_val[i][j + 1];
		calc_val[i][j] += 2 * area_val[i + 1][j];
		calc_val[i][j] += 2 * area_val[i][j - 1];

		calc_val[i][j] += area_val[i - 1][j - 1];
		calc_val[i][j] += area_val[i - 1][j + 1];
		calc_val[i][j] += area_val[i + 1][j - 1];
		calc_val[i][j] += area_val[i + 1][j + 1];
	    }
	}

	for (i = 0; i < 10; i++) {
	    for (j = 0; j < 10; j++) {
		area_val[i][j] = calc_val[i][j];
	    }
	}
    }

    /* now focus in to local 3x3 square */

    dx = pl->pos.x;
    dy = pl->pos.y;

    dx = dx - (dx / BLOCK_SZ * BLOCK_SZ);
    dy = dy - (dy / BLOCK_SZ * BLOCK_SZ);

    di = 3;
    dj = 3;

    if (dx > BLOCK_SZ / 2) {
	di++;
	dx -= BLOCK_SZ / 2;
    }
    if (dy > BLOCK_SZ / 2) {
	dj++;
	dy -= BLOCK_SZ / 2;
    }
    for (i = 0; i < 3; i++) {
	for (j = 0; j < 3; j++) {
	    area_val[i][j] = area_val[di + i][dj + j];
	}
    }

    *num_evade = true;

    if (ABS(pl->vel.x) < 0.5) {

	best_i = 1;
	best_j = (pl->vel.y > 0 ? 2 : 0);

    } else if (ABS(pl->vel.y) < 0.5) {

	best_i = (pl->vel.x > 0 ? 2 : 0);
	best_j = 1;

    } else {

	best_i = (pl->vel.x > 0 ? 2 : 0);
	best_j = (pl->vel.y > 0 ? 2 : 0);
    }

    best_val = area_val[best_i][best_j];
    near_wall = false;

    for (j = 2; j >= 0; j--) {
	for (i = 0; i <= 2; i++) {

	    if (i == 1 && j == 1)
		continue;

	    if (area_val[i][j] == 0) {
		near_wall = true;
		if (i == 1 && (j == 0 || j == 2)) {
		    best_i = 1;
		    best_j = (2 - j);
		    best_val = 99999;
		}
		continue;
	    }
	    if (area_val[i][j] > best_val) {
		best_i = i;
		best_j = j;
		best_val = area_val[i][j];
	    }
	}
    }

    if (area_val[1][1] >= best_val)
	return false;

    if (!near_wall) {
	if (BIT(pl->used, OBJ_REFUEL)) {
	    /* refueling, so hang around */
	    best_i = 1;
	    best_j = 1;
	    best_val = area_val[1][1];
	} else {
	    return false;
	}
    }
    if (best_j == 1) {
	if (dy < BLOCK_SZ / 6)
	    best_j = 2;
	if (dy > BLOCK_SZ / 3)
	    best_j = 0;
    }
    pl->turnspeed = MAX_PLAYER_TURNSPEED;
    pl->power = pl->mass / 2;
    LIMIT(pl->power, MIN_PLAYER_POWER, MAX_PLAYER_POWER);

    best_vx = (best_i - 1) * 0.75;
    best_vy = (best_j - 1) * 1.25;

    if (pl->vel.x > best_vx + 0.75)
	best_dir = 3 * RES / 8;
    else if (pl->vel.x < best_vx - 0.75)
	best_dir = RES / 8;
    else if (pl->vel.x > best_vx + 0.25)
	best_dir = 5 * RES / 16;
    else if (pl->vel.x < best_vx - 0.25)
	best_dir = 3 * RES / 16;
    else
	best_dir = RES / 4;

    delta_dir = best_dir - pl->dir;
    delta_dir = MOD2(delta_dir, RES);

    if (delta_dir > RES / 8 && delta_dir < 7 * RES / 8) {
	pl->turnacc = (delta_dir < RES / 2 ?
		       pl->turnspeed : (-pl->turnspeed));
    } else if (delta_dir > RES / 64 && delta_dir < 63 * RES / 64) {
	pl->turnspeed = MIN_PLAYER_TURNSPEED;
	pl->turnacc = (delta_dir < RES / 2 ?
		       pl->turnspeed : (-pl->turnspeed));
    } else {
	pl->turnacc = 0;
    }

    if (pl->vel.y > best_vy + 0.25) {
	CLR_BIT(pl->status, THRUSTING);
    } else if (pl->vel.y < best_vy - 0.25) {
	SET_BIT(pl->status, THRUSTING);
    }
    return true;
}


static bool Check_robot_evade(int ind, int mine_i, int ship_i)
{
    int         i;
    player     *pl;
    object     *shot;
    player     *ship;
    long        stop_dist;
    bool        evade;
    bool        left_ok, right_ok;
    int         safe_width;
    int         travel_dir;
    int         delta_dir;
    int         aux_dir;
    int         px[3], py[3];
    long        dist;
    int         locn_block;
    vector     *gravity;
    int         gravity_dir;
    long 	dx, dy;
    float	velocity;

    pl = Players[ind];
    safe_width = 3 * SHIP_SZ / 2;
    /* Prevent overflow. */
    velocity = (pl->velocity <= SPEED_LIMIT) ? pl->velocity : SPEED_LIMIT;
    stop_dist =
	(RES * velocity) / (MAX_PLAYER_TURNSPEED * pl->turnresistance)
	+ (velocity * velocity * pl->mass) / (2 * MAX_PLAYER_POWER)
	+ safe_width;
    /*
     * Limit the look ahead.  For very high speeds the current code
     * is ineffective and much too inefficient.
     */
    if (stop_dist > 10 * BLOCK_SZ) {
	stop_dist = 10 * BLOCK_SZ;
    }
    evade = false;

    if (pl->velocity <= 0.2) {
	vector	*grav = &World.gravity
	    [(int)pl->pos.x / BLOCK_SZ][(int)pl->pos.y / BLOCK_SZ];
	travel_dir = findDir(grav->x, grav->y);
    } else {
	travel_dir = findDir(pl->vel.x, pl->vel.y);
    }

    aux_dir = MOD2(travel_dir + RES / 4, RES);
    px[0] = pl->pos.x;		/* ship center x */
    py[0] = pl->pos.y;		/* ship center y */
    px[1] = px[0] + safe_width * tcos(aux_dir);	/* ship left side x */
    py[1] = py[0] + safe_width * tsin(aux_dir);	/* ship left side y */
    px[2] = 2 * px[0] - px[1];	/* ship right side x */
    py[2] = 2 * py[0] - py[1];	/* ship right side y */

    left_ok = true;
    right_ok = true;

    for (dist = 0; dist < stop_dist + BLOCK_SZ / 2; dist += BLOCK_SZ / 2) {
	for (i = 0; i < 3; i++) {
	    dx = (px[i] + dist * tcos(travel_dir)) / BLOCK_SZ;
	    dy = (py[i] + dist * tsin(travel_dir)) / BLOCK_SZ;

	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (dx < 0) dx += World.x;
		else if (dx >= World.x) dx -= World.x;
		if (dy < 0) dy += World.y;
		else if (dy >= World.y) dy -= World.y;
	    }
	    if (dx < 0 || dx >= World.x || dy < 0 || dy >= World.y) {
		evade = true;
		if (i == 1)
		    left_ok = false;
		if (i == 2)
		    right_ok = false;
		continue;
	    }
	    locn_block = World.block[dx][dy];
	    if (!EMPTY_SPACE(locn_block)) {
		evade = true;
		if (i == 1)
		    left_ok = false;
		if (i == 2)
		    right_ok = false;
		continue;
	    }
	    /* Watch out for strong gravity */
	    gravity = &World.gravity[dx][dy];
	    if (sqr(gravity->x) + sqr(gravity->y) >= 1.0) {
		gravity_dir = findDir(gravity->x - pl->pos.x,
				      gravity->y - pl->pos.y);
/* XXX: DOES THIS WORK?  strength of gravity - position?!?! */
		if (MOD2(gravity_dir - travel_dir, RES) <= RES / 4 ||
		    MOD2(gravity_dir - travel_dir, RES) >= 3 * RES / 4) {
		    evade = true;
		    if (i == 1)
			left_ok = false;
		    if (i == 2)
			right_ok = false;
		    continue;
		}
	    }
	}
    }

    if (mine_i >= 0) {
	shot = Obj[mine_i];
	aux_dir = Wrap_findDir(shot->pos.x + shot->vel.x - pl->pos.x,
			       shot->pos.y + shot->vel.y - pl->pos.y);
	delta_dir = MOD2(aux_dir - travel_dir, RES);
	if (delta_dir < RES / 4) {
	    left_ok = false;
	    evade = true;
	}
	if (delta_dir > RES * 3 / 4) {
	    right_ok = false;
	    evade = true;
	}
    }
    if (ship_i >= 0) {
	ship = Players[ship_i];
	aux_dir = Wrap_findDir(ship->pos.x - pl->pos.x + ship->vel.x * 2,
			       ship->pos.y - pl->pos.y + ship->vel.y * 2);
	delta_dir = MOD2(aux_dir - travel_dir, RES);
	if (delta_dir < RES / 4) {
	    left_ok = false;
	    evade = true;
	}
	if (delta_dir > RES * 3 / 4) {
	    right_ok = false;
	    evade = true;
	}
    }
    if (pl->velocity > MAX_ROBOT_SPEED)
	evade = true;

    if (!evade)
	return false;

    delta_dir = 0;
    while (!left_ok && !right_ok && delta_dir < 7 * RES / 8) {
	delta_dir += RES / 16;

	left_ok = true;
	aux_dir = MOD2(travel_dir + delta_dir, RES);
	for (dist = 0; dist < stop_dist + BLOCK_SZ / 2; dist += BLOCK_SZ / 2) {
	    dx = (px[0] + dist * tcos(aux_dir)) / BLOCK_SZ;
	    dy = (py[0] + dist * tsin(aux_dir)) / BLOCK_SZ;

	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (dx < 0) dx += World.x;
		else if (dx >= World.x) dx -= World.x;
		if (dy < 0) dy += World.y;
		else if (dy >= World.y) dy -= World.y;
	    }
	    if (dx < 0 || dx >= World.x || dy < 0 || dy >= World.y) {
		left_ok = false;
		continue;
	    }
	    locn_block = World.block[dx][dy];
	    if (!EMPTY_SPACE(locn_block)) {
		left_ok = false;
		continue;
	    }
	    /* watch out for strong gravity */
	    gravity = &World.gravity[dx][dy];
	    if (sqr(gravity->x) + sqr(gravity->y) >= 1.0) {
		gravity_dir = findDir(gravity->x - pl->pos.x,
				      gravity->y - pl->pos.y);
		if (MOD2(gravity_dir - travel_dir, RES) <= RES / 4 ||
		    MOD2(gravity_dir - travel_dir, RES) >= 3 * RES / 4) {

		    left_ok = false;
		    continue;
		}
	    }
	}

	right_ok = true;
	aux_dir = MOD2(travel_dir - delta_dir, RES);
	for (dist = 0; dist < stop_dist + BLOCK_SZ / 2; dist += BLOCK_SZ / 2) {
	    dx = (px[0] + dist * tcos(aux_dir)) / BLOCK_SZ;
	    dy = (py[0] + dist * tsin(aux_dir)) / BLOCK_SZ;

	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (dx < 0) dx += World.x;
		else if (dx >= World.x) dx -= World.x;
		if (dy < 0) dy += World.y;
		else if (dy >= World.y) dy -= World.y;
	    }
	    if (dx < 0 || dx >= World.x || dy < 0 || dy >= World.y) {
		right_ok = false;
		continue;
	    }
	    locn_block = World.block[dx][dy];
	    if (!EMPTY_SPACE(locn_block)) {
		right_ok = false;
		continue;
	    }
	    /* watch out for strong gravity */
	    gravity = &World.gravity[dx][dy];
	    if (sqr(gravity->x) + sqr(gravity->y) >= 1.0) {
		gravity_dir = findDir(gravity->x - pl->pos.x,
				      gravity->y - pl->pos.y);
		if (MOD2(gravity_dir - travel_dir, RES) <= RES / 4 ||
		    MOD2(gravity_dir - travel_dir, RES) >= 3 * RES / 4) {

		    right_ok = false;
		    continue;
		}
	    }
	}
    }

    pl->turnspeed = MAX_PLAYER_TURNSPEED;
    pl->power = MAX_PLAYER_POWER;

    delta_dir = MOD2(pl->dir - travel_dir, RES);

    if (pl->robot_mode != RM_EVADE_LEFT && pl->robot_mode != RM_EVADE_RIGHT) {
	if (left_ok && !right_ok)
	    pl->robot_mode = RM_EVADE_LEFT;
	else if (right_ok && !left_ok)
	    pl->robot_mode = RM_EVADE_RIGHT;
	else
	    pl->robot_mode = (delta_dir < RES / 2 ?
			      RM_EVADE_LEFT : RM_EVADE_RIGHT);
    }
    if (delta_dir < 3 * RES / 8 || delta_dir > 5 * RES / 8) {
	pl->turnacc = (pl->robot_mode == RM_EVADE_LEFT ?
		       pl->turnspeed : (-pl->turnspeed));
	CLR_BIT(pl->status, THRUSTING);
    } else {
	pl->turnacc = 0;
	SET_BIT(pl->status, THRUSTING);
	pl->robot_mode = (delta_dir < RES/2 ? RM_EVADE_LEFT : RM_EVADE_RIGHT);
    }

    return true;
}

static bool Check_robot_target(int ind,
			       int item_x, int item_y,
			       int new_mode, int attack_level)
{
    player     *pl;
    long        item_dist;
    int         item_dir;
    int         travel_dir;
    int         delta_dir;
    long        dx, dy;
    long        dist;
    int         locn_block;
    bool        clear_path;
    bool        slowing;

    pl = Players[ind];

    dx = item_x - pl->pos.x, dx = WRAP_DX(dx);
    dy = item_y - pl->pos.y, dy = WRAP_DY(dy);

    item_dist = LENGTH(dy, dx);

    if (dx == 0 && dy == 0) {
	vector	*grav = &World.gravity
	    [(int)pl->pos.x / BLOCK_SZ][(int)pl->pos.y / BLOCK_SZ];
	item_dir = findDir(grav->x, grav->y);
	item_dir = MOD2(item_dir + RES/2, RES);
    } else {
	item_dir = findDir(dx, dy);
    }

    if (new_mode == RM_REFUEL || new_mode == RM_CANNON_KILL)
	item_dist -= 2 * BLOCK_SZ;

    clear_path = true;

    for (dist = 0; clear_path && dist < item_dist; dist += BLOCK_SZ / 2) {

	dx = (pl->pos.x + dist * tcos(item_dir)) / BLOCK_SZ;
	dy = (pl->pos.y + dist * tsin(item_dir)) / BLOCK_SZ;

	if (BIT(World.rules->mode, WRAP_PLAY)) {
	    if (dx < 0) dx += World.x;
	    else if (dx >= World.x) dx -= World.x;
	    if (dy < 0) dy += World.y;
	    else if (dy >= World.y) dy -= World.y;
	}
	if (dx < 0 || dx >= World.x || dy < 0 || dy >= World.y) {
	    clear_path = false;
	    continue;
	}
	locn_block = World.block[dx][dy];
	if (!EMPTY_SPACE(locn_block) && locn_block != CANNON) {
	    clear_path = false;
	    continue;
	}
    }

    if (!clear_path)
	return false;

    if (pl->velocity <= 0.2) {
	vector	*grav = &World.gravity
	    [(int)pl->pos.x / BLOCK_SZ][(int)pl->pos.y / BLOCK_SZ];
	travel_dir = findDir(grav->x, grav->y);
    } else {
	travel_dir = findDir(pl->vel.x, pl->vel.y);
    }

    pl->turnspeed = MAX_PLAYER_TURNSPEED / 2;
    pl->power = MAX_PLAYER_POWER / 2;

    delta_dir = MOD2(item_dir - travel_dir, RES);
    if (delta_dir >= RES/4 && delta_dir <= 3*RES/4) {

	if (new_mode == RM_HARVEST) {	/* reverse direction of travel */
	    item_dir = MOD2(travel_dir + RES / 2, RES);
	}
	pl->turnspeed = MAX_PLAYER_TURNSPEED;
	slowing = true;

	if (pl->mines && item_dist < 8 * BLOCK_SZ) {
	    Place_moving_mine(ind, pl->vel.x, pl->vel.y);
	    pl->mines--;
	    new_mode = (rand() & 1) ? RM_EVADE_RIGHT : RM_EVADE_LEFT;
	}
    } else if (new_mode == RM_CANNON_KILL && item_dist <= 0) {

	/* too close, to move away */
	pl->turnspeed = MAX_PLAYER_TURNSPEED;
	item_dir = MOD2(item_dir + RES / 2, RES);
	slowing = true;
    } else {

	slowing = false;
    }

    delta_dir = MOD2(item_dir - pl->dir, RES);

    if (delta_dir > RES / 8 && delta_dir < 7 * RES / 8) {
	pl->turnacc = (delta_dir < RES / 2 ?
		       pl->turnspeed : (-pl->turnspeed));
    } else if ((delta_dir > RES / 16 && delta_dir < 15 * RES / 8)
	       || (pl->robot_count % 5) == 0) {

	pl->turnspeed = MAX_PLAYER_TURNSPEED / 2;
	pl->turnacc = (delta_dir < RES / 2
		       ? pl->turnspeed : (-pl->turnspeed));
    } else if (delta_dir > RES / 64 && delta_dir < 63 * RES / 64) {

	pl->turnspeed = MAX_PLAYER_TURNSPEED / 2;
	pl->turnacc = (delta_dir < RES / 2 ?
		       pl->turnspeed : (-pl->turnspeed));
    } else {
	pl->turnacc = 0.0;
    }

    if (slowing || BIT(pl->used, OBJ_SHIELD)) {

	SET_BIT(pl->status, THRUSTING);

    } else if (item_dist < 0) {

	CLR_BIT(pl->status, THRUSTING);

    } else if (item_dist < 3*BLOCK_SZ) {

	if (pl->velocity < NORMAL_ROBOT_SPEED / 2)
	    SET_BIT(pl->status, THRUSTING);
	if (pl->velocity > NORMAL_ROBOT_SPEED)
	    CLR_BIT(pl->status, THRUSTING);

    } else if (new_mode != RM_ATTACK) {

	if (pl->velocity < 2*NORMAL_ROBOT_SPEED)
	    SET_BIT(pl->status, THRUSTING);
	if (pl->velocity > 3*NORMAL_ROBOT_SPEED)
	    CLR_BIT(pl->status, THRUSTING);

    } else {

	if (pl->velocity < ATTACK_ROBOT_SPEED / 2)
	    SET_BIT(pl->status, THRUSTING);
	if (pl->velocity > ATTACK_ROBOT_SPEED)
	    CLR_BIT(pl->status, THRUSTING);
    }

    if (new_mode == RM_ATTACK) {
	if (pl->ecms > 0 && item_dist < ECM_DISTANCE / 2) {
	    USE_ECM(pl);
	} else if (pl->transporters > 0 && item_dist < TRANSPORTER_DISTANCE &&
	    pl->fuel.sum > -ED_TRANSPORTER)
	{
	    do_transporter(pl);
	    pl->transporters--;
	    Add_fuel(&(pl->fuel), ED_TRANSPORTER);
	}
	else if (pl->lasers > pl->num_pulses
	    && pl->robot_lock == LOCK_PLAYER
	    && -ED_LASER < pl->fuel.sum - pl->fuel.l3) {
	    player	*ship = Players[GetInd[pl->robot_lock_id]];
	    float	x1, y1, x3, y3, x4, y4, x5, y5;
	    float	ship_dist, dir3, dir4, dir5;

	    if (BIT(ship->status, PLAYING|PAUSE|GAME_OVER) == PLAYING) {

		x1 = pl->pos.x + pl->vel.x + ships[pl->dir].pts[0].x;
		y1 = pl->pos.y + pl->vel.y + ships[pl->dir].pts[0].y;
		x3 = ship->pos.x + ship->vel.x;
		y3 = ship->pos.y + ship->vel.y;

		ship_dist = Wrap_length(x3 - x1, y3 - y1);

		if (ship_dist < PULSE_SPEED*PULSE_LIFE(pl->lasers) + SHIP_SZ) {
		    dir3 = Wrap_findDir(x3 - x1, y3 - y1);
		    x4 = x3 + tcos((int)(dir3 - RES/4)) * SHIP_SZ;
		    y4 = y3 + tsin((int)(dir3 - RES/4)) * SHIP_SZ;
		    x5 = x3 + tcos((int)(dir3 + RES/4)) * SHIP_SZ;
		    y5 = y3 + tsin((int)(dir3 + RES/4)) * SHIP_SZ;
		    dir4 = Wrap_findDir(x4 - x1, y4 - y1);
		    dir5 = Wrap_findDir(x5 - x1, y5 - y1);
		    if ((dir4 > dir5)
			? (pl->dir >= dir4 || pl->dir <= dir5)
			: (pl->dir >= dir4 && pl->dir <= dir5)) {

			SET_BIT(pl->used, OBJ_LASER);
		    }
		}
	    }
	}
	if (BIT(pl->used, OBJ_LASER)) {
	    pl->turnacc = 0.0;
	}
	else if ((pl->robot_count % 10) == 0 && pl->missiles > 0) {
	  if((pl->missiles > NUKE_MIN_SMART/2) && (rand() & allowNukes))
	    Fire_shot(ind, OBJ_NUKE, pl->dir);
	  else
	    Fire_shot(ind,
		      (rand() & 1) ? OBJ_SMART_SHOT : OBJ_HEAT_SHOT,
		      pl->dir);
	}
	else if ((pl->robot_count % 2) == 0
		   && item_dist < VISIBILITY_DISTANCE
		   && (pl->robot_lock == LOCK_PLAYER /* robot has a target */
		       || (pl->robot_count
			   % (110 - Robots[pl->robot_ind].attack))
		       < 15+attack_level)) {

	    if (pl->missiles > 0 && (rand() & 63) == 0)
	      if((pl->missiles > NUKE_MIN_SMART/2) && (rand() & allowNukes))
		Fire_shot(ind, OBJ_NUKE, pl->dir);
	      else
		Fire_shot(ind, OBJ_SMART_SHOT, pl->dir);
	    else
	      Fire_normal_shots(ind);
	}
	if (pl->fuel.sum < pl->fuel.l2 && pl->mines > 0
	    && (pl->robot_count % 30) == 0) {
	    Place_mine(ind);
	    pl->mines--;
	    CLR_BIT(pl->used, OBJ_CLOAKING_DEVICE);
	}
    }
    if (new_mode == RM_CANNON_KILL && !slowing) {
	if ((pl->robot_count % 2) == 0 && item_dist < VISIBILITY_DISTANCE) {
	    Fire_normal_shots(ind);
	}
    }
    pl->robot_mode = new_mode;
    return true;
}


static bool Check_robot_hunt(int ind)
{
    player *pl;
    player *ship;
    int ship_dir;
    int travel_dir;
    int delta_dir;
    int adj_dir;
    bool toofast, tooslow;

    pl = Players[ind];

    if (pl->robot_lock != LOCK_PLAYER
	|| pl->robot_lock_id == pl->id)
	return false;
    if (pl->fuel.sum < MAX_PLAYER_FUEL/2)
	return false;

    ship = Players[GetInd[pl->robot_lock_id]];

    ship_dir = Wrap_findDir(ship->pos.x - pl->pos.x, ship->pos.y - pl->pos.y);

    if (pl->velocity <= 0.2) {
	vector	*grav = &World.gravity
	    [(int)pl->pos.x / BLOCK_SZ][(int)pl->pos.y / BLOCK_SZ];
	travel_dir = findDir(grav->x, grav->y);
    } else {
	travel_dir = findDir(pl->vel.x, pl->vel.y);
    }

    delta_dir = MOD2(ship_dir - travel_dir, RES);
    tooslow = (pl->velocity < ATTACK_ROBOT_SPEED/2);
    toofast = (pl->velocity > ATTACK_ROBOT_SPEED);

    if (!tooslow && !toofast
	&& (delta_dir <= RES/16 || delta_dir >= 15*RES/16)) {

	pl->turnacc = 0;
	CLR_BIT(pl->status, THRUSTING);
	pl->robot_mode = RM_ROBOT_IDLE;
	return true;
    }

    adj_dir = (delta_dir<RES/2 ? RES/4 : (-RES/4));

    if (tooslow) adj_dir = adj_dir/2;	/* point forwards more */
    if (toofast) adj_dir = 3*adj_dir/2;	/* point backwards more */

    adj_dir = MOD2(travel_dir + adj_dir, RES);
    delta_dir = MOD2(adj_dir - pl->dir, RES);

    if (delta_dir>=RES/16 && delta_dir<=15*RES/16) {
	pl->turnspeed = MAX_PLAYER_TURNSPEED/4;
	pl->turnacc = (delta_dir<RES/2 ? pl->turnspeed : (-pl->turnspeed));
    }

    if (delta_dir<RES/8 || delta_dir>7*RES/8) {
	SET_BIT(pl->status, THRUSTING);
    } else {
	CLR_BIT(pl->status, THRUSTING);
    }

    pl->robot_mode = RM_ROBOT_IDLE;
    return true;
}


void Update_robots(void)
{
    player     *pl, *ship;
    object     *shot;
    float      distance, mine_dist, item_dist, ship_dist,
		enemy_dist, cannon_dist, fuel_dist, fx, fy,
		speed, x_speed, y_speed;
    int         i, j, mine_i, item_i, ship_i,
		enemy_i, cannon_i, fuel_i, x, y;
    long        dx, dy;
    bool        harvest_checked;
    bool        fuel_checked;
    bool        evade_checked;
    int         attack_level;
    int         shoot_time;
    char	msg[MSG_LEN];
    static int	new_robot_delay;


    if (NumRobots < WantedNumRobots
	&& NumPlayers - NumPseudoPlayers < World.NumBases - 1
	&& NumRobots < MAX_ROBOTS
	&& !(BIT(World.rules->mode, TEAM_PLAY)
	     && World.teams[0].NumMembers >= World.teams[0].NumBases)) {

	if (++new_robot_delay >= RECOVERY_DELAY) {
	    New_robot();
	    new_robot_delay = 0;
	}
    } else if (NumPlayers - NumPseudoPlayers >= World.NumBases
	       && NumRobots > 0) {
	Delete_robot();
    }
    if (NumRobots <= 0)
	return;

    for (i = 0; i < NumPlayers; i++) {
	pl = Players[i];
	if (pl->robot_mode == RM_NOT_ROBOT)
	    continue;
	if (pl->robot_mode == RM_OBJECT) {
	    int         t = loops % (TANK_NOTHRUST_TIME + TANK_THRUST_TIME);

	    if (t == 0) {
		SET_BIT(pl->status, THRUSTING);
	    } else if (t == TANK_THRUST_TIME) {
		CLR_BIT(pl->status, THRUSTING);
	    }
	    continue;
	}

	if (robotsLeave && pl->life > 0) {
	    msg[0] = '\0';
	    if (robotLeaveLife > 0 && pl->life >= robotLeaveLife) {
		sprintf(msg, "%s retired.", pl->name);
	    }
	    else if (robotLeaveScore != 0 && pl->score < robotLeaveScore) {
		sprintf(msg, "%s left out of disappointment.", pl->name);
	    }
	    else if (robotLeaveRatio != 0 && pl->score / (pl->life + 1)
		    < robotLeaveRatio) {
		sprintf(msg, "%s played too badly.", pl->name);
	    }
	    if (msg[0] != '\0') {
		Set_message(msg);
		Delete_player(i);
		updateScores = true;
		i--;
		continue;
	    }
	}

	if (BIT(pl->status, PLAYING|GAME_OVER) != PLAYING)
	    continue;

	if (pl->robot_count <= 0)
	    pl->robot_count = 1000 + rand() % 20;

	pl->robot_count--;

	CLR_BIT(pl->used, OBJ_SHIELD | OBJ_CLOAKING_DEVICE | OBJ_LASER);
	harvest_checked = false;
	fuel_checked = false;
	evade_checked = false;

	mine_i = -1;
	mine_dist = SHIP_SZ + 200;
	item_i = -1;
	item_dist = VISIBILITY_DISTANCE;

	if (BIT(pl->have, OBJ_CLOAKING_DEVICE) && pl->fuel.sum > pl->fuel.l3)
	    SET_BIT(pl->used, OBJ_CLOAKING_DEVICE);

	for (j = 0; j < NumObjs; j++) {
	    int         shield_range = 20 + SHIP_SZ;

	    shot = Obj[j];

	    switch (shot->type) {
	    case OBJ_WIDEANGLE_SHOT:
	    case OBJ_BACK_SHOT:
	    case OBJ_ROCKET_PACK:
	    case OBJ_CLOAKING_DEVICE:
	    case OBJ_ENERGY_PACK:
	    case OBJ_MINE_PACK:
	    case OBJ_SENSOR_PACK:
	    case OBJ_ECM:
	    case OBJ_AFTERBURNER:
	    case OBJ_TANK:
	    case OBJ_LASER:
		if ((dx = shot->pos.x - pl->pos.x, dx = WRAP_DX(dx),
			ABS(dx)) < item_dist
		    && (dy = shot->pos.y - pl->pos.y, dy = WRAP_DY(dy),
			ABS(dy)) < item_dist
		    && (distance = LENGTH(dx, dy)) < item_dist) {
		    item_i = j;
		    item_dist = distance;
		}
		break;

	    case OBJ_SMART_SHOT:
	    case OBJ_HEAT_SHOT:
		shield_range += TORPEDO_RANGE;
	    case OBJ_MINE:
		fx = shot->pos.x - pl->pos.x;
		fy = shot->pos.y - pl->pos.y;
		if ((dx = fx, dx = WRAP_DX(dx), ABS(dx)) < mine_dist 
		    && (dy = fy, dy = WRAP_DY(dy), ABS(dy)) < mine_dist 
		    && (distance = LENGTH(dx, dy)) < mine_dist) {
		    mine_i = j;
		    mine_dist = distance;
		}
		if ((dx = fx + (shot->vel.x - pl->vel.x) * ROB_LOOK_AH,
			dx = WRAP_DX(dx), ABS(dx)) < mine_dist
		    && (dy = fy + (shot->vel.y - pl->vel.y) * ROB_LOOK_AH,
			dy = WRAP_DY(dy), ABS(dy)) < mine_dist
		    && (distance = LENGTH(dx, dy)) < mine_dist) {
		    mine_i = j;
		    mine_dist = distance;
		}
		break;
            case OBJ_NUKE:
		shield_range += NUKE_RANGE;
		break;
	    case OBJ_TORPEDO:
		shield_range += TORPEDO_RANGE;
		break;
	    }

	    if (BIT(shot->type, KILLING_SHOTS)
		&& (dx = shot->pos.x - pl->pos.x, dx = WRAP_DX(dx),
		    ABS(dx)) < shield_range
		&& (dy = shot->pos.y - pl->pos.y, dy = WRAP_DY(dy),
		    ABS(dy)) < shield_range
		&& sqr(dx) + sqr(dy) <= sqr(shield_range)
		&& (shot->type == OBJ_CANNON_SHOT
		/*  || shot->id == -1
		    || Players[GetInd[shot->id]]->score > 50	*/
		    || pl->robot_count % 100
		    < (Robots[pl->robot_ind].defense + 700) / 8)) {
		SET_BIT(pl->used, OBJ_SHIELD);
		SET_BIT(pl->status, THRUSTING);

		if (BIT(shot->type, OBJ_SMART_SHOT)) {
		    USE_ECM(pl);
		}
	    }
	}
	if (itemLaserProb > 0 && BIT(pl->used, OBJ_SHIELD) == 0) {
	    for (j = 0; j < NumPlayers; j++) {
		ship = Players[j];
		if (j == i
		    || BIT(ship->status, PLAYING|PAUSE|GAME_OVER) != PLAYING)
		    continue;
		if (ship->num_pulses > 0) {
		    distance = Wrap_length(pl->pos.x - ship->pos.x,
					   pl->pos.y - ship->pos.y);
		    if (PULSE_SPEED*PULSE_LIFE(ship->lasers) + 2*SHIP_SZ
			>= distance) {
			int delta_dir = Wrap_findDir(pl->pos.x - ship->pos.x,
						     pl->pos.y - ship->pos.y),
			    ship_dir = ship->dir + (ship->turnvel
				+ ship->turnacc) * ship->turnresistance;
			if ((delta_dir - ship_dir < 0)
			    ? (ship_dir - delta_dir < RES/8
				|| delta_dir + RES - ship_dir < RES/8)
			    : (delta_dir - ship_dir < RES/8
				|| ship_dir + RES - delta_dir < RES/8)) {
			    SET_BIT(pl->used, OBJ_SHIELD);
			    break;
			}
		    }
		}
	    }
	}

	/* Note: Only take time to navigate if not being shot at */
	if (!(BIT(pl->used, OBJ_SHIELD) && SET_BIT(pl->status, THRUSTING))
	    && Check_robot_navigate(i, &evade_checked)) {
	    if (playerShielding == 0
		&& playerStartsShielded != 0
		&& BIT(pl->have, OBJ_SHIELD)) {
		SET_BIT(pl->used, OBJ_SHIELD);
	    }
	    continue;
	}

	ship_i = -1;
	ship_dist = (pl->fuel.sum >= pl->fuel.l1 ? SHIP_SZ * 6 : 0);
	enemy_i = -1;
	enemy_dist = (pl->fuel.sum >= pl->fuel.l3
		      ? VISIBILITY_DISTANCE * 2 : VISIBILITY_DISTANCE);

	if (BIT(pl->used, OBJ_SHIELD))
	    ship_dist = 0;

	for (j = 0; j < NumPlayers; j++) {
	    ship = Players[j];
	    if (j == i
		|| BIT(ship->status, PLAYING|PAUSE|GAME_OVER) != PLAYING)
		continue;

	    dx = ship->pos.x - pl->pos.x, dx = WRAP_DX(dx);
	    dy = ship->pos.y - pl->pos.y, dy = WRAP_DY(dy);

	    if ((distance = LENGTH(dx, dy)) < ship_dist) {

		ship_i = j;
		ship_dist = distance;
	    }

	    if (pl->robot_lock == LOCK_PLAYER) {
		/* ignore all players unless target */
		if (pl->robot_lock_id == ship->id
		    && distance < enemy_dist) {

		    enemy_i = j;
		    enemy_dist = distance;
		}
	    } else {
		if (ship->robot_mode == RM_NOT_ROBOT
		    && distance < enemy_dist) {
		    enemy_i    = j;
		    enemy_dist = distance;
		}
	    }
	}

	if (ship_dist <= 3*SHIP_SZ)
	    SET_BIT(pl->used, OBJ_SHIELD);

	if (pl->robot_lock == LOCK_PLAYER
	    && ship_i != -1
	    && pl->robot_lock_id == Players[ship_i]->id)
	    ship_i = -1; /* don't avoid target */

	if (pl->lock.tagged == LOCK_PLAYER) {
	    ship = Players[GetInd[pl->lock.pl_id]];
	    if (BIT(ship->status, PLAYING|PAUSE|GAME_OVER) != PLAYING
		|| (pl->robot_lock == LOCK_PLAYER
		    && pl->robot_lock_id != pl->lock.pl_id)
		|| pl->lock.distance > 2 * VISIBILITY_DISTANCE
		|| (!(pl->visibility[GetInd[pl->lock.pl_id]].canSee)
		    && (pl->robot_count % 25) == 0)) {
		pl->lock.pl_id = 1;
		pl->lock.tagged = LOCK_NONE;
		pl->lock.pos.x = pl->pos.x;
		pl->lock.pos.y = pl->pos.y;
		pl->lock.distance = 0;
	    }
	}
	if (enemy_i >= 0) {
	    ship = Players[enemy_i];
	    if (pl->lock.tagged == LOCK_NONE
		|| enemy_dist < 3*pl->lock.distance/4) {
 		if (!(pl->visibility[enemy_i].canSee)
		    || (BIT(ship->status, THRUSTING)
			&& enemy_dist < VISIBILITY_DISTANCE)) {

		    pl->lock.pl_id = ship->id;
		    pl->lock.tagged = LOCK_PLAYER;
		    pl->lock.pos.x = ship->pos.x;
		    pl->lock.pos.y = ship->pos.y;
		    pl->lock.distance = enemy_dist;
		    pl->sensor_range = VISIBILITY_DISTANCE;
		}
	    }
	}
	if (!evade_checked) {
	    if (Check_robot_evade(i, mine_i, ship_i)) {
		if (playerShielding == 0
		    && playerStartsShielded != 0
		    && BIT(pl->have, OBJ_SHIELD)) {
		    SET_BIT(pl->used, OBJ_SHIELD);
		}
		continue;
	    }
	}
	if (item_i >= 0 && enemy_dist > /* 2* */ item_dist
	    /* && enemy_dist > 12*BLOCK_SZ */ ) {

	    harvest_checked = true;
	    dx = Obj[item_i]->pos.x;
	    dy = Obj[item_i]->pos.y + SHIP_SZ / 2;

	    if (Check_robot_target(i, dx, dy, RM_HARVEST, 0))
		continue;
	}
	if (pl->lock.tagged == LOCK_PLAYER) {

	    ship = Players[GetInd[pl->lock.pl_id]];
	    shoot_time = pl->lock.distance / (pl->shot_speed + 1);
	    dx = ship->pos.x + ship->vel.x * shoot_time;
	    dy = ship->pos.y + ship->vel.y * shoot_time + SHIP_SZ / 2;
	    attack_level = MAX(ship->score / 8, 0);

	    if (Check_robot_target(i, dx, dy, RM_ATTACK, attack_level))
		continue;
	}
	if (item_i >= 0 && !harvest_checked) {

	    dx = Obj[item_i]->pos.x;
	    dy = Obj[item_i]->pos.y + SHIP_SZ / 2;

	    if (Check_robot_target(i, dx, dy, RM_HARVEST, 0))
		continue;
	}

	if (Check_robot_hunt(i)) {
	    if (playerShielding == 0
		&& playerStartsShielded != 0
		&& BIT(pl->have, OBJ_SHIELD)) {
		SET_BIT(pl->used, OBJ_SHIELD);
	    }
	    continue;
	}

	cannon_i = -1;
	cannon_dist = VISIBILITY_DISTANCE;
	fuel_i = -1;
	fuel_dist = VISIBILITY_DISTANCE;

	for (j = 0; j < World.NumCannons; j++) {

	    if (World.cannon[j].dead_time > 0)
		continue;

	    if ((dx = World.cannon[j].pos.x*BLOCK_SZ + BLOCK_SZ/2 - pl->pos.x,
		    dx = WRAP_DX(dx), ABS(dx)) < cannon_dist
		&& (dy = World.cannon[j].pos.y*BLOCK_SZ+BLOCK_SZ/2-pl->pos.y,
		    dy = WRAP_DY(dy), ABS(dy)) < cannon_dist
		&& (distance = LENGTH(dx, dy)) < cannon_dist) {
		cannon_i = j;
		cannon_dist = distance;
	    }
	}

	for (j = 0; j < World.NumFuels; j++) {

	    if (World.fuel[j].fuel < 100 * FUEL_SCALE_FACT
		|| pl->fuel.sum >= MAX_PLAYER_FUEL - 200 * FUEL_SCALE_FACT)
		continue;

	    if ((dx = World.fuel[j].pos.x - pl->pos.x,
		    dx = WRAP_DX(dx), ABS(dx)) < fuel_dist
		&& (dy = World.fuel[j].pos.y - pl->pos.y,
		    dy = WRAP_DY(dy), ABS(dy)) < fuel_dist
		&& (distance = LENGTH(dx, dy)) < fuel_dist) {
		fuel_i = j;
		fuel_dist = distance;
	    }
	}

	if (fuel_i >= 0 && cannon_dist > fuel_dist) {

	    fuel_checked = true;
	    dx = World.fuel[fuel_i].pos.x;
	    dy = World.fuel[fuel_i].pos.y + SHIP_SZ / 2;

	    SET_BIT(pl->used, OBJ_REFUEL);
	    pl->fs = fuel_i;

	    if (Check_robot_target(i, dx, dy, RM_REFUEL, 0))
		continue;
	}
	if (cannon_i >= 0) {

	    dx = World.cannon[cannon_i].pos.x * BLOCK_SZ + BLOCK_SZ / 2;
	    dy = World.cannon[cannon_i].pos.y * BLOCK_SZ + BLOCK_SZ / 2;

	    if (Check_robot_target(i, dx, dy, RM_CANNON_KILL, 0))
		continue;
	}
	if (fuel_i >= 0 && !fuel_checked) {

	    dx = World.fuel[fuel_i].pos.x;
	    dy = World.fuel[fuel_i].pos.y + SHIP_SZ / 2;

	    SET_BIT(pl->used, OBJ_REFUEL);
	    pl->fs = fuel_i;

	    if (Check_robot_target(i, dx, dy, RM_REFUEL, 0))
		continue;
	}
	if (pl->fuel.sum < DEFAULT_PLAYER_FUEL)
	    Add_fuel(&(pl->fuel), (int)(FUEL_SCALE_FACT * 0.02));
	if (pl->fuel.sum < MIN_PLAYER_FUEL)
	    Add_fuel(&(pl->fuel), pl->fuel.sum - MIN_PLAYER_FUEL);

	if (playerShielding == 0
	    && playerStartsShielded != 0
	    && BIT(pl->have, OBJ_SHIELD)) {
	    SET_BIT(pl->used, OBJ_SHIELD);
	}

	x = pl->pos.x/BLOCK_SZ;
	y = pl->pos.y/BLOCK_SZ;
	LIMIT(x, 0, World.x);
	LIMIT(y, 0, World.y);
	x_speed = pl->vel.x - 2 * World.gravity[x][y].x;
	y_speed = pl->vel.y - 2 * World.gravity[x][y].y;
	speed = LENGTH(x_speed, y_speed);

	if (y_speed < (-NORMAL_ROBOT_SPEED) || (pl->robot_count % 64) < 32) {

	    pl->robot_mode = RM_ROBOT_CLIMB;
	    pl->turnspeed = MAX_PLAYER_TURNSPEED / 2;
	    pl->power = MAX_PLAYER_POWER / 2;
	    if (ABS(pl->dir - RES / 4) > RES / 16) {
		pl->turnacc = (pl->dir < RES / 4
			       || pl->dir >= 3 * RES / 4
			       ? pl->turnspeed : (-pl->turnspeed));
	    } else {
		pl->turnacc = 0.0;
	    }
	    if (y_speed < NORMAL_ROBOT_SPEED / 2
		&& pl->velocity < ATTACK_ROBOT_SPEED)
		SET_BIT(pl->status, THRUSTING);
	    else if (y_speed > NORMAL_ROBOT_SPEED)
		CLR_BIT(pl->status, THRUSTING);
	    continue;
	}
	/* must be idle */
	pl->robot_mode = RM_ROBOT_IDLE;
	pl->turnspeed = MAX_PLAYER_TURNSPEED / 2;
	pl->turnacc = 0;
	pl->power = MAX_PLAYER_POWER / 2;
	CLR_BIT(pl->status, THRUSTING);
	if (speed < NORMAL_ROBOT_SPEED / 2)
	    SET_BIT(pl->status, THRUSTING);
	else if (speed > NORMAL_ROBOT_SPEED)
	    CLR_BIT(pl->status, THRUSTING);
    }
}


void Robot_war(int ind, int killer)
{
    player		*pl = Players[ind];
    int			k;

    if (pl->robot_mode == RM_NOT_ROBOT
	|| pl->robot_mode == RM_OBJECT) {
	return;
    }
    if (killer != ind
	&& rand()%100 < Players[killer]->score - pl->score) {
	/*
	 * Give fuel for offensive.
	 */
	pl->fuel.sum = MAX_PLAYER_FUEL;

	if (pl->robot_lock != LOCK_PLAYER
	    || pl->robot_lock_id != Players[killer]->id) {
	    for (k = 0; k < NumPlayers; k++) {
		if (Players[k]->conn != NOT_CONNECTED) {
		    Send_war(Players[k]->conn, pl->id, 
			     Players[killer]->id);
		}
	    }
	    sound_play_all(DECLARE_WAR_SOUND);
	    pl->robot_lock_id = Players[killer]->id;
	    pl->robot_lock = LOCK_PLAYER;
	}
    }
}

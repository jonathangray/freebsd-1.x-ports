/* $Id: collision.c,v 1.1 1994/02/23 14:40:04 jkh Exp $
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

#define SERVER
#include <stdlib.h>
#include "global.h"
#include "map.h"
#include "score.h"
#include "robot.h"
#include "saudio.h"
#include "bit.h"
#include "item.h"
#include "netserver.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: collision.c,v 1.1 1994/02/23 14:40:04 jkh Exp $";
#endif

#define in_range(o1, o2, r)			\
    (ABS((o1)->intpos.x-(o2)->intpos.x)<(r)	\
     && ABS((o1)->intpos.y-(o2)->intpos.y)<(r))	\
    /* this in_range() macro still needs an edgewrap modification */


extern long KILLING_SHOTS;
static char msg[MSG_LEN];

static void WallCollide(object *obj, int x, int y,
			int count, int max, int axis);
static bool Landing(int ind, int point, int blockdir);
static int Rate(int winner, int looser);
static void ObjectCollision(int begin, int end);
static void PlayerCollision(int max_objs);
static void LaserCollision(void);
static void PlayerObjectCollision(int ind, int max_objs);


void SCORE(int ind, int points, int x, int y, char *msg)
{
    player	*pl = Players[ind];

    pl->score += (points);

    if (pl->conn != NOT_CONNECTED)
	Send_score_object(pl->conn, points, x, y, msg);

    updateScores = true;		
}

static int Rate(int winner, int loser)
{
    int t;

    t = ((RATE_SIZE/2) * RATE_RANGE) / (ABS(loser-winner) + RATE_RANGE);
    if (loser > winner)
	t = RATE_SIZE - t;
    return (t);
}


void Check_collision(void)
{
    object *obj;
    int i, max_objs = NumObjs;
    player *pl;

    for (i=0; i<NumObjs; i++) {
	obj = Obj[i];
	obj->intpos.x = (int)(obj->pos.x + 0.5);
	obj->intpos.y = (int)(obj->pos.y + 0.5);
    }
    for (i=0; i<NumPlayers; i++) {
	pl = Players[i];
	pl->intpos.x = (int)(pl->pos.x + 0.5);
	pl->intpos.y = (int)(pl->pos.y + 0.5);
    }
    ObjectCollision(0, max_objs);
    PlayerCollision(max_objs);
    LaserCollision();
    if (max_objs < NumObjs) {
	ObjectCollision(max_objs, NumObjs);
    }
}


static void ObjectCollision(int begin, int end)
{
    int		i, x, y, t;


    /* Shot - wall, and out of bounds */
    for (i = begin; i < end; i++) {
	object	*obj = Obj[i];
	int sx, sy;
	int dx, dy;
	int placed = obj->placed;
	int wrapped = obj->wrapped;
	int start;

	x = (int)obj->pos.x / BLOCK_SZ;
	y = (int)obj->pos.y / BLOCK_SZ;
	sx = (int)(obj->prevpos.x / BLOCK_SZ);
	sy = (int)(obj->prevpos.y / BLOCK_SZ);
	dx = x - sx;
	dy = y - sy;

	if (dx == 0 && dy == 0) {
	    if (placed) {
		WallCollide(obj, sx, sy, 0, 0, 0);
	    }
	} else {
	    if (ABS(dx) > ABS(dy)) {
		if (dx > 0) {
		    if (wrapped)
			start = dx;
		    else
			start = !placed;

		    for (t = start; t <= dx && obj->life > 0; t++) {
			WallCollide(obj, sx + t, sy + dy * t/dx, t, dx, 1);
		    }
		} else {
		    if (wrapped)
			start = dx;
		    else
			start = -!placed;

		    for (t = start; t >= dx && obj->life > 0; t--) {
			WallCollide(obj, sx + t, sy + dy * t/dx, t, dx, 1);
		    }
		}
	    } else {
		if (dy > 0) {
		    if (wrapped)
			start = dy;
		    else
			start = !placed;

		    for (t = start; t <= dy && obj->life > 0; t++) {
			WallCollide(obj, sx + dx * t/dy, sy + t, t, dy, 0);
		    }
		} else {
		    if (wrapped)
			start = dy;
		    else
			start = -!placed;

		    for (t = start; t >= dy && obj->life > 0; t--) {
		        WallCollide(obj, sx + dx * t/dy, sy + t, t, dy, 0);
		    }
		}
	    }
	}
    }
}


static void PlayerCollision(int max_objs)
{
    int		i, j, x, y, sc, sc2, t;
    player	*pl;
    int		xd, yd;


    /* Player - player, checkpoint, treasure, object and wall */
    for (i=0; i<NumPlayers; i++) {
	pl = Players[i];
	if (BIT(pl->status, PLAYING|PAUSE|GAME_OVER|KILLED) != PLAYING)
	    continue;

	if (pl->pos.x < 0 || pl->pos.y < 0
	    || pl->pos.x >= World.width
	    || pl->pos.y >= World.height) {
	    SET_BIT(pl->status, KILLED);
	    sprintf(msg, "%s left the known universe.", pl->name);
	    Set_message(msg);
	    SCORE(i, -Rate(WALL_SCORE, pl->score),
		  (int) pl->pos.x/ BLOCK_SZ,
		  (int) pl->pos.y/BLOCK_SZ,
		  pl->name);
	    continue;
	}

	/* Player - player */
	if (BIT(World.rules->mode, CRASH_WITH_PLAYER)) {
	    for (j=i+1; j<NumPlayers; j++)
		if (BIT(Players[j]->status, PLAYING|PAUSE|GAME_OVER|KILLED)
		    == PLAYING
		    && in_range((object *)pl, (object *)Players[j],
				2*SHIP_SZ-6)) {
		    sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_PLAYER_SOUND);
		    if (!TEAM(i, j) && !PSEUDO_TEAM(i,j)) {
			Add_fuel(&(pl->fuel), ED_PL_CRASH);
			Add_fuel(&(Players[j]->fuel), ED_PL_CRASH);
			Item_damage(i);
			Item_damage(j);
			pl->forceVisible = 20;
			Players[j]->forceVisible = 20;
		/*	Delta_mv((object *)pl, (object *)Players[j]);	*/
			Obj_repel((object *)pl, (object *)Players[j],
				  2*SHIP_SZ);
			if (pl->fuel.sum <= 0 || !BIT(pl->used, OBJ_SHIELD))
			    SET_BIT(pl->status, KILLED);
			if (Players[j]->fuel.sum <= 0
			    || !BIT(Players[j]->used, OBJ_SHIELD))
			    SET_BIT(Players[j]->status, KILLED);

			if (BIT(Players[j]->status, KILLED)) {
			    if (BIT(pl->status, KILLED)) {
				sprintf(msg, "%s and %s crashed.",
					pl->name, Players[j]->name);
				Set_message(msg);
				sc = Rate(Players[j]->score, pl->score) / 3;
				sc2 = Rate(pl->score, Players[j]->score) / 3;
				sprintf(msg, "[%s]", Players[j]->name);
				SCORE(i, -sc, 
				      (int) pl->pos.x/BLOCK_SZ, 
				      (int) pl->pos.y/BLOCK_SZ,
				      msg);
				sprintf(msg, "[%s]", pl->name);
				SCORE(j, -sc2, 
				      (int) Players[j]->pos.x/BLOCK_SZ, 
				      (int) Players[j]->pos.y/BLOCK_SZ,
				      msg);
			    } else {
				sprintf(msg, "%s ran over %s.",
					pl->name, Players[j]->name);
				Set_message(msg);
				sound_play_sensors(Players[j]->pos.x,
						   Players[j]->pos.y,
						   PLAYER_RAN_OVER_PLAYER_SOUND);
				sc = Rate(pl->score, Players[j]->score) / 3;
				SCORE(i, sc,
				      (int) pl->pos.x/BLOCK_SZ,
				      (int) pl->pos.y/BLOCK_SZ,
				      Players[j]->name);
				SCORE(j, -sc,
				      (int) Players[j]->pos.x/BLOCK_SZ, 
				      (int) Players[j]->pos.y/BLOCK_SZ,
				      pl->name);
			    }
			} else {
			    if (BIT(pl->status, KILLED)) {
				sprintf(msg, "%s ran over %s.",
				        Players[j]->name, pl->name);
				sound_play_sensors(pl->pos.x, pl->pos.y,
						   PLAYER_RAN_OVER_PLAYER_SOUND);
				sc = Rate(Players[j]->score, pl->score) / 3;
				SCORE(i, -sc, 
				      (int) pl->pos.x/BLOCK_SZ,
				      (int) pl->pos.y/BLOCK_SZ,
				      Players[j]->name);
				SCORE(j, sc, 
				      (int) Players[j]->pos.x/BLOCK_SZ, 
				      (int) Players[j]->pos.y/BLOCK_SZ,
				      pl->name);
				Set_message(msg);
			    }
			}

			if (Players[j]->robot_mode != RM_NOT_ROBOT
			    && Players[j]->robot_mode != RM_OBJECT
			    && BIT(Players[j]->status, KILLED)
			    && Players[j]->robot_lock == LOCK_PLAYER
			    && Players[j]->robot_lock_id == pl->id)
			    Players[j]->robot_lock = LOCK_NONE;

			if (pl->robot_mode != RM_NOT_ROBOT
			    && pl->robot_mode != RM_OBJECT
			    && BIT(pl->status, KILLED)
			    && pl->robot_lock == LOCK_PLAYER
			    && pl->robot_lock_id == Players[j]->id)
			    pl->robot_lock = LOCK_NONE;
		    }
		}
	}

	/* Player checkpoint */
	if (BIT(World.rules->mode, TIMING))
	    if (Wrap_length(pl->pos.x - World.check[pl->check].x*BLOCK_SZ,
		       pl->pos.y - World.check[pl->check].y*BLOCK_SZ) < 200) {

		if (pl->check == 0) {
		    pl->round++;
		    if (((pl->best_lap > pl->time - pl->last_lap)
			 || (pl->best_lap == 0))
			&& (pl->time != 0)) {
			pl->best_lap = pl->time - pl->last_lap;
		    }
		    pl->last_lap_time = pl->time - pl->last_lap;
		    pl->last_lap = pl->time;
		}

		pl->check++;

		if (pl->check == World.NumChecks)
		    pl->check = 0;
	    }

	/* Player picking up ball/treasure */
	if (!BIT(pl->used, OBJ_CONNECTOR)) {
	    pl->ball = NULL;
	} else {
	    if (pl->ball != NULL) {
		if (Wrap_length(pl->pos.x - pl->ball->pos.x,
				pl->pos.y - pl->ball->pos.y) 
		     > BALL_STRING_LENGTH) {
		    pl->ball->id = pl->id;
		    pl->ball->owner = pl->id;
		    pl->ball->status = GRAVITY;
		    if (pl->ball->treasure != -1)
			World.treasures[pl->ball->treasure].have = false;
		    SET_BIT(pl->have, OBJ_BALL);
		    pl->ball = NULL;
		}
	    } else if (!BIT(pl->have, OBJ_BALL)) {
		for (j=0 ; j < max_objs; j++) {
		    if (BIT(Obj[j]->type, OBJ_BALL) && Obj[j]->id == -1) {
			if (Wrap_length(pl->pos.x - Obj[j]->pos.x, 
					pl->pos.y - Obj[j]->pos.y) 
			      < BALL_STRING_LENGTH) {
			    pl->ball = Obj[j];
			    break;
			}
		    }
		}
	    }
	}
  
	PlayerObjectCollision(i, max_objs);

	/* Player - wall */
	if (!BIT(pl->status, KILLED)) {
	    for(j=0; j<3 && !BIT(pl->status, KILLED|WARPING); j++) {
		float tx = pl->pos.x + ships[pl->dir].pts[j].x;
		float ty = pl->pos.y + ships[pl->dir].pts[j].y;
		if (BIT(World.rules->mode, WRAP_PLAY)) {
		    if (tx < 0)
			tx += World.width;
		    if (tx >= World.width)
			tx -= World.width;
		    if (ty < 0)
			ty += World.height;
		    if (ty >= World.height)
			ty -= World.height;
		}
		xd = (int) tx;
		yd = (int) ty;
		x = (int)(tx / BLOCK_SZ);
		y = (int)(ty / BLOCK_SZ);
		if (x < 0 || x >= World.x || y < 0 || y >= World.y) {
#ifdef FOO
		    sprintf(msg, "%s left the known universe.", pl->name);
		    Set_message(msg);
		    SET_BIT(pl->status, KILLED);
		    SCORE(i, -Rate (WALL_SCORE, pl->score), 
			  (int) pl->pos.x/BLOCK_SZ,
			  (int) pl->pos.y/BLOCK_SZ, pl->name);
#endif
		}
		else
		    switch (World.block[x][y]) {
			/* NOTE:
			 * These should be modified so that it is called with
			 * the direction (outward normal) of the side crossed
			 * by the player.  Also, should be called for every
			 * block the fighter has been into this tick.
			 */
		    case TARGET:
			{
			    target_t *targ = World.targets;
			    int t = World.NumTargets;
			    while (t--) {
				if (targ->pos.x == x && targ->pos.y == y)
				    break;
				targ++;
			    }
			    if (targ->dead_time > 0)
				break;
			}

		    case FUEL:
		    case FILLED:
		    case FILLED_NO_DRAW:
			if (!Landing(i, j, 0)) {
			    SET_BIT(pl->status, KILLED);
			    SCORE(i, -Rate(WALL_SCORE, pl->score), 
				  (int) pl->pos.x/BLOCK_SZ, 
				  (int) pl->pos.y/BLOCK_SZ,
				  "[Wall]");
			}
			break;
		    case TREASURE:
			{
			    int t;

			    for(t = 0; t < World.NumTreasures; t++) {
				if (World.treasures[t].pos.x == x
				    && World.treasures[t].pos.y == y) {
				    SET_BIT(pl->status, KILLED);
				    sprintf(msg,
					    "%s crashed with a treasure.",
					    pl->name);
				    Set_message(msg);
				    SCORE(i, -Rate(WALL_SCORE, pl->score), 
					  (int) pl->pos.x/BLOCK_SZ,
					  (int) pl->pos.y/BLOCK_SZ,
					  "[Treasure]");
				}
			    }
			}
			break;
		    case REC_LU:
			if (xd % BLOCK_SZ <= yd % BLOCK_SZ) {
			    if (!Landing(i, j, 7*RES/8)) {
				SET_BIT(pl->status, KILLED);
				SCORE(i, -Rate(WALL_SCORE, pl->score), 
				      (int) pl->pos.x/BLOCK_SZ, 
				      (int) pl->pos.y/BLOCK_SZ,
				      "[Wall]");
			    }
			}
			break;
		    case REC_RU:
			if (xd % BLOCK_SZ >= BLOCK_SZ - (yd % BLOCK_SZ)) {
			    if (!Landing(i, j, 5*RES/8)) {
				SET_BIT(pl->status, KILLED);
				SCORE(i, -Rate(WALL_SCORE, pl->score), 
				      (int) pl->pos.x/BLOCK_SZ,
				      (int) pl->pos.y/BLOCK_SZ,
				      "[Wall]");
			    }
			}
			break;
		    case REC_LD:
			if (xd % BLOCK_SZ <= BLOCK_SZ - (yd % BLOCK_SZ)) {
			    if (!Landing(i, j, RES/8)) {
				SET_BIT(pl->status, KILLED);
				SCORE(i, -Rate(WALL_SCORE, pl->score), 
				      (int) pl->pos.x/BLOCK_SZ,
				      (int) pl->pos.y/BLOCK_SZ,
				      "[Wall]");
			    }
			}
			break;
		    case REC_RD:
			if (xd % BLOCK_SZ >= yd % BLOCK_SZ) {
			    if (!Landing(i, j, 3*RES/8)) {
				SET_BIT(pl->status, KILLED);
				SCORE(i, -Rate(WALL_SCORE, pl->score), 
				      (int) pl->pos.x/BLOCK_SZ,
				      (int) pl->pos.y/BLOCK_SZ,
				      "[Wall]");
			    }
			}
			break;
		    case CANNON:
			for(t = 0;
			    World.cannon[t].pos.x != x
			    || World.cannon[t].pos.y != y;
			    t++);

			if (World.cannon[t].dead_time > 0)
			    break;

			if ((World.cannon[t].dir == DIR_UP
			     && yd%BLOCK_SZ < BLOCK_SZ/3)
			    || (World.cannon[t].dir == DIR_DOWN
				&& yd%BLOCK_SZ > 2*BLOCK_SZ/3)
			    || (World.cannon[t].dir == DIR_RIGHT
				&& xd%BLOCK_SZ < BLOCK_SZ/3)
			    || (World.cannon[t].dir == DIR_LEFT
				&& xd%BLOCK_SZ > 2*BLOCK_SZ/3)) {

			    SET_BIT(pl->status, KILLED);
			    sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_CANNON_SOUND);
			    sprintf(msg, "%s crashed with a cannon.",
				    pl->name);
			    SCORE(i, -Rate(WALL_SCORE, pl->score), 
				  (int) pl->pos.x/BLOCK_SZ,
				  (int) pl->pos.y/BLOCK_SZ,
				  "[Cannon]");
			    Set_message(msg);

			    World.cannon[t].dead_time = CANNON_DEAD_TIME;
			    World.block
				[World.cannon[t].pos.x]
				[World.cannon[t].pos.y] = SPACE;
			    World.cannon[t].active = false;
			    World.cannon[t].conn_mask = 0;
			    World.cannon[t].last_change = loops;
			    Explode_object((float)(x*BLOCK_SZ),
					   (float)(y*BLOCK_SZ),
					   World.cannon[t].dir, RES*0.4,
					   120);
			}

			break;
		    case WORMHOLE:
			{
			    int hole = wormXY(x, y);

			    if (World.wormHoles[hole].type != WORM_OUT) {
				SET_BIT(pl->status, WARPING);
				pl->wormHoleHit = hole;
			    }
			    break;
			}
		    default:
			break;
		    }
	    }
	    
	    /*
	     * Don't re-warp us if we've just warped.
	     * Let us get clear of the wormhole first.
	     */
	    if (BIT(pl->status, WARPED))
		if (BIT(pl->status, WARPING) &&
		    pl->wormHoleDest == pl->wormHoleHit) {
		    CLR_BIT(pl->status, WARPING);
		} else {
		    CLR_BIT(pl->status, WARPED);
		}
	    
	    if (BIT(pl->status, KILLED)
		&& pl->score < 0
		&& pl->robot_mode != RM_NOT_ROBOT 
                && pl->robot_mode != RM_OBJECT) {
		pl->home_base = 0;
		Pick_startpos(i);
	    }
	}
    }
}


static void PlayerObjectCollision(int i, int max_objs)
{
    int		j, killer, range, sc;
    player	*pl;
    char	*type;
    object	*obj;


    /*
     * Collision between a player and an object.
     */
    pl = Players[i]; 
    if (BIT(pl->status, PLAYING|PAUSE|GAME_OVER|KILLED) != PLAYING)
	return;

    for (j=0; j<max_objs; j++) {
	obj = Obj[j];
	if (obj->life <= 0)
	    continue;

	switch (obj->type) {
	case OBJ_TORPEDO:
	    if (pl->id == obj->id && obj->info < 8)
		continue;
	    else
		range = SHIP_SZ + TORPEDO_RANGE;
	    break;
	case OBJ_NUKE:
	    if (pl->id==Obj[j]->id && Obj[j]->info<8)
		continue;
	    else 
		range=SHIP_SZ + NUKE_RANGE;
	    break;
	case OBJ_SMART_SHOT:
	case OBJ_HEAT_SHOT:
	    range = SHIP_SZ + MISSILE_RANGE;
	    break;
	case OBJ_MINE:
	    range = SHIP_SZ + MINE_RANGE;
	    break;

	case OBJ_ROCKET_PACK:
	case OBJ_SENSOR_PACK:
	case OBJ_ECM:
	case OBJ_MINE_PACK:
	case OBJ_TANK:
	case OBJ_CLOAKING_DEVICE:
	case OBJ_ENERGY_PACK:
	case OBJ_WIDEANGLE_SHOT:
	case OBJ_BACK_SHOT:
	case OBJ_AFTERBURNER:
	case OBJ_TRANSPORTER:
	case OBJ_LASER:
	    range = SHIP_SZ + ITEM_SIZE/2;
	    if (BIT(pl->used, OBJ_SHIELD) && shieldedItemPickup == false) {
		if (!in_range((object *)pl, obj, range))
		    continue;
		SET_BIT(obj->status, GRAVITY);
		Delta_mv((object *)pl, (object *)obj);
		continue;
	    }
	    break;

	default:
	    range = SHIP_SZ;
	    break;
	}
	

	if (!in_range((object *)pl, obj, range))
	    continue;

	if (obj->id != -1) {
	    if (obj->id == pl->id) {
		if (obj->type == OBJ_SPARK || obj->type == OBJ_MINE) {
		    continue;
		}
	    }
	    else if (TEAM(GetInd[obj->id], i)) {
		continue;
	    }
	}

	obj->life = 0;
	
	Delta_mv((object *)pl, (object *)obj);

	/*
	 * Object collision.
	 */
	switch (obj->type) {
	case OBJ_WIDEANGLE_SHOT:
	    pl->extra_shots++;
	    sound_play_sensors(pl->pos.x, pl->pos.y, WIDEANGLE_SHOT_PICKUP_SOUND);
	    break;
	case OBJ_ECM:
	    pl->ecms++;
	    sound_play_sensors(pl->pos.x, pl->pos.y, ECM_PICKUP_SOUND);
	    break;
	case OBJ_TRANSPORTER:
	    pl->transporters++;
	    sound_play_sensors(pl->pos.x, pl->pos.y, TRANSPORTER_PICKUP_SOUND);
	    break;
	case OBJ_SENSOR_PACK:
	    pl->sensors++;
	    pl->updateVisibility = 1;
	    sound_play_sensors(pl->pos.x, pl->pos.y, SENSOR_PACK_PICKUP_SOUND);
	    break;
	case OBJ_AFTERBURNER:
	    SET_BIT(pl->have, OBJ_AFTERBURNER);
	    if (++pl->afterburners > MAX_AFTERBURNER)
		pl->afterburners = MAX_AFTERBURNER;
	    sound_play_sensors(pl->pos.x, pl->pos.y, AFTERBURNER_PICKUP_SOUND);
	    break;
	case OBJ_BACK_SHOT:
	    SET_BIT(pl->have, OBJ_BACK_SHOT);
	    pl->back_shots++;
	    sound_play_sensors(pl->pos.x, pl->pos.y, BACK_SHOT_PICKUP_SOUND);
	    break;
	case OBJ_ROCKET_PACK:
	    pl->missiles += 4;
	    sound_play_sensors(pl->pos.x, pl->pos.y, ROCKET_PACK_PICKUP_SOUND);
	    break;
	case OBJ_CLOAKING_DEVICE:
	    SET_BIT(pl->have, OBJ_CLOAKING_DEVICE);
	    pl->cloaks++;
	    pl->updateVisibility = 1;
	    sound_play_sensors(pl->pos.x, pl->pos.y, CLOAKING_DEVICE_PICKUP_SOUND);
	    break;
	case OBJ_ENERGY_PACK:
	    Add_fuel(&(pl->fuel), ENERGY_PACK_FUEL);
	    sound_play_sensors(pl->pos.x, pl->pos.y, ENERGY_PACK_PICKUP_SOUND);
	    break;
	case OBJ_MINE_PACK:
	    pl->mines += 1 + (rand()&1);
	    sound_play_sensors(pl->pos.x, pl->pos.y, MINE_PACK_PICKUP_SOUND);
	    break;
	case OBJ_LASER:
	    if (++pl->lasers > MAX_LASERS) {
		pl->lasers = MAX_LASERS;
	    }
	    sound_play_sensors(pl->pos.x, pl->pos.y, LASER_PICKUP_SOUND);
	    break;
	case OBJ_TANK: {
	    int c = pl->fuel.current;

	    if (pl->fuel.num_tanks < MAX_TANKS) {
		/*
		 * Set a new, empty tank in the list.
		 * update max-fuel
		 */
		int no = ++(pl->fuel.num_tanks);

		SET_BIT(pl->have, OBJ_TANK);
		pl->fuel.current = no;
		pl->fuel.max += TANK_CAP(no);
		pl->fuel.tank[no] = 0;
		pl->emptymass += TANK_MASS;
	    }
	    Add_fuel(&(pl->fuel), TANK_FUEL(pl->fuel.current));
	    pl->fuel.current = c;
	    sound_play_sensors(pl->pos.x, pl->pos.y, TANK_PICKUP_SOUND);
	    break;
	}
	case OBJ_MINE:
	    sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_MINE_SOUND);
	    if (obj->id == -1)
		sprintf(msg, "%s hit a %smine.",
			pl->name, obj->status ? "moving " : "");
	    else {
		sprintf(msg, "%s hit mine %s by %s.", pl->name,
			obj->status ? "thrown" : "dropped",
			Players[killer=GetInd[obj->id]]->name);
		sc = Rate(Players[killer]->score, pl->score) / 6;
		SCORE(killer, sc, 
		      (int) Players[killer]->pos.x/BLOCK_SZ,
		      (int) Players[killer]->pos.y/BLOCK_SZ,
		      pl->name);
		SCORE(i, -sc,
		      (int) Players[i]->pos.x/BLOCK_SZ, 
		      (int) Players[i]->pos.y/BLOCK_SZ,
		      Players[killer]->name);
	    }
	    Set_message(msg);
	    break;
        case OBJ_NUKE:
	    sprintf(msg, "%s had a full on nuclear wind in the face",
		    pl->name);
	    if (Obj[j]->id != -1) {
		killer = GetInd[Obj[j]->id];
		sprintf(msg + strlen(msg), " courtesy of %s",
			Players[killer]->name);
	    } else {
		killer = i;
	    }
	    strcat(msg, ".");
	    Set_message(msg);
	    break;
	default:
	    break;
	}

	if (!BIT(obj->type, KILLING_SHOTS))
	    continue;

	type = NULL;

	if (BIT(pl->used, OBJ_SHIELD)) {
	    switch (obj->type) {

	    case OBJ_TORPEDO:
		type = "torpedo";
	    	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EAT_TORPEDO_SHOT_SOUND);
            case OBJ_NUKE:
		if (!type)
		    {
			type = "nuke";
	    		sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EAT_HEAT_SHOT_SOUND);
		    }
	    case OBJ_HEAT_SHOT:
		if (!type)
		    {
			type = "heat shot";
	    		sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EAT_HEAT_SHOT_SOUND);
		    }
	    case OBJ_SMART_SHOT:
		if (!type)
		    {
			type = "smart shot";
	    		sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EAT_SMART_SHOT_SOUND);
		    }
		if (obj->id == -1)
		    sprintf(msg, "%s ate a %s.", pl->name, type);
		else
		    sprintf(msg, "%s ate a %s from %s.", pl->name, type,
			    Players[ killer=GetInd[obj->id] ]->name);

		Add_fuel(&(pl->fuel), ED_SMART_SHOT_HIT);
		pl->forceVisible += 2;
		Set_message(msg);
		break;

	    case OBJ_SHOT:
	    case OBJ_CANNON_SHOT:
		sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EAT_SHOT_SOUND);
		Add_fuel(&(pl->fuel), ED_SHOT_HIT);
		pl->forceVisible += 1;
		break;

	    default:
		printf("You were hit by what?\n");
		break;
	    }
	} else {
	    type = NULL;
	    switch (obj->type) {
	    case OBJ_TORPEDO:
		if (type == NULL) {
		    type = "a torpedo from ";
		}
            case OBJ_NUKE:
		if (type == NULL) {
		    type = "a nuke from ";
		    if (rand()&3) {
			if (obj->id == -1)
			    sprintf(msg, "%s ate a %s.", pl->name, type);
			else
			    sprintf(msg, "%s ate a %s from %s.", pl->name,
				    type, Players[ GetInd[obj->id] ]->name);
			Add_fuel(&(pl->fuel), ED_SMART_SHOT_HIT);
			pl->forceVisible += 2;
			Set_message(msg);
			break;
		    }
		}
	    case OBJ_SHOT:
		if (type == NULL) {
		    type = "";
		}
	    case OBJ_SMART_SHOT:
		if (type == NULL) {
		    type = "a smart shot from ";
		}
	    case OBJ_HEAT_SHOT:
		if (type == NULL) {
		    type = "a heat shot from ";
		}
		SET_BIT(pl->status, KILLED);
		if (obj->id == -1) {
		    sprintf(msg, "%s was shot down from behind.", pl->name);
		    SCORE(i, PTS_PR_PL_SHOT,
			  (int) pl->pos.x/BLOCK_SZ,
			  (int) pl->pos.y/BLOCK_SZ, "N/A");
		    killer = i;
		} else {
		    sprintf(msg, "%s was shot down by %s%s.", pl->name, type,
			    Players[killer=GetInd[obj->id]]->name);
		    if (killer == i) {
			sound_play_sensors(pl->pos.x, pl->pos.y,
					   PLAYER_SHOT_THEMSELF_SOUND);
			strcat(msg, "  How strange!");
			SCORE(i, PTS_PR_PL_SHOT,
			      (int) pl->pos.x/BLOCK_SZ, 
			      (int) pl->pos.y/BLOCK_SZ, 
			      Players[killer]->name);
		    } else {
			sc = Rate(Players[killer]->score, pl->score);
			SCORE(killer, sc, (int) pl->pos.x/BLOCK_SZ,
			      (int) pl->pos.y/BLOCK_SZ, pl->name);
			SCORE(i, -sc, (int) pl->pos.x/BLOCK_SZ, 
			      (int) pl->pos.y/BLOCK_SZ, Players[killer]->name);
		    }
		}
		Set_message(msg);
		Robot_war(i, killer);
		break;
	    case OBJ_CANNON_SHOT:
		SET_BIT(pl->status, KILLED);
		sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_CANNONFIRE_SOUND);
		sprintf(msg, "%s was hit by cannonfire.", pl->name);
		Set_message(msg);
		SCORE(i, -Rate(CANNON_SCORE, pl->score)/4,
		      (int) pl->pos.x/BLOCK_SZ, 
		      (int) pl->pos.y/BLOCK_SZ, "Cannon");
		break;
	    default:
		break;
	    }
	}
	if (BIT(pl->status, KILLED))
	    break;
    }
}


static void WallCollide(object *obj, int x, int y,
			int count, int max, int axis)
{
    player *pl;
    int t, killer;

    if (x < 0 || x >= World.x || y < 0 || y >= World.y)
	obj->life = 0;
    else {
	int type = World.block[x][y];
	
	/* Simple and common, so compute first... */
	if (type == SPACE || type == BASE)
	    ;
	/* Simple and almost as common, so compute second... */
	else if (type == FUEL || type == FILLED || type == FILLED_NO_DRAW)
	    obj->life = 0;
	else if (type == TREASURE) {
	    obj->life = 0;
	    if (obj->type == OBJ_BALL) {
		int t;
		for (t = 0; t < World.NumTreasures; t++) {
		    if (World.treasures[t].pos.x == x
			&& World.treasures[t].pos.y == y) {
			if (t == obj->treasure)
			    obj->life = LONG_MAX;
			else if (obj->owner != -1 
				 && World.treasures[t].team == 
				 Players[GetInd[obj->owner]]->team) {
			    /* Treasure have been stolen and brought back */
			    /* too home treasure. The team should be */
			    /* punished */
			    Punish_team(GetInd[obj->owner], obj->treasure);
			    World.treasures[t].count = 10;
			    obj->count = 0;   			
			}
		    }	
		}
	    }
	} else if (type == TARGET) {
	    /* shot hit a target */
	    target_t *targ = World.targets;
	    int t = World.NumTargets, j;
	    int win_score = 0, lose_score = 0, sc;
	    int nobody_flag = 0, targets_remaining = 0;

	    while (t--) {
		if (targ->pos.x == x && targ->pos.y == y)
		    break;
		targ++;
	    }

	    /* is target currently present? */
	    if (targ->dead_time > 0)
		return;
	    obj->life = 0;
	    /* a normal shot or a direct mine hit work, cannons don't */
	    if (!BIT(obj->type, (KILLING_SHOTS|OBJ_MINE) & ~OBJ_CANNON_SHOT))
		return;
	    if (obj->id <= 0) {
		return;
	    }
	    killer = GetInd[obj->id];
	    if (targ->team == Players[killer]->team)
		return;

	    switch(obj->type) {
	    case OBJ_SHOT:
		targ->damage += ED_SHOT_HIT;
		break;
	    case OBJ_SMART_SHOT: case OBJ_TORPEDO: case OBJ_HEAT_SHOT:
		targ->damage += ED_SMART_SHOT_HIT;
		break;
	    case OBJ_MINE:
		targ->damage = 0;
		break;
	    }

	    targ->conn_mask = 0;
	    targ->last_change = loops;
	    if (targ->damage > 0)
		return;
	    targ->damage = TARGET_DAMAGE;
	    targ->dead_time = TARGET_DEAD_TIME;

	    Explode_object(x*BLOCK_SZ + BLOCK_SZ/2, y*BLOCK_SZ + BLOCK_SZ/2,
			   0, RES, 200);

	    if (BIT(World.rules->mode, TEAM_PLAY)) {
		for (j = 0; j < NumPlayers; j++) {
		    if (Players[j]->team == targ->team) {
			lose_score += Players[j]->score;
			nobody_flag = 1;
		    }
		    else if (Players[j]->team == Players[killer]->team)
			win_score += Players[j]->score;
		}
	    }

	    if (!nobody_flag) {
		SCORE(killer, Rate(Players[killer]->score, CANNON_SCORE)/3,
		      targ->pos.x, targ->pos.y, "");
		return;
	    }

	    sprintf(msg, "%s blew up team %d's target.",
		    Players[killer]->name, (int) targ->team);
	    Set_message(msg);

	    for (t = 0; t < World.NumTargets; t++) {
		if (World.targets[t].team == targ->team) {
		    if (World.targets[t].dead_time == 0) {
			targets_remaining++;
		    }
		}
	    }
	    sc = Rate(win_score, lose_score);
	    for (j = 0; j < NumPlayers; j++) {
		if (Players[j]->team == targ->team) {
		    SCORE(j, -sc, targ->pos.x, targ->pos.y,
			  "Target:");
		    if (targetKillTeam && targets_remaining == 0)
			SET_BIT(Players[j]->status, KILLED);
		}
		else if (Players[j]->team == Players[killer]->team &&
			 (Players[j]->team != TEAM_NOT_SET || j == killer)) {
		    SCORE(j, sc, targ->pos.x, targ->pos.y,
			  "Target:");
		}
	    }
	} else {
	    /* These cases are more complicated, because they don't
	       entirely fill the square they're in.   The algorithm
	       that got us here basically took the line segment that
	       connects the object's previous position with its
	       current one and then calls WallCollide for every square
	       that that segment passes through.   WallCollide must
	       now examine the subsegment of that segment which is
	       contained within the square, and see if that segment
	       intersects with the shape that is in this square.
	       
	       While that's not horribly complicated to do,
	       it is a bit complicated, and quite slow, so what
	       we do instead is to compute the two endpoints of the
	       subsegment, and see if they are contained within the
	       shape in this square.   Since all of the possible
	       shapes lie to one side or the other of the square,
	       this is fairly safe; the only time it will fail
	       is if an object passes entirely through the square
	       nearly parallel to the long side of a cannon; in that
	       case, neither endpoint would be within the cannon.
	       However, this isn't a very likely case, so it seems
	       safe to discount it. */
	    
	    int ex, ey;		/* Enter coordinates... */
	    int lx, ly;		/* Leave coordinates... */
	    
	    /* We can skip the complicated calculations if the
	       motion was entirely within this square. */
	    if (!count && !max) {
		ex = obj->prevpos.x;
		ey = obj->prevpos.y;
		lx = obj->pos.x;
		ly = obj->pos.y;
	    }
	    /* Otherwise, we have to do some thinking... */
	    else {
		float ddx, ddy;
		
		/* Compute the total distance travelled... */
		ddx = WRAP_DX(obj->pos.x - obj->prevpos.x);
		ddy = WRAP_DY(obj->pos.y - obj->prevpos.y);
		
		/* Did we start in this square? */
		if (!count) {
		    ex = obj->prevpos.x;
		    ey = obj->prevpos.y;
		}
		/* Nope (groan) */
		else {
		    if (max > 0) { /* Increasing on major axis */
			if (axis) { /* Major axis is X axis */
			    ex = x * BLOCK_SZ;
			    ey = obj->prevpos.y
				+ ddy * (WRAP_DX(ex - obj->prevpos.x) / ddx);
			}
			else {	/* Major axis is Y axis */
			    ey = y * BLOCK_SZ;
			    ex = obj->prevpos.x
				+ ddx * (WRAP_DY(ey - obj->prevpos.y) / ddy);
			}
		    }
		    else {	/* Decreasing on major axis */
			if (axis) { /* Major axis is X axis */
			    ex = x * BLOCK_SZ + BLOCK_SZ - 1;
			    ey = obj->prevpos.y
				+ ddy * (WRAP_DX(ex - obj->prevpos.x) / ddx);
			}
			else {	/* Major axis is Y axis */
			    ey = y * BLOCK_SZ + BLOCK_SZ - 1;
			    ex = obj->prevpos.x
				+ ddx * (WRAP_DY(ey - obj->prevpos.y) / ddy);
			}
		    }
		}
		
		/* Did we end in this square? */
		if (count == max) {
		    lx = obj->pos.x;
		    ly = obj->pos.y;
		}
		/* Nope (groan) */
		else {
		    if (max > 0) { /* Increasing on major axis */
			if (axis) { /* Major axis is X axis */
			    lx = x * BLOCK_SZ + BLOCK_SZ - 1;
			    ly = obj->prevpos.y
				+ ddy * (WRAP_DX(lx - obj->prevpos.x) / ddx);
			}
			else {	/* Major axis is Y axis */
			    ly = y * BLOCK_SZ + BLOCK_SZ - 1;
			    lx = obj->prevpos.x
				+ ddx * (WRAP_DY(ly - obj->prevpos.y) / ddy);
			}
		    }
		    else  {	/* Decreasing on major axis */
			if (axis) { /* Major axis is X axis */
			    lx = x * BLOCK_SZ;
			    ly = obj->prevpos.y
				+ ddy * (WRAP_DX(lx - obj->prevpos.x) / ddx);
			}
			else {	/* Major axis is Y axis */
			    ly = y * BLOCK_SZ;
			    lx = obj->prevpos.x
				+ ddx * (WRAP_DY(ly - obj->prevpos.y) / ddy);
			}
		    }
		}
	    }

	    /*
	     * By default, we have to keep recomputing for oddly-
	     * shaped objects.
	     */
	    if (count == max)
		obj->placed = 1;
	    switch (type) {
	    case REC_LU:
		if ((ex % BLOCK_SZ) < (ey % BLOCK_SZ)
		    || (lx % BLOCK_SZ) < (ly % BLOCK_SZ)) {
		    obj->life = 0;
		}
		break;
		
	    case REC_RU:
		if ((ex % BLOCK_SZ) >= BLOCK_SZ - (ey % BLOCK_SZ)
		    || (lx % BLOCK_SZ) >= BLOCK_SZ - (ly % BLOCK_SZ)) {
		    obj->life = 0;
		}
		break;
		
	    case REC_LD:
		if ((ex % BLOCK_SZ) <= BLOCK_SZ - (ey % BLOCK_SZ)
		    || (lx % BLOCK_SZ) <= BLOCK_SZ - (ly % BLOCK_SZ)) {
		    obj->life = 0;
		}
		break;
		
	    case REC_RD:
		if ((ex % BLOCK_SZ) > (ey % BLOCK_SZ)
		    || (lx % BLOCK_SZ) > (ly % BLOCK_SZ)) {
		    obj->life = 0;
		}
		break;
		
	    case CANNON:
		if (!BIT(obj->type, KILLING_SHOTS&(~OBJ_CANNON_SHOT)))
		    break;
		
		for(t=0; World.cannon[t].pos.x!=x
		    || World.cannon[t].pos.y!=y; t++);
		
		if (World.cannon[t].dead_time > 0)
		    break;
		
		if ((World.cannon[t].dir == DIR_UP
		     && (ey%BLOCK_SZ <= BLOCK_SZ/3
			 || ly%BLOCK_SZ <= BLOCK_SZ/3))
		    || (World.cannon[t].dir == DIR_DOWN
			&& (ey%BLOCK_SZ >= 2*BLOCK_SZ/3
			    || ly%BLOCK_SZ >= 2*BLOCK_SZ/3))
		    || (World.cannon[t].dir == DIR_RIGHT
			&& (ex%BLOCK_SZ <= BLOCK_SZ/3
			    || lx%BLOCK_SZ <= BLOCK_SZ/3))
		    || (World.cannon[t].dir == DIR_LEFT
			&& (ex%BLOCK_SZ >= 2*BLOCK_SZ/3
			    || lx%BLOCK_SZ >= 2*BLOCK_SZ/3))) {

		    World.cannon[t].dead_time		= CANNON_DEAD_TIME;
		    World.block
			[World.cannon[t].pos.x]
			[World.cannon[t].pos.y]		= SPACE;
		    World.cannon[t].active    		= false;
		    World.cannon[t].conn_mask		= 0;
		    World.cannon[t].last_change		= loops;
		    Explode_object(x * BLOCK_SZ + BLOCK_SZ/2,
				   y * BLOCK_SZ + BLOCK_SZ/2,
				   World.cannon[t].dir, RES * 0.4, 80);

		    if (obj->id >= 0) {
			killer = GetInd[obj->id];
			pl = Players[killer];
			SCORE(killer, Rate(pl->score, CANNON_SCORE)/4,
			      x, y, "");
		    }

		    obj->life = 0;
		}
		
		break;
		
	    default:
		/*
		 * As a special case, we don't have to recompute
		 * for this one.
		 */
		if (count == max)
		    obj->placed = 0;
		break;
	    }
	}
    }
}



static bool Landing(int ind, int point, int blockdir)
{
    int	x, y, depth;
    int landdir;
    bool diagonal;
    player *pl = Players[ind];
    float tx, ty;


    /* Head first? */
    if (point == 0) {
	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_WALL_SOUND);
	sprintf(msg, "%s smashed head first into a wall.", Players[ind]->name);
	Set_message(msg);
	return false;
    }
    
    /* Way too fast? */
    if (LENGTH(pl->vel.y, pl->vel.x) > 12.0) {
	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_WALL_SOUND);
	sprintf(msg, "%s smashed into a wall.", Players[ind]->name);
	Set_message(msg);
	return false;
    }
    
    tx = pl->pos.x + ships[pl->dir].pts[point].x;
    ty = pl->pos.y + ships[pl->dir].pts[point].y;
    if (BIT(World.rules->mode, WRAP_PLAY)) {
	if (tx < 0)
	    tx += World.width;
	if (tx >= World.width)
	    tx -= World.width;
	if (ty < 0)
	    ty += World.height;
	if (ty >= World.height)
	    ty -= World.height;
    }
    x = (int) tx;
    y = (int) ty;

    landdir = ((pl->dir + RES/16) / (RES/8)) * (RES/8);
    diagonal = (landdir % (RES/4) != 0);
 
    if (diagonal && blockdir != landdir) { /* Wrong angle? */
	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_WALL_SOUND);
	sprintf(msg, "%s crashed into a wall.", Players[ind]->name);
	Set_message(msg);
        return false;
    }

    /* Wrong angle? */
    if (!diagonal && blockdir != 0
	&& MOD2(landdir+(RES-blockdir)+RES/4, RES) <= RES/2) {
	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_WALL_SOUND);
	sprintf(msg, "%s crashed into a wall.", Players[ind]->name);
	Set_message(msg);
        return false;
    }

    depth = (diagonal && blockdir==0 ? 2*BLOCK_SZ : BLOCK_SZ);
    if (landdir < RES/4 || landdir > 3*RES/4)
	depth -= x % BLOCK_SZ;
    if (landdir > RES/4 && landdir < 3*RES/4)
	depth -= BLOCK_SZ - 1 - (x % BLOCK_SZ);
    if (landdir > 0 && landdir < RES/2)
	depth -= y % BLOCK_SZ;
    if (landdir > RES/2 && landdir < RES)
	depth -= BLOCK_SZ - 1 - (y % BLOCK_SZ);
    if (diagonal)
	depth = (depth+1)/2;

    if (depth > 20) {
	sprintf(msg, "%s crashed into a wall.", Players[ind]->name);
	sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_HIT_WALL_SOUND);
	Set_message(msg);
	return false;
    }

    if (depth <= 0)
	return true;
 
    /*
     * Update velocity, position, etc
     */
    if (diagonal) {
	float tmp = pl->vel.x;
	pl->vel.x = pl->vel.y;
	pl->vel.y = tmp;
    }
 
    if (landdir < RES/4 || landdir > 3*RES/4) {
	pl->pos.x = x + depth - ships[pl->dir].pts[point].x;
	pl->vel.x = ABS(pl->vel.x);
	if (BIT(World.rules->mode, WRAP_PLAY)) {
	    if (pl->pos.x < 0)
		pl->pos.x += World.width;
	    if (pl->pos.x >= World.width)
		pl->pos.x -= World.width;
	}
    }
    if (landdir > RES/4 && landdir < 3*RES/4) {
	pl->pos.x = x - depth - ships[pl->dir].pts[point].x;
	pl->vel.x = -ABS(pl->vel.x);
	if (BIT(World.rules->mode, WRAP_PLAY)) {
	    if (pl->pos.x < 0)
		pl->pos.x += World.width;
	    if (pl->pos.x >= World.width)
		pl->pos.x -= World.width;
	}
    }
    if (landdir > 0 && landdir < RES/2) {
	pl->pos.y = y + depth - ships[pl->dir].pts[point].y;
	pl->vel.y = ABS(pl->vel.y);
	if (BIT(World.rules->mode, WRAP_PLAY)) {
	    if (pl->pos.y < 0)
		pl->pos.y += World.height;
	    if (pl->pos.y >= World.height)
		pl->pos.y -= World.height;
	}
    }
    if (landdir > RES/2 && landdir < RES) {
	pl->pos.y = y - depth - ships[pl->dir].pts[point].y;
	pl->vel.y = -ABS(pl->vel.y);
	if (BIT(World.rules->mode, WRAP_PLAY)) {
	    if (pl->pos.y < 0)
		pl->pos.y += World.height;
	    if (pl->pos.y >= World.height)
		pl->pos.y -= World.height;
	}
    }
 
    pl->vel.x *= 0.90;
    pl->vel.y *= 0.90;
    pl->float_dir -= ((pl->dir - landdir)*0.2);
    if (pl->float_dir < 0)
	pl->float_dir += RES;
    if (pl->float_dir >= RES)
	pl->float_dir -= RES;
    pl->dir = pl->float_dir;

    return true;
}



int wormXY(int x, int y)
{
    int i;
    
    for (i = 0; i < World.NumWormholes; i++)
	if (World.wormHoles[i].pos.x == x &&
	    World.wormHoles[i].pos.y == y)
	    break;

    return i;
}


static void LaserCollision(void)
{
    typedef struct victim {
	int			ind;	/* player index */
	position		pos;
	float			prev_dist;
    } victim_t;

    int				ind, i, j, k, max, hits, sc,
				max_victims = 0,
				num_victims = 0;
    unsigned			size;
    victim_t			*victims = NULL;
    float			x, y, x1, x2, y1, y2, dx, dy, dist;
    player			*pl, *vic;
    pulse_t			*pulse;
    object			obj;
    char			msg[MSG_LEN];

    if (itemLaserProb <= 0) {
	return;
    }
    for (ind = 0; ind < NumPlayers; ind++) {
	pl = Players[ind];
	if (BIT(pl->used, OBJ_LASER) != 0) {
	    if (BIT(pl->status, PLAYING|GAME_OVER|KILLED|PAUSE) != PLAYING
		|| pl->lasers <= 0) {
		CLR_BIT(pl->used, OBJ_LASER);
	    }
	    else if (pl->lasers > pl->num_pulses
		&& pl->velocity < PULSE_SPEED - PULSE_SAMPLE_DISTANCE) {
		if (pl->fuel.sum <= -ED_LASER) {
		    CLR_BIT(pl->used, OBJ_LASER);
		} else {
		    Add_fuel(&(pl->fuel), ED_LASER);
		    if (pl->num_pulses >= pl->max_pulses) {
			size = pl->lasers * sizeof(pulse_t);
			if (pl->max_pulses <= 0) {
			    pl->pulses = (pulse_t *)malloc(size);
			    pl->num_pulses = 0;
			} else {
			    pl->pulses = (pulse_t *)realloc(pl->pulses, size);
			}
			if (pl->pulses == NULL) {
			    pl->max_pulses = 0;
			    pl->num_pulses = 0;
			    continue;
			}
			pl->max_pulses = pl->lasers;
		    }
		    pulse = &pl->pulses[pl->num_pulses++];
		    pulse->dir = pl->dir;
		    pulse->len = PULSE_LENGTH;
		    pulse->life = PULSE_LIFE(pl->lasers);
		    pulse->pos.x = pl->pos.x + ships[pl->dir].pts[0].x
			- PULSE_SPEED * tcos(pulse->dir);
		    pulse->pos.y = pl->pos.y + ships[pl->dir].pts[0].y
			- PULSE_SPEED * tsin(pulse->dir);
		    sound_play_sensors(pulse->pos.x, pulse->pos.y, FIRE_LASER_SOUND);
		}
	    }
	}
	for (k = 0; k < pl->num_pulses; k++) {
	    pulse = &pl->pulses[k];
	    if (--pulse->life < 0) {
		if (--pl->num_pulses > k) {
		    pl->pulses[k] = pl->pulses[pl->num_pulses];
		    k--;
		}
		continue;
	    }
	    if (pulse->len < PULSE_LENGTH) {
		pulse->len = 0;
		continue;
	    }

	    pulse->pos.x += tcos(pulse->dir) * PULSE_SPEED;
	    pulse->pos.y += tsin(pulse->dir) * PULSE_SPEED;
	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (pulse->pos.x < 0) {
		    pulse->pos.x += World.width;
		}
		else if (pulse->pos.x >= World.width) {
		    pulse->pos.x -= World.width;
		}
		if (pulse->pos.y < 0) {
		    pulse->pos.y += World.height;
		}
		else if (pulse->pos.y >= World.height) {
		    pulse->pos.y -= World.height;
		}
		x1 = pulse->pos.x;
		y1 = pulse->pos.y;
		x2 = x1 + tcos(pulse->dir) * pulse->len;
		y2 = y1 + tsin(pulse->dir) * pulse->len;
	    } else {
		x1 = pulse->pos.x;
		y1 = pulse->pos.y;
		if (x1 < 0 || x1 >= World.width
		    || y1 < 0 || y1 >= World.height) {
		    pulse->len = 0;
		    continue;
		}
		x2 = x1 + tcos(pulse->dir) * pulse->len;
		if (x2 < 0) {
		    pulse->len = pulse->len * (0 - x1) / (x2 - x1);
		    x2 = x1 + tcos(pulse->dir) * pulse->len;
		}
		if (x2 >= World.width) {
		    pulse->len = pulse->len * (World.width - 1 - x1)
			/ (x2 - x1);
		    x2 = x1 + tcos(pl->dir) * pulse->len;
		}
		y2 = y1 + tsin(pl->dir) * pulse->len;
		if (y2 < 0) {
		    pulse->len = pulse->len * (0 - y1) / (y2 - y1);
		    x2 = x1 + tcos(pl->dir) * pulse->len;
		    y2 = y1 + tsin(pl->dir) * pulse->len;
		}
		if (y2 > World.height) {
		    pulse->len = pulse->len * (World.height - 1 - y1)
			/ (y2 - y1);
		    x2 = x1 + tcos(pl->dir) * pulse->len;
		    y2 = y1 + tsin(pl->dir) * pulse->len;
		}
		if (pulse->len <= 0) {
		    pulse->len = 0;
		    continue;
		}
	    }

	    num_victims = 0;
	    for (i = 0; i < NumPlayers; i++) {
		vic = Players[i];
		if (BIT(vic->status, PLAYING|GAME_OVER|KILLED|PAUSE)
		    != PLAYING) {
		    continue;
		}
		if (TEAM(i, ind) || PSEUDO_TEAM(i, ind)) {
		    continue;
		}
		if (Wrap_length(vic->pos.x - x1, vic->pos.y - y1)
		    > pulse->len + SHIP_SZ) {
		    continue;
		}
		if (max_victims == 0) {
		    victims = (victim_t *) malloc(NumPlayers * sizeof(victim_t));
		    if (victims == NULL) {
			break;
		    }
		    max_victims = NumPlayers;
		}
		victims[num_victims].ind = i;
		victims[num_victims].pos = vic->pos;
		victims[num_victims].prev_dist = 1e10;
		num_victims++;
	    }
	    obj.type = OBJ_SHOT;
	    obj.life = 1;
	    obj.owner = pl->id;
	    obj.id = pl->id;
	    obj.count = 0;
	    dx = x2 - x1;
	    dy = y2 - y1;
	    max = MAX(ABS(dx), ABS(dy));
	    if (max == 0) {
		continue;
	    }

	    for (i = hits = 0; i <= max; i += PULSE_SAMPLE_DISTANCE) {

		x = x1 + (i * dx) / max;
		y = y1 + (i * dy) / max;
		if (BIT(World.rules->mode, WRAP_PLAY)) {
		    if (x < 0) {
			x += World.width;
		    }
		    else if (x >= World.width) {
			x -= World.width;
		    }
		    if (y < 0) {
			y += World.height;
		    }
		    else if (y >= World.height) {
			y -= World.height;
		    }
		}
		obj.pos.x = obj.prevpos.x = x;
		obj.pos.y = obj.prevpos.y = y;
		WallCollide(&obj,
			    (int)(x / BLOCK_SZ), (int)(y / BLOCK_SZ),
			    0, 0, 0);
		if (obj.life == 0) {
		    break;
		}
		for (j = 0; j < num_victims; j++) {
		    dist = Wrap_length(x - victims[j].pos.x,
				       y - victims[j].pos.y);
		    if (dist <= SHIP_SZ) {
			hits++;
			vic = Players[victims[j].ind];
			vic->forceVisible++;
			sound_play_sensors(vic->pos.x, vic->pos.y, PLAYER_EAT_LASER_SOUND);
			if (laserIsStunGun == true) {
			    if (BIT(vic->used, OBJ_SHIELD|OBJ_LASER|OBJ_FIRE)
				|| BIT(vic->status, THRUSTING)) {
				sprintf(msg,
					"%s got paralysed by %s's stun gun.",
					vic->name, pl->name);
				Set_message(msg);
				CLR_BIT(vic->used,
					OBJ_SHIELD|OBJ_LASER|OBJ_FIRE);
				CLR_BIT(vic->status, THRUSTING);
			    }
			} else {
			    Add_fuel(&(vic->fuel), ED_LASER_HIT);
			    if (BIT(vic->used, OBJ_SHIELD) == 0) {
				SET_BIT(vic->status, KILLED);
				sc = Rate(pl->score, vic->score);
				SCORE(ind, sc, x / BLOCK_SZ, y / BLOCK_SZ,
				      vic->name);
				SCORE(victims[j].ind, -sc, x / BLOCK_SZ,
				      y / BLOCK_SZ, pl->name);
				sound_play_sensors(vic->pos.x, vic->pos.y, PLAYER_ROASTED_SOUND);
				sprintf(msg,
					"%s got roasted alive by %s's laser.",
					vic->name, pl->name);
				Set_message(msg);
				Robot_war(victims[j].ind, ind);
			    }
			}
		    }
		    else if (dist >= victims[j].prev_dist) {
			victims[j] = victims[--num_victims];
			j--;
		    } else {
			victims[j].prev_dist = dist;
		    }
		}
		if (hits > 0) {
		    break;
		}
	    }
	    if (i < max) {
		pulse->len = (pulse->len * i) / max;
	    }
	}
    }
    if (max_victims > 0 && victims != NULL) {
	free(victims);
    }
}

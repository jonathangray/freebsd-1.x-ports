/* $Id: update.c,v 1.1 1994/02/23 14:40:08 jkh Exp $
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
#include "draw.h"
#include "robot.h"
#include "bit.h"
#include "saudio.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: update.c,v 1.1 1994/02/23 14:40:08 jkh Exp $";
#endif



#define speed_limit(obj) {					\
    if (ABS((obj).velocity>SPEED_LIMIT)) {			\
	int theta = findDir((obj).vel.x, (obj).vel.y);		\
	(obj).vel.x = tcos(theta) * SPEED_LIMIT;		\
	(obj).vel.y = tsin(theta) * SPEED_LIMIT;		\
    }								\
}

#define update_object_pos(obj)	{					\
    int x=(int)((obj).pos.x/BLOCK_SZ), y=(int)((obj).pos.y/BLOCK_SZ);	\
    (obj).wrapped = 0;                                               	\
    if (x<0 || x>=World.x || y<0 || y>=World.y) {			\
	LIMIT((obj).pos.x, 0.0, World.width-1.0);		    	\
	LIMIT((obj).pos.y, 0.0, World.height-1.0);		    	\
    } else {								\
	(obj).prevpos = (obj).pos;					\
	if (BIT((obj).status, GRAVITY)) {				\
	    (obj).pos.x += (obj).vel.x += (obj).acc.x +World.gravity[x][y].x;\
	    (obj).pos.y += (obj).vel.y += (obj).acc.y +World.gravity[x][y].y;\
	} else {							\
	    (obj).pos.x += (obj).vel.x += (obj).acc.x;			\
	    (obj).pos.y += (obj).vel.y += (obj).acc.y;			\
	}								\
	if ((obj).pos.x < 0 ||						\
	    (obj).pos.x >= World.width ||				\
	    (obj).pos.y < 0 ||						\
	    (obj).pos.y >= World.height) {				\
	    Limit_object_pos((object *)&(obj));				\
	}								\
    }									\
    /*  speed_limit(obj); */						\
    (obj).velocity = LENGTH((obj).vel.x, (obj).vel.y);			\
}


static char msg[MSG_LEN];


/*
 * This was part of the update_object_pos() macro first, but
 * some C preprocessor couldn't handle the length.
 * Because objects only very infrequently traverse the edge the
 * speed loss of moving this to a function is negligible.
 */
static void Limit_object_pos(object *obj)
{
    if (!BIT(World.rules->mode, WRAP_PLAY)) {
	if (obj->pos.x < 0) {
	    if (edgeBounce) {
		obj->pos.x = -obj->pos.x;
		if (obj->vel.x < 0) {
		    obj->vel.x = -obj->vel.x;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = MOD2(RES / 2 - obj->dir, RES);
		    }
		}
	    } else {
		obj->pos.x = 0;
		if (obj->vel.x < 0) {
		    obj->vel.x = 0;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = (obj->vel.y < 0) ? (3*RES/4) : RES/4;
		    }
		}
	    }
	}
	if (obj->pos.x >= World.width) {
	    if (edgeBounce) {
		obj->pos.x = 2 * World.width - obj->pos.x;
		if (obj->vel.x > 0) {
		    obj->vel.x = -obj->vel.x;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = MOD2(RES / 2 - obj->dir, RES);
		    }
		}
	    } else {
		obj->pos.x = World.width - 1;    
		if (obj->vel.x > 0) {
		    obj->vel.x = 0;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = (obj->vel.y < 0) ? (3*RES/4) : RES/4;
		    }
		}
	    }
	}
	if (obj->pos.y < 0) {
	    if (edgeBounce) {
		obj->pos.y = -obj->pos.y;
		if (obj->vel.y < 0) {
		    obj->vel.y = -obj->vel.y;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = MOD2(RES - obj->dir, RES);
		    }
		}
	    } else {
		obj->pos.y = 0;
		if (obj->pos.y < 0) {
		    obj->vel.y = 0;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = (obj->vel.x < 0) ? (RES/2) : 0;
		    }
		}
	    }
	}
	if (obj->pos.y >= World.height) {
	    if (edgeBounce) {
		obj->pos.y = 2 * World.height - obj->pos.y;
		if (obj->vel.y > 0) {
		    obj->vel.y = -obj->vel.y;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = MOD2(RES - obj->dir, RES);
		    }
		}
	    } else {
		obj->pos.y = World.height - 1;
		if (obj->vel.y > 0) {
		    obj->vel.y = 0;
		    if (obj->type != OBJ_PLAYER) {
			obj->dir = (obj->vel.x < 0) ? (RES/2) : 0;
		    }
		}
	    }
	}
    } else {
	if (obj->pos.x < 0) {
	    obj->pos.x += World.width;	    
	    obj->wrapped |= 1;
	}
	if (obj->pos.x >= World.width) {
	    obj->pos.x -= World.width;
	    obj->wrapped |= 1;
	}
	if (obj->pos.y < 0) {
	    obj->pos.y += World.height;
	    obj->wrapped |= 2;
	}
	if (obj->pos.y >= World.height) {
	    obj->pos.y -= World.height;
	    obj->wrapped |= 2;
	}
    }
}


#ifndef OLD_TRANSPORT_TO_HOME
static void Transport_to_home(int ind)
{
    /*
     * Transport a corpse from the place where it died back to its homebase.
     * During the first part of the distance we give it a positive constant
     * acceleration G, during the second part we make this a negative one -G.
     * This results in a visually pleasing take off and landing.
     */
    player		*pl = Players[ind];
    float		bx, by, dx, dy,	t, m;
    const int		T = RECOVERY_DELAY;

    bx = World.base[pl->home_base].pos.x * BLOCK_SZ + BLOCK_SZ/2;
    by = World.base[pl->home_base].pos.y * BLOCK_SZ + BLOCK_SZ/2;
    dx = WRAP_DX(bx - pl->pos.x);
    dy = WRAP_DY(by - pl->pos.y);
    t = pl->count + 0.5f;
    if (2 * t <= T) {
	m = 2 / t;
    } else {
	t = T - t;
	m = (4 * t) / (T * T - 2 * t * t);
    }
    pl->vel.x = dx * m;
    pl->vel.y = dy * m;
}
#endif


/********** **********
 * Updating objects and the like.
 */
void Update_objects(void)
{
    int i, j;
    player *pl;
    object *obj;


    /*
     * Update robots.
     */
    Update_robots();

    /*
     * Autorepeat fire, must unfortunately be done here, not in
     * the player loop below, because of collisions between the shots
     * and the auto-firing player that would otherwise occur.
     */
    if (fireRepeatRate > 0) {
	for (i = 0; i < NumPlayers; i++) {
	    pl = Players[i];
	    if (BIT(pl->used, OBJ_FIRE)
		&& loops - pl->shot_time >= fireRepeatRate) {
		Fire_normal_shots(i);
	    }
	}
    }

    /*
     * Special items.
     */
    for (i=0; i<NUM_ITEMS; i++)
	if (World.items[i].num < World.items[i].max
	    && World.items[i].chance > 0
	    && rand()%World.items[i].chance == 0) {

	    Place_item(i, 0);
	}

    /*
     * Let the fuel stations regenerate some fuel.
     */
    if (NumPlayers > 0) {
	int fuel = NumPlayers * STATION_REGENERATION;
	int frames_per_update = MAX_STATION_FUEL / (fuel * BLOCK_SZ);
	for (i=0; i<World.NumFuels; i++) {
	    if (World.fuel[i].fuel == MAX_STATION_FUEL) {
		continue;
	    }
	    if ((World.fuel[i].fuel += fuel) >= MAX_STATION_FUEL) {
		World.fuel[i].fuel = MAX_STATION_FUEL;
	    }
	    else if (World.fuel[i].last_change + frames_per_update > loops) {
		/*
		 * We don't send fuelstation info to the clients every frame
		 * if it wouldn't change their display.
		 */
		continue;
	    }
	    World.fuel[i].conn_mask = 0;
	    World.fuel[i].last_change = loops;
	}
    }

    /*
     * Update shots.
     */
    for (i=0; i<NumObjs; i++) {
	obj = Obj[i];

	update_object_pos(*obj);

	if (BIT(obj->type, OBJ_BALL) && obj->id != -1)
	    Move_ball(i);

        if (BIT(obj->type, OBJ_SMART_SHOT|OBJ_HEAT_SHOT
		|OBJ_TORPEDO|OBJ_NUKE))
	    Move_smart_shot(i);
    }

    /*
     * Updating cannons, maybe a little bit of fireworks too?
     */
    for (i=0; i<World.NumCannons; i++) {
	if (World.cannon[i].dead_time > 0) {
	    if (!--World.cannon[i].dead_time) {
		World.block[World.cannon[i].pos.x][World.cannon[i].pos.y]
		    = CANNON;
		World.cannon[i].conn_mask = 0;
		World.cannon[i].last_change = loops;
	    }
	    continue;
	}
	if (World.cannon[i].active) {
	    if (rand()%20 == 0)
		Cannon_fire(i);
	}
	World.cannon[i].active = false;
    }
    
    /*
     * Update targets
     */
    for (i = 0; i < World.NumTargets; i++) {
	if (World.targets[i].dead_time > 0) {
	    World.targets[i].dead_time--;
	    World.targets[i].conn_mask = 0;
	    World.targets[i].last_change = loops;
	    continue;
	}
	else if (World.targets[i].damage == TARGET_DAMAGE) {
	    continue;
	}
	World.targets[i].damage += TARGET_REPAIR_PER_FRAME;
	if (World.targets[i].damage >= TARGET_DAMAGE) {
	    World.targets[i].damage = TARGET_DAMAGE;
	}
	else if (World.targets[i].last_change + TARGET_UPDATE_DELAY < loops) {
	    /*
	     * We don't send target info to the clients every frame
	     * if the latest repair wouldn't change their display.
	     */
	    continue;
	}
	World.targets[i].conn_mask = 0;
	World.targets[i].last_change = loops;
    }

    /* * * * * *
     *
     * Player loop. Computes miscellaneous updates.
     *
     */
    for (i=0; i<NumPlayers; i++) {
#ifdef TURN_FUEL
        long tf = 0;
#endif

	pl = Players[i];

	/* Limits. */
	LIMIT(pl->power, MIN_PLAYER_POWER, MAX_PLAYER_POWER);
	LIMIT(pl->turnspeed, MIN_PLAYER_TURNSPEED, MAX_PLAYER_TURNSPEED);
	LIMIT(pl->turnresistance, MIN_PLAYER_TURNRESISTANCE,
				  MAX_PLAYER_TURNRESISTANCE);

	if (pl->count > 0) {
	    pl->count--;
	    if (!BIT(pl->status, PLAYING)) {
#ifdef OLD_TRANSPORT_TO_HOME
		pl->vel.x = WRAP_DX(BLOCK_SZ * World.base[pl->home_base].pos.x
			     + BLOCK_SZ/2 - pl->pos.x) / (pl->count + 1);
		pl->vel.y = WRAP_DY(BLOCK_SZ * World.base[pl->home_base].pos.y
			     + BLOCK_SZ/2 - pl->pos.y) / (pl->count + 1);
#else
		Transport_to_home(i);
#endif
		goto update;
	    }
	}

	if (pl->count == 0) {
	    pl->count = -1;

	    if (!BIT(pl->status, PLAYING)) {
		SET_BIT(pl->status, PLAYING);
		Go_home(i);
	    }
	    if (BIT(pl->status, SELF_DESTRUCT)) {
		sprintf(msg, "%s has comitted suicide.", pl->name);
		Set_message(msg);
		Throw_items(pl);
		Kill_player(i);
		pl->check = 0;
		pl->round = 0;
		pl->time  = 0;
	    }
	}

	if (BIT(pl->status, PLAYING|GAME_OVER|PAUSE) != PLAYING)
	    continue;

	if (pl->shield_time > 0) {
	    if (--pl->shield_time == 0) {
		CLR_BIT(pl->used, OBJ_SHIELD);
	    }
	    if (BIT(pl->used, OBJ_SHIELD) == 0) {
		CLR_BIT(pl->have, OBJ_SHIELD);
		pl->shield_time = 0;
	    }
	}

	/*
	 * Compute turn
	 */

	pl->turnvel	+= pl->turnacc;
	pl->turnvel	*= pl->turnresistance;
        
#ifdef TURN_FUEL
        tf = pl->oldturnvel-pl->turnvel;
        tf = TURN_FUEL(tf);
        if (pl->fuel.sum <= tf) {
            tf = 0;
            pl->turnacc = 0.0;
            pl->turnvel = pl->oldturnvel;
        } else {
            Add_fuel(&(pl->fuel),-tf);
            pl->oldturnvel = pl->turnvel;
        }
#endif


	pl->float_dir	+= pl->turnvel;
        
	if (pl->float_dir < 0)
	    pl->float_dir += RES;
	if (pl->float_dir >= RES)
	    pl->float_dir -= RES;

	pl->dir = pl->float_dir;
	pl->dir %= RES;


	/*
	 * Compute energy drainage
	 */
	if (BIT(pl->used, OBJ_SHIELD))
            Add_fuel(&(pl->fuel), ED_SHIELD);

	if (BIT(pl->used, OBJ_CLOAKING_DEVICE))
            Add_fuel(&(pl->fuel), ED_CLOAKING_DEVICE);

#define UPDATE_RATE 100

	for (j = 0; j < NumPlayers; j++) {
	    if (pl->forceVisible)
		Players[j]->visibility[i].canSee = 1;

	    if (i == j || !BIT(Players[j]->used, OBJ_CLOAKING_DEVICE))
		pl->visibility[j].canSee = 1;
	    else if (pl->updateVisibility
		     || Players[j]->updateVisibility
		     || rand() % UPDATE_RATE
		     < ABS(loops - pl->visibility[j].lastChange)) {

		pl->visibility[j].lastChange = loops;
		pl->visibility[j].canSee
		    = rand() % (pl->sensors + 1)
			> (rand() % (Players[j]->cloaks + 1));
	    }
	}

	if (BIT(pl->used, OBJ_REFUEL)) {
	    if ((Wrap_length((pl->pos.x-World.fuel[pl->fs].pos.x),
		             (pl->pos.y-World.fuel[pl->fs].pos.y)) > 90.0)
		|| (pl->fuel.sum >= pl->fuel.max)) {
		CLR_BIT(pl->used, OBJ_REFUEL);
	    } else {
                int i = pl->fuel.num_tanks;
                int ct = pl->fuel.current;
                
                do {
		    if (World.fuel[pl->fs].fuel > REFUEL_RATE) {
		        World.fuel[pl->fs].fuel -= REFUEL_RATE;
			World.fuel[pl->fs].conn_mask = 0;
			World.fuel[pl->fs].last_change = loops;
		        Add_fuel(&(pl->fuel), REFUEL_RATE);
		    } else {
		        Add_fuel(&(pl->fuel), World.fuel[pl->fs].fuel);
		        World.fuel[pl->fs].fuel = 0;
		        World.fuel[pl->fs].conn_mask = 0;
		        World.fuel[pl->fs].last_change = loops;
		        CLR_BIT(pl->used, OBJ_REFUEL);
                        break;
		    }
                    if (pl->fuel.current == pl->fuel.num_tanks)
                        pl->fuel.current = 0;
                    else
                        pl->fuel.current += 1;
                } while (i--);
                pl->fuel.current = ct;
            }
	}

	/* target repair */
	if (BIT(pl->used, OBJ_REPAIR)) {
	    target_t *targ = &World.targets[pl->repair_target];
	    float x = targ->pos.x*BLOCK_SZ+BLOCK_SZ/2;
	    float y = targ->pos.y*BLOCK_SZ+BLOCK_SZ/2;
	    if (Wrap_length(pl->pos.x - x, pl->pos.y - y) > 90.0
		|| targ->damage >= TARGET_DAMAGE
		|| targ->dead_time > 0) {
		CLR_BIT(pl->used, OBJ_REPAIR);
	    } else {
		int i = pl->fuel.num_tanks;
		int ct = pl->fuel.current;

		do {
		    if (pl->fuel.tank[pl->fuel.current] > REFUEL_RATE) {
			targ->damage += TARGET_FUEL_REPAIR_PER_FRAME;
			targ->conn_mask = 0;
			targ->last_change = loops;
			Add_fuel(&(pl->fuel), -REFUEL_RATE);
			if (targ->damage > TARGET_DAMAGE) {
			    targ->damage = TARGET_DAMAGE;
			    break;
			}
		    } else {
			CLR_BIT(pl->used, OBJ_REPAIR);
		    }
		    if (pl->fuel.current == pl->fuel.num_tanks)
			pl->fuel.current = 0;
		    else
			pl->fuel.current += 1;
		} while (i--);
		pl->fuel.current = ct;
	    }
	}

	if (pl->fuel.sum <= 0) {
	    CLR_BIT(pl->used, OBJ_SHIELD|OBJ_CLOAKING_DEVICE);
	    CLR_BIT(pl->status, THRUSTING);
	}
	if (pl->fuel.sum > (pl->fuel.max-REFUEL_RATE))
	    CLR_BIT(pl->used, OBJ_REFUEL);

	/*
	 * Update acceleration vector etc.
	 */
	if (BIT(pl->status, THRUSTING)) {
            float power = pl->power;
            float f = pl->power * 0.0006;
            int a = pl->afterburners;

            if (a) {
                power = AFTER_BURN_POWER(power, a);
                f = AFTER_BURN_FUEL((int)f, a);
            }
	    pl->acc.x = power * tcos(pl->dir) / pl->mass;
	    pl->acc.y = power * tsin(pl->dir) / pl->mass;
            Add_fuel(&(pl->fuel), -f * FUEL_SCALE_FACT); /* Decrement fuel */
	} else {
	    pl->acc.x = pl->acc.y = 0.0;
	}

	pl->mass = pl->emptymass + FUEL_MASS(pl->fuel.sum);

	if (BIT(pl->status, WARPING)) {
	    int wx, wy, proximity,
	    	nearestFront, nearestRear,
	    	proxFront, proxRear;

	    if (World.wormHoles[pl->wormHoleHit].countdown
		&& World.wormHoles[pl->wormHoleHit].lastplayer != i)
		j = World.wormHoles[pl->wormHoleHit].lastdest;
	    else if (rand() % 100 < 10)
		do
		    j = rand() % World.NumWormholes;
		while (World.wormHoles[j].type == WORM_IN
		       || pl->wormHoleHit == j);
	    else {
		nearestFront = nearestRear = -1;
		proxFront = proxRear = 10000000;
		
		for (j = 0; j < World.NumWormholes; j++) {
		    if (j == pl->wormHoleHit
			|| World.wormHoles[j].type == WORM_IN)
			continue;
		    
		    wx = (World.wormHoles[j].pos.x -
			  World.wormHoles[pl->wormHoleHit].pos.x) * BLOCK_SZ;
		    wy = (World.wormHoles[j].pos.y - 
			  World.wormHoles[pl->wormHoleHit].pos.y) * BLOCK_SZ;
		    
		    proximity = pl->vel.y * wx + pl->vel.x * wy;
		    proximity = ABS(proximity);
		    
		    if (pl->vel.x * wx + pl->vel.y * wy < 0) {
			if (proximity < proxRear) {
			    nearestRear = j;
			    proxRear = proximity;
			}
		    } else if (proximity < proxFront) {
			nearestFront = j;
			proxFront = proximity;
		    }
		}
		
#define RANDOM_REAR_WORM
#ifndef RANDOM_REAR_WORM
		j = nearestFront < 0 ? nearestRear : nearestFront;
#else /* RANDOM_REAR_WORM */
		if (nearestFront >= 0)
		    j = nearestFront;
		else
		    do
			j = rand() % World.NumWormholes;
		    while (World.wormHoles[j].type == WORM_IN);
#endif /* RANDOM_REAR_WORM */
	    }

            sound_play_sensors(pl->pos.x, pl->pos.y, WORM_HOLE_SOUND);

	    pl->wormHoleDest = j;
	    pl->pos.x = World.wormHoles[j].pos.x * BLOCK_SZ + BLOCK_SZ / 2;
	    pl->pos.y = World.wormHoles[j].pos.y * BLOCK_SZ + BLOCK_SZ / 2;
	    pl->vel.x /= WORM_BRAKE_FACTOR;
	    pl->vel.y /= WORM_BRAKE_FACTOR;
	    pl->prevpos = pl->pos;
	    pl->forceVisible += 15;

	    if (j != pl->wormHoleHit) {
		World.wormHoles[pl->wormHoleHit].lastplayer = i;
		World.wormHoles[pl->wormHoleHit].lastdest = j;
		World.wormHoles[pl->wormHoleHit].countdown = WORMCOUNT;
	    }

	    CLR_BIT(pl->status, WARPING);
	    SET_BIT(pl->status, WARPED);

            sound_play_sensors(pl->pos.x, pl->pos.y, WORM_HOLE_SOUND);
	}

	pl->ecmInfo.size >>= 1;

	if (BIT(pl->used, OBJ_ECM)) {
	    pl->ecmInfo.pos.x = pl->pos.x;
	    pl->ecmInfo.pos.y = pl->pos.y;
	    pl->ecmInfo.size = ECM_DISTANCE * 2;
	    CLR_BIT(pl->used, OBJ_ECM);
	}

	if (pl->transInfo.count)
	    pl->transInfo.count--;

    update:
	if (!BIT(pl->status, PAUSE))
	    update_object_pos(*pl);	    /* New position */

	if (BIT(pl->status, THRUSTING))
	    Thrust(i);
#ifdef TURN_FUEL
	if (tf)
            Turn_thrust(i, TURN_SPARKS(tf));
#endif

	/*
	 * Updating time player has used. Only used when timing
	 */
	if (pl->round != 0)
	    pl->time++;

	if (pl->round == 4) {
	    if ((pl->time < pl->best_run) || (pl->best_run == 0)) 
		pl->best_run = pl->time;
	    Players[i]->last_lap_time = Players[i]->time 
		- Players[i]->last_lap;
	    Kill_player(i);
	    pl->round = 0;
	    pl->check = 0;
	    pl->time = 0;
	}

	switch (pl->lock.tagged) {
	case LOCK_PLAYER:
	    pl->lock.distance = Wrap_length(pl->pos.x -
				     Players[GetInd[pl->lock.pl_id]]->pos.x,
				     pl->pos.y -
				     Players[GetInd[pl->lock.pl_id]]->pos.y);
	    pl->sensor_range = MAX(pl->fuel.sum * ENERGY_RANGE_FACTOR,
				   VISIBILITY_DISTANCE);
	    break;

	default:
	    break;
	}

	pl->used &= pl->have;
    }

    for (i=0; i<World.NumWormholes; i++)
       if (World.wormHoles[i].countdown)
	   --World.wormHoles[i].countdown;
	    
    for (i = 0; i < NumPlayers; i++) {
	Players[i]->updateVisibility = 0;

	if (Players[i]->forceVisible) {
	    Players[i]->forceVisible--;

	    if (!Players[i]->forceVisible)
		Players[i]->updateVisibility = 1;
	}
    }

    /*
     * Checking for collision, updating score etc. (see collision.c)
     */
    Check_collision();


    /*
     * Update tanks, Kill players that ought to be killed.
     */
    for (i=NumPlayers-1; i>=0; i--) {
        player *pl = Players[i];
        
	if (BIT(pl->status, PLAYING|PAUSE) == PLAYING)
	    Update_tanks(&(pl->fuel));
	if (BIT(pl->status, KILLED)) {
            if (pl->robot_mode != RM_OBJECT) {
		Throw_items(pl);
	        Kill_player(i);
            } else {
                NumPseudoPlayers--;
                Explode(i);
                Delete_player(i);
		updateScores = true;
            }
        }
    }

    /*
     * Kill shots that ought to be dead.
     */
    for (i=NumObjs-1; i>=0; i--)
	if (--(Obj[i]->life) <= 0)
	    Delete_shot(i);

    /*
     * Compute general game status, do we have a winner?
     * (not called after Game_Over() )
     */
    if (gameDuration >= 0.0) {
	Compute_game_status();
    }

    /*
     * Now update labels if need be.
     */
    if (updateScores && loops % UPDATE_SCORE_DELAY == 0)
	Update_score_table();
}

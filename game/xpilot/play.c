/* $Id: play.c,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define SERVER
#include "global.h"
#include "draw.h"
#include "score.h"
#include "robot.h"
#include "saudio.h"
#include "bit.h"
#include "netserver.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: play.c,v 1.1 1994/02/23 14:40:06 jkh Exp $";
#endif


/******************************
 * Functions for ship movement.
 */

void Thrust(int ind)
{
    player *pl = Players[ind];
    object *spark;
    int dir, num_sparks, the_color, alt_thrust, spread;
    float the_mass;
    const int max_spread = 1 + RES*0.4;
    const int half_spread = max_spread / 2;
    const int spreadoffset = pl->dir + RES/2 - half_spread;
    const int max_speed = 1 + (int)(pl->power * 0.14);
    const int max_life = 1 + (int)(pl->power * 0.35);
    float x, y, speed;

    sound_play_sensors(pl->pos.x, pl->pos.y, THRUST_SOUND);
    num_sparks = (pl->power*0.3) + (rand()%3) + 2;
    alt_thrust = pl->afterburners
	? AFTER_BURN_SPARKS(num_sparks-1, pl->afterburners) + 1
	    : -1;
    the_color = RED;
    the_mass = THRUST_MASS;
    
    x = pl->pos.x + (ships[pl->dir].pts[1].x + ships[pl->dir].pts[2].x) / 2;
    y = pl->pos.y + (ships[pl->dir].pts[1].y + ships[pl->dir].pts[2].y) / 2;

    for (;num_sparks && NumObjs<MAX_TOTAL_SHOTS; NumObjs++) {
	spread		= rand() % max_spread;
	dir		= MOD2(spreadoffset + spread, RES);
	speed		= 1 + rand() % max_speed;
	spark		= Obj[NumObjs];
	spark->dir	= dir;
	spark->vel.x	= pl->vel.x + tcos(dir) * speed;
	spark->vel.y	= pl->vel.y + tsin(dir) * speed;
	spark->pos.x	= x;
	spark->pos.y	= y;
	spark->prevpos  = spark->pos;
	spark->placed   = 1;
	spark->status	= GRAVITY;
	spark->acc.x	= spark->acc.y = 0.0;
	spark->mass	= the_mass;
	spark->type	= OBJ_SPARK;
	spark->life	= 3 + (rand() % max_life);
	spark->color	= the_color;
	spark->id	= pl->id;
        if (--num_sparks==alt_thrust) {
            the_color = BLUE;
            the_mass = THRUST_MASS*ALT_SPARK_MASS_FACT;
        }
    }
}


#ifdef TURN_FUEL
void Turn_thrust(int ind,int num_sparks)
{
    player *pl = Players[ind];
    object *spark;
    int i, dir;
    const int spread = (RES*0.2);
    const int spreadoffset = (spread/2);
    int x, y;
    int rate = ABS(Players[ind]->turnacc);

    x = pl->pos.x + ships[pl->dir].pts[0].x;
    y = pl->pos.y + ships[pl->dir].pts[0].y;

    for (i=0; i<num_sparks && NumObjs<MAX_TOTAL_SHOTS; i++, NumObjs++) {
	spark = Obj[NumObjs];
	dir = pl->dir + (RES/4) + (rand()%(1+spread)) - spreadoffset - 1;

	if (pl->turnacc > 0.0)
	    dir = dir + RES/2;

	spark->color	= RED;
	spark->id	= pl->id;
	spark->pos.x	= x;
	spark->pos.y	= y;
	spark->prevpos  = spark->pos;
	spark->placed   = 1;
	spark->vel.x	= pl->vel.x + (tcos(dir) * (rand()&3));
	spark->vel.y	= pl->vel.y + (tsin(dir) * (rand()&3));
	spark->status	= GRAVITY;
	spark->acc.x	= spark->acc.y = 0;
	spark->dir	= MOD2(spark->dir, RES);
	spark->mass	= THRUST_MASS;
	spark->type	= OBJ_SPARK;
	spark->life	= 1 + (rand()%(2*FPS));
    }
}
#endif


/* Calculates the recoil if a ship fires a shot */
void Recoil(object *ship, object *shot)
{
    ship->vel.x -= ((tcos(shot->dir) * ABS(shot->vel.x-ship->vel.x) *
	shot->mass) / ship->mass);
    ship->vel.y -= ((tsin(shot->dir) * ABS(shot->vel.y-ship->vel.y) *
	shot->mass) / ship->mass);
}


/* Calculates the effect of a collision between to objects */
void Delta_mv(object *ship, object *obj)
{
#if OLD_DELTA_MV
    float	dvx, dvy;
    int		ship_theta, obj_theta;
    float	dm;


    ship_theta = findDir(ship->vel.x, ship->vel.y);
    obj_theta = findDir(obj->vel.x, obj->vel.y);
    if (obj_theta == 0 && obj->vel.x == 0.0) {
	if (ship_theta == 0 && ship->vel.x == 0.0) {
	    return;
	}
	obj_theta = MOD2(ship_theta + RES/2, RES);
    }
    else if (ship_theta == 0 && ship->vel.x == 0.0) {
	ship_theta = MOD2(obj_theta + RES/2, RES);
    }

    dm = obj->mass/ship->mass;
    dvx = ABS(obj->vel.x - ship->vel.x);
    dvy = ABS(obj->vel.y - ship->vel.y);
    if (LENGTH(dvx, dvy) > SPEED_LIMIT) {
	int theta = findDir(dvx, dvy);
	dvx = tcos(theta) * SPEED_LIMIT;
	dvy = tsin(theta) * SPEED_LIMIT;
    }
    ship->vel.x += tcos(obj_theta) * dvx * dm;
    ship->vel.y += tsin(obj_theta) * dvy * dm;

    obj->vel.x += tcos(ship_theta) * dvx / dm;
    obj->vel.y += tsin(ship_theta) * dvy / dm;
#else
    float	vx, vy, m;

    m = ship->mass + obj->mass;
    vx = (ship->vel.x * ship->mass + obj->vel.x * obj->mass) / m;
    vy = (ship->vel.y * ship->mass + obj->vel.y * obj->mass) / m;
    ship->vel.x = vx;
    ship->vel.y = vy;
    obj->vel.x = vx;
    obj->vel.y = vy;
#endif
}


void Obj_repel(object *obj1, object *obj2, int repel_dist)
{
    float xd, yd, force, dm;
    int obj_theta;
 
    xd = WRAP_DX(obj2->pos.x - obj1->pos.x);
    yd = WRAP_DY(obj2->pos.y - obj1->pos.y);
    force = (repel_dist - LENGTH(xd, yd));
 
    if (force <= 0)
	return;
 
    force = MIN(force, 10);
 
    obj_theta = (int)findDir(xd, yd);
 
    dm = obj1->mass/obj2->mass;
    obj2->vel.x += tcos(obj_theta) * force * dm;
    obj2->vel.y += tsin(obj_theta) * force * dm;

    obj1->vel.x -= tcos(obj_theta) * force / dm;
    obj1->vel.y -= tsin(obj_theta) * force / dm;
}

 
void Item_damage(int ind)
{
    player *pl;
 
    if (rand() > destroyItemInCollisionProb * RAND_MAX)
       return;

    pl			= Players[ind];
    pl->extra_shots	= rand() % (pl->extra_shots + 1);
    pl->back_shots	= rand() % (pl->back_shots + 1);
    pl->missiles	= rand() % (pl->missiles + 1);
    pl->mines		= rand() % (pl->mines + 1);
    pl->ecms		= rand() % (pl->ecms + 1);
    pl->cloaks		= rand() % (pl->cloaks + 1);
    pl->sensors		= rand() % (pl->sensors + 1);
    pl->lasers		= rand() % (pl->lasers + 1);
 
    if (pl->cloaks <= 0) {
	CLR_BIT(pl->used, OBJ_CLOAKING_DEVICE);
    }
}
 


/***********************
 * Functions for shots.
 */

static object *objArray;

void Alloc_shots(int number)
{
    object *x = (object *)malloc(number*sizeof(object));
    int i;

    objArray = x;
    for (i=0; i<number; i++)
	Obj[i] = x++;
}


void Free_shots(void)
{
    free(objArray);
}


void Place_item(int type, player *pl)
{
    object *item;
    int x, y;
    float vx, vy;
    bool grav;


    if (NumObjs >= MAX_TOTAL_SHOTS)
        return;

    item = Obj[NumObjs++];

    if (pl) {
        grav = GRAVITY;
        x = pl->prevpos.x / BLOCK_SZ;
        y = pl->prevpos.y / BLOCK_SZ;
    } else {
        if ((rand()&127) < MovingItemsRand)
            grav = GRAVITY;
        else {
            grav = 0;
        }
	x = rand()%World.x;
	y = rand()%World.y;
    }
    while (World.block[x][y] != SPACE) {
	/*
	 * This will take very long (or forever) with maps
	 * that hardly have any (or none) spaces.
	 */
	x = rand()%World.x;
	y = rand()%World.y;
    }
    if (grav) {
        vx = (rand()&7)-3;
        vy = (rand()&7)-3;
        if (pl) {
            vx += pl->vel.x;
            vy += pl->vel.y;
        } else {
	    vy -= Gravity * 12;
	}
    } else {
	vx = vy = 0.0;
    }

    item->color = RED;
    item->info = type;
    item->status = grav;
    item->id = -1;
    item->pos.x = x * BLOCK_SZ + BLOCK_SZ/2;
    item->pos.y = y * BLOCK_SZ + BLOCK_SZ/2;
    item->prevpos = item->pos;
    item->placed = 1;
    item->vel.x = vx;
    item->vel.y = vy;
    item->acc.x =
    item->acc.y = 0.0;
    item->mass = 10.0;
    item->life = 1500 + (rand()&511);

    switch (type) {
    case ITEM_ROCKET_PACK:
	item->type = OBJ_ROCKET_PACK;
	break;
    case ITEM_SENSOR_PACK:
	item->type = OBJ_SENSOR_PACK;
	break;
    case ITEM_ECM:
	item->type = OBJ_ECM;
	break;
    case ITEM_MINE_PACK:
	item->type = OBJ_MINE_PACK;
	break;
    case ITEM_TANK:
	item->type = OBJ_TANK;
	break;
    case ITEM_CLOAKING_DEVICE:
	item->type = OBJ_CLOAKING_DEVICE;
	break;
    case ITEM_ENERGY_PACK:
	item->type = OBJ_ENERGY_PACK;
	break;
    case ITEM_WIDEANGLE_SHOT:
	item->type = OBJ_WIDEANGLE_SHOT;
	break;
    case ITEM_BACK_SHOT:
	item->type = OBJ_BACK_SHOT;
	break;
    case ITEM_AFTERBURNER:
	item->type = OBJ_AFTERBURNER;
	break;
    case ITEM_TRANSPORTER:
	item->type = OBJ_TRANSPORTER;
	break;
    case ITEM_LASER:
	item->type = OBJ_LASER;
	break;
    default:
	item->type = OBJ_ROCKET_PACK;
	break;
    }

    World.items[type].num++;
}


void Throw_items(player *pl)
{
    int i;

    if (!ThrowItemOnKillRand) return;
    for (i=pl->extra_shots-initialWideangles; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_WIDEANGLE_SHOT, pl);
    for (i=pl->ecms-initialECMs; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_ECM, pl);
    for (i=pl->sensors-initialSensors; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_SENSOR_PACK, pl);
    for (i=pl->afterburners-initialAfterburners; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_AFTERBURNER, pl);
    for (i=pl->transporters-initialTransporters; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_TRANSPORTER, pl);
    for (i=pl->back_shots-initialRearshots; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_BACK_SHOT, pl);
    for (i=pl->missiles-initialMissiles; i>0; i-=4)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_ROCKET_PACK, pl);
    for (i=pl->cloaks-initialCloaks; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_CLOAKING_DEVICE, pl);
    for (i=pl->mines-initialMines; i>0; i-=2)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_MINE_PACK, pl);
    for (i=pl->lasers-initialLasers; i>0; i--)
        if ((rand()&127) < ThrowItemOnKillRand)
            Place_item(ITEM_LASER, pl);
}


void Place_mine(int ind)
{
    object *mine;
    player *pl = Players[ind];

    if (NumObjs >= MAX_TOTAL_SHOTS)
	return;
    sound_play_sensors(pl->pos.x, pl->pos.y, DROP_MINE_SOUND);
    mine = Obj[NumObjs++];
    mine->type = OBJ_MINE;
    mine->color = BLUE;
    mine->info = OBJ_MINE;
    mine->status = 0;
    mine->id = pl->id;
    mine->pos.x = pl->pos.x;
    mine->pos.y = pl->pos.y;
    mine->prevpos = mine->pos;
    mine->placed = 1;
    mine->vel.x=mine->vel.y = mine->acc.x=mine->acc.y = 0.0;
    mine->mass = MINE_MASS;
    mine->life = MINE_LIFETIME;
}


void Place_moving_mine(int ind, float vx, float vy)
{
    object *mine;
    player *pl = Players[ind];


    if (NumObjs >= MAX_TOTAL_SHOTS)
	return;
    sound_play_sensors(pl->pos.x, pl->pos.y, DROP_MOVING_MINE_SOUND);
    mine = Obj[NumObjs++];
    mine->type = OBJ_MINE;
    mine->color = BLUE;
    mine->info = OBJ_MINE;
    mine->status = GRAVITY;
    mine->id = pl->id;
    mine->pos.x = pl->pos.x;
    mine->pos.y = pl->pos.y;
    mine->prevpos = mine->pos;
    mine->placed = 1;
    mine->acc.x = mine->acc.y = 0.0;
    mine->vel.x = vx * MINE_SPEED_FACT;
    mine->vel.y = vy * MINE_SPEED_FACT;
    mine->mass = MINE_MASS;
    mine->life = MINE_LIFETIME;
}


void Cannon_fire(int ind)
{
    object *shot;
    int dir, speed;
    const int spread = (RES*0.3);
    const int spreadoffset = (spread/2);

    if (NumObjs >= MAX_TOTAL_SHOTS)
	return;
    shot = Obj[NumObjs++];
    dir = (rand()%(1+spread)) - spreadoffset - 1;	/* Tmp direction */
    speed = 9+(rand()%4);
    shot->color = WHITE;
    shot->id = -1;
    shot->pos.x = World.cannon[ind].pos.x * BLOCK_SZ+BLOCK_SZ/2;
    shot->pos.y = World.cannon[ind].pos.y * BLOCK_SZ+BLOCK_SZ/2;
    shot->prevpos = shot->pos;
    shot->placed = 1;
    shot->status = GRAVITY;
    shot->acc.x = shot->acc.y = 0;
    shot->mass = 0.4;
    shot->type = OBJ_CANNON_SHOT;
    shot->life = 25 + (rand()%20);

    sound_play_sensors(shot->pos.x, shot->pos.y, CANNON_FIRE_SOUND);

    switch (World.cannon[ind].dir) {
    case DIR_UP:
	shot->pos.y += BLOCK_SZ/6;
	dir += DIR_UP;
	break;
    case DIR_DOWN:
	shot->pos.y -= BLOCK_SZ/6;
	dir += DIR_DOWN;
	break;
    case DIR_RIGHT:
	shot->pos.x += BLOCK_SZ/6;
	dir += DIR_RIGHT;
	break;
    case DIR_LEFT:
	shot->pos.x -= BLOCK_SZ/6;
	dir += DIR_LEFT;
	break;
    }

    shot->dir	= MOD2(shot->dir, RES);
    shot->vel.x	= speed*tcos(dir);
    shot->vel.y	= speed*tsin(dir);
}


void Make_ball(int id, float x, float y, bool grav, int treasure)
{
  object *ball;
  
  ball = Obj[NumObjs];

  ball->length = BALL_STRING_LENGTH;
  ball->life = LONG_MAX;
  ball->mass = 50;
  ball->vel.x = 0;	  	/* make the ball stuck a little */
  ball->vel.y = 0;		/* longer to the ground */
  ball->pos.x = x;
  ball->pos.y = y;
  ball->id = ball->owner = id;	/* Who is the ball attached on */
  ball->type = OBJ_BALL;
  ball->color = WHITE;
  ball->count = -1;
  if (grav)
      ball->status = GRAVITY;
  else
      ball->status = 0;	
  ball->treasure = treasure;
  NumObjs++;
}


void Punish_team(int ind, int treasureNum)
{
    u_short team = World.treasures[treasureNum].team;

    player *pl = Players[ind];
    int i;

    static char msg[MSG_LEN];

    sprintf(msg, "%s's (%d) team has destroyed team %d treasure",
	    pl->name, pl->team, team);
    Set_message(msg);    

    for (i=0; i < NumPlayers; i++)
	if (Players[i]->team == team) 
	    Players[i]->life = 0;

    updateScores = true;
}

void Fire_shot(int ind, int type, int dir) /* Initializes a new shot */
{
    char msg[MSG_LEN];
    object *shot;
    player *pl;
    int NukeSize;

    pl = Players[ind];
    if (pl->shots >= pl->shot_max
	|| NumObjs >= MAX_TOTAL_SHOTS
	|| BIT(pl->used, OBJ_SHIELD))
	return;

    shot = Obj[NumObjs];
    switch (type) {

    case OBJ_SHOT:
	if (pl->fuel.sum < -ED_SHOT)
	    return;
	sound_play_sensors(pl->pos.x, pl->pos.y, FIRE_SHOT_SOUND);
	shot->life = pl->shot_life;
	shot->mass = pl->shot_mass;
	shot->max_speed = SPEED_LIMIT;
 	Add_fuel(&(pl->fuel), ED_SHOT);
	shot->vel.x = pl->vel.x + tcos(dir) * pl->shot_speed;
	shot->vel.y = pl->vel.y + tsin(dir) * pl->shot_speed;
	break;

    case OBJ_NUKE:
	if (pl->fuel.sum < -ED_SMART_SHOT || pl->missiles < NUKE_MIN_SMART) {
	    return;
	}
	NukeSize = pl->missiles;
	if (maxMissilesPerNuke > 0 && NukeSize > maxMissilesPerNuke) {
	    NukeSize = maxMissilesPerNuke;
	}
	shot->mass = MISSILE_MASS * NukeSize * NUKE_MASS_MULT;
	shot->life = MISSILE_LIFETIME;
	Add_fuel(&(pl->fuel),ED_SMART_SHOT);
	shot->vel.x = pl->vel.x + (tcos(dir) * pl->shot_speed);
	shot->vel.y = pl->vel.y + (tsin(dir) * pl->shot_speed);
	shot->info = 0;
	shot->color = RED;
	pl->missiles -= NukeSize;
	sprintf(msg, "%s has launched a %d megaton nuke!", pl->name, NukeSize);
	Set_message(msg);
	sound_play_all(NUKE_LAUNCH_SOUND);
	break;

    case OBJ_TORPEDO:
         
	if (pl->fuel.sum < -ED_SMART_SHOT || (pl->missiles <= 0))
	    return;
	sound_play_sensors(pl->pos.x, pl->pos.y, FIRE_TORPEDO_SOUND);
	shot->mass = MISSILE_MASS;
	shot->life = MISSILE_LIFETIME;
	Add_fuel(&(pl->fuel), ED_SMART_SHOT);
	shot->vel.x = pl->vel.x + (tcos(dir) * pl->shot_speed);
	shot->vel.y = pl->vel.y + (tsin(dir) * pl->shot_speed);
	shot->info = 0;
	pl->missiles--;
	break;
 
    case OBJ_SMART_SHOT:
    case OBJ_HEAT_SHOT:
	{
	    int lock;
	  
	    if (pl->fuel.sum < -ED_SMART_SHOT || (pl->missiles <= 0))
		return;
	  
	    if (type == OBJ_HEAT_SHOT) {
#ifndef HEAT_LOCK
		lock = -1;
#else  /* HEAT_LOCK */
		if (pl->lock.tagged == LOCK_NONE
		    || ((pl->lock.distance > pl->sensor_range)
			&& BIT(World.rules->mode, LIMITED_VISIBILITY)))
		    lock = -1;
		else
		    lock = pl->lock.pl_id;
#endif /* HEAT_LOCK */
		sound_play_sensors(pl->pos.x, pl->pos.y, FIRE_HEAT_SHOT_SOUND);
	    } else {
		if (pl->lock.tagged == LOCK_NONE
		    || ((pl->lock.distance > pl->sensor_range)
			&& BIT(World.rules->mode, LIMITED_VISIBILITY))
		    || !pl->visibility[GetInd[pl->lock.pl_id]].canSee)
		    return;
		sound_play_sensors(pl->pos.x, pl->pos.y, FIRE_SMART_SHOT_SOUND);
		lock = pl->lock.pl_id;
		if (lock == pl->id) {
		    errno = 0;
		    error("%s fires smart at himself", pl->name);
		}
	    }
	    shot->mass = MISSILE_MASS;
	    shot->life = MISSILE_LIFETIME;
	    shot->max_speed = SMART_SHOT_MAX_SPEED;
	    shot->count = 0;
	    shot->turnspeed = SMART_TURNSPEED;
	    shot->info = lock;
	    Add_fuel(&(pl->fuel), ED_SMART_SHOT);
	    pl->missiles--;
	    if (type == OBJ_HEAT_SHOT) {
		shot->max_speed = SMART_SHOT_MAX_SPEED * HEAT_SPEED_FACT;
		shot->turnspeed = SMART_TURNSPEED * HEAT_SPEED_FACT;
		shot->vel.x =
		    pl->vel.x + tcos(dir) * pl->shot_speed * HEAT_SPEED_FACT;
		shot->vel.y =
		    pl->vel.y + tsin(dir) * pl->shot_speed * HEAT_SPEED_FACT;
	    } else {
		shot->vel.x = pl->vel.x + (tcos(dir) * pl->shot_speed);
		shot->vel.y = pl->vel.y + (tsin(dir) * pl->shot_speed);
	    }
	    break;
	}

    default:
	return;
    }
    shot->type	= type;
    shot->id	= pl->id;
    shot->color	= pl->color;
    shot->pos.x = pl->pos.x + ships[dir].pts[0].x;
    shot->pos.y = pl->pos.y + ships[dir].pts[0].y;
    shot->prevpos = shot->pos;
    shot->placed = 1;
    shot->status= GRAVITY;
    shot->acc.x = shot->acc.y = 0;
    shot->dir	= dir;
	
    Recoil((object *)pl, shot);

    NumObjs++; pl->shots++;
}


void Fire_normal_shots(int ind)
{
    player		*pl = Players[ind];
    int			i;

    pl->shot_time = loops;
    Fire_shot(ind, OBJ_SHOT, pl->dir);
    for (i = 0; i < pl->extra_shots; i++) {
	Fire_shot(ind, OBJ_SHOT, MOD2(pl->dir + (1 + i) * SHOTS_ANGLE, RES));
	Fire_shot(ind, OBJ_SHOT, MOD2(pl->dir - (1 + i) * SHOTS_ANGLE, RES));
    }
    for (i = 0; i < pl->back_shots; i++) {
	Fire_shot(ind, OBJ_SHOT,
		  MOD2(pl->dir + RES/2
		       + (pl->back_shots - 1 - 2 * i) * SHOTS_ANGLE/2,
		       RES));
    }
}


/* Removes shot from array */
void Delete_shot(int ind)
{
    object *shot = Obj[ind];	    /* Used when swapping places */
    int addMine = 0, x, y;
    int addHeat = 0, xh, yh;
    int treasure;

    switch (shot->type) {
    case OBJ_MINE:
	Explode_object(shot->prevpos.x, shot->prevpos.y, 0, RES, 500);
	sound_play_sensors(shot->pos.x, shot->pos.y, MINE_EXPLOSION_SOUND);
    case OBJ_SPARK:
    case OBJ_CANNON_SHOT:
    case OBJ_CANNON_DEBRIS:
    case OBJ_DEBRIS:
	break;
    case OBJ_BALL:
	treasure = shot->treasure;
	if (shot->id != -1)
	    CLR_BIT(Players[GetInd[shot->id]]->have, OBJ_BALL);
	if (shot->count < 0) {
	    Explode_object(shot->prevpos.x, shot->prevpos.y, 0, RES, 300);
	    Make_ball(-1,
		      World.treasures[treasure].pos.x*BLOCK_SZ+(BLOCK_SZ/2),
		      World.treasures[treasure].pos.y*BLOCK_SZ+10, 
		      false, treasure);
	    World.treasures[treasure].have = true;
	}
	break;
	/* Shots related to a player. */

    case OBJ_NUKE:
	sound_play_all(NUKE_EXPLOSION_SOUND);
	Explode_object(shot->prevpos.x, shot->prevpos.y,0,RES,
		       (shot->mass/MISSILE_MASS*30*NUKE_EXPLOSION_MULT));
	if (shot->id != -1) {
	    Players[GetInd[shot->id]]->shots--;
	}
	break;

    case OBJ_HEAT_SHOT:
    case OBJ_TORPEDO:
    case OBJ_SMART_SHOT:
	Explode_object(shot->pos.x, shot->pos.y, 0, RES, 30);
    case OBJ_SHOT:
	if (shot->id != -1) {
	    Players[GetInd[shot->id]]->shots--;
	}
	break;

	/* Special items. */
    case OBJ_ROCKET_PACK:
	if(shot->life == 0 && shot->color != WHITE) {
	    shot->color = WHITE;
	    shot->life	= FPS * WARN_TIME;
	    return;
	}
	World.items[ITEM_ROCKET_PACK].num--;
	if (shot->life == 0) {
	    addHeat = 1;
	    xh = shot->pos.x;
	    yh = shot->pos.y;
	}
	break;
    case OBJ_AFTERBURNER:
	World.items[ITEM_AFTERBURNER].num--;
	break;
    case OBJ_SENSOR_PACK:
	World.items[ITEM_SENSOR_PACK].num--;
	break;
    case OBJ_ECM:
	World.items[ITEM_ECM].num--;
	break;
    case OBJ_TRANSPORTER:
	World.items[ITEM_TRANSPORTER].num--;
	break;
    case OBJ_LASER:
	World.items[ITEM_LASER].num--;
	break;
    case OBJ_CLOAKING_DEVICE:
	World.items[ITEM_CLOAKING_DEVICE].num--;
	break;
    case OBJ_ENERGY_PACK:
	World.items[ITEM_ENERGY_PACK].num--;
	break;
    case OBJ_WIDEANGLE_SHOT:
	World.items[ITEM_WIDEANGLE_SHOT].num--;
	break;
    case OBJ_BACK_SHOT:
	World.items[ITEM_BACK_SHOT].num--;
	break;
    case OBJ_MINE_PACK:
        if(!shot->life && shot->color != WHITE) {
	    shot->color = WHITE;
	    shot->life  = FPS * WARN_TIME;
	    return;
	}
	World.items[ITEM_MINE_PACK].num--;
	if (shot->life == 0) {
	    addMine = 1;
	    x = shot->pos.x;
	    y = shot->pos.y;
	}
	break;
    case OBJ_TANK:
	World.items[ITEM_TANK].num--;
	break;
    default:
	printf("Delete_shot(): Unkown shot type %d.\n", shot->type);
	break;
    }


    Obj[ind] = Obj[--NumObjs];
    Obj[NumObjs] = shot;

    if (addMine) {
	object *mine;

	if (NumObjs >= MAX_TOTAL_SHOTS)
	    return;
	
	mine = Obj[NumObjs++];
	mine->type = OBJ_MINE;
	mine->color = BLUE;
	mine->info = OBJ_MINE;
	mine->status = rand()&3 ? 0 : GRAVITY;
	mine->id = -1;
	mine->pos.x = x;
	mine->pos.y = y;
	mine->prevpos = mine->pos;
	mine->placed = 1;
	mine->vel.x= mine->vel.y = mine->acc.x = mine->acc.y = 0.0;
	mine->mass = MINE_MASS;
	mine->life = MINE_LIFETIME;
    }
    if (addHeat) {
	object *heat;

	if (NumObjs >= MAX_TOTAL_SHOTS)
	    return;
	
	heat = Obj[NumObjs++];
	heat->mass = MISSILE_MASS;
	heat->life = MISSILE_LIFETIME;
        heat->max_speed = SMART_SHOT_MAX_SPEED * HEAT_SPEED_FACT;
        heat->count = HEAT_WIDE_TIMEOUT+HEAT_WIDE_ERROR;
        heat->turnspeed = SMART_TURNSPEED * HEAT_SPEED_FACT;
	heat->info = -1;
		/*
		 * Set the shot so that its initial velocity will tend to
		 * be towards the center of the map, but add just a bit
		 * of randomness so that it doesn't get boring...
		 */
	heat->vel.x = ((float)(World.width / 2 - xh)
			 / (World.width / 2)
			 * SMART_SHOT_MAX_SPEED);
 	  /* + ((rand() >> 2) % (int)(SMART_SHOT_MAX_SPEED / 6)
	     - SMART_SHOT_MAX_SPEED / 12); */
	heat->vel.y = ((float)(World.height / 2 - yh)
			 / (World.height / 2)
			 * SMART_SHOT_MAX_SPEED);
	  /* + ((rand() >> 2) % (int)(SMART_SHOT_MAX_SPEED / 6)
	     - SMART_SHOT_MAX_SPEED / 12); */
	heat->type = OBJ_HEAT_SHOT;
	heat->id = -1;
	heat->color = WHITE;
	heat->pos.x = xh;
	heat->pos.y = yh;
        heat->dir = (int)findDir(heat->vel.x, heat->vel.y);
        heat->acc.x=heat->acc.y = 0;
    }
}


void do_transporter(player *pl)
{
    int             i;
    float          l,
                    closestLength = TRANSPORTER_DISTANCE;
    player         *closestPlayer = NULL,
                   *p;
    bool            done = false;
    char            msg[MSG_LEN],
                   *what;

    for (i = 0; i < NumPlayers; i++)
    {
	p = Players[i];

	if (p != pl
	    && BIT(p->status, PLAYING|PAUSE|GAME_OVER) == PLAYING
	    && p->robot_mode != RM_OBJECT
	    && (l = Wrap_length((pl->pos.x - p->pos.x), (pl->pos.y - p->pos.y)))
		< closestLength)
	{
	    closestLength = l;
	    closestPlayer = p;
	}
    }

    if (!(p = closestPlayer))
    {
	sound_play_sensors(pl->pos.x, pl->pos.y, TRANSPORTER_FAIL_SOUND);
	return;
    }

    sound_play_sensors(pl->pos.x, pl->pos.y, TRANSPORTER_SUCCESS_SOUND);
    pl->transInfo.pl_id = p->id;
    pl->transInfo.count = 5;

#define STEAL(item, msg)						      \
{									      \
    if (!p->item)							      \
	break;								      \
    p->item--;								      \
    pl->item++;								      \
    what = msg;								      \
    done = true;							      \
}

    while (!done)
	switch (1 << (rand() & 31)) {
	    case OBJ_AFTERBURNER:
		STEAL(afterburners, "an afterburner");
		SET_BIT(pl->have, OBJ_AFTERBURNER);
		if (pl->afterburners > MAX_AFTERBURNER)
		    pl->afterburners = MAX_AFTERBURNER;
		break;
	    case OBJ_ROCKET_PACK:
		STEAL(missiles, "some missiles");

		if (p->missiles < 3) {
		    pl->missiles += p->missiles;
		    p->missiles = 0;
		}
		else {
		    p->missiles -= 3;
		    pl->missiles += 3;
		}
		break;
	    case OBJ_CLOAKING_DEVICE:
		STEAL(cloaks, "a cloaking device");
		p->updateVisibility = pl->updateVisibility = 1;
		if (!p->cloaks)
		    CLR_BIT(p->used, OBJ_CLOAKING_DEVICE);
		SET_BIT(pl->have, OBJ_CLOAKING_DEVICE);
		break;
	    case OBJ_WIDEANGLE_SHOT:
		STEAL(extra_shots, "a wide");
		break;
	    case OBJ_BACK_SHOT:
		STEAL(back_shots, "a rear");
		break;
	    case OBJ_MINE_PACK:
		STEAL(mines, "a mine");
		break;
	    case OBJ_SENSOR_PACK:
		STEAL(sensors, "a sensor");
		p->updateVisibility = pl->updateVisibility = 1;
		break;
	    case OBJ_ECM:
		STEAL(ecms, "an ECM");
		break;
	    case OBJ_TRANSPORTER:
		STEAL(transporters, "a transporter");
		break;
	    case OBJ_LASER:
		STEAL(lasers, "a laser");
		if (pl->lasers > MAX_LASERS) {
		    pl->lasers = MAX_LASERS;
		}
		break;
	    case OBJ_TANK:
		{
		    int             no,c,
		                    t;

		    if (pl->fuel.num_tanks == MAX_TANKS || !p->fuel.num_tanks)
			break;
		    t = (rand() % p->fuel.num_tanks) + 1;

		    /* remove the tank from the victim */
		    p->fuel.sum -= p->fuel.tank[t];
		    p->fuel.max -= TANK_CAP(t);
		    for (i = t; i < p->fuel.num_tanks; i++)
			p->fuel.tank[i] = p->fuel.tank[i + 1];
		    p->emptymass -= TANK_MASS;
		    p->fuel.num_tanks -= 1;
		    if (p->fuel.current)
			p->fuel.current--;

		    /* add the tank to the thief */
		    c = pl->fuel.current;
		    no = ++(pl->fuel.num_tanks);
		    SET_BIT(pl->have, OBJ_TANK);
		    pl->fuel.current = no;
		    pl->fuel.max += TANK_CAP(no);
		    pl->fuel.tank[no] = 0;
		    pl->emptymass += TANK_MASS;
		    Add_fuel(&(pl->fuel), p->fuel.tank[t]);
		    pl->fuel.current = c;

		    what = "a tank";
		    done = true;
		    break;
		}
	    case OBJ_ENERGY_PACK:	/* used to steal fuel */
#define MIN_FUEL_STEAL	10
#define MAX_FUEL_STEAL  50
		{
		    long            amount;
		    float          percent;

		    percent = ((rand() % (MAX_FUEL_STEAL - MIN_FUEL_STEAL) +
				MIN_FUEL_STEAL) / 100.0);
		    amount = (long)(p->fuel.sum * percent);
		    sprintf(msg, "%s stole %d units (%d%%) of fuel from %s",
			    pl->name, amount >> FUEL_SCALE_BITS,
			    (int) (percent * 100.0 + 0.5), p->name);
		    Add_fuel(&(pl->fuel), amount);
		    Add_fuel(&(p->fuel), -amount);
		    Set_message(msg);
		    return;
		}
	}

    sprintf(msg, "%s stole %s from %s.", pl->name, what, p->name);
    Set_message(msg);
}

#define CONFUSED_UPDATE_GRANULARITY	10
#define CONFUSED_TIME			3
#define ECM_DAMAGE_COUNT		30

void do_ecm(player *pl)
{
    object *shot;
    int i, j;
    player *p;

    sound_play_sensors(pl->pos.x, pl->pos.y, ECM_SOUND);
    for (i = 0; i < NumObjs; i++) {
	shot = Obj[i];

	if (shot->type == OBJ_SMART_SHOT &&
	    Wrap_length(pl->pos.x - shot->pos.x,
		   pl->pos.y - shot->pos.y) <= ECM_DISTANCE * ECM_MIS_FACT) {

	    SET_BIT(shot->status, CONFUSED);
	    shot->count = CONFUSED_TIME;

	    if ((pl->lock.distance <= pl->sensor_range
		 || !BIT(World.rules->mode, LIMITED_VISIBILITY))
		&& pl->visibility[GetInd[pl->lock.pl_id]].canSee)
		shot->new_info = pl->lock.pl_id;
	    else
		shot->new_info = Players[rand() % NumPlayers]->id;
	}
    }

    for (i = 0; i < NumPlayers; i++) {
	p = Players[i];

	if (p != pl && BIT(p->status, PLAYING|PAUSE|GAME_OVER) == PLAYING &&
	    Wrap_length(pl->pos.x - p->pos.x,
			pl->pos.y - p->pos.y) <= ECM_DISTANCE) {

	    int c = rand() % ECM_DAMAGE_COUNT;
	    p->forceVisible += c;

	    /* ECM destroys lasers */
	    if (p->lasers > 0) {
		p->lasers = ((ECM_DAMAGE_COUNT - 1 - c) * p->lasers)
		    / (ECM_DAMAGE_COUNT - 1);
	    }

	    if (p->robot_mode == RM_NOT_ROBOT || p->robot_mode == RM_OBJECT) {
		p->damaged += c;
	    } else
		if (pl->lock.tagged == LOCK_PLAYER
		    && (pl->lock.distance < pl->sensor_range
			|| !BIT(World.rules->mode, LIMITED_VISIBILITY))
		    && pl->visibility[GetInd[pl->lock.pl_id]].canSee
		    && pl->lock.pl_id != p->id) {
		    /*
		     * This is, except for the message, the same
		     * as a robot war declaration, but not currently
		     * treated as such.  The clients should be
		     * informed about the changed robot war situation.
		     */

		    static char msg[MSG_LEN];

		    sprintf(msg, "%s has programmed %s to seek %s.",
			    pl->name, p->name,
			    Players[GetInd[pl->lock.pl_id]]->name);
		    p->robot_lock_id = pl->lock.pl_id;
		    p->robot_lock = LOCK_PLAYER;
		    for (j = 0; j < NumPlayers; j++) {
			if (Players[j]->conn != NOT_CONNECTED) {
			    if (Players[j]->version < 0x3030) {
				Send_message(Players[j]->conn, msg);
				Send_war(Players[j]->conn, p->id,
					 p->robot_lock_id);
			    } else {
				Send_seek(Players[j]->conn, pl->id,
					  p->id, p->robot_lock_id);
			    }
			}
		    }
		}
	}
    }
}


void Move_ball(int ind)
{
    object		*ball = Obj[ind];
    player		*pl = Players[ GetInd[ball->id] ];
    vector		F;
    const float	k = 10.0, l0 = BALL_STRING_LENGTH, a = 0.01,
    			l = Wrap_length(pl->pos.x - ball->pos.x, 
				        pl->pos.y - ball->pos.y),
    			c = k * (1.0 - l0 / l)
			    - a * ABS(ball->length - l) * (ball->length - l);

    F.x = WRAP_DX(pl->pos.x - ball->pos.x) * c;
    F.y = WRAP_DY(pl->pos.y - ball->pos.y) * c;

    pl->vel.x -= F.x/pl->mass;
    pl->vel.y -= F.y/pl->mass;

    ball->vel.x += F.x/ball->mass;
    ball->vel.y += F.y/ball->mass;

    ball->length = l;
}


void Move_smart_shot(int ind)
{
    object *shot = Obj[ind];
    player *pl;
    int	   angle, theta;
    float min = 0.0;
    float acc;
    float x_dif = 0.0;
    float y_dif = 0.0;
    float shot_speed;


    if (shot->type == OBJ_TORPEDO) {
        if (shot->info++ < TORPEDO_SPEED_TIME) {
            shot->vel.x += TORPEDO_ACC*tcos(shot->dir);
            shot->vel.y += TORPEDO_ACC*tsin(shot->dir);
        }
        return;
    }
    
   if (shot->type==OBJ_NUKE) {
        if (shot->info++<NUKE_SPEED_TIME) {
            shot->vel.x += NUKE_ACC*tcos(shot->dir);
            shot->vel.y += NUKE_ACC*tsin(shot->dir);
        }
        return;
    }

    acc = SMART_SHOT_ACC;

    if (shot->type == OBJ_HEAT_SHOT) {
        acc = SMART_SHOT_ACC * HEAT_SPEED_FACT;
        if (shot->info >= 0) {
            /* Get player and set min to distance */
            pl = Players[ GetInd[shot->info] ];
            min = Wrap_length(pl->pos.x-shot->pos.x, pl->pos.y-shot->pos.y);
        } else {
	    /* No player. Number of moves so that new target is searched */
            pl = 0;
            shot->count = HEAT_WIDE_TIMEOUT + HEAT_WIDE_ERROR;
        }
        if (pl && BIT(pl->status, THRUSTING)) {
            /*
	     * Target is thrusting,
	     * set number to moves to correct error value
	     */
            if (min < HEAT_CLOSE_RANGE) {
                shot->count = HEAT_CLOSE_ERROR;
            } else if (min < HEAT_MID_RANGE) {
                shot->count = HEAT_MID_ERROR;
            } else {
                shot->count = HEAT_WIDE_ERROR;
            }
        } else {
            shot->count++;
            /* Look for new target */
            if ((min < HEAT_CLOSE_RANGE
		 && shot->count > HEAT_CLOSE_TIMEOUT + HEAT_CLOSE_ERROR)
                || (min < HEAT_MID_RANGE
                    && shot->count > HEAT_MID_TIMEOUT + HEAT_MID_ERROR)
                || shot->count > HEAT_WIDE_TIMEOUT + HEAT_WIDE_ERROR) {   
                float l;
                int i;

                min = HEAT_RANGE * (shot->count/HEAT_CLOSE_TIMEOUT);
                for (i=0; i<NumPlayers; i++) {
                    player *p = Players[i];
            
                    if (!BIT(p->status, THRUSTING))
                        continue;

                    l = Wrap_length(p->pos.x - shot->pos.x,
				    p->pos.y - shot->pos.y);
                    /*
		     * After burners can be detected easier;
		     * so scale the length:
		     */
                    l *= MAX_AFTERBURNER + 1 - p->afterburners;
                    l /= MAX_AFTERBURNER + 1;
                    if (BIT(p->have, OBJ_AFTERBURNER))
			l *= 16 - p->afterburners;
                    if (l < min) {
                        shot->info = Players[i]->id;
                        min = l;
                        shot->count =
			    l < HEAT_CLOSE_RANGE ?
				HEAT_CLOSE_ERROR : l < HEAT_MID_RANGE ?
				    HEAT_MID_ERROR : HEAT_WIDE_ERROR;
                        pl = p;
		    }
		}
	    }
	}
        if (shot->info < 0)
	    return;
        /*
	 * Heat seekers cannot fly exactly, if target is far away or thrust
	 * isn't active.  So simulate the error:
	 */
        x_dif = (rand()&3) * shot->count;
        y_dif = (rand()&3) * shot->count;

    } else {

	if (BIT(shot->status, CONFUSED)
	    && (!(loops % CONFUSED_UPDATE_GRANULARITY)
		|| shot->count == CONFUSED_TIME)) {

	    if (shot->count) {
	        shot->info = Players[rand() % NumPlayers]->id;
	        shot->count--;
	    } else {
	        CLR_BIT(shot->status, CONFUSED);
	        if (rand() % 100 < 80)
		    shot->info = shot->new_info;
	    }
        }
	pl = Players[GetInd[shot->info]];
    }
    
    /*
     * Use a little look ahead to fly more exact
     */
    shot_speed = (shot->velocity) ? shot->velocity : 1;
    min = Wrap_length(pl->pos.x - shot->pos.x, pl->pos.y - shot->pos.y)
	/ shot_speed;
    x_dif += pl->vel.x * min;
    y_dif += pl->vel.y * min;

    theta = Wrap_findDir(pl->pos.x + x_dif - shot->pos.x,
		         pl->pos.y + y_dif - shot->pos.y);

    {
	float x, y, vx, vy;
	int i, xi, yi, j, freemax, k, foundw;
	static struct {
	    int dx, dy;
	} sur[8] = {
	    {1,0}, {1,1}, {0,1}, {-1,1}, {-1,0}, {-1,-1}, {0,-1}, {1,-1}
	};

#define BLOCK_PARTS 2
	vx = shot->vel.x;
	vy = shot->vel.y;
	x = shot_speed / (BLOCK_SZ*BLOCK_PARTS);
	vx /= x; vy /= x;
	x = shot->pos.x; y = shot->pos.y;
	foundw = 0;

	for (i = SMART_SHOT_LOOK_AH; i > 0 && foundw == 0; i--) {
	    xi = (x += vx) / BLOCK_SZ;
	    yi = (y += vy) / BLOCK_SZ;
	    if (BIT(World.rules->mode, WRAP_PLAY)) {
		if (xi < 0) xi += World.x;
		else if (xi >= World.x) xi -= World.x;
		if (yi < 0) yi += World.y;
		else if (yi >= World.y) yi -= World.y;
	    }
	    if (xi < 0 || xi >= World.x || yi < 0 || yi >= World.y)
		break;

	    switch(World.block[xi][yi]) {
	    case FUEL:
	    case FILLED:
	    case FILLED_NO_DRAW:
	    case REC_LU:
	    case REC_RU:
	    case REC_LD:
	    case REC_RD:
	    case CANNON:
		if(Wrap_length(pl->pos.x-shot->pos.x, pl->pos.y-shot->pos.y)
		   > (SMART_SHOT_LOOK_AH-i) * (BLOCK_SZ/BLOCK_PARTS)) {

		    if(shot->velocity>SMART_SHOT_MIN_SPEED)
			shot->velocity -= acc * (SMART_SHOT_DECFACT+1);
		}
		foundw = 1;
	    }
	}

	i = ((int)(shot->dir * 8 / RES)&7) + 8;
	xi = shot->pos.x / BLOCK_SZ;
	yi = shot->pos.y /BLOCK_SZ;

	for(j=2, angle=-1, freemax=0; j>=-2; --j) {
	    int si, xt, yt;

	    for(si=1, k=0; si >= -1; --si) {
		xt = xi + sur[(i+j+si)&7].dx;
		yt = yi + sur[(i+j+si)&7].dy;

		if(xt >= 0 && xt < World.x && yt >= 0 && yt < World.y)
		    switch (World.block[xt][yt]) {
		    case FUEL:
		    case FILLED:
		    case FILLED_NO_DRAW:
		    case REC_LU:
		    case REC_RU:
		    case REC_LD:
		    case REC_RD:
		    case CANNON:
			if(!si)
			    k = -32;
			break;
		    default:
			++k;
			break;
		    }
	    }
	    if (k > freemax || k == freemax
		&& ((j == -1 && (rand()&1)) || j == 0 || j == 1)) {
		freemax = k > 2 ? 2 : k;
		angle = i + j;
	    }

	    if (k == 3 && !j) {
		angle = -1;
		break;
	    }
	}

	if (angle >= 0) {
	    i = angle&7;
	    theta = Wrap_findDir((yi + sur[i].dy) * BLOCK_SZ
			    - (shot->pos.y + 2 * shot->vel.y),
			    (xi + sur[i].dx) * BLOCK_SZ
			    - (shot->pos.x - 2 * shot->vel.x));
#ifdef SHOT_EXTRA_SLOWDOWN
	    if (!foundw && Wrap_length(pl->pos.x-shot->pos.x,
		    pl->pos.y-shot->pos.y) > (SHOT_LOOK_AH-i) * BLOCK_SZ) {
		if (shot->velocity
		    > (SMART_SHOT_MIN_SPEED + SMART_SHOT_MAX_SPEED)/2)
		    shot->velocity -= SMART_SHOT_DECC+SMART_SHOT_ACC;
	    }
#endif
	}
    } 
    angle = theta;

    if (angle < 0)
	angle += RES;
    angle %= RES;

    if (angle < shot->dir)
	angle += RES;
    angle = angle - shot->dir - RES/2;

    if (angle < 0)
	shot->dir += -angle < shot->turnspeed ? -angle : shot->turnspeed;
    else
	shot->dir -= angle < shot->turnspeed ? angle : shot->turnspeed;

    shot->dir = MOD2(shot->dir, RES); /* NOTE!!!! */

    if (shot->velocity < shot->max_speed)
	shot->velocity += acc;

    /*  shot->velocity = MIN(shot->velocity, shot->max_speed);  */

    shot->vel.x = tcos(shot->dir) * shot->velocity;
    shot->vel.y = tsin(shot->dir) * shot->velocity;
}


/*
 * Add fuel to fighter's tanks.
 * Maybe use more than one of tank to store the fuel.
 */
void Add_fuel(pl_fuel_t *ft, long fuel)
{
    if (ft->sum + fuel > ft->max)
        fuel = ft->max - ft->sum;
    else if (ft->sum + fuel < 0)
        fuel = -ft->sum;
    ft->sum += fuel;
    ft->tank[ft->current] += fuel;
}


/*
 * Move fuel from add-on tanks to main tank,
 * handle over and underflow of tanks.
 */
void Update_tanks(pl_fuel_t *ft)
{   
    if (ft->num_tanks) {
        int  t, check;
        long low_level;
        long fuel;
        long *f;

        /* Set low_level to minimum fuel in each tank */
        low_level = ft->sum / (ft->num_tanks + 1) - 1;
        if (low_level < 0)
	    low_level = 0;
        if (TANK_REFILL_LIMIT < low_level)
	    low_level = TANK_REFILL_LIMIT;
    
        t = ft->num_tanks;
        check = MAX_TANKS<<2;
        fuel = 0;
        f = ft->tank + t;

        while (t>=0 && check--) {
            long m = TANK_CAP(t);

            /* Add the previous over/underflow and do a new cut */
            *f += fuel;
            if (*f > m) {
                fuel = *f - m;
                *f = m;
            } else if (*f < 0) {
                fuel = *f;
                *f = 0;
            } else
                fuel = 0;
            
            /* If there is no over/underflow, let the fuel run to main-tank */
            if (!fuel) {
                if (t
                    && t != ft->current
                    && *f >= low_level + REFUEL_RATE
                    && *(f-1) <= TANK_CAP(t-1) - REFUEL_RATE) {

                    *f -= REFUEL_RATE;
                    fuel = REFUEL_RATE;
                } else if (t && *f < low_level) {
                    *f += REFUEL_RATE;
                    fuel = -REFUEL_RATE;
                }
            }
            if (fuel && t == 0) {
               t = ft->num_tanks;
               f = ft->tank + t;
            } else {
                t--;
                f--;
            }
        }
        if (!check) {
            error("fuel problem");
            fuel = ft->sum;
            ft->sum =
            ft->max = 0;
            t = 0;
            while (t <= ft->num_tanks) {
                if (fuel) {
                    if (fuel>TANK_CAP(t)) {
                        ft->tank[t] = TANK_CAP(t);
                        fuel -= TANK_CAP(t);
                    } else {
                        ft->tank[t] = fuel;
                        fuel = 0;
                    }
                    ft->sum += ft->tank[t];
                } else
		    ft->tank[t] = 0;
                ft->max += TANK_CAP(t);
                t++;
            }
        }
    } else
        ft->tank[0] = ft->sum;
}


/*
 * Use current tank as dummy target for heat seeking missles.
 */
void Tank_handle_detach(player *pl)
{
    player *dummy;
    int i, ct;
    
    /* Return, if no more players or no tanks */
    if (pl->fuel.num_tanks == 0 || NumPseudoPlayers == MAX_PSEUDO_PLAYERS)
	return;

    sound_play_sensors(pl->pos.x, pl->pos.y, TANK_DETACH_SOUND);

    /* If current tank is main, use another one */
    if ((ct=pl->fuel.current) == 0)
	ct = pl->fuel.num_tanks;

    Update_tanks(&(pl->fuel));
    /* Fork the current player */
    dummy               = Players[NumPlayers];
    *dummy              = *pl;
    strcat(dummy->name, "'s tank");
    dummy->robot_mode   = RM_OBJECT;
    dummy->mychar       = 'T';
    dummy->score	-= 500;		/* It'll hurt to be hit by this */
    updateScores	= true;
    dummy->count	= -1;		/* Don't commit suicide :) */
    dummy->conn		= NOT_CONNECTED;
    dummy->audio	= NULL;

    /* Fuel is the one from choosen tank */
    dummy->fuel.sum     =
    dummy->fuel.tank[0] = dummy->fuel.tank[ct];
    dummy->fuel.max     = TANK_CAP(ct);
    dummy->fuel.current = 0;
    dummy->fuel.num_tanks = 0;
    
    /* No after burner */
    dummy->afterburners = 0;

    /* No lasers */
    dummy->lasers = 0;
    dummy->num_pulses = 0;
    dummy->max_pulses = 0;
    dummy->pulses = NULL;

    /* Mass is only tank + fuel */
    dummy->mass = (dummy->emptymass = ShipMass) + FUEL_MASS(dummy->fuel.sum);
    dummy->have = DEF_HAVE;
    dummy->used = DEF_USED;
    dummy->power *= TANK_THRUST_FACT;


    /* Remember whose tank this is */
    dummy->robot_lock_id = pl->id;
    dummy->robot_lock = LOCK_NONE;

    /* Handling the id's and the Tables */
    dummy->id = Id;
    GetInd[Id] = NumPlayers;
    NumPlayers++;
    NumPseudoPlayers++;
    Id++;
    updateScores = true;

    /* The tank uses shield and thrust */
    dummy->status = (DEF_BITS & ~KILL_BITS) | PLAYING | GRAVITY | THRUSTING;
    dummy->have = DEF_HAVE;
    dummy->used = (DEF_USED & ~USED_KILL & pl->have) | OBJ_SHIELD;
    if (playerShielding == 0) {
	dummy->shield_time = 30 * FPS;
	dummy->have |= OBJ_SHIELD;
    }
    
    /* Maybe heat-seekers to retarget? */
    for (i=0; i < NumObjs; i++)
        if (Obj[i]->type == OBJ_HEAT_SHOT
	    && Obj[i]->info > 0
	    && Players[ GetInd[Obj[i]->info] ] == pl)
            Obj[i]->info = NumPlayers - 1;
    
    /* Remove tank, fuel and mass from myself */
    pl->fuel.sum -= pl->fuel.tank[ct];
    pl->fuel.max -= TANK_CAP(ct);

    for (i=ct; i < pl->fuel.num_tanks; i++)
        pl->fuel.tank[i] = pl->fuel.tank[i+1];

    pl->emptymass -= TANK_MASS;
    pl->fuel.num_tanks--;
    if (pl->fuel.current)
	pl->fuel.current--;

    for (i = 0; i < NumPlayers - 1; i++) {
	if (Players[i]->conn != NOT_CONNECTED) {
	    Send_player(Players[i]->conn, dummy->id, dummy->team,
			dummy->mychar, dummy->name,
			dummy->realname, dummy->hostname);
	    Send_score(Players[i]->conn, dummy->id, dummy->score, dummy->life,
		       dummy->mychar);
	}
    }
}


/****************************
 * Functions for explosions.
 */

void Explode_object(float x, float y,
		    int real_dir, int spread, int intensity)
{
    object *debris;
    int num_debris, speed, dir;
    register int i;
    const int spreadoffset = (spread/2);

    sound_play_sensors(x, y, OBJECT_EXPLOSION_SOUND);
    num_debris = (intensity/2) + (rand()%(1+intensity/2));
    for (i=0; i<num_debris && NumObjs<MAX_TOTAL_SHOTS; i++, NumObjs++) {
	debris = Obj[NumObjs];
	speed = DEBRIS_SPEED(intensity);
	dir = real_dir + (rand()%(1+spread)) - spreadoffset - 1;
	debris->color = RED;
	debris->id = -1;
	debris->pos.x = x;
	debris->pos.y = y;
	debris->prevpos = debris->pos;
	debris->placed = 1;
	debris->vel.x = (tcos(dir) * speed);
	debris->vel.y = (tsin(dir) * speed);
	debris->status = GRAVITY;
	debris->acc.x = debris->acc.y = 0;
	debris->dir = dir;
	debris->mass = DEBRIS_MASS;
	debris->type = OBJ_CANNON_DEBRIS;
	debris->life = DEBRIS_LIFE(intensity);
	if (debris->life > World.hypotenuse / speed) {
	    debris->life = World.hypotenuse / speed;
	}
    }
}


/* Explode a fighter */
void Explode(int ind)
{
    player *pl;
    object *debris;
    int i, dir, num_debris, speed;

    pl = Players[ind];
    sound_play_sensors(pl->pos.x, pl->pos.y, PLAYER_EXPLOSION_SOUND);
    num_debris = 1+(pl->fuel.sum/(8.0*FUEL_SCALE_FACT))
                 +(rand()%((int)(1+pl->mass*4.0)));
    /*  shot_mass = pl->mass / num_debris;	Not used! */
    for (i=0; i<num_debris && NumObjs<MAX_TOTAL_SHOTS; i++, NumObjs++) {
	debris = Obj[NumObjs];
	dir = rand()%RES;
	speed = PL_DEBRIS_SPEED(pl->mass);
	debris->color=RED;
	debris->id = pl->id;
	debris->pos.x = pl->pos.x;
	debris->pos.y = pl->pos.y;
	debris->prevpos = debris->pos;
	debris->placed = 1;
	debris->vel.x = pl->vel.x + (tcos(dir) * speed);
	debris->vel.y = pl->vel.y + (tsin(dir) * speed);
	debris->status = GRAVITY;
	debris->acc.x = debris->acc.y = 0;
	debris->dir = dir;
	debris->mass = PL_DEBRIS_MASS;
	debris->type = OBJ_DEBRIS;
	debris->life = PL_DEBRIS_LIFE(pl->mass);
    }
}

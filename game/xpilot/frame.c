/* $Id: frame.c,v 1.1 1994/02/23 14:40:05 jkh Exp $
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
#include <sys/types.h>
#ifndef  VMS
#include <sys/param.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define SERVER
#include "global.h"
#include "version.h"
#include "bit.h"
#include "netserver.h"
#include "saudio.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: frame.c,v 1.1 1994/02/23 14:40:05 jkh Exp $";
#endif


/*
 * Structure for calculating if a pixel is visible by a player.
 */
typedef struct {
    position	world;			/* Lower left hand corner is this */
					/* world coordinate */
    position	realWorld;		/* If the player is on the edge of
					   the screen, these are the world
					   coordinates before adjustment... */
    int		wrappedWorld;		/* Nonzero if we're near the edge
					   of the world (realWorld != world) */
} pixel_visibility_t;

/*
 * Structure with player position info measured in blocks instead of pixels.
 * Used for map state info updating.
 */
typedef struct {
    ipos		world;
    ipos		realWorld;
    int			wrappedWorld;
} block_visibility_t;

typedef struct {
    unsigned char	x, y;
} debris_t;


long			loops = 0;

static pixel_visibility_t pv;
static int		view_width,
			view_height,
			horizontal_blocks,
			vertical_blocks,
			debris_x_areas,
			debris_y_areas,
			debris_areas,
			debris_colors,
			spark_rand;
static debris_t		*debris_ptr[DEBRIS_TYPES];
static int		debris_num[DEBRIS_TYPES],
			debris_max[DEBRIS_TYPES];


static int inview(float x, float y)
{
    if (x > pv.world.x
	&& x < pv.world.x + view_width
	&& y > pv.world.y
	&& y < pv.world.y + view_height) {
	return 1;
    }
    if (!pv.wrappedWorld) {
	return 0;
    }
    if ((pv.wrappedWorld & 1) && x > view_width) {
	x -= World.width;
    }
    if ((pv.wrappedWorld & 2) && y > view_height) {
	y -= World.height;
    }
    return (x > pv.realWorld.x
	&& x < pv.realWorld.x + view_width
	&& y > pv.realWorld.y
	&& y < pv.realWorld.y + view_height);
}

static int block_inview(block_visibility_t *pl, int x, int y)
{
    if (x >= pl->world.x
	&& x <= pl->world.x + horizontal_blocks
	&& y >= pl->world.y
	&& y <= pl->world.y + vertical_blocks) {
	return 1;
    }
    if (!pl->wrappedWorld) {
	return 0;
    }
    if ((pl->wrappedWorld & 1) && x > horizontal_blocks) {
	x -= World.x;
    }
    if ((pl->wrappedWorld & 2) && y > vertical_blocks) {
	y -= World.y;
    }
    return (x >= pl->realWorld.x
	&& x <= pl->realWorld.x + horizontal_blocks
	&& y >= pl->realWorld.y
	&& y <= pl->realWorld.y + vertical_blocks);
}

static void debris_store(float xf, float yf, int color)
{
#define ptr		(debris_ptr[i])
#define num		(debris_num[i])
#define max		(debris_max[i])

    int			i,
			xd,
			yd;

    if (xf < 0) {
	xf += World.width;
    }
    if (yf < 0) {
	yf += World.height;
    }
    xd = (int) (xf + 0.5);
    yd = (int) (yf + 0.5);
    if ((unsigned) xd >= view_width || (unsigned) yd >= view_height) {
	/*
	 * There's some rounding error or so somewhere.
	 * Should be possible to resolve it.
	 */
	return;
    }

    i = color * debris_areas
	+ (((yd >> 8) % debris_y_areas) * debris_x_areas)
	+ ((xd >> 8) % debris_x_areas);

    if (num >= 255) {
	return;
    }
    if (num >= max) {
	if (num == 0) {
	    ptr = (debris_t *) malloc((max = 16) * sizeof(*ptr));
	} else {
	    ptr = (debris_t *) realloc(ptr, (max += max) * sizeof(*ptr));
	}
	if (ptr == 0) {
	    error("No memory for debris");
	    num = 0;
	    return;
	}
    }
    ptr[num].x = (unsigned char) xd;
    ptr[num].y = (unsigned char) yd;
    num++;

#undef ptr
#undef num
#undef max
}

static void debris_end(int conn)
{
    int			i;

    for (i = 0; i < DEBRIS_TYPES; i++) {
	if (debris_num[i] != 0) {
	    Send_debris(conn, i,
			(unsigned char *) debris_ptr[i],
			debris_num[i]);
	    debris_num[i] = 0;
	}
    }
}

static int Frame_status(int conn, int ind)
{
    player		*pl = Players[ind];
    int			n,
			lock_ind,
			lock_id = -1,
			lock_dist = 0,
			lock_dir = 0;

    if (pl->lock.tagged == LOCK_PLAYER) {
	lock_id = pl->lock.pl_id;
	lock_ind = GetInd[lock_id];
	/*
	 * Don't send direction and distance if:
	 * 1) we have limited visibility and the player is out of range.
	 * 2) the player is invisible and he's not in our team.
	 * 3) he's not actively playing.
	 * 4) we have blind mode and he's not on the visible screen.
	 * 5) his distance is zero.
	 */
	if ((!BIT(World.rules->mode, LIMITED_VISIBILITY)
	    || pl->lock.distance <= pl->sensor_range)
#ifndef SHOW_CLOAKERS_RANGE
	    && (pl->visibility[lock_ind].canSee
		|| TEAM(ind, lock_ind))
#endif
	    && BIT(Players[lock_ind]->status, PLAYING|GAME_OVER) == PLAYING
	    && (playersOnRadar
	    || inview(Players[lock_ind]->pos.x, Players[lock_ind]->pos.y))
	    && pl->lock.distance != 0) {
	    lock_dir = Wrap_findDir(Players[lock_ind]->pos.x - pl->pos.x,
				    Players[lock_ind]->pos.y - pl->pos.y);
	    lock_dist = pl->lock.distance;
	}
    }

    n = Send_self(conn,
	(int) (pl->pos.x + 0.5),
	(int) (pl->pos.y + 0.5),
	(int) pl->vel.x,
	(int) pl->vel.y,
	pl->dir,
	pl->power,
	pl->turnspeed,
	pl->turnresistance,
	lock_id,
	lock_dist,
	lock_dir,
	pl->check,
	pl->cloaks,
	pl->sensors,
	pl->mines,
	pl->missiles,
	pl->ecms,
	pl->transporters,
	pl->extra_shots,
	pl->back_shots,
	pl->afterburners,
	pl->lasers,
	pl->fuel.num_tanks,
	pl->fuel.current,
	pl->fuel.sum,
	pl->fuel.max,
	Players[GetInd[Get_player_id(conn)]]->status);
    if (n <= 0) {
	return 0;
    }

    if (BIT(pl->status, SELF_DESTRUCT) && pl->count > 0) {
	Send_destruct(conn, pl->count);
    }
    if (ShutdownServer != -1) {
	Send_shutdown(conn, ShutdownServer, ShutdownDelay);
    }
    return 1;
}

static void Frame_map(int conn, int ind)
{
    player		*pl = Players[ind];
    int			i,
			x,
			y,
			conn_bit = (1 << conn);
    block_visibility_t	bv;

    x = pl->pos.x / BLOCK_SZ;
    y = pl->pos.y / BLOCK_SZ;
    bv.world.x = x - (horizontal_blocks >> 1);
    bv.world.y = y - (vertical_blocks >> 1);
    bv.wrappedWorld = 0;
    if (BIT(World.rules->mode, WRAP_PLAY)) {
	bv.realWorld = bv.world;
	if (bv.world.x < 0) {
	    bv.wrappedWorld |= 1;
	    bv.world.x += World.x;
	} else if (bv.world.x + horizontal_blocks > World.x) {
	    bv.realWorld.x -= World.x;
	    bv.wrappedWorld |= 1;
	}
	if (bv.world.y < 0) {
	    bv.wrappedWorld |= 2;
	    bv.world.y += World.y;
	} else if (bv.world.y + vertical_blocks > World.y) {
	    bv.realWorld.y -= World.y;
	    bv.wrappedWorld |= 2;
	}
    }

    for (i = 0; i < World.NumFuels; i++) {
	if (BIT(World.fuel[i].conn_mask, conn_bit) == 0) {
	    if (block_inview(&bv,
			     World.fuel[i].pos.x / BLOCK_SZ,
			     World.fuel[i].pos.y / BLOCK_SZ)) {
		Send_fuel(conn, i, (int) World.fuel[i].fuel);
	    }
	}
    }

    for (i = 0; i < World.NumCannons; i++) {
	if (block_inview(&bv,
			 World.cannon[i].pos.x,
			 World.cannon[i].pos.y)) {
	    if (BIT(World.cannon[i].conn_mask, conn_bit) == 0) {
		Send_cannon(conn, i, World.cannon[i].dead_time);
	    }
	    if (World.cannon[i].dead_time <= 0) {
		World.cannon[i].active = true;
	    }
	}
    }

    for (i = 0; i < World.NumTargets; i++) {
	if (BIT(World.targets[i].conn_mask, conn_bit) == 0) {
	    if (block_inview(&bv,
			     World.targets[i].pos.x,
			     World.targets[i].pos.y)) {
		Send_target(conn, i, World.targets[i].dead_time,
			    World.targets[i].damage);
	    }
	}
    }
}

static void Frame_shots(int conn, int ind)
{
    player		*pl = Players[ind];
    int			i, color, x, y, fuzz = 0;
    object		*shot;

    for (i = 0; i < NumObjs; i++) {
	shot = Obj[i];
	if (!inview(shot->pos.x, shot->pos.y)) {
	    continue;
	}
	x = (int) (shot->pos.x + 0.5);
	y = (int) (shot->pos.y + 0.5);
	if ((color = shot->color) == BLACK) {
	    printf("black %d,%d\n", shot->type, shot->id);
	    color = WHITE;
	}
	switch (shot->type) {
	case OBJ_SPARK:
	case OBJ_DEBRIS:
	case OBJ_CANNON_DEBRIS:
	    if ((fuzz >>= 7) < 0x40) {
		fuzz = rand();
	    }
	    if ((fuzz & 0x7F) >= spark_rand) {
		/*
		 * produce a sparkling effect by not displaying
		 * particles every frame.
		 */
		break;
	    }
	    if (debris_colors >= 3) {
		if (debris_colors > 4) {
		    if (color == BLUE) {
			color = (shot->life >> 1);
		    } else {
			color = (shot->life >> 2);
		    }
		} else {
		    if (color == BLUE) {
			color = (shot->life >> 2);
		    } else {
			color = (shot->life >> 3);
		    }

		}
		if (color >= debris_colors) {
		    color = debris_colors - 1;
		}
	    }

	    debris_store(shot->pos.x - pv.world.x,
			 shot->pos.y - pv.world.y,
                         color);
	    break;
	case OBJ_SHOT:
	    if (shot->id != -1
		&& shot->id != pl->id
		&& TEAM(ind, GetInd[shot->id])) {
		color = BLUE;
	    }
	    /*FALLTHROUGH*/
	case OBJ_CANNON_SHOT:
	    Send_shot(conn, x, y, color);
	    break;
	case OBJ_TORPEDO:
	case OBJ_NUKE:
	case OBJ_SMART_SHOT:
	case OBJ_HEAT_SHOT:
	    Send_smart(conn, x, y, shot->dir);
	    break;
	case OBJ_BALL:
	    Send_ball(conn, x, y, shot->id);
	    break;
	case OBJ_MINE:
	    Send_mine(conn, x, y);
	    break;
	case OBJ_WIDEANGLE_SHOT:
	    Send_item(conn, x, y, ITEM_WIDEANGLE_SHOT);
	    break;
	case OBJ_AFTERBURNER:
	    Send_item(conn, x, y, ITEM_AFTERBURNER);
	    break;
	case OBJ_BACK_SHOT:
	    Send_item(conn, x, y, ITEM_BACK_SHOT);
	    break;
	case OBJ_ROCKET_PACK:
	    Send_item(conn, x, y, ITEM_ROCKET_PACK);
	    break;
	case OBJ_ENERGY_PACK:
	    Send_item(conn, x, y, ITEM_ENERGY_PACK);
	    break;
	case OBJ_MINE_PACK:
	    Send_item(conn, x, y, ITEM_MINE_PACK);
	    break;
	case OBJ_SENSOR_PACK:
	    Send_item(conn, x, y, ITEM_SENSOR_PACK);
	    break;
	case OBJ_ECM:
	    Send_item(conn, x, y, ITEM_ECM);
	    break;
	case OBJ_TANK:
	    Send_item(conn, x, y, ITEM_TANK);
	    break;
	case OBJ_CLOAKING_DEVICE:
	    Send_item(conn, x, y, ITEM_CLOAKING_DEVICE);
	    break;
	case OBJ_TRANSPORTER:
	    Send_item(conn, x, y, ITEM_TRANSPORTER);
	    break;
	case OBJ_LASER:
	    if (pl->version >= 0x3041) {
		Send_item(conn, x, y, ITEM_LASER);
	    }
	    break;
	default:
	    error("Frame_shots: Shot type %d not defined.", shot->type);
	    break;
	}
    }
}

static void Frame_ships(int conn, int ind)
{
    player		*pl = Players[ind],
			*pl_i;
    pulse_t		*pulse;
    int			i, j, color, dir;
    float		x, y;

    for (i = 0; i < NumPlayers; i++) {
	pl_i = Players[i];
	if (!BIT(pl_i->status, PLAYING|PAUSE)) {
	    continue;
	}
	if (BIT(pl_i->status, GAME_OVER)) {
	    continue;
	}
	for (j = 0; j < pl_i->num_pulses; j++) {
	    pulse = &pl_i->pulses[j];
	    if (pulse->len <= 0) {
		continue;
	    }
	    x = pulse->pos.x;
	    y = pulse->pos.y;
	    if (BIT (World.rules->mode, WRAP_PLAY)) {
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
	    if (inview(x, y)) {
		dir = pulse->dir;
	    } else {
		x += tcos(pulse->dir) * pulse->len;
		y += tsin(pulse->dir) * pulse->len;
		if (BIT (World.rules->mode, WRAP_PLAY)) {
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
		if (inview(x, y)) {
		    dir = MOD2(pulse->dir + RES/2, RES);
		}
		else {
		    continue;
		}
	    }
	    if (TEAM(ind, i) && ind != i) {
		color = BLUE;
	    } else {
		color = RED;
	    }
	    Send_laser(conn,
		color,
		(int) (x + 0.5),
		(int) (y + 0.5),
		pulse->len,
		dir);
	}
	if (!inview(pl_i->pos.x, pl_i->pos.y)) {
	    continue;
	}
	if (BIT(pl_i->status, PAUSE)) {
	    Send_paused(conn,
		(int) (pl_i->pos.x + 0.5),
		(int) (pl_i->pos.y + 0.5),
		pl_i->count);
	    continue;
	}
	if (pl_i->ecmInfo.size > 0) {
	    Send_ecm(conn,
		(int) (pl_i->ecmInfo.pos.x + 0.5),
		(int) (pl_i->ecmInfo.pos.y + 0.5),
		pl_i->ecmInfo.size);
	}
	if (pl_i->transInfo.count) {
	    player *pl = Players[GetInd[pl_i->transInfo.pl_id]];
	    Send_trans(conn, (int) (pl->pos.x + 0.5), (int) (pl->pos.y + 0.5),
		       (int) (pl_i->pos.x + 0.5), (int) (pl_i->pos.y + 0.5));
	}

	/* Don't transmit information is fighter is invisible */
	if (pl->visibility[i].canSee
	    || i == ind
	    || TEAM(i, ind)) {
	    /*
	     * Transmit ship information
	     */
	    Send_ship(conn,
		(int) (pl_i->pos.x + 0.5),
		(int) (pl_i->pos.y + 0.5),
		pl_i->id,
		pl_i->dir,
		BIT(pl_i->used, OBJ_SHIELD) != 0,
		BIT(pl_i->used, OBJ_CLOAKING_DEVICE) != 0
	    );
	}
	if (BIT(pl_i->used, OBJ_REFUEL)) {
	    if (inview(World.fuel[pl_i->fs].pos.x,
		       World.fuel[pl_i->fs].pos.y)) {
		Send_refuel(conn,
		    (int) (World.fuel[pl_i->fs].pos.x + 0.5),
		    (int) (World.fuel[pl_i->fs].pos.y + 0.5),
		    (int) (pl_i->pos.x + 0.5),
		    (int) (pl_i->pos.y + 0.5));
	    }
	}
	if (BIT(pl_i->used, OBJ_REPAIR)) {
	    float x = World.targets[pl_i->repair_target].pos.x
		* BLOCK_SZ+BLOCK_SZ/2;
	    float y = World.targets[pl_i->repair_target].pos.y
		* BLOCK_SZ+BLOCK_SZ/2;
	    if (inview(x, y)) {
		/* same packet as refuel */
		Send_refuel(conn,
		    (int) (pl_i->pos.x + 0.5), (int) (pl_i->pos.y + 0.5),
		    (int) (x + 0.5), (int) (y + 0.5));
	    }
	}
	if (pl_i->ball != NULL
	    && inview(pl_i->ball->pos.x, pl_i->ball->pos.y)) {
	    Send_connector(conn,
		(int) (pl_i->ball->pos.x + 0.5),
		(int) (pl_i->ball->pos.y + 0.5),
		(int) (pl_i->pos.x + 0.5),
		(int) (pl_i->pos.y + 0.5));  
	}
    }
}

static void Frame_radar(int conn, int ind)
{
    int			i;
    player		*pl = Players[ind];
    object		*shot;
    float		x, y;
    
#ifndef NO_SMART_MIS_RADAR
    if (missilesOnRadar) {
	if (loops & 1) {
	    for (i = 0; i < NumObjs; i++) {
		shot = Obj[i];
		if (BIT(shot->type, (OBJ_SMART_SHOT|OBJ_TORPEDO|OBJ_NUKE
				    |OBJ_HEAT_SHOT))) {
		    x = shot->pos.x;
		    y = shot->pos.y;
		    if (Wrap_length(pl->pos.x - x,
				    pl->pos.y - y) <= pl->sensor_range) {
			Send_radar(conn, (int) (x + 0.5), (int) (y + 0.5));
		    }
		}
	    }
	}
    }
#endif
    if (playersOnRadar) {
	for (i = 0; i < NumPlayers; i++) {
	    if (i == ind
		|| BIT(Players[i]->status, PLAYING|GAME_OVER) != PLAYING
		|| !pl->visibility[i].canSee) {
		continue;
	    }
	    x = Players[i]->pos.x;
	    y = Players[i]->pos.y;
	    if (BIT(World.rules->mode, LIMITED_VISIBILITY)
		&& Wrap_length(pl->pos.x - x,
			       pl->pos.y - y) > pl->sensor_range) {
		continue;
	    }
	    if (BIT(pl->used, OBJ_COMPASS)
		&& pl->lock.tagged == LOCK_PLAYER
		&& GetInd[pl->lock.pl_id] == i
		&& loops % 5 >= 3) {
		continue;
	    }
	    Send_radar(conn, (int) (x + 0.5), (int) (y + 0.5));
	}
    }
}

static void Frame_parameters(int conn, int ind)
{
    player		*pl = Players[ind];

    Get_display_parameters(conn, &view_width, &view_height,
			   &debris_colors, &spark_rand);
    debris_x_areas = (view_width + 255) >> 8;
    debris_y_areas = (view_height + 255) >> 8;
    debris_areas = debris_x_areas * debris_y_areas;
    horizontal_blocks = (view_width + (BLOCK_SZ - 1)) / BLOCK_SZ;
    vertical_blocks = (view_height + (BLOCK_SZ - 1)) / BLOCK_SZ;

    pv.world.x = pl->pos.x - view_width / 2;	/* Scroll */
    pv.world.y = pl->pos.y - view_height / 2;
    pv.wrappedWorld = 0;
    if (BIT (World.rules->mode, WRAP_PLAY)) {
	pv.realWorld = pv.world;
	if (pv.world.x < 0) {
	    pv.wrappedWorld |= 1;
	    pv.world.x += World.width;
	} else if (pv.world.x + view_width >= World.width) {
	    pv.realWorld.x -= World.width;
	    pv.wrappedWorld |= 1;
	}

	if (pv.world.y < 0) {
	    pv.wrappedWorld |= 2;
	    pv.world.y += World.height;
	} else if (pv.world.y + view_height >= World.height) {
	    pv.realWorld.y -= World.height;
	    pv.wrappedWorld |= 2;
	}
    }
}

void Frame_update(void)
{
    int			i,
			conn,
			ind;
    player		*pl;
    time_t		newTimeLeft = 0;
    static time_t	oldTimeLeft;
    static bool		game_over_called = false;
    
    if (++loops >= LONG_MAX)	/* Used for misc. timing purposes */
	loops = 0;

    if (gameDuration > 0.0
	&& game_over_called == false
	&& oldTimeLeft != (newTimeLeft = gameOverTime - time(NULL))) {
	/*
	 * Do this once a second.
	 */
	if (newTimeLeft <= 0) {
	    Game_Over();
	    ShutdownServer = 30 * FPS;	/* Shutdown in 30 seconds */
	    game_over_called = true;
	}
    }

    for (i = 0; i < NumPlayers; i++) {
	pl = Players[i];
	conn = pl->conn;
	if (conn == NOT_CONNECTED) {
	    continue;
	}
	if (BIT(pl->status, PAUSE) && (loops & 0x01) == 0) {
	    /*
	     * Reduce the frame rate for paused players
	     * to reduce network load.
	     */
	    continue;
	}
	if (Send_start_of_frame(conn) == -1) {
	    continue;
	}
	if (newTimeLeft != oldTimeLeft) {
	    Send_time_left(conn, newTimeLeft);
	}
	/*
	 * If status is GAME_OVER, the user may look through the other
	 * players 'eyes'.  This is done by using two indexes, one
	 * determining which data should be used (ind, set below) and
	 * one determining which connection to send it to (conn).
	 */
	if (BIT(pl->status, GAME_OVER|PLAYING) == (GAME_OVER|PLAYING)
	    && (pl->lock.tagged == LOCK_PLAYER)) {
	    ind = GetInd[pl->lock.pl_id];
	    Send_eyes(conn, pl->lock.pl_id);
	} else {
	    ind = i;
	}
	if (Players[ind]->damaged > 0) {
	    Send_damaged(conn, Players[ind]->damaged);
	    Players[ind]->damaged--;
	} else {
	    Frame_parameters(conn, ind);
	    if (Frame_status(conn, ind) <= 0) {
		continue;
	    }
	    Frame_map(conn, ind);
	    Frame_shots(conn, ind);
	    Frame_ships(conn, ind);
	    Frame_radar(conn, ind);
	    debris_end(conn);
	}
	sound_play_queued(Players[ind]);
	Send_end_of_frame(conn);
    }
    oldTimeLeft = newTimeLeft;
}

void Set_message(char *message)
{
    player		*pl;
    int			i;
    char		*msg,
			tmp[MSG_LEN];

    if ((i = strlen(message)) >= MSG_LEN) {
#ifndef SILENT
	errno = 0;
	error("Max message len exceed (%d,%s)", i, message);
#endif
	strncpy(tmp, message, MSG_LEN - 1);
	tmp[MSG_LEN - 1] = '\0';
	msg = tmp;
    } else {
	msg = message;
    }
    for (i = 0; i < NumPlayers; i++) {
	pl = Players[i];
	if (pl->conn != NOT_CONNECTED) {
	    Send_message(pl->conn, msg);
	}
    }
}

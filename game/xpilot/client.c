/* $Id: client.c,v 1.1 1994/02/23 14:40:03 jkh Exp $
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

#include <X11/Xproto.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>

#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#if defined(__hpux) || defined(VMS)
#include <time.h>
#else
#include <sys/time.h>
#endif

#include "version.h"
#include "client.h"
#include "setup.h"
#include "error.h"
#include "rules.h"
#include "bit.h"
#include "netclient.h"
#include "paint.h"
#include "xinit.h"

#define MAX_CHECKPOINT	26

int			scoresChanged = 0;
int			RadarHeight = 0;

ipos	pos;
ipos	vel;
ipos	world;
ipos	realWorld;
short	wrappedWorld;
short	heading;
short	nextCheckPoint;
short	numCloaks;
short	numSensors;
short	numMines;
short	numRockets;
short	numEcms;
short 	numTransporters;
short	numFrontShots;
short	numBackShots;
short	numAfterburners;
short	numLasers;

short	lock_id;		/* Id of player locked onto */
short	lock_dir;		/* Direction of lock */
short	lock_dist;		/* Distance to player locked onto */

short	selfVisible;		/* Are we alive and playing? */
short	damaged;		/* Damaged by ECM */
short	destruct;		/* If self destructing */
short	shutdown_delay;
short	shutdown_count;

int	map_point_distance;	/* spacing of navigation points */
int	map_point_size;		/* size of navigation points */
int	spark_size;		/* size of debris and spark */
long	control_count;		/* Display control for how long? */

long	fuelSum;			/* Sum of fuel in all tanks */
long	fuelMax;			/* How much fuel can you take? */
short	fuelCurrent;			/* Number of currently used tank */
short	numTanks;			/* Number of tanks */
long	fuelCount;			/* Display fuel for how long? */
int	fuelLevel1;			/* Fuel critical level */
int	fuelLevel2;			/* Fuel warning level */
int	fuelLevel3;			/* Fuel notify level */

float	power;			/* Force of thrust */
float	power_s;		/* Saved power fiks */
float	turnspeed;		/* How fast player acc-turns */
float	turnspeed_s;		/* Saved turnspeed */
float	turnresistance;		/* How much is lost in % */
float	turnresistance_s;	/* Saved (see above) */
float	spark_prob;		/* Sparkling effect user configurable */
int	charsPerSecond;		/* Message output speed (configurable) */

float	hud_move_fact;		/* scale the hud-movement (speed) */
float	ptr_move_fact;		/* scale the speed pointer length */
long	instruments;		/* Instruments on screen (bitmask) */
int	packet_size;		/* Current frame update packet size */
int	packet_loss;		/* lost packets per second */
int	packet_drop;		/* dropped packets per second */
char	*packet_measure;	/* packet measurement in a second */
long	packet_loop;		/* start of measurement */

bool	showRealName = false;	/* Show realname instead of nick name */
char	name[MAX_CHARS];	/* Nick-name of player */
char	realname[MAX_CHARS];	/* Real name of player */
char	servername[MAX_CHARS];	/* Name of server connecting to */
unsigned	version;	/* Version of the server */
int	toggle_shield;		/* Are shields toggled by a press? */
int	shields;		/* When shields are considered up */

#ifdef SOUND
char 	sounds[MAX_CHARS];	/* audio mappings */
char 	audioServer[MAX_CHARS];	/* audio server */
int 	maxVolume;		/* maximum volume (in percent) */
#endif /* SOUND */

static other_t		*Others = 0;
static int		num_others = 0,
			max_others = 0;

static fuelstation_t	*fuels = 0;
static int		num_fuels = 0;

static homebase_t	*bases = 0;
static int		num_bases = 0;

static cannontime_t	*cannons = 0;
static int		num_cannons = 0;

static target_t		*targets = 0;
static int		num_targets = 0;

static checkpoint_t	checks[MAX_CHECKPOINT];

score_object_t		score_objects[MAX_SCORE_OBJECTS];
int			score_object = 0;


static fuelstation_t *Fuelstation_by_pos(int x, int y)
{
    int			i, l, h, pos;

    l = 0;
    h = num_fuels - 1;
    pos = x * Setup->y + y;
    while (l < h) {
	i = (l + h) >> 1;
	if (pos > fuels[i].pos) {
	    l = i + 1;
	} else {
	    h = i;
	}
    }
    if (l == h && pos == fuels[l].pos) {
	return &fuels[l];
    }
    error("No fuelstation at (%d,%d)", x, y);
    return NULL;
}

int Fuel_by_pos(int x, int y)
{
    fuelstation_t	*fuelp;

    if ((fuelp = Fuelstation_by_pos(x, y)) == NULL) {
	return 0;
    }
    return fuelp->fuel;
}

int Target_alive(int x, int y, int *damage)
{
    int 		i, l, h, pos;

    l = 0;
    h = num_targets - 1;
    pos = x * Setup->y + y;
    while (l < h) {
	i = (l + h) >> 1;
	if (pos > targets[i].pos) {
	    l = i + 1;
	} else {
	    h = i;
	}
    }
    if (l == h && pos == targets[l].pos) {
	*damage = targets[l].damage;
	return targets[l].dead_time;
    }
    error("No targets at (%d,%d)", x, y);
    return -1;
}

int Handle_fuel(int ind, int fuel)
{
    if (ind < 0 || ind >= num_fuels) {
	errno = 0;
	error("Bad fuelstation index (%d)", ind);
	return -1;
    }
    fuels[ind].fuel = fuel;
    return 0;
}

static cannontime_t *Cannon_by_pos(int x, int y)
{
    int			i, l, h, pos;

    l = 0;
    h = num_cannons - 1;
    pos = x * Setup->y + y;
    while (l < h) {
	i = (l + h) >> 1;
	if (pos > cannons[i].pos) {
	    l = i + 1;
	} else {
	    h = i;
	}
    }
    if (l == h && pos == cannons[l].pos) {
	return &cannons[l];
    }
    error("No cannon at (%d,%d)", x, y);
    return NULL;
}

int Cannon_dead_time_by_pos(int x, int y, int *dot)
{
    cannontime_t	*cannonp;

    if ((cannonp = Cannon_by_pos(x, y)) == NULL) {
	return -1;
    }
    *dot = cannonp->dot;
    return cannonp->dead_time;
}

int Handle_cannon(int ind, int dead_time)
{
    if (ind < 0 || ind >= num_cannons) {
	errno = 0;
	error("Bad cannon index (%d)", ind);
	return 0;
    }
    cannons[ind].dead_time = dead_time;
    return 0;
}

int Handle_target(int num, int dead_time, int damage)
{
    if (num < 0 || num >= num_targets) {
	errno = 0;
	error("Bad target index (%d)", num);
	return 0;
    }
    if (dead_time == 0
	&& (damage < 1
	|| damage > TARGET_DAMAGE)) {
	printf ("target %d, dead %d, damage %d\n", num, dead_time, damage);
    }
    targets[num].dead_time = dead_time;
    targets[num].damage = damage;
    return 0;
}

static homebase_t *Homebase_by_pos(int x, int y)
{
    int			i, l, h, pos;

    l = 0;
    h = num_bases - 1;
    pos = x * Setup->y + y;
    while (l < h) {
	i = (l + h) >> 1;
	if (pos > bases[i].pos) {
	    l = i + 1;
	} else {
	    h = i;
	}
    }
    if (l == h && pos == bases[l].pos) {
	return &bases[l];
    }
    error("No homebase at (%d,%d)", x, y);
    return NULL;
}

int Base_info_by_pos(int x, int y, int *idp, int *teamp)
{
    homebase_t	*basep;

    if ((basep = Homebase_by_pos(x, y)) == NULL) {
	return -1;
    }
    *idp = basep->id;
    *teamp = basep->team;
    return 0;
}

int Handle_base(int id, int ind)
{
    int		i;

    if (ind < 0 || ind >= num_bases) {
	errno = 0;
	error("Bad homebase index (%d)", ind);
	return -1;
    }
    for (i = 0; i < num_bases; i++) {
	if (bases[i].id == id) {
	    bases[i].id = -1;
	}
    }
    bases[ind].id = id;

    return 0;
}

int Check_pos_by_index(int ind, int *xp, int *yp)
{
    if (ind < 0 || ind >= MAX_CHECKPOINT) {
	errno = 0;
	error("Bad check index");
	*xp = 0;
	*yp = 0;
	return -1;
    }
    *xp = checks[ind].pos / Setup->y;
    *yp = checks[ind].pos % Setup->y;
    return 0;
}

int Check_index_by_pos(int x, int y)
{
    int			i, pos;

    pos = x * Setup->y + y;
    for (i = 0; i < MAX_CHECKPOINT; i++) {
	if (pos == checks[i].pos) {
	    return i;
	}
    }
    errno = 0;
    error("Can't find checkpoint (%d,%d)", x, y);
    return 0;
}

#if 0
/*
 * Sets as many blocks as possible to FILLED_NO_DRAW.
 * You won't notice any difference. :)
 * (BG: try speedups + if fuel can be part of the optimizing types)
 */
static void Map_optimize(void)
{
    int		x, y;
    byte	*line, *n_line, *p_line;
    byte	down[256], up[256], left[256], right[256];

    memset(down, 0, sizeof down);
    memset(up, 0, sizeof up);
    memset(left, 0, sizeof left);
    memset(right, 0, sizeof right);
    down[SETUP_REC_LD] = 1;
    down[SETUP_REC_RD] = 1;
    down[SETUP_FILLED] = 1;
    down[SETUP_FUEL] = 1;
    up[SETUP_REC_LU] = 1;
    up[SETUP_REC_RU] = 1;
    up[SETUP_FILLED] = 1;
    up[SETUP_FUEL] = 1;
    left[SETUP_REC_RU] = 1;
    left[SETUP_REC_RD] = 1;
    left[SETUP_FILLED] = 1;
    left[SETUP_FUEL] = 1;
    right[SETUP_REC_LU] = 1;
    right[SETUP_REC_LD] = 1;
    right[SETUP_FILLED] = 1;
    right[SETUP_FUEL] = 1;

    line = (byte*)Setup->map_data;
    n_line = (byte*)(Setup->map_data + Setup->y);

    for (x = 1; x < Setup->x - 1; x++) {
	p_line = line;
	line = n_line;
	n_line += Setup->y;

	for (y = 1; y < Setup->y - 1; y++) {
	    if (line[y] == SETUP_FILLED) {
		if (down[line[y+1]] == 0) {
                    y++;
		    continue;
                }
		if (up[line[y-1]] == 0)
		    continue;
		if (left[p_line[y]] == 0)
		    continue;
		if (right[n_line[y]] == 0)
		    continue;
		line[y++] = SETUP_FILLED_NO_DRAW;
	    }
	}
    }
}
#endif

/*
 * Optimise the drawing of all blue space dots by converting
 * certain map objects into a specialised form of their type.
 */
void Map_dots(void)
{
    int			i,
			x,
			y,
			start;

    /*
     * Restore the map to unoptimised form.
     */
    for (i = Setup->x * Setup->y; i-- > 0; ) {
	if (Setup->map_data[i] == SETUP_SPACE_DOT) {
	    Setup->map_data[i] = SETUP_SPACE;
	}
    }

    /*
     * Optimise.
     */
    if (map_point_size > 0) {
	if (BIT(Setup->mode, WRAP_PLAY)) {
	    for (x = 0; x < Setup->x; x++) {
		if (Setup->map_data[x * Setup->y] == SETUP_SPACE) {
		    Setup->map_data[x * Setup->y] = SETUP_SPACE_DOT;
		}
	    }
	    for (y = 0; y < Setup->y; y++) {
		if (Setup->map_data[y] == SETUP_SPACE) {
		    Setup->map_data[y] = SETUP_SPACE_DOT;
		}
	    }
	    start = map_point_distance;
	} else {
	    start = 0;
	}
	if (map_point_distance > 0) {
	    for (x = start; x < Setup->x; x += map_point_distance) {
		for (y = start; y < Setup->y; y += map_point_distance) {
		    if (Setup->map_data[x * Setup->y + y] == SETUP_SPACE) {
			Setup->map_data[x * Setup->y + y] = SETUP_SPACE_DOT;
		    }
		}
	    }
	}
	for (i = 0; i < num_cannons; i++) {
	    x = cannons[i].pos / Setup->y;
	    y = cannons[i].pos % Setup->y;
	    if ((x == 0 || y == 0) && BIT(Setup->mode, WRAP_PLAY)) {
		cannons[i].dot = 1;
	    }
	    else if (map_point_distance > 0
		&& x % map_point_distance == 0
		&& y % map_point_distance == 0) {
		cannons[i].dot = 1;
	    } else {
		cannons[i].dot = 0;
	    }
	}
    }
}

/*
 * Optimise the drawing of all blue map objects by converting
 * their map type to a bitmask with bits for each blue segment.
 */
void Map_blue(void)
{
    int			i,
			x,
			y,
			type,
			newtype;
    unsigned char	blue[256];

    /*
     * Restore an optimised map to its original unoptimised state.
     */
    for (x = i = 0; x < Setup->x; x++) {
	for (y = 0; y < Setup->y; y++, i++) {
	    type = Setup->map_data[i];
	    if ((type & BLUE_BIT) == 0) {
		if (type == SETUP_FILLED_NO_DRAW) {
		    Setup->map_data[i] = SETUP_FILLED;
		}
	    }
	    else if ((type & BLUE_FUEL) == BLUE_FUEL) {
		Setup->map_data[i] = SETUP_FUEL;
	    }
	    else if (type & BLUE_OPEN) {
		if (type & BLUE_BELOW) {
		    Setup->map_data[i] = SETUP_REC_RD;
		} else {
		    Setup->map_data[i] = SETUP_REC_LU;
		}
	    }
	    else if (type & BLUE_CLOSED) {
		if (type & BLUE_BELOW) {
		    Setup->map_data[i] = SETUP_REC_LD;
		} else {
		    Setup->map_data[i] = SETUP_REC_RU;
		}
	    } else {
		Setup->map_data[i] = SETUP_FILLED;
	    }
	}
    }

    /*
     * Optimise the map for blue.
     */
    memset(blue, 0, sizeof blue);
    blue[SETUP_FILLED] = BLUE_LEFT | BLUE_UP | BLUE_RIGHT | BLUE_DOWN;
    blue[SETUP_FILLED_NO_DRAW] = blue[SETUP_FILLED];
    blue[SETUP_FUEL] = blue[SETUP_FILLED];
    blue[SETUP_REC_RU] = BLUE_RIGHT | BLUE_UP;
    blue[SETUP_REC_RD] = BLUE_RIGHT | BLUE_DOWN;
    blue[SETUP_REC_LU] = BLUE_LEFT | BLUE_UP;
    blue[SETUP_REC_LD] = BLUE_LEFT | BLUE_DOWN;
    blue[BLUE_BIT|BLUE_OPEN] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_LEFT] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_UP] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_LEFT|BLUE_UP] =
	blue[SETUP_REC_LU];
    blue[BLUE_BIT|BLUE_OPEN|BLUE_BELOW] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_BELOW|BLUE_RIGHT] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_BELOW|BLUE_DOWN] =
    blue[BLUE_BIT|BLUE_OPEN|BLUE_BELOW|BLUE_RIGHT|BLUE_DOWN] =
	blue[SETUP_REC_RD];
    blue[BLUE_BIT|BLUE_CLOSED] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_RIGHT] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_UP] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_RIGHT|BLUE_UP] =
	blue[SETUP_REC_RU];
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_BELOW] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_BELOW|BLUE_LEFT] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_BELOW|BLUE_DOWN] =
    blue[BLUE_BIT|BLUE_CLOSED|BLUE_BELOW|BLUE_LEFT|BLUE_DOWN] =
	blue[SETUP_REC_LD];
    for (i = BLUE_BIT; i < sizeof blue; i++) {
	if ((i & BLUE_FUEL) == BLUE_FUEL
	    || (i & (BLUE_OPEN|BLUE_CLOSED)) == 0) {
	    blue[i] = blue[SETUP_FILLED];
	}
    }
    for (x = i = 0; x < Setup->x; x++) {
	for (y = 0; y < Setup->y; y++, i++) {
	    type = Setup->map_data[i];
	    newtype = 0;
	    switch (type) {
	    case SETUP_FILLED:
	    case SETUP_FILLED_NO_DRAW:
	    case SETUP_FUEL:
		newtype = BLUE_BIT;
		if (type == SETUP_FUEL) {
		    newtype |= BLUE_FUEL;
		}
		if ((x == 0)
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[(Setup->x - 1) * Setup->y + y]]
			    & BLUE_RIGHT))
		    : !(blue[Setup->map_data[(x - 1) * Setup->y + y]]
			& BLUE_RIGHT))
		    newtype |= BLUE_LEFT;
		if ((y == 0)
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[x * Setup->y + Setup->y - 1]]
			    & BLUE_UP))
		    : !(blue[Setup->map_data[x * Setup->y + (y - 1)]]
			& BLUE_UP))
		    newtype |= BLUE_DOWN;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((x == Setup->x - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[y]]
				& BLUE_LEFT))
			: !(blue[Setup->map_data[(x + 1) * Setup->y + y]]
			    & BLUE_LEFT)))
		    newtype |= BLUE_RIGHT;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((y == Setup->y - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[x * Setup->y]]
				& BLUE_DOWN))
			: !(blue[Setup->map_data[x * Setup->y + (y + 1)]]
			    & BLUE_DOWN)))
		    newtype |= BLUE_UP;
		break;
		
	    case SETUP_REC_LU:
		newtype = BLUE_BIT | BLUE_OPEN;
		if (x == 0
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[(Setup->x - 1) * Setup->y + y]]
			    & BLUE_RIGHT))
		    : !(blue[Setup->map_data[(x - 1) * Setup->y + y]]
			& BLUE_RIGHT))
		    newtype |= BLUE_LEFT;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((y == Setup->y - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[x * Setup->y]]
				& BLUE_DOWN))
			: !(blue[Setup->map_data[x * Setup->y + (y + 1)]]
			    & BLUE_DOWN)))
		    newtype |= BLUE_UP;
		break;
		
	    case SETUP_REC_RU:
		newtype = BLUE_BIT | BLUE_CLOSED;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((x == Setup->x - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[y]]
				& BLUE_LEFT))
			: !(blue[Setup->map_data[(x + 1) * Setup->y + y]]
			    & BLUE_LEFT)))
		    newtype |= BLUE_RIGHT;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((y == Setup->y - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[x * Setup->y]]
				& BLUE_DOWN))
			: !(blue[Setup->map_data[x * Setup->y + (y + 1)]]
			    & BLUE_DOWN)))
		    newtype |= BLUE_UP;
		break;
		
	    case SETUP_REC_LD:
		newtype = BLUE_BIT | BLUE_BELOW | BLUE_CLOSED;
		if ((x == 0)
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[(Setup->x - 1) * Setup->y + y]]
			    & BLUE_RIGHT))
		    : !(blue[Setup->map_data[(x - 1) * Setup->y + y]]
			& BLUE_RIGHT))
		    newtype |= BLUE_LEFT;
		if ((y == 0)
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[x * Setup->y + Setup->y - 1]]
			    & BLUE_UP))
		    : !(blue[Setup->map_data[x * Setup->y + (y - 1)]]
			& BLUE_UP))
		    newtype |= BLUE_DOWN;
		break;
		
	    case SETUP_REC_RD:
		newtype = BLUE_BIT | BLUE_BELOW | BLUE_OPEN;
		if (!BIT(instruments, SHOW_OUTLINE_WORLD)
		    || ((x == Setup->x - 1)
			? (!BIT(Setup->mode, WRAP_PLAY)
			   || !(blue[Setup->map_data[y]]
				& BLUE_LEFT))
			: !(blue[Setup->map_data[(x + 1) * Setup->y + y]]
			    & BLUE_LEFT)))
		    newtype |= BLUE_RIGHT;
		if ((y == 0)
		    ? (!BIT(Setup->mode, WRAP_PLAY) ||
			!(blue[Setup->map_data[x * Setup->y + Setup->y - 1]]
			    & BLUE_UP))
		    : !(blue[Setup->map_data[x * Setup->y + (y - 1)]]
			& BLUE_UP))
		    newtype |= BLUE_DOWN;
		break;

	    default:
		continue;
	    }
	    if (newtype != 0) {
		if (newtype == BLUE_BIT) {
		    newtype = SETUP_FILLED_NO_DRAW;
		}
		Setup->map_data[i] = newtype;
	    }
	}
    }
}

static int Map_init(void)
{
    int			i,
			max,
			type;
    byte		types[256];

    num_fuels = 0;
    num_bases = 0;
    num_cannons = 0;
    num_targets = 0;
    fuels = NULL;
    bases = NULL;
    cannons = NULL;
    targets = NULL;
    memset(types, 0, sizeof types);
    types[SETUP_FUEL] = 1;
    types[SETUP_CANNON_UP] = 2;
    types[SETUP_CANNON_RIGHT] = 2;
    types[SETUP_CANNON_DOWN] = 2;
    types[SETUP_CANNON_LEFT] = 2;
    for (i = SETUP_TARGET; i < SETUP_TARGET + 10; i++) {
	types[i] = 3;
    }
    for (i = SETUP_BASE_LOWEST; i <= SETUP_BASE_HIGHEST; i++) {
	types[i] = 4;
    }
    max = Setup->x * Setup->y;
    for (i = 0; i < max; i++) {
	switch (types[Setup->map_data[i]]) {
	case 1: num_fuels++; break;
	case 2: num_cannons++; break;
	case 3: num_targets++; break;
	case 4: num_bases++; break;
	}
    }
    if (num_bases != 0) {
	bases = (homebase_t *) malloc(num_bases * sizeof(homebase_t));
	if (bases == NULL) {
	    error("No memory for Map bases (%d)", num_bases);
	    return -1;
	}
	num_bases = 0;
    }
    if (num_fuels != 0) {
	fuels = (fuelstation_t *) malloc(num_fuels * sizeof(fuelstation_t));
	if (fuels == NULL) {
	    error("No memory for Map fuels (%d)", num_fuels);
	    return -1;
	}
	num_fuels = 0;
    }
    if (num_targets != 0) {
	targets = (target_t *) malloc(num_targets * sizeof(target_t));
	if (targets == NULL) {
	    error("No memory for Map targets (%d)", num_targets);
	    return -1;
	}
	num_targets = 0;
    }
    if (num_cannons != 0) {
	cannons = (cannontime_t *) malloc(num_cannons * sizeof(cannontime_t));
	if (cannons == NULL) {
	    error("No memory for Map cannons (%d)", num_cannons);
	    return -1;
	}
	num_cannons = 0;
    }
    for (i = 0; i < MAX_CHECKPOINT; i++) {
	types[SETUP_CHECK + i] = 5;
    }
    for (i = 0; i < max; i++) {
	type = Setup->map_data[i];
	switch (types[type]) {
	case 1:
	    fuels[num_fuels].pos = i;
	    fuels[num_fuels].fuel = MAX_STATION_FUEL;
	    num_fuels++;
	    break;
	case 2:
	    cannons[num_cannons].pos = i;
	    cannons[num_cannons].dead_time = 0;
	    cannons[num_cannons].dot = 0;
	    num_cannons++;
	    break;
	case 3:
	    targets[num_targets].pos = i;
	    targets[num_targets].dead_time = 0;
	    targets[num_targets].damage = TARGET_DAMAGE;
	    num_targets++;
	    break;
	case 4:
	    bases[num_bases].pos = i;
	    bases[num_bases].id = -1;
	    bases[num_bases].team = type % 10;
	    num_bases++;
	    Setup->map_data[i] = type - (type % 10);
	    break;
	case 5:
	    checks[type - SETUP_CHECK].pos = i;
	    Setup->map_data[i] = SETUP_CHECK;
	    break;
	}
    }
    return 0;
}

static int Map_cleanup(void)
{
    if (num_bases > 0) {
	if (bases != NULL) {
	    free(bases);
	    bases = NULL;
	}
	num_bases = 0;
    }
    if (num_fuels > 0) {
	if (fuels != NULL) {
	    free(fuels);
	    fuels = NULL;
	}
	num_fuels = 0;
    }
    if (num_targets > 0) {
	if (targets != NULL) {
	    free(targets);
	    targets = NULL;
	}
	num_targets = 0;
    }
    return 0;
}

other_t *Other_by_id(int id)
{
    int			i;

    if (id != -1) {
	for (i = 0; i < num_others; i++) {
	    if (Others[i].id == id) {
		return &Others[i];
	    }
	}
    }
    return NULL;
}

int Handle_leave(int id)
{
    other_t		*other;
    int			i;
    char		msg[MSG_LEN];

    if ((other = Other_by_id(id)) != NULL) {
	if (other == self) {
	    errno = 0;
	    error("Self left?!");
	    self = NULL;
	}
	/*
	 * Ignore tanks and robots.
	 */
	if (other->mychar != 'T'
	    && other->mychar != 'R') {
	    sprintf(msg, "%s left the game.", other->name);
	    Add_message(msg);
	}
	num_others--;
	while (other < &Others[num_others]) {
	    *other = other[1];
	    other++;
	}
	scoresChanged = 1;
    }
    for (i = 0; i < num_others; i++) {
	other = &Others[i];
	if (other->war_id == id) {
	    other->war_id = -1;
	    scoresChanged = 1;
	}
    }
    return 0;
}

int Handle_player(int id, int team, int mychar, char *player_name,
		  char *real_name, char *host_name)
{
    other_t		*other;

    if ((other = Other_by_id(id)) == NULL) {
	if (num_others >= max_others) {
	    max_others += 5;
	    if (num_others == 0) {
		Others = (other_t *) malloc(max_others * sizeof(other_t));
	    } else {
		Others = (other_t *) realloc(Others,
		    max_others * sizeof(other_t));
	    }
	    if (Others == NULL) {
		error("Not enough memory for player info");
		num_others = max_others = 0;
		self = NULL;
		return -1;
	    }
	    if (self != NULL) {
		/*
		 * We've made `self' the first member of Others[].
		 */
		self = &Others[0];
	    }
	}
	other = &Others[num_others++];
    }
    if (self == NULL
	&& strcmp(name, player_name) == 0) {
	if (other != &Others[0]) {
	    /*
	     * Make `self' the first member of Others[].
	     */
	    *other = Others[0];
	    other = &Others[0];
	}
	self = other;
    }
    other->id = id;
    other->team = team;
    other->score = 0;
    other->life = 0;
    other->mychar = mychar;
    other->war_id = -1;
    other->name_width = 0;
    strncpy(other->name, player_name, sizeof(other->name));
    other->name[sizeof(other->name) - 1] = '\0';
    strncpy(other->real, real_name, sizeof(other->real));
    other->real[sizeof(other->real) - 1] = '\0';
    strncpy(other->host, host_name, sizeof(other->host));
    other->host[sizeof(other->host) - 1] = '\0';
    scoresChanged = 1;

    return 0;
}

int Handle_war(int robot_id, int killer_id)
{
    other_t		*robot,
			*killer;
    char		msg[MSG_LEN];

    if ((robot = Other_by_id(robot_id)) == NULL) {
	errno = 0;
	error("Can't update war for non-existing player (%d,%d)", robot_id, killer_id);
	return 0;
    }
    if (killer_id == -1) {
	/*
	 * Robot is no longer in war mode.
	 */
	robot->war_id = -1;
	return 0;
    }
    if ((killer = Other_by_id(killer_id)) == NULL) {
	errno = 0;
	error("Can't update war against non-existing player (%d,%d)", robot_id, killer_id);
	return 0;
    }
    robot->war_id = killer_id;
    sprintf(msg, "%s declares war on %s.", robot->name, killer->name);
    Add_message(msg);
    scoresChanged = 1;

    return 0;
}

int Handle_seek(int programmer_id, int robot_id, int sought_id)
{
    other_t		*programmer,
			*robot,
			*sought;
    char		msg[MSG_LEN];

    if ((programmer = Other_by_id(programmer_id)) == NULL
	|| (robot = Other_by_id(robot_id)) == NULL
	|| (sought = Other_by_id(sought_id)) == NULL) {
	errno = 0;
	error("Bad player seek (%d,%d,%d)",
	      programmer_id, robot_id, sought_id);
	return 0;
    }
    robot->war_id = sought_id;
    sprintf(msg, "%s has programmed %s to seek %s.",
	    programmer->name, robot->name, sought->name);
    Add_message(msg);
    scoresChanged = 1;


    return 0;
}

int Handle_score(int id, int score, int life, int mychar)
{
    other_t		*other;

    if ((other = Other_by_id(id)) == NULL) {
	errno = 0;
	error("Can't update score for non-existing player %d,%d,%d", id, score, life);
	return 0;
    }
    else if (other->score != score
	|| other->life != life
	|| other->mychar != mychar && version >= 0x3040) {
	other->score = score;
	other->life = life;
	if (version >= 0x3040) {
	    other->mychar = mychar;
	}
	scoresChanged = 1;
    }

    return 0;
}

int Handle_score_object(int score, int x, int y, char *msg)
{
    score_object_t*	sobj = &score_objects[score_object];

    sobj->score = score;
    sobj->x = x;
    sobj->y = y;
    sobj->count = 1;

    /* Initialize sobj->hud_msg (is shown on the HUD) */
    if (msg[0] != '\0') {
	sprintf(sobj->hud_msg, "%s %d", msg, score);
	sobj->hud_msg_len = strlen(sobj->hud_msg);
	sobj->hud_msg_width = XTextWidth(gameFont,
					 sobj->hud_msg, sobj->hud_msg_len);
    } else
	sobj->hud_msg_len = 0;

    /* Initialize sobj->msg data (is shown on game area) */
    sprintf(sobj->msg, "%d", score);
    sobj->msg_len = strlen(sobj->msg);
    sobj->msg_width = XTextWidth(gameFont, sobj->msg, sobj->msg_len);

    /* Update global index variable */
    score_object = (score_object + 1) % MAX_SCORE_OBJECTS;

    return 0;
}

void Client_score_table(void)
{
    other_t		*other,
			**order;
    int			i, j, k, best = -1;
    float		ratio, best_ratio = -1e7;

    if (scoresChanged == 0) {
	return;
    }
    if (players_exposed == false) {
	return;
    }
    if (num_others < 1) {
	Paint_score_start();
	scoresChanged = 0;
	return;
    }
    if ((order = (other_t **)malloc(num_others * sizeof(other_t *))) == NULL) {
	error("No memory for score");
	return;
    }
    for (i = 0; i < num_others; i++) {
	other = &Others[i];
	if (BIT(Setup->mode, LIMITED_LIVES)) {
	    ratio = (float) other->score;
	} else {
	    ratio = (float) other->score / (other->life + 1);
	}
	if (best == -1
	    || ratio > best_ratio) {
	    best_ratio = ratio;
	    best = i;
	}
	for (j = 0; j < i; j++) {
	    if (order[j]->score < other->score) {
		for (k = i; k > j; k--) {
		    order[k] = order[k - 1];
		}
		break;
	    } 
	}
	order[j] = other;
    }
    Paint_score_start();
    for (i = 0; i < num_others; i++) {
	other = order[i];
	j = other - Others;
	Paint_score_entry(i, other, j == best ? true : false);
    }
    free(order);
    scoresChanged = 0;
}

int Client_init(char *server, unsigned server_version)
{
    extern void Make_table(void);
    extern void Make_ships(void);

    version = server_version;

    Make_table();
    Make_ships();

    strncpy(servername, server, sizeof(servername) - 1);

    return 0;
}

int Client_setup(void)
{
    if (Map_init() == -1) {
	return -1;
    }
    Map_dots();
    Map_blue();

    RadarHeight = (int)((256.0/Setup->x) * Setup->y + 0.5);

    if (Init_window() == -1) {
	return -1;
    }
    if (Alloc_msgs(MAX_MSGS) == -1) {
	return -1;
    }
    return 0;
}

int Client_power(void)
{
    if (Send_power(power) == -1
	|| Send_power_s(power_s) == -1
	|| Send_turnspeed(turnspeed) == -1
	|| Send_turnspeed_s(turnspeed_s) == -1
	|| Send_turnresistance(turnresistance) == -1
	|| Send_turnresistance_s(turnresistance_s) == -1
	|| Send_display() == -1) {
	return -1;
    }
    return 0;
}

int Client_start(void)
{
    Key_init();

    return 0;
}

void Client_cleanup(void)
{
    Quit();
    if (max_others > 0) {
	free(Others);
	num_others = max_others = 0;
    }
    Map_cleanup();
}

int Client_fd(void)
{
    return ConnectionNumber(dpy);
}

int Client_input(int new_input)
{
    return xevent(new_input);
}

void Client_flush(void)
{
    XFlush(dpy);
}

void Client_sync(void)
{
    XSync(dpy, False);
}

int Client_wrap_mode(void)
{
    return (BIT(Setup->mode, WRAP_PLAY) != 0);
}

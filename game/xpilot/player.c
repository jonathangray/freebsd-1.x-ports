/* $Id: player.c,v 1.1 1994/02/23 14:40:07 jkh Exp $
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
#define SERVER
#include "global.h"
#include "map.h"
#include "score.h"
#include "robot.h"
#include "bit.h"
#include "netserver.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: player.c,v 1.1 1994/02/23 14:40:07 jkh Exp $";
#endif


static char msg[MSG_LEN];

bool	updateScores = true;


/********** **********
 * Functions on player array.
 */

void Pick_startpos(int ind)
{
    player	*pl = Players[ind];
    int		i, num_free, pick, seen;
    static int	prev_num_bases = 0;
    static char	*free_bases = NULL;

    if (pl->robot_mode == RM_OBJECT) {
	pl->home_base = 0;
	return;
    }
    if (prev_num_bases != World.NumBases) {
	prev_num_bases = World.NumBases;
	if (free_bases != NULL) {
	    free(free_bases);
	}
	free_bases = (char *) malloc(World.NumBases * sizeof(*free_bases));
	if (free_bases == NULL) {
	    error("Can't allocate memory for free_bases");
	    End_game();
	}
    }
    num_free = 0;
    for (i = 0; i < World.NumBases; i++) {
	if (World.base[i].team == pl->team) {
	    num_free++;
	    free_bases[i] = 1;
	} else {
	    free_bases[i] = 0;
	}
    }
    for (i = 0; i < NumPlayers; i++) {
	if (i != ind
	    && Players[i]->robot_mode != RM_OBJECT
	    && free_bases[Players[i]->home_base] != 0) {
	    free_bases[Players[i]->home_base] = 0;
	    num_free--;
	}
    }
    pick = rand() % num_free;
    seen = 0;
    for (i = 0; i < World.NumBases; i++) {
	if (free_bases[i] != 0) {
	    if (seen < pick) {
		seen++;
	    } else {
		pl->home_base = i;
		if (ind < NumPlayers) {
		    for (i = 0; i < NumPlayers; i++) {
			if (Players[i]->conn != NOT_CONNECTED) {
			    Send_base(Players[i]->conn,
				      pl->id, 
				      pl->home_base);
			}
		    }
		    if (BIT(pl->status, PLAYING) == 0) {
			pl->count = RECOVERY_DELAY;
		    }
		    else if (BIT(pl->status, PAUSE|GAME_OVER)) {
			Go_home(ind);
		    }
		}
		return;
	    }
	}
    }
    error("Can't pick startpos (ind=%d,num=%d,free=%d,pick=%d,seen=%d)",
	ind, World.NumBases, num_free, pick, seen);
    End_game();
}



void Go_home(int ind)
{
    player	*pl = Players[ind];
    int		i, x, y;


    x = World.base[pl->home_base].pos.x;
    y = World.base[pl->home_base].pos.y;

    pl->dir = pl->float_dir = World.base[pl->home_base].dir;

    pl->pos.x = x * BLOCK_SZ + BLOCK_SZ/2.0;
    pl->pos.y = y * BLOCK_SZ + BLOCK_SZ/2.0;
/*    pl->pos.y = y * BLOCK_SZ - ships[DIR_UP].pts[1].y;*/
    pl->prevpos = pl->pos;
    pl->acc.x = pl->acc.y = 0.0;
    pl->vel.x = pl->vel.y = pl->velocity = 0.0;
    pl->turnacc = pl->turnvel = 0.0;
    memset(pl->last_keyv, 0, sizeof(pl->last_keyv));
    memset(pl->prev_keyv, 0, sizeof(pl->prev_keyv));
    pl->key_changed = 0;
    CLR_BIT(pl->used, OBJ_CONNECTOR | OBJ_REFUEL | OBJ_REPAIR);
    if (playerStartsShielded != 0) {
	SET_BIT(pl->used, OBJ_SHIELD);
	if (playerShielding == 0) {
	    pl->shield_time = 2 * FPS;
	    SET_BIT(pl->have, OBJ_SHIELD);
	}
    }
    CLR_BIT(pl->status, THRUSTING);
    pl->updateVisibility = 1;
    for (i = 0; i < NumPlayers; i++) {
	pl->visibility[i].lastChange = 0;
	Players[i]->visibility[ind].lastChange = 0;
    }

    if (pl->robot_mode != RM_NOT_ROBOT)
	pl->robot_mode = RM_TAKE_OFF;
}


void Init_player(int ind)
{
    player *pl = Players[ind];
    bool too_late = false;
    int i;


    pl->vel.x	= pl->vel.y	= 0.0;
    pl->acc.x	= pl->acc.y	= 0.0;
    pl->dir	= pl->float_dir = DIR_UP;
    pl->turnvel		= 0.0;
#ifdef	TURN_FUEL
    pl->oldturnvel	= 0.0;
#endif
    pl->turnacc		= 0.0;
    pl->mass		= ShipMass;
    pl->emptymass	= ShipMass;
    pl->fuel.num_tanks  = 0;
    pl->fuel.current    = 0;
    pl->fuel.sum        =
    pl->fuel.tank[0]    = ( (initialFuel << FUEL_SCALE_BITS)
			   + (((rand()%400)-200) << FUEL_SCALE_BITS) );
    pl->fuel.max        = TANK_CAP(0);
    for (i = 1; i <= initialTanks; i++) {
	pl->fuel.num_tanks++;
	SET_BIT(pl->have, OBJ_TANK);
	pl->fuel.current = i;
	pl->fuel.max += TANK_CAP(i);
	pl->fuel.tank[i] = 0;
	pl->emptymass += TANK_MASS;
	Add_fuel(&pl->fuel, TANK_FUEL(pl->fuel.current));
    }
    pl->fuel.current = 0;

    pl->afterburners   = initialAfterburners;
    pl->transporters    = initialTransporters;
    pl->transInfo.count	= 0;

    pl->power			= 45.0;
    pl->turnspeed		= 30.0;
    pl->turnresistance		= 0.12;
    pl->power_s			= 35.0;
    pl->turnspeed_s		= 25.0;
    pl->turnresistance_s	= 0.12;

    if (BIT(World.rules->mode, TIMING)) {
	pl->power	= MAX_PLAYER_POWER;
	pl->turnspeed	= 27.0;
    }

    pl->check		= 0;
    pl->round		= 0;
    pl->time		= 0;
    pl->last_lap_time	= 0;
    pl->last_time	= 0;
    pl->last_lap	= 0;
    pl->best_run	= 0;
    pl->best_lap	= 0;
    pl->count		= -1;
    pl->shield_time	= 0;

    pl->type		= OBJ_PLAYER;
    pl->shots		= 0;
    pl->extra_shots	= initialWideangles;
    pl->back_shots	= initialRearshots;
    pl->missiles	= initialMissiles;
    pl->mines		= initialMines;
    pl->cloaks		= initialCloaks;
    pl->sensors		= initialSensors;
    pl->forceVisible	= 0;
    pl->shot_speed	= ShotsSpeed;
    pl->sensor_range	= MAX(pl->fuel.sum * ENERGY_RANGE_FACTOR,
                              VISIBILITY_DISTANCE);
    pl->max_speed	= SPEED_LIMIT - pl->shot_speed;
    pl->shot_max	= ShotsMax;
    pl->shot_life	= ShotsLife;
    pl->shot_mass	= ShotsMass;
    pl->color		= WHITE;
    pl->score		= 0;
    pl->prev_score	= 0;
    pl->fs		= 0;
    pl->repair_target	= 0;
    pl->name[0]		= '\0';
    pl->ecms 		= initialECMs;
    pl->lasers 		= initialLasers;
    pl->num_pulses	= 0;
    pl->max_pulses	= 0;
    pl->pulses		= NULL;
    pl->ecmInfo.size	= 0;
    pl->damaged 	= 0;

    pl->mode		= World.rules->mode;
    pl->status		= PLAYING | GRAVITY | DEF_BITS;
    pl->have		= DEF_HAVE;
    pl->used		= DEF_USED;

    {
	static u_short	pseudo_team_no = 0;
        pl->pseudo_team = pseudo_team_no++;
    }
    pl->mychar		= ' ';
    pl->prev_mychar	= pl->mychar;
    pl->life		= World.rules->lives;
    pl->prev_life	= pl->life;
    pl->ball 		= NULL;

    /*
     * If limited lives and if nobody has lost a life yet, you may enter
     * now, otherwise you will have to wait 'til everyone gets GAME OVER.
     */
    if (BIT(pl->mode, LIMITED_LIVES)) {
	for (i=0; i<NumPlayers; i++)
	    /* If anybody has lost a life, then it's too late to join */
	    if (Players[i]->life < World.rules->lives)
		too_late = true;
	if (too_late) {
	    pl->mychar	= 'W';
	    pl->prev_life = pl->life = 0;
	    SET_BIT(pl->status, GAME_OVER);
	}
    }

    pl->team = TEAM_NOT_SET;

    pl->lock.tagged	= LOCK_NONE;
    pl->lock.pl_id	= 0;
    pl->lock.pos.x	= pl->pos.x;
    pl->lock.pos.y	= pl->pos.y;

    pl->robot_mode	= RM_NOT_ROBOT;
    pl->robot_count	= 0;
    pl->robot_ind	= -1;
    pl->robot_lock	= LOCK_NONE;
    pl->robot_lock_id	= 0;

    pl->wormDrawCount   = 0;

    pl->id		= Id;
    GetInd[Id]		= ind;
    pl->rplay_fd	= -1;
    pl->conn		= NOT_CONNECTED;
    pl->audio		= NULL;
}


static player			*playerArray;
static struct _visibility	*visibilityArray;

void Alloc_players(int number)
{
    player *p;
    struct _visibility *t;
    int i;


    /* Allocate space for pointers */
    Players = (player **)malloc(number * sizeof(player *));

    /* Allocate space for all entries, all player structs */
    p = playerArray = (player *)malloc(number * sizeof(player));

    /* Allocate space for all visibility arrays, n arrays of n entries */
    t = visibilityArray = (struct _visibility *)
	malloc(number * number * sizeof(struct _visibility));

    for (i=0; i<number; i++) {
	Players[i] = p++;
	Players[i]->visibility = t;
	/* Advance to next block/array */
	t += number;
    }
}



void Free_players(void)
{
    free(Players);
    free(playerArray);
    free(visibilityArray);
}



void Update_score_table(void)
{
    int			i, j;
    player		*pl, *tmp;

    for (j = 0; j < NumPlayers; j++) {
	pl = Players[j];
	if (pl->score != pl->prev_score
	    || pl->life != pl->prev_life
	    || pl->mychar != pl->prev_mychar) {
	    pl->prev_score = pl->score;
	    pl->prev_life = pl->life;
	    pl->prev_mychar = pl->mychar;
	    for (i = 0; i < NumPlayers; i++) {
		tmp = Players[i];
		if (tmp->conn == NOT_CONNECTED) {
		    continue;
		}
		Send_score(tmp->conn, pl->id, pl->score, pl->life, pl->mychar);
	    }
	}
    }
    updateScores = false;
}

void Reset_all_players(void)
{
    int i,j;

    for (i=0; i<NumPlayers; i++) {
	CLR_BIT(Players[i]->status, GAME_OVER);
	CLR_BIT(Players[i]->have, OBJ_BALL);
	if (Players[i]->mychar != 'P') {
	    Players[i]->mychar = ' ';
	    Players[i]->life = World.rules->lives;
	}
	if (Players[i]->robot_mode != RM_NOT_ROBOT)
	    Players[i]->mychar = 'R';
	if (Players[i]->robot_mode == RM_OBJECT)
	    Players[i]->mychar = 'T';
    }
    if (BIT(World.rules->mode, TEAM_PLAY)) {

	/* Detach any balls and kill ball */
	/* We are starting all over again */
	for(j=0;j < NumObjs ; j++) {
	    if (Obj[j]->type == OBJ_BALL) {
		Obj[j]->id = -1;
		Obj[j]->life = 0;
		Obj[j]->count = 10;
	    }
	}

	/* Reset the treasures */
	for(i=0; i < World.NumTreasures; i++) {
	    Make_ball( -1, World.treasures[i].pos.x 
		      * BLOCK_SZ + (BLOCK_SZ/2),
		      World.treasures[i].pos.y * BLOCK_SZ + 10, false, i);
	    World.treasures[i].have = true;
	    World.treasures[i].count = 0;
	}
    }

    Update_score_table();
}

void Compute_game_status(void)
{
    int i;

    if (BIT(World.rules->mode, TEAM_PLAY)) {

	/* Do we have a winning team ? */

	enum TeamState {
	    TeamEmpty,
	    TeamDead,
	    TeamAlive
	}	team_state[MAX_TEAMS];
	int	num_dead_teams = 0;
	int	num_alive_teams = 0;
	int	winning_team = -1;

	for (i = 0; i < MAX_TEAMS; i++) {
	    team_state[i] = TeamEmpty;
	}

	for (i = 0; i < NumPlayers; i++) {
	    if (Players[i]->robot_mode == RM_OBJECT) {
		/* Ignore tanks. */
		continue;
	    }
	    else if (BIT(Players[i]->status, PAUSE)) {
		/* Ignore paused players. */
		continue;
	    }
	    else if (BIT(Players[i]->status, GAME_OVER)) {
		if (team_state[Players[i]->team] == TeamEmpty) {
		    /* Assume all teammembers are dead. */
		    num_dead_teams++;
		    team_state[Players[i]->team] = TeamDead;
		}
	    }
	    /*
	     * If the player is not paused and he is not in the
	     * game over mode then he is considered alive.
	     * But he may not be playing though if the rest of the team
	     * was genocided very quickly after game reset, while this
	     * player was still being transported back to his homebase.
	     */
	    else if (team_state[Players[i]->team] != TeamAlive) {
		if (team_state[Players[i]->team] == TeamDead) {
		    /* Oops!  Not all teammembers were dead yet. */
		    num_dead_teams--;
		}
		team_state[Players[i]->team] = TeamAlive;
		if (++num_alive_teams > 1) {
		    /* Still a team battle going on. */
		    return;
		} else {
		    /* Remember a team which was alive. */
		    winning_team = Players[i]->team;
		}
	    }
	}
	
	if ((num_alive_teams == 1) && (num_dead_teams > 0)) {
	    sprintf(msg, "Team %d has won the game!", winning_team);
	    Set_message(msg);
	    for (i = 0; i < NumPlayers; i++) {
		if (BIT(Players[i]->status, PAUSE)) {
		    /* Paused players don't get any points. */
		    continue;
		}
		else if (Players[i]->robot_mode == RM_OBJECT) {
		    /* Ignore tanks. */
		    continue;
		}
		else if (Players[i]->team == winning_team) {
		    SCORE(i, PTS_GAME_WON, 
			  (int) Players[i]->pos.x/ BLOCK_SZ,
			  (int) Players[i]->pos.y/BLOCK_SZ, "Winner");
		}
	    }
	    /* Start up all player's again */
	    Reset_all_players();
	} else if (num_alive_teams == 0 && num_dead_teams > 1) {
	    sprintf(msg, "We have a draw!");
	    Set_message(msg);
	    /* Start up all player's again */
	    Reset_all_players();    
	}
    } else {

    /* Do we have a winner ? (No team play) */
	int num_alive_players = 0;
	int num_active_players = 0;
	int num_alive_robots = 0;
	int num_active_humans = 0;
	int ind = -1;

	for (i=0; i < NumPlayers; i++)  {
	    if (!BIT(Players[i]->status, PAUSE)) {
		if (!BIT(Players[i]->status, GAME_OVER)) {
		    num_alive_players++;
		    if (Players[i]->robot_mode != RM_NOT_ROBOT
			&& Players[i]->robot_mode != RM_OBJECT) {
			num_alive_robots++;
		    }
		    ind = i; 	/* Tag player that's alive */
		}
		else if (Players[i]->robot_mode == RM_NOT_ROBOT) {
		    num_active_humans++;
		}
		num_active_players++;
	    }
	}

	if (num_alive_players == 1 && num_active_players > 1) {
	    sprintf(msg, "%s has won the game!", Players[ind]->name);
	    Set_message(msg);
	    SCORE(ind, PTS_GAME_WON, (int) Players[ind]->pos.x/ BLOCK_SZ,
		  (int) Players[ind]->pos.y/BLOCK_SZ, "Winner");
	    /* Start up all player's again */
	    Reset_all_players();    
	} else if (num_alive_players == 0 && num_active_players >= 1) {
	    sprintf(msg, "We have a draw!");
	    Set_message(msg);
	    /* Start up all player's again */
	    Reset_all_players();    	    
	}
	else if (num_alive_robots > 1
	    && num_alive_players == num_alive_robots
	    && num_active_humans > 0) {
	    Set_message("The robots have won the game!");
	    for (i = 0; i < NumPlayers; i++)  {
		if (!BIT(Players[i]->status, PAUSE|GAME_OVER)) {
		    Players[i]->score += PTS_GAME_WON;
		}
	    }
	    /* Start up all player's again */
	    Reset_all_players();    
	}
    }
}

void Delete_player(int ind)
{
    player *pl;
    int i, id;


    pl = Players[ind];
    id = pl->id;

    for (i=0; i<NumObjs; i++)		/* Delete all remaining shots */
	if (Obj[i]->id == id)
	    Delete_shot(i);

    if (pl->max_pulses > 0 && pl->pulses != NULL) {
	free(pl->pulses);
	pl->max_pulses = 0;
	pl->num_pulses = 0;
	pl->pulses = NULL;
    }

#ifdef SOUND
    sound_close(pl);
#endif /* SOUND */
    NumPlayers--;
    
    if (pl->team != TEAM_NOT_SET && pl->robot_mode != RM_OBJECT)
	World.teams[pl->team].NumMembers--;

    if (pl->robot_mode != RM_NOT_ROBOT && pl->robot_mode != RM_OBJECT)
	NumRobots--;

    /*
     * Swap entry no 'ind' with the last one.
     */
    pl			= Players[NumPlayers];	/* Swap pointers... */
    Players[NumPlayers]	= Players[ind];
    Players[ind]	= pl;

    GetInd[Players[ind]->id] = ind;

    for (i=0; i<NumPlayers; i++) {
	if ((Players[i]->lock.pl_id == id) || NumPlayers <= 1)
	    Players[i]->lock.tagged = LOCK_NONE;
	if (Players[i]->robot_mode != RM_NOT_ROBOT
	    && Players[i]->robot_lock == LOCK_PLAYER
	    && Players[i]->robot_lock_id == id) {
	    Players[i]->robot_lock = LOCK_NONE;
	}
    }

    for (i = 0; i < NumPlayers; i++) {
	if (Players[i]->conn != NOT_CONNECTED) {
	    Send_leave(Players[i]->conn, id);
	}
    }
}

void Kill_player(int ind)
{
    player *pl;
    int i;

    Explode(ind);

    pl			= Players[ind];
    pl->vel.x		= pl->vel.y	= 0.0;
    pl->acc.x		= pl->acc.y	= 0.0;
    pl->emptymass	= pl->mass	= ShipMass;
    pl->status		|= DEF_BITS;
    pl->status		&= ~(KILL_BITS);
    pl->extra_shots	= initialWideangles;
    pl->back_shots	= initialRearshots;
    pl->missiles	= initialMissiles;
    pl->mines		= initialMines;
    pl->cloaks		= initialCloaks;
    pl->sensors		= initialSensors;
    pl->forceVisible	= 0;
    pl->shot_speed	= ShotsSpeed;
    pl->shot_max	= ShotsMax;
    pl->shot_life	= ShotsLife;
    pl->shot_mass	= ShotsMass;
    pl->last_time	= pl->time;
    pl->last_lap	= 0;
    pl->count		= RECOVERY_DELAY;
    pl->ecms 		= initialECMs;
    pl->ecmInfo.size	= 0;
    pl->lasers 		= initialLasers;
    pl->damaged 	= 0;
    pl->lock.distance	= 0;

    pl->fuel.current    = 0;
    pl->fuel.num_tanks	= 0;
    pl->fuel.max        = TANK_CAP(0);
    pl->fuel.sum       	*= 0.90;		/* Loose 10% of fuel */
    if (pl->fuel.sum>pl->fuel.max) pl->fuel.sum = pl->fuel.max;
    pl->fuel.tank[0]    =
    pl->fuel.sum        = MAX(pl->fuel.sum,
                              MIN_PLAYER_FUEL+(rand()%(int)MIN_PLAYER_FUEL)/5);
    for (i = 1; i <= initialTanks; i++) {
	pl->fuel.num_tanks++;
	SET_BIT(pl->have, OBJ_TANK);
	pl->fuel.current = i;
	pl->fuel.max += TANK_CAP(i);
	pl->fuel.tank[i] = 0;
	pl->emptymass += TANK_MASS;
	Add_fuel(&pl->fuel, TANK_FUEL(pl->fuel.current));
    }
    pl->fuel.current    = 0;

    pl->afterburners	= initialAfterburners;
    pl->transporters    = initialTransporters;
    pl->transInfo.count	= 0;

    if (pl->max_pulses > 0 && pl->num_pulses == 0) {
	free(pl->pulses);
	pl->pulses = NULL;
	pl->max_pulses = 0;
    }

    if (BIT(World.rules->mode, TIMING))
	pl->fuel.sum = pl->fuel.tank[0] = RACE_PLAYER_FUEL;

    /* Detach ball from player */
    if (BIT(pl->have, OBJ_BALL))
	for(i=0; i<NumObjs; i++) 
	    if (BIT(Obj[i]->type, OBJ_BALL) && Obj[i]->id == pl->id)
		Obj[i]->id = -1;

    if (BIT(pl->mode, LIMITED_LIVES)) {
	if (pl->life-- <= 0) {
	    pl->life = 0;
	    SET_BIT(pl->status, GAME_OVER);
	    pl->mychar = 'D';
	}
    } else {
	pl->life++;
    }

    pl->have	= DEF_HAVE;
    pl->used	|= DEF_USED;
    pl->used	&= ~(USED_KILL);
    pl->used	&= pl->have;
}
